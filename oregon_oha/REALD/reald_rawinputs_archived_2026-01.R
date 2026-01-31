# Script that reads in person-level PUMS data and recreates, almost perfectly,
# the original (2025) REALD assignments generated with the IPUMS datasets
# NOTES:
# - due to size I did NOT upload the PUMS to the repo
#   re-running the script will require doing that manually
#   file readin is in pums_raw = read.csv('global_inpus/psam_p41.csv')) |>
# - there's also some code in here (e.g., update_counts()) that I used
#   when reconciling but are not needed for downscaling
# - future improvements and/or other notes are headed with NOTE:
#   (doing a ctrl+f for these will give all updates)
# - Not sure yet if this script will be permanent or if it is mutable
#   i.e., not sure if I will make a separate script for incorporating
#   into the REALD workflow or if I will build on this one
# - I will go through at some point and tidy up the regex calls... 
#   again not sure if I'll do that here or elsewhere
# 
# Scott Nordstrom - finalized 23 Jan 2026 (scottn@pdx.edu)

# Packages typically used
library(dplyr)
library(tidyr)
library(purrr)

# Set global option to suppress message from summarise()
options(dplyr.summarise.inform = FALSE)

rm(list = ls())

# Wrapper for comparisons

update_counts = function() {
  
  if (!exists('orig_reald')) {
    
    orig_reald <<- read.csv('~/Desktop/pums5y23_reald_forcompare.csv') |>
      select(-GQ)
    
    pums_compare = merge(
      pums_out |>
        select(serialno, sporder, starts_with(c('AIAN', 'Asn', 'Afr', 'Lat', 'MENA', 'NHPI', 'Wht', 'Other'))),
      orig_reald,
      by.x = c('serialno', 'sporder'), by.y = c('CBSERIAL', 'PERNUM')
    )
    
    nrow(pums_compare)
    
    orig_counts <<- orig_reald |>
      select(starts_with(c('AIAN', 'Asn', 'Afr', 'Lat', 'MENA', 'NHPI', 'Wht', 'Other'))) |>
      apply(2, sum) |>
      (\(vec) data.frame(reald = names(vec), in.old = vec))()
    
  }
  
  new_counts = pums_out |>
    select(starts_with(c('AIAN', 'Asn', 'Afr', 'Lat', 'MENA', 'NHPI', 'Wht', 'Other'))) |>
    apply(2, sum) |>
    (\(vec) data.frame(reald = names(vec), in.new = vec))()
  
  compare_counts = merge(new_counts, orig_counts, all = TRUE)
  
  compare_counts
  
}

###### ================================================= ########
# Read in files

### Read in PUMS and do some formatting
pums_raw = read.csv('global_inputs/psam_p41.csv') |>
  rename_with(tolower) |>
  select(
    serialno, sporder, puma, pwgtp,
    matches('^rac'), matches('^anc.'), lanp, hisp, pobp
  ) 

head(pums_raw)
nrow(pums_raw)
names(pums_raw)

# # Additional read-in for assigned probability of Jewish ancestry
# pums_jet = readRDS(file = 'oregon_oha/01_data_inputs/pums_jet_v03.rds') 
# 
# # Check to make sure number of rows matches
# nrow(pums_jet) == nrow(pums_raw) # they don't... at the moment...
# 
# pums_jet = pums_jet |>
#   mutate(
#     # Add in Jewish grouping columns
#     JAshkenazi = ethgrpi %in% 1,
#     JSephardic = ethgrpi %in% 2,
#     JOther     = ethgrpi %in% 3
#   ) |>
#   # Get rid of other columns
#   select(-contains('eth'))
# 
# head(pums_jet)

### Read in data dictionaries
# Get the names of the PUMS data dictionary files
grep('pums\\_data\\_dict', dir('global_inputs/'), value = TRUE) |>
  # name each list element after the table (extracted from the filename)
  set_names(nm = \(fn) gsub('.+\\_([a-z0-9]+)\\.txt$', '\\1', fn)) |>
  # Read in CSVs
  map(.f = \(fn) read.table(paste0('global_inputs/', fn), sep = '.', quote = "")) |>
  # Add in the column names (they aren't in the dictionary files)
  imap(.f = \(df, tabname) set_names(df, nm = c(tabname, paste0(tabname, '_str')))) |>
  # # Rename the rac2p frames (only) to facilitate merging
  # map(.f = \(df) rename_with(df, ~ gsub('2p\\d{2}', '2p', .))) |>
  # Change everything to lowercase
  map(.f = \(df) df |> mutate(across(where(is.character), tolower))) |>
  # create an object for each of these (I'm not sure this is working...)
  iwalk(.f = \(df, tabname) assign(x = paste0(tabname, '_dict'), value = df, envir = .GlobalEnv))
  
# Start merging objects together (pums_str = pums with strings)
pums_str = pums_raw |>
  # Merge in ancestry 1
  merge(anc1p_dict, all.x = TRUE) |>
  # Merge in ancestry 2
  merge(anc2p_dict, all.x = TRUE) |>
  # Merge in hispanic ancestry
  merge(hisp_dict, all.x = TRUE) |>
  # Merge in language dictionary
  merge(lanp_dict, all.x = TRUE) |>
  # Merge in place of birth dictionary
  merge(pobp_dict, all.x = TRUE) |>
  # Merge in race 1
  merge(rac1p_dict, all.x = TRUE) |>
  # Merge in race 2 (2019-2022)
  merge(rac2p19_dict, all.x = TRUE) |>
  # Merge in race 2 (2023)
  merge(rac2p23_dict, all.x = TRUE) |>
  # Combine rac2 into a single column
  mutate(
    rac2p = ifelse(rac2p19 > 0, rac2p19, rac2p23),
    rac2p_str = ifelse(rac2p19 > 0, rac2p19_str, rac2p23_str)
  ) |>
  # Remove the separate rac2 columns
  select(-matches('rac2p\\d')) |>
  # Merge in race 3
  merge(rac3p_dict) |>
  # Change race dummy columns to logicals
  mutate(across(c(racaian, racasn, racblk, racnh, racpi, racsor, racwht), as.logical)) |>
  # Add Hispanic logical column
  mutate(hisplog = hisp > 1) |>
  # Rearrange columns
  select(serialno, sporder, puma, pwgtp, where(is.numeric), where(is.character), where(is.logical))
  


###### ================================================= ########
# Assign REALD groups

# Make new data frame for assignment
pums_out = pums_str

# Initialize new columns
pums_out[, c(
  ### American Indian and Alaska Native
  "AIANInd", "AIANAlask", "AIANCan", "AIANLat",
  ### Asian groups
  "AsnAfghan", "AsnInd", "AsnCambod", "AsnChinese", "AsnMyan", "AsnFilipino",
  "AsnHmong", "AsnIndones", "AsnJapanese", "AsnKorean", "AsnLao", "AsnPakistani", 
  "AsnSouth", "AsnTaiwanese", "AsnThai", "AsnViet", "AsnOther",
  ### Black and African groups
  "AfrAm", "AfrCarib", "AfrEthiopian", "AfrHaitian", "AfrJamaican", "AfrNigerian",
  "AfrSomali", "AfrOther",
  ### Latin groups
  "LatAfr", "LatCen", "LatCub", "LatDom", "LatGuat", "LatMex", "LatPR", 
  "LatSalv", "LatSou", "LatOther",
  ### Middle Eastern/North African groups
  "MENAEgypt", "MENAIraq", "MENAIran", "MENAIsr", "MENALeban", "MENAPalest", "MENASyr",
  "MENATurkish", "MENAOther",
  ### Pacific Islander groups
  "NHPICham", "NHPIMarshall", "NHPICOFA", "NHPISamoan", "NHPIHawaii", "NHPIFijian", 
  "NHPITongan", "NHPIOther",
  ### White groups
  "WhtEng", "WhtGer", "WhtIre", "WhtItal", "WhtPol", "WhtRom", "WhtRus",
  "WhtSco", "WhtSlav", "WhtUkr", "WhtOther",
  ### Other groups
  "OtherUnspec"
)] = NA


# Begin assignments
pums_out = pums_out |>
  mutate(
    ##### == LATIN GROUPS == #####
    ### - Afro-Latino
    # Identifies as Black and 
    # Latino or identifying with a Latin American (Spain, Mexican, Cent. Am.,
    # South. Am., PR, Cuba, DR; 200-295) or Brazilian (360) ancestry 
    # NOTE: was originally a bug in here where anc1p was checked twice (rather than anc1/2p)
    LatAfr = racblk & (hisplog | anc1p %in% c(200:296, 360)), # | anc2p %in% c(200:296, 360)),
    ### = Hispanic (Central American)
    LatCen =  (hisplog | racsor) & (
      # Ancestry or Hispanic Identifier includes: Costa Rican, Honduran, Nicaraguan, Panamanian, 
      # Belize, "Central American Other"
      if_any(
        c(anc1p_str, anc2p_str, hisp_str), 
        ~ grepl('costa', .)  | grepl('hondur', .) | grepl('nicarag', .) |
          grepl('panama', .) | grepl('beliz', .)  | grepl('other\\scentral\\samerican$', .)
      ) | (
        # or, lists birthplace as Belize, Costa Rica , Honduras, Nicaragua, Panama 
        # AND has "other" detailed Hispanic ID (498)
        # AND ancestry Latin American (250), Hispanic (290), Uncoded (996) or not given (999)
        (grepl('beliz', pobp_str) | grepl('costa', pobp_str) | grepl('hondur', pobp_str) | 
           grepl('nicarag', pobp_str) | grepl('panam', pobp_str)) & 
          (hisp %in% 24) &
          (anc1p %in% c(250:252, 290, 996:999) | anc2p %in% c(250:252, 290))
      )
    ),
    ### - Cuban
    LatCub = (hisplog | racsor) & (
      # Ancestry or specific Hispanic ancestry includes Cuba
      if_any(c(anc1p_str, anc2p_str, hisp_str), ~ grepl('cuba', .)) |
        # or, lists birthplace as Cuba AND has "other" detailed Hispanic ID (24) 
        # and ancestry Latin American (250), Hispanic (290), Uncoded (996) or not given (999)
        (
          grepl('cuba', pobp_str) & (hisp %in% 24) &
            (anc1p %in% c(250:252, 290, 996:999) | anc2p %in% c(250:252, 290))
        )
    ),
    ### - Dominican
    LatDom = (hisplog | racsor) & (
      # Ancestry or specific Hispanic ancestry includes Dominican
      if_any(c(anc1p_str, anc2p_str, hisp_str), ~ grepl('dominican', .)) |
        # or, lists detailed birthplace as DR AND has "other" detailed Hispanic ID (24) 
        # and ancestry Latin American (250), Hispanic (290), Uncoded (996) or not given (999)
        (
          grepl('dominican', pobp_str) & (hisp %in% 24) &
            (anc1p %in% c(250:252, 290, 996:999) | anc2p %in% c(250:252, 290))
        )
    ),
    ### - Guatemalan
    LatGuat = (hisplog | racsor) & (
      # Ancestry or specific Hispanic ancestry includes Guatemala
      if_any(c(anc1p_str, anc2p_str, hisp_str), ~ grepl('guatemal', .)) |
        # or, lists detailed birthplace as Guatemala AND has "other" detailed Hispanic ID (24) 
        # and ancestry Latin American (250), Hispanic (290), Uncoded (996) or not given (999)
        (
          grepl('guatemal', pobp_str) & (hisp %in% 24) &
            (anc1p %in% c(250:252, 290, 996:999) | anc2p %in% c(250:252, 290))
        )
    ),
    ### - Mexican
    LatMex = (hisplog | racsor) & (
      # Ancestry or Hispanic ancestry is mexican or chicana/o
      if_any(c(anc1p_str, anc2p_str, hisp_str), ~ grepl('mex', .) | grepl('chic', .)) |
        # or born in Mexico but listed as Hispanic, other (24) and has 
        # Latin American (250) Hispanic (290) or uncodable/unknown ancestry (996-999)
        (
          grepl('^mex', pobp_str) & (hisp %in% 24) &
            (anc1p %in% c(250:252, 290, 996:999) | anc2p %in% c(250:252, 290))
        )
    ),
    ### - Puerto Rican
    LatPR = (hisplog | racsor) & (
      # Ancestry includes Puerto Rican (261) or Hispanic flag specifies PR
      if_any(c(anc1p_str, anc2p_str, hisp_str), ~ grepl('puerto', .)) |
        # or born in Puerto Rico but listed as Hispanic, other (24) and has 
        # Latin American (250) Hispanic (290) or uncodable/unknown ancestry (996-999) 
        (
          grepl('puerto', pobp_str) & (hisp %in% 24) &
            (anc1p %in% c(250:252, 290, 996:999) | anc2p %in% c(250:252, 290))
        )
    ),
    ### - Salvadorean
    LatSalv = (hisplog | racsor) & (
      # Ancestry includes El Salvador or Hispanic flag specifies El Salvador
      if_any(c(anc1p_str, anc2p_str, hisp_str), ~ grepl('salva', .)) |
        # or born in El Salvador but listed as Hispanic, other (24) and has 
        # Latin American (250) Hispanic (290) or uncodable/unknown ancestry (996-999) 
        (
          grepl('salva', pobp_str) & (hisp %in% 24) &
            (anc1p %in% c(250:252, 290, 996:999) | anc2p %in% c(250:252, 290))
        )
    ),
    ### - South American
    LatSou =  (hisplog | racsor) & (
      # Lists ancestry or Hispanic identity as: Argentinian, Bolivian, Chilean, 
      # Colombian, Ecuadorian, Paraguayan, Peruvian, Uruguayan, Venezuelan, 
      # South American other, Brazilian
      if_any(
        c(anc1p_str, anc2p_str, hisp_str), 
        ~ grepl('argent', .) | grepl('boliv', .) | grepl('chile', .) |
          grepl('colomb', .) | grepl('ecuad', .) | grepl('parag', .) |
          grepl('peruv?', .) | grepl('urugu', .) | grepl('venez', .) | 
          grepl('bra[sz]il', .) | # grepl('(other\\s)?south\\samerican(\\soth)?', .)
          # NOTE: above is a code snippet that includes south american indian
          # I commented out for reconciliation with the prior version which does not include this
          # (note: can also grep the rac3p_str for south american indian)
          grepl('south american$', .)
      ) |
        # or born in South America (360-375) and Hispanic ID other (498) or has
        # Latin American (250) Hispanic (290) or uncodable/unknown ancestry (996-999) 
        (
          pobp %in% 360:374 & (hisp %in% 24) &
            (anc1p %in% c(250:252, 290, 996:999) | anc2p %in% c(250:252, 290))
        )
    ),
    ### = Hispanic (Other)
    # Hispanic flag or race "other" plus Hispanic ancestry AND not captured by the other categories
    # NOTE: original script also had ANCESTR1 double-coded instead of ANCESTR1/2
    LatOther = (hisplog | (racsor & (anc1p %in% c(200:295, 360:370) | anc1p %in% c(200:295, 360:370)))) & 
      !(LatAfr | LatCen | LatCub | LatDom | LatGuat | LatMex | LatPR | LatSalv | LatSou),
    
    ##### == INDIGENOUS GROUPS == #####
    ### - Alaska Native
    # Not born in Canada, AND
    AIANAlask = (racaian | racsor) & !grepl('canad', pobp_str) & (
      # Ancestry, race, or language includes Aleut, Eskimo, Inuit, Tlingit, 
      # Alaska Athabaskan, Athab(/p)ascan, Inupiat, Yupik, or other Alaska Native tribes
      if_any(
        c(anc1p_str, anc2p_str, rac2p_str, rac3p_str, lanp_str), 
        ~ grepl('aleut', .) | grepl('eskim', .) | grepl('inuit', .) | grepl('^alask', .) |
          grepl('tling', .) | grepl('atha[bp]', .) | grepl('inupia', .) |
          grepl("yup\'",  .) | grepl('other\\salaska\\snative', .)
      )
      # NOTE: in new version, can add flag for rac1p == 5 (alaska native alone)
    ),
    ### - Canadian Indian, First Nations, Metis
    # Is not white alone AND is race "other" or First Nations AND 
    AIANCan = (rac1p > 1) & (racaian | racsor) & 
      # born in Canada or having Canadian ancestry AND
      if_any(c(anc1p_str, anc2p_str, pobp_str), ~ grepl('canad', .)) & (
        # Ancestry, detailed race, or language matches northern North American Indigenous IDs
        if_any(
          c(anc1p_str, anc2p_str, rac2p_str, rac3p_str, lanp_str), 
          ~ grepl('aleut', .) | grepl('eskim', .) | grepl('inuit', .) |
            grepl('tling', .) | grepl('atha[bp]', .) | grepl('inupia', .) |
            grepl("yup\'",  .) | grepl('ojib', .)  | grepl('salish', .) |
            grepl('iroqu', .)  | grepl('shosh', .) | grepl('colvil', .) |
            grepl('akota', .) | grepl('siou(x)?', .) | grepl('AIAN', .) |
            grepl('([^(central)|(south)|(mexican)|(latin)|(\\/or)]\\s|^)american indian', .) | 
            grepl('native\\s(north\\s)?samerican', .)
        )
      ),
    ### - Latin American Indian/Indigenous
    AIANLat = (racaian | racsor) & (
      # Have race/language/ancestry matching specific (mex/centr/south) american indian or
      # some specific tribal affiliations
      # NOTE: will probably want to go in here in a future version and add more tribes
      if_any(
        c(anc1p_str, anc2p_str, rac2p_str, rac3p_str, lanp_str),
        # NOTE: original version has a bug that does not catch Mexican (American) Indian
        # ~ grepl('((central)|(south)|(mexican)|(latin))\\s(american\\s)?indian', .) |
        ~ grepl('((central)|(south)|(latin)) american indian', .) |
          # grepl('mexican indian', .) |
          grepl('aztec|inca|maya|mixtec|taino|tarasc|yaqui', .)
      ) | (
        # NOTE: putting this in here because the prior script caught mexican 
        # american indian for race but not for ancestry (lol)
        grepl('mexican american indian', rac2p_str)
      ) | 
        # OR
        # or, listing "tribe not specified" or "other specified Amer. Indian tribe" or listing 'AIAN' as one of many races
        # and listing birth place elsewhere in Americas
        # ((RACED %in% c(361, 399) | grepl('AIAN', raced.char)) & BPL %in% 110:300)
        # ((grepl('specified', rac2p_str) | grepl('([^(\\/or)] |^)american indian', rac3p_str)) & pobp %in% c(72:78, 300:399))
        # NOTE: includes Canada for BPL here (as in original) but this should be fixed...
        # (there are six records in here that are off too, but I don't know how to fix them or why they don't appear in original)
        ((grepl('specified', rac2p_str) | grepl('american indian', rac3p_str)) & pobp %in% c(72:78, 300:399))
    ),
    ### - American Indian
    # Not born elsewhere in India AND not Latin or Alaska Native AND
    AIANInd = (racaian) & !grepl('^india$', pobp_str) & !(AIANLat | AIANAlask) & (
      # Ancestry listed as American Indian
      # ((ANCESTR1 %in% 920 | ANCESTR2 %in% 920)) | 
      # NOTE: there's a rac3 item that gets flagged here but not in the old script...
        if_any(
          c(anc1p_str, anc2p_str, rac2p_str, rac3p_str, lanp_str),
          ~ grepl('([^((central)|(south)|(mexican)|(latin)|(\\/or))]\\s|^)american\\sindian', .) |
            grepl('native american', .) |
            grepl('apache', .) | grepl('blackf', .) | grepl('cherok', .) | grepl('cheyen', .) | 
            grepl('creek', .) | grepl('chickas', .) | grepl('choct', .) | grepl('comanch', .) | 
            grepl('crow', .) | grepl('iroqu', .) | grepl('kiowa', .) | grepl('lumbee', .) | 
            grepl('navaj', .) | grepl('osage', .) | grepl('p(a)?iute', .) | grepl('pima', .) | 
            grepl('pot(t)?[ao]w', .) | grepl('pueblo', .) | grepl('seminol', .) | grepl('siou(x)?', .) | 
            grepl('tling', .) | grepl('tohono', .) | grepl('hopi', .) | grepl('delaw', .) | 
            grepl('salish', .) | grepl('yak[ai]ma', .) | grepl('colvil', .) | grepl('houma', .) | 
            grepl('menom[io]n', .) | grepl('yuma', .) | grepl('trib[ae]', .) | grepl('atha[bp]', .) | 
            grepl('algon', .) | grepl('flathead', .) | grepl('hokan', .) | grepl('mus[kc]og', .) | 
            grepl('penuti', .) | grepl('zuni', .) | grepl('keres', .) | grepl('caddoan', .) | 
            grepl('shosho', .) | grepl('papago', .) | grepl('tanoan', .) | grepl('yaqu', .)
        ) |
        # or, specific lower 48 tribe listed 
        # Apache (302), Blackfoot (303), Cherokee (304), Cheyenne (305), Chickasaw (306),
        # Chippewa (307), Choctaw (308), Comanche (309), Creek (310), Crow (311), Iroquois (312),
        # Kiowa (313), Lumbee (314), Navajo (315), Osage (316), Paiute (317), Pima (318),
        # Potawotomi (319), Pueblo (320), Seminole (321), Shoshone (322), Sioux (323),
        # Tlingit (324), Tohono O'Odham (325), Hopi (328), Delaware (350), 
        # Puget Sound Salish (352), Yakama (353), Colvile (355), Houma (356), Menominee (357),
        # Yuman (358), Other specified tribe (361), Two more tribes (362), AN and AI (398), not specified (363-4, 399)
        # RACED %in% c(302:325, 328, 350, 352:353, 355:358, 361:363, 398:399) |
        # or, language is American Indian (all) (70), Algonquian (72), Salish/Flathead (73), 
        # Athapascan (74), Navajo (75), Penutian-Sahaptin (76), Other Penutian (77), Zuni (78), Yuman (79), 
        # Other Hokan (80), Siouan langs. (81), Muskogean (82), Keres (83), Caddoan (85), Shoshonean/Hopi (86), 
        # Pima/Papago (87), Tanoan (90), American Indian n.s. (93)
        # LANGUAGE %in% c(70, 72:83, 85:87, 90, 93) |
        # or, multiple races listed, including American Indian (AIAN) or tribe and not born in Latin America or India
        # (RACED > 800 & (grepl('AIAN', raced.char) | grepl('[Aa]merican\\s[Ii]ndian', raced.char)) & !(BPL %in% c(110:300, 521)))
          ((grepl('specified', rac2p_str) | grepl('american indian', rac3p_str)) & !(pobp %in% c(210, 300:399)))
      ),
    
    ##### == BLACK GROUPS == #####
    ### - African American
    # Identifies as Black AND
    AfrAm = racblk & (
      # has "Afro-American" or "African American" ancestry, or is born in the 50 states + Guam + Samoa
      (if_any(c(anc1p_str, anc2p_str), ~ grepl('^(afr.+)?american$', .) | grepl('united states|black|afro$', .) | pobp < 67))
    ),
    ### - Afro-Caribbean
    # IDs as Black AND
    AfrCarib = racblk & (
      if_any(
        c(anc1p_str, anc2p_str, pobp_str),
        # NOTE: original script had a mistake that failed to pick up Afro-Cubans
        ~ grepl('puerto', .) | grepl('dominic', .) | grepl('baham', .) | # grepl('cuba', .) |
          grepl('barbad', .) | grepl('beliz', .) | grepl('bermud', .)  | grepl('cayman', .) |
          grepl('west\\sindi[ae]', .) | grepl('trinidad', .) | grepl('tobag', .) | grepl('aruba', .) |
          grepl('maart', .) | grepl('caic', .) | grepl('anguil', .) | grepl('virgin\\sisl', .) |
          grepl('grenad', .) | grepl('lucia', .) | grepl('guadal', .) | grepl('cayen', .) | grepl('guyan', .) 
      ) | grepl('cuba', pobp_str)
      # Ancestry includes: Puerto Rico (261), Cuba (271), Dominican Republic (271),
      # Bahamian (300), Barbadian (301), Belizean (302), Bermudan (303), Cayman Islander (304),
      # Dutch West Indian (310), Aruban (311), St Maarten (312), 
      # Trinidadian/Tobagonian (314), Trinidadian (315), Tobagonian (316), US VI (317),
      # British VI (318), Turks and Caicos (323), Anguilla (324), Dominica Islander (328),
      # Grenadian (329), St Lucia (331), Fr. West Indies (332), Guadaloupe (333), Cayenne (334),
      # West Indian (335), Other West Indian (337). Guyanese (370)
      # NOT including Haiti, Jamaica,
      # ANCESTR1 %in% c(261, 261, 275, 300:304, 310:335, 337, 370) | ANCESTR2 %in% c(261, 261, 275, 300:304, 310:335, 337, 370) |
      #   # or, birth place (detailed) is Puerto Rico (11000) USVI (11500), Bermuda (16010), Belize (21010), Cuba (25000),
      #   # West Indies (26000), Dominican Rep. (26010) West Indies sans. Haiti, Jamaica (remaining: 26040 - 26094), Guyana (30040)
      #   BPLD %in% c(11000:11500, 16010, 21010, 25000, 26000:26010, 26040:26094, 30040)
    ),
    ### - Ethiopian
    # Ancestry, language, or birthplace is Ethiopian, Eritrean, Amharic/Ethiopian language
    AfrEthiopian = racblk & if_any(c(anc1p_str, anc2p_str, pobp_str, lanp_str), ~ grepl('ethio|eritr|amhar|tigrin|oromo', .)),
    ### - Haitian
    # IDs as Black and has Haitian ancestry or birthplace
    # NOTE: future version should include Haitian creole (just add lanp to the c())
    AfrHaitian = racblk & if_any(c(anc1p_str, anc2p_str, pobp_str), ~ grepl('haiti', .)),
    ### - Jamaican
    # IDs as Black and ancestry, language, or birthplace includes Jamaica
    AfrJamaican = racblk & if_any(c(anc1p_str, anc2p_str, pobp_str, lanp), ~ grepl('jamaic', .)),
    ### - Nigerian
    # IDs as Black and ancestry or birthplace includes Nigeria
    AfrNigerian = racblk & if_any(c(anc1p_str, anc2p_str, pobp_str), ~ grepl('nigeria', .)),
    ### - Somalian
    # IDs as Black and ancestry or birthplace includes Somali
    AfrSomali = racblk & if_any(c(anc1p_str, anc2p_str, pobp_str), ~ grepl('somali', .)),
    ### = Other
    # Identifies as Black but isn't classified elsewhere
    AfrOther = racblk & !(AfrAm | AfrCarib | AfrEthiopian | AfrHaitian | AfrJamaican | AfrNigerian | AfrSomali),
    
    ##### == ASIAN GROUPS == #####
    ### - Afghan
    # (not forcing racasn)
    # Ancestry is Afghan OR
    AsnAfghan = if_any(c(anc1p_str, anc2p_str), ~ grepl('afgh', .)) | (
      # Born in Afghanistan and speaks Persian language/dialect
      grepl('afgh', pobp_str) & (
        grepl('persia', lanp_str) | grepl('dari', lanp_str) | grepl('pasht', lanp_str) | grepl('farsi', lanp_str)
      )
    ),
    ### - Asian Indian
    # Identifies as Asian (or identifies as "American Indian" but born in India)
    AsnInd = (racasn | rac1p %in% 8 | (racaian & grepl('^india$', pobp_str))) & (
      # Ancestry includes Asian Indian, Bengali, East Indies, Punjabi, Karnatakan, Assamese, Gujarati
      if_any(
        c(anc1p_str, anc2p_str),
        ~ grepl('asian india|bengal|east indi[ea]|punjab|karna|assam|gujar', .) |
          # NOTE: this is added here ONLY for matching with original script
          # (where Bangladeshi is a subcategory within 'Bengali' ancestry...)
          grepl('banglad', .)
      ) | (
      # OR  born in India and speaking Indian language
        grepl('^india$', pobp_str) & (
          grepl('hindi|urdu|other indo|sanskr|bengal|p[ua]njab|marath|gujara', lanp_str) |
          grepl('bihari|rajasth|oriya|assam|kashmi|kannad|dravid|tel[ue]g|malayal|konkan', lanp_str)
        ) 
      ) |
      # OR specifies "Asian indian" in race
        if_any(c(rac2p_str, rac3p_str), ~ grepl('asian\\sindian($|( alone)|(; [a-z]+[^\\/][$ ]))', .))
    ),
    ### = Cambodian
    # Identifies as Asian/other AND
    AsnCambod = (racasn | rac1p %in% 8) & (
      # ancestry, language, race includes Cambodian or Khmer
      if_any(c(anc1p_str, anc2p_str, rac2p_str, lanp_str), ~ grepl('khmer|cambod', .)) | (
        # Or, born in Cambodia, and have no other reported ancestry, race includes some other Asian
        grepl('cambod', pobp_str) & anc1p %in% 999 & grepl('([^(\\/or)]\\s|^)other\\sasian', rac3p_str)
      )
    ),
    ### = Chinese
    # Asian and
    AsnChinese = (racasn | rac1p %in% 8) & !(rac2p_str %in% 'taiwanese alone') & (
      # NOTE: note Taiwanese exception above
      # Ancestry includes Chinese, Cantonese, Mandarin, Hong Kong (NOT Taiwan)
      if_any(
        c(anc1p_str, anc2p_str, rac2p_str), 
        ~ grepl('chin[ae]|canton|mandar|hong kong', .)
      ) | (
        # Separate flag for rac3p, where some of the labels are unreliable
        grepl('chinese', rac3p_str) & !(rac3p %in% c(65:66, 85:86, 94:95))
      ) | # OR birthplace is China or Hong Kong, 
          # NOTE: might want to correct this to birthplace and a language condition
        (pobp_str %in% c('china', 'hong kong'))
    ),
    ### - Myanmar
    # Identifies as Asian AND
    AsnMyan = (racasn | rac1p %in% 8) & (
      # ancestry, race, or language includes Myanmar, Burma, Lisu, Lolo, Kachin
      if_any(
        c(anc1p_str, anc2p_str, rac2p_str, rac3p_str, lanp_str),
        ~ grepl('myanm', .) | grepl('burm[ae]', .) | grepl('lisu', .) | grepl('lolo', .) | 
          grepl('kachin', .) | grepl('karen', .) | grepl('chin[^ae]', .)
      ) | (
        # Or, born in Myanmar and have no other reported ancestry
        # NOTE: "other asian" could include specific non-laotian groups e.g. Bhutan (see also AsnLao below)
          grepl('myanm', pobp_str) & anc1p %in% 999 & grepl('([^(\\/or)]\\s|^)other\\sasian', rac3p_str)
        )
    ),
    ### - Filipino
    # Asian and
    AsnFilipino = (racasn | rac1p %in% 8) & (
      # with ancestry or race including Filipino
      if_any(
        c(anc1p_str, anc2p_str, rac2p_str, rac3p_str), 
        # (below string is to exclude some rac3p clunkers; rac3p (85, 87, 96)
        ~ grepl('filipino($|( alone)|;( \\w+[^\\/]($|\\w+ )))', .)
        ) # | (
        # OR, born in Philippines or speaking a Filipino language
        # have no other reported ancestry and listing race as other/write-in
        # NOTE: clean this up in the next version so that ancestry 999 applies to both conditions (see commented code below)
        # NOTE: there's something fishy happening with the RACED in the IPUMS version. Old script didn't catch a lot of
        # entries for which rac3p includes 'filipino'...
        # if_any(c(lanp_str, pobp_str), ~ grepl('tagal', .) | grepl('philip', .) | grepl('[il]loc', .) | grepl('[cs]ebua', .)) &
        #   anc1p %in% 999 & grepl('([^(\\/or)]\\s|^)other\\sasian', rac3p_str)
        # Detailed race includes other Asian and
      #   grepl('([^(\\/or)]\\s|^)other\\sasian', rac3p_str) & (
      #     # either speaks a Filipino language OR
      #     grepl('tagal|filip|[il]loc|[cs]ebua', lanp_str) | (
      #       # no ancestry listed and born in Philippines
      #       anc1p %in% 999 & grepl('philip', pobp_str)
      #     )
      #   )
      # )
    ),
    ### - Hmong
    # Asian and
    AsnHmong = (racasn | rac1p %in% 8) & (
      # Hmong ancestry (768) or Mien (656) or Hmong (661) as detailed race or 
      # language is Miao/Hmong (4420) or Iu Mien (4430)
      if_any(
        c(anc1p_str, anc2p_str, lanp_str, rac2p_str, rac3p_str), ~ grepl('hmong|mien|miao', .)
      )
    ),
    ### - Indonesian
    # IDs as Asian or other and
    AsnIndones = (racasn | rac1p %in% 8) & (
      # ancestry or race includes Indonesian
      if_any(c(anc1p_str, anc2p_str, rac2p_str, rac3p_str), ~ grepl('indones', .)) | (
        # OR, born in Indonesia and have no other reported ancestry
        grepl('indones', pobp_str) & anc1p %in% 999 & grepl('([^(\\/or)]\\s|^)other\\sasian', rac3p_str)
      )
    ),
    ### - Japanese
    # IDs as Asian or other race alone and
    AsnJapanese = (racasn | rac1p %in% 8) & (
      # Ancestry includes Japanese or Okinawan
      if_any(c(anc1p_str, anc2p_str, rac2p_str), ~ grepl('japan|okinaw', .)) | (
        # Special flags for rac3p (because some of these labels are not reliable)
        grepl('japan', rac3p_str) & !(rac3p %in% c(54, 66, 88, 94, 97)) 
      ) | (
        # Or, born in Japan (501) and have no other reported ancestry
        # NOTE: same issue as above with birthplace and other specified race/ancestry
        pobp_str %in% 'japan' & anc1p %in% 999 & grepl('japanese; and\\/or asian', rac3p_str)
      )
    ),
    ### - Korean
    # IDs as Asian or other race alone and
    AsnKorean = (racasn | rac1p %in% 8) & (
      # Ancestry or race includes Korea
      if_any(
        c(anc1p_str, anc2p_str, rac2p_str, rac3p_str), 
        # doing this because there's something funny going on with one of the rac3p codes
        ~ grepl('(^|(^| )[^j]\\w+; )korean($| alone|; \\w+\\;($| ))', .)
      ) |
        # or race includes "Asian" and language is Korean
        (grepl('kor', lanp_str) & (grepl('asian', rac2p_str) | grepl('asian', rac3p_str)))
    ),
    ### - Laotian
    # Asian or other race alone and
    AsnLao = (racasn | rac1p %in% 8) & (
      # Ancestry or race include Lao
      if_any(c(anc1p_str, anc2p_str, rac2p_str, rac3p_str), ~ grepl('lao', .)) | (
        # OR speaks Laotian and race includes "other Asian" or "write in"
        # NOTE: "other asian" could include specific non-laotian groups e.g. Thailand...
        grepl('lao', lanp_str) & grepl('([^(\\/or)]\\s|^)other\\sasian', rac3p_str)
      )
    ),
    ### - Pakistani
    # Asian or other race alone and
    AsnPakistani = (racasn | rac1p %in% 8) & (
      # and ancestry or race includes Pakistani
      if_any(c(anc1p_str, anc2p_str, rac2p_str, rac3p_str), ~ grepl('pakis', .)) | 
        # or born in Pakistan (52410) and speaks Urdu (3103)
        (grepl('pakis', pobp_str) & grepl('urdu', lanp_str))
    ),
    ### - South Asian
    # Identifies as Asian and not born in India and:
    AsnSouth = (racasn | rac1p %in% 8) & !(pobp_str %in% 'india') & (
      # has ancestry matching south asian nationalities/ethnic groups
      # NOTE: coding issue in original script means bangladeshi does NOT get flagged in the other script...
      if_any(
        c(anc1p_str, anc2p_str, rac2p_str, rac3p_str, lanp_str),
        # ~ grepl('nepal|maldiv|bhutan|lanka|bangla|tamil|tibet|sing?ha|^shan|sindh|brune', .)
        ~ grepl('nepal|maldiv|bhutan|lanka|tamil|tibet|sing?ha|^shan|sindh|brune', .)
      ) | (
        # or, race reported as "other Asian" or "Asian write-in" and 
        # born in Brunei, Bangladesh, Bhutan, Sri Lanka, Maldives , Nepal
        # (bug in original script)
        grepl('asian indian|([^(\\/or)] |^)other\\sasian', rac3p_str) & 
          grepl('brune|bangla|bhutan|lanka|maldiv|nepal', pobp_str)
      )
      # NOTE: error in original script (missing parentheses, see L445) means there are a lot of records getting
      # wrongly tagged as AsnSouth in the original (not repeated here)
    ),
    ### - Taiwanese
    # Identifies as Asian, and
    AsnTaiwanese = (racasn | rac1p %in% 8) & (
      # Ancestry or race includes Taiwan
      if_any(c(anc1p_str, anc2p_str, rac2p_str, rac3p_str), ~ grepl('^taiwan', .)) |
      # or, was born in Taiwan and speaks Chinese or English
        (pobp_str %in% 'taiwan' & (lanp_str %in% c('chinese', 'mandarin', 'cantonese', 'min nan chinese') | is.na(lanp)))
      # NOTE: in update, should put a flag in to exclude rac2 "chinese, not taiwanese"
      # (allowing English in this picks up a couple of respondents who are Indian...)
      # NOTE: original script does not pick up the "chinese and taiwanese" race, by mistake?
    ),
    ### - Thai
    # Identifies as Asian and
    AsnThai = (racasn | rac1p %in% 8) & (
      # Ancestry, race, language, or birthplace includes Thai/Thailand
      if_any(c(anc1p_str, anc2p_str, pobp_str, lanp_str, rac2p_str, rac3p_str), ~ grepl('thai', .))
    ),
    ### - Vietnamese
    # Asian and
    AsnViet = (racasn | rac1p %in% 8) & (
      # ancestry or race includes vietnamese (excluding the and/or vietnamese rac3 AND the 'vietnamese; other...)
      # NOTE: will want to go back and modify to include Vietnamese; other Asian rac3
      if_any(c(anc1p_str, anc2p_str, rac2p_str, rac3p_str), ~ grepl('viet', .) & !grepl('other', .)) | (
        # or detailed race includes "other asian" and birthplace or language is vietnam(ese)
        # NOTE: IPUMS encoding means that original script did not pick up a (correct) rac3p including vietnam
        # (raced == 944 probably) (maybe trailing semicolon can suppress for now...)
        # NOTE: same issue with 'other asian alone' noted above
        if_any(c(lanp_str, pobp_str), ~ grepl('viet', .)) & grepl('([^(\\/or)]\\s|^)other\\sasian', rac3p_str)
      )
    ),
    ### - Other Asian
    # Asian or
    AsnOther = (racasn | (
      # race is "some other race alone" AND
      rac1p %in% 8 &
        # born in Bangladesh (202), Bhutan (203), Myanmar (205), Cambodia (206), China (207), Hong Kong (208),
        # India (210), Indonesia (211), Japan (215), Laos (223), Malaysia (226), Nepal (229), Pakistan (231),
        # Philippines (233), Singapore (236), Sri Lanka (238), Taiwan (240), Thailand (242), Vietnam (247),
        # Asia (249), South Central Asia n.s. (253) AND
        # NOTE: excluding kazakhstan, kyrgysztan, mongolia, uzbekistan here
        pobp %in% c(202:211, 215, 223, 226, 229:233, 236:238, 240:242, 247, 249:253) & 
        # Language includes India NEC (1340), Hindi (1350), Urdu (1360), Bengali (1380), Punjabi (1420), 
        # Marathi (1440), Gujarathi (1450), Nepalese (1500), Sinhalese (1530), Other Indo-European (1564), 
        # Telegu (1730), Kannada (1737), Malayalam (1750), Tamil (1765), Khmer (1900), Vietnamese (1960),
        # Chinese (1970), Mandarin (2000), Min Nan Chinese (2030), Cantonese (2050), Tibetan (2100), 
        # Iu Mien (2525), Hmong (2535), Japanese (2560), Korean (2575), Malay (2715), Indonesian (2770),
        # Other languages of Asia (2850), Filipino (2910), Tagalog (2920), Cebuano (2950), Ilocano (3150),
        # Other Philippine Languages (3190)
        # NOTE: may want to add some others in here (e.g., Farsi)
        lanp %in% c(1340:1420, 1440:1530, 1564, 1730:3190)
      )
    ) & !(
      # and not in any other category
      AsnAfghan | AsnInd | AsnCambod | AsnChinese | AsnMyan | AsnFilipino | AsnHmong | AsnIndones | 
        AsnJapanese | AsnKorean | AsnLao | AsnPakistani | AsnSouth | AsnThai | AsnViet | AsnTaiwanese
    ),
  
    ##### == PAC ISLANDER GROUPS == #####
    ### - Chamorro
    # Identifies as Pacific Islander or other race alone, and
    NHPICham = (racpi | racnh | rac1p %in% 8) & (
      # Ancestry, language, race, or birthplace includes Guam, Chamorro, N. Mariana Islands
      if_any(
        c(anc1p_str, anc2p_str, lanp_str, rac2p_str, rac3p_str, pobp_str), 
        ~ grepl('chamo', .) | grepl('guam', .) | grepl('mariana', .)
      ) 
    ),
    ### - COFA (Confederated States of Micronesia)
    # IDs as Pacific Islander (or native hawaiian?), and
    NHPICOFA = (racpi | racnh | rac1p %in% 8) & (
      # Race, ancestry, language, birthplace includes Micronesian states and islands
      if_any(
        c(anc1p_str, anc2p_str, lanp_str, rac2p_str, rac3p_str, pobp_str), 
        ~ grepl('micrones', .) | grepl('palau', .) | grepl('kosra', .) | grepl('ponape', .) |
          grepl('chuuk', .) | grepl('yap', .) | grepl('truk', .)
        )
      ),
    ### - Fijian
    # IDs as Pacific Islander, and
    NHPIFijian = (racpi | racnh | rac1p %in% 8) & (
      # Ancestry includes Fijian (841) or detailed race includes Fijian
      if_any(c(anc1p_str, anc2p_str, rac2p_str, rac3p_str), ~ grepl('fiji', .)) | (
        # or, detailed race specifies other PI and born in Fiji
        grepl('fiji', pobp_str) & (
          # grepl('[^(\\/or)](\\snative\\shawaiian\\sand)\\s?other\\spacific', rac3p_str) | 
          #   grepl('^other\\spacific', rac3p_str)
          # NOTE: below is to match up with the original output, but will want to exclude and/or
          grepl('(native hawaiian and )?other pacific island', rac3p_str)
        )
      )
    ),
    ### - Native Hawai'ian
    # NOTE: update to only use racnh (current code is just to match)
    NHPIHawaii = (racnh | racpi | rac1p %in% 8) & (
      # Language or ancestry includes Hawaiian
      if_any(c(anc1p_str, anc2p_str, lanp_str), ~ grepl('hawai', .)) |
        # or race includes native hawaiian (but not the uninformative "and/or" NH) 
        if_any(c(rac2p_str, rac3p_str), ~ grepl('([^(\\/or)] |^)native hawaiian', .))
    ),
    ### - Marshallese
    # IDs as Pacific Islander and
    NHPIMarshall = (racpi | racnh | rac1p %in% 8) & (
      # Ancestry, race, birthplace, or language includes Marshallese
      # NOTE: current version does not check rac2p, which has Marshallese; this is
      # only for reconciliation with old dataset; fix (add in rac2p to grep) upon upgrade
      # if_any(c(anc1p_str, anc2p_str, lanp_str, rac2p_str, rac3p_str, pobp_str), ~ grepl('marshall', .))
      if_any(c(anc1p_str, anc2p_str, lanp_str, rac3p_str, pobp_str), ~ grepl('marshall', .))
    ),
    ### - Samoan
    # NOTE: estimate currently under-estimating (maybe because of RACEPACIS flag)
    # IDs as Pacific Islander AND 
    NHPISamoan = (racpi | racnh | rac1p %in% 8) & (
      # Ancestry, race, birthplace, or language includes Samoa
      if_any(c(anc1p_str, anc2p_str, lanp_str, rac2p_str, rac3p_str, pobp_str), ~ grepl('samoa', .))
    ),
    ### - Tongan
    # IDs as Pacific Islander AND
    NHPITongan = (racpi | racnh | rac1p %in% 8) & (
      # Ancestry, race, birthplace, or language includes Tonga
      if_any(c(anc1p_str, anc2p_str, lanp_str, rac2p_str, rac3p_str, pobp_str), ~ grepl('tonga', .))
    ),
    ### - Other Pacific Islander
    # Pacific Islander or other race alone and language in Chamorro (3220),
    # Marshallese (3270), Chuukese (3350), Samoan (3420), Tongan (3500), Hawaiian
    # (3570) or other eastern malayo-polynesian languages (3600)
    # birthplace in Fiji (508), Marshall Islands (511), Micronesia (512), Tonga (523), or Samoa (527)
    # AND not matching any other PI groups
    NHPIOther = (
      racpi | racnh |
        (rac1p %in% 8 & (lanp %in% 3220:3600 | pobp %in% c(508:512, 523:527))) |
        grepl('([^(\\/or) ]|; |^)(native hawaiian and )?(other )?pacific island', rac3p_str)
    ) &
      !(NHPICham | NHPIMarshall | NHPICOFA | NHPISamoan | NHPIHawaii | NHPITongan | NHPIFijian),
    
    ##### == MIDDLE EASTERN GROUPS == #####
    ### - Egyptian
    MENAEgypt = (
      # Ancestry is Egyptian 
      if_any(c(anc1p_str, anc2p_str), ~ grepl('egypt', .)) | 
        # OR born in Egypt and ancestry is Middle Eastern or Arab/other Arab
        if_any(c(anc1p_str, anc2p_str), ~ grepl('^arab|mideast|egypt', .) & grepl('egypt', pobp_str))
    ),
    ### - Iraqi
    MENAIraq = (
      # Ancestry includes Iraqi 
      if_any(c(anc1p_str, anc2p_str), ~ grepl('iraq', .)) |
        # OR born in Iraq and ancestry is Assyrian, Kurdish, Middle Eastern, Arab
        (
          if_any(c(anc1p_str, anc2p_str), ~ grepl('^arab|mideast|assyri|kurd', .)) &
            grepl('iraq', pobp_str)
        )
    ),
    ### - Iranian
    # Has Iranian ancestry
    MENAIran = if_any(c(anc1p_str, anc2p_str), ~ grepl('iran', .)) | 
      # or was born in Iran and speaks Farsi or Dari
      (grepl('iran', pobp_str) & (grepl('farsi', lanp_str) | grepl('dari', lanp_str))),
    ### - Israeli
    # Has Israeli ancestry
    MENAIsr = if_any(c(anc1p_str, anc2p_str), ~ grepl('israel', .)) | 
      # OR born in Israel/Palestine and speaks Hebrew or Yiddish
      (grepl('israel', pobp_str) & grepl('hebrew|yidd', lanp_str)),
    ### - Lebanese
    # Has Lebanese ancestry
    MENALeban = if_any(c(anc1p_str, anc2p_str), ~ grepl('leban', .)) | (
      # OR born in Lebanon AND 
      grepl('leban', pobp_str) & (
        # either speaks Arabic/Neo-Aramaic or has MidEast/Arab ancestry
        if_any(c(anc1p_str, anc2p_str), ~ grepl('^arab|mideast', .)) | grepl('(arab|arama)ic', lanp_str)
      )
    ),
    ### - Palestinian
    # Has Palestinian ancestry
    MENAPalest = if_any(c(anc1p_str, anc2p_str), ~ grepl('palest', .)) | (
      # OR born in Palestine/Israel AND 
      (grepl('israel', pobp_str) | grepl('palest', pobp_str)) & (
        # either speaks Arabic/Neo-Aramaic or has MidEast/Arab ancestry
        if_any(c(anc1p_str, anc2p_str), ~ grepl('^arab|mideast', .)) | grepl('(arab|arama)ic', lanp_str)
      )
    ),
    ### - Syrian
    # Has Syrian ancestry (429)
    MENASyr = if_any(c(anc1p_str, anc2p_str), ~ grepl('^syria', .)) | (
      # or was born in Syria AND
      grepl('syria', pobp_str) & (
        # has MidEast/Arab, Assyrian, or Kurdish ancestry or speaks Arabic/Neo-Aramaic
        if_any(c(anc1p_str, anc2p_str), ~ grepl('^arab|mideast|assyr|kurd', .)) |
          grepl('(arab|arama)ic', lanp_str)
      )
    ),
    ### - Turkish
    # Has Turkish ancestry or speaks Turkish
    # (Turkish language flag may be too generous alone, but it gets us to the ACS counts)
    MENATurkish = if_any(c(anc1p_str, anc2p_str, lanp_str), ~ grepl('turkis', .)),
    ### - MENA Other
    # NOTE: in next version, should also exclude all other MENA groups
    MENAOther = (
      if_any(
        c(anc1p_str, anc2p_str),
        ~ grepl('alger', .) | grepl('liby', .) | grepl('moroc', .) | grepl('tunis', .) | 
          grepl('north\\safr', .) | grepl('kuwai', .) | grepl('saudi', .) | grepl('yemen', .) | 
          grepl('^oman', .) | grepl('qatar', .) | grepl('jordan', .) | grepl('emirat', .) | 
          grepl('assyr', .) | grepl('chald', .) | grepl('mideast', .) | grepl('arab(ic)?$', .) |
          grepl('kurd', .)
      ) | (
        # OR
        # or, speaks Arabic/Neo-Aramaic (57-58) or Farsi/Dari (29-30) AND
        (
          grepl('arabic', lanp_str) | grepl('aramaic', lanp_str) | grepl('farsi', lanp_str) | 
            grepl('dari', lanp_str) | grepl('kurd', lanp_str)
        ) & (
          # was born in Algeria (60011), Libya (60013), Morocco (60014),
          # Tunisia (60016), Western Sahara (60019), Bahrain (530), Cypress (531),
          # Jordan (535), Kuwait (536), Oman (538), Qatar (539), Saudi Arabia (540),
          # UAE (543), Yemen (544-545), Persian Gulf States n.s. (546), Middle East n.s. (547),
          # Southwest Asia n.s.. (548), Asia Minor n.s. (549), South Asia nec (550)
          grepl('alger', pobp_str) | grepl('liby', pobp_str) | grepl('moroc', pobp_str) | grepl('tunis', pobp_str) |
            grepl('bahra', pobp_str) | grepl('cypr', pobp_str) | grepl('jordan', pobp_str) | grepl('kuwai', pobp_str) |
            grepl('^oman', pobp_str) | grepl('qata', pobp_str) | grepl('saudi', pobp_str) | grepl('emira', pobp_str) |
            grepl('yemen', pobp_str)
        )
      )
    ),
    
    ##### == WHITE GROUPS == #####
    ### - English
    # White AND
    WhtEng = (racwht | (anc1p < 210 & (anc2p < 210 | anc2p %in% 999) & rac1p %in% 8)) & (
      # has English ancestry OR 
      if_any(c(anc1p_str, anc2p_str), ~ grepl('^engl', .)) | (
        # NOTE: bug in original code classified 30-40 records with british+(scots/welsh) as English
        # below code is CORRECT but does not match new code (fix upon upgrade)
        # Ancestry includes British Isles without specifying welsh, scotish/scots irish
      #   if_any(c(anc1p_str, anc2p_str), ~ grepl('^brit', .)) & 
      #     !if_any(c(anc1p_str, anc2p_str), ~ grepl('wels', .) | grepl('^scot', .))
      # ) | (
      # THIS code is incorrect but will match previous output
        (
          grepl('^brit', anc1p_str) | 
            grepl('^brit', anc2p_str) & !if_any(c(anc1p_str, anc2p_str), ~ grepl('wels', .) | grepl('^scot', .))
      ) | (
        # OR speaks English, born in England, and ancestry is non-descript n/w Euro, Euro, or not given
        grepl('engl', pobp_str) & is.na(lanp) & (
          grepl('(^[nw].+|^)euro', anc1p_str) | anc1p %in% c(996, 998:999)
        )
      )
      )
    ),
    ### - German
    # White AND
    WhtGer = (racwht | (anc1p < 210 & (anc2p < 210 | anc2p %in% 999) & rac1p %in% 8)) & (
      # ancestry includes German (not German Russian) or Prussian, OR
      # (NOTE: this does include PA German... probably should remove...)
      if_any(c(anc1p_str, anc2p_str), ~ grepl('german(ic)?$', .) | grepl('pruss', .)) | (
        # was born in Germany (453) and speaks German (2 - includes Swiss German)
        grepl('germa', pobp_str) & grepl('germa', lanp_str)
      )
    ),
    ### - Irish
    # White and 
    WhtIre = (racwht | (anc1p < 210 & (anc2p < 210 | anc2p %in% 999) & rac1p %in% 8)) & (
      # ancestry includes Irish (not scots-irish)
      if_any(c(anc1p_str, anc2p_str), ~ grepl('^iri', .) | grepl('^celt', .)) | (
        # OR or born in Ireland, speaks English or Irish, and no ancestry or non-descript European
        grepl('^ire', pobp_str) & (is.na(lanp) | grepl('^iri', lanp_str)) & (
          grepl('^[nw].+euro', anc1p_str) | grepl('^euro', anc1p_str) | anc1p %in% c(996, 998:999)
        )
      )
    ),
    ### - Italian
    # White, and
    WhtItal = (racwht | (anc1p < 210 & (anc2p < 210 | anc2p %in% 999) & rac1p %in% 8)) & (
      # ancestry includes Italian or Sicilian
      if_any(c(anc1p_str, anc2p_str), ~ grepl('ital', .) | grepl('sicil', .)) | (
        # OR born In Italy and having only Southern European or no ancestry
        grepl('ital', pobp_str) & (grepl('^s.+euro', anc1p_str) | (anc1p %in% c(996, 998:999)))
      )
    ),
    ### - Polish
    # White, and 
    WhtPol = (racwht | (anc1p < 210 & (anc2p < 210 | anc2p %in% 999) & rac1p %in% 8)) & (
      # ancestry includes Polish, OR
      if_any(c(anc1p_str, anc2p_str), ~ grepl('polish', .)) | (
        # born in Poland or speaking Polish and ancestry is nec (East) European  or NA (996, 999)
        if_any(c(lanp_str, pobp_str), ~ grepl('pol(ish|and)', .)) & 
          (grepl('(^e.+|^)euro', anc1p_str) | (anc1p %in% c(996, 998:999)))
      )
    ),
    ### - Romanian
    # White, and 
    WhtRom = (racwht | (anc1p < 210 & (anc2p < 210 | anc2p %in% 999) & rac1p %in% 8)) & (
      # ancestry includes Rom or Romanian OR
      if_any(c(anc1p_str, anc2p_str), ~ grepl('^rom', .)) | (
        # or born in Romania (456) or speaks Rumanian (14) and Eastern/nondescript European or no ancestry (999)
        if_any(c(lanp_str, pobp_str), ~ grepl('^r[ou]m', .)) & 
          (grepl('(^e.+|^)euro', anc1p_str) | (anc1p %in% c(996, 998:999)))
      )
    ),
    ### - Russian
    # White, and
    WhtRus = (racwht | (anc1p < 210 & (anc2p < 210 | anc2p %in% 999) & rac1p %in% 8)) & (
      # includes Russian ancestry, OR
      if_any(c(anc1p_str, anc2p_str), ~ grepl('^russ', .)) | (
        # speaks Russian and
        grepl('^russ', lanp_str) & (
          # has Cossack ancestry (in our recoding it's turkestani only? no cossack)
          if_any(c(anc1p_str, anc2p_str), ~ grepl('^turk.+stan', .)) | (
            # or born in Russia or former USSR and nondescript (E) Euro or no ancestry reported (996-9)
            (
              grepl('rus', pobp_str) | grepl('ussr', pobp_str) | grepl('georgia$', pobp_str) |
                grepl('^azerb', pobp_str) | grepl('moldov', pobp_str) | grepl('^armen', pobp_str) |
                grepl('kazakh', pobp_str) | grepl('kyrg', pobp_str) | grepl('turkmen', pobp_str) |
                grepl('uzbek', pobp_str) | grepl('ukrai', pobp_str) 
            ) & ((grepl('(^e.+|^)euro', anc1p_str) | (anc1p %in% c(996, 998:999))))
            # NOTE: in update, aove line should probably include anc2 for e/european
          )
        )
      )
    ),
    ### - Scottish
    # White, and
    WhtSco = (racwht | (anc1p < 210 & (anc2p < 210 | anc2p %in% 999) & rac1p %in% 8)) & (
      # Ancestry includes Scottish (88) and Scots-Irish (87), 
      # or born in Scotland and having Euro nec (183, 195) or un-reported (996, 999) ancestry and speaking English
      if_any(c(anc1p_str, anc2p_str), ~ grepl('^scot', .)) | (
        grepl('^scot', pobp_str) & is.na(lanp) & ((grepl('(^[nw].+|^)euro', anc1p_str) | (anc1p %in% c(996, 998:999))))
      )
    ),
    ### - Ukrainian
    # White, and
    WhtUkr = (racwht | (anc1p < 210 & (anc2p < 210 | anc2p %in% 999) & rac1p %in% 8)) & (
      # includes Ukrainian ancestry OR
      if_any(c(anc1p_str, anc2p_str), ~ grepl('^ukra', .)) | 
        # speaks Ukrainian and
        grepl('^ukra', lanp_str) & (
          # has Cossack ancestry OR
          if_any(c(anc1p_str, anc2p_str), ~ grepl('^coss', .)) | (
            # born in Ukraine or soviet Union and has Euro/Eastern Euro/no ancestry
            (grepl('^ukra', pobp_str) | grepl('ussr', pobp_str)) & 
            ((grepl('(^e.+|^)euro', anc1p_str) | (anc1p %in% c(996, 998:999))))
          )
        )
    ),
    ### - Slavic
    # White, and
    WhtSlav = (racwht | (anc1p < 210 & (anc2p < 210 | anc2p %in% 999) & rac1p %in% 8)) & (
      if_any(
        # Ancestry includes Bulgarian, Belorussian, Czech, Croatian, Bohemian,
        # Serbian, Macedonian, Bosnian, Slovakian, Slovenian, Yugoslavian, Slav
        c(anc1p_str, anc2p_str),
        ~ grepl('bulga', .) | grepl('belor', .) | grepl('czech', .) | grepl('croat', .) | 
          grepl('^bohe', .) | grepl('serb', .) | grepl('maced', .) | grepl('bosni', .) |
          grepl('slov[ae]', .) | grepl('yugos', .) | grepl('slav', .)
      ) | (
        grepl('czech', lanp_str) | grepl('slov[ea]', lanp_str) | grepl('croat', lanp_str) |
          grepl('serb', lanp_str) | grepl('bosn', lanp_str)
      ) |
        (
          # or, born in Bulgaria, Czechoslovakia, Yugoslavia (+bosnia, serbia, montenegro, croatia) 
          # and un-listed/non-descript ancestry
          (
            grepl('bulga|czech|sl[oa]v|bosn|serb|monten|croat|kosov', pobp_str) &
            ((grepl('(^[es].+|^)euro', anc1p_str) | (anc1p %in% c(996, 998:999))))
          )
        )
    ),
    ### - White, other
    # IDs as white or has *both* ancestries from Europe, non-Hispanic, other
    WhtOther = (racwht | (anc1p < 210 & (anc2p < 210 | anc2p %in% 999) & rac1p %in% 8 & !hisplog)) & !(
      WhtEng | WhtGer | WhtIre | WhtItal | WhtPol | WhtRom | WhtRus | WhtSco | WhtUkr | WhtSlav
    ),
    
    ##### == OTHER == #####
    OtherUnspec = !if_any(
      c(starts_with('MENA'), starts_with('Wht'), starts_with('Asn'), starts_with('Afr'),
        starts_with('Lat'), starts_with('AIAN'), starts_with('NHPI')), ~ .
    ) & (rac1p %in% 8)
  )
  
# ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< #

compare_counts = update_counts()

compare_counts |>
  filter(in.new != in.old) |>
  mutate(count.change = in.new - in.old) |>
  arrange(abs(count.change))


# # Groups that have a discrepancy in counts:
#           reald in.new in.old count.change
# 1       AIANInd   8416   8417           -1
# 2        AsnLao    272    271            1
# 3       AsnMyan    103    102            1
# 4       AIANCan     85     82            3
# 5   AsnFilipino   2186   2189           -3
# 6  AsnTaiwanese    368    364            4
# 7       AIANLat    725    719            6
# 8   AsnJapanese   1707   1720          -13
# 9       AsnViet   1750   1737           13
# 10   AsnChinese   3006   3022          -16
# 11    NHPIOther    433    455          -22
# 12   NHPIHawaii    612    581           31
# 13     AsnOther    845    798           47
# 14     AsnSouth    234    356         -122


# ><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>< #

### Some code for comparing and debugging:
# pums_compare = merge(
#   pums_out |> select(serialno, sporder, AsnChinese),
#   # pums_out |> select(serialno, sporder, AIANCan, AIANLat, AIANInd),
#   orig_reald |> filter(AsnChinese) |> select(CBSERIAL, PERNUM) |> mutate(in.y = TRUE),
#   by.x = c('serialno', 'sporder'), by.y = c('CBSERIAL', 'PERNUM'),
#   all = TRUE
# ) |>
#   mutate(in.y = ifelse(is.na(in.y), FALSE, in.y))
# 
# pums_compare |>
#   filter(AsnChinese + in.y == 1) |>
#   merge(pums_out |> select(serialno, sporder, ends_with('str'))) |> select(-c(hisp_str)) |>
#   arrange(AsnChinese)

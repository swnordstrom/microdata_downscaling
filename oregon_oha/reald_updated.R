# Assigning finer-grained, more detailed REALD classes
# see scripts `reald_reclass.R` and 
# https://github.com/PSU-Population-Research-Center/census_reald/blob/main/oha/ACS_Setup_2v11_RELD_ORandWA.do
# Scott N, August 2025

library(ipumsr)
library(dplyr)
library(tidyr)

rm(list = ls())

# Get PUMS ddi
pums.raw.ddi = read_ipums_ddi('oregon_oha/01_data_inputs/usa_00066.xml')
pums.raw = read_ipums_micro(pums.raw.ddi)

head(pums.raw)
nrow(pums.raw)
names(pums.raw)

# Additional read-in for assigned probability of Jewish ancestry
pums.jet = readRDS(file = 'oregon_oha/01_data_inputs/pums_jet_v03.rds') 

# Check to make sure number of rows matches
nrow(pums.jet) == nrow(pums.raw) # and they do - very good!

pums.jet = pums.jet %>%
  mutate(
    # Format serial numbers to match data frames
    serialno = as.numeric(ifelse(grepl('GQ', serialno), gsub('GQ', '01', serialno), gsub('HU', '00', serialno))),
    # Format Jewish ethnicity groupings for new columns
    # ethngrpi = case_match(
    #   0 ~ 'NotJewish',
    #   1 ~ 'JEthAshk',
    #   2 ~ 'JEthSeph',
    #   3 ~ 'JEthOther'
    JAshkenazi = ethgrpi %in% 1,
    JSephardic = ethgrpi %in% 2,
    JOther     = ethgrpi %in% 3
  ) %>%
  select(-contains('eth')) %>%
  # Rename columns
  rename(
    CBSERIAL = serialno,
    PERNUM   = sporder
  )
  
head(pums.jet)


###### ================================================= ########

# REALD groups

pums.out = pums.raw
# Intialize new columns
pums.out[, c(
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

pums.out = pums.out %>%
  filter(STATEFIP %in% 41) %>%
  # Adding in column to represent detailed race as a characterstring
  mutate(raced.char = as_factor(RACED)) %>%
  mutate(
    
    ##### == LATIN GROUPS == #####
    ### - Afro-Latino
    # Identifies as Black and Latino or identifying with a Latin American (Spain, Mexican, Cent. Am., South. Am., Brazil, PR, Cuba, DR) ancestry (200-295, 360-65)
    LatAfr = (RACBLK > 1) & (HISPAN > 0 | ANCESTR1 %in% c(200:295, 360:365) | ANCESTR1 %in% c(200:295, 360:365)),
    ### = Hispanic (Central American)
    LatCen =  (HISPAN > 0 | RACOTHER > 1) & (
      # Ancestry includes Costa Rican (221), Honduran (223), Nicaraguan (224), Panamanian (225), Belize (302)
      # (NOT Salvadorian or Guatemalan)
      ANCESTR1 %in% c(221, 223:225, 302) | ANCESTR2 %in% c(221, 223:225, 302) |
        # or lists detailed Hispanic ethnicity as Costa Rican (411), Honduran (413), 
        # Nicaraguan (414), Panamanian (415), Central America other (417)
        HISPAND %in% c(411, 413:415, 417) | 
        # or, lists birthplace as Belize (21010), Costa Rica (21020), Honduras (21050),
        # Nicaragua (21060), Panama (21070) AND has "other" detailed Hispanic ID (498) and ancestry
        # Latin American (227), Hispanic (290), Uncoded (996) or not given (999)
        (
          BPLD %in% c(21010:21020, 21050:21070) & HISPAND %in% 498 & 
            (ANCESTR1 %in% c(227, 290, 996:999) | ANCESTR2 %in% c(227, 290))
        )
    ),
    ### - Cuban
    LatCub = (HISPAN > 0 | RACOTHER > 1) &  (
      # Ancestry includes Cuban (271) or Hispanic flag specifies Cuba (3)
      ANCESTR1 %in% 271 | ANCESTR2 %in% 271 | HISPAN %in% 3 |
      # or, lists birthplace as Cuba (250) AND has "other" detailed Hispanic ID (498) 
      # and ancestry Latin American (227), Hispanic (290), Uncoded (996) or not given (999)
      (
        BPL %in% 250 & HISPAND %in% 498 & 
          (ANCESTR1 %in% c(227, 290, 996:999) | ANCESTR2 %in% c(227, 290))
      )
    ),
    ### - Dominican
    LatDom = (HISPAN > 0 | RACOTHER > 1) & (
      # Ancestry includes Dominican (275) or detailed Hispanic flag specifies Dominican (460)
      ANCESTR1 %in% 275 | ANCESTR2 %in% 275 | HISPAND %in% 460 |
        # or, lists detailed birthplace as DR (26010) AND has "other" detailed Hispanic ID (498) 
        # and ancestry Latin American (227), Hispanic (290), Uncoded (996) or not given (999)
        (
          BPLD %in% 26010 & HISPAND %in% 498 & 
            (ANCESTR1 %in% c(227, 290, 996:999) | ANCESTR2 %in% c(227, 290))
        )
    ),
    ### - Guatemalan
    LatGuat = (HISPAN > 0 | RACOTHER > 1) & (
      # Ancestry includes Guatemalan (222) or detailed Hispanic flag specifies Guatemala (412)
      ANCESTR1 %in% 222 | ANCESTR2 %in% 222 | HISPAND %in% 412 |
      # or, lists detaield birthplace as Guatemala (21040) AND has "other" detailed Hispanic ID (498) 
      # and ancestry Latin American (227), Hispanic (290), Uncoded (996) or not given (999)
      (
        BPLD %in% 21040 & HISPAND %in% 498 & 
          (ANCESTR1 %in% c(227, 290, 996:999) | ANCESTR2 %in% c(227, 290))
      )
    ),
    ### - Hispanic (Mexican)
    LatMex = (HISPAN > 0 | RACOTHER > 1) & (
      # Ancestry: Mexican (210), Mexican American (211), Chicano/a (213), Nuevo Mexicano (218)
      (ANCESTR1 %in% c(210:213, 218)) | (ANCESTR2 %in% c(210:213, 218)) |
        # or identifies as Mexican hispanic in HISPAN field
        (HISPAN %in% 1) | 
        # or born in Mexico (200) but listed as Hispanic, other (498) and has 
        # Latin American (227) Hispanic (290) or uncodable/unknown ancestry (996-999)
        (
          BPL %in% 200 & HISPAND %in% 498 & 
            (ANCESTR1 %in% c(227, 290, 996:999) | ANCESTR2 %in% c(227, 290))
        )
    ),
    ### - Puerto Rican
    LatPR = (HISPAN > 0 | RACOTHER > 1) & (
      # Ancestry includes Puerto Rican (261) or Hispanic flag specifies PR (2)
      ANCESTR1 %in% 261 | ANCESTR2 %in% 261 | HISPAN %in% 2 |
        # or born in Puerto Rico (110) but listed as Hispanic, other (498) and has 
        # Latin American (227) Hispanic (290) or uncodable/unknown ancestry (996-999) 
        (
          BPL %in% 110 & HISPAND %in% 498 & 
            (ANCESTR1 %in% c(227, 290, 996:999) | ANCESTR2 %in% c(227, 290))
        )
    ),
    ### - Salvadorean
    LatSalv = (HISPAN > 0 | RACOTHER > 1) & (
      # Ancestry includes El Salvador or Hispanic flag specifies El Salvador
      ANCESTR1 %in% 226 | ANCESTR2 %in% 226 | HISPAND %in% 416 |
        # or born in El Salvador (21030) but listed as Hispanic, other (498) and has 
        # Latin American (227) Hispanic (290) or uncodable/unknown ancestry (996-999) 
        (
          BPLD %in% 21030 & HISPAND %in% 498 & 
            (ANCESTR1 %in% c(227, 290, 996:999) | ANCESTR2 %in% c(227, 290))
        )
        
    ),
    ### - Hispanic (South American)
    LatSou =  (HISPAN > 0 | RACOTHER > 1) & (
      # Lists ancestry as: Argentinian (231), Bolivian (232), Chilean (233), Colombian (234),
      # Ecuadorian (235), Paraguayan (236), Peruvian (237), Uruguayan (238),
      # Venezuelan (239), South American (248), Brazilian (360)
      # South American identifying by ancestry flag,
      ANCESTR1 %in% c(231:248, 360) | ANCESTR2 %in% c(231:248, 360) | HISPAND %in% 420:431 | 
      # or born in South America and Hispanic ID other (498) or has
      # Latin American (227) Hispanic (290) or uncodable/unknown ancestry (996-999) 
      (
        BPL %in% 300 & HISPAND %in% 498 & 
          (ANCESTR1 %in% c(227, 290, 996:999) | ANCESTR2 %in% c(227, 290))
      )
    ),
    ### = Hispanic (Other)
    # Hispanic flag for "other" (4) or Hispanic ancestry and not captured by the other categories
    LatOther = (HISPAN > 0 | (RACOTHER > 1 & (ANCESTR1 %in% c(200:295, 360:370) | ANCESTR1 %in% c(200:295, 360:370)))) & 
      !(LatAfr | LatCen | LatCub | LatDom | LatGuat | LatMex | LatPR | LatSalv | LatSou),
    
    ##### == INDIGENOUS GROUPS == #####
    ### - Alaska Native
    # Not born in Canada, AND
    AIANAlask = (RACAMIND > 1 | RACOTHER > 1) & (BPL != 150) & (
      # Ancestry in Aleut (921), Eskimo (922), Inuit (923) or speaks Aleut/Eskimo (71) or
      # lists detailed race as Tlingit (324), Alaska Athabaskan (370), Aleut (371), Eskimo (372),
      # Inupiat (374), Yupik, or other Alaska Native tribes (379-381)
      # AND not born in Canada
      # NOTE: this leaves out non-specified AIAN people born in Alaska
      ANCESTR1 %in% 921:923 | ANCESTR2 %in% 921:923 | LANGUAGE %in% 71 | RACED %in% c(324, 370:381)
    ),
    ### - Canadian Indian, First Nations, Metis
    # Is not white alone and is race "other" or First Nations and Born in Canada or having Canadian ancestry AND
    AIANCan = (RACE > 1) & (RACAMIND > 1 | RACOTHER > 1) & (BPL %in% 150 | ANCESTR1 %in% 931:935 | ANCESTR2 %in% 931:935) & ( 
      # Ancestry includes American Indian (920), Aleut (921), Eskimo (922), Inuit (923),
      ANCESTR1 %in% 920:923 | ANCESTR2 %in% 920:923 |
        # or speaks Aleut/Eskimo (71), Algonquian (72), Salish/Flathead (73),
        # Athapascan (74), Siouan languages (81), Iroquoian (84), or "Native" (94),
        LANGUAGE %in% c(71:74, 81, 84, 94) | 
        # or lists detailed race as shoshone (322), sioux (323), tlingit (324), puget sound salish (352),
        # colville (355), other american indian tribes (361-364), Alaska native tribes (370-381), AI and AN (398),
        # no tribe specified (399)
        RACED %in% c(322:324, 352, 353, 355, 361:364, 370:399) |
        # or, multiple races listed, including American Indian (AIAN) or tribe
        RACED > 800 & (grepl('AIAN', raced.char) | grepl('[Aa]merican\\s[Ii]ndian', raced.char))
    ),
    ### - Latin American Indian/Indigenous
    AIANLat = (RACAMIND > 1 | RACOTHER > 1) & (
      # Ancestry in Central American Indian (913), South American Indian (914)
      ANCESTR1 %in% 913:914 | ANCESTR2 %in% 913:914 |
        # or detailed race specifying a Latin American tribe
        # Central/Spanish American Indian (329-30), Aztec (340), Inca (341), Maya (342),
        # Mixtec (343), Taino (344), Tarasco (345), South/Mex/ American Indian (359, 360), All other Lat. Am. Indian alone (364)
        RACED %in% c(329:345, 351, 354, 359:360, 364) |
        # or specifying speaking an Latin American indigenous language (e.g., 89: Aztec)
        LANGUAGE %in% c(88:89, 92) |
        # or, listing "tribe not specified" or "other specified Amer. Indian tribe" or listing 'AIAN' as one of many races
        # and listing birth place elsewhere in Americas
        ((RACED %in% c(361, 399) | grepl('AIAN', raced.char)) & BPL %in% 110:300)
    ),
    ### - American Indian
    # Not born elsewhere in India AND not Latin or Alaska Native AND
    AIANInd = (RACAMIND > 1) & !(BPL %in% 521) & !(AIANLat | AIANAlask) & (
      # Ancestry listed as American Indian
      ((ANCESTR1 %in% 920 | ANCESTR2 %in% 920)) | 
        # or, specific lower 48 tribe listed as detailed race
        # Apache (302), Blackfoot (303), Cherokee (304), Cheyenne (305), Chickasaw (306),
        # Chippewa (307), Choctaw (308), Comanche (309), Creek (310), Crow (311), Iroquois (312),
        # Kiowa (313), Lumbee (314), Navajo (315), Osage (316), Paiute (317), Pima (318),
        # Potawotomi (319), Pueblo (320), Seminole (321), Shoshone (322), Sioux (323),
        # Tlingit (324), Tohono O'Odham (325), Hopi (328), Delaware (350), 
        # Puget Sound Salish (352), Yakama (353), Colvile (355), Houma (356), Menominee (357),
        # Yuman (358), Other specified tribe (361), Two more tribes (362), AN and AI (398), not specified (363-4, 399)
        RACED %in% c(302:325, 328, 350, 352:353, 355:358, 361:363, 398:399) |
        # or, language is American Indian (all) (70), Algonquian (72), Salish/Flathead (73), 
        # Athapascan (74), Navajo (75), Penutian-Sahaptin (76), Other Penutian (77), Zuni (78), Yuman (79), 
        # Other Hokan (80), Siouan langs. (81), Muskogean (82), Keres (83), Caddoan (85), Shoshonean/Hopi (86), 
        # Pima/Papago (87), Tanoan (90), American Indian n.s. (93)
        LANGUAGE %in% c(70, 72:83, 85:87, 90, 93) |
        # or, multiple races listed, including American Indian (AIAN) or tribe and not born in Latin America or India
        (RACED > 800 & (grepl('AIAN', raced.char) | grepl('[Aa]merican\\s[Ii]ndian', raced.char)) & !(BPL %in% c(110:300, 521)))
    ),

        
    ##### == BLACK GROUPS == #####
    ### - African American
    # Identifies as Black AND
    AfrAm = (RACBLK > 1) & (
      # has "Afro-American" or "African American" ancestry, or is born in the 50 states + Guam + Samoa
      (ANCESTR1 %in% c(900:902, 940) | ANCESTR2 %in% c(900:902, 940) | BPL < 110)
    ),
    ### - Afro-Caribbean
    # IDs as Black AND
    AfrCarib = RACBLK > 1 & (
      # Ancestry includes: Puerto Rico (261), Cuba (271), Dominican Republic (271),
      # Bahamian (300), Barbadian (301), Belizean (302), Bermudan (303), Cayman Islander (304),
      # Dutch West Indian (310), Aruban (311), St Maarten (312), 
      # Trinidadian/Tobagonian (314), Trinidadian (315), Tobagonian (316), US VI (317),
      # British VI (318), Turks and Caicos (323), Anguilla (324), Dominica Islander (328),
      # Grenadian (329), St Lucia (331), Fr. West Indies (332), Guadaloupe (333), Cayenne (334),
      # West Indian (335), Other West Indian (337). Guyanese (370)
      # NOT including Haiti, Jamaica,
      ANCESTR1 %in% c(261, 261, 275, 300:304, 310:335, 337, 370) | ANCESTR2 %in% c(261, 261, 275, 300:304, 310:335, 337, 370) |
        # or, birth place (detailed) is Puerto Rico (11000) USVI (11500), Bermuda (16010), Belize (21010), Cuba (25000),
        # West Indies (26000), Dominican Rep. (26010) West Indies sans. Haiti, Jamaica (remaining: 26040 - 26094), Guyana (30040)
        BPLD %in% c(11000:11500, 16010, 21010, 25000, 26000:26010, 26040:26094, 30040)
    ),
    ### - Ethiopian
    # Ancestry includes Ethiopian (522) or Eritrean (523) or born in Ethiopia (60044) or Eritrea (60065) or speaks Amharic/Ethiopian language 60
    AfrEthiopian = RACBLK > 1 & (ANCESTR1 %in% 522:523 | ANCESTR2 %in% 522:523 | BPLD %in% c(60044, 60065) | LANGUAGE %in% 60),
    ### - Haitian
    # IDs as Black and has Haitian ancestry (336) or was born in Haiti (26020) 
    # NOTE: missing Creole flag (no distinct Haitian Creole in IPUMS - lumped with French Creole)
    AfrHaitian = RACBLK > 1 & (ANCESTR1 %in% 336 | ANCESTR2 %in% 336 | BPLD %in% 26020),
    ### - Jamaican
    # IDs as Black and has Jamaican ancestry (308) or was born in Jamaica (26030) or speaks Jamaican Creole (110)
    AfrJamaican = RACBLK > 1 & (ANCESTR1 %in% 308 | ANCESTR2 %in% 308 | BPLD %in% 26030 | LANGUAGED %in% 110),
    ### - Nigerian
    # IDs as Black and Ancestry includes Nigeria (553) or born in Nigeria (60031)
    AfrNigerian = RACBLK > 1 & (ANCESTR1 %in% 553 | ANCESTR2 %in% 553 | BPLD %in% 60031),
    ### - Somalian
    # IDs as Black and Ancestry includes Somali
    AfrSomali = RACBLK > 1 & (ANCESTR1 %in% 568 | ANCESTR2 %in% 568 | BPLD %in% 60053),
    ### = Other
    # Identifies as Black but isn't classified elsewhere
    AfrOther = RACBLK > 1 & !(AfrAm | AfrCarib | AfrEthiopian | AfrHaitian | AfrJamaican | AfrNigerian | AfrSomali),
    
    ##### == ASIAN GROUPS == #####
    ### - Afghan
    # Removing the Asian flag here (Afghanistan includes some Persian people who may not ID as Asian)
    AsnAfghan = (
      # Ancestry includes Afghan or, born in Afghanistan and speaks Persian languages
      ANCESTR1 %in% 600 | ANCESTR2 %in% 600 | (BPL %in% 520 & LANGUAGE %in% 29:30)
    ),
    ### - Asian Indian
    # Identifies as Asian (or identifies as "American Indian" but born in India)
    AsnInd = (RACASIAN > 1 | RACE %in% 7 | (RACAMIND > 1 & BPLD %in% 52100)) & (
      # Ancestry includes
      # Asian Indian (615), Bengali (603), East Indies (675), Punjabi (650), 
      # Karnatakan (region of Kannada speakers; 632), Assamese (626), Gujarati (630)
      ANCESTR1 %in% c(615, 603, 675, 650, 632, 626, 630) |
        ANCESTR2 %in% c(615, 603, 675, 650, 632, 626, 630) |
        # # or, born in India (52100) and speaks Hindi (3100-3102), Urdu (3103), Other Indo-Iranian (3104),
        # Sanskrit (3111), Bengali (3112), Panjabi (3113), Marathi (3114), Gujarathi (3115), Bihari (3116),
        # Rajasthani (3117), Oriya (3118), Assamese (3119), Kashmiri (3120), Kannada (3130), India nec (3140),
        # Dravidian (4000), Telegu (4003), Malayalam (4004)
        (BPLD %in% 52100 & LANGUAGED %in% c(3100:3104, 3111:3120, 3130:3140, 4000, 4003:4004)) | 
        # or, detailed race IDed as Asian Indian (610), Sikh (657), Asian Indian + asian write-in (678),
        # white + asian indian (814), black + asian indian (835), AIAN and asian indian (852),
        # asian indian and PI write-in (866), asian indian + other write-in (884), 
        RACED %in% c(610, 657, 678, 814, 835, 852, 866, 884) 
    ),
    ### = Cambodian
    # Identifies as Asian AND
    AsnCambod = (RACASIAN > 1 | RACE %in% 7) & (
      # ancestry includes cambodian (703) or khmer (704),
      # or detailed race is Cambodian (660)
      # or language is Mon-Khmer/Cambodian
      ANCESTR1 %in% 703:704 | ANCESTR2 %in% 703:704 | RACED %in% 660 | LANGUAGED %in% 5120 |
        # Or, born in Cambodia (511) and have no other reported ancestry
        (
          BPL %in% 511 & ANCESTR1 %in% 999 &
           (grepl('[Oo]ther\\s[Aa]sian', raced.char) | grepl('[Aa]sian\\swrite', raced.char))
        )
    ),
    ### = Chinese
    # Asian and
    AsnChinese = (RACASIAN > 1 | RACE %in% 7) & (
      # Ancestry includes Chinese (706), Cantonese (707), Mandarin (709), Hong Kong (716)
      # (NOT Taiwan)
      ANCESTR1 %in% c(706:707, 709, 716) | ANCESTR2 %in% c(706:707, 709, 716) | 
        # or birthplace is China (50000) or Hong Kong (50010), or detailed race includes Chinese (alone or together with others)
        BPLD %in% 50000:50010 | RACED %in% c(400, 420, 673:676, 811, 832, 861:862, 881, 887, 911:912, 964) # |
        # or speaks Chinese (43), has Asian specified in three+ race count with no other Asian ancestry listed
        # (LANGUAGE %in% 43 & grepl('Asian', raced.char) & RACED > 900 & ANCESTR1 %in% c(996, 999))
    ),
    ### - Myanmar
    # Identifies as Asian AND
    AsnMyan = (RACASIAN > 1 | RACE %in% 7) & (
      # ancestry includes Myanmar/Burmese (700) or detailed race includes Myanmar (665)
      # or language is Burmese/Lisu/Lolo (45), Kachin (46)
      ANCESTR1 %in% 700 | ANCESTR2 %in% 700 | LANGUAGE %in% 45:46 | RACED %in% 665 |
        # Or, born in Cambodia (5213) and have no other reported ancestry
        (
          BPLD %in% 52130 & ANCESTR1 %in% 999 &
            (grepl('[Oo]ther\\s[Aa]sian', raced.char) | grepl('[Aa]sian\\swrite', raced.char))
        )
    ),
    ### - Filipino
    # Asian and
    AsnFilipino = (RACASIAN > 1 | RACE %in% 7) & (
      # with ancestry or detailed race including Filipino (ancestry 720)
      ANCESTR1 %in% 720 | ANCESTR2 %in% 720 | 
        # or detailed race includes Filipino (together or in conjunction with others)
        RACED %in% c(600, 674, 677, 813, 834, 851, 862, 864:865, 883, 912, 914, 916:917, 921) | 
        # Or, born in Philippines (515) and have no other reported ancestry
        (
          BPL %in% 515 & ANCESTR1 %in% 999 &
            (grepl('[Oo]ther\\s[Aa]sian', raced.char) | grepl('[Aa]sian\\swrite', raced.char))
        ) |
        # or Tagalog speaker with Asian, other included in race
        ((grepl('Asian\\swrite\\_in', raced.char) | grepl('[Oo]ther\\s[Aa]sian', raced.char)) & LANGUAGE %in% 54)
    ),
    ### - Hmong
    # Asian and
    AsnHmong = (RACASIAN > 1 | RACE %in% 7) & (
      # Hmong ancestry (768) or Mien (656) or Hmong (661) as detailed race or 
      # language is Miao/Hmong (4420) or Iu Mien (4430)
      ANCESTR1 %in% 768 | ANCESTR2 %in% 768 | RACED %in% c(656, 661) | LANGUAGED %in% 4420:4430
    ),
    ### - Indonesian
    # IDs as Asian or other and
    AsnIndones = (RACASIAN > 1 | RACE %in% 7) & (
      # ancestry includes Indonesian (730), detailed race includes Indonesian (666) 
      ANCESTR1 %in% 730 | ANCESTR2 %in% 730 | RACED %in% 666 |
        # Or, born in Indonesia (512) and have no other reported ancestry
        (
          BPL %in% 512 & ANCESTR1 %in% 999 &
            (grepl('[Oo]ther\\s[Aa]sian', raced.char) | grepl('[Aa]sian\\swrite', raced.char))
        )
    ),
    ### - Japanese
    AsnJapanese = (RACASIAN > 1 | RACE %in% 7) & (
      # Asian and has Japanese as ancestry (740 - 749, including Okinawan/Ryuku)
      ANCESTR1 %in% 740:749 | ANCESTR2 %in% 740:749 | 
        # or race is Japanese (alone or with others)
        RACED %in% c(500, 673, 677, 812, 833, 863, 869, 882, 913, 926, 964) |
        # Or, born in Japan (501) and have no other reported ancestry
        (
          BPL %in% 501 & ANCESTR1 %in% 999 &
            (grepl('[Oo]ther\\s[Aa]sian', raced.char) | grepl('[Aa]sian\\swrite', raced.char))
        ) 
    ),
    ### - Korean
    # IDs as Asian and
    AsnKorean = (RACASIAN > 1 | RACE %in% 7) & (
      # ancestry includes Korean (750) or detailed race is Korean (alone or together with others)
      ANCESTR1 %in% 750 | ANCESTR2 %in% 750 | RACED %in% c(620, 815, 836, 869, 887) |
        # or race includes "Asian" and language is Korean
        (grepl('[Aa]sian', raced.char) & LANGUAGE %in% 49)
    ),
    ### - Laotian
    # Asian and not Hmong
    AsnLao = (RACASIAN > 1 | RACE %in% 7) & (
      # Laotian ancestry (765) or detailed race is Laotian (662)
      ANCESTR1 %in% 765 | ANCESTR2 %in% 765 | RACED %in% 662 |
        # or speaks Laotian (4720) but detailed race includes "other Asian" or "write in"
        ((grepl('[Oo]ther\\s[Aa]sian', raced.char) | grepl('[Aa]sian\\swrite', raced.char)) & LANGUAGED %in% 4720)
    ),
    ### - Pakistani
    # Asian and
    AsnPakistani = (RACASIAN > 1 | RACE %in% 7) & (
      # and ancestry includes Pakistani (680) or detailed race includes Pakistani (669),
      # or born in Pakistan (52410) and speaks Urdu (3103)
      ANCESTR1 %in% 680 | ANCESTR2 %in% 680 | RACED %in% 669 | (BPLD %in% 52140 & LANGUAGED %in% 3103)
    ),
    ### - South Asian
    # Identifies as Asian and not born in India and:
    AsnSouth = (RACASIAN > 1 | RACE %in% 7) & !(BPLD %in% 52100) & (
      # Nepali (609), Maldivan (695), Bhutanese (607), Sri Lankan (690),
      # Tibetan (714), Tamil (656), Singhalese (691), Shan (702)
      ANCESTR1 %in% c(609, 695, 607, 690, 714, 656, 691, 702) |
        ANCESTR2 %in% c(609, 695, 607, 690, 714, 656, 691, 702) |
        # or, speaks Sindhi (3121), Maldivan (3122), Sinhalese (3123), Nepali (4011), Tamil (4005), or Tibetan (4400)
        LANGUAGED %in% c(3121:3123, 4005, 4011, 4400) |
        (
          # or, race reported as "other Asian" or "Asian write-in" and 
          # born in Brunei (51000), Bangladesh (52110), Bhutan (52120), Sri Lanka (52150), 
          # Maldives (52300), or Nepal (52400)
          grepl('[Aa]sian\\swrite', raced.char) | grepl('[Oo]ther\\s[Aa]sian', raced.char) & 
          BPLD %in% c(51000, 52110:52120, 52150, 52300:52400)
        ) |
        # or, detailed race listed as Bhutanese (641), Nepalese (643), Bangladeshi (664), Sri Lankan (670)
        RACED %in% c(641, 643, 664, 670)
    ),
    ### - Taiwanese
    # Identifies as Asian, and
    AsnTaiwanese = (RACASIAN > 1 | RACE %in% 7) & (
      # has Taiwanese ancestry (782) or Taiwanese (410) listed as detailed race
      ANCESTR1 %in% 782 | ANCESTR2 %in% 782 | RACED %in% 410 |
        # or, was born in Taiwan (50040) and speaks Chinese (43) (or English, 1) 
        (BPLD %in% 50040 & LANGUAGE %in% c(1, 43))
    ),
    ### - Thai
    # Identifies as Asian and
    AsnThai = (RACASIAN > 1 | RACE %in% 7) & (
      # Ancestry includes Thai (776) or detailed race is Thai (663)       
      # or, born in Thailand or speaks Thai
      ANCESTR1 %in% 776 | ANCESTR2 %in% 776 | RACED %in% 663 | BPL %in% 517 | LANGUAGED %in% 4710
    ),
    ### - Vietnamese
    # Asian and
    AsnViet = (RACASIAN > 1 | RACE %in% 7) & (
      # ancestry includes vietnamese (785) or race includes vietnamese (alone or with other races)
      ANCESTR1 %in% 785 | ANCESTR2 %in% 785 | RACED %in% c(640, 675, 816) |
        # or detailed race includes "other asian" or "asian write-in" and birthplace or language is vietnam(ese)
        ((grepl('[Oo]ther\\sAsian', raced.char) | grepl('[Aa]sian\\swrite', raced.char)) & (BPL %in% 518 | LANGUAGE %in% 50))
    ),
    ### - Other Asian
    # Asian and not caught in any of the other groupings
    AsnOther = (RACASIAN > 1 | (RACE %in% 7 & BPL %in% 500:524 & LANGUAGE %in% c(31, 40:54))) & !(
      AsnAfghan | AsnInd | AsnCambod | AsnChinese | AsnMyan | AsnFilipino | AsnHmong | AsnIndones | 
        AsnJapanese | AsnKorean | AsnLao | AsnPakistani | AsnSouth | AsnThai | AsnViet | AsnTaiwanese
    ),
    
    ##### == PAC ISLANDER GROUPS == #####
    ### - Chamorro
    # Identifies as Pacific Islander, and
    NHPICham = (RACPACIS > 1 | RACE %in% 7) & (
      # Guamanian (821) or Chamorro (822) ancestry 
      ANCESTR1 %in% 821:822 | ANCESTR2 %in% 821:822 | 
        # or born in Guam (10500) or N. Mariana Islands (71047) or speaks Chamorro (5503)
        # or detailed race is Chamorro (685), Guamanian (691), or white+chamorro (823)
        BPLD %in% c(10500, 71047) | LANGUAGED %in% 5503 | RACED %in% c(685, 691, 823)
    ),
    ### - COFA (Confederated States of Micronesia)
    # IDs as Pacific Islander, and
    NHPICOFA = (RACPACIS > 1 | RACE %in% 7) & (
      # Ancestry includes Micronesian (820), Palauan (824). Kosraean (826), Ponapean (827),
      # Chuukese (828), Yap Islander (829)
      ANCESTR1 %in% c(820, 824, 826:829) | ANCESTR2 %in% c(820, 824, 826:829) |
        # or, detailed race listed as Palau (687), Other Micronesian (688), Chuukese (690)
        # or born in Micronesia or speaks Trukese (5511)
        RACED %in% c(687:688, 690) | BPLD %in% 71042 | LANGUAGED %in% 5511
    ),
    ### - Fijian
    # IDs as Pacific Islander, and
    NHPIFijian = (RACPACIS > 1 | RACE %in% 7) & (
      # Ancestry includes Fijian (841) or detailed race includes Fijian
      ANCESTR1 %in% 841 | ANCESTR2 %in% 841 | RACED %in% 695 |
        # or, detailed race specifies PI and born in Fiji (71015)
        ((grepl('PI', raced.char) | grepl('[Pp]acific', raced.char)) & BPLD %in% 71015)
    ),
    ### - Native Hawai'ian
    # NOTE: currently under-counting
    # Identifies as Pacific Islander and
    NHPIHawaii = (RACPACIS > 1 | RACE %in% 7) & (
      # Hawaiian ancestry (811)
      ANCESTR1 %in% 811 | ANCESTR2 %in% 811 | 
        # or detailed race string includes Hawaiian
        RACED %in% c(630, 821, 861:864, 911:914, 926:927, 936, 964) |
        # or speaks Hawaiian
        LANGUAGE %in% 56
    ),
    ### - Marshallese
    # IDs as Pacific Islander and
    NHPIMarshall = (RACPACIS > 1 | RACE %in% 7) & (
      # Ancestry or detailed race includes Marshallese (ancestr 825, raced 692)
      # or, born in Marshall Islands or speaks Marshallese language
      ANCESTR1 %in% 825 | ANCESTR2 %in% 825 | RACED %in% 692 | BPLD %in% 71041 | LANGUAGED %in% 5506
    ),
    ### - Samoan
    # NOTE: estimate currently under-estimating (maybe because of RACEPACIS flag)
    # IDs as Pacific Islander AND 
    NHPISamoan = (RACPACIS > 1 | RACE %in% 7) & (
      # Ancestry includes Samoan (814) or detailed race test includes Samoa (alone: 680, or with white: 822)
      # or birthplace is American Samoa (100) or language is Samoan (5522)
      ANCESTR1 %in% 814 | ANCESTR2 %in% 814 | RACED %in% c(680, 822) | BPL %in% 100 | LANGUAGED %in% 5522
    ),
    ### - Tongan
    # IDs as Pacific Islander AND
    NHPITongan = (RACPACIS > 1 | RACE %in% 7) & (
      # Ancestry includes Tongan (815) or detailed race is Tongan (682) 
      # or birthplace is Tonga (71023) or language is Tongan (5523)
      ANCESTR1 %in% 815 | ANCESTR2 %in% 815 | RACED %in% 682 | BPLD %in% 71023 | LANGUAGED %in% 5523
    ),
    ### - Other Pacific Islander
    # (TO DO: figure out what to grep for pacific islander RACED) AND not any of the other groups
    NHPIOther = (RACPACIS > 1 | (RACE %in% 7 & (LANGUAGE %in% 54:56 | BPL %in% 710)) ) &
      !(NHPICham | NHPIMarshall | NHPICOFA | NHPISamoan | NHPIHawaii | NHPITongan | NHPIFijian),
    
    ##### == MIDDLE EASTERN GROUPS == #####
    ### - Egyptian
    # Ancestry is Egyptian (402) or ancestry is Middle Eastern (490) or Arab (495:496) and born in Egypt (60012)
    MENAEgypt = (
      ANCESTR1 %in% 402 | ANCESTR2 %in% 402 | 
        ((ANCESTR1 %in% 490:496 | ANCESTR2 %in% 490:496) & BPLD %in% 60012)
    ),
    ### - Iraqi
    MENAIraq = (
      # Ancestry includes Iraqi 
      ANCESTR1 %in% 417 | ANCESTR2 %in% 417 | 
        # ancestry is Assyrian (482), Kurdish (442), Middle Eastern (490), Arab (495:496)
        # and born in Iraq (532)
        ((ANCESTR1 %in% c(442, 482, 490:496) | ANCESTR2 %in% c(442, 482, 490:496)) & BPL %in% 532)
    ),
    ### - Iranian
    # Has Iranian ancestry or was born in Iran (416) and speaks Farsi/other Persian (29)
    MENAIran = (ANCESTR1 %in% 416 | ANCESTR2 %in% 416 | (BPL %in% 522 & LANGUAGE %in% 29)),
    ### - Israeli
    # Has Israeli ancestry (419) or born in Israel/Palestine (534) and speaks Hebrew (59) or Yiddish (3)
    MENAIsr = (ANCESTR1 %in% 419 | ANCESTR2 %in% 419 | (BPL %in% 534 & LANGUAGE %in% c(3, 59))),
    ### - Lebanese
    # Has Lebanese ancestry (425) 
    # or born in Lebanon and either speaks Arabic (57) or has MidEast (490) or Arab (495-496) ancestry
    MENALeban = (
      ANCESTR1 %in% 425 | ANCESTR2 %in% 425 | 
        (BPL %in% 537 & (LANGUAGE %in% 57:58 | ANCESTR1 %in% 490:496 | ANCESTR2 %in% 490:496))
    ),
    ### - Palestinian
    # Has Palestinian ancestry (465) 
    # or was born in Palestine/(I) and either speaks Arabic (57-58) or has Mid East(490) or Arab (495-6) ancestry
    MENAPalest = (
      ANCESTR1 %in% 465 | ANCESTR2 %in% 465 | 
        (BPL %in% 534 & (LANGUAGE %in% 57:58 | ANCESTR1 %in% 490:496 | ANCESTR2 %in% 490:496))
    ),
    ### - Syrian
    MENASyr = (
      # Has Syrian ancestry (429)
      ANCESTR1 %in% 429 | ANCESTR2 %in% 429 | 
        # or was born in Syria (541) and has MidEast (490), Arab (495-6), Assyrian (482) or Kurdish (442) ancestry
        ((ANCESTR1 %in% c(442, 482, 490:496) | ANCESTR2 %in% c(442, 482, 490:496)) & BPL %in% 541)
    ),
    ### - Turkish
    # Has Turkish ancestry (434) or was born and speaks Turkish (36)
    # (Turkish language flag may be too generous alone, but it gets us to the ACS counts)
    MENATurkish = (ANCESTR1 %in% 434 | ANCESTR2 %in% 434 | LANGUAGE %in% 36),
    ### - MENA Other
    MENAOther = (
      # Ancestry includes Algerian (400), Libyan (404), Moroccan (406), Tunisian (408), North African (411), Jordanian (421),
      # Kuwaiti (423), Saudi Arabian (427), Yemeni (435), Omani (436), Qatari (439), Kurdish (442),
      # Emirati (480), Syriac/Chaldean (482), Middle Eastern (490), Arab (485), Other Arab (496),
      ANCESTR1 %in% c(400, 404:411, 421:423, 427, 435:442, 470:496) |
        ANCESTR2 %in% c(400, 404:411, 421:423, 427, 435:442, 470:496) |
        # or, speaks Arabic (57-58) or Farsi (29-30) AND was born in Algeria (60011), Libya (60013), Morocco (60014),
        # Tunisia (60016), Western Sahara (60019), Bahrain (530), Cypress (531),
        # Jordan (535), Kuwait (536), Oman (538), Qatar (539), Saudi Arabia (540),
        # UAE (543), Yemen (544-545), Persian Gulf States n.s. (546), Middle East n.s. (547),
        # Southwest Asia n.s.. (548), Asia Minor n.s. (549), South Asia nec (550)
        (
          (BPL %in% c(530:531, 535:536, 538:540, 543:550) | BPLD %in% c(60011, 60013:60014, 60016, 60019)) &
            LANGUAGE %in% c(29:30, 57:58)
        )
    ),
    
    ##### == WHITE GROUPS == #####
    ### - English
    # White, and
    WhtEng = (RACWHT > 1 | (ANCESTR1 < 210 & (ANCESTR2 < 210 | ANCESTR2 %in% 999) & RACE %in% 7)) & (
      # has English ancestry (22)
      ANCESTR1 %in% 22 | ANCESTR2 %in% 22 |
      # or has British Isles (11-12) ancestry without specifying Welsh (97) or Scotish (88) or Scots Irish (87)
        (ANCESTR1 %in% 11:12 | ANCESTR2 %in% 11:12 & !(ANCESTR1 %in% c(87:88, 97) | ANCESTR2 %in% c(87:88, 97))) |
        # or speaks English and was born in England (410) and ancestry is non-descript Euro or not given
        (BPL %in% 410 & LANGUAGE %in% 1 & ANCESTR1 %in% c(183, 187, 195, 996, 999))
    ),
    ### - German
    # White and ancestry includes German (32) or Prussian (40) or was born in Germany (453) and speaks German (2)
    WhtGer = (RACWHT > 1 | (ANCESTR1 < 210 & (ANCESTR2 < 210 | ANCESTR2 %in% 999) & RACE %in% 7)) & (
      ANCESTR1 %in% c(32, 40) | ANCESTR2 %in% c(32, 40) | (BPL %in% 453 & LANGUAGE %in% 2)
    ),
    ### - Irish
    # White and 
    WhtIre = (RACWHT > 1 | (ANCESTR1 < 210 & (ANCESTR2 < 210 | ANCESTR2 %in% 999) & RACE %in% 7)) & (
      # ancestry includes Irish (50) or born in Ireland (414) with no reported Ancestry (996-999) and speaks English/Celtic (1, 15)
      ANCESTR1 %in% 50 | ANCESTR2 %in% 50 | (BPL %in% 414 & LANGUAGE %in% c(1, 15) & ANCESTR1 %in% c(183, 187, 996, 999))
    ),
    ### - Italian
    # White, and ancestry includes Italian (51) or Sicilian (68) or born in Italy (434) and missing ancestry (999)
    WhtItal = (RACWHT > 1 | (ANCESTR1 < 210 & (ANCESTR2 < 210 | ANCESTR2 %in% 999) & RACE %in% 7)) & (
      ANCESTR1 %in% c(51, 68) | ANCESTR2 %in% c(51, 68) | (BPL %in% 434 & ANCESTR1 %in% c(185, 996, 999))
    ),
    ### - Polish
    # White, and ancestry includes Polish (142), 
    # speaks Polish (21) or born in Poland (455) and ancestry is nec European (190, 195) or NA (996-9)
    WhtPol = (RACWHT > 1 | (ANCESTR1 < 210 & (ANCESTR2 < 210 | ANCESTR2 %in% 999) & RACE %in% 7)) & (
      ANCESTR1 %in% 142 | ANCESTR2 %in% 142 | ((BPL %in% 455 | LANGUAGE %in% 21)  & ANCESTR1 %in% c(190, 195, 996, 999))
    ),
    ### - Romanian
    # White, and 
    WhtRom = (RACWHT > 1 | (ANCESTR1 < 210 & (ANCESTR2 < 210 | ANCESTR2 %in% 999) & RACE %in% 7)) & (
      # ancestry includes Rom (124) or Romanian (142)
      ANCESTR1 %in% c(124, 144) | ANCESTR2 %in% c(124, 144) | 
        # or born in Romania (456) or speaks Rumanian (14) and Eastern/nondescript European or no ancestry (999)
        ((BPL %in% 456 | LANGUAGE %in% 14) & (ANCESTR1 %in% c(190, 195, 996, 999)))
      ),
    ### - Russian
    # White, and
    WhtRus = (RACWHT > 1 | (ANCESTR1 < 210 & (ANCESTR2 < 210 | ANCESTR2 %in% 999) & RACE %in% 7)) & (
      # includes Russian (148) ancestry
      ANCESTR1 %in% 148 | ANCESTR2 %in% 148 | 
        # or includes Cossack ancestry (108) and speak Russian (18)
        ((ANCESTR1 %in% 108 | ANCESTR2 %in% 108) & LANGUAGE %in% 18) |
        # or born in Russia/former USSR (465) and speak Russian (18) and nondescript Euro or no ancestry reported (996-9)
        (BPL %in% 465 & LANGUAGE %in% 18 & ANCESTR1 %in% c(190, 195, 996, 999))
    ),
    ### - Scottish
    # White, and
    WhtSco = (RACWHT > 1 | (ANCESTR1 < 210 & (ANCESTR2 < 210 | ANCESTR2 %in% 999) & RACE %in% 7)) & (
      # Ancestry includes Scottish (88) and Scots-Irish (87), 
      # or born in Scotland and having Euro nec (183, 195) or un-reported (996, 999) ancestry and speaking English
      ANCESTR1 %in% 87:88 | ANCESTR2 %in% 87:88 | (BPL %in% 411 & LANGUAGE %in% 1 & ANCESTR1 %in% c(183, 187, 195, 996:999))
    ),
    ### - Ukrainian
    # White, and
    WhtUkr = (RACWHT > 1 | (ANCESTR1 < 210 & (ANCESTR2 < 210 | ANCESTR2 %in% 999) & RACE %in% 7)) & (
      # includes Ukrainian (171) ancestry
      ANCESTR1 %in% 171 | ANCESTR2 %in% 171 | 
        # or includes Cossack ancestry (108) and speak Ukrainian (19)
        ((ANCESTR1 %in% 108 | ANCESTR2 %in% 108) & LANGUAGE %in% 19) |
        # or born in Russia/former USSR (465) and speak Ukrainian (19) and nondescript Euro or no ancestry reported (996-9)
        (BPL %in% 465 & LANGUAGE %in% 19 & ANCESTR1 %in% c(190, 195, 996, 999))
    ),
    ### - Slavic
    # White, and
    WhtSlav = (RACWHT > 1 | (ANCESTR1 < 210 & (ANCESTR2 < 210 | ANCESTR2 %in% 999) & RACE %in% 7)) & (
        # Ancestry includes Bulgarian (103), Belorussian (102), Czech (111), Croatian (109), Bohemian (112),
        # Serbian (152), Macedonian (130), Slovakian (153), Slovenian (154), Yugoslavian (176), 
        # Slav (178), Bohemian (112)
        ANCESTR1 %in% c(102:103, 109:112, 130, 152:154, 172:178) |
          ANCESTR2 %in% c(102:103, 109:112, 130, 152:154, 172:178) |
          # Czech (20), Slovak (22), Serbo-Croat/Slavonian/Yugoslavian (23), Slovene (24)
          LANGUAGE %in% c(20, 22:24) |
          # or, born in Bulgaria (451), Czechoslovakia (452), Yugoslavia (457) and un-listed or non-descript ancestry
          (BPL %in% c(451:452, 457) & ANCESTR1 %in% c(181, 190, 195, 996:999))
    ),
    ### - White, other
    # IDs as white or has *both* ancestries from Europe, non-Hispanic, other
    WhtOther = (RACWHT > 1 | (ANCESTR1 < 210 & (ANCESTR2 < 210 | ANCESTR2 %in% 999) & HISPAN < 1 & RACE %in% 7)) & !(
      WhtEng | WhtGer | WhtIre | WhtItal | WhtPol | WhtRom | WhtRus | WhtSco | WhtUkr | WhtSlav
    ),
    
    ##### == OTHER == #####
    OtherUnspec = !if_any(
      c(starts_with('MENA'), starts_with('Wht'), starts_with('Asn'), starts_with('Afr'),
        starts_with('Lat'), starts_with('AIAN'), starts_with('NHPI')), ~ .
    ) & (RACE %in% 7)
    # OtherUnspec = !if_any(where(is.logical), ~ .) & (RACE %in% 7)
      # (ANCESTR1 %in% 995:999 | (ANCESTR1 %in% 940 & ANCESTR2 %in% 999)) & 
      # BPL < 100 & RACE %in% 7 & LANGUAGE %in% 0:1
  ) %>%
  select(-raced.char)



# some of the grepling on RACED is not helpful (ors instead of ands)

nrow(pums.out)
head(pums.out)

# Okay... let's see what we've got.
pums.out %>%
  select(where(is.logical))%>%
  apply(2, sum)

pums.out %>%
  select(c(PERWT, where(is.logical))) %>%
  # select(c(PERWT, starts_with('NHPI'))) %>%
  mutate(across(where(is.logical), ~ PERWT * as.numeric(.))) %>%
  apply(2, sum) # %>%
  # sort()
# looks plausible at first pass

# How many records are remaining
# pums.out %>% merge(pums.jet) %>% filter(!if_any(where(is.logical), ~ .)) %>% nrow()

# Look at records that are unassigned anywhere
# pums.out %>% filter(!if_any(where(is.logical), ~ .)) %>%
#   select(contains('RAC'), LANGUAGE, LANGUAGED, BPL, ANCESTR1, ANCESTR2) %>%
#   print(n = 100)

# Scott notes

# birth clues? when to count a birthplace, how much other info needs to be missing
#   e.g., born in PR, Hispanic (HISPAND 498), no ancestries listed... LatPR?
# afro-latino: should this be more careful? e.g., does the child of black + mexican US parents count?
# not using a race flag for AIAN  groups... might return to this
# afro-caribbeans and afro-latinos - how much overlap is okay here?s

# double-999s - feel free to assign as needed?
# could use grepl('<><>\\swrite', rac.char) together with birthplace for picking up some extras?
#   e.g., for Cambodia: bpl %in% 511 and grepl('[Aa]sian\\swrite', rac.char) (and double-999s?)
# ah shit... language == 0 for the young'uns!!
# go through at the end and see how many non-white groups have RACE == 1...


###### ================================================= ########

# Do the primary (rarest) race assignment

pums.rarest = pums.out %>% 
  # Merge to get the Jewish groupings
  merge(pums.jet, all = TRUE) %>%
  # Fill in any missing Jewish assignments as FALSE
  mutate(across(starts_with('J'), ~ ifelse(is.na(.), FALSE, .))) %>%
  # add a 'primary' column to store the primary
  mutate(primary = 'unassigned')

re.grp.totals = pums.rarest %>%
  mutate(
    nhpi = if_any(starts_with('NHPI'), ~ .),
    asn  = if_any(starts_with('Asn') , ~ .),
    aian = if_any(starts_with('AIAN'), ~ .),
    wht  = if_any(starts_with('Wht') , ~ .),
    afr  = if_any(starts_with('Afr') , ~ .),
    lat  = if_any(starts_with('Lat'),  ~ .),
    mena = if_any(starts_with('MENA'), ~ .),
    j    = if_any(starts_with('J')   , ~ .)
  ) %>%
  select(PERWT, nhpi, asn, aian, wht, afr, lat, mena, j) %>%
  # Get sums (number of people) identifying as each group
  mutate(across(where(is.logical), ~ PERWT * .)) %>%
  # Remove person weight colum (population total) because it isn't needed
  select(-PERWT) %>%
  apply(2, sum) %>%
  # sort from rarest to most common
  sort()

re.grp.totals

for (i in 1:length(re.grp.totals)) {
  # Iterate over the RE groups
  # (these are already sorted, so we're iterating from smallest to largest)
  
  # Get the name of current race group
  this.re.grp = names(re.grp.totals)[i]
  
  # Get the totals within this race group
  grp.re.totals = pums.rarest %>%
    # filter out individuals without a primary race assigned
    filter(primary %in% 'unassigned') %>%
    # Estimate the number of people counting within each race
    # (usually I hate the ignore.case arg but here it's helpful!)
    select(PERWT, starts_with(this.re.grp, ignore.case = TRUE)) %>%
    # select(-c(matches('[Oo]th'))) %>%
    mutate(across(where(is.logical), ~ PERWT * as.numeric(.))) %>%
    apply(2, sum)
  
  # Now, assign rarest within this group
  while (length(grp.re.totals) > 1) {

    (cur.rarest = names(which.min(grp.re.totals[!names(grp.re.totals) %in% 'PERWT'])))
    print(grp.re.totals[cur.rarest])
    
    pums.rarest$primary = ifelse(
      pums.rarest[,cur.rarest] & pums.rarest$primary %in% 'unassigned',
      cur.rarest,
      pums.rarest$primary
    )
    pums.rarest[, cur.rarest] = NULL
    
    grp.re.totals = pums.rarest %>%
      # filter out individuals without a primary race assigned
      filter(primary %in% 'unassigned') %>%
      # Estimate the number of people counting within each race
      select(PERWT, starts_with(this.re.grp, ignore.case = TRUE)) %>%
      # select(-c(matches('[Oo]th'))) %>%
      mutate(across(where(is.logical), ~ PERWT * as.numeric(.))) %>%
      apply(2, sum)
    
    print(grp.re.totals)
    
  }
  
}

primary.counts = pums.rarest %>% 
  # Get a person count for each *primary* group
  group_by(re = primary) %>% 
  summarise(n.primary = sum(PERWT)) %>%
  mutate(re = ifelse(re %in% 'unassigned', 'OtherUnspec', re))

re.aoic.counts = pums.out %>% 
  # Merge to get the Jewish groupings
  merge(pums.jet, all = TRUE) %>%
  # Fill in any missing Jewish assignments as FALSE
  mutate(across(starts_with('J'), ~ ifelse(is.na(.), FALSE, .))) %>%
  # Get a person count for each group *alone or in combination*
  select(PERWT, where(is.logical)) %>%
  mutate(across(where(is.logical), ~ PERWT * as.numeric(.))) %>%
  # Remove PERWT (total) column
  select(-PERWT) %>%
  apply(2, sum) %>%
  data.frame(
    re = names(.),
    n.aoic = .
  )

counts.combined = merge(primary.counts, re.aoic.counts, all = TRUE) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))

counts.combined %>% 
  mutate(primary.aoic.ratio = n.primary / n.aoic) %>%
  arrange(primary.aoic.ratio) %>%
  mutate(primary.aoic.ratio = round(primary.aoic.ratio, 2))


counts.combined

write.csv(
  counts.combined,
  file = 'oregon_oha/reclassified_RE_counts_2025-09-16_for_OHA.csv',
  row.names = FALSE, na = ''
)

### = Get group totals

# AOIC totals (from the full, all AOIC PUMS merged with Jewish imputes)
aoic.re.grp.totals = pums.out %>% 
  # Merge to get the Jewish groupings
  merge(pums.jet, all = TRUE) %>%
  # Fill in any missing Jewish assignments as FALSE
  mutate(across(starts_with('J'), ~ ifelse(is.na(.), FALSE, .))) %>%
  mutate(
    nhpi = if_any(starts_with('NHPI')),
    asn  = if_any(starts_with('Asn')),
    aian = if_any(starts_with('AIAN')),
    wht  = if_any(starts_with('Wht')),
    afr  = if_any(starts_with('Afr')),
    lat  = if_any(starts_with('Lat')),
    mena = if_any(starts_with('MENA')),
    j    = if_any(starts_with('J')),
    oth  = if_any(starts_with('Oth'))
  ) %>%
  select(PERWT, nhpi, asn, aian, wht, afr, lat, mena, j, oth) %>%
  # Get sums (number of people) identifying as each group
  mutate(across(where(is.logical), ~ PERWT * .)) %>%
  # Remove person weight colum (population total) because it isn't needed
  select(-PERWT) %>%
  apply(2, sum) %>%
  # Convert to data frame
  data.frame(
    grp = names(.),
    n.aoic = .
  )

# Get primary totals
prim.re.grp.totals = primary.counts %>%
  # Condense into groups
  mutate(
    grp = case_when(
      grepl('^NHPI', re) ~ 'nhpi',
      grepl('^Asn',  re) ~ 'asn',
      grepl('^AIAN', re) ~ 'aian',
      grepl('^Wht',  re) ~ 'wht',
      grepl('^Afr',  re) ~ 'afr',
      grepl('^Lat',  re) ~ 'lat',
      grepl('^MENA', re) ~ 'mena',
      grepl('^J',    re) ~ 'j',
      grepl('^Oth',  re) ~ 'oth'
    )
  ) %>%
  group_by(grp) %>% 
  summarise(n.primary = sum(n.primary)) %>%
  ungroup()

combined.grp.totals = merge( prim.re.grp.totals, aoic.re.grp.totals, all = TRUE)

combined.grp.totals

write.csv(
  combined.grp.totals,
  file = 'oregon_oha/reclassified_RE_grp_counts_2025-09-16_for_OHA.csv',
  row.names = FALSE, na = ''
)

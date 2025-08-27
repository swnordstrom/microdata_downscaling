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
pums.jet = readRDS(file = 'oregon_oha/01_data_inputs/pums_jud.rds') 

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
  "WhtSco", "WhtSlav", "WhtUkr", "WhtOther"
)] = NA

pums.out = pums.out %>%
  filter(STATEFIP %in% 41) %>%
  # Adding in column to represent detailed race as a characterstring
  mutate(raced.char = as_factor(RACED)) %>%
  mutate(
    
    ##### == LATIN GROUPS == #####
    ### - Afro-Latino
    # Identifies as Black and Latino (might be too simple, maybe add birth flags?)
    LatAfr = (RACBLK > 1) & (HISPAN > 1),
    ### = Hispanic (Central American)
    LatCen = (
      # Ancestry includes Costa Rican (221), Honduran (223), Nicaraguan (224), Panamanian (225) 
      # (NOT Salvadorian or Guatemalan)
      ANCESTR1 %in% c(221, 223:225) | ANCESTR2 %in% c(221, 223:225) |
        # or lists detailed Hispanic ethnicity as Costa Rican (411), Honduran (413), 
        # Nicaraguan (414), Panamanian (415), Central America other (417)
        HISPAND %in% c(411, 413:415, 417) | 
        # or, lists birthplace as Costa Rica (21010), Honduras (21050), Nicaragua (21060), 
        # Panama (21070) AND has "other" detailed Hispanic ID
        (BPLD %in% c(21020, 21050:21070) & HISPAND %in% 498)
    ),
    ### - Cuban
    # Ancestry includes Cuban (271) or Hispanic flag specifies Cuba (3)
    LatCub = (ANCESTR1 %in% 271 | ANCESTR2 %in% 271 | HISPAN %in% 3),
    ### - Dominican
    # Ancestry includes Dominican (275) or detailed Hispanic flag specifies Dominican (460)
    LatDom = (ANCESTR1 %in% 275 | ANCESTR2 %in% 275 | HISPAND %in% 460),
    ### - Guatemalan
    # Ancestry includes Guatemalan (222) or detailed Hispanic flag specifies Guatemala (412)
    LatGuat = (ANCESTR1 %in% 222 | ANCESTR2 %in% 222 | HISPAND %in% 412),
    ### - Hispanic (Mexican)
    LatMex = (
      # Ancestry: Mexican (210), Mexican American (211), Chicano/a (213), Nuevo Mexicano (218)
      (ANCESTR1 %in% c(210:213, 218)) | (ANCESTR2 %in% c(210:213, 218)) |
        # or identifies as Mexican hispanic in HISPAN field
        # or born in Mexico but listed as Hispanic, other
        (HISPAN %in% 1) | (BPL %in% 200 & HISPAND %in% 498)
    ),
    ### - Puerto Rican
    # Ancestry includes Puerto Rican (261) or Hispanic flag specifies PR (2)
    LatPR = (ANCESTR1 %in% 261 | ANCESTR2 %in% 261 | HISPAN %in% 2),
    ### - Salvadorean
    # Ancestry includes El Salvador or Hispanic flag specifies El Salvador
    LatSalv = (ANCESTR1 %in% 226 | ANCESTR2 %in% 226 | HISPAND %in% 416),
    ### - Hispanic (South American)
    # Lists ancestry as: Argentinian (231), Bolivian (232), Chilean (233), Colombian (234),
    # Ecuadorian (235), Paraguayan (236), Peruvian (237), Uruguayan (238),
    # Venezuelan (239), South American (248), Brazilian (360)
    LatSou = (ANCESTR1 %in% c(231:248, 360)) | (ANCESTR2 %in% c(231:248, 360)) |
      # and South American identifying, or born in South America and Hispanic (other) identifying
      (HISPAND %in% 420:431) | (BPL %in% 300 & HISPAND %in% 498),
    ### = Hispanic (Other)
    # Hispanic flag for "other" and not captured by the other categories
    LatOther = (HISPAN %in% 4) & !(LatAfr | LatCen | LatCub | LatDom | LatGuat | LatMex | LatPR | LatSalv | LatSou),
    
    ##### == INDIGENOUS GROUPS == #####
    ### - Alaska Native
    # Not born in Canada, AND
    AIANAlask = (BPL != 150) & (
      # Ancestry in Aleut (921), Eskimo (922), Inuit (923) or speaks Aleut/Eskimo (71) or
      # lists detailed race as Tlingit (324), Alaska Athabaskan (370), Aleut (371), Eskimo (372),
      # Inupiat (374), Yupik, or other Alaska Native tribes (379-381)
      # AND not born in Canada
      # NOTE: this leaves out non-specified AIAN people born in Alaska
      ANCESTR1 %in% 921:923 | ANCESTR2 %in% 921:923 | LANGUAGE %in% 71 | RACED %in% c(324, 370:381)
    ),
    ### - Canadian Indian, First Nations, Metis
    # Born in Canada or having Canadian ancestry AND
    AIANCan = (BPL %in% 150 | ANCESTR1 %in% 931:935 | ANCESTR2 %in% 931:935) & ( 
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
    AIANLat = (
      # Ancestry in Central American Indian (913), South American Indian (914)
      ANCESTR1 %in% 913:914 | ANCESTR2 %in% 913:914 |
        # or detailed race specifying a Latin American tribe
        # Central/Spanish American Indian (329-30), Aztec (340), Inca (341), Maya (342),
        # Mixtec (343), 
        RACED %in% c(329:345, 351, 354, 359:360, 364) |
        # or specifying speaking an Latin American indigenous language (e.g., 89: Aztec)
        LANGUAGE %in% c(88:89, 92)
    ),
    ### - American Indian
    # Not born elsewhere in the Americas (or India) AND not Latin or Alaska Native AND
    AIANInd = (RACAMIND > 1) & !(BPL %in% c(110:300, 521)) & !(AIANLat | AIANAlask) & (
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
        # or, language is American Indian (all) (70), Aleut /Eskimo (71), Algonquian (72), Salish/Flathead (73), 
        # Athapascan (74), Navajo (75), Penutian-Sahaptin (76), Other Penutian (77), Zuni (78), Yuman (79), 
        # Other Hokan (80), Siouan langs. (81), Muskogean (82), Keres (83), Caddoan (85), Shoshonean/Hopi (86), 
        # Pima/Papago (87), Tanoan (90), American Indian n.s. (93)
        LANGUAGE %in% c(70:83, 85:87, 90, 93) |
        # or, multiple races listed, including American Indian (AIAN) or tribe
        (RACED > 800 & (grepl('AIAN', raced.char) | grepl('[Aa]merican\\s[Ii]ndian', raced.char)))
    ),

        
    ##### == BLACK GROUPS == #####
    ### - African American
    # Identifies as Black AND
    AfrAm = (RACBLK > 1) & (
      # has "Afro-American" or "African American" ancestry, or is born in the 50 states + Guam + Samoa
      ANCESTR1 %in% 920:922 | ANCESTR2 %in% 920:922 | (BPL < 110) |
        # or was not born in/having ancestry from Caribbean, Latin America, African
        !(
          # Birth places: Puerto Rico (110), C. America (210), Cuba (250), West Indies (260), 
          # South America (300), elsewhere in Americas (299), Africa (600)
          BPL %in% c(110, 210, 250, 260, 299, 300, 600) |
            # Ancestry listed specific African (400-401, 500s), Lat Am + Caribbean (221 - 399)
            ANCESTR1 %in% c(221:275, 300:411, 500:599) |
            ANCESTR1 %in% c(221:275, 300:411, 500:599)
        )
    ),
    ### - Afro-Caribbean
    # IDs as Black AND
    AfrCarib = RACBLK > 1 & (
      # Ancestry includes:
      # Bahamian (300), Barbadian (301), Belizean (202), Bermudan (303), Cayman Islander (304),
      # Dutch West Indian (310), Aruban (311), St Maarten (312), 
      # Trinidadian/Tobagonian (314), Trinidadian (315), Tobagonian (316), US VI (317),
      # British VI (318), Turks and Caicos (323), Anguilla (324), Dominica Islander (328),
      # Grenadian (329), St Lucia (331), Fr. West Indies (332), Guadaloupe (333), Cayenne (334),
      # West Indian (335), Other West Indian (337). Guyanese (370)
      # NOT including Haiti, Jamaica, PR, Dominican Republic, Cuba here
      ANCESTR1 %in% c(300:304, 310:335, 337, 370) | ANCESTR2 %in% c(300:304, 310:335, 337, 370) |
        # or, birth place (detailed) is USVI (11500), Bermuda (16010), Belize (21010), 
        # West Indies sans. PR, Haiti, Jamaica (remaining: 26040 - 26094)
        BPLD %in% c(11500, 16010, 26040:26094)
    ),
    ### - Ethiopian
    # Ancestry includes Ethiopian (522) or Eritrean (523) or born in Africa and speaks Amharic/Ethiopian language 60
    AfrEthiopian = RACBLK > 1 & (ANCESTR1 %in% 522:523 | ANCESTR2 %in% 522:523 | (BPL %in% 600 & LANGUAGE %in% 60)),
    ### - Haitian
    # IDs as Black and has Haitian ancestry (336) or was born in Haiti (26020) and speaks French
    AfrHaitian = RACBLK > 1 & (ANCESTR1 %in% 336 | ANCESTR2 %in% 336 | (BPLD %in% 26020 & LANGUAGE %in% 11)),
    ### - Jamaican
    # IDs as Black and has Jamaican ancestry (308) or was born in Jamaica (26030)
    AfrJamaican = RACBLK > 1 & (ANCESTR1 %in% 308 | ANCESTR2 %in% 308 | BPLD %in% 26030),
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
    # Identifies as Asian AND 
    AsnAfghan = (RACASIAN > 1) & (
      # Ancestry includes Afghan or, born in Afghanistan and speaks Persian languages
      ANCESTR1 %in% 600 | ANCESTR2 %in% 600 | (BPL %in% 520 & LANGUAGE %in% 29:30)
    ),
    ### - Asian Indian
    # Identifies as Asian AND
    AsnInd = (RACASIAN > 1) & (
      # Ancestry includes
      # Asian Indian (615), Bengali (603), East Indies (675), Punjabi (650), 
      # Karnatakan (region of Kannada speakers; 632), Assamese (626), Gujarati (630),
      ANCESTR1 %in% c(615, 603, 675, 650, 632, 626, 630) |
        ANCESTR2 %in% c(615, 603, 675, 650, 632, 626, 630) |
        # # or, born in India (521)
        # BPL %in% 521 |
        # or, detailed race IDed as Asian Indian (610), Sikh (657), Asian Indian + asian write-in (678),
        # white + asian indian (814), black + asian indian (835), AIAN and asian indian (852),
        # asian indian and PI write-in (866), asian indian + other write-in (884), 
        RACED %in% c(610, 657, 678, 814, 835, 852, 866) 
    ),
    ### = Cambodian
    # Identifies as Asian AND
    AsnCambod = (RACASIAN > 1) & (
      # ancestry includes cambodian (703) or khmer (704),
      # or detailed race is Cambodian (660)
      # (removed: birthplace is Cambodia)
      ANCESTR1 %in% 703:704 | ANCESTR2 %in% 703:704 | RACED %in% 660
    ),
    ### = Chinese
    # Asian and
    AsnChinese = (RACASIAN > 1) & (
      # Ancestry includes Chinese (706), Cantonese (707), Mandarin (709), Hong Kong (716)
      # (NOT Taiwan)
      ANCESTR1 %in% c(706:707, 709, 716) | ANCESTR2 %in% c(706:707, 709, 716) | 
        # or birthplace is China (50000) or Hong Kong (50010), or detailed race includes Chinese (alone or together with others)
        BPLD %in% 50000:50010 | RACED %in% c(400, 420, 673:676, 811, 832, 861:862, 881, 887, 911:912, 964) |
        # or speaks Chinese (43), has Asian specified in three+ race count with no other Asian ancestry listed
        (LANGUAGE %in% 43 & grepl('Asian', as_factor(RACED)) & RACED > 900 & !(ANCESTR1 %in% 700:799 | ANCESTR2 %in% 700:799))
    ),
    ### - Myanmar
    # Identifies as Asian AND
    AsnMyan = (RACASIAN > 1) & (
      # ancestry includes Myanmar/Burmese (700) or detailed race includes Myanmar (665)
      # or language is Burmese/Lisu/Lolo (45), Kachin (46)
      ANCESTR1 %in% 700 | ANCESTR2 %in% 700 | LANGUAGE %in% 45:46 | RACED %in% 665
    ),
    ### - Filipino
    # Asian and
    AsnFilipino = (RACASIAN > 1) & (
      # with ancestry or detailed race including Filipino (ancestry 720)
      ANCESTR1 %in% 720 | ANCESTR2 %in% 720 | 
        # or detailed race includes Filipino (together or in conjunction with others)
        RACED %in% c(600, 674, 677, 813, 834, 851, 862, 864:865, 883, 912, 914, 916:917, 921) | 
        # or Tagalog speaker with Asian, other included in race
        ((grepl('Asian\\swrite\\_in', raced.char) | grepl('[Oo]ther\\s[Aa]sian', raced.char)) & LANGUAGE %in% 54)
    ),
    ### - Hmong
    # Asian and Hmong ancestry (768) or Mien (656) or Hmong (661) as detailed race
    AsnHmong = (RACASIAN > 1) & (ANCESTR1 %in% 768 | ANCESTR2 %in% 768 | RACED %in% c(656, 661)),
    ### - Indonesian
    # IDs as Asian and ancestry includes Indonesian (730), detailed race includes Indonesian (666) 
    AsnIndones = (RACASIAN > 1) & (ANCESTR1 %in% 730 | ANCESTR2 %in% 730 | RACED %in% 666),
    ### - Japanese
    # Asian and has Japanese as ancestry (740 - 749, including Okinawan/Ryuku) or race
    # NOTE: count comes in below ACS table count because a couple thousand people identify as white...
    AsnJapanese = (RACASIAN > 1) & (
      ANCESTR1 %in% 740:749 | ANCESTR2 %in% 740:749 | 
        RACED %in% c(500, 673, 677, 812, 833, 863, 869, 882, 913, 926, 964)
    ),
    ### - Korean
    # IDs as Asian and
    AsnKorean = (RACASIAN > 1) & (
      # ancestry includes Korean (750) or detailed race is Korean (alone or together with others)
      ANCESTR1 %in% 750 | ANCESTR2 %in% 750 | RACED %in% c(620, 815, 836, 869, 887)
    ),
    ### - Laotian
    # Asian and not Hmong
    AsnLao = (RACASIAN > 1) & !AsnHmong & (
      # Laotian ancestry (765) or detailed race is Laotian (662)
      ANCESTR1 %in% 765 | ANCESTR2 %in% 765 | RACED %in% 662 |
        # or speaks Laotian (4720) but detailed race includes "other Asian" or "write in"
        ((grepl('[Oo]ther\\s[Aa]sian', raced.char) | grepl('[Aa]sian\\swrite', raced.char)) & LANGUAGED %in% 4720)
    ),
    ### - Pakistani
    # Asian and
    AsnPakistani = (RACASIAN > 1) & (
      # and ancestry includes Pakistani (680) or detailed race includes Pakistani (669),
      # or born in Pakistan and speaks Urdu
      ANCESTR1 %in% 680 | ANCESTR2 %in% 680 | RACED %in% 669 | (BPLD %in% 52140 & LANGUAGED %in% 3103)
    ),
    ### - South Asian
    # Identifies as Asian and not born in Indian and:
    AsnSouth = (RACASIAN > 1) & !(BPL %in% 521) & (
      # Nepali (609), Maldivan (695), Bhutanese (607), Sri Lankan (690),
      # Tibetan (714), Tamil (656), Singhalese (691), Shan (702)
      ANCESTR1 %in% c(609, 695, 607, 690, 730, 714, 656, 691, 702) |
        ANCESTR2 %in% c(609, 695, 607, 690, 730, 714, 656, 691, 702) |
        # or, speaks Tibetan (44)
        LANGUAGE %in% 44 | 
        # or, race reported as "other Asian" or "Asian write-in" and 
        # born in Brunei (510), Malaysia (514), Singapore (516), SE Asia nec (519), Maldives (523), Nepal (524)
        (grepl('[Aa]sian\\swrite', raced.char) | grepl('[Oo]ther\\s[Aa]sian', raced.char)) & BPL %in% c(510, 514, 516, 519, 523:524) |
        # or, detailed race listed as Bhutanese (641), Nepalese (643), Bangladeshi (664), 
        # Indonesian (666), Pakistani (669), Sri Lankan (670)
        RACED %in% c(641, 643, 664, 669:670)
    ),
    ### - Taiwanese
    # Identifies as Asian, and
    AsnTaiwanese = (RACASIAN > 1) & (
      # has Taiwanese ancestry (782) or Taiwanese (410) listed as detailed race
      ANCESTR1 %in% 782 | ANCESTR2 %in% 782 | RACED %in% 410 |
        # or, was born in Taiwan and speaks Chinese (43) (or English, 1) 
        (BPLD %in% 50040 & LANGUAGE %in% c(1, 43))
    ),
    ### - Thai
    # Identifies as Asian and
    AsnThai = (RACASIAN > 1) & (
      # Ancestry includes Thai (776) or detailed race is Thai (663)
      ANCESTR1 %in% 776 | ANCESTR2 %in% 776 | RACED %in% 663 |
        # or, born in Thailand and speaks Thai
        (BPL %in% 517 | LANGUAGED %in% 4710)
    ),
    ### - Vietnamese
    # Asian and
    AsnViet = (RACASIAN > 1) & (
      # ancestry includes vietnamese (785) or race includes vietnamese (alone or with other races)
      ANCESTR1 %in% 785 | ANCESTR2 %in% 785 | RACED %in% c(640, 675, 816) |
        # or detailed race includes "other asian" or "asian write-in" and birthplace or language is vietnam(ese)
        (grepl('[Oo]ther\\sAsian', raced.char) | grepl('[Aa]sian\\swrite', raced.char)) & (BPL %in% 518 | LANGUAGE %in% 50)
    ),
    ### - Other Asian
    # Asian and not caught in any of the other groupings
    AsnOther = (RACASIAN > 1) & !(
      AsnAfghan | AsnInd | AsnCambod | AsnChinese | AsnMyan | AsnFilipino | AsnHmong | AsnIndones | 
        AsnJapanese | AsnKorean | AsnLao | AsnPakistani | AsnSouth | AsnThai | AsnViet | AsnTaiwanese
    ),
    
    ##### == PAC ISLANDER GROUPS == #####
    ### - Chamorro
    # Identifies as Pacific Islander, and
    NHPICham = (RACPACIS > 1) & (
      # Guamanian (821) or Chamorro (822) ancestry 
      ANCESTR1 %in% 821:822 | ANCESTR2 %in% 821:822 | 
        # or born in Guam (10500) or N. Mariana Islands (71047) and speaks Chamorro (5503)
        # or detailed race is Guamanian (685), Guamanian (691), or white+chamorro (823)
        # NOTE: still under-estimating here... could get a couple hundred more people if changing the *and* to *or* for language+bpl
        (BPLD %in% c(10500, 71047) & LANGUAGED %in% 5503) | (RACED %in% c(685, 691, 823))
    ),
    ### - COFA (Confederated States of Micronesia)
    # IDs as Pacific Islander, and
    NHPICOFA = (RACPACIS > 1) & (
      # Ancestry includes Micronesian (820), Palauan (824). Kosraean (826), Ponapean (827),
      # Chuukese (828), Yap Islander (829)
      ANCESTR1 %in% c(820, 824, 826:829) | ANCESTR2 %in% c(820, 824, 826:829) |
        # or, detailed race listed as Palau (687), Other Micronesian (688), Chuukese (690)
        # or born in Micronesia and speaks Trukese (5511) or other Malayan Language (5300)
        RACED %in% c(687:688, 690) | (BPLD %in% 71042 & LANGUAGED %in% c(5300, 5511))
    ),
    ### - Fijian
    # IDs as Pacific Islander, and
    NHPIFijian = (RACPACIS > 1) & (
      # Ancestry includes Fijian (841) or detailed race includes Fijian
      ANCESTR1 %in% 841 | ANCESTR2 %in% 841 | RACED %in% 695 |
        # or, detailed race specifies PI and born in Fiji (71015)
        ((grepl('PI', raced.char) | grepl('[Pp]acific', raced.char)) & BPLD %in% 71015)
    ),
    ### - Native Hawai'ian
    # NOTE: currently under-counting
    # Identifies as Pacific Islander and
    NHPIHawaii = (RACPACIS > 1) & (
      # Hawaiian ancestry (811) or detailed race string includes Hawaiian AND IDs as Pacific Islander
      ANCESTR1 %in% 811 | ANCESTR2 %in% 811 | RACED %in% c(630, 821, 861:864, 911:914, 926:927, 936, 964)
    ),
    ### - Marshallese
    # IDs as Pacific Islander and
    NHPIMarshall = (RACPACIS > 1) & (
      # Ancestry or detailed race inclues Marshallese (ancestr 825, raced 692)
      ANCESTR1 %in% 825 | ANCESTR2 %in% 825 | RACED %in% 692 |
        # or, born in Marshall Islands and speaks Marshallese language
        (BPLD %in% 71041 & LANGUAGED %in% 5506)
    ),
    ### - Samoan
    # NOTE: estimate currently under-estimating (maybe because of RACEPACIS flag)
    # IDs as Pacific Islander AND 
    NHPISamoan = (RACPACIS > 1) & (
      # Ancestry includes Samoan (814) or detailed race test includes Samoa (alone: 680, or with white: 822)
      # or birthplace is American Samoa (100) or language is Samoan (5522)
      ANCESTR1 %in% 814 | ANCESTR2 %in% 814 | RACED %in% c(680, 822) | BPL %in% 100 | LANGUAGED %in% 5522
    ),
    ### - Tongan
    # IDs as Pacific Islander AND
    NHPITongan = (RACPACIS > 1) & (
      # Ancestry includes Tongan (815) or detailed race is Tongan (682) 
      # or birthplace is Tonga (71023) or language is Tongan (5523)
      ANCESTR1 %in% 815 | ANCESTR2 %in% 815 | RACED %in% 682 | BPLD %in% 71023 | LANGUAGED %in% 5523
    ),
    ### - Other Pacific Islander
    # (TO DO: figure out what to grep for pacific islander RACED) AND not any of the other groups
    NHPIOther = (RACPACIS > 1) & (grepl('[Pp]acific', raced.char) | grepl('PI', raced.char)) &
      !(NHPICham | NHPIMarshall | NHPICOFA | NHPISamoan | NHPIHawaii | NHPITongan | NHPIFijian),
    
    ##### == MIDDLE EASTERN GROUPS == #####
    ### - Egyptian
    # Ancestry is Egyptian (402) or race is Middle Eastern (490) or Arab (495:496) and born in Egypt (60012)
    MENAEgypt = (ANCESTR1 %in% 402 | ANCESTR2 %in% 402 | (RACED %in% 490:496 & BPLD %in% 60012)),
    ### - Iraqi
    MENAIraq = (
      # Ancestry includes Iraqi 
      ANCESTR1 %in% 417 | ANCESTR2 %in% 417 | 
        # or detailed race is Middle Eastern (490), Arab (495:496) or ancestry is Assyrian (482) or Kurdish (442)
        # and born in Iraq
        ((ANCESTR1 %in% c(442, 482) | ANCESTR2 %in% c(442, 482) | RACED %in% 490:496) & BPL %in% 532)
    ),
    ### - Iranian
    # Has Iranian ancestry or was born in Iran (416) and speaks Farsi/other Persian (29)
    MENAIran = (ANCESTR1 %in% 416 | ANCESTR2 %in% 416 | (BPL %in% 522 & LANGUAGE %in% 29)),
    ### - Israeli
    # Has Israeli ancestry (419) or born in Israel/Palestine (534) and speaks Hebrew (59) or Yiddish (3)
    MENAIsr = (ANCESTR1 %in% 419 | ANCESTR2 %in% 419 | (BPL %in% 534 & LANGUAGE %in% c(3, 59))),
    ### - Lebanese
    # Has Lebanese ancestry (425) 
    # or born in Lebanon and either speaks Arabic (57) or has MidEast (490) or Arab (495-496) as detailed race
    MENALeban = (ANCESTR1 %in% 425 | ANCESTR2 %in% 425 | (BPL %in% 537 & (LANGUAGE %in% 57:58 | RACED %in% 490:496))),
    ### - Palestinian
    # Has Palestinian ancestry (465) 
    # or was born in Palestine/(I) and either speaks Arabic (57) or has Mid East(490) or Arab (495-6) for detailed race
    MENAPalest = (ANCESTR1 %in% 465 | ANCESTR2 %in% 465 | (BPL %in% 534 & (LANGUAGE %in% 57:58 | RACED %in% 490:496))),
    ### - Syrian
    MENASyr = (
      # Has Syrian ancestry (429)
      ANCESTR1 %in% 429 | ANCESTR2 %in% 429 | 
        # or was born in Syria (541) and has MidEast (490) or Arab (495-6) race or Assyrian (482) or Kurdish (442) ancestry
        ((ANCESTR1 %in% c(442, 482) | ANCESTR2 %in% c(442, 482) | RACED %in% 490:496) & BPL %in% 532)
    ),
    ### - Turkish
    # Has Turkish ancestry (434) or was born in Turkey (542) and speaks Turkish (36)
    MENATurkish = (ANCESTR1 %in% 434 | ANCESTR2 %in% 434 | (BPL %in% 542 & LANGUAGED %in% 36)),
    ### - MENA Other
    MENAOther = (
      # Ancestry includes Algerian (400), Libyan (404), Moroccan (406), North African (411), Jordanian (421), Kuwaiti (423),
      # Saudi Arabian (427), Yemeni (435), Omani (436), Qatari (439), Kurdish (442), 
      # Emirati (480), Syriac/Chaldean (482), Middle Eastern (490), Arab (485), Other Arab (496),
      ANCESTR1 %in% c(400, 404:411, 421:423, 427, 435:442, 470:496) |
        ANCESTR2 %in% c(400, 404:411, 421:423, 427, 435:442, 470:496) |
        # or, speaks Arabic (57-58) or Farsi (29-30) AND was born in Algeria (60010), Libya (60013), Morocco (60014),
        # Tunisia (60016), Western Sahara (60019), Bahrain (530), Cypress (531),
        # Jordan (535), Kuwait (536), Oman (538), Qatar (539), Saudi Arabia (540),
        # UAE (543), Yemen (544-545), Persian Gulf States n.s. (546), Middle East n.s. (547),
        # Southwest Asia n.s.. (548), Asia Minor n.s. (549), South Asia nec (550)
        (
          (BPL %in% c(530:531, 535:536, 538:540, 543:550) | BPLD %in% c(60010, 60013:60014, 60016, 60019)) &
            LANGUAGE %in% c(29:30, 57:58)
        )
    ),
    
    ##### == WHITE GROUPS == #####
    ### - English
    # White, and
    WhtEng = (RACWHT > 1) & (
      # has English ancestry (22)
      ANCESTR1 %in% 22 | ANCESTR2 %in% 22 |
      # or has British Isles (11-12) ancestry without specifying Welsh (97) or Scotish (88) or Scots Irish (87)
        (ANCESTR1 %in% 11:12 | ANCESTR2 %in% 11:12 & !(ANCESTR1 %in% c(87:88, 97) | ANCESTR2 %in% c(87:88, 97))) |
        # or speaks English and was born in England
        (BPL %in% 410 & LANGUAGE %in% 1)
    ),
    ### - German
    # White and ancestry includes German (32) or Prussian (40) or was born in Germany (453) and speaks German (2)
    WhtGer = (RACWHT > 1) & (ANCESTR1 %in% c(32, 40) | ANCESTR2 %in% c(32, 40) | (BPL %in% 453 & LANGUAGE %in% 2)),
    ### - Irish
    # White and 
    WhtIre = (RACWHT > 1) & (
      # ancestry includes Irish (50) or born in Ireland (414) with no reported Ancestry (996-999) and speaks English/Celtic (1, 15)
      ANCESTR1 %in% 50 | ANCESTR2 %in% 50 | (BPL %in% 453 & LANGUAGE %in% c(1, 15) & ANCESTR1 %in% c(996, 999))
    ),
    ### - Italian
    # White, and ancestry includes Italian (51) or Sicilian (68) or born in Italy and missing ancestry (999)
    WhtItal = (RACWHT > 1) & (ANCESTR1 %in% c(51, 68) | ANCESTR2 %in% c(51, 68) | (BPL %in% 434 & ANCESTR1 %in% c(996, 999))),
    ### - Polish
    # White, and ancestry includes Polish (142) or born in Poland (454) and ancestry is nec European (190, 195) or NA (996-9)
    WhtPol = (RACWHT > 1) & (ANCESTR1 %in% 142 | ANCESTR2 %in% 142 | (BPL %in% 454 & ANCESTR1 %in% c(190, 194, 996, 999))),
    ### - Romanian
    # White, and 
    WhtRom = (RACWHT > 1) & (
      # ancestry includes Rom (124) or Romanian (142)
      ANCESTR1 %in% c(124, 142) | ANCESTR2 %in% c(124, 142) | 
        # or born in Romania (456) and no ancestry (999) or speaks Rumanian (14)
        (BPL %in% 456 & (ANCESTR1 %in% c(996, 999) | LANGUAGE %in% 14))
      ),
    ### - Russian
    # White, and
    WhtRus = (RACWHT > 1) & (
      # includes Russian (148) ancestry
      ANCESTR1 %in% 148 | ANCESTR2 %in% 148 | 
        # or includes Cossack ancestry (108) and speak Russian (18)
        ((ANCESTR1 %in% 108 | ANCESTR2 %in% 108) & LANGUAGE %in% 18) |
        # or born in Russia/former USSR (465) and speak Russian (18) and no ancestry reported (996-9)
        (BPL %in% 465 & LANGUAGE %in% 18 & ANCESTR1 %in% c(996, 999))
    ),
    ### - Scottish
    # White, and
    WhtSco = (RACWHT > 1) & (
      # Ancestry includes Scottish (88) and Scots-Irish (87), 
      # or born in Scotland and having Euro nec (183, 195) or un-reported (996, 999) ancestry and speaking English
      ANCESTR1 %in% 87:88 | ANCESTR2 %in% 87:88 | (BPL %in% 411 & LANGUAGE %in% 1 & ANCESTR1 %in% c(190, 195, 996:999))
    ),
    ### - Ukrainian
    # White, and
    WhtUkr = (RACWHT > 1) & (
      # includes Ukrainian (171) ancestry
      ANCESTR1 %in% 171 | ANCESTR2 %in% 171 | 
        # or includes Cossack ancestry (108) and speak Ukrainian (19)
        ((ANCESTR1 %in% 108 | ANCESTR2 %in% 108) & LANGUAGE %in% 19) |
        # or born in Russia/former USSR (465) and speak Ukrainian (19) and no ancestry reported (996-9)
        (BPL %in% 465 & LANGUAGE %in% 19 & ANCESTR1 %in% c(996, 999))
    ),
    ### - Slavic
    # White, and
    WhtSlav = (RACWHT > 1) & (
        # Ancestry includes Bulgarian (103), Belorussian (102), Czech (111), Croatian (109),
        # Serbian (152), Macedonian (130), Slovakian (153), Slovenian (154), Yugoslavian (176), 
        # Slav (178), Bohemian (112)
        ANCESTR1 %in% c(102:103, 109:112, 130, 152:154, 172:178) |
          ANCESTR2 %in% c(102:103, 109:112, 130, 152:154, 172:178) |
          # Czech (20), Slovak (22), Serbo-Croat/Slavonian/Yugoslavian (23)
          LANGUAGE %in% c(20, 22:23) |
          # or, born in Bulgaria (451), Czechoslovakia (452), Yugoslavia (457) and un-listed ancestry
          (BPL %in% c(451:452, 457) & ANCESTR1 %in% 996:999)
    ),
    ### - White, other
    WhtOther = (RACWHT > 1) & !(
      WhtEng | WhtGer | WhtIre | WhtItal | WhtPol | WhtRom | WhtRus | WhtSco | WhtUkr | WhtSlav
    )
  ) %>%
  # Merge to get Jewish identities above
  merge(pums.jet)

# some of the grepling on RACED is not helpful (ors instead of ands)


# Okay... let's see what we've got.
pums.out %>%
  select(where(is.logical))%>%
  apply(2, sum)

ck = pums.out %>%
  select(c(PERWT, where(is.logical))) %>%
  mutate(across(where(is.logical), ~ PERWT * as.numeric(.))) %>%
  apply(2, sum)

sort(ck)
# looks plausible at first pass

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

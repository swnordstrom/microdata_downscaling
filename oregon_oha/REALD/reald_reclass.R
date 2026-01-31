# Re-writing the original REALD-assignment script in R
# see script: 
# https://github.com/PSU-Population-Research-Center/census_reald/blob/main/oha/ACS_Setup_2v11_RELD_ORandWA.do
# Some modifications and/or improvements made.
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

###### ================================================= ########


# REALD groups

pums.out = pums.raw
# Intialize new columns
pums.out[, c(
  "HisMex", "HisCen", "HisSou", "HisOth",
  "Cham", "Marshallese", "COFA", "Samoan", "Hawaiian", "NHPIoth",
  "EastEur", "Slavic", "WestEur", "WhiteOth",
  "AmInd", "AlaskNat", "FirstNat", "LatInd",
  "AfrAm", "Caribbean", "Ethiopian", "Somali", "African",
  "MidEast", "NoAfr",
  "AsianInd", "Cambodian", "Chinese", "Myanmar", "Filipino",
  "Hmong", "Japanese", "Korean", "Laotian", "SoAsian", "Vietnamese", "AsianOther"
)] = NA

pums.out = pums.out %>%
  filter(STATEFIP %in% 41) %>%
  mutate(
    ##### == HISPANIC GROUPS == #####
    ### = Hispanic (Mexican)
    # Ancestry: Mexican (210), Mexican American (211), Chicano/a (213), Nuevo Mexicano (218)
    HisMex = (ANCESTR1 %in% c(210:213, 218)) | (ANCESTR2 %in% c(210:213, 218)) |
      # or identifies as Mexican hispanic in HISPAN field
      # or born in Mexico but listed as Hispanic, other
      (HISPAN %in% 1) | (BPL %in% 200 & HISPAND %in% 498),
    ### = Hispanic (Central American)
    # Ancestry: Costa Rican (221), Guatemalan (222), Honduran (223), Nicaraguan (224),
    # Panamanian (225), Salvadoran (226)
    HisCen = (ANCESTR1 %in% 221:226) | (ANCESTR2 %in% 221:226) |
    # or listed as Central American Hispanic or 
      # lists Central America (210) as birth place and lists self as Hispanic, Other
      (HISPAND %in% 411:417) | (BPL %in% 210 & HISPAND %in% 498),
    ### = Hispanic (South American)
    # Lists ancestry as: Argentinian (231), Bolivian (232), Chilean (233), Colombian (234),
    # Ecuadorian (235), Paraguayan (236), Peruvian (237), Uruguayan (238),
    # Venezuelan (239), South American (248), Brazilian (360)
    HisSou = (ANCESTR1 %in% c(231:248, 360)) | (ANCESTR2 %in% c(231:248, 360)) |
      # and South American identifying, or born in South America and Hispanic (other) identifying
     (HISPAND %in% 420:431) | (BPL %in% 300 & HISPAND %in% 498),
    ### = Hispanic (Other)
    # Ancestry listed as: Puerto Rican (261), Cuban (271), Domihican (275),
    # 'Other Spanish/Hispanic' (296)
    HisOth = (ANCESTR1 %in% c(261:275, 296) | ANCESTR2 %in% c(261:275, 296)) |
      # or identifying as Cuban, Puerto Rican, Dominican
      # or, born in Cuba, Puerto Rico, Dominican Republic and other Hispanic
    (HISPAND %in% c(200, 300, 460)) | (BPLD %in% c(11000, 25000, 26010) & HISPAND %in% 498),
    ### There are also flags in the original script for matching Black
    ### Portuguese/Spanish speakers but I don't think this is wise
    
    ##### == INDIGENOUS GROUPS == #####
    ### = Latin American Indians
    # Ancestry in Central American Indian (913), South American Indian (914)
    LatInd = ANCESTR1 %in% 913:914 | ANCESTR2 %in% 913:914 |
      # or detailed race specifying a Latin American tribe
      # Central/Spanish American Indian (329-30), Aztec (340), Inca (341), Maya (342),
      # Mixtec (343), 
      RACED %in% c(329:345, 351, 354, 359:360, 364) |
      # or specifying speaking an Latin American indigenous language (e.g., 89: Aztec)
      LANGUAGE %in% c(88:89, 92),
    ### = Alaska Native
    # Ancestry in Aleut (921), Eskimo (922), Inuit (923) or speaks Aleut/Eskimo or
    # lists detailed race as Tlingit (324), Alaska Athabaskan (370), Aleut (371), Eskimo (372),
    # Inupiat (374), Yupik, or other Alaska Native tribes (379-381)
    # AND not born in Canada
    # NOTE: this leaves out non-specified AIAN people born in Alaska
    AlaskNat = (BPL != 150) & (
      ANCESTR1 %in% 921:923 | ANCESTR2 %in% 921:923 | LANGUAGE %in% 71 | RACED %in% c(324, 370:381)
    ),
    ### = American Indian
    # Not born elsewhere in the Americas (or India lol) AND not Latin or Alaska Native AND
    AmInd = !(BPL %in% c(110:300, 521)) & !(LatInd | AlaskNat) & (
      # Ancestry listed as American Indian AND IDs as American Indian
      ((ANCESTR1 %in% 920 | ANCESTR2 %in% 920) & RACAMIND > 1) | 
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
        RACED > 800 & (grepl('AIAN', as_factor(RACED)) | grepl('[Aa]merican\\s[Ii]ndian', as_factor(RACED)))
    ),
    ### = Canadian Indian, First Nations, Metis
    # Born in Canada or having Canadian ancestry AND
    FirstNat = (BPL %in% 150 | ANCESTR1 %in% 931 | ANCESTR2 %in% 931) & ( 
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
        RACED > 800 & (grepl('AIAN', as_factor(RACED)) | grepl('[Aa]merican\\s[Ii]ndian', as_factor(RACED)))
    ),
    
    ##### == BLACK GROUPS == #####
    ### = Somalian
    # Ancestry includes Somali and is Black
    Somali = (ANCESTR1 %in% 568 | ANCESTR2 %in% 568) & RACBLK > 1,
    ### = Ethiopian
    # Ancestry includes Ethiopian (522) or Eritrean (523) or born in Africa and speaks Amharic/Ethiopian language
    Ethiopian = (ANCESTR1 %in% 522:523 | ANCESTR2 %in% 522:523 | (BPL %in% 600 & LANGUAGE %in% 60)) & RACBLK > 1,
    ### = Caribbean
    # IDs as Black AND
    Caribbean = RACBLK > 1 & (
      # Ancestry includes:
      # Bahamian (300), Barbadian (301), Belizean (202), Bermudan (303), Cayman Islander (304),
      # Jamaican (308), Dutch West Indian (310), Aruban (311), St Maarten (312), 
      # Trinidadian/Tobagonian (314), Trinidadian (315), Tobagonian (316), US VI (317),
      # British VI (318), Turks and Caicos (323), Anguilla (324), Dominica Islander (328),
      # Grenadian (329), St Lucia (331), Fr. West Indies (332), Guadaloupe (333), Cayenne (334),
      # West Indian (335), Haitian (336), Other West Indian (337). Guyanese (370)
      # Not in original file, but adding: Puerto Rican (261), Dominican (271), Dominian (275)
      ANCESTR1 %in% c(261, 271, 275, 300:337, 370) | ANCESTR2 %in% c(261, 271, 275, 300:337, 370) |
        # or, birth place in PR (110), USVI (115), Cuba (250), West Indies (260)
        BPL %in% c(110:115, 250:260)
    ),
    ### = African
    # IDs as Black (but not Ethiopian or Somalian)
    African = RACBLK > 1 & !(Somali | Ethiopian | Caribbean) & (
      # Ancestry in
      # Angolan (500), Beninian (501), Botswanan (504), Burundian (506),
      # Cameroonian (508), Cape Verdean (510), Chadian (513), Congolese (515),
      # Congo-Brazzaville (516), Djibouti (519), Equitorial Guinean (520).
      # Gabonese (525), Gambian (527), Ghanian (529), Guinean (530), Guinea Bissau (531),
      # Ivory Coast (532), Kenyan (534), Lesotho (538), Liberian (541), Madagascan (543),
      # Malawian (545), Malian (546), Mozambican (549), Namibian (550), Nigerian (551, 553)
      # Fulani (554), Hausa (555), Ibo (556), Tiv (557), Rwandan (561), Senegalese (564),
      # Sierra Leonian (566), Swaziland (569), South African (570), Zulu (574), Sudanese (576), 
      # Dinka (577), Nuer (578), Fur (579), Tanzanian (582), Tanzanyikan (583), 
      # Zanzibar Islander (584), Togo (586), Ugandan (588), Upper Voltan (589), Zairian (591), 
      # Zambian (592), Zimbabwean (593), African Islands (594), Other Subsaharn African (595),
      # Central (596), East (597), West (598) African, African (599)
      ANCESTR1 %in% c(500:520, 525:566, 569:599) | ANCESTR2 %in% c(500:520, 525:566, 569:599) |
        # or, speaks Hamitic (61), other Afro-Asiatic lang (62), Sub-Saharan African language (63), African, n.s.
        # or, was born in Africa (600)
        LANGUAGE %in% 61:64 | BPL %in% 600
    ),
    ### = African American
    # Identifies as Black AND
    AfrAm = (RACBLK > 1) & (
      # has "Afro-American" or "African American" ancestry, or is born in the 50 states + Guam + Samoa
      ANCESTR1 %in% 920:922 | ANCESTR2 %in% 920:922 | (BPL < 110) |
        # or was not born in/having ancestry from Caribbean, Latin America, African?
        !(
          # Birth places: Puerto Rico (110), C. America (210), Cuba (250), West Indies (260), 
          # South America (300), elsewhere in Americas (299), Africa (600)
          BPL %in% c(110, 210, 250, 260, 299, 300, 600) |
            # Ancestry listed specific African (400-401, 500s), Lat Am + Caribbean (221 - 399)
            ANCESTR1 %in% c(221:275, 300:411, 500:599) |
            ANCESTR1 %in% c(221:275, 300:411, 500:599)
        )
      # NOTE: need to include multiple races in here
      # and should we exclude language flags? e.g., Sub-Saharan Lang. speakers in Wisconsin...
    ),
    ### = Other
    # Identifies as Black but isn't classified elsewhere
    AfrOther = RACBLK > 1 & !(Somali | Ethiopian | Caribbean | AfrAm),
    
    ##### == ASIAN GROUPS == #####
    ### = South Asian
    # Identifies as Asian and not born in Indian and:
    SoAsian = (RACASIAN > 1) & !(BPL %in% 521) & (
      # Nepali (609), Pakistani (680), Maldivan (695), Bhutanese (607), Sri Lankan (690),
      # Indonesian (730), Tibetan (714), Tamil (656), Singhalese (691), Shan (702)
      # NOTE: no ancestry categorization for Bruneian, Bangladeshi
      # NOTE: did not include Uzbek, Afghan, or Tajik
      ANCESTR1 %in% c(609, 680, 695, 607, 690, 730, 714, 656, 691, 702) |
        ANCESTR2 %in% c(609, 680, 695, 607, 690, 730, 714, 656, 691, 702) |
        # or, speaks Tibetan (44) or Indonesian (52)
        LANGUAGE %in% c(44, 52) | 
        # or, born in Brunei (510), Indonesian (512), Malaysian (514), 
        # Singapore (516), Thai (517), SE Asia nec (519), Maldives (523), Nepal (524)
        BPL %in% c(510, 512, 514, 516:517, 519, 523:524) |
        # or, detailed race listed as Bhutanese (641), Nepalese (643), Bangladeshi (664), 
        # Indonesian (666), Pakistani (669), Sri Lankan (670)
        RACED %in% c(641, 643, 664, 666, 669:670)
    ),
    ### = Asian Indian
    # Identifies as Asian AND
    AsianInd = (RACASIAN > 1) & (
      # ancestry includes
      # Asian Indian (615), Bengali (603), East Indies (675), Punjabi (650), 
      # Karnatakan (region of Kannada speakers; 632), Assamese (626), Gujarati (630),
      ANCESTR1 %in% c(615, 603, 675, 650, 632, 626, 630) |
        ANCESTR2 %in% c(615, 603, 675, 650, 632, 626, 630) |
        # or, born in India (521) or detailed race IDed as Asian Indian (610) or Sikh 
        BPL %in% 521 | RACED %in% c(610, 657)
    ),
    ### = Myanmar
    # Identifies as Asian AND
    Myanmar = (RACASIAN > 1) & (
      # ancestry includes Myanmar/Burmese (700) or detailed race includes Myanmar (665)
      # or language is Burmese/Lisu/Lolo (45), Kachin (46)
      ANCESTR1 %in% 700 | ANCESTR2 %in% 700 | LANGUAGE %in% 45:46 | RACED %in% 665
    ),
    ### = Cambodian
    # Identifies as Asian AND
    Cambodian = (RACASIAN > 1) & (
      # ancestry includes cambodian (703) or khmer (704), or birthplace is cambodia (511)
      # or detailed race is Cambodian
      ANCESTR1 %in% 703:704 | ANCESTR2 %in% 703:704 | BPL %in% 511 | RACED %in% 660
    ),
    ### = Chinese
    # Asian and
    Chinese = (RACASIAN > 1) & (
      # Ancestry includes Chinese (706), Cantonese (707), Mandarin (709), Hong Kong (716), Taiwanese (782),
      ANCESTR1 %in% c(706:707, 709, 716, 782) | ANCESTR2 %in% c(706:707, 709, 716, 782) | 
        # or birthplace is China, or detailed race includes Chinese (entered manually...)
        BPL %in% 500 | RACED %in% c(400, 420, 673:676, 811, 832, 861:862, 881, 887, 911:912, 964) |
        # or speaks Chinese (43), has Asian specified in three+ race count with no other Asian ancestry listed
        (LANGUAGE %in% 43 & grepl('Asian', as_factor(RACED)) & RACED > 900 & !(ANCESTR1 %in% 700:799 | ANCESTR2 %in% 700:799))
    ),
    ### = Hmong
    # Asian and Hmong ancestry (768) or Mien (656) or Hmong (661) as detailed race
    Hmong = (RACASIAN > 1) & (ANCESTR1 %in% 768 | ANCESTR2 %in% 768 | RACED %in% c(656, 661)),
    ### = Laotian
    # Asian and Laotian ancestry (765) or born in Laos (513) or detailed race is Laotian (662)
    Laotian = (RACASIAN > 1) & !Hmong & (ANCESTR1 %in% 765 | ANCESTR2 %in% 765 | BPL %in% 513 | RACED %in% 662),
    ### = Korean
    # Asian and ancestry includes Korean (750) or race includes Korean (not including language)
    Korean = (RACASIAN > 1) & (
      # ancestry includes Korean (750) or detailed race is Korean (alone or together with...)
      ANCESTR1 %in% 750 | ANCESTR2 %in% 750 | RACED %in% c(620, 815, 836, 869, 887)
    ),
    ### = Japanese
    # Asian and has Japanese as ancestry or race
    # NOTE: count comes in below ACS table count because a couple thousand people identify as white...
    Japanese = (RACASIAN > 1) & (
      ANCESTR1 %in% 740:749 | ANCESTR2 %in% 740:749 | 
        RACED %in% c(500, 673, 677, 812, 833, 863, 869, 882, 913, 926, 964)
    ),
    ### = Filipino
    # Asian and
    Filipino = (RACASIAN > 1) & (
      # with ancestry or detailed race including Filipino (ancestry 720)
      ANCESTR1 %in% 720 | ANCESTR2 %in% 720 | 
        RACED %in% c(600, 674, 677, 813, 834, 851, 862, 864:865, 883, 912, 914, 916:917, 921) | 
        # or Tagalog speaker with Asian, other included in race
        (grepl('Asian\\swrite\\_in', as_factor(RACED)) & LANGUAGE %in% 54)
    ),
    ### = Vietnamese
    # Asian and
    Vietnamese = (RACASIAN > 1) & (
      # ancestry includes vietnamese (785) or race includes vietnamese
      ANCESTR1 %in% 785 | ANCESTR2 %in% 785 | RACED %in% c(640, 675, 816) |
        # or detailed race includes "other asian" and birthplace or language is vietnam(ese)
        grepl('[Oo]ther\\sAsian', as_factor(RACED)) & (BPL %in% 518 | LANGUAGE %in% 50)
    ),
    ### = Other Asian
    AsianOther = (RACASIAN > 1) & !(
      Chinese | Japanese | Korean | Vietnamese | Hmong | Laotian | SoAsian | AsianInd | Myanmar | Cambodian | Filipino
    ),
    
    ##### == PAC ISLANDER GROUPS == #####
    ### = Chamorro
    # Identifies as Pacific Islander, and
    Cham = (RACPACIS > 1) & (
      # Guamanian (821) or Chamorro (822) ancestry 
      ANCESTR1 %in% 821:822 | ANCESTR2 %in% 821:822 | 
      # or born in Guam (10500) or N. Marianna Islands (71047) and speaks Chamorro (5503)
      # or detailed race is Guamanian (685), Guamanian (691), or white+chamorro (823)
      # NOTE: still under-estimating here... could get a couple hundred more people if changing the *and* to *or* for language+bpl
      (BPLD %in% c(10500, 71047) & LANGUAGED %in% 5503) | (RACED %in% c(685, 691, 823))
    ),
    ### = Marshallese
    # Ancestry or detailed race inclues Marshallese (ancestr 825, raced 692)
    Marshallese = (ANCESTR1 %in% 825 | ANCESTR2 %in% 825 | RACED %in% 692) |
      # or, born in Marshall Islands and speaks Marshallese language
      (BPLD %in% 71041 & LANGUAGED %in% 5506),
    ### = COFA (Confederated States of Micronesia)
    # Ancestry includes Micronesian (820), Palauan (824). Kosraean (826), Ponapean (827),
    # Chuukese (828), Yap Islander (829)
    COFA = (ANCESTR1 %in% c(820, 824, 826:829) | ANCESTR2 %in% c(820, 824, 826:829)) |
      # or, detailed race listed as Palau (687), Other Micronesian (688), Chuukese (690)
      # or born in Micronesia and speaks Trukese (5511) or other Malayan Language (5300)
      RACED %in% c(687:688, 690) | (BPLD %in% 71042 & LANGUAGED %in% c(5300, 5511)),
    ### = Samoan
    # IDs as Pacific Islander AND 
    Samoan = (RACPACIS > 1) & (
      # Ancestry includes Samoan (814) or detailed race test includes Samoa
      # or birthplace is American Samoa (100)
      # NOTE: estimate currently under-estimating (maybe because of RACEPACIS flag)
      ANCESTR1 %in% 814 | ANCESTR2 %in% 814 | grepl('[Ss]amoa', as_factor(RACED)) | BPL %in% 100
    ),
    ### = Native Hawai'ian
    # Hawaiian ancestry (811) or detailed race string inclues Hawaiian AND IDs as Pacific Islander
    # NOTE: currently under-counting
    Hawaiian = (RACPACIS > 1) & (
      ANCESTR1 %in% 811 | ANCESTR2 %in% 811 | 
        RACED %in% c(630, 821, 861:864, 911:914, 926:927, 936, 964)
    ),
    ### = Other Pacific Islander
    # (TO DO: figure out what to grep for pacific islander RACED) AND not any of the other groups
    NHPIoth = RACPACIS > 1 & (grepl('[Pp]acific', as_factor(RACED)) | grepl('PI', as_factor(RACED))) &
      !(Cham | Marshallese | COFA | Samoan | Hawaiian),
    
    ##### == MIDDLE EASTERN GROUPS == #####
    ### = North African
    # Ancestry in Algerian (400), Egyptian (402), Libyan (404), Moroccan (406), North African (411)
    NoAfr = ANCESTR1 %in% 400:411 | ANCESTR2 %in% 400:411 | 
      # or, speaks Arabic (57) and born in Algeria (60010), Egypt (60012), Libya (60013), Morocco (60014),
      # Tunisia (60016), Western Sahara (60019)
      (BPLD %in% c(60010:60014, 60016:60019) & LANGUAGE %in% 57),
    ### = Middle Eastern
    # Ancestry includes Iranian (416), Iraqi (417), Israeli (419), Jordanian (421), Kuwaiti (423), 
    # Lebanese (425), Saudi Arabian (427), Syrian (429), Turkish (434), Yemeni (435)
    # Omani (436), Qatari (439), Kurdish (442), Palestinian (465), Emirati (480)
    # Syriac/Chaldean (482), Middle Eastern (490), Arab (485), Other Arab (496),
    # Afghan (600)
    MidEast = ANCESTR1 %in% c(416:429, 434:496, 600) | ANCESTR2 %in% c(416:429, 434:496, 600) | 
      # or, birthplace in # Afghanistan (520), Iran (522), Bahrain (530), Cypress (531), Iraq (532),
      # Iraq/Saudi Arabia (533), Israel/Palestine (534), Jordan (545), Kuwait (536),
      # Lebanon (537), Oman (538), Qatar (539), Saudi Arabia (540), Syria (541), Turkey (542),
      # UAE (543), Yemen (544-545), Persian Gulf States n.s. (546), Middle East n.s. (547),
      # Southwest Asia n.s.. (548), Asia Minor n.s. (549), South Asia nec (550)
      # AND speaks Farsi (29), Other Persian dialects (30), Turkish (36),
      # Near East (Levantine) Arabic (58), Hebrew/Israeli (59)
      BPL %in% c(520, 522, 530:550) & LANGUAGE %in% c(29:30, 36, 57:59),
    
    ##### == WHITE GROUPS == #####
    ### = Eastern European
    # is white, and
    EastEur = (RACWHT > 1) & (
      # Ancestry includes Armenian (431), Albanian (100), Azerbaijani (101), Estonian (115), 
      # Georgian (120), Hungarian (125), Latvian (128), Lithuanian (129), Moldavian (146), 
      # Romanian (144), Rom (124), 'Eastern European, nec' (190)
      (ANCESTR1 %in% c(431, 100:101, 115, 120, 124:129, 144:146, 190) | 
         ANCESTR2 %in% c(431, 100:101, 115, 120, 124:129, 144:146, 190)) |
      # or, born in Albania (430), Romania (456), Estonia (460), Latvia (461), 
      # Lithuania (462), Baltic n.s. (463)
        BPL %in% c(430, 456, 460:463) |
      # or, language is Yiddish/"Jewish" (3), Rumanian (14), Albanian (17), Lithuanian (25),
        # Armenian (28), Romany/Gypsy (32), Caucasian/Georgian/Avar (38)
        LANGUAGE %in% c(3, 14, 17, 25:26, 28, 32, 38)
      ),
    ### = Slavic
    # is white, and
    Slavic = (RACWHT > 1) & (
      # Ancestry includes Bulgarian (103), Belorussian (102), Czech (111), Croatian (109), 
      # Cossack (108), Serbian (152), Macedonian (130), Polish (142), Russian (148), 
      # Slovakian (153), Slovenian (154), Ukrainian (171), Yugoslavian (176), Slav (178), Bohemian (112)
      ANCESTR1 %in% c(102:103, 108:112, 130, 142, 148, 152:154, 171:178) |
        ANCESTR2 %in% c(102:103, 108:112, 130, 142, 148, 152:154, 171:178) |
      # or, speaks Russian (18), Ukranian/Ruthenian (19), Czech (20), Polish (21), Slovak (22), 
        # Serbo-Croat/Slavonian/Yugoslavian (23)
        LANGUAGE %in% (18:24) |
      # or, born in Bulgaria (451), Czechoslovakia (452), Poland (455), Yugoslavia (457), other USSR/Russia
        BPL %in% c(451:452, 455, 457, 465)
      ),
    ### = Western European
    WestEur = (RACWHT > 1) & (
      # Ancestry includes Northern European nec (183). Western European NEC (187), Alsatian (1), 
      # Austrian (3), Basque (5), Belgian, (8), Flemish (9), British (11), British Isles (12), 
      # Danish (20), English (22), Finnish (24), French (26), German (32), Prussian (40), Greek (46),
      # Icelander (49), Irish (50), Italian (51), Sicilian (68), Luxemberger (77), Maltese (78),
      # Norwegian (82), Portuguese (84), Scotch Irish (87), Scottish (88), Swedish (89), 
      # Swiss (91), Welsh (97), Scandinavian/Nordic (98), Spaniard (200)
      ANCESTR1 %in% c(183, 187, 1:98, 200) | ANCESTR2 %in% c(183, 187, 1:98, 200) |
        # or, birthplace is Denmark (400), Finland (401), Icleand (402), Lapland (403), Norway (404), 
        # Sweden (405), England (410), Scotland (411), Wales (412), UK (413), Ireland (414),
        # N. Europe (419), Belgium (420), France (421), Liechtenstein (422), Luxembourg (423), Monaco (424)
        # Netherlands (425), Switzerland (426), W. Europe (429), Andorra (431), Gibraltar (432),
        # Greece (433), Italy (434), Malta (435), Portugal (436), San Marino (437), Spain (438),
        # Vatican City (439), Austria (450), Germany (453), Hungary (454)
        BPL %in% c(400:429, 431:439, 450, 453:454) # |
        # Language: speaks German (2), Dutch (4), Swedish (5), Danish (6), Norwegian (7), 
        # Itlaian (10), Celtic (15), Greek (16), Finnish (33), Hungarian (34), Basque (39), 
        # NOTE: not including spanish or portuguese here here...
        # NOTE: Currently commented out...
        # c(2, 4:11, 15:16, 33:34, 39) ~ 'WestEur',
    ),
    ### = Other White
    # White and not caught by any of the above...
    WhiteOth = (RACWHT > 1) & !(EastEur | WestEur | Slavic)
  )
  
# some of the grepling on RACED is not helpful (ors instead of ands)

# go through at the end and see how many non-white groups have RACE == 1...
  
# Okay... let's see what we've got.
pums.out %>%
  select(where(is.logical))%>%
  apply(2, sum)

pums.out %>%
  select(c(PERWT, where(is.logical))) %>%
  mutate(across(where(is.logical), ~ PERWT * as.numeric(.))) %>%
  apply(2, sum)



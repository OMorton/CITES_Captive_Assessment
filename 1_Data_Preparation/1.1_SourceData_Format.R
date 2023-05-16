###########################
##--- Data formatting ---##
###########################

## Purpose
## Simple script to just extract the species lists for "captive" species in trade and fetch their IUCN statuses.
## Format the country data into a more useful format.

#### Packages and presets ####
options(scipen = 999, na.action = "na.pass")
.libPaths("C:/Packages") ## Set up for working from home.

library(tidyverse)
library(rredlist)


## Read in the 49 seperate .csv's from CITES latest Bulk release 2022 v1
## Takes a few minutes to run, total records 23,680,557
CITES_MASTER <- list.files(path="G:/My Drive/TUoS/Data_Sources/CITES/CITES_all_records_2022.1", 
                           full.names = TRUE, pattern="*.csv") %>% 
  lapply(read_csv, na = "", col_types = cols(Unit = col_character(), 
                                             Import.permit.RandomID = col_character(),
                                             Export.permit.RandomID = col_character(), 
                                             Origin.permit.RandomID = col_character(),
                                             Purpose = col_character())) %>% 
  bind_rows
## Remove all re-exports as per published CITES guidelines and Pers Comm with UNEP-WMC and CITES
## remove all reports where the origin is stated and is not the same as the exporter.
# this leaves 13.4 out 23.7 million shipments
CITES_TRUE <- CITES_MASTER %>% filter(Origin == Exporter  | is.na(Origin))


#### Get the CITES species list ####

## This will be used for trait data, range data and Listing data matching.
## Focus on only the target taxa and time frame and remove ambiguous records using "ssp" or "hybrid"
## 908,843 records
CITES_Taxa <- CITES_TRUE %>% 
  filter(Class %in% c("Aves", "Reptilia"), Year %in% 2000:2020,
         Source %in% c("C", "D", "F", "R")) 

## 1368 C, D, F, R Bird species (2000 - 2020)
CITES_Taxa_List <- data.frame(Taxon = unique(CITES_Taxa$Taxon)) %>%
  filter(!grepl("spp", Taxon), !grepl("hybrid", Taxon))

write.csv(CITES_Taxa_List, "Outputs/CITES/Full_CITES_Taxa_List.csv")

#### Get the IUCN Statuses ####

## Results in 805 unique "species" 
## Get list and correct names
List_update <- CITES_Taxa_List %>% mutate(IUCNName = case_when(Taxon == "Aceros cassidix" ~ "Rhyticeros cassidix",
                                                               Taxon == "Aceros corrugatus" ~"Rhabdotorrhinus corrugatus",
                                                               Taxon == "Aceros leucocephalus" ~ "Rhabdotorrhinus leucocephalus",
                                                               Taxon == "Aglaiocercus kingi" ~ "Aglaiocercus kingii",
                                                               Taxon == "Alisterus chloropterus mozskowskii" ~ "Alisterus chloropterus",
                                                               Taxon == "Amazona festiva festiva" ~ "Amazona festiva",
                                                               Taxon == "Amazona mercenaria" ~ "Amazona mercenarius",
                                                               Taxon == "Amazona xanthops" ~ "Alipiopsitta xanthops",
                                                               Taxon == "Anas clypeata" ~ "Spatula clypeata",
                                                               Taxon == "Anas formosa" ~ "Sibirionetta formosa",
                                                               Taxon == "Anas penelope" ~ "Mareca penelope",
                                                               Taxon == "Anas querquedula" ~ "Spatula querquedula",
                                                               Taxon == "Aquila clanga" ~ "Clanga clanga",
                                                               Taxon == "Aquila pomarina" ~ "Clanga pomarina",
                                                               Taxon == "Aratinga acuticaudata" ~ "Psittacara acuticaudatus",
                                                               Taxon == "Aratinga aurea" ~ "Eupsittula aurea",
                                                               Taxon == "Aratinga cactorum" ~ "Eupsittula cactorum",
                                                               Taxon == "Aratinga canicularis" ~ "Eupsittula canicularis",
                                                               Taxon == "Aratinga chloroptera" ~ "Psittacara chloropterus",
                                                               Taxon == "Aratinga erythrogenys" ~ "Psittacara erythrogenys",
                                                               Taxon == "Aratinga euops" ~ "Psittacara euops",
                                                               Taxon == "Aratinga finschi" ~ "Psittacara finschi",
                                                               Taxon == "Aratinga holochlora" ~ "Psittacara holochlorus",
                                                               Taxon == "Aratinga leucophthalma" ~ "Psittacara leucophthalmus",
                                                               Taxon == "Aratinga mitrata" ~ "Psittacara mitratus",
                                                               Taxon == "Aratinga nana" ~ "Eupsittula nana",
                                                               Taxon == "Aratinga pertinax" ~ "Eupsittula pertinax",
                                                               Taxon == "Aratinga wagleri" ~ "Psittacara wagleri",
                                                               Taxon == "Baillonius bailloni" ~ "Pteroglossus bailloni",
                                                               Taxon == "Barnardius barnardi" ~ "Barnardius zonarius",
                                                               Taxon == "Branta canadensis leucopareia" ~ "Branta canadensis",
                                                               Taxon == "Bubo bubo bengalensis" ~ "Bubo bubo",
                                                               Taxon == "Buceros hydrocorax hydrocorax" ~ "Buceros hydrocorax",
                                                               Taxon == "Buteo magnirostris" ~ "Rupornis magnirostris",
                                                               Taxon == "Buteo poecilochrous" ~ "Geranoaetus polyosoma",
                                                               Taxon == "Buteo polyosoma" ~ "Geranoaetus polyosoma",
                                                               Taxon == "Calyptorhynchus baudinii" ~ "Zanda baudinii",
                                                               Taxon == "Calyptorhynchus funereus" ~ "Zanda funerea",
                                                               Taxon == "Calyptorhynchus latirostris" ~ "Zanda latirostris",
                                                               Taxon == "Carduelis cucullata" ~ "Spinus cucullatus",
                                                               Taxon == "Carduelis yarrellii" ~ "Spinus yarrellii", 
                                                               Taxon == "Chalcopsitta sintillata" ~ "Chalcopsitta scintillata",
                                                               Taxon == "Damophila julie" ~ "Amazilia julie",
                                                               Taxon == "Eos rubra" ~ "Eos bornea",
                                                               Taxon == "Eos squamata riciniata" ~ "Eos squamata",
                                                               Taxon == "Estrilda caerulescens" ~ "Estrilda coerulescens",
                                                               Taxon == "Eupodotis vigorsii" ~ "Heterotetrax vigorsii",
                                                               Taxon == "Falco pelegrinoides" ~ "Falco peregrinus",
                                                               Taxon == "Falco pelegrinoides babylonicus" ~ "Falco peregrinus",
                                                               Taxon == "Falco peregrinus anatum" ~ "Falco peregrinus",
                                                               Taxon == "Falco peregrinus peregrinus" ~ "Falco peregrinus",
                                                               Taxon == "Grus canadensis pratensis" ~ "Grus canadensis",
                                                               Taxon == "Grus leucogeranus" ~ "Leucogeranus leucogeranus",
                                                               Taxon == "Guarouba guarouba" ~ "Guaruba guarouba",
                                                               Taxon == "Gyps rueppellii" ~ "Gyps rueppelli",
                                                               Taxon == "Hieraaetus fasciatus" ~ "Aquila fasciata",
                                                               Taxon == "Hieraaetus spilogaster" ~ "Aquila spilogaster",
                                                               Taxon == "Leptoptilos crumeniferus" ~ "Leptoptilos crumenifer",
                                                               Taxon == "Leucopternis albicollis" ~ "Pseudastur albicollis",
                                                               Taxon == "Lonchura bicolor" ~ "Spermestes bicolor",
                                                               Taxon == "Lonchura cantans" ~ "Euodice cantans",
                                                               Taxon == "Lonchura cucullata" ~ "Spermestes cucullata",
                                                               Taxon == "Lonchura fringilloides" ~ "Spermestes fringilloides",
                                                               Taxon == "Lophura imperialis" ~ "Lophura imperialis", ## remove hybrid sp
                                                               Taxon == "Morelia boeleni" ~ "Simalia boeleni",
                                                               Taxon == "Nandayus nenday" ~ "Aratinga nenday",
                                                               Taxon == "Nigrita canicapilla" ~ "Nigrita canicapillus",
                                                               Taxon == "Nyctea scandiaca" ~ "Bubo scandiacus",
                                                               Taxon == "Orthopsittaca manilata" ~ "Orthopsittaca manilatus",
                                                               Taxon == "Otus asio" ~ "Megascops asio",
                                                               Taxon == "Otus choliba" ~ "Megascops choliba",
                                                               Taxon == "Otus kennicottii" ~ "Megascops kennicottii",
                                                               Taxon == "Otus roboratus" ~ "Megascops roboratus",
                                                               Taxon == "Otus watsonii" ~ "Megascops watsonii",
                                                               Taxon == "Paradisaea rudolphi" ~ "Paradisornis rudolphi",
                                                               Taxon == "Phoenicopterus ruber ruber" ~ "Phoenicopterus ruber",
                                                               Taxon == "Pionopsitta barrabandi" ~ "Pyrilia barrabandi",
                                                               Taxon == "Pitta guajana" ~ "Hydrornis guajanus",
                                                               Taxon == "Platycercus barnardi" ~ "Barnardius zonarius",
                                                               Taxon == "Platycercus zonarius" ~ "Barnardius zonarius",
                                                               Taxon == "Poephila cincta cincta" ~ "Poephila cincta",
                                                               Taxon == "Psephotus chrysopterygius" ~ "Psephotellus chrysopterygius",
                                                               Taxon == "Psephotus dissimilis" ~ "Psephotellus dissimilis",
                                                               Taxon == "Psephotus pulcherrimus" ~ "Psephotellus pulcherrimus",
                                                               Taxon == "Psephotus varius" ~ "Psephotellus varius",
                                                               Taxon == "Pseudoscops clamator" ~ "Asio clamator",
                                                               Taxon == "Psittacula calthorpae" ~ "Nicopsitta calthrapae",
                                                               Taxon == "Psittacula columboides" ~ "Nicopsitta columboides",
                                                               Taxon == "Psittacula cyanocephala" ~ "Himalayapsitta cyanocephala",
                                                               Taxon == "Psittacula echo" ~ "Alexandrinus eques",
                                                               Taxon == "Psittacula eupatria" ~ "Palaeornis eupatria",
                                                               Taxon == "Psittacula finschii" ~ "Himalayapsitta finschii",
                                                               Taxon == "Psittacula himalayana" ~ "Himalayapsitta himalayana",
                                                               Taxon == "Psittacula krameri" ~ "Alexandrinus krameri",
                                                               Taxon == "Psittacula longicauda" ~ "Belocercus longicaudus",
                                                               Taxon == "Psittacula roseata" ~ "Himalayapsitta roseata",
                                                               Taxon == "Psittacus erithacus timneh" ~ "Psittacus timneh",
                                                               Taxon == "Psitteuteles iris" ~ "Trichoglossus iris",
                                                               Taxon == "Pterocnemia pennata" ~ "Rhea pennata",
                                                               Taxon == "Pterocnemia pennata pennata" ~ "Rhea pennata",
                                                               Taxon == "Ptiloris magnificus" ~ "Lophorina magnifica",
                                                               Taxon == "Rhea americana albescens" ~ "Rhea americana",
                                                               Taxon == "Serinus leucopygius" ~ "Crithagra leucopygia",
                                                               Taxon == "Serinus mozambicus" ~ "Crithagra mozambica",
                                                               Taxon == "Spizaetus africanus" ~ "Aquila africana",
                                                               Taxon == "Spizaetus nipalensis" ~ "Nisaetus nipalensis",
                                                               Taxon == "Spizastur melanoleucus" ~ "Spizaetus melanoleucus",
                                                               Taxon == "Streptopelia senegalensis" ~ "Spilopelia senegalensis",
                                                               Taxon == "Strix virgata" ~ "Ciccaba virgata",
                                                               Taxon == "Tauraco porphyreolophus" ~ "Gallirex porphyreolophus",
                                                               Taxon == "Torgos tracheliotus" ~ "Torgos tracheliotos",
                                                               Taxon == "Spizaetus cirrhatus" ~ "Nisaetus cirrhatus",
                                                               Taxon == "Anas oustaleti" ~ "Anas oustaleti", # Mariana mallard EX
                                                               Taxon == "Colinus virginianus ridgwayi" ~ "Colinus virginianus",
                                                               Taxon == "Diphyllodes magnificus" ~ "Cicinnurus magnificus",
                                                               Taxon == "Lonchura oryzivora" ~ "Padda oryzivora",
                                                               Taxon == "Penelopides exarhatus" ~ "Rhabdotorrhinus exarhatus",
                                                               Taxon == "Aceros waldeni" ~ "Rhabdotorrhinus waldeni",
                                                               Taxon == "Pionopsitta haematotis" ~ "Pyrilia haematotis",
                                                               Taxon == "Ptilinopus marchei" ~ "Ramphiculus marchei",
                                                               Taxon == "Chlorostilbon swainsonii" ~ "Riccordia swainsonii",
                                                               Taxon == "Lichenostomus melanops cassidix" ~ "Lichenostomus melanops",
                                                               Taxon == "Amazilia tobaci" ~ "Saucerottia tobaci",
                                                               Taxon == "Chlorostilbon notatus" ~ "Chlorestes notata",
                                                               Taxon == "Lophura hatinhensis" ~ "Lophura edwardsi",
                                                               Taxon == "Northiella haematogaster narethae" ~ "Northiella haematogaster",
                                                               Taxon == "Amazilia amazilia" ~ "Amazilis amazilia",
                                                               Taxon == "Amazilia chionogaster" ~ "Elliotomyia chionogaster",
                                                               Taxon == "Pionopsitta caica" ~ "Pyrilia caica",
                                                               Taxon == "Gallicolumba criniger" ~ "Gallicolumba crinigera",
                                                               Taxon == "Oroaetus isidori" ~ "Spizaetus isidori",
                                                               Taxon == "Milvago chimango" ~ "Phalcoboenus chimango",
                                                               Taxon == "Pionopsitta pulchra" ~ "Pyrilia pulchra",
                                                               ## repts
                                                               Taxon == "Aldabrachelys gigantea" ~ "Geochelone gigantea",
                                                               Taxon == "Boa constrictor occidentalis" ~ "Boa constrictor",
                                                               Taxon == "Caiman crocodilus fuscus" ~ "Caiman crocodilus",
                                                               Taxon == "Chelonoidis carbonarius" ~ "Chelonoidis carbonarius", ## NE
                                                               Taxon == "Chelonoidis denticulatus" ~ "Chelonoidis denticulata",
                                                               Taxon == "Cyclagras gigas" ~ "Hydrodynastes gigas",
                                                               Taxon == "Epicrates cenchria cenchria" ~ "Epicrates cenchria",
                                                               Taxon == "Epicrates fordii" ~ "Chilabothrus fordii",
                                                               Taxon == "Homopus signatus" ~ "Chersobius signatus",
                                                               Taxon == "Karusaurus polyzonus" ~ "Karusasaurus polyzonus",
                                                               Taxon == "Mauremys iversoni" ~ "Mauremys iversoni", ## hybrid
                                                               Taxon == "Pelomedusa subrufa" ~ "Pelomedusa galeata",
                                                               Taxon == "Python molurus molurus" ~ "Python molurus",
                                                               Taxon == "Python timoriensis" ~ "Malayopython timoriensis",
                                                               Taxon == "Trachemys scripta elegans" ~ "Trachemys scripta",
                                                               Taxon == "Caiman crocodilus apaporiensis" ~ "Caiman crocodilus",
                                                               Taxon == "Caiman crocodilus crocodilus" ~ "Caiman crocodilus",
                                                               Taxon == "Caiman crocodilus fuscus" ~ "Caiman crocodilus",
                                                               Taxon == "Caiman crocodilus yacare" ~ "Caiman yacare",
                                                               Taxon == "Epicrates angulifer" ~ "Chilabothrus angulifer",
                                                               Taxon == "Epicrates subflavus" ~ "Chilabothrus subflavus",
                                                               Taxon == "Gongylophis colubrinus" ~ "Eryx colubrinus",
                                                               Taxon == "Gongylophis conicus" ~ "Eryx conicus",
                                                               Taxon == "Kinixys belliana" ~ "Kinixys belliana", ## NE
                                                               Taxon == "Ptyas mucosus" ~ "Ptyas mucosa",
                                                               Taxon == "Python reticulatus" ~ "Malayopython reticulatus",
                                                               Taxon == "Vipera wagneri" ~ "Montivipera wagneri",
                                                               Taxon == "Elaphe radiata" ~ "Coelognathus radiatus",
                                                               Taxon == "Epicrates inornatus" ~ "Chilabothrus inornatus",
                                                               Taxon == "Gongylophis muelleri" ~ "Eryx muelleri",
                                                               Taxon == "Phelsuma andamanense" ~ "Phelsuma andamanensis",
                                                               Taxon == "Enhydris bocourti" ~ "Subsessor bocourti",
                                                               Taxon == "Epicrates chrysogaster" ~ "Chilabothrus chrysogaster",
                                                               Taxon == "Crotalus durissus unicolor" ~ "Crotalus durissus",
                                                               Taxon == "Kinixys spekii" ~ "Kinixys spekii", ## NE
                                                               Taxon == "Pelodiscus axenaria" ~ "Pelodiscus axenaria", ## NE
                                                               Taxon == "Podocnemis vogli" ~ "Podocnemis vogli", ## NE
                                                               Taxon == "Varanus ornatus" ~ "Varanus niloticus",
                                                               Taxon == "Varanus similis" ~ "Varanus scalaris",
                                                               Taxon == "Psammobates oculifer" ~ "Psammobates oculifer", ## NE
                                                               Taxon == "Cyclura ricordi" ~ "Cyclura ricordii",
                                                               Taxon == "Woodworthia chrysosireticus" ~ "Woodworthia chrysosiretica", 
                                                               Taxon == "Phrynosoma cerroense" ~ "Phrynosoma cerroense", ## NE
                                                               Taxon == "Morelia nauta" ~ "Simalia nauta",
                                                               Taxon == "Crocodylus cataphractus" ~ "Mecistops cataphractus",
                                                               Taxon == "Lapemis curtus" ~ "Hydrophis curtus",
                                                               Taxon == "Leiopython hoserae" ~ "Leiopython meridionalis",
                                                               Taxon == "Epicrates striatus" ~ "Chilabothrus striatus",
                                                               Taxon == "Epicrates gracilis" ~ "Chilabothrus gracilis",
                                                               Taxon == "Woodworthia maculatus" ~ "Woodworthia maculata",
                                                               Taxon == "Psammobates tentorius trimeni" ~ "Psammobates tentorius",
                                                               Taxon == "Trimeresurus mangshanensis" ~ "Protobothrops mangshanensis",
                                                               Taxon == "Uromastyx nigriventris" ~ "Uromastyx nigriventris", ## NE
                                                               Taxon == "Heloderma horridum charlesbogerti" ~ "Heloderma charlesbogerti",
                                                               Taxon == "Morelia clastolepis" ~ "Simalia clastolepis",
                                                               Taxon == "Mauremys pritchardi" ~ "Mauremys reevesii",
                                                               Taxon == "Pelusios castaneus" ~ "Pelusios castaneus", ## NE
                                                               Taxon == "Pelusios gabonensis" ~ "Pelusios gabonensis", ## NE
                                                               Taxon == "Rhacodactylus ciliatus" ~ "Correlophus ciliatus",
                                                               Taxon == "Kinixys zombensis" ~ "Kinixys zombensis", ## NE
                                                               Taxon == "Morelia tracyae" ~ "Simalia tracyae",
                                                               TRUE ~ Taxon))

## get key
## token can be generated here
## https://apiv3.iucnredlist.org/api/v3/token
apikey <- "a3fc116c122aefc621329055aeae8f67483e575c518acc14fcb77709bd94f6a2"


## Dummy data frame for the loops
df <- data.frame(IUCNName = character(),
                 Year = character(),
                 IUCN_code = character(),
                 IUCN_cat = character())

## Get species historical statuses.
## Needs an API key and because of the delay needed between calls takes ~1 hour to run.
## Do not remove the delay see rredlist guide for details.
## https://cran.r-project.org/web/packages/rredlist/rredlist.pdf

for(i in 1:nrow(List_update)){ # would have used for(sp in speciesList) but need i for progress bar?
  ## incorporate 2s delay between each query
  Sys.sleep(2)
  ## Progress update
  cat('Species=',i, '\n')
  ## get historical data from website
  sp <- List_update$IUCNName[i]
  iucnHistory <- rl_history(name=sp, key=apikey)
  # IF species cannot be found
  if (length(iucnHistory$result) == 0){ 
    spDf <- data.frame(IUCNName = sp,
                       Year = NA,
                       IUCN_code = NA,
                       IUCN_cat = NA)
    df <- rbind(df, spDf)
    # cat('Check ', sp, '\n')
  } else { 
    spdf <- data.frame(IUCNName = sp,
                       Year = iucnHistory$result$year,
                       IUCN_code = iucnHistory$result$code,
                       IUCN_cat = iucnHistory$result$category)
    df <- rbind(df, spdf)
  }
}

## Got this fro repts and aves
#write.csv(df, "Outputs/IUCN/IUCN_API_Out.csv")  
df <- read.csv("Outputs/IUCN/IUCN_API_Out.csv") %>% select(-X)

## two hybirds, 11 NA, and one extinct species 
df %>% filter(is.na(Year))
df_all <- left_join(df, List_update)
check <- df_all %>% group_by(Taxon) %>% filter(any(IUCN_code %in% c("CT", "NR", "K", "R", "T", "V", "E", "I", "nt", NA)))
length(unique(df_all$Taxon)) ## 1368

## Therefore remove these
Historic_IUCN <- df_all %>% 
  ## Set Not assessed species as not assessed in 2000 otherwise they are also NA for Year and get removed later when they shouldnt
  mutate(Year = if_else(is.na(Year), 2000, as.numeric(Year))) %>%
  filter(!IUCN_code %in% c("CT", "NR", "K", "R", "T", "V", "E", "I", "nt"))

length(unique(Historic_IUCN$Taxon)) ## 1368

## Cyanopsitta spixii, Strigops habroptila, Psephotus pulcherrimus
## Keep these but flag them
Historic_IUCN %>% filter(IUCN_code %in% c("EX", "EW", "Ex"))

## Convert the 1994 system to post-2000
Historic_IUCN_up <- Historic_IUCN %>%
  mutate(IUCN_code = replace(IUCN_code, IUCN_code == "LR/lc", "LC"),
         IUCN_code = replace(IUCN_code, IUCN_code == "LR/nt", "NT"),
         IUCN_code = replace(IUCN_code, IUCN_code == "LR/cd", "NT"),
         IUCN_code = replace(IUCN_code, IUCN_code == "Ex", "EX"))

## Check removal and conversion left only the post 2001 framework
unique(Historic_IUCN_up$IUCN_code)
unique(Historic_IUCN_up$Taxon) ## 1368

## Backbone of values 2000 - 2020
backbone <- expand.grid(Year = as.integer(1988:2022), Taxon = unique(Historic_IUCN_up$Taxon))

## left join this and create your unrolled status
## Some species in trade before being IUCN assessed these are the NA values
Historic_IUCN_up$Year <- as.integer(Historic_IUCN_up$Year)
backbone$Year <- as.integer(backbone$Year)

## Here we add the backbone of species and dates to the IUCN data
df_new <- left_join(backbone, Historic_IUCN_up) %>%
  arrange(Taxon, Year) %>% group_by(Taxon) %>% 
  fill(IUCN_code , .direction = "down") %>% 
  fill(IUCNName, .direction = "down") %>% 
  fill(IUCNName, .direction = "up") %>% 
  fill(Taxon, .direction = "down") %>% 
  fill(Taxon, .direction = "up") %>% 
  filter(Year %in% c(2000:2020)) %>% ungroup() %>%
  select(Year, Taxon, IUCN_code, IUCNName, Taxon) %>% 
  mutate(IUCN_code = replace_na(IUCN_code, "Not assessed"),
         IUCN_code = ifelse(IUCN_code == "DD", "Not assessed", IUCN_code)) %>% 
  distinct() %>%
  ## deal with EX species
  mutate(IUCN_code = ifelse(Taxon == "Anas oustaleti", "EX", IUCN_code))

## Now we have a clean time series of all statuses for all species 2000 - 2020 (0 - 20)
## Note for completeness we keep the Lophura hybrid
length(unique(df_new$Taxon)) ## 1368 species


## Check no species were assessed twice in one year
df_new %>% group_by(Year, Taxon) %>% tally() %>% filter(n != 1)
df_new <- df_new %>% filter(!(Year == 2018 & Taxon == "Homopus signatus" & IUCN_code == "VU"))
write.csv(df_new, "Outputs/IUCN/BirdRept_IUCN_2000_2020.csv")

#### Get the country list ####
## Get all countries that are involved in the trade of the focal species.
## we can then cross reference this with the country distribution and border data we have
## to ensure that all countries have a match
CITES_Countries <- CITES_MASTER %>% filter(Taxon %in% CITES_Taxa_List$Taxon)

CITES_Countries_List <- data.frame(Country = unique(c(unique(CITES_Countries$Exporter),
                                                        unique(CITES_Countries$Importer))))

write.csv(CITES_Countries_List, "Outputs/CITES/Full_CITES_Countries_List.csv", na = "")

#### Get register data ####

CaptiveRegister <- data.table::fread("Data/CITES_CaptiveRegister_Summary.csv", na.strings = "") %>% select(1:3)
CITES_Parties <- data.table::fread("Data/CITES_Parties.csv", na.strings = "") %>% select(2:3) %>% 
  rename("Country" = 1, "ISO" = 2)


CaptiveRegister <- CaptiveRegister %>% 
  fill(Country, .direction = "down") %>%
  fill(Code, .direction = "down") %>% 
  left_join(CITES_Parties)

write.csv(CaptiveRegister, "Outputs/CITES/CaptiveRegister_List.csv", na = "")

#### Get the neighbor country format ####
Neighbouring_Countries <- data.table::fread("Data/Neighbouring_Countries.csv", na.strings = "")

Long_form_nei <- Neighbouring_Countries %>% 
  select(cca2, region, subregion, borders) %>% 
  separate_rows(borders) %>%
  rename("Country" = "cca2") %>%
  left_join( select(Neighbouring_Countries, cca2, cca3), 
             by = c("borders" = "cca3")) %>% rename("borders2" = "cca2")

write.csv(Long_form_nei, "Outputs/Countries/Neighbours_List.csv", na = "")

#### Get the distribution country format ####
Distribution_Countries <- data.table::fread("Data/Distribution_Countries.csv", na.strings = "")

Long_form_dis <- Distribution_Countries %>% 
  select(`Scientific Name`, All_DistributionISOCodes) %>% 
  separate_rows(All_DistributionISOCodes) %>%
  rename("CITES_name" = "Scientific Name", "Distribution" = "All_DistributionISOCodes")

write.csv(Long_form_dis, "Outputs/Countries/Distribution_List.csv", na = "")

#### Listed time series ####

CITES_Species <- CITES_TRUE %>% 
  filter(Class %in% c("Aves", "Reptilia"), Year %in% 2000:2020,
         Source %in% c("C", "D", "F", "R")) %>%
  group_by(Taxon) %>% slice(1) %>% select(Class, Order, Family, Genus, Taxon) %>% ungroup() %>%
  filter(!grepl("spp", Taxon), !grepl("hybrid", Taxon))

length(unique(CITES_Species$Taxon)) ## 1368

## Read in the cites historic listings data
Historic_CITES <- data.table::fread("Data/History_of_CITES_Listings_2021.csv") %>% 
  mutate(Year = format(as.Date(EffectiveAt, format="%d/%m/%Y"),"%Y"))

## Get the unique listings from the listing data/
## This tidies and gets the first year a species is CITES listed (the start of its possible time series)
Addition <- Historic_CITES %>% group_by(Class, Order, Family, Genus, FullName, Year, ChangeType, Appendix) %>% 
  tally() %>%
  mutate(FullName = ifelse(FullName == Order, NA, FullName),
         FullName = ifelse(FullName == Family, NA, FullName),
         FullName = ifelse(FullName == Genus, NA, FullName)) %>%
  rename(Taxon = FullName) %>%
  ungroup() %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.)))) %>%
  filter(Class %in% c("Aves", "Reptilia"), ChangeType == "ADDITION") %>% ungroup()

Deletion <- Historic_CITES %>% group_by(Class, Order, Family, Genus, FullName, Year, ChangeType, Appendix) %>% 
  tally() %>%
  mutate(FullName = ifelse(FullName == Order, NA, FullName),
         FullName = ifelse(FullName == Family, NA, FullName),
         FullName = ifelse(FullName == Genus, NA, FullName)) %>%
  rename(Taxon = FullName) %>%
  ungroup() %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.)))) %>%
  filter(Class %in% c("Aves", "Reptilia"), ChangeType == "DELETION") %>% ungroup()

## First match all species level CITES listings
A_SP <- Addition %>% filter(!is.na(Taxon)) %>% select(Taxon, Year, Appendix)
Sp_join_A <- left_join(CITES_Species, A_SP, by = "Taxon") %>% mutate(Year = as.integer(Year))

D_SP <- Deletion %>% filter(!is.na(Taxon)) %>% select(Taxon, Year, Appendix)
Sp_join_D <- left_join(CITES_Species, D_SP, by = "Taxon") %>% mutate(Year = as.integer(Year))

sp_done_A <- Sp_join_A %>% filter(!is.na(Year)) %>% mutate(ADD = "ADD")
sp_done_D <- Sp_join_D %>% filter(!is.na(Year)) %>% mutate(DEL = "DEL")

Sp_series <- expand.grid(Taxon = unique(sp_done_A$Taxon), Year = 1975:2020) %>% 
  left_join(select(sp_done_A, Taxon, Year, Appendix, ADD)) %>%
  left_join(select(sp_done_D, Taxon, Year, DEL), by = c("Taxon", "Year")) %>%
  mutate(Change = case_when(ADD == "ADD" ~ "ADD",
                            ADD == "ADD" & DEL == "DEL" ~ "ADD",
                            DEL == "DEL" ~ "DEL",
                            TRUE ~ ADD)) %>%
  arrange(Taxon, Year) %>% group_by(Taxon) %>% 
  fill(Change, .direction = "down") %>%
  fill(Appendix, .direction = "down") %>%
  mutate(Appendix = ifelse(Change == "DEL", "Not listed", Appendix),
         Appendix = ifelse(is.na(Appendix), "Not listed", Appendix))
  
check <- Sp_series %>% group_by(Taxon) %>% filter(n_distinct(Appendix) > 1)
n_distinct(Sp_series$Taxon) ## 538

## Second match at genus level
genus_to_match <- Sp_join_A %>% filter(is.na(Year)) %>% select(-Year) %>% select(Taxon, Genus, Family, Order)
## get all the genus level appendix listings
A_GEN <- Addition %>% filter(is.na(Taxon), !is.na(Genus)) %>% select(Genus, Year, Appendix)
GEN_join_A <- left_join(genus_to_match, A_GEN, by = "Genus") %>% mutate(Year = as.integer(Year))

D_GEN <- Deletion %>% filter(is.na(Taxon), !is.na(Genus)) %>% select(Genus, Year, Appendix)
GEN_join_D <- left_join(genus_to_match, D_GEN, by = "Genus") %>% mutate(Year = as.integer(Year))

GEN_done_A <- GEN_join_A %>% filter(!is.na(Year)) %>% mutate(ADD = "ADD")
GEN_done_D <- GEN_join_D %>% filter(!is.na(Year)) %>% mutate(DEL = "DEL")

GEN_series <- expand.grid(Taxon = unique(GEN_done_A$Taxon), Year = 1975:2020) %>% 
  left_join(select(GEN_done_A, Taxon, Year, Appendix, ADD)) %>%
  left_join(select(GEN_done_D, Taxon, Year, DEL), by = c("Taxon", "Year")) %>%
  mutate(Change = case_when(ADD == "ADD" ~ "ADD",
                            ADD == "ADD" & DEL == "DEL" ~ "ADD",
                            DEL == "DEL" ~ "DEL",
                            TRUE ~ ADD)) %>%
  arrange(Taxon, Year) %>% group_by(Taxon) %>% 
  fill(Change, .direction = "down") %>%
  fill(Appendix, .direction = "down")  %>%
  mutate(Appendix = ifelse(Change == "DEL", "Not listed", Appendix),
         Appendix = ifelse(is.na(Appendix), "Not listed", Appendix))

n_distinct(GEN_series$Taxon) ## 376


## third match at family level
## get all the family level listings 53 listings
## Second match at genus level
fam_to_match <- GEN_join_A %>% filter(is.na(Year)) %>% select(-Year) %>% select(Taxon, Family, Order)
## get all the genus level appendix listings
A_FAM <- Addition %>% filter(is.na(Taxon), is.na(Genus), !is.na(Family)) %>% select(Family, Year, Appendix)
FAM_join_A <- left_join(fam_to_match, A_FAM, by = "Family") %>% mutate(Year = as.integer(Year))

D_FAM <- Deletion %>% filter(is.na(Taxon), is.na(Genus), !is.na(Family)) %>% select(Family, Year, Appendix)
FAM_join_D <- left_join(fam_to_match, D_FAM, by = "Family") %>% mutate(Year = as.integer(Year))

FAM_done_A <- FAM_join_A %>% filter(!is.na(Year)) %>% mutate(ADD = "ADD")
FAM_done_D <- FAM_join_D %>% filter(!is.na(Year)) %>% mutate(DEL = "DEL")

FAM_series <- expand.grid(Taxon = unique(FAM_done_A$Taxon), Year = 1975:2020) %>% 
  left_join(select(FAM_done_A, Taxon, Year, Appendix, ADD)) %>%
  left_join(select(FAM_done_D, Taxon, Year, DEL), by = c("Taxon", "Year")) %>%
  mutate(Change = case_when(ADD == "ADD" ~ "ADD",
                            ADD == "ADD" & DEL == "DEL" ~ "ADD",
                            DEL == "DEL" ~ "DEL",
                            TRUE ~ ADD)) %>%
  arrange(Taxon, Year) %>% group_by(Taxon) %>% 
  fill(Change, .direction = "down") %>%
  fill(Appendix, .direction = "down") %>%
  mutate(Appendix = ifelse(Change == "DEL", "Not listed", Appendix),
         Appendix = ifelse(is.na(Appendix), "Not listed", Appendix))

n_distinct(FAM_series$Taxon) ## 357

## Fourth match at order level
Ord_to_match <- FAM_join_A %>% filter(is.na(Year)) %>% select(-Year) %>% select(Taxon, Family, Order)
## get all the genus level appendix listings
A_ORD <- Addition %>% filter(is.na(Taxon), is.na(Genus), is.na(Family), !is.na(Order)) %>% 
  select(Order, Year, Appendix)
ORD_join_A <- left_join(Ord_to_match, A_ORD, by = "Order") %>% mutate(Year = as.integer(Year))

D_ORD <- Deletion %>% filter(is.na(Taxon), is.na(Genus), is.na(Family), !is.na(Order)) %>%
  select(Order, Year, Appendix)
ORD_join_D <- left_join(Ord_to_match, D_ORD, by = "Order") %>% mutate(Year = as.integer(Year))

ORD_done_A <- ORD_join_A %>% filter(!is.na(Year)) %>% mutate(ADD = "ADD")
ORD_done_D <- ORD_join_D %>% filter(!is.na(Year)) %>% mutate(DEL = "DEL")

ORD_series <- expand.grid(Taxon = unique(ORD_done_A$Taxon), Year = 1975:2020) %>% 
  left_join(select(ORD_done_A, Taxon, Year, Appendix, ADD)) %>%
  left_join(select(ORD_done_D, Taxon, Year, DEL), by = c("Taxon", "Year")) %>%
  mutate(Change = case_when(ADD == "ADD" ~ "ADD",
                            ADD == "ADD" & DEL == "DEL" ~ "ADD",
                            DEL == "DEL" ~ "DEL",
                            TRUE ~ ADD)) %>%
  arrange(Taxon, Year) %>% group_by(Taxon) %>% 
  fill(Change, .direction = "down") %>%
  fill(Appendix, .direction = "down") %>%
  mutate(Appendix = ifelse(Change == "DEL", "Not listed", Appendix),
         Appendix = ifelse(is.na(Appendix), "Not listed", Appendix))

n_distinct(ORD_series$Taxon) ## 59

All_sp_fl <- rbind(Sp_series, GEN_series, FAM_series, ORD_series)
n_distinct(All_sp_fl$Taxon)

SP <-CITES_Taxa_List %>% filter(!Taxon %in% unique(All_sp_fl$Taxon))
checks <- CITES_Taxa %>% filter(Taxon %in% unique(SP$Taxon))

## 11 species recorded in the database are actually never listed
## only 2 species with odd series (Lophura hatinhensis and Poephila cincta), we note in both cases
## this may be a naming error hatinhensis has never been listed but is considerd a variant of edwardsii
## and only the ssp P. c. cincta is listed. 
## Current approach will be to correct all names in the data set.
All_sp_fl %>% filter(Year >1999)

All_sp_fl <- left_join(All_sp_fl, CITES_Species)

write.csv(All_sp_fl, "Outputs/CITES/Listed_Time_Series.csv")

#################################################
##--- Resolve the species and country names ---##
#################################################

## Purpose

#### Packages and presets ####
options(scipen = 999, na.action = "na.pass")
.libPaths("C:/Packages") ## Set up for working from home.

library(tidyverse)

#### Data ####
CITES_Country_List <- data.table::fread("Outputs/CITES/Full_CITES_Countries_List.csv", na.strings = "", header = TRUE) %>% select(-V1)
Distribution_List <- data.table::fread("Outputs/Countries/Distribution_List.csv", na.strings = "", header = TRUE) %>% select(-V1)
Neighbours_List <- data.table::fread("Outputs/Countries/Neighbours_List.csv", na.strings = "", header = TRUE) %>% select(-V1)
Aves_IUCN <- data.table::fread("Outputs/IUCN/Bird_IUCN_2000_2020.csv")
Bird_et_al <- data.table::fread("Data/Bird_et_al_2020.csv")

#### Species taxonomy ####

## When resolving the taxonomy we default to keeping the CITES taxonomic designation, even
## though in some cases this is out of date. This is for consistency with policy and CITES documents.

## CITESdb species
CITES_Aves_List <- Aves_IUCN %>% distinct(Taxon, IUCNName)
## Distrib species
Distr_Check_List <- Distribution_List %>% distinct(CITES_name) %>% mutate(Check = "Present_Distr")
## LH species
LH_Check_List <- Bird_et_al %>% distinct(Taxon)  %>% mutate(Check = "Present_Distr")

## Distrib check
## UNEP WCMC use the species plus data for species ranges and occurrence. This is fine for recent years 
## but for species that are not listed currently the data isnt accesible in bulk so we can use the redlist
## to get data from the remaining species.
## Needs discussion exactly where speciesplus data is drawn from as it differs significantly to the redlist 
## for some species, better resolution for ssps but also seems to miss more range countries for some
## full species e.g Agapornis roseicollis
Check_Distr <- left_join(CITES_Aves_List, Distr_Check_List, by = c("Taxon" = "CITES_name")) 

Distr_na <- filter(Check_Distr, is.na(Check))
apikey <- "a3fc116c122aefc621329055aeae8f67483e575c518acc14fcb77709bd94f6a2"
## Dummy data frame for the loops
df <- data.frame(IUCNName = character(),
                 Taxon = character(),
                 code = character())

## Get species historical statuses.
## Needs an API key and because of the delay needed between calls takes ~1 hour to run.
## Do not remove the delay see rredlist guide for details.
## https://cran.r-project.org/web/packages/rredlist/rredlist.pdf

for(i in 1:nrow(Distr_na)){ # would have used for(sp in speciesList) but need i for progress bar?
  ## incorporate 2s delay between each query
  Sys.sleep(2)
  ## Progress update
  cat('Species=',i, '\n')
  ## get historical data from website
  sp <- Distr_na$IUCNName[i]
  CITES_sp <- Distr_na$Taxon[i]
  iucnOcc <- rl_occ_country(name=sp, key=apikey)
  # IF species cannot be found
  if (length(iucnOcc$result) == 0){ 
    spDf <- data.frame(IUCNName = sp,
                       Taxon = CITES_sp,
                       code = NA)
    df <- rbind(df, spDf)
    # cat('Check ', sp, '\n')
  } else { 
    spdf <- data.frame(IUCNName = sp,
                       Taxon = CITES_sp,
                       code = iucnOcc$result$code)
    df <- rbind(df, spdf)
  }
}

filter(df, is.na(code)) ## only the two species (extinct and hybrid)

## Get the distribution naming resolved
Distribution_full <- Distribution_List %>% select(-V1) %>% 
  mutate(IUCNName = NA, Distr_Source = "SpeciesPlus") %>%
  rbind(rename(df, "CITES_name" = "Taxon", "Distribution" = "code") %>% 
          mutate(Distr_Source = "IUCNRedList")) %>% 
  mutate(Distr_name = ifelse(is.na(IUCNName), CITES_name, IUCNName)) %>%
  select(-IUCNName) %>%
  left_join(CITES_Aves_List, by = c("CITES_name" = "Taxon")) %>%
  filter(CITES_name %in% CITES_Aves_List$Taxon)

n_distinct(Distribution_full$CITES_name) ## 805
 
## Resolve LH data naming
check <- left_join(Distribution_full, select(Bird_et_al, Sequence, Taxon), by = c("IUCNName" = "Taxon"))
check %>% filter(is.na (Sequence)) %>% distinct(CITES_name, IUCNName)

Distribution_full <- Distribution_full %>% mutate(LH_name = case_when(IUCNName == "Amazilis amazilia" ~ "Amazilia amazilia",
                                                 IUCNName == "Elliotomyia chionogaster" ~ "Amazilia chionogaster",
                                                 IUCNName == "Saucerottia tobaci" ~ "Amazilia tobaci",
                                                 IUCNName == "Chlorestes notata" ~ "Chlorostilbon notatus",
                                                 IUCNName == "Riccordia swainsonii" ~ "Chlorostilbon swainsonii",
                                                 IUCNName == "Padda oryzivora" ~ "Lonchura oryzivora",
                                                 IUCNName == "Trichoglossus iris" ~ "Psitteuteles iris",
                                                 IUCNName == "Nicopsitta calthrapae" ~ "Psittacula calthrapae",
                                                 IUCNName == "Nicopsitta columboides" ~ "Psittacula columboides",
                                                 IUCNName == "Himalayapsitta cyanocephala" ~ "Psittacula cyanocephala",
                                                 IUCNName == "Alexandrinus eques" ~ "Psittacula eques",
                                                 IUCNName == "Palaeornis eupatria" ~ "Psittacula eupatria",
                                                 IUCNName == "Himalayapsitta finschii" ~ "Psittacula finschii",
                                                 IUCNName == "Himalayapsitta himalayana" ~ "Psittacula himalayana",
                                                 IUCNName == "Belocercus longicaudus" ~ "Psittacula longicauda",
                                                 IUCNName == "Himalayapsitta roseata" ~ "Psittacula roseata",
                                                 IUCNName == "Anas oustaleti" ~ "Anas oustaleti", ## EX
                                                 IUCNName == "Grus antigone" ~ "Antigone antigone",
                                                 IUCNName == "Grus canadensis" ~ "Antigone canadensis",
                                                 IUCNName == "Grus rubicunda" ~ "Antigone rubicunda",
                                                 IUCNName == "Grus vipio" ~ "Antigone vipio",
                                                 IUCNName == "Lophura imperialis" ~ "Lophura imperialis", ## hybrid
                                                 IUCNName == "Alexandrinus krameri" ~ "Psittacula krameri",
                                                 TRUE ~ IUCNName))

check2 <- left_join(Distribution_full, select(Bird_et_al, Sequence, Taxon), by = c("LH_name" = "Taxon"))
check2 %>% filter(is.na (Sequence)) %>% distinct(CITES_name, IUCNName) ## the EX and hybrid

## Coverage is complete for all LH Traits (bar the two species)
check_cover <- left_join(Distribution_full, Bird_et_al, by = c("LH_name" = "Taxon")) %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

## Add percentiles to Bird et al.
Bird_et_al_perc <- Bird_et_al %>% mutate(Max_longevity_Perc = ecdf(Bird_et_al$Max_longevity)(Max_longevity),
                                         Adult_survival_Perc = ecdf(Bird_et_al$Adult_survival)(Adult_survival),
                                         Age_at_first_breeding_Perc = ecdf(Bird_et_al$Age_at_first_breeding)(Age_at_first_breeding),
                                         GenLength_Perc = ecdf(Bird_et_al$GenLength)(GenLength),
                                         Z_Perc = ecdf(Bird_et_al$Z)(Z))


## Make the full naming key
Distr_LH_Full <- left_join(Distribution_full, 
                         select(Bird_et_al_perc, Taxon, Adult_survival, Age_at_first_breeding,
                                GenLength, Max_longevity, Z, 
                                Adult_survival_Perc, Age_at_first_breeding_Perc,
                                GenLength_Perc, Max_longevity_Perc, Z_Perc), by = c("LH_name" = "Taxon")) %>% 
  distinct()

## Get the Distr data
Distr_key <- Distr_LH_Full %>% distinct(CITES_name, Distr_name, IUCNName, LH_name, Distribution, Distr_Source)
## Get the LH data
LH_key <- Distr_LH_Full %>% distinct(CITES_name, Distr_name, IUCNName, LH_name, 
                                     Adult_survival, Age_at_first_breeding,
                                     GenLength, Max_longevity, Z,
                                     Adult_survival_Perc, Age_at_first_breeding_Perc,
                                     GenLength_Perc, Max_longevity_Perc, Z_Perc)

write.csv(Distr_LH_Full, "Outputs/Naming/Distr_Trait_Key.csv")
write.csv(Distr_key, "Outputs/Naming/Distr_Key.csv")
write.csv(LH_key, "Outputs/Naming/Trait_Key.csv")


#### Country nomenclature ####
## 12 countries do not have matches with out country data
## 3 of these are "various", "unknown" and "Intro from the sea"
## 9 of these are former territories of countries so will be ignored in the subsequent analysis.
left_join(CITES_Country_List, Neighbours_List) %>% filter(is.na(region))

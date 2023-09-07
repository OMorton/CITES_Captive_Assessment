###########################
##--- Data Assessment ---##
###########################


#### Packages and presets ####
options(scipen = 999, na.action = "na.pass")
.libPaths("C:/Packages") ## Set up for working from home.

library(tidyverse)

source("2_Assessment_Pipeline/Functions.R")

#### Data Read in ####

## Uses some non-relative paths to reach the single storage location of the master cites database
## as folder is too large to sensibly move into every project directory. Data available here 
## https://trade.cites.org/

Prep_term <- data_prep(CITES_path = "G:/My Drive/TUoS/Data_Sources/CITES/CITES_all_records_2022.1",
                       CITES_data = NA, 
                       return_full_data = TRUE, return_only_collated = FALSE,
                       output_path = "Outputs/Assessment_Data/BirdRept/Term_level/",
                       woe_path = "G:/My Drive/TUoS/Data_Sources/CITES/Conversion/Harfoot_Clean.csv",
                       focal_reporter = "E", focal_level = "term", focal_class = c("Aves", "Reptilia"))

CITES_AvesRept_Term <- Prep_term$Full_data_to_check
Full <- Prep_term$CITES_MASTER

Prep_WOE <- data_prep(CITES_path = "G:/My Drive/TUoS/Data_Sources/CITES/CITES_all_records_2022.1",
                  CITES_data = Prep_term$CITES_MASTER, 
                  return_full_data = FALSE, return_only_collated = TRUE,
                  output_path = "Outputs/Assessment_Data/BirdRept/WOE_level/",
                  woe_path = "G:/My Drive/TUoS/Data_Sources/CITES/Conversion/Harfoot_Clean.csv",
                  focal_reporter = "E", focal_level = "woe", focal_class = c("Aves", "Reptilia"))

CITES_AvesRept_WOE <- Prep_WOE$Full_data_to_check

## IUCN Data
IUCN_dat <- data.table::fread("Outputs/IUCN/BirdRept_IUCN_2000_2020.csv", na.strings = "") %>%
  select(-V1)

## Listings data
Listings_dat <- data.table::fread("Outputs/CITES/Listed_Time_Series.csv", na.strings = "") %>%
  select(-V1, -ADD, -DEL, -Change) %>%
  filter(Year > 1999) %>%
  ## missing ssp
  rbind(data.frame(Taxon = "Crotalus durissus unicolor", Year = 2000:2020, Appendix = "III",
                   Class = "Reptilia", Order = "Serpentes", Family = "Viperidae",
                   Genus = "Crotalus")) 

## Check species listed twice in a single year (split)
CheckL <-Listings_dat %>% group_by(Taxon,Year, Class, Order, Family, Genus) %>% tally() %>% filter(n >1)
split_sp <- 
  rbind(data.frame(Taxon = "Crocodylus acutus", Year = 2000:2020, Appendix = "I",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus"),
      data.frame(Taxon = "Crocodylus moreletii", Year = 2000:2020, Appendix = "I",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "I",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus"),
      data.frame(Taxon = "Crocodylus porosus", Year = 2000:2020, Appendix = "I",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus"),
      data.frame(Taxon = "Melanosuchus niger", Year = 2000:2020, Appendix = "I",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus")) 

Listings_dat <- Listings_dat %>% filter(!Taxon %in% unique(split_sp$Taxon)) %>%
  rbind(split_sp)


split_sp_EX <- rbind(data.frame(Taxon = "Crocodylus acutus", Year = 2005:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "CU"),
      data.frame(Taxon = "Crocodylus acutus", Year = 2017:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "CO"),
      data.frame(Taxon = "Crocodylus moreletii", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "MX"),
      data.frame(Taxon = "Crocodylus moreletii", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "BZ"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "TZ"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "BW"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "ET"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "KE"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "MG"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "MW"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "MZ"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "UG"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "ZA"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "ZM"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "ZW"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2005:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "NA"),
      data.frame(Taxon = "Crocodylus niloticus", Year = 2010:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "EG"),
      data.frame(Taxon = "Crocodylus porosus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "AU"),
      data.frame(Taxon = "Crocodylus porosus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "ID"),
      data.frame(Taxon = "Crocodylus porosus", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "PG"),
      data.frame(Taxon = "Crocodylus porosus", Year = 2017:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "MY"),
      data.frame(Taxon = "Melanosuchus niger", Year = 2000:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "EC"),
      data.frame(Taxon = "Melanosuchus niger", Year = 2007:2020, Appendix = "II",
                 Class = "Reptilia", Order = "Crocodylia", Family = "Crocodylidae",
                 Genus = "Crocodylus", Exporter = "BR")) 

## Distribution data
Distr_dat <- data.table::fread("Outputs/Naming/Distr_Trait_Key.csv", na.strings = "", header = TRUE) %>%
  select(-V1) %>%
  group_by(CITES_name, Distr_Source, Distr_name, IUCNName, LH_name, Adult_survival, Age_at_first_breeding,
           GenLength, Max_longevity, Z, Adult_survival_Perc, Age_at_first_breeding_Perc,
           GenLength_Perc, Max_longevity_Perc, Z_Perc) %>%
  summarise(Distribution = paste(Distribution, collapse=', '))

## Neighbouring countries data 
Neighbour_dat <- data.table::fread("Outputs/Countries/Neighbours_List.csv", na.strings = "", header = TRUE) %>%
  select(-V1) %>% 
  ## Sort out the NAs so they are not pasted as text (triple X wont be matched to any two letter iso code)
  mutate(borders2 = if_else(is.na(borders2), "XXX", borders2)) %>%
  group_by(Country, region) %>%
  summarise(Bordering = paste(borders2, collapse=', '))

## Breeding register
CaptiveRegister <- data.table::fread("Outputs/CITES/CaptiveRegister_List.csv", na.strings = "", header = TRUE) %>%
  select(-V1) %>%
  ## just get presence of species in country with approved breeder
  group_by(ISO, `Species bred`) %>%
  summarise(Registered_breeders = paste(Code, collapse=', '))
  

#### Checklist data ####
AvesRept_to_check_term <- CITES_AvesRept_Term %>% 
  ## Add IUCN
  left_join(IUCN_dat, by = c("Taxon", "Year")) %>%
  ## Add Appendix
  left_join(Listings_dat, by = c("Taxon", "Year", "Class", "Order")) %>%
  ## Add split listed apps
  left_join(split_sp_EX, by = c("Taxon", "Year", "Class", "Family", "Genus", "Order", "Exporter")) %>%
  ## Sort out the splits
  mutate(Appendix = ifelse(is.na(Appendix.y), Appendix.x, Appendix.y)) %>%
  select(-Appendix.x, -Appendix.y) %>%
  ## Add registered captive breeders
  left_join(CaptiveRegister, by = c("Taxon" = "Species bred", "Exporter" = "ISO")) %>%
  ## Add species distribution data 
  left_join(Distr_dat, by = c("Taxon" = "CITES_name", "IUCNName")) %>%
  ## Add geographic neighbour data
  left_join(Neighbour_dat, by = c("Exporter" = "Country")) %>%
  mutate(Bordering = ifelse(is.na(Bordering), "XXX", Bordering))

AvesRept_to_check_WOE <- CITES_AvesRept_WOE %>% 
  ## Add IUCN
  left_join(IUCN_dat, by = c("Taxon", "Year")) %>%
  ## Add Appendix
  left_join(Listings_dat, by = c("Taxon", "Year", "Class", "Order")) %>%
  ## Add split listed apps
  left_join(split_sp_EX, by = c("Taxon", "Year", "Class", "Family", "Genus", "Order", "Exporter")) %>%
  ## Sort out the splits
  mutate(Appendix = ifelse(is.na(Appendix.y), Appendix.x, Appendix.y)) %>%
  select(-Appendix.x, -Appendix.y) %>%
  ## Add registered captive breeders
  left_join(CaptiveRegister, by = c("Taxon" = "Species bred", "Exporter" = "ISO")) %>%
  ## Add species distribution data 
  left_join(Distr_dat, by = c("Taxon" = "CITES_name", "IUCNName")) %>%
  ## Add geographic neighbour data
  left_join(Neighbour_dat, by = c("Exporter" = "Country")) %>%
  mutate(Bordering = ifelse(is.na(Bordering), "XXX", Bordering))

#### Data Checks ####

AvesRept_checked_term <- captive_assess(data = AvesRept_to_check_term, focal_reporter = "E", Class_for_traits = "Aves")
AvesRept_checked_WOE <- captive_assess(data = AvesRept_to_check_WOE, focal_reporter = "E", Class_for_traits = "Aves")

write.csv(AvesRept_checked_WOE, "Check.csv", na= "")

AvesRept_checked_WOE <- AvesRept_checked_WOE %>% filter(Check_7_8_9_INVALID == "VALID") %>%
  group_by(ROW_ID) %>% 
  mutate(Score_LH = case_when(Appendix == "I" ~
                                Check_1 + Check_2 +  Check_3 + Check_4 + Check_5 +
                                Check_6 + Check_7 + Check_8 + Check_9 + Check_10 +
                                Check_11 + Check_12 + Check_13 + Check_14,
                                Appendix != "I" ~
                                Check_1 + Check_2 + Check_3 + Check_4 + Check_5 +
                                Check_6 + Check_7 + Check_8 + Check_9 +
                                Check_14),
         Score = case_when(Appendix == "I" ~
                                Check_1 + Check_2 +  Check_3 + Check_4 + Check_5 +
                                Check_6 + Check_7 + Check_8 + Check_9 + Check_10 +
                                Check_11 + Check_12 + Check_13,
                                Appendix != "I" ~
                                Check_1 + Check_2 + Check_3 + Check_4 + Check_5 +
                                Check_6 + Check_7 + Check_8 + Check_9),
         Max_score = case_when(Appendix == "I"  ~ 13,
                               Appendix != "I"  ~ 9),
         Max_score_LH = case_when(Appendix == "I"  ~ 17,
                                  Appendix != "I"  ~ 13),
         App_sum = ifelse(Appendix == "I", "I", "II, III"))

AvesRept_checked_term <- AvesRept_checked_term %>% filter(Check_7_8_9_INVALID == "VALID") %>%
  group_by(ROW_ID) %>% 
  mutate(Score_LH = case_when(Appendix == "I" ~
                                Check_1 + Check_2 +  Check_3 + Check_4 + Check_5 +
                                Check_6 + Check_7 + Check_8 + Check_9 + Check_10 +
                                Check_11 + Check_12 + Check_13 + Check_14,
                              Appendix != "I" ~
                                Check_1 + Check_2 + Check_3 + Check_4 + Check_5 +
                                Check_6 + Check_7 + Check_8 + Check_9 +
                                Check_14),
         Score = case_when(Appendix == "I" ~
                             Check_1 + Check_2 +  Check_3 + Check_4 + Check_5 +
                             Check_6 + Check_7 + Check_8 + Check_9 + Check_10 +
                             Check_11 + Check_12 + Check_13,
                           Appendix != "I" ~
                             Check_1 + Check_2 + Check_3 + Check_4 + Check_5 +
                             Check_6 + Check_7 + Check_8 + Check_9),
         Max_score = case_when(Appendix == "I"  ~ 13,
                               Appendix != "I"  ~ 9),
         Max_score_LH = case_when(Appendix == "I"  ~ 17,
                                  Appendix != "I"  ~ 13),
         App_sum = ifelse(Appendix == "I", "I", "II, III"))

write.csv(AvesRept_checked_WOE, "Outputs/Full_Check/BirdsRept/AvesRept_checked_WOE.csv", na= "")
write.csv(AvesRept_checked_term, "Outputs/Full_Check/BirdsRept/AvesRept_checked_term.csv", na= "")


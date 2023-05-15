#####################
##--- Functions ---##
#####################


#### Packages and presets ####
options(scipen = 999, na.action = "na.pass")
.libPaths("C:/Packages") ## Set up for working from home.

library(tidyverse)

#### Data prep function ####

# CITES_path - path to the folder containing the 46 cites csv's (read in)
# CITES_data - if the cites data exists in memory no need to specify CITES_path just directly supply data
# output_path - path to the folder to write out the files for use in checking
# return_full_data - do you want the full 23 million row cites data to be given as an output.
# return_only_collated - save memory only return the collated 
# focal_reporter - Do you want to focus on ER or IR trends (ER recommended)
# focal_level - either "term" or "woe"
# woe_path - if the focal level is on woes give the path to the desired conversion table here.
# focal_class - taxonomic class to focus on

data_prep <- function(CITES_path = "G:/My Drive/TUoS/Data_Sources/CITES/CITES_all_records_2022.1",
                      CITES_data = NA,
                      output_path = "Outputs/Assessment_Data/Birds/Term_level/",
                      return_full_data = TRUE, return_only_collated = FALSE,
                      focal_reporter = "E", focal_level = "term", focal_class = "Aves",
                      woe_path = "G:/My Drive/TUoS/Data_Sources/CITES/Conversion/Harfoot_Clean.csv")
{
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)
  
  if(all(is.na(CITES_data))) {
  
  cat("Reading in CITES data\n")
  
  ## Read in the 49 seperate .csv's from CITES latest Bulk release 2022 v1
  CITES_MASTER <- list.files(path=paste0(CITES_path), 
                           full.names = TRUE, pattern="*.csv") %>% 
    lapply(read_csv, na = "", col_types = cols(Unit = col_character(), 
                                             Import.permit.RandomID = col_character(),
                                             Export.permit.RandomID = col_character(), 
                                             Origin.permit.RandomID = col_character(),
                                             Purpose = col_character())) %>% 
    bind_rows
  } else {
    CITES_MASTER <- CITES_data
  }

  ## Remove all re-exports as per published CITES guidelines and Pers Comm with UNEP-WMC and CITES
  ## remove all reports where the origin is stated and is not the same as the exporter.
  CITES_TRUE <- CITES_MASTER %>% filter(Origin == Exporter  | is.na(Origin), 
                                        Unit == "Number of specimens" | is.na(Unit),
                                        Class %in% focal_class) %>%
    ## correct the two potential misidents in the data
    mutate(Taxon = ifelse(Taxon == "Poephila cincta", "Poephila cincta cincta", Taxon),
           Taxon = ifelse(Taxon == "Lophura hatinhensis", "Lophura edwardsi", Taxon))

  App_group <- c("Year", "Taxon", "Class", "Order", "Exporter", "Appendix", "Term")
  Sum_group <- c("Year", "Taxon", "Class", "Order", "Exporter", "Term")
  Hist_group <- c("Taxon", "Class", "Order", "Exporter", "Term", "ROW_ID")
  
  if(focal_level == "woe") {
    
    cat("Standardising records to WOEs\n")
    
    WOE_Factors <- data.table::fread(woe_path, na.strings = "")
    WOE_Factors_all <- WOE_Factors %>% filter(Species_specific == 0) %>% select(Class, Term, Factor)
    WOE_Factors_sp <- WOE_Factors %>% filter(Species_specific == 1) %>% select(Class, Term, Taxa_applicable, Factor)

## Match the trade terms with the WOE conversion terms and add the factor for conversion.
    CITES_TRUE <- left_join(CITES_TRUE, WOE_Factors_all, by = c("Class", "Term")) %>% 
                  left_join(WOE_Factors_sp, by = c("Class", "Term", "Taxon" = "Taxa_applicable")) %>%
                  mutate(Factor = ifelse(is.na(Factor.y), Factor.x, Factor.y)) %>%
                  select(-Factor.x, -Factor.y) %>%
                  ## Calculate WOEs - you take the quantity traded multiplied by conversion term.
                  mutate(Quantity = Factor*Quantity) %>%
                  ## Keep only records that could be converted
                  filter(!is.na(Quantity))
  
    App_group <- c("Year", "Taxon", "Class", "Order", "Exporter", "Appendix")
    Sum_group <- c("Year", "Taxon", "Class", "Order", "Exporter")
    Hist_group <- c("Taxon", "Class", "Order", "Exporter", "ROW_ID")
    }

  if(focal_reporter == "E") {
    contrast_reporter = "I"
  } else {
    contrast_reporter = "E"
  }
cat("Targetting class: **", focal_class, "**, at the **", focal_level, "** level\n")
  #### Get the CITESbird data to test ####
  
  cat("Summarising CITES data\n")
  
  ## Focus on only the target taxa and time frame and remove ambiguous records using "ssp" or "hybrid"
  ## 30,868 records
  CITES_Focal_Capt <- CITES_TRUE %>% 
    filter(Class %in% focal_class, Year %in% 2000:2020,
           ## R could also be added to align with UNEP
           Source %in% c("C", "D", "F"), 
           ## Focus on ER trade in number of individuals
           Reporter.type == focal_reporter, 
           Appendix != "N") %>%
    ## Summarise to grouped vol
    group_by_at(App_group) %>% 
    summarise(Vol = sum(Quantity)) %>%
    filter(!grepl("spp", Taxon), !grepl("hybrid", Taxon))
  
  ## Check all 28 species that appear in multiple appendices in a single year (check that none are truly split listed.)
  Check <- CITES_Focal_Capt %>% group_by_at(Sum_group) %>% tally() %>% 
    filter(n >1) %>% ungroup() %>% distinct(Taxon)
  
  cat(paste0("Warning: There were ", n_distinct(Check), " species that appeared in multiple Appendices from 1 Exporter. See Check.\n" ))
  
  ## Output main data of summed term/woe records to check
  CITES_Focal_Capt <- CITES_Focal_Capt %>%
    ## Summarise to grouped vol
    group_by_at(Sum_group) %>% 
    summarise(Vol = sum(Vol)) %>%
    ungroup() %>%
    ## add unique id
    mutate(ROW_ID = row_number())
  
  #### Historic imports ####
  
  cat("Compiling historic live imports to exporting country\n")
  
  ## get the live imports of any kind per importer per year
  Historic_live_imports <- CITES_TRUE %>% 
    filter(Class %in% focal_class, Term == "live") %>%
    group_by(Year, Taxon, Class, Importer) %>% 
    summarise(Vol = sum(Quantity)) %>%
    filter(!grepl("spp", Taxon), !grepl("hybrid", Taxon)) %>% 
    group_by(Taxon, Importer) %>%
    summarise(Years_imported = paste(Year, collapse=', '))
  
  #### Contrast reporter records ####
  
  cat(paste0("Compiling contrasting ", contrast_reporter, " reported trade records\n" ))
  
  reporter_contrast_sum <- function(data = CITES_TRUE, codes = c("C", "D", "F")) {
    
    if(all(codes %in% c("C", "D", "F"))) {
      lab = "Capt"
    }
    if(all(codes %in% c("R"))) {
      lab = "Ranch"
    }
    if(all(codes %in% c("W", "U", "X"))) {
      lab = "Wild"
    }
    
    CITES_TRUE %>% 
      filter(Class %in% focal_class, Year %in% 2000:2020,
             Source %in% codes, 
             Reporter.type == contrast_reporter, Unit == "Number of specimens" | is.na(Unit), 
             Appendix != "N") %>%
      group_by_at(Sum_group) %>% 
      summarise(!!paste0(lab, "_Vol_", contrast_reporter, "R") := sum(Quantity)) %>%
      filter(!grepl("spp", Taxon), !grepl("hybrid", Taxon)) %>%
      ungroup()
    }
  
  ## Capt
  CITES_Capt_Contrast <- reporter_contrast_sum(data = CITES_TRUE,  codes = c("C", "D", "F"))
  ## Wild
  CITES_Wild_Contrast <- reporter_contrast_sum(data = CITES_TRUE,  codes = c("W", "X", "U"))
  ## Ranch
  CITES_Ranch_Contrast <- reporter_contrast_sum(data = CITES_TRUE,  codes = c("R"))
  
  
  #### Historic source records ####
  
  cat("Compiling historic CITES data across sources\n")
  
  focal_history <- function(data = CITES_TRUE, codes = c("C", "D", "F")) {
          if(all(codes %in% c("C", "D", "F"))) {
            lab = "Capt"
          }
          if(all(codes %in% c("R"))) {
            lab = "Ranch"
          }
          if(all(codes %in% c("W", "U", "X"))) {
            lab = "Wild"
          }
          CITES_Taxa_Hist <- CITES_TRUE %>% 
          filter(Class %in% focal_class, Year %in% 1995:2020,
                 ## R could also be added to align with UNEP
                 Source %in% codes, 
                 Reporter.type == focal_reporter, Unit == "Number of specimens" | is.na(Unit), 
                 Appendix != "N") %>%
          group_by_at(Sum_group) %>% 
          summarise(Vol = sum(Quantity)) %>%
          filter(!grepl("spp", Taxon), !grepl("hybrid", Taxon)) %>%
          ungroup()
  
  
         CITES_Focal_Capt %>% 
            group_by_at(Hist_group) %>%
            summarise(Year = seq(from = Year - 5, to = Year, length.out = 6)) %>%
            left_join(CITES_Taxa_Hist,
                      by = Sum_group) %>%
            mutate(Hist_vol = ifelse(is.na(Vol), 0, Vol)) %>% 
            arrange(ROW_ID, Year) %>%
            group_by(ROW_ID) %>%
            mutate(Year_Past = c("Year_5", "Year_4", "Year_3", "Year_2", "Year_1", "Year_0")) %>%
            select(ROW_ID, Year_Past, Year, Hist_vol) %>%
            pivot_wider(names_from = Year_Past, values_from = c(Hist_vol, Year)) %>%
            rename(!!paste0("Year_5_vol_", lab) := 2, !!paste0("Year_4_vol_", lab) := 3, 
                   !!paste0("Year_3_vol_", lab) := 4, !!paste0("Year_2_vol_", lab) := 5, 
                   !!paste0("Year_1_vol_", lab) := 6, !!paste0("Year_0_vol_", lab) := 7,
                   !!paste0("Year_5_", lab) := 8, !!paste0("Year_4_", lab) := 9, 
                   !!paste0("Year_3_", lab) := 10, !!paste0("Year_2_", lab) := 11, 
                   !!paste0("Year_1_", lab) := 12, !!paste0("Year_0_", lab) := 13)
  }
  
  ## Hist Capt
  CITES_focal_hist_capt <- focal_history(data = CITES_TRUE, codes = c("C", "D", "F"))
  ## Hist wild
  CITES_focal_hist_wild <- focal_history(data = CITES_TRUE, codes = c("W", "U", "X"))
  ## Hist ranch
  CITES_focal_hist_ranch <- focal_history(data = CITES_TRUE, codes = c("R"))
  
  #### Collating the data ####
  
  cat("Collating data to check\n")
  
  Full_data_to_check <- CITES_Focal_Capt %>%
    ## Add historic live import records to the exporting country
    left_join(Historic_live_imports, by = c("Taxon", "Exporter" = "Importer")) %>%
    ## Add other reporting source captive trade
    left_join(CITES_Capt_Contrast, by = Sum_group) %>%
    mutate(Capt_Vol_IR = if_else(is.na(Capt_Vol_IR), 0, Capt_Vol_IR)) %>%
    ## Add other reporting source wild trade
    left_join(CITES_Wild_Contrast, by = Sum_group) %>%
    mutate(Wild_Vol_IR = if_else(is.na(Wild_Vol_IR), 0, Wild_Vol_IR)) %>%
    ## Add other reporting source ranch trade
    left_join(CITES_Ranch_Contrast, by = Sum_group) %>%
    mutate(Ranch_Vol_IR = if_else(is.na(Ranch_Vol_IR), 0, Ranch_Vol_IR)) %>%
    ## Add historic capt trade
    left_join(CITES_focal_hist_capt, by = "ROW_ID") %>%
    ## Add historic wild trade
    left_join(CITES_focal_hist_wild, by = "ROW_ID") %>%
    ## Add historic ranch trade
    left_join(CITES_focal_hist_ranch, by = "ROW_ID")
  
  #### Writing out data block ####
  
  cat(paste0("Writing out data to ", output_path))
  
  write.csv(Full_data_to_check, 
            paste0(output_path, "Full_data_to_check_", paste(focal_class, collapse = ""), "_", focal_level, "_", focal_reporter, "R.csv"), na = "")
  
  write.csv(CITES_Focal_Capt, 
            paste0(output_path, "Capt_", paste(focal_class, collapse = ""), "_", focal_level, "_", focal_reporter, "R.csv"), na = "")
  write.csv(Historic_live_imports, 
            paste0(output_path, "All_historic_live_imports_", paste(focal_class, collapse = ""), "_", focal_level, ".csv"), na = "") 
  
  write.csv(CITES_Capt_Contrast, 
            paste0(output_path, "Capt_",paste(focal_class, collapse = ""), "_", focal_level, "_", contrast_reporter, "R.csv"), na = "")
  write.csv(CITES_Wild_Contrast, 
            paste0(output_path, "Wild_",paste(focal_class, collapse = ""), "_", focal_level, "_", contrast_reporter, "R.csv"), na = "")
  write.csv(CITES_Ranch_Contrast, 
            paste0(output_path, "Ranch_",paste(focal_class, collapse = ""), "_", focal_level, "_", contrast_reporter, "R.csv"), na = "")
  
  write.csv(CITES_focal_hist_capt, 
            paste0(output_path, "Hist_Capt_", paste(focal_class, collapse = ""), "_", focal_level, "_", focal_reporter, "R.csv"), na = "")
  write.csv(CITES_focal_hist_wild, 
            paste0(output_path, "Hist_Wild_", paste(focal_class, collapse = ""), "_", focal_level, "_", focal_reporter, "R.csv"), na = "")
  write.csv(CITES_focal_hist_ranch, 
            paste0(output_path, "Hist_Ranch_", paste(focal_class, collapse = ""), "_", focal_level, "_", focal_reporter, "R.csv"), na = "")
  
  if(return_full_data == TRUE & return_only_collated == FALSE) {
    return(list("CITES_MASTER" = CITES_MASTER, 
                "CITES_Focal_Capt" = CITES_Focal_Capt, "Historic_live_imports" = Historic_live_imports,
                "CITES_Capt_Contrast" = CITES_Capt_Contrast, "CITES_Wild_Contrast" = CITES_Wild_Contrast,
                "CITES_Ranch_Contrast" = CITES_Ranch_Contrast,
                "CITES_focal_hist_capt" = CITES_focal_hist_capt, "CITES_focal_hist_wild" = CITES_focal_hist_wild,
                "CITES_focal_hist_ranch" = CITES_focal_hist_ranch, 
                "Check" = Check, "Full_data_to_check" = Full_data_to_check))
  } 
  if(return_full_data == FALSE & return_only_collated == FALSE) {
    return(list("CITES_Focal_Capt" = CITES_Focal_Capt, "Historic_live_imports" = Historic_live_imports,
                "CITES_Capt_Contrast" = CITES_Capt_Contrast, "CITES_Wild_Contrast" = CITES_Wild_Contrast,
                "CITES_Ranch_Contrast" = CITES_Ranch_Contrast,
                "CITES_focal_hist_capt" = CITES_focal_hist_capt, "CITES_focal_hist_wild" = CITES_focal_hist_wild,
                "CITES_focal_hist_ranch" = CITES_focal_hist_ranch, 
                "Check" = Check, "Full_data_to_check" = Full_data_to_check))
    }
  if(return_full_data == FALSE & return_only_collated == TRUE) {
    return(list("Check" = Check, "Full_data_to_check" = Full_data_to_check))
  }
  if(return_full_data == TRUE & return_only_collated == TRUE) {
    return(list("CITES_MASTER" = CITES_MASTER, "Check" = Check, "Full_data_to_check" = Full_data_to_check))
  }

}

#### Assessment criteria function ####

# data - supply the data (output Full_data_to_check from data_prep() function)
# focal_reporter - either E or I (E recommended)
# Class_for_traits - Aves or NA

captive_assess <- function(data = data, focal_reporter = "E", Class_for_traits = "Aves") 
  {
  
  ## Set up the contrast reporter source
  contrast_reporter = "I"
  
  if(focal_reporter == "I") {
    contrast_reporter = "E"
  }
  
  ## Error catch for if data_prep() column format supplied
  if(any(!c("Year_5_vol_Capt", "Year_4_vol_Capt", "Year_3_vol_Capt", "Year_2_vol_Capt", 
        "Year_1_vol_Capt", "Year_0_vol_Capt",
        "Year_5_vol_Ranch", "Year_4_vol_Ranch", "Year_3_vol_Ranch", 
        "Year_2_vol_Ranch", "Year_1_vol_Ranch", "Year_0_vol_Ranch",
        "Year_5_vol_Wild", "Year_4_vol_Wild", "Year_3_vol_Wild", "Year_2_vol_Wild", 
        "Year_1_vol_Wild",  "Year_0_vol_Wild",
        paste0("Wild_Vol_", contrast_reporter, "R"), 
        paste0("Ranch_Vol_", contrast_reporter, "R"),
        paste0("Capt_Vol_", contrast_reporter, "R"), 
        "Registered_breeders", "Distribution", "Exporter", "Bordering", 
        "Years_imported", "ROW_ID") %in% colnames(data))){
    stop("Columns not named in the format supplied by data_prep()")
    
  }
  
  ## Rather than use paste/!=/!! inside dplyr verbs rename the IR/ER 
  ## ending variables to end in _Contrast
  colnames(data) <- gsub(x = colnames(data), pattern = paste0(contrast_reporter, "R"), 
                         replacement = "Contrast")  
  
  data2 <- data %>% group_by(ROW_ID) %>% mutate(
    ## 1 - Aligns with AC31 Doc 19.1 Criteria i) Sig Increase
    ## Is the current records volumes more than double the 
    ## mean of the last 5 years trade?
    Check_1 = 
      Vol > 2*mean(c(Year_5_vol_Capt, 
                   Year_4_vol_Capt,
                   Year_3_vol_Capt,
                   Year_2_vol_Capt,
                   Year_1_vol_Capt)),
    
    ## 2 - Aligns with AC31 Doc 19.1 Criteria i) Significant increase
    ## Is the current records volumes more than quadruple the 
    ## mean of the last 5 years trade?
    Check_2 = 
      Vol > 4*mean(c(Year_5_vol_Capt, 
                   Year_4_vol_Capt,
                   Year_3_vol_Capt,
                   Year_2_vol_Capt,
                   Year_1_vol_Capt)),
    
    ## 3 - Aligns with AC31 Doc 19.1 Criteria ii) Significant numbers
    ## The criteria applied uses multipliers - justification unclear.
    ## this currently just returns the volume and can be used to weight trades
    ## later.
    Check_3 = case_when(IUCN_code %in% c("NT", "VU", "EN", "CR", "DD", "EX", "EW", "Not assessed") &
                        Vol > 12.5 ~ TRUE,
                        IUCN_code == "LC" & Vol > 50 ~ TRUE,
                        TRUE ~ FALSE),
    
    ## 4 - Aligns with AC31 Doc 19.1 Criteria iii) Shifts in source codes
    ## Phrasing in the doc uses unclear language (doubling) that appears to not
    ## account for differing scales.
    ## Is there evidence of shifts in source codes?
    ## Ranched Version
    Check_4 = case_when(
      ## check captive trend is increasing 
      Check_1 == TRUE & 
        ## Get the captive increase relative to 5-year captive mean
        abs((Vol - mean(c(Year_5_vol_Capt, 
                        Year_4_vol_Capt,
                        Year_3_vol_Capt,
                        Year_2_vol_Capt,
                        Year_1_vol_Capt))) +
              ## Get the ranched change relative to 5-year ranch mean
              (Year_0_vol_Ranch - mean(c(Year_5_vol_Ranch,
                                       Year_4_vol_Ranch,
                                       Year_3_vol_Ranch,
                                       Year_2_vol_Ranch,
                                       Year_1_vol_Ranch))))  <=
      ## Get the absolute difference between the sources e.g. would be 
      ## zero if the increase in captive trade was fully matched by a
      ## decrease in ranched trade.
        
      ## Check the absolute difference is at least 50% of the largest shift.
      ## This ensure that the differences are the majority absorbed by the other
      ## source and prevents instances like where captive trade doubles from 
      ## 1000 to 2000 and ranched trade halves from 20 to 10. One doubles and
      ## the other halves but the relative scales are so disparate to be off no
      ## plausible association.
        0.5*max(abs((Vol - mean(c(Year_5_vol_Capt, 
                                Year_4_vol_Capt,
                                Year_3_vol_Capt,
                                Year_2_vol_Capt,
                                Year_1_vol_Capt)))),
                abs(Year_0_vol_Ranch - mean(c(Year_5_vol_Ranch,
                                            Year_4_vol_Ranch,
                                            Year_3_vol_Ranch,
                                            Year_2_vol_Ranch,
                                            Year_1_vol_Ranch)))) ~ TRUE,
      ## Now the decrease
      Vol < 0.5*mean(c(Year_5_vol_Capt, 
                     Year_4_vol_Capt,
                     Year_3_vol_Capt,
                     Year_2_vol_Capt,
                     Year_1_vol_Capt)) &
        ## Get the captive increase relative to 5-year captive mean
        abs((Vol - mean(c(Year_5_vol_Capt, 
                          Year_4_vol_Capt,
                          Year_3_vol_Capt,
                          Year_2_vol_Capt,
                          Year_1_vol_Capt))) +
              ## Get the ranched change relative to 5-year ranch mean
              (Year_0_vol_Ranch - mean(c(Year_5_vol_Ranch,
                                         Year_4_vol_Ranch,
                                         Year_3_vol_Ranch,
                                         Year_2_vol_Ranch,
                                         Year_1_vol_Ranch))))  <=
        0.5*max(abs((Vol - mean(c(Year_5_vol_Capt, 
                                  Year_4_vol_Capt,
                                  Year_3_vol_Capt,
                                  Year_2_vol_Capt,
                                  Year_1_vol_Capt)))),
                abs(Year_0_vol_Ranch - mean(c(Year_5_vol_Ranch,
                                              Year_4_vol_Ranch,
                                              Year_3_vol_Ranch,
                                              Year_2_vol_Ranch,
                                              Year_1_vol_Ranch)))) ~ TRUE,
                     .default = FALSE),
    
    ## 5 - Aligns with AC31 Doc 19.1 Criteria iii) Shifts in source codes
    ## Phrasing in the doc uses unclear language (doubling) that appears to not
    ## account for differing scales.
    ## Is there evidence of shifts in source codes?
    ## Wild Version
    Check_5 = case_when(
      Check_1 == TRUE & 
        ## check captive trend is increasing 
        abs((Vol - mean(c(Year_5_vol_Capt, 
                        Year_4_vol_Capt,
                        Year_3_vol_Capt,
                        Year_2_vol_Capt,
                        Year_1_vol_Capt))) +
              ## Get the wild change relative to 5-year ranch mean
              (Year_0_vol_Wild - mean(c(Year_5_vol_Wild,
                                      Year_4_vol_Wild,
                                      Year_3_vol_Wild,
                                      Year_2_vol_Wild,
                                      Year_1_vol_Wild)))) <=
        ## Get the absolute difference between the sources e.g. would be 
        ## zero if the increase in captive trade was fully matched by a
        ## decrease in ranched trade.
        
        ## Check the absolute difference is at least 50% of the largest shift.
        ## This ensure that the differences are the majority absorbed by the other
        ## source and prevents instances like where captive trade doubles from 
        ## 1000 to 2000 and ranched trade halves from 20 to 10. One doubles and
        ## the other halves but the relative scales are so disparate to be off no
        ## plausible association.
        0.5*max(abs((Vol - mean(c(Year_5_vol_Capt, 
                                 Year_4_vol_Capt,
                                 Year_3_vol_Capt,
                                 Year_2_vol_Capt,
                                 Year_1_vol_Capt)))),
                 abs(Year_0_vol_Wild - mean(c(Year_5_vol_Wild,
                                            Year_4_vol_Wild,
                                            Year_3_vol_Wild,
                                            Year_2_vol_Wild,
                                            Year_1_vol_Wild)))) ~ TRUE, 
      Vol < 0.5*mean(c(Year_5_vol_Capt, 
                       Year_4_vol_Capt,
                       Year_3_vol_Capt,
                       Year_2_vol_Capt,
                       Year_1_vol_Capt)) &
        abs((Vol - mean(c(Year_5_vol_Capt, 
                          Year_4_vol_Capt,
                          Year_3_vol_Capt,
                          Year_2_vol_Capt,
                          Year_1_vol_Capt))) +
              ## Get the wild change relative to 5-year ranch mean
              (Year_0_vol_Wild - mean(c(Year_5_vol_Wild,
                                        Year_4_vol_Wild,
                                        Year_3_vol_Wild,
                                        Year_2_vol_Wild,
                                        Year_1_vol_Wild)))) <=
        0.5*max(abs((Vol - mean(c(Year_5_vol_Capt, 
                                  Year_4_vol_Capt,
                                  Year_3_vol_Capt,
                                  Year_2_vol_Capt,
                                  Year_1_vol_Capt)))),
                abs(Year_0_vol_Wild - mean(c(Year_5_vol_Wild,
                                             Year_4_vol_Wild,
                                             Year_3_vol_Wild,
                                             Year_2_vol_Wild,
                                             Year_1_vol_Wild)))) ~ TRUE,
      .default =  FALSE),
    
    ## For subsequent use check are total ER and IR reported volumes roughly
    ## equivalent (plus/minus 25%)
    Check_6_7_equivalent_reporting =
      ifelse((Capt_Vol_Contrast + Ranch_Vol_Contrast + Wild_Vol_Contrast) <= 
               (Vol + Year_0_vol_Wild + Year_0_vol_Ranch)*1.25 &
               (Capt_Vol_Contrast + Ranch_Vol_Contrast + Wild_Vol_Contrast) >= 
               (Vol + Year_0_vol_Wild + Year_0_vol_Ranch)*0.75,
             TRUE, FALSE),
    
    ## 6 - Aligns with AC31 Doc 19.1 Criteria iv) Reporting inconsistencies
    ## Assesses captive to wild source switching in ER and IR trade.
    ## Is there evidence that reporters source codes switch/diagree?
    Check_6 = ifelse(
      ## Checks that total volumes are largely equivalent and therefore 
      ## the proportions can be compared
      Check_6_7_equivalent_reporting == TRUE &
        ## Check that the difference in proportion between ER and IR reported 
        ## captive trade is >10%
                       (abs(Vol/(Vol + Year_0_vol_Wild) -
                          Capt_Vol_Contrast/(Capt_Vol_Contrast + Wild_Vol_Contrast)) >= 0.1) &
        ## Check there is a corresponding difference in ER and IR reported
        ## wild trade.
        ## This is important to confirm there is a compensatory change in both
        ## sources
                       (abs(Year_0_vol_Wild/(Vol + Year_0_vol_Wild) -
                          Wild_Vol_Contrast/(Capt_Vol_Contrast + Wild_Vol_Contrast)) >= 0.1) &
        ## Final step is just to confirm that one change is an increase (+) 
        ## and the other is a decrease (-).
                       sign(Year_0_vol_Wild/(Vol + Year_0_vol_Wild) -
                              Wild_Vol_Contrast/(Capt_Vol_Contrast + Wild_Vol_Contrast)) != 
                       sign(Vol/(Vol + Year_0_vol_Wild) -
                              Capt_Vol_Contrast/(Capt_Vol_Contrast + Wild_Vol_Contrast)),
                     TRUE, FALSE),
    
    ## 7 - Aligns with AC31 Doc 19.1 Criteria iv) Reporting inconsistencies
    ## Assesses captive to ranch source switching in ER and IR trade.
    ## Is there evidence that reporters source codes switch/diagree?    
    Check_7 = ifelse(
      ## Checks that total volumes are largely equivalent and therefore 
      ## the proportions can be compared
      Check_6_7_equivalent_reporting == TRUE &
        ## Check that the difference in proportion between ER and IR reported 
        ## captive trade is >10%
                       (abs(Vol/(Vol + Year_0_vol_Ranch) -
                          Capt_Vol_Contrast/(Capt_Vol_Contrast + Ranch_Vol_Contrast)) >= 0.1) &
        ## Check there is a corresponding difference in ER and IR reported
        ## ranched trade.
        ## This is important to confirm there is a compensatory change in both
        ## sources
                       (abs(Year_0_vol_Ranch/(Vol + Year_0_vol_Ranch) -
                          Ranch_Vol_Contrast/(Capt_Vol_Contrast + Ranch_Vol_Contrast)) >= 0.1) &
        ## Final step is just to confirm that one change is an increase (+) 
        ## and the other is a decrease (-).
                       sign(Year_0_vol_Ranch/(Vol + Year_0_vol_Ranch) -
                              Ranch_Vol_Contrast/(Capt_Vol_Contrast + Ranch_Vol_Contrast)) != 
                       sign(Vol/(Vol + Year_0_vol_Ranch) -
                              Capt_Vol_Contrast/(Capt_Vol_Contrast + Ranch_Vol_Contrast)),
                     TRUE, FALSE),
    
    ## 8 - IR are not within 25% of ER
    ## Not sure whether this check will be kept
    Check_8 = if_else(Capt_Vol_Contrast >= Vol*1.25 |
                        Capt_Vol_Contrast <= Vol*0.75,
                      TRUE, FALSE),
    
    ## 9 - Aligns with AC31 Doc 19.1 Criterion v) Incorrect application of 
    ## source codes
    ## No registered breeder?
    Check_9 = is.na(Registered_breeders),
    
    ## Check that the exporter country is still recognized
    Check_10_11_12_INVALID = ifelse(Exporter %in% c("YU", "CS"), 
                                    "INVALID", "VALID"),
    
    ## 10 - Aligns with AC31 Doc 19.1 Criterion vi) Legal acquisition
    ## Is the species exported from a non range state?
    Check_10 = ifelse(str_detect(Distribution, Exporter), 
                      FALSE, TRUE),
    
    ## 11 - Aligns with AC31 Doc 19.1 Criterion vi) Legal acquisition
    ## The Exporter doesn't neighbor a range state?
    Check_11 = ifelse(grepl(pattern = paste(
      unlist(str_split(Bordering, pattern = ", ")), collapse = "|"), Distribution),
      FALSE, TRUE),
    
    ## 12 - Aligns with AC31 Doc 19.1 Criterion vi) Legal acquisition
    ## The species has never been imported live in previous years
    Check_12 = all(as.numeric(unlist(
      str_split(Years_imported, pattern = ", "))) >= Year)
  ) %>%
    ungroup() %>%
    ## Catch for species that were never imported 
    mutate(Check_12 = ifelse(is.na(Check_12), TRUE, Check_12)) %>%
    ## If the exporter is a range state (Check 10), Check 11 and 12 automatically
    ## pass.
    mutate(Check_11 = ifelse(Check_10 == FALSE, FALSE, Check_11),
           Check_12 = ifelse(Check_10 == FALSE, FALSE, Check_12),
           ## If the total contrast source trade was zero it generates instances
           ## of 0 / 0 in checks 7 and 8, this is a catch all for those instances
           ## records automatically pass as if there is no reported trade there 
           ## cannot be source switching.
           Check_6 = ifelse((Capt_Vol_Contrast + Wild_Vol_Contrast) == 0,
                            FALSE, Check_6),
           Check_7 = ifelse((Capt_Vol_Contrast + Ranch_Vol_Contrast) == 0,
                            FALSE, Check_7))
  
if(Class_for_traits == "Aves"){
  
  if(any(!c("Adult_survival_Perc", "Age_at_first_breeding_Perc", "GenLength_Perc", 
            "Max_longevity_Perc") %in% colnames(data))){
    stop("Bird percentile trait data not supplied")}
  
  ## 13 - Score out of 4 for how "slow" life history traits are
  data2 <- data2 %>% group_by(ROW_ID) %>% mutate(
    Check_13 = (Adult_survival_Perc <= .25) + 
    (Age_at_first_breeding_Perc >= .75) +
    (GenLength_Perc >= .75) +
    (Max_longevity_Perc >= .75)) %>%
    mutate(Check_13 = ifelse(Class == "Reptilia", NA, Check_13))
}
  return(data2)
}

#### Count/Volume per check tally shortcut ####

## data - a raw checks output from the assessment function
## groups - what groups do you want to summarise to?
## App1_only - logical condition whether to focus on criteria that apply to all
## or just criteria that specifally apply to App1
## format - long or wide data output

checks_summary <- function(data, groups, App1_only = FALSE, format = "long") {
  if(App1_only == TRUE) {
    
    sum <- data %>% filter(Appendix == "I") %>%
      group_by(!!!syms(groups)) %>% 
      summarise(Check_1_count = sum(Check_1), Check_1_vol =  sum(Vol[Check_1 == TRUE]),
                Check_2_count = sum(Check_2), Check_2_vol =  sum(Vol[Check_2 == TRUE]),
                Check_3_count = sum(Check_3), Check_3_vol =  sum(Vol[Check_3 == TRUE]),
                Check_4_count = sum(Check_4), Check_4_vol =  sum(Vol[Check_4 == TRUE]),
                Check_5_count = sum(Check_5), Check_5_vol =  sum(Vol[Check_5 == TRUE]),
                Check_6_count = sum(Check_6), Check_6_vol =  sum(Vol[Check_6 == TRUE]),
                Check_7_count = sum(Check_7), Check_7_vol =  sum(Vol[Check_7 == TRUE]),
                Check_8_count = sum(Check_8), Check_8_vol =  sum(Vol[Check_8 == TRUE]),
                Check_9_count = sum(Check_9), Check_9_vol =  sum(Vol[Check_9 == TRUE]),
                Check_10_count = sum(Check_10), Check_10_vol =  sum(Vol[Check_10 == TRUE]),
                Check_11_count = sum(Check_11), Check_11_vol =  sum(Vol[Check_11 == TRUE]),
                Check_12_count = sum(Check_12), Check_12_vol =  sum(Vol[Check_12 == TRUE]),
                Total_count = n(), Total_vol = sum(Vol),
                Check_1_count_prop = Check_1_count/Total_count, Check_1_vol_prop = Check_1_vol/Total_vol,
                Check_2_count_prop = Check_2_count/Total_count, Check_2_vol_prop = Check_2_vol/Total_vol,
                Check_3_count_prop = Check_3_count/Total_count, Check_3_vol_prop = Check_3_vol/Total_vol,
                Check_4_count_prop = Check_4_count/Total_count, Check_4_vol_prop = Check_4_vol/Total_vol,
                Check_5_count_prop = Check_5_count/Total_count, Check_5_vol_prop = Check_5_vol/Total_vol,
                Check_6_count_prop = Check_6_count/Total_count, Check_6_vol_prop = Check_6_vol/Total_vol,
                Check_7_count_prop = Check_7_count/Total_count, Check_7_vol_prop = Check_7_vol/Total_vol,
                Check_8_count_prop = Check_8_count/Total_count, Check_8_vol_prop = Check_8_vol/Total_vol,
                Check_9_count_prop = Check_9_count/Total_count, Check_9_vol_prop = Check_9_vol/Total_vol,
                Check_10_count_prop = Check_10_count/Total_count, Check_10_vol_prop = Check_10_vol/Total_vol,
                Check_11_count_prop = Check_11_count/Total_count, Check_11_vol_prop = Check_11_vol/Total_vol,
                Check_12_count_prop = Check_12_count/Total_count, Check_12_vol_prop = Check_12_vol/Total_vol)
  } else {
   sum <-  data %>% group_by(!!!syms(groups)) %>% 
      summarise(Check_1_count = sum(Check_1), Check_1_vol =  sum(Vol[Check_1 == TRUE]),
                Check_2_count = sum(Check_2), Check_2_vol =  sum(Vol[Check_2 == TRUE]),
                Check_3_count = sum(Check_3), Check_3_vol =  sum(Vol[Check_3 == TRUE]),
                Check_4_count = sum(Check_4), Check_4_vol =  sum(Vol[Check_4 == TRUE]),
                Check_5_count = sum(Check_5), Check_5_vol =  sum(Vol[Check_5 == TRUE]),
                Check_10_count = sum(Check_10), Check_10_vol =  sum(Vol[Check_10 == TRUE]),
                Check_11_count = sum(Check_11), Check_11_vol =  sum(Vol[Check_11 == TRUE]),
                Check_12_count = sum(Check_12), Check_12_vol =  sum(Vol[Check_12 == TRUE]),
                Total_count = n(), Total_vol = sum(Vol),
                Check_1_count_prop = Check_1_count/Total_count, Check_1_vol_prop = Check_1_vol/Total_vol,
                Check_2_count_prop = Check_2_count/Total_count, Check_2_vol_prop = Check_2_vol/Total_vol,
                Check_3_count_prop = Check_3_count/Total_count, Check_3_vol_prop = Check_3_vol/Total_vol,
                Check_4_count_prop = Check_4_count/Total_count, Check_4_vol_prop = Check_4_vol/Total_vol,
                Check_5_count_prop = Check_5_count/Total_count, Check_5_vol_prop = Check_5_vol/Total_vol,
                Check_10_count_prop = Check_10_count/Total_count, Check_10_vol_prop = Check_10_vol/Total_vol,
                Check_11_count_prop = Check_11_count/Total_count, Check_11_vol_prop = Check_11_vol/Total_vol,
                Check_12_count_prop = Check_12_count/Total_count, Check_12_vol_prop = Check_12_vol/Total_vol)
  }
  if(format == "long"){
  sum <- sum %>%
    pivot_longer(!c(!!!syms(groups), Total_count, Total_vol), 
                 names_to = c("Check",".value"), 
                 names_pattern = c("(\\w+\\d+)_(\\w+)")) %>%
    select(!!!syms(groups), Check, count, vol, Total_count, Total_vol, count_prop, vol_prop) %>%
    mutate(Focus = case_when(
      Check %in% c("Check_1", "Check_2", 
                   "Check_3", "Check_4", "Check_5") ~ "Trade trends",
      Check %in% c("Check_6", "Check_7", 
                   "Check_8", "Check_9") ~ "Reporting inconsistencies",
      Check %in% c("Check_10", "Check_11",
                   "Check_12") ~ "Legal acquisition",
      Check %in% c("Check_13") ~ "Species biology"),
      Check = factor(Check, levels = c("Check_1", "Check_2","Check_3", "Check_4",
                                       "Check_5", "Check_6", "Check_7", "Check_8", 
                                       "Check_9", "Check_10", "Check_11",
                                       "Check_12")))
  }
  
  return(sum)
}


##### Top 10 species data format ####

ID_dataseries <- function(data = data, ID_list) {
  Output <- data.frame()
  
  for (i in 1:length(ID_list)) {
    
    ID <- ID_list[i]
  
  Capt_vol <- data %>% filter(ROW_ID == ID) %>%
    select(ROW_ID, Taxon, Exporter, Year_5_vol_Capt, Year_4_vol_Capt, Year_3_vol_Capt, 
           Year_2_vol_Capt, Year_1_vol_Capt, Year_0_vol_Capt) %>%
    pivot_longer(!c(ROW_ID, Taxon, Exporter), names_to = "var", values_to = "Vol") %>% 
    select(Vol, ROW_ID, Taxon, Exporter) %>%
    mutate(Type = "Capt_vol")
  
  Wild_vol <- data %>% filter(ROW_ID == ID) %>%
    select(ROW_ID, Taxon, Exporter, Year_5_vol_Wild, Year_4_vol_Wild, Year_3_vol_Wild, 
           Year_2_vol_Wild, Year_1_vol_Wild, Year_0_vol_Wild) %>%
    pivot_longer(!c(ROW_ID, Taxon, Exporter), names_to = "var", values_to = "Vol") %>% 
    select(Vol, ROW_ID, Taxon, Exporter) %>%  mutate(Type = "Wild_vol")
  
  Ranch_vol <- data %>% filter(ROW_ID == ID) %>%
    select(ROW_ID,Taxon, Exporter, Year_5_vol_Ranch, Year_4_vol_Ranch, Year_3_vol_Ranch, 
           Year_2_vol_Ranch, Year_1_vol_Ranch, Year_0_vol_Ranch) %>%
    pivot_longer(!c(ROW_ID, Taxon, Exporter), names_to = "var", values_to = "Vol") %>% 
    select(Vol, ROW_ID, Taxon, Exporter) %>%  mutate(Type = "Ranch_vol")
  
  Contr_capt <- data %>% filter(ROW_ID == ID) %>% 
    select(Capt_Vol_Contrast, ROW_ID, Taxon, Year, Exporter) %>%
    rename("Vol" = 1) %>% mutate( Type = "Capt_contast")
  Contr_wild <- data %>% filter(ROW_ID == ID) %>% 
    select(Wild_Vol_Contrast, ROW_ID, Taxon, Year, Exporter) %>%
    rename("Vol" = 1) %>% mutate( Type = "Wild_contast")
  Contr_ranch <- data %>% filter(ROW_ID == ID) %>% 
    select(Ranch_Vol_Contrast, ROW_ID, Taxon, Year, Exporter) %>%
    rename("Vol" = 1) %>% mutate( Type = "Ranch_contast")
  
  Year_series <- data %>% filter(ROW_ID == ID) %>%
    select(ROW_ID, Year_5_Capt, Year_4_Capt, Year_3_Capt,
           Year_2_Capt, Year_1_Capt, Year_0_Capt) %>%
    pivot_longer(!ROW_ID, names_to = "var", values_to = "Year") %>% select(Year)
  
  dat <- rbind(cbind(Capt_vol, Year_series), cbind(Wild_vol, Year_series), 
               cbind(Ranch_vol, Year_series), Contr_capt, Contr_wild, Contr_ranch)
  
  Output <- rbind(Output, dat)
  }
return(Output)
}

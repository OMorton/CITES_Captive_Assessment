## CITES Captive Trade Assessment

The output of this is in submission as **Assessing and improving the veracity of trade in captive-bred animals** at the *Journal of Environmental Management*.

### File summary

#### 1. Data Preparation 

***1.1_SourceData_Format.R*** contains code to extract species lists from the CITES trade database and extract each species Listed times series and historic IUCN status over that time frame.

***1.2_SourceData_Naming.R*** processes this output, adds species distribution data and reconciles taxonomies between the IUCN, CITES and life history databases.


#### 2. Assessment Pipeline

***2.1_Run_Assessment.R*** collates historic, concurrent, importer and exporter trade and runs and summarizes the assessment criteria detailed in the main manuscript.

***2.2_Assessment_Interpretation.R*** processes the assessed trade records and summarizes the results of the targeted assessments.

***Functions.R*** contains a number of convenience functions written to ease the assessment process, the main two being `data_prep()` which processes the raw CITES database (removing re-exports, splitting by exporter types summaries trade into yearly, species, exporter totals, collating trade per purpose and source and collating historic trade for each record) and `captive_assess()`which runs the criteria and targetted assessments.

All the data used is publicly accessible and linked within the manuscript.

# CMS HCC Model V22 for Continuing Community Enrollees
#Assumes that these data frames have been imported into the R environment
#PERSON File Input
# 1. HICNO (or other person identification variable. Character or numeric type and unique to an individual
# 2. SEX -one character, 1=male; 2=female 
# 3. DOB - date of birth
# 4. MCAID -numeric, =1 if number of State Part B BUYIN (MEDICAID)Months of base year >0, =0 otherwise 
# 5. NEMCAID -numeric, =1 if a new enrollee and number of State Part B BUYIN (MEDICAID) months of payment year >0; =0 otherwise 
# 6. OREC - -one character, original reason for entitlement with the following values: 0 - OLD AGE (OASI), 1 - DISABILITY (DIB), 2 - ESRD, 3 - BOTH DIB AND ESRD
#DIAG File Input
# 1. HICNO (or other person identification variable that must be the same as in PERSON file) - person identifier of character or numeric type and unique to an individual 
# 2. DIAG - ICD-9-CM diagnosis code, 5 character field, no periods, left justified. The user may include all diagnoses or limit the codes to those used by the
# 	model. Codes should be to the greatest level of available specificity. Diagnoses should be included only from providers and physician specialties as
#	provided in prior notices.
# --------------------------------------------------------------
# Calculated fields
# Assume DOB is in yyyy-mm-dd format, calculate age from today
#Evaluate CMS-HCC risk adjustment score
#Machine-readable model definition for V22
#Fix documentation
#Remove dependency on icd package
#Extend to V21 and V23

library(stringr)
library(reshape2)
library(dplyr)

#' randomDate Function
#' @param size integer is the number of dates to simulate (default is 100)
#' @param seed integer is the random seed for generating random numbers (default is 2)
#' @param start_time string is the YYYY/MM/DD start time to start the sampled random dates (default is 1930/01/01)
#' @param end_time string is the YYYY/MM/DD end time to cut off the sampled random dates (default is today's date)
#' @return vector of random dates length size
#' @export
random_date <- function(size = 100, seed = 2, start_time = "1930/01/01", end_time = Sys.Date()) {
  start_time <- as.POSIXct(as.Date(start_time))
  end_time <- as.POSIXct(as.Date(end_time))
  date_time <- as.numeric(difftime(end_time, start_time, unit = "sec"))
  set.seed(seed)
  end_value <- sort(runif(size, 0, date_time))
  random_time <- start_time + end_value
  random_time <- as.Date(random_time)
  return(random_time)
}

#' generateTestPERSON
#' @param size integer is the number of dates to simulate (default is 100)
#' @param seed integer is the random seed starting point
#' @param start_time string is the YYYY/MM/DD start time to start the sampled random dates (default is 1930/01/01)
#' @param end_time string is the YYYY/MM/DD end time to cut off the sampled random dates (default is today's date)
#' @return Data Frame for the PERSON demographics information required for risk adjustment
#' @export
generate_PERSON <- function(size = 100, seed = 2, start_time = "1930/01/01", end_time = Sys.Date()) {
  set.seed(seed)
  HICNO <- 1:size
  SEX <- sample(x = c("M", "F"), size, replace = TRUE)
  DOB <- random_date(size, seed, start_time, end_time)
  MCAID <- sample(x = c(0, 1), size, replace = TRUE)
  NMCAID <- sample(x = c(0, 1), size, replace = TRUE)
  OREC <- sample(x = 0:3, size, replace = TRUE)
  PERSON <- data.frame(HICNO = HICNO, SEX = SEX, DOB = DOB, MCAID = MCAID, NMCAID = NMCAID, OREC = OREC, stringsAsFactors = FALSE)
  return(PERSON)
}

#' Generate sample diagnosis data
#'@param cmshcc_map data frame is the CMSHCC diagnosis to HCC mapping in question (Required)
#' @param size integer is the number of rows of diagnoses that are required (default 100)
#' @param seed integer is the random seed starting value for reproducability (default 2)
#' @param max_dx integer is the maximum number of diagnoses that a beneficiary can have (default 10)
#' @return data frame DIAG is the list of HICNO and ICD diagnoses
#' @export
generate_DIAG <- function(cmshcc_map, size = 100, seed = 2, max_dx = 10) {
  set.seed(seed)
  num_dx <- sample(x = 0:max_dx, size, replace = TRUE)
  tot_dx <- sum(num_dx)
  dxs <- sample(cmshcc_map$DX, tot_dx, replace = TRUE)
  HICNO <-rep(x = 1:size, times = num_dx)
  HICNO <- as.character(HICNO)
  DIAG <- data.frame(HICNO = HICNO, DX = dxs, stringsAsFactors = FALSE)
  return(DIAG)
}

#' load_hcc_map
#' @param string file name for the ICD -> HCC mapping file
#' @return Data Frame The data frame of the mapping file
#' @export
load_cmshcc_map <- function(file_name = "data/2017_Midyear_Final ICD-10-CM Mappings_standard.RData") {
  #cmshcc_map <- read.csv(file_name, header=TRUE, sep=",", stringsAsFactors=FALSE)
  load(file_name)
  cmshcc_map <- mappings
  cmshcc_map <- cmshcc_map[c("DX", "CMSHCC")]
  cmshcc_map <- na.omit(cmshcc_map)
  cmshcc_map$CMSHCC <- as.character(cmshcc_map$CMSHCC)
  cmshcc_map$CMSHCC <- str_pad(cmshcc_map$CMSHCC, 3, "left", "0")
  cmshcc_map$CMSHCC <- paste0("HCC", cmshcc_map$CMSHCC)
  cmshcc_dummy_row <- data.frame(DX="DUMMY", CMSHCC="DUMMY", stringsAsFactors=FALSE) # Needed to ensure that every HICNO has an HCC
  cmshcc_map <- rbind(cmshcc_map, cmshcc_dummy_row)
  return(cmshcc_map)
}

#' get_hcc_grid
#' @param 
#' @param
#' @param 
#' @return 
#' @export
get_hcc_grid <- function(PERSON, DIAG, cmshcc_map) {
  dummy_HCC_DIAG <- data.frame(HICNO="DUMMY", DX=cmshcc_map$DX, stringsAsFactors=FALSE)
  dummy_PERSON_DIAG <- data.frame(HICNO=PERSON$HICNO, DX="DUMMY", stringsAsFactors=FALSE)
  DIAG <- rbind(DIAG, dummy_HCC_DIAG, dummy_PERSON_DIAG) # ensures that all HCC columns appear in the grid
  merge_df <- merge(DIAG, cmshcc_map, on="DX")
  merge_df$DX <- NULL
  merge_df <- distinct(merge_df)
  merge_df$indicator <- 1
  hcc_grid <- dcast(merge_df, HICNO ~ CMSHCC, value.var="indicator", fill=0)
  hcc_grid <- subset(hcc_grid, HICNO!="DUMMY")
  hcc_grid$DUMMY <- NULL
  hcc_grid
}

#' person_age
#' @param DOB date is the date of birth for the person
#' @param ASOF date is the date as of the risk scores are evaluated
#' @return numeric age
#' @export
person_age <- function(DOB, ASOF=Sys.Date()) {
  as.numeric(round(difftime(ASOF, DOB, units = "weeks")/52.25))
}

#' person_age_band
#' @param breaks vector of integers representing the lower endpoints for the age bands
#' @param break_labels vector of strings representing the labels for each age band
#' @export
person_age_band <- function(ages,  genders, breaks = c(0, 34, 44, 54, 59, 64, 69, 74, 79, 84, 89, 94, 120), break_labels = c("0_34", "35_44", "45_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_84", "85_89", "90_94", "95_GT")) {
  age_bands = cut(x = ages, breaks = breaks, labels=break_labels, include.lowest = FALSE, right = TRUE)
  age_bands = as.character(age_bands)
  age_bands = paste(genders, age_bands, sep="")
  return(age_bands)
}

#' evaluate_v22_2017
#' @param PERSON 
#' @param DIAG
#' @param model_type
evaluate_v22_2017 <- function(PERSON, DIAG, model_type) {
  factors_v22_2017 <- read.csv('data/factors_v22_2017.csv', header = TRUE, sep = ",", stringsAsFactors = FALSE)
  factors_v22_2017$description <- NULL
  
  PERSON$DISABL <- (PERSON$AGE < 65) & (PERSON$OREC != 0)
  PERSON$ORIGDS <- (!PERSON$DISABL) & (PERSON$OREC == 1)

  # Demographic factors
  
  demographic_factors <- subset(factors_v22_2017, factor_type == 'demographic')
  demographic_factors$factor_type <- NULL
  demographic_factors <- demographic_factors[c("factor", model_type)]
  names(demographic_factors) <- replace(names(demographic_factors), names(demographic_factors) == model_type, "demographic_score")
  
  PERSON <- merge(PERSON, demographic_factors, by.x = "AGE_BAND", by.y = "factor")
  
  # Demographic interaction factors
  
  PERSON$ORIGDS_MCAID_F <- (PERSON$ORIGDS == 1) & (PERSON$SEX == "F") & (PERSON$MCAID == 1)
  PERSON$ORIGDS_MCAID_M <- (PERSON$ORIGDS == 1) & (PERSON$SEX == "M") & (PERSON$MCAID == 1)
  
  demographic_interaction_factors <- subset(factors_v22_2017, factor_type == "demographic_interaction")
  demographic_factors$factor_type <- NULL
  demographic_interaction_factors_names <- demographic_interaction_factors$factor
  demographic_interaction_factors <- demographic_interaction_factors[model_type]
  names(demographic_interaction_factors) <- c("demographic_interaction_score")
  
  PERSON$demographic_interaction_score <- as.matrix(PERSON[, demographic_interaction_factors_names]) %*% as.matrix(demographic_interaction_factors)
  
  # Condition factors
  PERSON <- merge(PERSON, hcc_grid, by = "HICNO")
    
  # Apply Hierarchies
  PERSON$HCC012 <- PERSON$HCC012 & (!PERSON$HCC008) & (!PERSON$HCC009) & (!PERSON$HCC010) & (!PERSON$HCC011)
  PERSON$HCC011 <- PERSON$HCC011 & (!PERSON$HCC008) & (!PERSON$HCC009) & (!PERSON$HCC010)
  PERSON$HCC010 <- PERSON$HCC010 & (!PERSON$HCC008) & (!PERSON$HCC009)
  PERSON$HCC009 <- PERSON$HCC009 & (!PERSON$HCC008)
  PERSON$HCC019 <- PERSON$HCC019 & (!PERSON$HCC017) & (!PERSON$HCC018)
  PERSON$HCC018 <- PERSON$HCC018 & (!PERSON$HCC019)
  PERSON$HCC029 <- PERSON$HCC029 & (!PERSON$HCC027) & (!PERSON$HCC028)
  PERSON$HCC028 <- PERSON$HCC028 & (!PERSON$HCC027)
  PERSON$HCC080 <- PERSON$HCC080 & (!PERSON$HCC027) & (!PERSON$HCC166)
  PERSON$HCC048 <- PERSON$HCC048 & (!PERSON$HCC046)
  PERSON$HCC055 <- PERSON$HCC055 & (!PERSON$HCC054)
  PERSON$HCC058 <- PERSON$HCC058 & (!PERSON$HCC057)
  PERSON$HCC169 <- PERSON$HCC169 & (!PERSON$HCC072) & (!PERSON$HCC071) & (!PERSON$HCC070)
  PERSON$HCC104 <- PERSON$HCC104 & (!PERSON$HCC071) & (!PERSON$HCC070) & (!PERSON$HCC103)
  PERSON$HCC103 <- PERSON$HCC103 & (!PERSON$HCC070)
  PERSON$HCC072 <- PERSON$HCC072 & (!PERSON$HCC071) & (!PERSON$HCC070)
  PERSON$HCC071 <- PERSON$HCC071 & (!PERSON$HCC070)
  PERSON$HCC084 <- PERSON$HCC084 & (!PERSON$HCC083) & (!PERSON$HCC082)
  PERSON$HCC083 <- PERSON$HCC083 & (!PERSON$HCC082)
  PERSON$HCC088 <- PERSON$HCC088 & (!PERSON$HCC087) & (!PERSON$HCC086)
  PERSON$HCC087 <- PERSON$HCC087 & (!PERSON$HCC086)
  PERSON$HCC100 <- PERSON$HCC100 & (!PERSON$HCC099)
  PERSON$HCC108 <- PERSON$HCC108 & (!PERSON$HCC107) & (!PERSON$HCC106)
  PERSON$HCC161 <- PERSON$HCC161 & (!PERSON$HCC106)
  PERSON$HCC189 <- PERSON$HCC189 & (!PERSON$HCC106)
  PERSON$HCC107 <- PERSON$HCC107 & (!PERSON$HCC106)
  PERSON$HCC112 <- PERSON$HCC112 & (!PERSON$HCC111) & (!PERSON$HCC110)
  PERSON$HCC111 <- PERSON$HCC111 & (!PERSON$HCC110)
  PERSON$HCC115 <- PERSON$HCC115 & (!PERSON$HCC114)
  PERSON$HCC137 <- PERSON$HCC137 & (!PERSON$HCC136) & (!PERSON$HCC135) & (!PERSON$HCC134)
  PERSON$HCC136 <- PERSON$HCC136 & (!PERSON$HCC135) & (!PERSON$HCC134)
  PERSON$HCC135 <- PERSON$HCC135 & (!PERSON$HCC134)
  PERSON$HCC161 <- PERSON$HCC161 & (!PERSON$HCC158) & (!PERSON$HCC157)
  PERSON$HCC158 <- PERSON$HCC158 & (!PERSON$HCC157)
  PERSON$HCC167 <- PERSON$HCC167 & (!PERSON$HCC166)
    
  # Generate Condition Scores
  condition_factors <- subset(factors_v22_2017, factor_type == "condition_category")
  condition_factors$factor_type <- NULL
  condition_factors_names <- condition_factors$factor
  condition_factors <- condition_factors[model_type]
  names(condition_factors) <- c("condition_score")
  
  PERSON$condition_score <- as.matrix(PERSON[, condition_factors_names]) %*% as.matrix(condition_factors)
  
  # Condition Groupings
  PERSON$gCancer <- PERSON$HCC008 | PERSON$HCC009 | PERSON$HCC010 | PERSON$HCC011 | PERSON$HCC012
  PERSON$gDiabetesMellit <- PERSON$HCC017 | PERSON$HCC018 | PERSON$HCC019
  PERSON$gCopdCF <- PERSON$HCC110 | PERSON$HCC111
  PERSON$gRenal <- PERSON$HCC134 | PERSON$HCC135 | PERSON$HCC136 | PERSON$HCC137
  PERSON$gRespDepArre <- PERSON$HCC082 | PERSON$HCC083 | PERSON$HCC084
  PERSON$gSubstanceAbuse <- PERSON$HCC054 | PERSON$HCC055
  PERSON$gPsychiatric <- PERSON$HCC057 | PERSON$HCC058
  PERSON$PRESSURE_ULCER <- PERSON$HCC157 | PERSON$HCC158
  PERSON$SEPSIS <- PERSON$HCC002
  PERSON$ARTIF_OPENINGS <- PERSON$HCC188
  PERSON$ASP_SPEC_BACT_PNEUM <- PERSON$HCC114
  PERSON$SCHIZOPHRENIA <- PERSON$HCC057
  PERSON$SEIZURES <- PERSON$HCC079
  PERSON$CHF <- PERSON$HCC085
  
  # Condition Interactions
  
  PERSON$HCC47_gCancer <- PERSON$gCancer & PERSON$HCC047
  PERSON$HCC85_gDiabetesMellit <- PERSON$HCC085 & PERSON$gDiabetesMellit
  PERSON$HCC85_gCopdCF <- PERSON$HCC085 & PERSON$gCopdCF
  PERSON$HCC85_gRenal <- PERSON$HCC085 & PERSON$gRenal
  PERSON$gRespDepandArre_gCopdCF <- PERSON$gRespDepArre & PERSON$gCopdCF
  PERSON$HCC85_HCC96 <- PERSON$HCC085 & PERSON$HCC096
  PERSON$gSubstanceAbuse_gPsychiatric <- PERSON$gSubstanceAbuse & PERSON$gPsychiatric
  PERSON$SEPSIS_PRESSURE_ULCER <- PERSON$SEPSIS & PERSON$PRESSURE_ULCER
  PERSON$SEPSIS_ARTIF_OPENINGS <- PERSON$SEPSIS & PERSON$ARTIF_OPENINGS
  PERSON$ART_OPENINGS_PRESSURE_ULCER <- PERSON$ARTIF_OPENINGS & PERSON$PRESSURE_ULCER
  PERSON$gCopdCF_ASP_SPEC_BACT_PNEUM <- PERSON$gCopdCF & PERSON$ASP_SPEC_BACT_PNEUM
  PERSON$ASP_SPEC_BACT_PNEUM_PRES_ULC <- PERSON$ASP_SPEC_BACT_PNEUM & PERSON$PRESSURE_ULCER
  PERSON$SEPSIS_ASP_SPEC_BACT_PNEUM <- PERSON$SEPSIS & PERSON$ASP_SPEC_BACT_PNEUM
  PERSON$SCHIZOPHRENIA_gCopdCF <- PERSON$SCHIZOPHRENIA & PERSON$gCopdCF
  PERSON$SCHIZOPHRENIA_CHF <- PERSON$SCHIZOPHRENIA & PERSON$HCC085
  PERSON$SCHIZOPHRENIA_SEIZURES <- PERSON$SCHIZOPHRENIA & PERSON$SEIZURES
  
  # Generate Condition Interaction Scores
  condition_interaction_factors <- subset(factors_v22_2017, factor_type == "condition_interaction")
  condition_interaction_factors$factor_type <- NULL
  condition_interaction_factors_names <- condition_interaction_factors$factor
  condition_interaction_factors <- condition_interaction_factors[model_type]
  names(condition_interaction_factors) <- c("condition_interaction_score")
  
  PERSON$condition_interaction_score <- as.matrix(PERSON[, condition_interaction_factors_names]) %*% as.matrix(condition_interaction_factors)
  
  # Generate Condition Demographic Interaction Scores
  
  PERSON$DISABLED_HCC85 <- PERSON$DISABL & PERSON$HCC085
  PERSON$DISABLED_PRESSURE_ULCER <- PERSON$DISABL & PERSON$PRESSURE_ULCER
  PERSON$DISABLED_HCC161 <- PERSON$DISABL & PERSON$HCC161
  PERSON$DISABLED_HCC39 <- PERSON$DISABL & PERSON$HCC039
  PERSON$DISABLED_HCC77 <- PERSON$DISABL & PERSON$HCC077
  PERSON$DISABLED_HCC6 <- PERSON$DISABL & PERSON$HCC006
  
  # Generate Demographic Condition Interaction Scores
  demographic_condition_interaction_factors <- subset(factors_v22_2017, factor_type == "demographic_condition_interaction")
  demographic_condition_interaction_factors$factor_type <- NULL
  demographic_condition_interaction_factors_names <- demographic_condition_interaction_factors$factor
  demographic_condition_interaction_factors <- demographic_condition_interaction_factors[model_type]
  names(demographic_condition_interaction_factors) <- c("demographic_condition_interaction_score")
  
  PERSON$demographic_condition_interaction_score <- as.matrix(PERSON[, demographic_condition_interaction_factors_names]) %*% as.matrix(demographic_condition_interaction_factors)
  
  # Add final score
  PERSON[[model_type]] <- PERSON$demographic_score + PERSON$demographic_interaction_score + PERSON$condition_score + PERSON$condition_interaction_score + PERSON$demographic_condition_interaction_score
  return(PERSON[[model_type]])
}

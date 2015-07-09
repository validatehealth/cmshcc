# --------------------------------------------------------------
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
icd9RiskAdjCMSHCC14 <- function(DIAG, PERSON, cmshcc_list, factor_list = factors) {
  PERSON$AGE <- as.numeric(round(difftime(Sys.Date(), as.Date(PERSON$DOB, "%Y-%m-%d", tz = "UTC"), units = "weeks")/52.25))
  PERSON$DISABL <- (PERSON$AGE < 65) & (PERSON$OREC != 0)
  PERSON$ORIGDS <- (PERSON$AGE >= 65) & (PERSON$OREC %in% c(1,3))
  breaks <- c(0, 35, 45, 55, 60, 65, 70, 75, 80, 85, 90, 95, 120)
  PERSON$AGE_BAND <- cut(x = PERSON$AGE, breaks = breaks, include.lowest = TRUE, right = FALSE)
  
  female_age_factors <- factor_list$female_age_factors
  male_age_factors <- factor_list$male_age_factors
  
  PERSON$AGEGENDER_SCORE <- (PERSON$SEX == 1) * male_age_factors[PERSON$AGE_BAND] + (PERSON$SEX == 2) * female_age_factors[PERSON$AGE_BAND]
  PERSON$MCAID_FEMALE_AGED <- (PERSON$MCAID == 1) & (PERSON$SEX == 2) & (PERSON$DISABL == 0)
  PERSON$MCAID_FEMALE_DISABL <- (PERSON$MCAID == 1) & (PERSON$SEX == 2) & (PERSON$DISABL == 1)
  PERSON$MCAID_MALE_AGED <- (PERSON$MCAID == 1) & (PERSON$SEX == 1) & (PERSON$DISABL == 0)
  PERSON$MCAID_MALE_DISABL <- (PERSON$MCAID == 1) & (PERSON$SEX == 2) & (PERSON$DISABL == 1)
  PERSON$ORIGDS_FEMALE <- (PERSON$ORIGDS == 1) & (PERSON$SEX == 2)
  PERSON$ORIGDS_MALE <- (PERSON$ORIGDS == 1) & (PERSON$SEX == 1)
  
  demointeraction_factors <- factor_list$demointeraction_factors
  PERSON$DEMOINTERACTION_SCORE <- as.matrix(PERSON[,c("MCAID_FEMALE_AGED", "MCAID_FEMALE_DISABL", "MCAID_MALE_AGED", "MCAID_MALE_DISABL", "ORIGDS_FEMALE", "ORIGDS_MALE")]) %*% demointeraction_factors
  
  #Evaluate using icd9 package by Jack Wasey
  PERSON <- cbind(PERSON, as.data.frame(icd9Comorbid(icd9df = DIAG, icd9Mapping = cmshcc_list, visitId = "HICNO")))
  
  #Apply Hierarchies
  PERSON$HCC12 <- PERSON$HCC12 & (!PERSON$HCC8) & (!PERSON$HCC9) & (!PERSON$HCC10) & (!PERSON$HCC11)
  PERSON$HCC11 <- PERSON$HCC11 & (!PERSON$HCC8) & (!PERSON$HCC9) & (!PERSON$HCC10)
  PERSON$HCC10 <- PERSON$HCC10 & (!PERSON$HCC8) & (!PERSON$HCC9)
  PERSON$HCC9 <- PERSON$HCC9 & (!PERSON$HCC8)
  PERSON$HCC19 <- PERSON$HCC19 & (!PERSON$HCC17) & (!PERSON$HCC18)
  PERSON$HCC18 <- PERSON$HCC18 & (!PERSON$HCC19)
  PERSON$HCC29 <- PERSON$HCC29 & (!PERSON$HCC27) & (!PERSON$HCC28)
  PERSON$HCC28 <- PERSON$HCC28 & (!PERSON$HCC27)
  PERSON$HCC80 <- PERSON$HCC80 & (!PERSON$HCC27) & (!PERSON$HCC166)
  PERSON$HCC48 <- PERSON$HCC48 & (!PERSON$HCC46)
  PERSON$HCC55 <- PERSON$HCC55 & (!PERSON$HCC54)
  PERSON$HCC58 <- PERSON$HCC58 & (!PERSON$HCC57)
  PERSON$HCC169 <- PERSON$HCC169 & (!PERSON$HCC72) & (!PERSON$HCC71) & (!PERSON$HCC70)
  PERSON$HCC104 <- PERSON$HCC104 & (!PERSON$HCC71) & (!PERSON$HCC70) & (!PERSON$HCC103)
  PERSON$HCC103 <- PERSON$HCC103 & (!PERSON$HCC70)
  PERSON$HCC72 <- PERSON$HCC72 & (!PERSON$HCC71) & (!PERSON$HCC70)
  PERSON$HCC71 <- PERSON$HCC71 & (!PERSON$HCC70)
  PERSON$HCC84 <- PERSON$HCC84 & (!PERSON$HCC83) & (!PERSON$HCC82)
  PERSON$HCC83 <- PERSON$HCC83 & (!PERSON$HCC82)
  PERSON$HCC88 <- PERSON$HCC88 & (!PERSON$HCC87) & (!PERSON$HCC86)
  PERSON$HCC87 <- PERSON$HCC87 & (!PERSON$HCC86)
  PERSON$HCC100 <- PERSON$HCC100 & (!PERSON$HCC99)
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
  
  #Generate Disease Scores
  disease_factors <- factor_list$disease_factors
  PERSON$DISEASE_SCORE <- as.matrix(PERSON[, names(cmshcc_list)]) %*% disease_factors
  
  #Condition Category Groupings
  PERSON$CANCER <- PERSON$HCC8 | PERSON$HCC9 | PERSON$HCC10 | PERSON$HCC11 | PERSON$HCC12
  PERSON$IMMUNE <- PERSON$HCC47
  PERSON$CHF <- PERSON$HCC85
  PERSON$COPD <- PERSON$HCC110 | PERSON$HCC111
  PERSON$RENAL <- PERSON$HCC134 | PERSON$HCC135 | PERSON$HCC136 | PERSON$HCC137
  PERSON$CARD_RESP_FAIL <- PERSON$HCC82 | PERSON$HCC83 | PERSON$HCC84
  PERSON$DIABETES <- PERSON$HCC17 | PERSON$HCC18 | PERSON$HCC19
  PERSON$SEPSIS <- PERSON$HCC2
 
   #Disease x Disease Interaction Terms
  PERSON$CANCER_IMMUNE <- PERSON$CANCER & PERSON$IMMUNE
  PERSON$CHF_COPD <- PERSON$CHF & PERSON$COPD
  PERSON$CHF_RENAL <- PERSON$CHF & PERSON$RENAL
  PERSON$COPD_CARD_RESP_FAIL <- PERSON$COPD & PERSON$CARD_RESP_FAIL
  PERSON$DIABETES_CHF <- PERSON$DIABETES & PERSON$CHF
  PERSON$SEPSIS_CARD_RESP_FAIL <- PERSON$SEPSIS & PERSON$CARD_RESP_FAIL
 
   interaction_terms <- c("CANCER_IMMUNE", "CHF_COPD", "CHF_RENAL", "COPD_CARD_RESP_FAIL", "DIABETES_CHF", "SEPSIS_CARD_RESP_FAIL")
  interaction_factors <- factor_list$interaction_factors
  PERSON$DISEASE_INTERACTION <- as.matrix(PERSON[, interaction_terms]) %*% interaction_factors
  
  #Disability x Disease Interaction Terms
  PERSON$DISABL_HCC6 <- PERSON$DISABL & PERSON$HCC6
  PERSON$DISABL_HCC34 <- PERSON$DISABL & PERSON$HCC34
  PERSON$DISABL_HCC46 <- PERSON$DISABL & PERSON$HCC46
  PERSON$DISABL_HCC54 <- PERSON$DISABL & PERSON$HCC54
  PERSON$DISABL_HCC110 <- PERSON$DISABL & PERSON$HCC110
  PERSON$DISABL_HCC176 <- PERSON$DISABL & PERSON$HCC176
  
  disabl_int_terms <- c("DISABL_HCC6", "DISABL_HCC34", "DISABL_HCC46", "DISABL_HCC54", "DISABL_HCC110", "DISABL_HCC176")
  disabl_int_factors <- factor_list$disabl_int_factors
  PERSON$DISABL_INTERACTION <- as.matrix(PERSON[, disabl_int_terms]) %*% disabl_int_factors
  
  #Total Risk Adjustment Scores
  PERSON$TOTAL <- PERSON$AGEGENDER_SCORE + PERSON$DEMOINTERACTION_SCORE + PERSON$DISEASE_SCORE + PERSON$DISEASE_INTERACTION + PERSON$DISABL_INTERACTION
  return(PERSON$TOTAL)
}
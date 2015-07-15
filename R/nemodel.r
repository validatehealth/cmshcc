icd9RiskAdjCMSHCCne <- function(PERSON, date = Sys.Date(), factor_list = factors) {
  PERSON$AGE <- as.numeric(round(difftime(Sys.Date(), as.Date(PERSON$DOB, "%Y-%m-%d", tz = "UTC"), units = "weeks")/52.25))
  PERSON$ORIGDS <- (PERSON$AGE >= 65) & (PERSON$OREC %in% c(1,3))
  breaks <- c(0, 35, 45, 55, 60, 65, 66, 67, 68, 69, 70, 75, 80, 85, 90, 95, 120)
  PERSON$AGE_BAND <- cut(x = PERSON$AGE, breaks = breaks, include.lowest = TRUE, right = FALSE, labels = FALSE)
  
  PERSON$FEMALE <- as.numeric((PERSON$MCAID == 0) & (PERSON$SEX == 2) & (PERSON$ORIGDS == 0))
  PERSON$FEMALE_MCAID <- as.numeric((PERSON$MCAID == 1) & (PERSON$SEX == 2) & (PERSON$ORIGDS == 0))
  PERSON$FEMALE_ORIGDS <- as.numeric((PERSON$MCAID == 0) & (PERSON$SEX == 2) & (PERSON$ORIGDS == 1))
  PERSON$FEMALE_MCAID_ORIGDS <- as.numeric((PERSON$MCAID == 1) & (PERSON$SEX == 2) & (PERSON$ORIGDS == 1))
  PERSON$MALE <- as.numeric((PERSON$MCAID == 0) & (PERSON$SEX == 1) & (PERSON$ORIGDS == 0))
  PERSON$MALE_MCAID <- as.numeric((PERSON$MCAID == 1) & (PERSON$SEX == 1) & (PERSON$ORIGDS == 0))
  PERSON$MALE_ORIGDS <- as.numeric((PERSON$MCAID == 0) & (PERSON$SEX == 1) & (PERSON$ORIGDS == 1))
  PERSON$MALE_MCAID_ORIGDS <- as.numeric((PERSON$MCAID == 1) & (PERSON$SEX == 1) & (PERSON$ORIGDS == 1))

  PERSON$TOTAL <- PERSON$FEMALE * factor_list$female_factors[PERSON$AGE_BAND] + PERSON$FEMALE_MCAID * factor_list$female_medicaid_factors[PERSON$AGE_BAND] + PERSON$FEMALE_ORIGDS * factor_list$female_origdbld_factors[PERSON$AGE_BAND] + PERSON$FEMALE_MCAID_ORIGDS * factor_list$female_medicaid_origdbld_factors[PERSON$AGE_BAND] + PERSON$MALE * factor_list$male_factors[PERSON$AGE_BAND] + PERSON$MALE_MCAID * factor_list$male_medicaid_factors[PERSON$AGE_BAND] + PERSON$MALE_ORIGDS * factor_list$male_origdbld_factors[PERSON$AGE_BAND] + PERSON$MALE_MCAID_ORIGDS * factor_list$male_medicaid_origdbld_fact[PERSON$AGE_BAND]
  return(PERSON$TOTAL)
  
}
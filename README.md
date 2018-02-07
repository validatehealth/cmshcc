Based on presentation from:
http://ase.uva.nl/binaries/content/assets/subsites/amsterdam-school-of-economics/r-in-insurance/webster-risk-adjustment-in-r.pdf

1) install.packages("devtools") if not already installed
2) install cmshcc package using:  devtools::install_github("healthactuary/cmshcc")
3) load the cmshcc package using library(cmshcc)
4) generate a sample PERSON data frame or use your own with same format: PERSON <- generate_PERSON()
5) load the CMS-HCC diagnosis code to HCC mapping: cmshcc_map <- load_cmshcc_map()
6) create a diagnosis data frame using mapping or use your own data with same format: DIAG <- generate_DIAG(cmshcc_map)
7) Evaluate the 2017 V22 CMS-HCC models:
  Community_NonDual_Aged = evaluate_v22_2017(PERSON, DIAG, "Community_NonDual_Aged")
  Community_NonDual_Disabled = evaluate_v22_2017(PERSON, DIAG, "Community_NonDual_Disabled")
  Community_FBDual_Aged = evaluate_v22_2017(PERSON, DIAG, "Community_FBDual_Aged")
  Community_FBDual_Disabled = evaluate_v22_2017(PERSON, DIAG, "Community_FBDual_Disabled")
  Community_PBDual_Aged = evaluate_v22_2017(PERSON, DIAG, "Community_PBDual_Aged")
  Community_PBDual_Disabled = evaluate_v22_2017(PERSON, DIAG, "Community_PBDual_Disabled")
  Institutional = evaluate_v22_2017(PERSON, DIAG, "Institutional")

  Bind together results in one dataframe if preferable:
  
  results <- cbind(Community_NonDual_Aged, Community_NonDual_Disabled, Community_FBDual_Aged, Community_FBDual_Disabled, Community_PBDual_Aged, Community_PBDual_Disabled, Institutional)
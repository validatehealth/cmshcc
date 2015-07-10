#Generates a random date
randomDate <- function(size = 100, start_time = "1930/01/01", end_time = "2010/12/31") {
  start_time <- as.POSIXct(as.Date(start_time))
  end_time <- as.POSIXct(as.Date(end_time))
  date_time <- as.numeric(difftime(end_time, start_time, unit = "sec"))
  end_value <- sort(runif(size, 0, date_time))
  random_time <- start_time + end_value
  return(random_time)
}

#Generates a sample dataframe of people of specified size
generateTestPERSON <- function(size = 100, seed = 2, start_time = "1930/01/01", end_time = "2010/12/31") {
  set.seed(seed)
  HICNO <- 1:size
  SEX <- sample(x = c(1, 2), size, replace = TRUE)
  DOB <- randomDate(size, start_time, end_time)
  MCAID <- sample(x = c(0, 1), size, replace = TRUE)
  NMCAID <- sample(x = c(0, 1), size, replace = TRUE)
  OREC <- sample(x = 0:3, size, replace = TRUE)
  PERSON <- data.frame(HICNO = HICNO, SEX = SEX, DOB = DOB, MCAID = MCAID, NMCAID = NMCAID, OREC = OREC, stringsAsFactors = FALSE)
  return(PERSON)
}

#Generates a sample dataframe of diagnoses of specified size
generateTestDIAG <- function(size = 100, seed = 2, max_dx = 10, cmshcc_map) {
  set.seed(seed)
  num_dx <- sample(x = 1:max_dx, size, replace = TRUE)
  tot_dx <- sum(num_dx)
  dxs <- sample(cmshcc_map$icd9, tot_dx, replace = TRUE)
  HICNO <-rep(x = 1:size, times = num_dx)
  DIAG <- data.frame(HICNO = HICNO, DIAGS = dxs, stringsAsFactors = FALSE)
  return(DIAG)
}

#Load icd9HCC mapping
loadicd9HCC <- function() {
	cmshcc_map <- read.csv(file.choose(), header=FALSE, sep="", stringsAsFactors=FALSE)
	names(cmshcc_map) <- c("icd9", "hcc")
	#Generate list of HCC mapping
	hccs <- sort(unique(cmshcc_map$hcc))
	cmshcc_list <- list()
	for(i in 1:length(hccs)) {
		label <- paste0("HCC", hccs[i])
		cmshcc_list[[label]] <- subset(cmshcc_map, hcc == hccs[i])$icd9
	}
	cmshcc_list
}

#New Enrollee Model
age_gender_only <- function(PERSON, date = Sys.Date(), factor_list = factors){
  PERSON$AGE <- as.numeric(round(difftime(date, as.Date(PERSON$DOB, "%Y-%m-%d", tz = "UTC"), units = "weeks")/52.25))
  PERSON$DISABL <- (PERSON$AGE < 65) & (PERSON$OREC != 0)
  PERSON$ORIGDS <- (PERSON$AGE >= 65) & (PERSON$OREC %in% c(1,3))
  breaks <- c(0, 35, 45, 55, 60, 65, 70, 75, 80, 85, 90, 95, 120)
  PERSON$AGE_BAND <- cut(x = PERSON$AGE, breaks = breaks, include.lowest = TRUE, right = FALSE)
  
  female_age_factors <- factor_list$female_age_factors
  male_age_factors <- factor_list$male_age_factors
  PERSON$AGEGENDER_SCORE <- (PERSON$SEX == 1) * male_age_factors[PERSON$AGE_BAND] + (PERSON$SEX == 2) * female_age_factors[PERSON$AGE_BAND]
  return(PERSON$AGEGENDER_SCORE)
}
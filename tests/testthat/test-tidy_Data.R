test_that("Check tidy_Data functionality", {
  
  last100days = Sys.Date() - 0:99
  pInfuse1 <- runif(100)
  countryuse <- sample(c("CountryA","CountryB","CountryC"), 100, replace = TRUE)
  testData <- data.frame(DateReport = last100days, geoid = runif(100), RegionName = as.character(runif(100)), Country = countryuse, pInf = pInfuse1, geometry = runif(100))
  
  expect_equal(testData, tidy_Data(testData, tidy = FALSE), ignore_attr = TRUE)
  expect_vector(tidy_Data(testData, dropNACountry = FALSE), size = c(100))
  expect_vector(tidy_Data(testData, dropNACountry = FALSE, DaysOld = 30, dropNAall = TRUE), size = c(31))
  expect_vector(tidy_Data(testData, dropNACountry = FALSE, DaysOld = 40, dropNAall = TRUE), size = c(41))
  
  testData2 <- cbind(testData,c("1998-01-01"),2,"hi","CountryD",NA,2)
  expect_equal(testData2, tidy_Data(testData2, tidy = FALSE), ignore_attr = TRUE)
  expect_vector(tidy_Data(testData2, dropNACountry = FALSE), size = c(100))
  expect_vector(tidy_Data(testData2, dropNACountry = FALSE, DaysOld = 30, dropNAall = TRUE), size = c(31))
  expect_vector(tidy_Data(testData2, dropNACountry = FALSE, DaysOld = 40, dropNAall = TRUE), size = c(41))
  
  pc = which(testData$pInf> 0.5)
  expect_equal(sum(is.na(tidy_Data(testData, DaysOld = 30, minimumpercapitaactivecases = 0.5, dropNACountry = FALSE)[["pInf"]])), 100-length(which(pc<=31)), ignore_attr = TRUE)
  RE = c()
  RE$minimumRisk = 100
  RE$ascertainmentbias = 2
  RE$maximumN = 10
  #	minimumpercapitaactivecases <- (1 / RiskEval$ascertainmentbias) * (1 - (1 - RiskEval$minimumRisk / 100)^(1 / RiskEval$maximumN))
  #	minimumpercapitaactivecases <- (1 / 2) * (1 - (1 - 100 / 100)^(1 / 10))  == 0.5 
  expect_equal(tidy_Data(testData, RiskEval=RE,DaysOld = 30, dropNACountry = FALSE), tidy_Data(testData, DaysOld = 30, minimumpercapitaactivecases = 0.5, dropNACountry = FALSE), ignore_attr = TRUE)
  RE$minimumRisk = 50
  #RE takes precedent.
  minpc = (1 / 2) * (1 - (1 - 50 / 100)^(1 / 10))
  expect_equal(tidy_Data(testData, DaysOld = 30, RiskEval=RE, minimumpercapitaactivecases = 0.5, dropNACountry = FALSE), tidy_Data(testData, DaysOld = 30, minimumpercapitaactivecases = minpc, dropNACountry = FALSE), ignore_attr = TRUE)
  
  ca <- which(testData$CountryName == "CountryA")
  testData$pInf[ca] = NA
  expect_vector(tidy_Data(testData, dropNACountry = TRUE), size = c(100 - length(ca)))
})

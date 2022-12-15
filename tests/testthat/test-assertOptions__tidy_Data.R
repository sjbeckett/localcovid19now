test_that("check assertion options", {
  
  expect_error(assertOptions__tidy_Data(tidy = 1, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = NULL, dropNACountry = TRUE , dropNAall = TRUE))
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = "hi", minimumpercapitaactivecases = 0.5, RiskEval = NULL, dropNACountry = TRUE , dropNAall = TRUE))
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = "hi", RiskEval = NULL, dropNACountry = TRUE , dropNAall = TRUE))
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = NULL, dropNACountry = 3 , dropNAall = TRUE))
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = NULL, dropNACountry = TRUE , dropNAall = 5))
  
  RE = c()
  RE$minimumRisk = 50
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = TRUE))
  
  RE$ascertainmentbias = 2
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = TRUE))
  
  RE$maximumN = 10
  
  expect_error(assertOptions__tidy_Data(tidy = 1, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = TRUE))
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = "hi", minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = TRUE))
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = "hi", RiskEval = RE, dropNACountry = TRUE , dropNAall = TRUE))
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = 3 , dropNAall = TRUE))
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = 5))
  
  RE$minimumRisk = 125
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = FALSE))
  
  RE$minimumRisk = -75
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = FALSE))
  
  RE$minimumRisk = "hi"
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = FALSE))
  
  RE$minimumRisk = 30
  RE$ascertainmentbias = -2
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = FALSE))
  
  RE$ascertainmentbias = "hi"
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = FALSE))
  
  RE$ascertainmentbias = 2
  RE$maximumN = -5
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = FALSE))
  
  RE$maximumN = "hi"
  expect_error(assertOptions__tidy_Data(tidy = TRUE, DaysOld = 25, minimumpercapitaactivecases = 0.5, RiskEval = RE, dropNACountry = TRUE , dropNAall = FALSE))
  
})

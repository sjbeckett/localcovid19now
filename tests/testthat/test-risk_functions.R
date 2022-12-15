test_that("Check binomial risk probability works", {
  
  p <- 365/6671
  cR <- calcRisk(p,50)
  eR <- estRisk(p,1,50)
  expect_equal(cR, 94)
  expect_equal(eR, 94)
  
  expect_equal(length(cR), 1)
  expect_equal(length(eR), 1)
  
  expect_lt(calcRisk(p,50),calcRisk(p,150))
  expect_lt(calcRisk(p*0.9,50),calcRisk(p,50))
  
  expect_lt(estRisk(p,1,50),estRisk(p,1,150))
  expect_lt(estRisk(p*0.9,1,50),estRisk(p,1,50))
  expect_lt(estRisk(p,1,50),estRisk(p,2,50))
  
  expect_error(calcRisk(p,-4))
  expect_error(calcRisk(p, NA))
  
  expect_error(calcRisk(p,-4))
  expect_error(calcRisk(p))
  
  expect_error(estRisk(p))
  expect_error(estRisk(p,-1,50))
  expect_error(estRisk(p,-1,-50))
  expect_error(estRisk(p,1,-50))
  
})

## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SGDinference)
set.seed(100723)

## -----------------------------------------------------------------------------
    y = Census2000$ln_hrwage 
  edu = Census2000$edyrs
  exp = Census2000$exp
 exp2 = exp^2/100

## -----------------------------------------------------------------------------
mincer = lm(y ~ edu + exp + exp2)
inference = lmtest::coefci(mincer, df = Inf,
                             vcov = sandwich::vcovHC)
results = cbind(mincer$coefficients,inference)
colnames(results)[1] = "estimate"
print(results)

## -----------------------------------------------------------------------------
 mincer_sgd = sgdi_lm(y ~ edu + exp + exp2)
 print(mincer_sgd)

## -----------------------------------------------------------------------------
 mincer_sgd = sgd_lm(y ~ edu + exp + exp2)
 print(mincer_sgd)

## -----------------------------------------------------------------------------
library(microbenchmark)
res <- microbenchmark(sgd_lm(y ~ edu + exp + exp2),
                      sgdi_lm(y ~ edu + exp + exp2),
                      times=100L)
print(res)

## -----------------------------------------------------------------------------
mincer_sgd_path = sgdi_lm(y ~ edu + exp + exp2, path = TRUE, path_index = 2)

## -----------------------------------------------------------------------------
plot(mincer_sgd_path$path_coefficients, ylab="Return to Education", xlab="Steps", type="l")

## -----------------------------------------------------------------------------
plot(mincer_sgd_path$path_coefficients[1:2000], ylab="Return to Education", xlab="Steps", type="l")
print(c("2000th step", mincer_sgd_path$path_coefficients[2000]))
print(c("Final Estimate", mincer_sgd_path$coefficients[2]))

## -----------------------------------------------------------------------------
 mincer_sgd = sgdi_qr(y ~ edu + exp + exp2)
 print(mincer_sgd)

## -----------------------------------------------------------------------------
 mincer_sgd = sgdi_qr(y ~ edu + exp + exp2)
 print(mincer_sgd)
 mincer_sgd_median = sgdi_qr(y ~ edu + exp + exp2, qt=0.5)
 print(mincer_sgd_median)

## -----------------------------------------------------------------------------
 mincer_sgd_p10 = sgdi_qr(y ~ edu + exp + exp2, qt=0.1)
 print(mincer_sgd_p10)
 mincer_sgd_p90 = sgdi_qr(y ~ edu + exp + exp2, qt=0.9)
 print(mincer_sgd_p90)

## -----------------------------------------------------------------------------
mincer_sgd_path = sgdi_qr(y ~ edu + exp + exp2, path = TRUE, path_index = 2)
plot(mincer_sgd_path$path_coefficients[1:2000], ylab="Return to Education", xlab="Steps", type="l")
print(c("2000th step", mincer_sgd_path$path_coefficients[2000]))
print(c("Final Estimate", mincer_sgd_path$coefficients[2]))


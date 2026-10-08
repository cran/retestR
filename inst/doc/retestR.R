## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(retestR)
sim <- rt_simulate(n_persons = 800, form_exposed = 30, form_new = 15, seed = 1)
head(sim$data$persons)
table(sim$truth$preknowledge)

## -----------------------------------------------------------------------------
fit <- rt_fit(sim, growth = ~ log(days_between) + remediation)
fit

## -----------------------------------------------------------------------------
risk <- rt_risk(fit, n_null = 4, alpha = 0.01, seed = 1)
risk

## -----------------------------------------------------------------------------
table(flagged = risk$flag, preknowledge = sim$truth$preknowledge)

## -----------------------------------------------------------------------------
raw <- rt_raw_gain(sim)
k <- sum(risk$flag)
raw_flag <- raw$gain >= sort(raw$gain, decreasing = TRUE)[k]
honest_remediated <- !sim$truth$preknowledge & sim$truth$remediation == 1
c(model_caught = sum(risk$flag & sim$truth$preknowledge),
  raw_caught = sum(raw_flag & sim$truth$preknowledge),
  model_flags_remediated = sum(risk$flag & honest_remediated),
  raw_flags_remediated = sum(raw_flag & honest_remediated))


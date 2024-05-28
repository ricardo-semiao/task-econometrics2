# Packages and Functions --------------------------------------------------

library(glue)
library(vars)
library(tidyverse)
box::use(../util_functions[...]) # Or, run:
#source("https://github.com/ricardo-semiao/task-econometrics2/blob/main/util_functions.R?raw=TRUE")

stargazer_ps3 <- function(mods, filename, preds, mses, ...) {
  output_stargazer(mods, filename,
    add.lines = list(
      c("Predictions", round(preds, 2)),
      c("MSE", round(mses, 2))
    ),
    omit.stat = "f",
    no.space = TRUE,
    ...
  )
}



# Data and Exploratory Analisis -------------------------------------------

data <- read_csv("ps3/data/data_brazil.csv") %>%
  set_names("Date", "Gdp", "Exchange", "Ipc") %>%
  slice_head(n = -1)

data <- mutate(data, ExchangeDetrended = replace(
  Exchange,
  !is.na(Exchange),
  na.omit(Exchange) %>% `-`(., mFilter::hpfilter(., freq = 100)$trend)
))

data_train <- filter(data, 1942 <= Date & Date <= 2019)
data_test <- filter(data, Date == 2020)

varutils::ggvar_values(data[-1],
  args_facet = list(scale = "free_y")
)
output_ggplot("ps3/figures/explore_values.png", 5, 4)

varutils::ggvar_acf(data[-c(1,5)],
  args_facet = list(scale = "free_y"),
  na.action = na.pass
)
output_ggplot("ps3/figures/explore_acf.png", 5, 3)

varutils::ggvar_ccf_grid(data[-c(1,2)],
  na.action = na.pass
)
output_ggplot("ps3/figures/explore_ccf.png", 5, 5)

imap(data[c("Exchange", "Gdp", "Ipc")], function(x, name) {
  output_dftest(ts(na.omit(x)), "ps3/tables/dftest_{str_to_lower(name)}.tex",
    nlag = 3
  )
})



# Question 1 --------------------------------------------------------------

formulas <- list(
  Gdp ~ l(Gdp, 1:2) + l(Exchange, 1),
  Gdp ~ l(Gdp, 1:2) + l(Ipc, 1:2),
  Gdp ~ l(Gdp, 1:2) + l(Exchange, 1:2) + l(Ipc, 1:2),
  Gdp ~ l(Gdp, 1:2)
)

models_q1 <- map(formulas, \(.x) {
  #a <- reformulate_tslm(.x)
  dynlm(.x, data_train)
})

predictions_q1 <- map_dbl(models_q1, \(.x) {
  predict_tslm(.x, data_train)
})
mses_q1 <- (data_test$Gdp - predictions_q1)^2

stargazer_ps3(models_q1, "ps3/tables/ardl.tex", predictions_q1, mses_q1,
  dep.var.labels = "GDP Growth",
  label = "tb:ardl"
)



# Question 2 --------------------------------------------------------------

data_var <- data_train %>%
  select(-Date, -Exchange) %>%
  na.omit()

ps <- 1:3 %>% set_names(glue("var_{.}"))

models_q2 <- map(ps, ~VAR(data_var, p = .x))

predictions_q2 <- map(models_q2, function(mod) {
  map_dbl(predict(mod, n.ahead = 1)$fcst, ~.x[, "fcst"])
})
mses_q2 <- map(predictions_q2, ~(as.numeric(data_test[names(.x)]) - .x)^2)

pwalk(list(models_q2, predictions_q2, mses_q2, ps), function(mod, preds, mses, p) {
  stargazer_ps3(mod$varresult, glue("ps3/tables/var_{p}.tex"), preds, mses,
    column.labels = colnames(data_var),
    dep.var.labels.include = FALSE,
    model.numbers = FALSE
  )
})

models_q2$var_2 <- VAR(data_var, p = 2) #{vars} gets confused with models created outside global env

# Install from: devtools::install_github("ricardo-semiao/varutils")
varutils::ggvar_irf(models_q2[[2]], # the default is orthogonalization
  n.ahead = 10,
  runs = 1000,
  facet = "ggh4x",
  args_facet = list(scales = "free_y", independent = "y")
)
output_ggplot("ps3/figures/irfs.png", 7, 6)

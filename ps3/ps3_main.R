# Packages and Functions --------------------------------------------------

library(glue)
library(stargazer)
library(vars)
library(tidyverse)

lags_convert <- function(x) {
  pattern <- "^l\\((.+), ([0-9c\\(\\), :]+)\\)$"
  if (grepl(pattern, x)) {
    paste0(
      "lag(",
      gsub(pattern, '\\1', x),
      ", ",
      eval(parse(text = gsub(pattern, '\\2', x))),
      ")",
      collapse = " + "
    )
  } else {
    x
  }
}

formulate_dylm <- function(formula) {
  formula_rhs <- as.character(formula)[3] %>%
    str_split_1("( ?\\+ ?)|( ?- ?)") %>%
    map_chr(lags_convert) %>%
    paste0(collapse = " + ")
  
  c(as.character(formula)[2:1], formula_rhs) %>%
    paste0(collapse = " ") %>%
    as.formula()
}

predict_dylm <- function(model, new_data, n = 1) {
  variables <- colnames(model$model)[-1] %>% set_names()
  predict_data <- with(new_data,
    map_dfc(variables, ~eval(parse(text = .x)))
  ) %>%
    slice_tail(n = n)
  
  model$coefficients[1] + model$coefficients[-1] %*% t(predict_data)
}



# Data --------------------------------------------------------------------

data <- read_csv("ps3/data/data_brazil.csv") %>%
  set_names("Date", "Gdp", "Exchange", "Ipc") %>%
  slice_head(n = -1)

data_train <- filter(data, 1942 <= Date & Date <= 2019)
data_test <- filter(data, Date == 2020)



# Question 1 --------------------------------------------------------------

formulas <- list(
  Gdp ~ l(Gdp, 1:2) + l(Exchange, 1),
  Gdp ~ l(Gdp, 1:2) + l(Ipc, 1:2),
  Gdp ~ l(Gdp, 1:2) + l(Exchange, 1:2) + l(Ipc, 1:2),
  Gdp ~ l(Gdp, 1:2)
)

models_q1 <- map(formulas, ~lm(formulate_dylm(.x), data_train))

predictions_q1 <- map_dbl(models_q1, ~predict_dylm(.x, data))
mses_q1 <- (data_test$Gdp - predictions_q1)^2

stargazer(models_q1,
  dep.var.labels = "GDP Growth",
  label = glue("tb:ardl"),
  add.lines = list(
    c("Predictions", round(predictions_q1, 2)),
    c("MSE", round(mses_q1, 2))
  ),
  omit.stat = "f",
  table.placement = "H",
  no.space = TRUE
) %>%
  capture.output() %>%
  writeLines("ps3/tables/ardl.tex")



# Question 2 --------------------------------------------------------------

data_var <- data %>%
  select(-Date) %>%
  na.omit()

#labels <- paste0("VAR(", rep(1:3, each = 3), "): ", rep(colnames(data_var), 3))
ps <- 1:3

models_q2 <- map(ps, ~VAR(data_var, p = .x))

predictions_q2 <- map(models_q2, ~ predict(.x, n.ahead = 1)$fcst %>% map_dbl(~.x[,"fcst"]))
mses_q2 <- map(predictions_q2, ~(as.numeric(data_test[,-1]) - .x)^2)

pwalk(list(models_q2, predictions_q2, mses_q2, ps), function(mod, pred, mse, p) {
  stargazer(mod$varresult,
    column.labels = colnames(data_var),
    dep.var.labels.include = FALSE,
    model.numbers = FALSE,
    label = glue("tb:var{p}"),
    add.lines = list(
      c("Predictions", round(pred, 2)),
      c("MSE", round(mse, 2))
    ),
    omit.stat = "f",
    table.placement = "H",
    no.space = TRUE
  ) %>%
    capture.output() %>%
    writeLines(glue("ps3/tables/var{p}.tex"))
})

models_q2[[1]] <- VAR(data_var, p = 2) #{vars} gets confused with models created outside global env

# Install from: devtools::install_github("ricardo-semiao/varutils")
varutils::ggvar_irf(models_q2[[1]],
  n.ahead = 10,
  runs = 1000, # the default is orthogonalization
  facet = "ggh4x",
  args_facet = list(scales = "free_y", independent = "y")
)
ggsave(filename = "ps3/figures/irfs.png", width = 7, height = 6)



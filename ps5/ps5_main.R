
# Setup -------------------------------------------------------------------

try(setwd("ps5"), silent = TRUE)

library(glue)
library(gmm)
library(broom)
library(tidyverse)
box::use(../util_functions[
  output_stargazer,
  output_ggplot
]) # Or, run:
#source("https://github.com/ricardo-semiao/task-econometrics2/blob/main/util_functions.R?raw=TRUE")



# Question 1 --------------------------------------------------------------

set.seed(220122)

data_exp <- rexp(100000, 5)

g_exp <- function(theta, x) {
  cbind(1/theta - x, 1/(theta^2) - (x - 1/theta)^2)
}

g_exp_grad <- function(theta, x) {
  cbind(-1/theta^2, 2*x/theta^2)
}

mod_exp <- gmm(
  g_exp, data_exp, t0 = 5, gradv = g_exp_grad, type = "twoStep",
  method = "Brent", lower = 0, upper = 10
)

summary(mod_exp)
tidy(mod_exp) %>% output_stargazer("tables/gmm_exp.tex")



# Question 2 --------------------------------------------------------------

set.seed(220122)

data_arma <- arima.sim(list(ar = 0.2, ma = c(0.1, 0.1)), 100000) %>%
  as.vector() %>%
  tibble(y0 = .) %>%
  mutate(y1 = lag(y0, 1), y3 = lag(y0, 3)) %>%
  na.omit()

mod_arma <- gmm(
  data_arma$y0 ~ data_arma$y1, data_arma$y3, type = "twoStep"
)

summary(mod_arma)
tidy(mod_arma) %>% output_stargazer("tables/gmm_arma.tex")



# Question 3 --------------------------------------------------------------

set.seed(220122)

data_selic <- read_delim("data/selic.csv") %>%
  transmute(
    Date = as.Date(Data, format = "%d/%m/%Y"),
    Selic = (`Taxa (% a.a.)` + 1)^(1/365) - 1
  )

data_stocks <- map(c("ABEV3", "BBDC3", "BVSP", "ITUB3"), function(x) {
  read_delim(glue("data/{x}.csv")) %>%
    transmute(Date, "{x}" := (Close - lag(Close))/lag(Close))
}) %>%
  reduce(left_join, by = "Date")

data_capm <- left_join(data_stocks, data_selic, by = "Date") %>%
  transmute(across(-c(Date, Selic), ~.x - Selic)) %>%
  na.omit()

mod_capm <- gmm(
  as.matrix(select(data_capm, -BVSP)) ~ data_capm$BVSP, data_capm$BVSP,
  type = "twoStep"
)

summary(mod_capm)
tidy(mod_capms) %>% output_stargazer("tables/gmm_capms.tex")


test_R <- cbind(diag(3), matrix(0,nrow = 3, ncol = 3))
test_c <- rep(0, 3)

test_result <- car::linearHypothesis(
  model = mod_capm,
  hypothesis.matrix = test_R,
  rhs = test_c,
  test = "Chisq"
)

tidy(test_result) %>% output_stargazer("tables/test_capms.tex")

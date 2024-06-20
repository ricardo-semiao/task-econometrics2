
# Setup -------------------------------------------------------------------

try(setwd("ps5"), silent = TRUE)

library(glue)
library(tidyverse)
box::use(../util_functions[
  output_stargazer,
  output_ggplot
]) # Or, run:
#source("https://github.com/ricardo-semiao/task-econometrics2/blob/main/util_functions.R?raw=TRUE")



# Question 1 --------------------------------------------------------------

set.seed(220122)

x_exp <- rexp(100000, 5)

g_exp <- function(theta, x) {
  moment1 <- 1/theta - x
  cbind(moment1, 1/(theta^2) - (-moment1)^2)
}

g_exp_grad <- function(theta, x) {
  moment1 <- -1/theta^2
  cbind(moment1, 2*x*moment1)
}

mod_exp <- gmm::gmm(
  g_exp, x_exp, t0 = 5, gradv = g_exp_grad,
  type = "twoStep",
  method = "Brent", lower = 0, upper = 10
)

broom::tidy(mod_exp) %>% output_stargazer("tables/gmm_exp.tex")



# Question 2 --------------------------------------------------------------

set.seed(220122)

x_arma <- arima.sim(list(ar = 0.2, ma = c(0.1, 0.1)), 100000)

g_arma <- function(theta, x) {
  residuals <- x - lag(x, 3)*theta
  cbind(
    residuals,
    residuals^2 - 1,
    residuals * lag(residuals, 1),
    residuals * lag(residuals, 2)
  )
}

#g_arma_grad <- function(theta, x) {}

mod_arma <- gmm::gmm(
  g_arma, x_arma, t0 = c(0.2, 0.1, 0.1), #gradv = g_arma_grad,
  type = "twoStep"
)

broom::tidy(mod_arma) %>% output_stargazer("tables/gmm_arma.tex")



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

mod_capms <- map_dfr(select(data_capm, -BVSP), function(y) {
  g_capm <- function(theta, x) {
    residuals <- y - theta[1] - theta[2]*x
    cbind(residuals, residuals*x)
  }
  
  #g_capm_grad <- function(theta, x) {}
  
  mod <- gmm::gmm(
    g_capm, data_capm$BVSP, t0 = c(0, 1), #gradv = g_capm_grad,
    type = "twoStep"
  )
  
  broom::tidy(mod)
})

mod_capms %>% output_stargazer("tables/gmm_capms.tex")


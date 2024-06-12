# Packages and Functions --------------------------------------------------

try(setwd("ps4"), silent = TRUE)

library(glue)
library(tidyverse)
box::use(../util_functions[output_dftest]) # Or, run:
#source("https://github.com/ricardo-semiao/task-econometrics2/blob/main/util_functions.R?raw=TRUE")

theme_set(theme_bw())



# Question 1 --------------------------------------------------------------

data_brazil <- read_csv("data/brazil_data.csv",
  col_select = -4,
  col_types = "cnn"
) %>%
  set_names(c("Date", "BrazilExchange", "BrazilIpca")) %>%
  mutate(Date = as.Date(glue("{Date}.01"), format = "%Y.%m.%d"))

data_usa <- read_csv("data/usa_data.csv") %>%
  set_names(c("Date", "EuaCpi"))

data <- full_join(data_brazil, data_usa, by = "Date") %>%
  filter(Date >= as.Date("1995-01-01") & Date <= as.Date("2019-12-01")) %>%
  mutate(across(-Date, ~ 100 * (log(.x) - log(.x[1]))))


# Acording to PPP, $g_{exchange} = g_{price}/g_{price*}$, so
#$ln g_{exchange} = ln g_{price} - ln g_{price*}$, and:
data$Z <- t(c(1, -1, 1)) %*% t(select(data, -Date)) %>% as.vector()


varutils::ggvar_values_colored(data, index = data$Date)


iwalk(select(data, -Date), function(x, name) {
  cat(glue("\n\n{name}:\n\n"))
  output_dftest(ts(na.omit(x)), "tables/dftest_{str_to_lower(name)}.tex",
    nlag = 3,
    caption = unclass(glue("ADF Test - {name}"))
  )
})



# Question 2 --------------------------------------------------------------

coint_vecs <- map(colnames(data)[-1], function(col_name) {
  data_mod <- select(data, -Date) %>%
    relocate(all_of(col_name), .before = 1)
  
  mod <- lm(as.formula(glue("{col_name} ~ .")), data_mod)
  
  output_dftest(ts(na.omit(mod$residuals)),
    "tables/po_{str_to_lower(col_name)}.tex",
    nlag = 3,
    caption = unclass(glue("PO Test - {col_name}")),
    pval = FALSE
  )
  
  c(1, -mod$coefficients[-1]) %>% set_names(colnames(data_mod))
})

coint_vecs %>%
  bind_rows() %>%
  stargazer::stargazer(
    header = FALSE,
    summary = FALSE,
    rownames = FALSE,
    label = "tb:coint_vecs"
  ) %>%
  capture.output() %>%
  writeLines("tables/coint_vecs.tex")



# Question 3 --------------------------------------------------------------

test_jo <- urca::ca.jo(data[,-1],
  ecdet = "none",
  type  = "eigen",
  K = 2,
  spec = "transitory"
)

test_jo@V

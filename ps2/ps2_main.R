

# Data and Functions ------------------------------------------------------

library(tidyverse)
library(glue)
library(patchwork)
library(furrr)

set.seed(20240513)
plan(multisession, workers = 7)
theme_set(theme_bw())



# Question 2 --------------------------------------------------------------

mc_reps <- 10000
sample_size <- 10000
t <- 1:sample_size

delta <- 1
dfs <- c(1, 2, 5, 100000)

rejections <- matrix(nrow = mc_reps, ncol = length(dfs))

for (i in 1:mc_reps) {
  rejections[i,] <- map_lgl(dfs, function(df) {
    y <- delta*t + rt(sample_size, df)
    coefs <- summary(lm(y ~ t))$coefficients
    abs((coefs[2,1] - 1) / coefs[2,2]) >= qnorm(0.95)
  })
}

colMeans(rejections)


# Parallel version: 
rejections <- future_map(1:mc_reps, function(rep) {
  map_lgl(dfs, function(df) {
    y <- delta*t + rt(sample_size, df)
    coefs <- summary(lm(y ~ t))$coefficients
    abs((coefs[2,1] - 1) / coefs[2,2]) >= qnorm(0.95)
  })
},
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
)

colMeans(do.call(rbind, rejections))
# It might be a poor practice to parralelize the the monte-carlo level of the
#simulation, so I did not used these results



# Question 4 --------------------------------------------------------------

data <- read.csv("ps2/data/corn-production-land-us.csv") %>%
  rename(Hectares = 4, Production = 5)


g_hist <- ggplot(data, aes(Year, Production)) +
  geom_line() +
  labs(title = "Historical Values")

# Install from: devtools::install_github("ricardo-semiao/varutils")
g_acf <- varutils::ggvar_acf(data, series = "Production", lag.max = 50) +
  theme(strip.background = element_blank(), strip.text.x = element_blank()) +
  labs(title = "Auto-Correlation")

g_hist + g_acf + plot_annotation("US Corn Production")
ggsave(filename = "ps2/figures/corn_prod.png", width = 6, height = 3.5)


test_result <- aTSA::adf.test(data$Production)

test_result_pretty <- test_result %>%
  imap_dfr(~ tibble(type = .y, as_tibble(.x))) %>%
  mutate(
    p.value = glue("({round(p.value, 2)})"),
    ADF = round(ADF, 2)
  ) %>%
  unite(Statistic, ADF, p.value, sep = "\n") %>%
  pivot_wider(names_from = type, values_from = "Statistic") %>%
  set_names("Lag", glue("Type {1:3}"))

stargazer::stargazer(test_result_pretty,
    summary = FALSE,
    rownames = FALSE
  ) %>%
  capture.output() %>%
  writeLines("ps2/tables/adf_test.tex")

# Other functions: tseries::adf.test, urca::ur.df, bootUR::adf, CADFtest::CADFtest, fUInitRoots::adfTest




# Data and Functions ------------------------------------------------------

library(tidyverse)
library(glue)
library(patchwork)

set.seed(20240513)
theme_set(theme_bw())



# Question 2 --------------------------------------------------------------

sample_size <- 10000
t <- 1:sample_size

delta <- 1

dfs <- c(1, 5)

mc_reps <- 10000

rejections <- matrix(nrow = mc_reps, ncol = 2)

for (i in 1:mc_reps) {
  y <- map(dfs, ~ delta*t + rt(sample_size, .x))
  coefs <- map(y, ~ summary(lm(.x ~ t))$coefficients)
  rejections[i,] <- map_lgl(coefs, ~ abs((.x[2,1] - 1) / .x[2,2]) >= qnorm(0.95))
}

colMeans(rejections)



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
ggsave(filename = "ps2/figures/corn_prod.png")



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

stargazer::stargazer(test_result_pretty, summary = FALSE) %>%
  capture.output() %>%
  writeLines("ps2/tables/adf_test.tex")

# Other functions: tseries::adf.test, urca::ur.df, bootUR::adf, CADFtest::CADFtest, fUInitRoots::adfTest


# Data and Functions ------------------------------------------------------

library(glue)
library(furrr)
library(tidyverse)
box::use(../util_functions[
  prettify_model_names,
  output_stargazer,
  output_ggplot
]) # Or, run:
#source("https://github.com/ricardo-semiao/task-econometrics2/blob/main/util_functions.R?raw=TRUE")

set.seed(20240513)
plan(multisession, workers = 7)

theme_set(theme_bw())
pal <- c("darkgreen", "darkorange")


get_prob_conv_data <- function(x, thresholds, coefs) {
  map(thresholds, function(thresh) {
    colMeans(x > matrix(coefs*thresh, nrow = 1)[rep(1, mc_reps),])
  }) %>%
    do.call(rbind, .)
}

get_dist_conv_data <- function(x, quantiles) {
  apply(x, 2, \(x) quantile(x, probs = quantiles, na.rm = TRUE))
}

get_test_size_data <- function(x) {
  colMeans(x)
}


plot_prob_conv <- function(x, model_name, coefs) {
  graph_data <- x %>%
    pivot_longer(-(distribution:threshold), names_to = "coef") %>%
    mutate(
      across(c(threshold, distribution), as.factor),
      coef = fct_relevel(coef, names(coefs))
    )
  
  ggplot(graph_data, aes(size, value, color = threshold)) +
    geom_line() +
    geom_hline(yintercept = 0, linewidth = 0.75, linetype = "dashed") +
    facet_grid(vars(distribution), vars(coef), scales = "free_y", labeller = labels) +
    labs(
      title = glue("Convergence in Probability - Model {model_name}"),
      x = "Sample Size",
      y = "Proportion of Rejections",
      color = "Threshold\n(prop. of coef.)"
    ) +
    scale_color_brewer(palette = "BuPu")
}

plot_dist_conv <- function(x, model_name, coefs) {
  graph_data <- x %>%
    filter(size %in% c(30, 50, 100, 150, 250, 500)) %>%
    pivot_longer(-(distribution:quantile), names_to = "coef") %>%
    mutate(
      across(c(size, distribution), as.factor),
      coef = fct_relevel(coef, names(coefs))
    )
  
  ggplot(graph_data, aes(value, quantile)) +
    geom_line(aes(color = size)) +
    stat_function(fun = pnorm, linewidth = 0.75, linetype = "dashed") +
    facet_grid(vars(distribution), vars(coef), scales = "free_x", labeller = labels) +
    labs(
      title = glue("Convergence in Distribution - Model {model_name}"),
      x = "Normalized coefficient",
      y = "CDF",
      color = "Sample size"
    ) +
    scale_color_brewer(palette = "BuPu")
}

plot_test_size <- function(x, model_name, coefs) {
  graph_data <- x %>%
    pivot_longer(-(distribution:size), names_to = "coef") %>%
    mutate(
      distribution = as.factor(distribution),
      coef = fct_relevel(coef, names(coefs))
    )
  
  ggplot(graph_data, aes(size, value)) +
    geom_line(color = "#153E7E") +
    geom_hline(yintercept = 0.05, linewidth = 0.75, linetype = "dashed") +
    facet_grid(vars(distribution), vars(coef), scales = "free_y", labeller = labels) +
    labs(
      title = glue("Convergence of Test Size - Model {model_name}"),
      x = "Sample size",
      y = "Rejection rate"
    )
}



# Question 1 --------------------------------------------------------------

# Setup:
data <- read_csv("ps1/data/data_gdp_brazil.csv") %>%
  rename(PIB = 2)

orders <- list(
  ar1 = c(1, 0, 0),
  ar2 = c(2, 0, 0),
  ma1 = c(0, 0, 1),
  ma2 = c(0, 0, 2),
  arma1_1 = c(1, 0, 1),
  arma2_1 = c(2, 0, 1),
  arma1_2 = c(1, 0, 2),
  arma2_2 = c(2, 0, 2)
)

models <- map(orders, ~ arima(data$PIB, order = .x))

pretty_names <- prettify_model_names(names(models))


# Results:
iwalk(list(`1_to_4` = 1:4, `5_to_8` = 5:8), function(inds, name) {
  output_stargazer(models[inds], "ps1/tables/models_{name}.tex",
    column.labels = pretty_names[inds],
    dep.var.caption = "",
    model.numbers = FALSE,
    report = "vcsp",
    omit.stat = "aic",
    add.lines = list(
      c("AIC", map_dbl(models, ~ round(AIC(.x) ,3))),
      c("BIC", map_dbl(models, ~ round(BIC(.x) ,3)))
  ))
})

predictions <- imap(models, function(mod, name) {
  predict(mod, n.ahead = 10) %>%
    as_tibble() %>%
    mutate(
      Data = max(data$Data) + 1:10,
      value = "Prediction",
      PIB = pred,
      pred = NULL
    )
})

iwalk(predictions, function(pred, name) {
  plot <- ggplot(data, aes(Data, PIB)) +
    geom_line() +
    geom_line(data = pred,
      linetype = "dashed", color = pal[1], linewidth = 0.75
    ) +
    geom_ribbon(data = pred, aes(ymin = PIB - se, ymax = PIB + se),
      linetype = "dashed", alpha = 0.2, fill = pal[2], color = pal[2], linewidth = 0.75
    ) +
    labs(
      title = glue("Prediction of PIB - Model {pretty_names[name]}"),
      x = "Year"
    )
  
  output_ggplot("ps1/figures/{name}_pred.png", 6, 3, plot)
})



# Question 2 --------------------------------------------------------------

# Setup:
parameters <- list(
  ma1 = list(order = c(0,0,1), ma = 0.5),
  ar1 = list(order = c(1,0,0), ar = 0.3),
  arma1_1 = list(order = c(1,0,1), ar = 0.3, ma = 0.5)
)

distributions <- list(norm = rnorm, exp = rexp)

sample_sizes <- c(
  seq(30, 100, 1),
  seq(110, 200, 10),
  seq(250, 500, 50)
)

mc_reps <- 500 #1000

thresholds <- c(0.2, 0.5, 0.8)
quantiles <- seq(0, 1, 0.01)

labels <- labeller(
  coef = c(ar1 = "AR1", ma1 = "MA1", intercept = "Intercept"),
  distribution = c(norm = "Normal", exp = "Exponential")
)

combinations <- list(
  conv_prob = expand_grid(
    distribution = names(distributions), size = sample_sizes, threshold = thresholds
  ),
  conv_dist = expand_grid(
    distribution = names(distributions), size = sample_sizes, quantile = quantiles
  ),
  test_size = expand_grid(
    distribution = names(distributions), size = sample_sizes
  )
)


# Results:
#p_name = "ma1"; params = parameters[[p_name]]
all_results <- imap(parameters, function(params, p_name) {
  
  coefs <- c(params[-1], "intercept" = 0, recursive = TRUE)
  
  #d_name = "exp"; dist = distributions[[d_name]]
  data_per_dist <- imap(distributions, function(dist, d_name) {
    
    cat(glue("\nIteration: {p_name} - {d_name}\n"))
    
    #size = sample_sizes[[1]]
    data_per_size <- future_map(sample_sizes, function(size) {
      
      #rep = NULL
      mc_results <- map(1:mc_reps, function(rep) {
        y <- do.call(arima.sim,
          args = c(model = list(params), n = size, rand.gen = dist)
        )
        
        model <- arima(y, order = params$order, method = "ML")
        
        bias <- model$coef - coefs
        bias_normalized <- bias/sqrt(diag(model$var.coef))
        
        list(
          bias = abs(bias),
          bias_normalized = bias_normalized,
          reject = abs(bias_normalized) >= qnorm(0.975)
        )
      })
      
      mc_results_agg <- transpose(mc_results) %>% map(~ do.call(rbind, .x))
      coefs <- set_names(coefs, colnames(mc_results_agg[[1]]))
      
      list(
        conv_prob = get_prob_conv_data(mc_results_agg$bias, thresholds, coefs),
        conv_dist = get_dist_conv_data(mc_results_agg$bias_normalized, quantiles),
        test_size = get_test_size_data(mc_results_agg$reject)
      )
    },
      .options = furrr_options(seed = TRUE),
      .progress = TRUE
    )
    
    transpose(data_per_size) %>% map(~ do.call(rbind, .x))
  })
  
  data_agg <- list_transpose(data_per_dist) %>%
    map2(combinations, ~ do.call(rbind, .x) %>% cbind(.y, .) %>% as_tibble())
  
  model_name <- prettify_model_names(p_name)
  
  list(
    datas = data_agg,
    plots = list(
      conv_prob = plot_prob_conv(data_agg$conv_prob, model_name, coefs),
      conv_dist = plot_dist_conv(data_agg$conv_dist, model_name, coefs),
      test_size = plot_test_size(data_agg$test_size, model_name, coefs)
    )
  )
})

if (FALSE) {
  saveRDS(all_results, "ps1/data/simulation_results.rds")
  
  iwalk(all_results, function(p, p_name) {
    iwalk(p$plots, function(g, g_name) {
      output_ggplot("ps1/figures/{p_name}_{g_name}.png", 6, 4.5, g)
    })
  })
}

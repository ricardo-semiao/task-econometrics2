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

formulate_tslm <- function(formula) {
  formula_rhs <- as.character(formula)[3] %>%
    str_split_1("( ?\\+ ?)|( ?- ?)") %>%
    map_chr(lags_convert) %>%
    paste0(collapse = " + ")
  
  c(as.character(formula)[2:1], formula_rhs) %>%
    paste0(collapse = " ") %>%
    as.formula()
}

predict_tslm <- function(model, new_data, n = 1) {
  variables <- colnames(model$model)[-1] %>% set_names()
  predict_data <- with(new_data,
                       map_dfc(variables, ~eval(parse(text = .x)))
  ) %>%
    slice_tail(n = n)
  
  model$coefficients[1] + model$coefficients[-1] %*% t(predict_data)
}

require(rlang)
require(purrr)

expr_collapse <- function(expr, collapse = "+") {
  reduce(expr, ~as.call(list(sym(collapse), .x, .y)))
}


add_lags <- function(call_short, order = TRUE) {
  lags <- eval(call_short[[3]]) %||% 1
  if (order) lags <- lags[order(lags)]
  call_long_items <- map(lags, ~ expr(lag(!!call_short[[2]], !!.x)))
  expr_collapse(call_long_items)
}

add_diffs <- function(call_short, order = TRUE) {
  lags <- eval(call_short[[3]]) %||% 1
  if (order) lags <- lags[order(lags)]
  call_long_items <- map(lags, ~ expr(diff(!!call_short[[2]], !!.x)))
  expr_collapse(call_long_items)
}

add_trend <- function(call_short) stop("t() is not yet implemented")

add_season <- function(call_short) stop("s() is not yet implemented")

add_harmon <- function(call_short, order = TRUE) {
  multiples <- eval(call_short[[2]]) %||% 1
  if (order) multiples <- multiples[order(multiples)]
  mode <- eval(call_short[[3]]) %||% "both"
  
  sin_items <- map(multiples, ~ expr(sin(!!.x * 2 * pi * !!index(!!formula[[1]]))))
  cos_items <- map(multiples, ~ expr(cos(!!.x * 2 * pi * !!index(!!formula[[1]]))))
  switch(mode,
    "both" = c(expr_collapse(sin_items), expr_collapse(cos_items)),
    "sin" = expr_collapse(sin_items),
    "cos" = expr_collapse(cos_items)
  )
}


flatten_formula_rhs <- function(formula, reverse = TRUE) {
  formula_rhs_flat <- list()
  f <- if (is_formula(formula)) formula[[3]] else formula
  
  if (f[[1]] != as.symbol("+")) {
    formula_rhs_flat <- list(f)
  } else {
    formula_rhs_flat <- c(
      formula_rhs_flat,
      flatten_formula_rhs(f[[3]]),
      flatten_formula_rhs(f[[2]])
    )
  }

  if (reverse) formula_rhs_flat <- rev(formula_rhs_flat)
  set_names(formula_rhs_flat, as.character(formula_rhs_flat))
}

reformulate_tslm <- function(formula, reverse = TRUE, order = TRUE) {
  formula_rhs_flat <- flatten_formula_rhs(formula, reverse)
  
  formula_rhs_items <- map(formula_rhs_flat, function(call) {
    if (is_symbol(call)) return(call)
    switch(as.character(call[[1]]),
           "l" = add_lags(call, order = order),
           "d" = add_diffs(call),
           "t" = add_trend(call),
           "s" = add_season(call),
           "h" = add_harmon(call),
           call
    )
  })
  
  formula[[3]] <- expr_collapse(formula_rhs_items)
  formula
}

predict_tslm <- function(model, new_data, n = 1) {
  formula_rhs_flat <- flatten_formula_rhs(model$terms)
  pred_data <- with(new_data, map_dfc(formula_rhs_flat, ~eval(.x)))
  pred_data <- pred_data[(nrow(pred_data) + 1 - n):nrow(pred_data),]
  model$coefficients[1] + model$coefficients[-1] %*% t(pred_data)
}


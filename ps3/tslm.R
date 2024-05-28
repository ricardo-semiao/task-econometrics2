require(rlang)
require(purrr)

expr_collapse <- function(expr, collapse = "+") {
  reduce(expr, ~as.call(list(sym(collapse), .x, .y)))
}

add_lags <- function(call_short, order = TRUE) {
  lags <- eval(call_short[[3]])
  if (order) lags <- lags[order(lags)]
  call_long_items <- map(lags, ~ expr(lag(!!call_short[[2]], !!.x)))
  expr_collapse(call_long_items)
}
add_diffs <- function(call_short) stop("d() is not yet implemented")
add_trend <- function(call_short) stop("t() is not yet implemented")
add_season <- function(call_short) stop("s() is not yet implemented")

flatten_formula_rhs <- function(formula, reverse = TRUE) {
  formula_rhs_flat <- list()
  f <- if (is_formula(formula)) formula[[3]] else formula
  i <- 1
  
  is_plus_call <- f[[1]] == as.symbol("+")
  if (!is_plus_call) formula_rhs_flat <- list(f)
  
  while (is_plus_call) {
    formula_rhs_flat[[i]] <- f[[3]]
    is_plus_call <- f[[2]][[1]] == as.symbol("+")
    add <- flatten_formula_rhs(f[[2]])
    if (!is_plus_call) formula_rhs_flat[[i + 1]] <- add
    f <- add
    i <- i + 1
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
           call
    )
  })
  
  formula[[3]] <- expr_collapse(formula_rhs_items)
  formula
}

predict_tslm <- function(model, new_data, n = 1) {
  formula_rhs_flat <- flatten_formula_rhs(model$terms)
  pred_data <- with(new_data, map_dfc(formula_rhs_flat, ~eval(.x)))
  pred_data <- pred_data[(length(pred_data) + 1 - n):length(pred_data),]
  model$coefficients[1] + model$coefficients[-1] %*% t(pred_data)
}


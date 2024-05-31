box::use(
  utils[capture.output],
  stats[as.formula],
  dplyr[...],
  tidyr[...],
  purrr[...],
  stringr[...],
  rlang[...],
  ggplot2[ggsave, last_plot],
  stargazer[stargazer],
  glue[glue]
)

# Functions for Pretty Output ---------------------------------------------

eval_identifiers <- function(filename, label, env_call) {
  filename <- glue(filename, .envir = env_call)
  
  if (isFALSE(label)) {
    label <- NULL
  } else {
    if (is.null(label)) {
      label <- gsub("^.+/(.+)\\..+$", "tb:\\1", filename)
      warning(glue("No label supplied, using '{label}'"), call. = FALSE)
    } else {
      label <- glue(label, .envir = env_call)
    }
  }
  
  list(filename = filename, label = label)
}


#' Prettify Model Names
#' @export
prettify_model_names <- function(x) {
  x %>%
    str_replace("([a-z]+)([0-9]+[_0-9]*)*", "\\1(\\2)") %>%
    str_replace("_", ",") %>%
    str_to_upper() %>%
    set_names(x)
}


#' Save aTSA's DF Test as Latex File
#' @export
output_dftest <- function(data, filename, label = NULL, ...) {
  id <- eval_identifiers(filename, label, parent.frame(1))
  
  pretty_dftest <- aTSA::adf.test(data, ..., output = FALSE) %>%
    imap_dfr(~ tibble(type = .y, as_tibble(.x))) %>%
    mutate(
      p.value = glue("({round(p.value, 2)})"),
      ADF = round(ADF, 2)
    ) %>%
    unite(Statistic, ADF, p.value, sep = "\n") %>%
    pivot_wider(names_from = type, values_from = "Statistic") %>%
    set_names("Lag", glue("Type {1:3}"))

  pretty_dftest %>%
    stargazer(
      header = FALSE,
      summary = FALSE,
      rownames = FALSE,
      label = id$label
    ) %>%
    capture.output() %>%
    writeLines(id$filename)
  
  print(pretty_dftest)
}


#' Save stargazer as Latex File
#' @export
output_stargazer <- function(x, filename, label = "", ...) {
  id <- eval_identifiers(filename, label, parent.frame(1))
  
  x %>%
    stargazer(
      out.header = FALSE,
      table.placement = "H",
      label = id$label,
      ...
    ) %>%
    capture.output() %>%
    writeLines(id$filename)
}


#' Save ggplot Picture
#' @export
output_ggplot <- function(filename, width, height, plot = last_plot(), ...) {
  id <- eval_identifiers(filename, FALSE, parent.frame(1))
  ggsave(id$filename, plot = plot, width = width, height = height)
}


# Functions for tslm ------------------------------------------------------

expr_collapse <- function(expr, collapse = "+") {
  reduce(expr, ~as.call(list(sym(collapse), .x, .y)))
}

l <- function(x, lags = 1) {
  map(lags, \(lag) expr(lag({{x}}, lag)))
}

d <- function(x, lags = 1) {
  map(lags, \(lag) expr(diff({{x}}, lag)))
}

t <- function(...) stop("t() is not yet implemented")

s <- function(...) stop("s() is not yet implemented")

h <- function(multiples, mode = "both") {
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


#' Convert Shorthands on dynlm Style Formula
#' @export
reformulate_tslm <- function(formula, reverse = TRUE) {
  formula_rhs_flat <- flatten_formula_rhs(formula, reverse)
  
  formula[[3]] <- map(formula_rhs_flat, function(call) {
    if (is_symbol(call)) call else expr_collapse(!!call)
  }) %>%
    expr_collapse

  formula
}


#' Predict dynlm Style Models
#' @export
predict_tslm <- function(model, new_data, n = 1) {
  formula_rhs_flat <- flatten_formula_rhs(model$terms)
  pred_data <- with(new_data, map_dfc(formula_rhs_flat, ~eval(.x)))
  pred_data <- pred_data[(nrow(pred_data) + 1 - n):nrow(pred_data),]
  model$coefficients[1] + model$coefficients[-1] %*% t(pred_data)
}

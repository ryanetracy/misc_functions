package_loader <- function(package_list) {
  for (p in package_list) {
    if (!(p %in% installed.packages())) {
      install.packages(p)
    }
    lapply(p, library, character.only = T)
  }
}


model_summary_lmer <- function(model, paired = T) {
  summ <- summary(model)
  mod_se <- sqrt(diag(vcov(model)))
  std_coef <- parameters::standardize_parameters(model,
                                                 verbose = F)
  
  std_coef_val <- std_coef[, 2]
  std_coef_ll <- std_coef[, 4]
  std_coef_ul <- std_coef[, 5]
  
  summ_df <- as.data.frame(summ$coefficients)
  
  d_val <- effectsize::t_to_d(t = summ_df$`t value`,
                              df = summ_df$df,
                              paired = paired)
  
  est_d <- d_val[, 1]
  est_d_ci_ll <- d_val[, 3]
  est_d_ci_ul <- d_val[, 4]
  
  output <- cbind(
    round(summ$coefficients, 3),
    coef_ci_ll = round(fixef(model) - (1.96 * mod_se), 3),
    coef_ci_ul = round(fixef(model) + (1.96 * mod_se), 3),
    std_beta = round(std_coef_val, 3),
    std_beta_ci_ll = round(std_coef_ll, 3),
    std_beta_ci_ul = round(std_coef_ul, 3),
    est_d = round(est_d, 3),
    est_d_ci_ll = round(est_d_ci_ll, 3),
    est_d_ci_ul = round(est_d_ci_ul, 3)
  )
  
  output <- as.data.frame(output)
  names(output)[5] <- 'p_value'
  
  
  return(output)
}


model_summary_glmer <- function(model) {
  summ <- summary(model)
  odds_ratio <- round(exp(fixef(model)), 3)
  mod_se <- sqrt(diag(vcov(model)))
  
  output <- cbind(
    round(summ$coefficients, 3),
    odds_ratio,
    OR_ci_ll = round(fixef(model) - (1.96 * mod_se), 3),
    OR_ci_ul = round(fixef(model) + (1.96 * mod_se), 3)
  )
  
  output <- as.data.frame(output)
  names(output)[5] <- 'p_value'
  
  return(output)
}


get_cohens_d <- function(m1, m2, sd1, sd2) {
  pooled_sd <- sqrt((sd1 ^ 2 + sd2 ^ 2) / 2)
  
  d_val <- (m2 - m1) / pooled_sd
  
  return(d_val)
}


find_nas <- function(df, by_col = TRUE) {
  missing_df <- as.data.frame(is.na(df))
  if (by_col) {
    cols_missing <- data.frame(n_missing = colSums(missing_df))
    return(cols_missing)
  }
  else {
    rows_missing <- data.frame(n_missing = rowSums(missing_df))
    return(rows_missing)
  }
}

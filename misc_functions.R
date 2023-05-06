package_loader <- function(package_list) {
  for (p in package_list) {
    if (
      !(p %in% installed.packages())
    ) {
      install.packages(p)
    }
    lapply(p, library, character.only = T)
  }
}


model_summary_lmer <- function(model) {
  summ <- summary(model)
  mod_se <- sqrt(diag(vcov(model)))
  std_coef <- parameters::standardize_parameters(model,
                                                 verbose = F)
  
  output <- cbind(
    round(summ$coefficients, 3),
    coef_ci_ll <- round(fixef(model) - (1.96 * mod_se), 3),
    coef_ci_ul <- round(fixef(model) + (1.96 * mod_se), 3),
    std_beta <- std_coef
  )
  
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
  names(output)[4] <- 'p_value'
  
  return(output)
}
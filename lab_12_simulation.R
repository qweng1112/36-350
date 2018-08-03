generate_data = function(n,p) {
  return(list(covariates = matrix(rnorm(n=n*p), nrow=n, ncol=p), responses = as.vector(rnorm(n))))
}

model_select = function(covariates, responses, cutoff) {
  regression.lm = lm(responses ~ covariates)
  p_values = coef(summary(regression.lm))[-1,4]
  
  good_cova = as.numeric(which(p_values <= cutoff))
  
  if (length(good_cova) == 0) {
    return(numeric(0))
  }
  
  new_reg.lm = lm(responses ~ covariates[,good_cova])
  return(reduced_p_values = coef(summary(new_reg.lm))[,4])
}
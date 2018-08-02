generate_data = function(n,p) {
  return(list(covariates = matrix(rnorm(n=n*p), nrow=n, ncol=p), responses = as.vector(rnorm(n))))
}


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

run_simulation = function(n_trials, n, p, cutoff) {
  #browser()
  p_values = replicate(n_trials, {model_select(generate_data(n,p)$covariates,
                                               generate_data(n,p)$responses, 
                                               cutoff)})
  jpeg(file = paste0("hist_pvalues", n, "_", p, ".jpeg"))
  hist(unlist(p_values), xlab = "returned p-values")
  dev.off()
}

n = c(100, 1000, 10000)
p = c(10, 20, 50)
n_trials = 1000
cutoff = 0.05

comb = expand.grid(n,p)
names(comb) = c("n", "p")

par(mfrow = c(3,3))
set.seed(4)
for(i in 1:nrow(comb)) {
  run_simulation(n_trials, comb[i,1], comb[i,2], cutoff)
}

par(mfrow = c(3,3))
set.seed(5)
for(i in 1:nrow(comb)) {
  run_simulation(n_trials, comb[i,1], comb[i,2], cutoff=1)
}
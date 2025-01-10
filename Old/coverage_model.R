
# BDB 2025 - Coverage -----------------------------------------------------

## data looks like

n_def = 11
n_att = 6
n_obs = 100

X = matrix(NA, n_def * n_obs, n_att)
for(i in 1:n_att) X[,i] = rep(cumsum(rnorm(n_obs)), n_def)
colnames(X) = paste0("x_", 1:n_att)

y_pos = as.vector(sapply(1:n_def, function(i) 2*i + cumsum(rnorm(n_obs))))

data = data.frame(ID = rep(1:n_def, each = n_obs),
                  y_pos = y_pos)
data = cbind(data, X)


## likelihood function

# devtools::install_github("janoleko/LaMa")
library(LaMa)
           
nll = function(par){
  getAll(par, dat)
  
  Gamma = tpm(eta)
  
  sigma = exp(logsigma)
  alpha = plogis(logitalpha)
  
  # assuming y variable is centered -> pulling to the middle
  # alpha * X + (1-alpha) * y_middle
  Mu = alpha * X
  REPORT(Mu)
  
  allprobs = matrix(1, length(y_pos), n_att)
  ind = which(!is.na(y_pos))
  for(j in 1:n_att){
    allprobs[ind,j] = dnorm(y_pos, Mu[,j], sigma)
  }
  
  -forward(Delta, Gamma, allprobs, trackID = ID)
}

## deterministic initial distribution
Delta = t(
  sapply(split(data, data$ID),
         function(x){
           delta = numeric(n_att)
           delta[which.min(abs(x$y_pos[1] - x[1,-(1:2)]))] = 1
           delta
         })
)
  
## fit model
dat = list(y_obs = data$y_obs,
           X = data[,-(1:2)],
           ID = data$ID,
           n_att = n_att, 
           Delta = as.matrix(Delta))

par = list(eta = rep(-4, n_att * (n_att - 1)),
           logsigma = log(1),
           logitalpha = qlogis(0.95))

obj = MakeADFun(nll, par)
opt = nlminb(obj$par, obj$fn, obj$gr)

mod = obj$report()
# Delta = mod$Delta
# Gamma = mod$Gamma
# allprobs = mod$allprobs
# trackID = mod$trackID

probs = stateprobs(mod = mod)
plot(probs[1,], type = "h")
for(t in 2:n_obs){
  plot(probs[t,], type = "h")
  Sys.sleep(0.1)
}

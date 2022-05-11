## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(AccelBenchmark)

## ----poisson, message=FALSE, warning=FALSE------------------------------------
# special control parameters for quasi-newton
qn_control = list(qn=2)

# set seed for reproducibility 
# randomness comes from parameter initialization 
# or data generation process
set.seed(54321)
res_poi = benchmark(
  task = "poissmix",
  algorithm = c("raw", "squarem", "qn", "pem", "daarem", "nes"),
  ntimes = 20,
  control = list(maxiter=2000, tol=1e-7),
  control.spec = list(qn_v2=qn_control),
  verbose = FALSE
)

plot(res_poi, type="iter", start_from=5)

summary(res_poi, rate_tol=0.1)

## ----lasso, eval=FALSE--------------------------------------------------------
#  sqem_ctrl = list(step.min0=0)
#  
#  set.seed(54321)
#  res_l1 = benchmark(
#    "lasso",
#    algorithm=c("raw", "squarem", "qn_v3", "pem", "daarem", "nes"),
#    ntimes = 200,
#    control = list(maxiter=20000),
#    control.spec = list(squarem=sqem_ctrl),
#    lam = 1
#  )

## ----toy, message=FALSE, warning=FALSE----------------------------------------
# Initialization function 
# make sure this function takes no parameters
toy_init = function(){
  runif(1, 0, 1)
}

# Fixed point iteration
toy_fixptfn = function(par, a, b){
  b / (a + par)
}

# Loss function
toy_objfn = function(par, a, b){
  sqrt((par - (sqrt(a^2 + 4*b) - a) / 2)^2)
}

# task function
# returned list can contain any extra parameters
# they will be passed in fixptfn and objfn
# make sure their parameter names match
toy_task = function(a=1, b=1){
  list(
    initfn = toy_init,
    fixptfn = toy_fixptfn,
    objfn = toy_objfn,
    a = a, b = b
  )
}

# benchmarking
set.seed(54321)
res_toy = benchmark(
  toy_task,
  algorithm=c("raw", "squarem", "qn", "pem", "daarem", "nes"),
  ntimes = 10,
  control = list(maxiter=1000),
  verbose = FALSE,
  a = 11, b = 100
)

summary(res_toy, loss_tol=1e-7)


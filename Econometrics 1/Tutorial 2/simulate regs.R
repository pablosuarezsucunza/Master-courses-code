# doing parallel programming --------------------------------------------------------------------------------------
library(MASS)
library(tidyverse)
library(foreach)
library(doParallel)

remove(list = ls())

mu = c(25, 28, 9)
Sigma = matrix(c(1, 0.3, -0.2, 0.3, 1, -0.4, -0.2, -0.4, 1), nrow = 3, ncol = 3)

set.seed(1016)

registerDoParallel(cores = 7) # set number of cores to use in parallel

simulations <- 10000

start <- Sys.time() # to check how much time it takes
betas <- foreach(k = 1:simulations, .combine = "rbind") %dopar% {
  df <- data.frame(MASS::mvrnorm(n = 100, mu, Sigma)) 
  reg <- lm(data = df, X1 ~ X2 + X3)
  
  b = c(reg$coefficients[1], reg$coefficients[2], reg$coefficients[3])
}

betas <- as.data.frame(betas) %>% 
  rename(beta1 = X2,
         beta2 = X3)
row.names(betas) <- NULL

#to check how much time it took the simulations
start - Sys.time()














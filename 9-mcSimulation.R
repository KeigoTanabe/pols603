# ---------------------------
# Title: Review of MC experiments (POLS 603)
# Keigo Tanabe
# Last modified on: Apr 6, 2022
# ---------------------------

# Preparation  -------------
library(tidyverse); library(here); library(conflicted)
library(glue)
set.seed(20220406) 


# DGP  -----------------
n <- 1000  # sample size. 
X <- runif(n, -5, 5) # X is fixed 
reps <- 5000 # set the number of repetitions. at least 1000. often more. But note it takes longer.

b0 <- 1
b1 <- 2


# MC experiment --------
n_max <- reps
par.est <- matrix(NA, nrow=n_max, ncol = 2)  
for (i in 1:n_max) {
  
  # Draw ----
  error <- rnorm(n, 0, 4) # the stochastic part N(0,1) error.
  Y <- b0 + b1*X + error  # the true DGP 
  
  # Estimation ----
  model <- lm(Y ~ X) # ols
  

  # Saving the results ----
  par.est[i,1] <- model$coef[1] # 'i' for each iteration.
  par.est[i,2] <- model$coef[2] 
  
  if(i %% 1000 == 0){print(i)}else{next}

}


# Results
hist(par.est[,1], breaks=250)
hist(par.est[,2], breaks=250)


# MC experiment with plots and comments -------
ped <- T # turn F to run 5000 simulations without plots and comments. 
n_max <- 5 # if ped = T, n_max <- 5. otherwise, n_max <- reps

# an empty matrix to store the estimates
par.est <- matrix(NA, nrow=n_max, ncol = 2)  
  # you can also use lists, vectors, or dataframe to store results.
 
for (i in 1:n_max) {
  
  # time ----

  # Draw ----
  if(ped){cat("================ New observations ================\n")}
  error <- rnorm(n, 0, 4) # the stochastic part N(0,1) error.
  Y <- b0 + b1*X + error  # the true DGP 
  
  if(ped){cat("# See the plot.")}
  
  if(ped){print(plot(X, Y, pch=19, xlim=c(-5,5),ylim=c(-20,20),main = glue::glue("Data for iteration: {i}")))}
  
  # Estimation ----
  model <- lm(Y ~ X) # ols
  
  if(ped){Sys.sleep(1)
    mtext(glue("E[y|x] = {round(coef(model)[1],3)} + {round(coef(model)[2],3)}*x"))
    Sys.sleep(1)}
  
  # Saving the results ----
  if(ped){cat("================ Save the results ================\n")}
  par.est[i,1] <- model$coef[1] # 'i' for each iteration.
  par.est[i,2] <- model$coef[2] 
  
  if(ped){Sys.sleep(1)
    print(head(par.est))
    Sys.sleep(1.5)}
  

  # Progress
  if(ped){
    cat(" ================ completed draw",i, " ================\n \n")
    Sys.sleep(1)
    }else{if(i %% 1000 == 0){print(i)}else{next}}
  
  # cat("completed in ", tm - Sys.time(), " seconds\n")
}



# for more.
browseURL("https://bookdown.org/rdpeng/rprogdatascience/parallel-computation.html")
browseURL("https://methods.sagepub.com/book/monte-carlo-simulation-and-resampling-methods-for-social-science")



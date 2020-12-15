#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(plyr)
library(dequer)
library(hash)
library(earth)
library(mda)

compute_penalty_bic <- function(free_parameters, data_size){
  #print(log(data_size)/2*free_parameters)
  return (log(data_size)/2*free_parameters) 
}

print_score <- function(log_likelihood, penalty, predictor_vars, score_file, metrics_file, mars_model){
  tmp <- unique(unlist(strsplit(predictor_vars, "V")))
  parents = tmp[tmp!=""]
  i=as.integer(parents)
  parents = i-1
  score <-  toString(log_likelihood - penalty)
  score <-  paste(score, length(parents))
  parent_str <- paste(parents, collapse = " ")
  score_string <- paste(score, parent_str)
  metrics <- paste(mars_model$glm.stats[6], mars_model$gcv, mars_model$rsq, length(mars_model$coefficients), length(parents))
  write(score_string, score_file, append = TRUE)
  write(metrics, metrics_file, append = TRUE)

}

print_score_null <- function(log_likelihood, score_file){
  score <-  toString(log_likelihood) 
  score_string <- paste(score, "0")
  write(score_string, score_file, append = TRUE)
}


score_model <- function(mars_model, response_var_id, predictor_vars, data, score_file, metrics_file){ 
  #get penalty
  penalty <- compute_penalty_bic(length(mars_model$coefficients), nrow(data))
  #compute loglikelihood
  log_likelihood=0
  Omega_Pi_i <- expand.grid(replicate(length(predictor_vars), 0:1, simplify = FALSE))
  names(Omega_Pi_i) <- predictor_vars
  
  for (j in seq(1,2^length(predictor_vars))){
    #for (j in seq(1,2)){
    pi_ij <- data.frame(Omega_Pi_i[j,])
    names(pi_ij) <- predictor_vars
    #print(pi_ij)
    theta_ij1 <- predict(mars_model, pi_ij, type="response")
    #print(theta_ij1)
    theta_ij0 <- 1 - theta_ij1
    df_pi_ij <- match_df(data,pi_ij, on=names(pi_ij))
    n_ij <- nrow(df_pi_ij)
    #print(df_pi_ij[,response_var_id])
    #print(n_ij)
    n_ij1 <- length(which(df_pi_ij[,response_var_id] ==1))
    #print(n_ij1)
    n_ij0 <- n_ij - n_ij1    
    #print(" current log-likelihood")
    #print(log_likelihood)
    #print("update")
    #print (n_ij0*log(theta_ij0) + n_ij1*log(theta_ij1))
    log_likelihood <-  log_likelihood + n_ij1*log(theta_ij1)
    log_likelihood <-  log_likelihood + n_ij0*log(theta_ij0)
    #print("new log-likelihood")
    #print(log_likelihood)
    #print("next")
  }
  
  #print score to file
  print_score(log_likelihood, penalty, predictor_vars, score_file, metrics_file, mars_model)
}

score_null <- function(response_var_id, data, score_file){
  n_ij1 <- length(which(data[,response_var_id] ==1))
  n_ij0 <- nrow(data) - n_ij1   
  
  print(n_ij0)
  print(n_ij1)
  
  theta_ij1 <- n_ij1/nrow(data)
  theta_ij0 <- 1 - theta_ij1
  
  print(theta_ij0)
  print(theta_ij1)
  
  log_likelihood =   n_ij0*log(theta_ij0) + n_ij1*log(theta_ij1) 
  penalty <- compute_penalty_bic(1, nrow(data))
  print_score_null(log_likelihood-penalty, score_file)
}

create_key <- function(parentSet) {
  sorted <- sort(parentSet)
  return(paste(sorted, sep = '', collapse = ''))
}


#To Run

# 1) Load the CSV with the observations
c <- read.csv(args[1], header=FALSE)
solution_file = paste(c(args[2], "_"), collapse = "")
#for ( i in seq(1,ncol(c))){
{
  response_var_id = as.numeric(args[3])+1
  response_var_name = names(c)[response_var_id]
  output_file = paste(c(solution_file, toString(response_var_id)), collapse = "")
  output_file2 = paste(c(output_file, "_metrics"), collapse = "")
  write("parent set, no_of_terms, aic, R^2, CV", output_file2)
  score_null(1, c, output_file)
  if(nrow(unique(c[response_var_id])) == 1) next
  f_base <- as.formula(paste(c(response_var_name, " ~."), collapse = " "))
  print(f_base)
  # 2) Create MARS models with interaction terms and without
  mars1 <- earth(f_base, degree = 10, glm=list(family=binomial), pmethod = "none", data=c)
  mars2 <- earth(f_base, glm=list(family=binomial),pmethod = "none", data=c)
  if(length(mars1$coefficients) + length(mars2$coefficients) == 2) next
  # 3) Obtain all variable names used by the two MARS models
  s <- names(mars1$bx[1,])
  t <- names(mars2$bx[1,])
  s <- c(s,t) 
  s2<- unique(unlist(strsplit(s, "\\*")))
  s2 <- s2[-1]
  # 4) Create a queue of potential parent sets and initialize it with the set from 3)
  q <- queue()
  finished <- hash()
  pushback(q, s2)
  # 5) While the queue is not empty...
  while (length(q) > 0) {
    # 5.1) Extract the first argument, and create a string hash key from it
    parentSet <- pop(q)
    key <- create_key(parentSet)
    # 5.2) If we have not seen this parent set before...
    if (!has.key(key, finished)) {
      finished[key] = 1 
      # 5.2.1) Create a MARS model for it
      predictor = paste(parentSet, collapse = " + ")
      f <- as.formula(paste(c(response_var_name, "~", predictor), collapse=" "))
      print(f)
      mars <- earth(f, degree =10, glm=list(family=binomial), data=c)
      # 5.2.2) Extract the terms used by it
      s <- names(mars$bx[1,])
      s <- unique(unlist(strsplit(s, "\\*")))
      s <- s[-1]
      # 5.2.3) If it is a new set
      if (length(s) > 0) {
        sKey <- create_key(s)
        if (!has.key(sKey, finished)) {
          finished[sKey] = 1
          score_model(mars, 1, s, c, output_file, output_file2)
          usedVariables <- parentSet
          # 5.2.4) Create smaller parent sets and add them to the queue
          for (i in 1:length(usedVariables)) {
            newParentSet <- usedVariables[-i] 
            if (length(newParentSet) > 0) {
              pushback(q, newParentSet)
            } 
          }
        }  
      }
    }
  }
}
print("completed")



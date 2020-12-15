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

print_score <- function(log_likelihood, penalty, predictor_vars, score_file, metrics_file, fda_model){
  tmp <- unique(unlist(strsplit(predictor_vars, "V")))
  parents = tmp[tmp!=""]
  i=as.integer(parents)
  parents = i-1
  score <-  toString(log_likelihood - penalty)
  score <-  paste(score, length(parents))

  parent_str <- paste(parents, collapse = " ")
  score_string <- paste(score, parent_str)
  metrics <- paste(fda_model$fit$gcv, fda_model$fit$rsq, length(fda_model$fit$coefficients), length(parents))
  write(score_string, score_file, append = TRUE)
  write(metrics, metrics_file, append = TRUE)

}

print_score_null <- function(log_likelihood, score_file){
  score <-  toString(log_likelihood) 
  score_string <- paste(score, "0")
  write(score_string, score_file, append = TRUE)
}


score_model <- function(fda_model, response_var_id, predictor_vars, Omega_Pi_i, data, domains, score_file, metrics_file){ 
  #get penalty
  r_Pi_i = 1
  for ( j in length(predictor_vars)) 
    r_Pi_i = r_Pi_i * length(domains[j])
  
  penalty <- compute_penalty_bic(length(domains[response_var_id])*r_Pi_i, nrow(data))
  #compute loglikelihood
  log_likelihood=0
  Omega_Pi_i_padded <- data.frame(matrix(-1, ncol = ncol(c), nrow = nrow(Omega_Pi_i)))
  names(Omega_Pi_i_padded) <- names(c)
 
  for (a in seq(1, length(predictor_vars))) 
  {
    Omega_Pi_i_padded[predictor_vars[a]] = Omega_Pi_i[predictor_vars[a]]
  }
    
  #print(Omega_Pi_i_padded)
  for (j in seq(1,nrow(Omega_Pi_i))){
    #for (j in seq(1,2)){
    pi_ij <- data.frame(Omega_Pi_i_padded[j,])
   
    print(pi_ij)
    theta_ij <- predict(fda_model, pi_ij, type="posterior")
    print(theta_ij)
    #print(Omega_Pi_i)
    pi_ij_t <- data.frame(Omega_Pi_i[j,])
    names(pi_ij_t) <- predictor_vars
    #print(pi_ij_t)
    df_pi_ij <- match_df(data,pi_ij_t, on=names(pi_ij_t))
    #print(df_pi_ij)
    n_ij <- nrow(df_pi_ij)
    log_likelihood=0
    #print(df_pi_ij[,response_var_id])
    #print(n_ij)
    for (k in domains[[response_var_id]])
    {
      n_ijk <- length(which(df_pi_ij[,response_var_id] ==k))
       #print("k=")
       #print(k)
        #print("nijk=")
        #print(n_ijk)
        print("theta=")
        print(theta_ij[k+1])
      log_likelihood <-  log_likelihood + n_ijk*log(theta_ij[k+1])
      print(log_likelihood)
    }
    
    #print(n_ij1)
      
    #print(" current log-likelihood")
    #print(log_likelihood)
    #print("update")
    #print (n_ij0*log(theta_ij0) + n_ij1*log(theta_ij1)
    #print("new log-likelihood")
    print(log_likelihood)
    #print("next")
  }
  
  #print score to file
  if (!is.na(log_likelihood))
    print_score(log_likelihood, penalty, predictor_vars, score_file, metrics_file, fda_model)
}

score_null <- function(domains, response_var_id, data, score_file){

  log_likelihood=0
  for (k in domains[[response_var_id]])
  {
    n_ijk <-length(which(data[,response_var_id] ==k))
    theta_ijk = n_ijk/nrow(data)
    # print("k=")
    # print(k)
    print("nijk=")
    print(n_ijk)
    print("theta=")
    log_likelihood <-  log_likelihood + n_ijk*log(theta_ijk)
    print(log_likelihood)
  }
  
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
domains = list()
for (i in seq(1,ncol(c))) domains[[i]] = unique(c[,i])
names(domains) = names(c)
solution_file = paste(c(args[2], "_"), collapse = "")
#for ( i in seq(1,ncol(c))){
{
  response_var_id = as.numeric(args[3])+1
  response_var_name = names(c)[response_var_id]
  output_file = paste(c(solution_file, toString(response_var_id-1)), collapse = "")
  output_file2 = paste(c(output_file, "_metrics"), collapse = "")
  #write("parent set, no_of_terms, aic, R^2, CV", output_file2)
  score_null(domains, response_var_id, c, output_file)
  if(nrow(unique(c[response_var_id])) == 1) next
  f_base <- as.formula(paste(c(response_var_name, " ~."), collapse = " "))
  print(f_base)
  # 2) Create MARS models with interaction terms and without
  fda1 <- fda(f_base, degree = 10, keep.fitted=TRUE, method=earth, keepxy=TRUE, pmethod="none", data=c)
  fda2 <- fda(f_base, keep.fitted=TRUE, method=earth, keepxy=TRUE, pmethod="none", data=c)
  if(length(coef(fda1)[,1]) + length(coef(fda2)[,1]) == 2) next
  # 3) Obtain all variable names used by the two MARS models
  s <- names(coef(fda1)[,1])
  t <- names(coef(fda2)[,1])
  s <- c(s,t) 
  s2 <- unique(unlist(strsplit(s, "\\*")))
  s3 <- unique(unlist(strsplit(s2, "[()-]")))
  s4 <- grep("V", s3, value=TRUE)
  # 4) Create a queue of potential parent sets and initialize it with the set from 3)
  q <- queue()
  finished <- hash()
  pushback(q, s4)
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
      fda3 <- fda(f, degree = 10, keep.fitted=TRUE, method=earth, keepxy=TRUE, pmethod="backward", data=c)
      # 5.2.2) Extract the terms used by it
      if(fda3$dimension == 0) 
      {
        print("skipping")
        next
      }
      print(coef(fda3))
      #print(s)
      s <- names(coef(fda3)[,1])
      s2 <- unique(unlist(strsplit(s, "\\*")))
      s3 <- unique(unlist(strsplit(s2, "[()-]")))
      s4 <- grep("V", s3, value=TRUE)
      print(s4)
      domains2 = list()
      for (i in s4) 
      {
        domains2[[i]] = domains[[i]]
      }
        
      Omega_Pi = expand.grid(domains2)
      print(Omega_Pi)
      # 5.2.3) If it is a new set
      if (length(s) > 0) {
        sKey <- create_key(s)
        if (!has.key(sKey, finished)) {
          finished[sKey] = 1
          r_i = length(unique(c[response_var_id]))
          r_Pi_i=0
          score_model(fda3, response_var_id, s4, Omega_Pi, c, domains,output_file, output_file2)
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



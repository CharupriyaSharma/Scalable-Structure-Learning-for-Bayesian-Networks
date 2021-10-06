#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
library(dplyr)
library(plyr)
library(dequer)
library(hash)
library(earth, "~/Documents/earth-src")
library(mda, "~/Documents/mda")
#library(Binarize)
library(bnlearn)

compute_penalty_bic <- function(free_parameters, data_size){
  # print("free_parameters")
  # print(free_parameters)
  # print("weight")
  # print(log(data_size)/2)
  # print("penalty")
  # print(free_parameters*log(data_size)/2)
  return (free_parameters*log(data_size)/2) 
}

print_score <- function(log_likelihood, penalty, cKL, max, predictor_vars, score_file, metrics_file, fda_model){
  print("print_score")
  tmp <- unique(unlist(strsplit(predictor_vars, "V")))
  parents = tmp[tmp!=""]
  print("parents")
  print(parents)
  #i=as.integer(parents)
  #parents = i-1
  score <-  toString(log_likelihood - penalty)
  score <-  paste(score, length(parents))

  parent_str <- paste(sort(parents), collapse = " ")
  score_string <- paste(score, parent_str)
  metrics <- paste(score, toString(cKL), length(coef(fda_model)),length(coef(fda_model))/max, fda_model$fit$gcv, fda_model$fit$rsq, length(fda_model$fit$coefficients), length(parents))
  write(score_string, score_file, append = TRUE)
  write(metrics, metrics_file, append = TRUE)

}

print_score_null <- function(log_likelihood, score_file){
  score <-  toString(log_likelihood) 
  score_string <- paste(score, "0")
  write(score_string, score_file, append = TRUE) 
}


score_model <- function(fda_model, response_var_id, predictor_vars, Omega_Pi_i, data, domains, score_file, metrics_file){
  print("score_model")
  #get penalty
  r_Pi_i = 1
  N = nrow(data) 
  #print(predictor_vars)
  for ( j in predictor_vars) {
    #print(domains[[j]])
    r_Pi_i = r_Pi_i * length(domains[[j]])}
  r_i = length(domains[[response_var_id]])
  maxFreeParam = (r_i -1)*r_Pi_i
  # print(domains[response_var_id])
  # print(r_i)
  # print(predictor_vars)
  # print(r_Pi_i)
  #  print(coef(fda_model))
  #  print(length((coef(fda_model))))
  penalty <- compute_penalty_bic(length(coef(fda_model)), N)
  #compute loglikelihood
  log_likelihood=0
  Omega_Pi_i_padded <- data.frame(matrix(-1, ncol = ncol(c), nrow = nrow(Omega_Pi_i)))
  names(Omega_Pi_i_padded) <- names(c)

  #print("Omega_pi_i")
  #print(Omega_Pi_i)
  for (a in seq(1, length(predictor_vars))) 
  {
    #print("a")
    #print(a) 
    Omega_Pi_i_padded[predictor_vars[a]] = Omega_Pi_i[predictor_vars[a]]
  }
  ckl =0
  print(Omega_Pi_i_padded)
  for (j in seq(1,nrow(Omega_Pi_i))){
    #for (j in seq(1,2)){
    pi_ij <- data.frame(Omega_Pi_i_padded[j,])
   
    print("pi_ij")
    print((pi_ij))
    print(coef(fda_model))
    theta_ij <- predict(fda_model, newdata=pi_ij, type="posterior")
    print(theta_ij)
    #print(Omega_Pi_i)
    pi_ij_t <- data.frame(Omega_Pi_i[j,])
    names(pi_ij_t) <- predictor_vars
    #print(pi_ij_t)
    df_pi_ij <- match_df(data,pi_ij_t, on=names(pi_ij_t))
    #print(df_pi_ij)
    n_ij <- nrow(df_pi_ij)
    tempkl =0  
    #print(df_pi_ij[,response_var_id])
    #print(n_ij)
    for (k in domains[[response_var_id]])
    {
      n_ijk <- length(which(df_pi_ij[,response_var_id] ==k))
      #print("k=")
      #print(k)
      #print("nijk=")
      #print(n_ijk)
      #print("nij=")
      #print(n_ij)
      #print("theta=")
      #print(theta_ij[k+1])
      #print(" current log-likelihood")
      #print(log_likelihood)
      cpt_ijk= n_ijk/n_ij
      if(n_ij==0 || n_ijk==0)
        cpt_ijk=1
      tempkl = tempkl +  cpt_ijk*log(cpt_ijk/theta_ij[k])
      if(is.na(tempkl)){
        print("nijk=")
        print(n_ijk)
        print("nij=")
        print(n_ij)
      }
      log_likelihood =  log_likelihood + n_ijk*log(theta_ij[k])
      
      # print(" adding  to current log-likelihood")
      # print(n_ijk*log(theta_ij[k+1]))
    }
    
    ckl = ckl + tempkl*n_ij/N
    #print(n_ij1)
      
    #print(" current log-likelihood")
    #print(log_likelihood)
    #print("update")
    #print (n_ij0*log(theta_ij0) + n_ij1*log(theta_ij1)
    
    #print("next")
  }
  
  #print score to file
  # print("new log-likelihood")
  # print(log_likelihood)
  
  if (!is.na(log_likelihood))
    print_score(log_likelihood, penalty, ckl, maxFreeParam, predictor_vars, score_file, metrics_file, fda_model)}

score_null <- function(domains, response_var_id, data, score_file){

  log_likelihood=0
  for (k in domains[[response_var_id]])
  {
    n_ijk <-length(which(data[,response_var_id] ==k))
    theta_ijk = n_ijk/nrow(data)
    #  print("k=")
    #  print(k)
    # print("nijk=")
    # print(n_ijk)
    # print("theta=")
    log_likelihood <-  log_likelihood + n_ijk*log(theta_ijk)
    #print(log_likelihood)
  }
  
  penalty <- compute_penalty_bic(length(domains[[response_var_id]])-1, nrow(data))
  print_score_null(log_likelihood-penalty, score_file)
}

create_key <- function(parentSet) {
  sorted <- sort(parentSet)
  return(paste(sorted, sep = '', collapse = ''))
}


#To Run

# 1) Load the CSV with the observations
print("Starting!")
c <- read.csv(args[1], header=TRUE)
d = sprintf("start_%s_end", names(c))
names(c) <- d
print(names(c))
response_var_id = as.numeric(args[3])+1
response_var_name = names(c)[response_var_id]
solution_file = paste(c(args[2], "_"), collapse = "")

output_file = paste(c(solution_file, toString(response_var_id-1)), collapse = "")
output_file2 = paste(c(output_file, "_metrics"), collapse = "")

direction = as.numeric(args[4])

domains = list()
if(nrow(unique(c[response_var_id])) == 1) 
{
  for (i in seq(1,ncol(c))) 
    domains[[i]] = unique(c[,i])
  #score_null(domains, response_var_id, c, output_file)
  print("unary variable")
  quit()
  #score null here
}
# for (i in seq(1,ncol(c))) 
# {
# 
#   if (nrow(unique(c[i])) > 1) 
#     c[,i] = (Binarize::binarize.kMeans(c[,i]))@binarizedMeasurements
# }

for (i in names(c))
{
  if (nrow(unique(c[i])) == 1)
  {
    c = select(c, -all_of(i))
  }

}
for (i in seq(1,ncol(c)))
{
  domains[[i]] = unique(c[,i])
  c[,i] = factor(c[,i])
  #print(c[,i])
}
 
print("processed")
names(domains) = names(c)
#write.csv(c, output_file,row.names = FALSE)
#write.csv(c, output_file2,row.names = FALSE)
#for ( i in seq(1,ncol(c))){
{
  
  f_base <- as.formula(paste(c(response_var_name, " ~."), collapse = " "))
  print(f_base)
  # 2) Create MARS models with interaction terms and without
  fda1 <- fda(f_base, degree = 10, keep.fitted=TRUE, method=earth, pmethod="none", keepxy=TRUE, data=c)
  fda2 <- fda(f_base, keep.fitted=TRUE, method=earth, pmethod="none", keepxy=TRUE, data=c)
  print("inital calls over")
  if(length(coef(fda1)[,1]) + length(coef(fda2)[,1]) == 2) next
  # 3) Obtain all variable names used by the two MARS models
  s <- names(coef(fda1)[,1])
  t <- names(coef(fda2)[,1])
  #print(coef(fda1))
  s <- c(s,t) 
  #print(s)
  s2 <- unique(unlist(strsplit(s, "\\*")))
  s3 <- unique(unlist(strsplit(s2, "[()-]")))
  #print(s3)
  # s4 is the set with all candidate parents
  #s4 <- grep("V", s3, value=TRUE)
  s4 <- unique(unlist(strsplit(s3, "\\_end")))
  s5 = s4[ grepl('^start_', s4)]
  s6 <- unique(unlist(strsplit(s5, "start\\_")))
  s6 = s6[s6!=""]
  s6 = s6[s6!="Intercept"]
  s6 = sprintf("start_%s_end", s6)
  allParentCandidates <- s6
  #print("s6")
  #print(s6)
  # 4) Create a queue of potential parent sets and initialize it with the set from 3)
  q <- queue()
  # If the 4th argument is 0, we start with the set containing all parent candidates, otherwise with all singleton parent sets
  if (direction == 0) {
    pushback(q, s6)
  } else {
    for (i in 1:length(s6)) {
      pushback(q, c(s6[i]))  
    }
  }
  finished <- hash()
  print(q)
  # 5) While the queue is not empty...
  while (length(q) > 0) {
    # 5.1) Extract the first argument, and create a string hash key from it
    parentSet <- pop(q)
    #print("parentSet")
    #print(parentSet)
    key <- create_key(parentSet)
    # 5.2) If we have not seen this parent set before...
    if (!has.key(key, finished)) {
      finished[key] = 1 
      # 5.2.1) Create a MARS model for it
      predictor = paste(parentSet, collapse = " + ")
      f <- as.formula(paste(c(response_var_name, "~", predictor), collapse=" "))
      #print("Calling FDA")
      #print(f)
      fda3 <- fda(f, degree = 10, keep.fitted=TRUE, method=earth, keepxy=TRUE , data=c)
      
      #print("Done calling FDA")
      if(fda3$dimension > 1) {
        # 5.2.2) Extract the terms used by it
        print("FDA")
        print(f)
        print(fda3)
        print(fda3$fit)
        # if (fda3$fit$gcv == 0)
        # {
        #   print(coef(fda3))
        # }
        
        s <- names(coef(fda3)[,1])
        #print(s)
        s2 <- unique(unlist(strsplit(s, "\\*")))
        s3 <- unique(unlist(strsplit(s2, "[()-]")))
       
        #s4 <- grep("V", s3, value=TRUE)
        s4 <- unique(unlist(strsplit(s3, "\\_end")))
        s5 = s4[ grepl('^start_', s4)]
        s6 <- unique(unlist(strsplit(s5, "start\\_")))
        s6 = s6[s6!=""]
        s6 = s6[s6!="Intercept"]
        s6 = sprintf("start_%s_end", s6)
        #print("s6")
        #print(s6)
        domains2 = list()
        for (i in s6) 
        {
          domains2[[i]] = domains[[i]]
        }
        Omega_Pi = expand.grid(domains2)        
        if (length(s) > 0) {
          print("model found")
          sKey <- create_key(s)
          if (!has.key(sKey, finished)) {
            finished[sKey] = 1
            r_i = length(unique(c[response_var_id]))
            r_Pi_i=0
            #score_model(fda3, response_var_id, s6, Omega_Pi, c, domains,output_file, output_file2)
          }
        }
      }
      usedVariables <- parentSet
      # 5.2.4) Create next parent sets and add them to the queue
      if (direction == 0) {
        for (i in 1:length(usedVariables)) {
          newParentSet <- usedVariables[-i] 
          if (length(newParentSet) > 0) {
            pushback(q, newParentSet)
          } 
        }            
      } else {
        for (i in 1:length(allParentCandidates)) {
          if (allParentCandidates[i] %in% usedVariables) {
            
          } else { 
            newParentSet <- c(usedVariables, allParentCandidates[i])
            if (length(newParentSet) > 0) {
              pushback(q, newParentSet)
            } 
          }
        }
      }
    }
  }
}




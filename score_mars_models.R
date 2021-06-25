library(plyr)
library(earth)
library(hash)
library(dequer)

compute_penalty_bic <- function(free_parameters, data_size){
  #print(log(data_size)/2*free_parameters)
  return (log10(data_size)/2*free_parameters)
}

print_score <- function(log_likelihood, penalty, predictor_vars, score_file){
  tmp <- unique(unlist(strsplit(predictor_vars, "\\.")))
  parents= tmp[-1]
  score <-  toString(log_likelihood - penalty)
  score <- paste(score, length(parents))
  parent_str <- paste(parents, collapse = " ")
  score_string <- paste(score, parent_str)
  write(score_string, score_file, append = TRUE)
}

print_score_null <- function(log_likelihood, score_file){
  score <-  toString(log_likelihood)
  score_string <- paste(score, "0")
  write(score_string, score_file, append = TRUE)
}


score_model <- function(mars_model, response_var_id, predictor_vars, data, score_file){ 
  if(length(mars_model$coefficients)==1) return
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
  print_score(log_likelihood, penalty, predictor_vars, score_file)
}

score_null <- function(response_var_id, data, score_file){
  n_ij1 <- length(which(data[,response_var_id] ==1))
  n_ij0 <- nrow(data) - n_ij1   
  
  theta_ij1 <- n_ij1/nrow(data)
  theta_ij0 <- 1 - theta_ij1

  log_likelihood =   n_ij0*log10(theta_ij0) + n_ij1*log(theta_ij1)
  log_likelihood =  log_likelihood + n_ij0*log10(theta_ij0)
  
  print_score_null(log_likelihood, score_file)
}

#To Run

# 1) Load the CSV with the observations
c <- read.csv("/Users/charupriyasharma/Documents/CSV/zoo.csv")
# 2) Create MARS models with interaction terms and without
mars1 <- earth(X0 ~., degree = 10, glm=list(family=binomial), data=c)
mars2 <- earth(X0 ~., glm=list(family=binomial), data=c)
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
  key <- paste(parentSet, sep = '', collapse = '')
  # 5.2) If we have not seen this parent set before...
  if (!has.key(key, finished)) {
    # 5.2.1) Score it
    response_var_id = i 
    response_var_name = names(c)[response_var_id]
    predictor = paste(parentSet, collapse = " + ")
    f <- as.formula(paste(c(response_var_name, "~", predictor), collapse=" "))
    mars <- earth(f, degree =10, glm=list(family=binomial), data=c)
    
    s <- names(mars$bx[1,])
    s <- unique(unlist(strsplit(s, "\\*")))
    s <- s[-1]
    
    score_model(mars, 1, s, c, "test.tmp")
    usedVariables <- s
    # 5.2.2) Create smaller parent sets and add them to the queue
    for (i in 1:length(usedVariables)) {
      newParentSet <- usedVariables[-i] 
      if (length(newParentSet) > 0) {
        pushback(q, newParentSet)
      }
    }
    # 5.2.3) Mark the current parent set as processed
    finished[key] = 1
    print(parentSet)
  }
}

score_null(1, c, "test.tmp")
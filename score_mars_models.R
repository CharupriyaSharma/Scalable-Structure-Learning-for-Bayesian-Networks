library(plyr)

compute_penalty_bic <- function(free_parameters, data_size){
  return (log(data_size)/2*free_parameters)
}

print_score <- function(log_likelihood, penalty, predictor_vars, score_file){
  tmp <- unique(unlist(strsplit(predictor_vars, "\\.")))
  parents= tmp[tmp!="X0"]
  score <-  toString(log_likelihood - penalty)
  score <- paste(score, length(parents))
  parent_str <- paste(parents, collapse = " ")
  score_string <- paste(score, parent_str)
  write(score_string, score_file, append = TRUE)
}

score_model <- function(mars_model, response_var, predictor_vars, data, score_file){ 
  #get penalty
  penalty <- compute_penalty_bic(length(mars_model$coefficents), nrow(c))
  
  #compute loglikelihood
  log_likelihood=0
  Omega_Pi_i <- expand.grid(replicate(length(predictor_vars), 0:1, simplify = FALSE))
  names(Omega_Pi_i) <- predictor_vars
  
  for (j in seq(1,2^length(predictor_vars))){
    pi_ij <- data.frame(Omega_Pi_i[j,])
    print(pi_ij)
    theta_ij1 <- predict(mars_model, pi_ij, type="response")
    theta_ij0 <- 1 - theta_ij1

    df_pi_ij <- match_df(data,pi_ij)
    n_ij <- nrow(df_pi_ij)
    n_ij1 <- length(which(df_pi_ij$response_var ==1))
    n_ij0 <- n_ij - n_ij1    
    
    log_likelihood <-  log_likelihood + n_ij1*log(theta_ij1)
    log_likelihood <-  log_likelihood + n_ij0*log(theta_ij0)
    }
  
  #print score to file
  print_score(log_likelihood, penalty, predictor_vars, score_file)
  }
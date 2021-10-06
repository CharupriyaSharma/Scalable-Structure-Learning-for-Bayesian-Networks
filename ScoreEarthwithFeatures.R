#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
library(dplyr)
library(plyr)
library(dequer)
library(hash)
library(earth)

print_score <- function(predictor_vars, score, score_file){
  tmp <- unique(unlist(strsplit(predictor_vars, "V")))
  parents = tmp[tmp!=""]
  i=as.integer(parents)
  parents = i-1
  score <-  toString(score)
  score <-  paste(score, length(parents))
  
  parent_str <- paste(sort(parents), collapse = " ")
  score_string <- paste(score, parent_str)
  write(score_string, score_file, append = TRUE)
  
}

print_score_null <- function(score, score_file){
  score_string <- paste(score, "0")
  write(score_string, score_file, append = TRUE) 
}

score_null_hypothesis <- function(response_var_id, data, score_file){
  nullscore=0
  print(data[response_var_id])
  for (i in nrow(data)){
    print(as.integer(data[response_var_id,i]))
    nullscore = nullscore + as.integer(data[response_var_id,i])*as.integer(data[response_var_id,i])
  }
  print(nullscore)
  print(nullscore/nrow(data))
  print_score_null(nullscore, score_file)
}

create_key <- function(parentSet) {
  sorted <- sort(parentSet)
  return(paste(sorted, sep = '', collapse = ''))
}


#To Run

# 1) Load the CSV with the observations
print("Starting!")
c <- read.csv(args[1], header=TRUE)
response_var_id = as.numeric(args[3])+1
response_var_name = names(c)[response_var_id]
solution_file = paste(c(args[2], "_"), collapse = "")

output_file = paste(c(solution_file, toString(response_var_id-1)), collapse = "")

direction = as.numeric(args[4])
feats <- read.csv(args[5], header=FALSE)
names(feats) = c()
domains = list()
if(nrow(unique(c[response_var_id])) == 1) 
{
  for (i in seq(1,ncol(c))) 
    domains[[i]] = unique(c[,i])
  #score_null(domains, response_var_id, c, output_file)
  #print("unary variable")
  quit()
  #score null here
}
# for (i in seq(1,ncol(c))) 
# {
#   
#   if (nrow(unique(c[i])) > 1) 
#     c[,i] = (Binarize::binarize.kMeans(c[,i]))@binarizedMeasurements
# }
# for (i in seq(1,ncol(c))) 
#   domains[[i]] = unique(c[,i])
# 
# names(domains) = names(c)
for (i in names(c))
{
  
  if (nrow(unique(c[i])) == 1)
  {
    c = select(c, -all_of(i))
  }
  
}
for (i in seq(1,ncol(c)))
{
  c[,i] = sub("^", "s_", c[,i] )
  domains[[i]] = unique(c[,i])
  c[,i] = factor(c[,i])
  #print(c[,i])
}

print("processed")
names(domains) = names(c)

#for ( i in seq(1,ncol(c))){
{
  max=0
  ctr=0
  f_base <- as.formula(paste(c(response_var_name, " ~."), collapse = " "))
  # print(f_base)
  # 2) Create MARS models with feature terms
  s = unlist(feats[response_var_id, 1:5])
  s2 = s
  for (i in seq(1,length(s))){
    s2[i] = names(c[s[i]])
  }
  print(s2)
  quit()
  allParentCandidates <- s2
  #print(s4)
  # 4) Create a queue of potential parent sets and initialize it with the set from 3)
  q <- queue()
  # If the 4th argument is 0, we start with the set containing all parent candidates, otherwise with all singleton parent sets
  if (direction == 0) {
    pushback(q, s2)
  } else {
    for (i in 1:length(s2)) {
      pushback(q, c(s2[i]))  
    }
  }
  finished <- hash()
  # 5) While the queue is not empty...
  while (length(q) > 0) {
    # 5.1) Extract the first argument, and create a string hash key from it
    parentSet <- pop(q)
    #print(parentSet)
    key <- create_key(parentSet)
    # 5.2) If we have not seen this parent set before...
    if (!has.key(key, finished)) {
      finished[key] = 1 
      # 5.2.1) Create a MARS model for it
      predictor = paste(parentSet, collapse = " + ")
      f <- as.formula(paste(c(response_var_name, "~", predictor), collapse=" "))
      #print("Calling FDA")
      e3 <- earth(f, degree = 10, keepxy=TRUE , data=c)
      #print(coef(fda3))
      ctr = ctr + 1
      ###print("Done calling FDA")
      if (ctr == 200)
        print_score_null(max, output_file)
      if(length(e3$coefficients) > 1) {
        print(f)
        #print(e3$gcv)
        if (max < e3$gcv){
          max = e3$gcv
        }
        # 5.2.2) Extract the terms used by it
        s <- names(e3$bx[1,])
        s2 <- unique(unlist(strsplit(s, "\\*")))
        s3 <- unique(unlist(strsplit(s2, "[()-]")))
        s4 <- unique(unlist(strsplit(s3, "s")))
        # s4 is the set with all candidate parents
        s5 <- grep("V", s4, value=TRUE)
        #print(s5)
        if (length(s5) > 0) {
          sKey <- create_key(s)
          if (!has.key(sKey, finished)) {
            finished[sKey] = 1
            print_score(s5, e3$gcv, output_file)
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
  print_score_null(max, output_file)
}




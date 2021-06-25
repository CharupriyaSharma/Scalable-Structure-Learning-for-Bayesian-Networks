#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(dplyr)
library(bnlearn)
library(caret)
library(randomForest)
library(rpart)


bn <- read.bif(args[1])
dat <- read.csv(args[2], header=FALSE)
dat <- dat[-c(1), ]
csvrows = nrow(dat)
#local_vals <- data.frame(matrix(ncol = ncol(dat), nrow = csvrows))
colnames(dat) <- names(bn)
nfeatures = as.integer(args[3])

j=0
for (node in bn)
{
  j= j+1
  #print(dat[,j])
  #print(dimnames(node$prob)[[1]])
  dat[,j] = factor(dat[,j], levels=dimnames(node$prob)[[1]])
  #print(dat[,j])
}
#exit()
#print(dat)
j=0
for (node in bn)
{ 
  print(paste("node id" , as.character(j), sep=" "))
  j= j+1
  print("parents")
  #node_name = names(bn)[j]
  # if (length((node$parents)) == 0 || nrow(unique(dat[j])) == 1)
  # {
  #   # for (i in seq(1, csvrows))
  #   # {
  #   #   local_vals[i,j] = node$prob[dat[i,j]]
  #   # }
  #   next
  # }
  randForest <- randomForest::randomForest(dat[,-j], dat[,j], ntrees=10, importance=TRUE )
  decisiontree <- rpart::rpart(dat[,j]~., data=dat)
  print(node$parents)
  features = tail(names(sort(randForest$importance[,1])),nfeatures)
  print("features : random forest")
  print(features)
  print("features : decision tree")
  print(names(decisiontree$variable.importance))
  print()
  
}

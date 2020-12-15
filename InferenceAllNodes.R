#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

library(dplyr)
library(bnlearn)

bn <- read.bif(args[1])
dat <- read.csv(args[2])
dat <- dat[-c(1), ]
csvrows = nrow(dat)
local_vals <- data.frame(matrix(ncol = ncol(dat), nrow = csvrows))
colnames(local_vals) <- names(bn)
j=0
for (node in bn)
{
  j= j+1
  dat[,j] = factor(dat[,j], levels=dimnames(node$prob)[[1]])
}
j=0
for (node in bn)
{
  j= j+1
  node_name = names(bn)[j]
    if (length((node$parents)) == 0 || nrow(unique(dat[j])) == 1)
    {
      for (i in seq(1, csvrows))
      {
        local_vals[i,j] = node$prob[dat[i,j]]
      }
      next
    }

    #print(node_name)
    parent_vals = select(dat, node$parents)
    pred_node = predict(bn, node_name, parent_vals, prob=TRUE)
    pred_node_prob = attr(pred_node, "prob")
    for (i in seq(1,csvrows))
    {
        local_vals[i,j] = pred_node_prob[,i][dat[i,j]]
    }
}

inference = local_vals %>% mutate(Prod = Reduce(`*`, .))
#print(inference)
writeLines(as.character(inference$Prod), args[3])
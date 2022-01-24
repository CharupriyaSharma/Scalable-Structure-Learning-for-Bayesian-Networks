#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)
library(bnlearn)

n = load(args[1])
dataset  = tools::file_path_sans_ext(basename(args[1]))
all = paste(dataset, ".all", sep="")
dag= paste(dataset, ".dag", sep="")

sink(all)
print(bn)
sink()

for(b in bn)
{
  child = paste(b$node, ":")
  parents = paste(b$parents, collapse=", ")
  parentset = paste(child, parents, sep=" ")
  write(parentset,dag, append = TRUE)
}

#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

rm(list=ls())
library(bnlearn)

genSample <- function(sizes, rep, name, networkame) {
  net <- read.bif(networkame)
  
  for (n in sizes) {
    for (i in 1:1) {
      samp = rbn(net, n)
      write.table(
        t(sapply(samp, nlevels)),
        file = paste0("./",name,"/", n, ".", i, ".csv"),
        row.names = FALSE,
        col.names = TRUE,
        sep = ' ',
        quote = FALSE
      )
      write.table(
        samp,
        file = paste0(name, "/", name,"_", n, ".csv"),
        row.names = FALSE,
        col.names = TRUE,
        append = TRUE,
        sep = ' ',
        quote = FALSE
      )
    }
  }
}


sizes <- c(3)
rep = 1
name = "munin4" #change dataname here"
networkame = paste(name, ".bif", sep="")

if (!dir.exists(paste0("./",name))) {
  dir.create(paste0("./",name))
}
genSample(sizes, rep, name, networkame)

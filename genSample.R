rm(list=ls())
library(bnlearn)

genSample <- function(sizes, rep, name, networkame) {
  net <- read.bif(networkame)
  
  for (n in sizes) {
    for (i in 1:1) {
      samp = rbn(net, n)
      # write.table(
      #   t(sapply(samp, nlevels)),
      #   file = paste0("./",name,"/", n, ".", i, ".csv"),
      #   row.names = FALSE,
      #   col.names = TRUE,
      #   sep = ',',
      #   quote = FALSE
      # )
      write.table(
        samp,
        file = paste0("./",name,"/", n, ".", i, ".csv"),
        row.names = FALSE,
        col.names = TRUE,
        append = TRUE,
        sep = ',',
        quote = FALSE
      )
    }
  }
}


sizes <- c(20, 500, 1000, 2000, 5000)
rep = 1
name ="survey"
networkame = "survey.bif"

if (!dir.exists(paste0("./",name))) {
  dir.create(paste0("./",name))
}
genSample(sizes, rep, name, networkame)

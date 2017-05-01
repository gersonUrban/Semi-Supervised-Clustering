setwd("C:/Users/Gerson/Documents/Mestrado/Algoritmos/DataSets")
db1 = read.csv("dadosGerados_PCM_100.csv")
fac = as.factor(db1[,3])
plot(db1)
plot(db1[,1:2])
col.list <- c("seagreen","blue","green","black")
palette(col.list)
plot(db1[,1:2], pch = c(15, 16, 17)[fac], col=fac)

setwd("C:/Users/Gerson/Documents/GitHub/Semi-Supervised-Clustering")

source("KFCM.R")
kfcm = kfcm(db1[,1:2],2)
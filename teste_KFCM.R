rm(list = ls())
setwd("C:/Users/Gerson/Documents/AlgoritmosAntigos/DataSets")
db = read.csv("dadosGerados_PCM_100.csv")
fac = as.factor(db1[,3])
plot(db)
plot(db[,1:2])
col.list <- c("seagreen","blue","green","black")
palette(col.list)
plot(db[,1:2], pch = c(15, 16, 17)[fac], col=fac)

setwd("C:/Users/Gerson/Documents/GitHub/Semi-Supervised-Clustering")

kernel_f = "RBF"
V = NULL
th = 0.01
iter.max = 200
C = 2
db = db[,1:2]
source("KFCM.R")

kfcm_result = kfcm(db1[,1:2],C, kernel_f = "RBF", th = 0.001)

kfcm_result = res
#Plot apenas com os valores classificados
family = as.factor(kfcm_result$cluster)
tam = length(table(family))
palette(c("blue","red"))
plot(db[,1:2], pch = c(15,16)[family], col=family)


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
C = 2
kfcm_result = kfcm(db1[,1:2],C, kernel_f = "RBF")

kfcm_result

#Encontrando a classificacao
classificados = db
classificados[,dim+1] = 0
size = vector(length = C)

#Fiz na "mao" arrumar depois
for(i in 1:N){
  for(j in 1:C){
    if(U[i,j] == max(U[i,])){
      classificados[i,(dim+1)] = j
      size[j] = size[j] + 1
    }
  }
}


#Plot apenas com os valores classificados
family = as.factor(classificados[,3])
tam = length(table(family))
palette(CORES2[[2]])
plot(db, pch = c(15,16)[family], col=family)

kernel_f = "RBF"
e = kernel_func(db[1,])

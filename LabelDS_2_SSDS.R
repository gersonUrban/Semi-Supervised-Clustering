library(dplyr)
setwd("C:/Users/Gerson/Documents/AlgoritmosAntigos/DataSets")
db1 = read.csv("dadosGerados_PCM_100.csv")

#dimensão da base de dados e seu tamanho
dim = ncol(db1)-1
N = nrow(db1)  

#Renomeando as colunas
colnames(db1) = c(1:dim,"class")

#Encontrando apenas as instancias rotuladas
b = db1$class
aux = b == 3
b2 = b
b2[aux] = 0

#transformando o DS em SS
db1[,3] = b2
db = db1[,3] > 0
db = db1[db,]
db3 = db
N = nrow(db)
dados = sample(1:N,N*0.1)
db3[dados,3] = 0
write.csv(db3,"Circular_443_SS90.csv", row.names = F)
plot(db3[,1:2])

#Normalizando
minimo <- apply(db3[,1:dim],2, min)
maximo <- apply(db3[,1:dim],2, max)
teste = sapply(db3[,1:dim],function(x){
  return((x - minimo)/(maximo-minimo))
})
plot(teste)
db3 = cbind(teste,db3[,dim+1])
write.csv(db3,"Circular_443_SS90_N.csv", row.names = F)
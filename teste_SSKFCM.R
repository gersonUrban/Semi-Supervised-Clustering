setwd("C:/Users/Gerson/Documents/Mestrado/Algoritmos/DataSets")
db = read.csv("Circular_443_SS10.csv")

fac = as.factor(db[,3])
plot(db)
plot(db[,1:2])
col.list <- c("gray","blue","red","green")
palette(col.list)
plot(db[,1:2], pch = c(3, 15, 16)[fac], col=fac)

setwd("C:/Users/Gerson/Documents/GitHub/Semi-Supervised-Clustering")

source("SSKFCM.R")
C = 2
sskfcm_result = sskfcm(db1[,1:2],C, kernel_f = "RBF")

#Plot apenas com os valores classificados
family = as.factor(sskfcm_result$cluster)
tam = length(table(family))
palette(c("blue","red"))
plot(db, pch = c(15,16)[family], col=family)


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
dados = sample(1:N,N*0.9)
db3[dados,3] = 0
write.csv(db3,"Circular_443_SS10.csv", row.names = F)





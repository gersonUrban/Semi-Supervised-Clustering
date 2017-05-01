setwd("C:/Users/Gerson/Documents/Mestrado/Algoritmos/DataSets")
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
dados = sample(1:N,N*0.9)
db3[dados,3] = 0
write.csv(db3,"Circular_443_SS10.csv", row.names = F)
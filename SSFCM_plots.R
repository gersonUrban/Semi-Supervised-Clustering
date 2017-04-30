#ADICIONAR:
# KFCM
#SSFCM_MACARIO
#WSSFCM_KONG
#SSFCM_LI
#SSKFCM


setwd("C:/Users/Gerson/Documents")
library(ggplot2) 
library(caret)
library(e1071)
library(gridExtra)
source("SSFCM_Pedrycz.R")
source("SSFCM_Pedrycz_Fuzzy.R")
source("FCM_function.R")
#Lendo DB
#db = read.csv("AlgoritmosAntigos/DataSets/dadosGerados_FCM2_SS.csv")
#db = db[,2:4]
#db2 = read.csv("AlgoritmosAntigos/DataSets/dadosGerados_FCM2.csv")

#db = read.csv("AlgoritmosAntigos/DataSets/dadosGeradosFCM2_SS_90.csv")
#db = db[,2:4]
#db2 = read.csv("AlgoritmosAntigos/DataSets/dadosGerados_FCM2.csv")

db2 = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/BG_10k/BarsGaussAN0_10000.csv")
db2 = db2[4000:5000,]
db = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/BG_10k/BarsGaussAN0_1000_SS.csv")
db = db[,2:4]

plot(db2[,1:2])

#db = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/MGK3D2_10k/MixGaussiansK3D2N0_10000_SS.csv")
#db = db[,2:4]
#db2 = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/MGK3D2_10k/MixGaussiansK3D2N0_10000.csv")
#db2 = db2[1:1000,]

#db = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/MGK3D2_10k/MixGaussiansK3D2N0_10000_SS.csv")
#db = db[,2:4]
#db2 = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/MGK3D2_10k/MixGaussiansK3D2N0_10000.csv")
#db2 = db2[1:1000,]

#db = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/MGK3D3_10k/MixGaussiansK3D3N0_1000_SS.csv")
#db = db[,2:4]
#db2 = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/MGK3D3_10k/MixGaussiansK3D3N0_10000.csv")
#db2 = db2[1:1000,]

#db = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/MGK4D2_10k/MixGaussiansK4D2N0_1000_SS.csv")
#db = db[,2:4]
#db2 = read.csv("OnDemand-master/DS_Datasets/Synthetic/Stationary/MGK4D2_10k/MixGaussiansK4D2N0_10000.csv")
#db2 = db2[1:1000,]



#Quantidade de grupos (Na verdade k deve ser igual a quantidade de classes)
dim = ncol(db)-1
N = nrow(db)
#Renomeando as colunas
colnames(db) = c(1:dim,"class")

#Encontrando a qtd de classes (ARRUMAR MELHOR DEPOIS)
b = db$class
aux = b > 0
b2 = b
b2[aux] = 1

#Encontrando a quantidade de classes
rotulados = as.factor(db$class[aux])
classes = as.numeric(names(table(rotulados)))
nclasses = length(classes)


#threshold
th = 0.001
#Valor alpha do algoritmo
alpha = 0.99

########################CRIANDO PALETAS DE CORES######################################
CORES = NULL
CORES[[2]] = c("gray","red")
CORES[[3]] = c("gray","red","blue")
CORES[[4]] = c("gray","red","blue","green")
CORES[[5]] = c("gray","red","blue","green","yellow")
CORES[[1]] = "gray"

CORES2 = NULL
CORES2[[2]] = c("red","blue")
CORES2[[3]] = c("red","blue","green")
CORES2[[4]] = c("red","blue","green","yellow")
CORES2[[1]] = "red"

CORES3 = NULL
CORES3[[2]] = c("red","black")
CORES3[[3]] = c("red","blue","black")
CORES3[[4]] = c("red","blue","green","black")
CORES3[[5]] = c("red","blue","green","yellow","black")
CORES3[[1]] = "black"

##############################ABRINDO O PDF###########################################
#pdf("ComparacaoResultados_10_1_90.PDF")
##############################Apenas plots para demonstrar############################
family = as.factor(db[,dim+1])
tam = length(table(family))
palette(CORES[[tam]])
#plot(db[,1:2], pch = c(3, 15:(15+tam-2))[family], col=family, xlim=c(15,125), ylim = c(15,125))
plot(db[,1:2], pch = c(3, 15:(15+tam-2))[family], col=family, main = "Distribuição dos dados")

#Plotando o supervisionado
family = as.factor(db2[,dim+1])
tam = length(table(family))
palette(CORES[[tam]])
#plot(db[,1:2], pch = c(3, 15:(15+tam-2))[family], col=family, xlim=c(15,125), ylim = c(15,125))
plot(db2[,1:2], pch = c(3, 15:(15+tam-2))[family], col=family, main = "Distribuição dos dados - Supervisionado")


dbc = db[db$class >= 1,]
family = as.factor(dbc[,dim+1])
tam = length(table(family))
palette(CORES2[[tam]])
plot(dbc[,1:2], pch = c(15:(15+tam-1))[family], col=family, main = "Distribuição dos dados rotulados")
#plot(db2[,1:2], pch = c(15, 16, 17)[family], col=family, xlim=c(15,125), ylim = c(15,125))

###########################################################################################

################OBTENDO CLUSTERIZAÇÕES##################
############teste
resultado = read.csv("BarsGaussAN0_200_MiC.csv")
resultado = resultado[,2:7]
V = matrix(ncol = 2, nrow = 4)
V = teste2$centers - 10
#V[1,] = c(-4,2)
#V[2,] = c(1,4)
#V[3,] = c(0,4)
#V[4,] = c(4,-2)
ssfcm_mahalanobis_fuzzy = ssfcm_pedrycz_fuzzy(resultado[,1:2],V,resultado[,3:ncol(resultado)],alpha,"mahalanobis",th,200)
ssfcm_euclidian_fuzzy = ssfcm_pedrycz_fuzzy(resultado[,1:2],V,resultado[,3:ncol(resultado)],alpha,"euclidian",th,200)
ssfcm_mahalanobis = ssfcm_euclidian_fuzzy
ssfcm_mahalanobis = ssfcm_mahalanobis_fuzzy
#Plotando resultado
classificacao = vector(length = nrow(resultado))
res = resultado[,3:ncol(resultado)]
for(i in 1:nrow(res)){
  for(j in 1:3){
    if(res[i,j] > 0){
      res[i,j] = j
    }
    else{
      res[i,j] = 0
    }
  }
}
for(i in 1:200){
  classificacao[i] = sum(res[i,])
}
family = as.factor(classificacao)
tam = length(table(family))
palette(CORES[[tam]])
#plot(db[,1:2], pch = c(3, 15:(15+tam-2))[family], col=family, xlim=c(15,125), ylim = c(15,125))
plot(resultado[,1:2], pch = c(3, 15:(15+tam-2))[family], col=family, main = "Distribuição dos dados")
db = resultado[,1:2]
db[,3] = ssfcm_euclidian_fuzzy$cluster
family = as.factor(db[,3])
tam = length(table(family))
palette(CORES2[[tam]])
plot(db[,1:2], pch = c(15:(15+tam-1))[family], col=family, main = a[[i]]$call )
###################

ssfcm_mahalanobis = ssfcm_pedrycz(db,NULL,NULL,alpha,"mahalanobis",th,200)
ssfcm_euclidian = ssfcm_pedrycz(db,NULL,NULL,alpha,"euclidian",0.001,200)
fcm_euclidian = cmeans(db[,1:dim], inicializeV(dim,nclasses,db), iter.max=100, verbose=FALSE, dist="euclidean", method="cmeans", m=2, rate.par = NULL)
fcm_mahalanobis = fcm_function(db[,1:dim],inicializeV(dim,nclasses,db),"mahalanobis",0.001,200)
#fcm_euclidian2 = fcm_function(db[,1:dim],inicializeV(dim,nclasses,db),"euclidian",0.001,200)

##############FAZENDO PLOTS DOS RESULTADOS###########################################
qtdAlgs = 4
a = NULL
ssfcm_mahalanobis$call = "SSFCM Mahalanobis"
a[[1]] = ssfcm_mahalanobis
ssfcm_euclidian$call = "SSFCM Euclidian"
a[[2]] = ssfcm_euclidian
fcm_mahalanobis$call = "FCM Mahalanobis"
a[[3]] = fcm_mahalanobis
fcm_euclidian$call = "FCM Euclidian"
a[[4]] = fcm_euclidian
#ssfcm_mahalanobis$call = "FCM Euclidian2"
#a[[5]] = fcm_euclidian2

for(i in 1:qtdAlgs){
  #Plot apenas com os valores classificados
  family = as.factor(a[[i]]$cluster)
  tam = length(table(family))
  palette(CORES2[[tam]])
  plot(db[,1:2], pch = c(15:(15+tam-1))[family], col=family, main = a[[i]]$call )
  
  #Plot dos resultados com os valores dos centroides
  V = cbind(a[[i]]$centers,length(a[[i]]$size)+1)
  colnames(V) = colnames(db)
  classificados = cbind(db[,1:dim],a[[i]]$cluster)
  colnames(classificados) = colnames(V)
  classificados = rbind(classificados,V)
  family = as.factor(classificados[,dim+1])
  tam = length(table(family))
  palette(CORES3[[tam]])
  plot(classificados[,1:2], pch = c(15:(15+tam-2), 3)[family], col=family, main = a[[i]]$call)
}

###################Fazendo matrix de confusão para avaliar resultados#####################
mcFCM_euclidian = confusionMatrix(as.factor(fcm_euclidian$cluster), as.factor(db2[,dim+1]))
#mcFCM_euclidian2 = confusionMatrix(as.factor(fcm_euclidian2$cluster), as.factor(db2[,dim+1]))
mcFCM_mahalanobis = confusionMatrix(as.factor(fcm_mahalanobis$cluster), as.factor(db2[,dim+1]))
mcSSFCM_euclidian = confusionMatrix(as.factor(ssfcm_euclidian$cluster), as.factor(db2[,dim+1]))
mcSSFCM_mahalanobis = confusionMatrix(as.factor(ssfcm_mahalanobis$cluster), as.factor(db2[,dim+1]))


##########################Adicionando os resultados a uma estrutura######################
resultados = cbind("SSFCM Mahalanobis",mcSSFCM_mahalanobis$overall["Accuracy"])
resultados = rbind(resultados,cbind("SSFCM Euclidian",mcSSFCM_euclidian$overall["Accuracy"]))
resultados = rbind(resultados,cbind("FCM Mahalanobis",mcFCM_mahalanobis$overall["Accuracy"]))
resultados = rbind(resultados,cbind("FCM Euclidian",mcFCM_euclidian$overall["Accuracy"]))
#resultados = rbind(resultados,cbind("FCM2 Euclidian",mcFCM_euclidian2$overall["Accuracy"]))

#Renomeando linhas e colunas
row.names(resultados) = c(1:qtdAlgs)
colnames(resultados) = c("Algoritmo","Accuracy")
#plot.new()
frame()
grid.table(resultados)
dev.off()

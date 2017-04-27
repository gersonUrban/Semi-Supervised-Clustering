ssfcm_pedrycz_fuzzy <- function(db, V = NULL, fuzzy.matrix = NULL , alpha = 0.99, distance = "mahalanobis", th = 0.01, iter.max = 200){
  
  #dimensão da base de dados e seu tamanho
  dim = ncol(db)
  N = nrow(db)  
  
  #Renomeando as colunas
  colnames(db) = c(1:dim)
  
  b = vector(length = N)
  b[1:N] = 0
  
  nclasses = nrow(V)
  #criando os prototipos caso não 
  #if(is.null(V)){
  #  V = inicializeV(dim,nclasses,db)
  #}
  
  #Encontrando os classificados para fazer a matriz de covariacia para a distancia
  #lista = NULL
  #for(i in 1:nclasses){
  #  t = db[,dim+1] == i
  #  lista[[i]] = db[t,1:dim]
  #}
  #for(i in 1:nclasses){
  #  lista[[i]] = rbind(lista[[i]],V[i,1:dim])
  #}
  
  #Criando uma matrix de pertinencia vazia
  U = matrix(ncol = nclasses, nrow = N, 0)
  
  #Inicializando a matriz de pertinencia U
  U = update.partition_matrix_f(db,V,alpha,fuzzy.matrix,distance)
  
  erro = 1 + th
  t = 0
  #Inicializando o algoritmo
  while(erro > th){
    #for(t in 1:iter.max){
    t = t + 1
    V = update.prototypes_f(U,db)
    
    U2 = update.partition_matrix_f(db,V,alpha,fuzzy.matrix,distance)
    
    #Verificando a condicao de parada(diferenca entre pesos menor que threshold)
    erro = 0
    for(i in 1:N){
      sum = 0
      for(j in 1:nclasses){
        sum = sum + abs(U2[i,j] - U[i,j])
      }
      erro = erro + sum
    }
    
    U = U2
    cat("\n iteracao: ",t)
    cat("\n Diferenca: ",erro)
    #cat("\n Th: ",th)
  }
  
  #Encontrando a classificacao
  classificados = db
  classificados[,dim+1] = 0
  size = vector(length = nclasses)
  
  #Fiz na "mao" arrumar depois
  for(i in 1:N){
    for(j in 1:nclasses){
      if(U[i,j] == max(U[i,])){
        classificados[i,(dim+1)] = j
        size[j] = size[j] + 1
      }
    }
  }
  
  #Preparando o retorno
  res = NULL
  res$centers = V
  res$size = size
  res$cluster = classificados[,dim+1]
  res$membership = U
  res$iter = t
  res$withinerror = "TODO"
  res$call = "TODO"
  
  return(res)
}

update.prototypes_f <- function(U,db){
  #A equação foi separada em duas etapas C = A/B
  
  #Inicializando valores
  N = nrow(db)
  k = ncol(U)
  dim = ncol(db)
  
  #Primeira parte
  A = matrix(ncol = dim, nrow = k)
  for(i in 1:k){
    for(j in 1:dim){
      A[i,j] = sum((U[,i]^2) * db[,j])
    }
  }
  
  #Segunda Parte
  B = vector(length = k)
  for(i in 1:k){
    B[i] = sum(U[,i]^2)
  }
  C = A/B
  return(C)
}

#Função que retorna a matriz de pertinencia utilizando distancia de mahanalobis
update.partition_matrix_f <- function(db, V, alpha,FMatrix, distance){
  #Necessario utilizar a função dist.mahalanobis
  #Encontrando valores principais
  k = nrow(V)
  dim = ncol(db)
  N = nrow(db)
  
  b = vector(length = N)
  b[1:N] = 0
  
  #Renomeando as colunas do DB
  colnames(db) = c(1:dim)
  
  #To simplificate, The FUZZY ISODATA was separeted in 4 parts
  #Perhaps the update matrix partition is U = A * ((B/C)+D)
  #First Part
  A = 1 / (1+alpha)
  
  #Second Part
  B = vector(length = N)
  for(i in 1:N){
    B[i] = 1 + (alpha * (1 - (b[i]*sum(FMatrix[i,]))))
  }
  
  #Third Part
  if(distance == "mahalanobis"){
    maha = dist.mahalanobis_f(db,V,U)#passo a lista
  }
  else if(distance == "euclidian"){
    maha = dist.euclidian_f(db,V)
  }
  else{
    maha = dist.mahalanobis_f(db,V,U)
  }
  C = matrix(nrow = N, ncol = k)
  
  for(i in 1:N){
    for(j in 1:k){
      C[i,j] = sum(maha[i,j]/maha[i,])
    }
  }
  
  #Fourth Part
  D = matrix(nrow = N, ncol = k)
  for(i in 1:N){
    for(j in 1:k){
      D[i,j] = alpha * FMatrix[i,j] * b[i]
    }
  }
  
  #Finalizing equation 
  U = A* ((B/C)+D)
  return(U)
}


#Função que retorna a distancia de mahanaloobis de todos os pontos para todos os clusters
dist.mahalanobis_f <- function(db, V, U){
  rho = 1
  N = nrow(db)
  P = array(1, dim=c(ncol(V),ncol(V),nrow(V)))
  for(j in 1:nrow(V)){
    sum = 0
    for(i in 1:N){
      a = as.matrix(db[i,]-V[j,])
      sum = sum + (U[i,j]^2)*(a[1,]) %*% as.matrix(t(a[1,]))
    }
    sum2 = sum(U[,j]^2)
    P[,,j] = sum/sum2
  }
  #P = as.matrix(P)
  M = array(1, dim=c(ncol(V),ncol(V),nrow(V)))
  Minv = array(1, dim=c(ncol(V),ncol(V),nrow(V)))
  for(i in 1:nrow(V)){
    M[,,i] = sqrt(1/(rho*det(P[,,i]))) * P[,,i]
    Minv[,,i] = solve(M[,,i]) %*% M[,,i]
  }
  
  d2_mahalanobis = matrix(nrow = N, ncol = nrow(V), 0)
  for(i in 1:nrow(V)){
    for(k in 1:N){
      a = as.matrix(db[k,] - V[i,])
      d2_mahalanobis[k,i] = t(a[1,]) %*% Minv[,,i] %*% a[1,]
    }
  }
  return(d2_mahalanobis)
  #Encontrando matriz inversa de M
  
}
# dist.mahalanobis_f <- function(db, V){
#   
#   average = colMeans(V)
#   variance = var(V)
#   #variance = var(db)##############################
#   N = nrow(db)
#   k = nrow(V)
#   dim = ncol(db)
#   
#   d2_mahalanobis = matrix(nrow = N, ncol = k, 0)
#   
#   for(i in 1:N){
#     for(j in 1:k){
#       d2_mahalanobis[i,j] = mahalanobis(db[i,1:dim], V[j,], variance)
#       #d2_mahalanobis[i,j] = mahalanobis(db[i,1:dim], average[j], variance[[j]])
#     }
#   }
#   return(d2_mahalanobis)
# }

#Função que retorna a distancia euclidiana de todos os pontos para todos os clusters
#Feito na mão, depois arrumo
dist.euclidian_f <- function(db, V){
  N = nrow(db)
  k = nrow(V)
  dim = ncol(db)
  colnames(V) = colnames(db[,1:dim])
  distancia = rbind(db[,1:dim],V)
  distancia = as.matrix(dist(distancia))
  lista = NULL
  for(i in 1:k){
    lista[[i]] = distancia[N+i,]
  }
  d2_euclidian = matrix(nrow = N, ncol = k)
  for(i in 1:N){
    for(j in 1:k){
      d2_euclidian[i,j] = lista[[j]][[i]]
    }
  }
  return(d2_euclidian)
}

inicializeV_f <- function(dim,nclasses,db){
  ##Encontrando os chutes iniciais do centroide 
  #Pois eu mesclei cluster com classe, e ele estava procurando a mais perto, 
  #portanto resolvi fazer a media dos pontos classificados como chute inicial do centroides (ARRUMAR DEPOIS)
  V = matrix(ncol = dim, nrow = nclasses, sample(0:130, size=nclasses*dim, replace=TRUE))
  
  #Fazendo na mão, depois arrumo
  lista = NULL
  for(i in 1:nclasses){
    t = db[,dim+1] == i
    lista[[i]] = db[t,1:dim]
  }
  V = matrix(ncol = dim, nrow = nclasses)
  for(i in 1:nclasses){
    V[i,] = colMeans(lista[[i]])
  }
  return(V)
}
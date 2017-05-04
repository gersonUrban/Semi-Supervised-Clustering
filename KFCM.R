#kfcm <- function(db,C, V = NULL, m = 2, kernel_f = NULL, th = 0.01, iter.max = 200){
  
 source("KFCM_utils.R")
  kernel_f <<- kernel_f
  
  #Verificando se tem o valor de C ou V
  if(is.null(V) && is.null(C)){
    cat("C e V NULL")
    return(NULL)
  }
  
  if(is.null(kernel_f)){
    kernel_f = "polynomial"
  }
  
  dim = ncol(db)
  N = nrow(db)
  colnames(db) = c(1:dim)
  
  db = as.data.frame(db)
  colnames(db)<- c(1:dim)
  if(is.null(V)){
    #Caso tenha sido entrado apenas o valor de C
    min = min(db[,1:dim])
    max = max(db[,1:dim])
    V = initialize_V(C, dim, max, min)
    V = as.data.frame(V)
    colnames(V) <- c(1:dim)
  }else{
    #Caso tenha sido entrado apenas o valor de V
    C = nrow(V)
  }
  
  
  #Criando uma matrix de pertinencia vazia
  U = matrix(ncol = C, nrow = N, 0)
  
  #Inicializando a matriz de pertinencia U
  U = initialize_U(db,V)
  
  erro = 1 + th
  t = 0
  while((erro > th)&&(t < iter.max)){
    t = t + 1
    cat("\n iteracao: ",t)
    
    k_m = update_kernel_matrix(U,db)
    U2 = update_U(db,k_m)
    
    #Verificando a condicao de parada(diferenca entre pesos menor que threshold)
    erro = 0
    for(i in 1:N){
      sum = 0
      for(j in 1:C){
        sum = sum + abs(U2[i,j] - U[i,j])
      }
      erro = erro + sum
    }
    
    cat(" , Diferenca: ",erro)
    
    U = U2
    
  }
  
  # #Encontrando a "classificacao"
  # classificados = db
  # classificados[,dim] = 0
  # size = vector(length = C)
  # 
  # #Fiz na "mao" arrumar depois
  # for(i in 1:N){
  #   for(j in 1:C){
  #     if(U[i,j] == max(U[i,])){
  #       classificados[i,dim] = j
  #       size[j] = size[j] + 1
  #     }
  #   }
  # }
  
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
  
  #Preparando o retorno
  res = NULL
  #res$centers = V #Pensar em como arrumar o centroide
  res$centers = k_m$K_V_V
  res$K_X_V = k_m$K_X_V
  res$size = size
  res$cluster = classificados[,dim+1]
  res$membership = U
  res$iter = t
  res$withinerror = "TODO"
  res$call = "TODO"
 # return(res)
#}

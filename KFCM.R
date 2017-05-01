kfcm <- function(db,C, V = NULL, m = 2, kernel_f = NULL, th = 0.01){
  
  #Verificando se tem o valor de C ou V
  if(is.null(V) && is.null(C)){
    cat("C e V NULL")
    return(NULL)
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
  }
  else{
    #Caso tenha sido entrado apenas o valor de V
    C = nrow(V)
  }
  
  
  #Criando uma matrix de pertinencia vazia
  U = matrix(ncol = C, nrow = N, 0)
  
  #Inicializando a matriz de pertinencia U
  U = initialize_U(db,V)
  
  erro = 1 + th
  t = 0
  while(erro > th){
    t = t + 1
    
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
    
    U = U2
    
  }
  
  #Encontrando a "classificacao"
  classificados = db
  classificados[,dim] = 0
  size = vector(length = C)
  
  #Fiz na "mao" arrumar depois
  for(i in 1:N){
    for(j in 1:C){
      if(U[i,j] == max(U[i,])){
        classificados[i,dim] = j
        size[j] = size[j] + 1
      }
    }
  }
  
  #Preparando o retorno
  res = NULL
  #res$centers = V #Pensar em como arrumar o centroide
  res$size = size
  res$cluster = classificados[,dim+1]
  res$membership = U
  res$iter = t
  res$withinerror = "TODO"
  res$call = "TODO"
}

initialize_V <- function(C, dim, max, min){
  #Criando uma matrix de pertinencia vazia
  V = matrix(ncol = dim, nrow = C, sample(min:max,C*dim))
  return(V)
}

update_kernel_matrix <- function(U, db){
  
  dim = ncol(db)
  C = ncol(U)
  N = nrow(U)
  
  kernel_matrix = NULL
  kernel_matrix$K_X_V = matrix(ncol = C, nrow = N, 0)
  kernel_matrix$K_V_V = matrix(nrow = C,0)
  
  #Arrumar depois, tirar os la�os e o 2(no lugar do m)
  # for(i in 1:N){
  #   for(j in 1:C){
  #     for(k in 1:N){
  #       sum = (U[k,j])^2 * kernel_func(db[k,],db[i,])
  #       for(l in 1:N){
  #         sum2 = (U[k,j])^2 * (U[l,j])^2 * kernel_func(db[k,],db[l,])
  #       }
  #     }
  #     kernel_matrix$K_X_V[i,j] = sum/sum((U[,j])^2)
  #     kernel_matrix$K_V_V[j,] = sum2/(sum((U[,j])^2))^2
  #   }
  # }
  
  
  for(j in 1:C){
    sum3 = 0
    for(i in 1:N){
      sum = 0
      sum2 = 0
      for(k in 1:N){
        sum = sum + ((U[k,j])^2 * kernel_func(db[k,],db[i,]))
        sum2 = sum2 + ((U[i,j])^2 * (U[k,j])^2 * kernel_func(db[i,],db[k,]))
        # for(l in 1:N){
        #   sum2 = (U[k,j])^2 * (U[l,j])^2 * kernel_func(db[k,],db[l,])
        # }
      }
      kernel_matrix$K_X_V[i,j] = sum/sum((U[,j])^2)
      sum3 = sum3 + sum2
    }
    cat("\n j = ",j)
    kernel_matrix$K_V_V[j,] = sum2/(sum((U[,j])^2))^2
  }
  
  
  
  return(kernel_matrix)
}


initialize_U <- function(db, V){
  #Talvez seja melhor dar um source
  C = nrow(V)
  dim = ncol(db)
  N = nrow(db)
  
  #inicializa =  A/B onde B = sum(C)
  
  #Encontrado A
  A = matrix(nrow = N, ncol = C)
  for(i in 1:N){
    for(j in 1:C){
      #elevei a 2 e tirei raiz para funcionar
      A[i,j] = 1/ sqrt( ((kernel_func(db[i,])- (2*kernel_func(db[i,],V[j,]))) + (kernel_func(V[j,])))^2 )
    }
  }
  
  B = matrix(nrow = N)
  
  for(i in 1:N){
    B[i,] = sum(A[i,])
  }
  
  for(i in 1:N){
    U[i,1:dim] = A[i,1:dim]/B[i]
  }
  
  return(U)
}

update_U <- function(db, kernel_matrix){
  C = ncol(kernel_matrix$K_X_V)
  dim = ncol(db)
  N = nrow(db)
  
  #inicializa =  A/B onde B = sum(C)
  
  #Encontrado A
  A = matrix(nrow = N, ncol = C)
  for(i in 1:N){
    for(j in 1:C){
      A[i,j] = 1/sqrt( ((kernel_func(db[i,])) - (2*kernel_matrix$K_X_V[i,j]) + (kernel_matrix$K_V_V[j,]))^2)
    }
  }
  
  B = matrix(nrow = N)
  
  for(i in 1:N){
    B[i,] = sum(A[i,])
  }
  
  for(i in 1:N){
    U[i,1:dim] = A[i,1:dim]/B[i]
  }
  return(U)
}


kernel_func <- function(a,b = NULL){
  a = as.matrix(a)
  a = a[1,]
  if(is.null(b)){
    phi = (a %*% a)^2
  }
  else{
    b = as.matrix(b)
    b = b[1,]
    phi = (a %*% b)^2
  }
  return(phi)
}

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
  
  #Arrumar depois, tirar os laços e o 2(no lugar do m)
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
  
  U = as.data.frame(matrix(ncol = C, nrow = N))
  
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
  U = as.data.frame(matrix(ncol = C, nrow = N))
  for(i in 1:N){
    U[i,1:dim] = A[i,1:dim]/B[i]
  }
  return(U)
}


kernel_func <- function(a,b = NULL){#, kernel_f = "polynomial"){
  if(kernel_f == "polynomial"){
    a = as.matrix(a)
    a = a[1,]
    if(is.null(b)){
      phi = ((a %*% a)+1)^2
    }
    else{
      b = as.matrix(b)
      b = b[1,]
      phi = ((a %*% b)+1)^2
    }
  }
  if(kernel_f == "RBF"){
    #Gaussian Kernel
    if(is.null(b)){
      d = dist(rbind(a,a))
    }
    else{
      d = dist(rbind(a,b))
    }
    phi = exp(-(d^2)/2)
  }
  if(kernel_f == "tanh"){
    a = as.matrix(a)
    a = a[1,]
    if(is.null(b)){
      phi = tanh((a %*% a)+1)
    }
    else{
      b = as.matrix(b)
      b = b[1,]
      phi = tanh((a %*% b)+1)
    }
  }
  return(phi)
}
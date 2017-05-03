

# break_db <- function(db){
#   return(db2)
# }
atualiza_V <- function(db_L, U_L){
  N_L = nrow(db_L)
  dim = ncol(db_L) - 1
  C = ncol(U_L)
  
  #Criando V como definido na eq 12
  V = as.data.frame(matrix(ncol = dim, nrow = C))
  for(j in 1:C){
    sum = 0
    for(i in 1:N_L){
      sum = sum + (U_L[i,j] * db_L[i,1:dim])
    }
    V[j,] = (sum / sum(U_L[,j]))
  }
  
  colnames(V) = c(1:dim)
  return(V)
}


initialize_V <- function(db_L, U_L){
  N_L = nrow(db_L)
  dim = ncol(db_L) - 1
  C = ncol(U_L)
  
  cat("Criando V para inicializacao \n")
  #Criando V como definido na eq 12
  V = as.data.frame(matrix(ncol = dim, nrow = C))
  for(j in 1:C){
    sum = 0
    for(i in 1:N_L){
      sum = sum + (U_L[i,j] * db_L[i,1:dim])
    }
    V[j,] = (sum / sum(U_L[,j]))
  }
  
  colnames(V) = c(1:dim)
  #Calculando phi como definido na eq 11
  #Como K(xi,xi) = phi(xi) * phi(xi), então phi(xi) = sqrt(K(xi,xi))
  
  cat("Encontrando phi \n")
  phi = as.data.frame(matrix(nrow = C))
  for(j in 1:C){
    sum = 0
    sum2 = 0
    for(i in 1:N_L){
      sum = sum + (U_L[i,j]* kernel_func(db_L[i,1:dim],V[j,])* sqrt(kernel_func(db_L[i,1:dim])))#testar colocar o db_L com o label para phi(x)
      sum2 = sum2 + (U_L[i,j]* kernel_func(db_L[i,1:dim],V[j,]))
    }
    phi[j,] = sum/sum2
  }
  colnames(phi) = c("1")
  return(phi)
}

update_V <- function(db_L,db_U,U_L,U_U,V){
  # Fazendo como na eq 10
  #Como K(xj,Vi) = phi(Xj) * phi(Vi), então K(xj,Vi) = sqrt(K(xj,xj)) * phi(Vi)
  # E como na eq 10 é K(xj,Vi) * phi(Xj), então = K(xj,xj) * phi(vi)
  N_L = nrow(db_L)
  N_U = nrow(db_U)
  C = ncol(U_U)
  dim = ncol(db_U)
  
  phi = as.data.frame(matrix(nrow = C))
  for(i in 1:C){
    sum = 0
    sum2 = 0
    sum3 = 0
    sum4 = 0
    for(l in 1:N_L){
      sum = sum + (U_L[l,i]* kernel_func(db_L[l,])* V[i,])
      sum2 = sum2 + (U_L[l,i]* sqrt(kernel_func(db_L[l,]))*V[i,])
    }
    for(u in 1:N_U){
      sum3 = sum3 + (U_U[u,i]* kernel_func(db_U[u,])* V[i,])
      sum4 = sum4 + (U_U[u,i]* sqrt(kernel_func(db_U[u,]))*V[i,])
    }
    phi[i,] = (sum + sum3)/(sum2 + sum4)
  }
  return(V)
}

initialize_U_U <- function(db_U,K){
  
  #Fazendo como descrito no artigo
  N_U = nrow(db_U)
  cat("N_U :", N_U,"   -   C: ",K)
  U_U = matrix(nrow = N_U, ncol = K)
  
  for(i in 1:N_U){
    #Generate C positive random numbers
    r = runif(K)
    for(j in 1:K){
      U_U[i,j] = r[j]/sum(r)
    }
  }
  
  return(U_U)
}

update_U_U <- function(db_U,U_U,V){
  #Provavelmente tenho que arrumar
  N_U = nrow(db_U)
  C = ncol(U_U)
  U = as.data.frame(matrix(ncol = C,nrow = N_U))
  for(i in 1:N_U){
    for(j in 1:C){
      sum = 0
      for(k in 1:C){
        sum = sum + (U_U[i,j] * (phi_V[k,] * sqrt(kernel_func(db_U[i,]))))
        #sum = sum + (U_U[i,j] * V[k,] * sqrt(kernel_func(db_U[i,])))
        #sum = sum + (U_U[i,j] * kernel_func(db_U[i,],V[k,]))
        #cat("\n U ",kernel_func(db_U[i,],V[k,]))
      }
      #cat(sum,"   -   ",(U_U[i,j]* phi_V[j,] * sqrt(kernel_func(db_U[i,]))),"\n")
      U[i,j] = (U_U[i,j]* phi_V[j,] * sqrt(kernel_func(db_U[i,])))/sum
      #U[i,j] = (U_U[i,j]* V[j,] * sqrt(kernel_func(db_U[i,])))/sum
      #U[i,j] = (U_U[i,j]* kernel_func(db_U[i,],V[j,]))/sum
    }
  }
  return(U)
}

initialize_U_L <- function(db_L,C){
  #(já que não é dado um valor pelo especialista, como descrito no artigo, então vou setar 0.99 e 0.01 de acordo com seus rotulos)
  N_L = nrow(db_L)
  dim = ncol(db_L)-1
  U_L = matrix(nrow = N_L, ncol = C, 0.01)
  for(i in 1: N_L){
    for(j in 1:C){
      if(db_L[i,dim+1] == j){
        U_L[i,j] = 0.99
      }
    }
  }
  
  return(U_L)
}

update_U_L <- function(db_L,U_L,V){
  N_L = nrow(db_L)
  C = ncol(U_L)
  dim = ncol(db_L)-1
  U = as.data.frame(matrix(ncol = C,nrow = N_L))
  
  for(i in 1:N_L){
    for(j in 1:C){
      sum = 0
      #kernel1 = kernel_func(db_L[i,1:dim]) -(2*kernel_func(db_L[i,1:dim],V[j,])) + kernel_func(V[j,])
      kernel1 = kernel_func(db_L[i,1:dim]) -(2* sqrt(kernel_func(db_L[i,1:dim])) *phi_V[j,]) + (phi_V[j,]^2)
      for(k in 1:C){
        #kernel2 = kernel_func(db_L[i,1:dim]) -(2*kernel_func(db_L[i,1:dim],V[k,])) + kernel_func(V[k,])
        kernel2 = kernel_func(db_L[i,1:dim]) -(2* sqrt(kernel_func(db_L[i,1:dim])) *phi_V[k,]) + (phi_V[k,]^2)
        sum = sum + (kernel1/kernel2)
      }
      U[i,j] = 1/sum
    }
  }
  return(U)
}


update_sigma <- function(){
  
  return(sigma)
}

difference_calc <- function(){
  
  return(difference)
}

kernel_func <- function(a,b = NULL){
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
    phi = exp(-(d^2)/(2*sigma))
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
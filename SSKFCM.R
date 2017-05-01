sskfcm_function <- function(db ,V = NULL, m = 2, kernel_f = "polynomial", th = 0.01, iter.max = 200){
  #O db deve estar comvalores numéricos para as classes e 0 para os não rotulados
  
  #dimensão da base de dados e seu tamanho
  dim = ncol(db)-1
  N = nrow(db)  
  
  #Renomeando as colunas
  colnames(db) = c(1:dim,"class")
  
  #Separando os dois data sets, em rotulados e não rotulados
  b = db$class
  aux = b > 0
  #Unlabel DataSet
  db_U = db[!aux,1:dim]
  #Label DataSet
  db_L = db[aux,1:(dim+1)]
  
  #Encontrando a quantidade de classes
  rotulados = as.factor(db_L[,3])
  classes = as.numeric(names(table(rotulados)))
  C = length(classes)
  
  #Criando a matriz U_L 
  U_L = initialize_U_L(db_L, C)
  
  #Encontrando os valores iniciais de V
  phi_V = initialize_V(db_L, U_L)
  
  #Criando matriz U_U
  U_U = initialize_U_U(db_U, C)
  
  
}



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
  
  phi = as.data.frame(matrix(nrow = C))
  for(j in 1:C){
    sum = 0
    sum2 = 0
    for(i in 1:N_L){
      sum = sum + (U_L[i,j]* kernel_func(db_L[i,],V[j,])* sqrt(kernel_func(db_L[i,])))
      sum2 = sum2 + (U_L[i,j]* kernel_func(db_L[i,],V[j,]))
    }
    phi[j,] = sum/sum2
  }
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
      sum2 = sum2 + (U_L[l,j]* sqrt(kernel_func(db_L[l,]))*V[i,])
    }
    for(u in 1:N_U){
      sum3 = sum3 + (U_U[u,i]* kernel_func(db_U[u,])* V[i,])
      sum4 = sum4 + (U_U[u,j]* sqrt(kernel_func(db_U[u,]))*V[i,])
    }
    phi[i,] = (sum + sum3)/(sum2 + sum4)
  }
  return(V)
}

initialize_U_U <- function(db_U,C){
  
  #Fazendo como descrito no artigo
  N_U = nrow(db_U)
  U_U = matrix(nrow = N_U, ncol = C)
  
  for(i in 1:N_U){
    #Generate C positive random numbers
    r = runif(C)
    for(j in 1:C){
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
        #sum = sum + (U_U[i,j] * phi_V[k,] * sqrt(kernel_func(db_U[i,])))
        sum = sum + (U_U[i,j] * V[k,] * sqrt(kernel_func(db_U[i,])))
        #sum = sum + (U_U[i,j] * kernel_func(db_U[i,],V[k,]))
        #cat("\n U ",kernel_func(db_U[i,],V[k,]))
      }
      #cat(sum,"   -   ",(U_U[i,j]* phi_V[j,] * sqrt(kernel_func(db_U[i,]))),"\n")
      #U[i,j] = (U_U[i,j]* phi_V[j,] * sqrt(kernel_func(db_U[i,])))/sum
      U[i,j] = (U_U[i,j]* V[j,] * sqrt(kernel_func(db_U[i,])))/sum
      #U[i,j] = (U_U[i,j]* kernel_func(db_U[i,],V[j,]))/sum
    }
  }
  
}

initialize_U_L <- function(db_L,C){
  #(já que não é dado um valor pelo especialista, como descrito no artigo, então vou setar 0.99 e 0.01 de acordo com seus rotulos)
  N_L = nrow(db_L)
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

update_U_L <- function(){
  
  return(U)
}

kernel.func <- function(a,...,b){
  
  return(phi)
}

update_sigma <- function(){
  
  return(sigma)
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
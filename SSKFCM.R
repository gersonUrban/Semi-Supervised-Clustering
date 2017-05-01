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
  db_L = db[aux,]
  
  
  #Encontrando a quantidade de classes
  rotulados = as.factor(db$class[aux])
  classes = as.numeric(names(table(rotulados)))
  nclasses = length(classes)
}

break_db <- function(db){
  
  db2 = NULL
  db2$label <- 0 
  db2$unlabel <- 0
  return(db2)
}

initialize_V <- function(){
  
  return(V)
}

update_V <- function(){
  
  return(V)
}

update_U_def <- function(){
  
}

update_U_super <- function(){
  
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
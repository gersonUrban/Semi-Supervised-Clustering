#sskfcm_function <- function(db, m = 2, kernel_f = "polynomial", th = 0.01, iter.max = 50){
  source("SSKFCM_utils.R")
  #O db deve estar comvalores numéricos para as classes e 0 para os não rotulados
  cat("Inicializando SSKFCM\n")
  
  #Tornando variavel global
  kernel_f <<- kernel_f
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
  rotulados = as.factor(db_L[,dim+1])
  classes = as.numeric(names(table(rotulados)))
  C = length(classes)
  
  cat("Criando matriz U_L\n")
  #Criando a matriz U_L 
  U_L = initialize_U_L(db_L, C)
  
  cat("Criando phi V U\n")
  #Encontrando os valores iniciais de V
  phi_V = initialize_V(db_L, U_L)
  
  cat("Criando matriz U_U\n")
  #Criando matriz U_U
  U_U = initialize_U_U(db_U, C)
  t=0
  cat("Inicializando loop SSKFCM\n")
  while(t < iter.max){
    cat("\n Iteracao : ",t)
    #Atualizando a matriz de pertinencia L
    U_L2 = update_U_L(db_L,U_L,phi_V)
    U_L = U_L2
    
    #Atualizando a matriz de pertinencia U
    #Enviando V, o certo seria phi_V (eu acho)
    U_U2 = update_U_U(db_U,U_U,phi_V)
    U_U = U_U2
    
    phi_V2 = update_V(db_L,db_U,U_L,U_U,phi_V)
    phi_V = phi_V2
    t = t+1
  }
  
  resultado = NULL
  resultado$U_L = U_L
  resultado$U_U = U_U
  resultado$phi_V = phi_V
  #return(resultado)
  
#}
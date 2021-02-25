#Separação aleatória
train_test_split = function(df, test_perc){
  length_df = nrow(df)
  index_permutation = sample(length_df)
  train_size = round((1-test_perc) * length_df)
  train = df[index_permutation[1:train_size],]
  test = df[index_permutation[(train_size+1):length_df],]
  return(list("train" = train,
              "test" = test))
}

#Separação plena (não há IDs iguais no treino e teste)
train_test_plena = function(df, test_perc, ID){
  #seleciona número de indivíduos distintos
  length_id = df %>% select(ID) %>% n_distinct()
  #embaralha IDs
  ID_permutation = sample(length_id)
  #ve qual quantos Ids vão ser selecionados para cada banco
  train_threshold = round((1-test_perc) * length_id)
  ID_train = ID_permutation[1:train_threshold]
  ID_test = ID_permutation[train_threshold + 1: length_id]
  #filtra os IDs de cada grupo
  train = df %>% filter(ID %in% ID_train)
  test = df %>% filter(ID %in% ID_test)
  return(list("train" = train,
              "test" = test))
}

#Cross validation aleatório
cv_split = function(df, n_cv, n_interacao){
  #Definindo semente
  set.seed(42)
  #pega o número de linhas
  length_df = nrow(df)
  #embaralha os numeros de linhas e embaralha o banco
  rows <- sample(length_df)
  df = df[rows,]
  #Cria indice pra cada linha
  df = df %>% mutate(index_row = seq(1,length_df))
  #define o tamanho da parte
  size_parte = as.integer(length_df/n_cv)
  #pega os threshold
  partes = c(1, size_parte)
  for (i in 2:n_cv){
    partes[i+1] <- as.integer(size_parte*i)
  }
  #bota no teste os referentes da intereção n
  test = df[partes[n_interacao]:partes[n_interacao+1],]
  #Pega os index das linhas do teste
  ID_test = test$index_row
  #e filtra por aqueles que não estão no teste
  train = df %>% filter(!index_row %in% ID_test)
  return(list("train" = train,
              "test" = test))
}

#Cross validation pleno (não há IDs iguais no treino e teste)
cv_pleno = function(df, n_cv, n_interacao, ID){
  #Definindo semente
  set.seed(42)
  #pega o número de linhas
  length_df = nrow(df)
  length_id = df %>% select(ID) %>% n_distinct()
  #embaralha os numeros de linhas e embaralha o banco
  rows <- sample(length_df)
  df = df[rows,]
  #define o tamanho da parte
  size_parte = as.integer(length_id/n_cv)
  #pega os threshold
  partes = c(1, size_parte)
  for (i in 2:n_cv){
    partes[i+1] <- as.integer(size_parte*i)
  }
  #bota no teste os referentes da intereção n
  ID_test = c(partes[n_interacao]:partes[n_interacao+1])
  test = df %>% filter(ID %in% ID_test)
  #e filtra por aqueles que não estão no teste
  train = df %>% filter(!ID %in% ID_test)
  return(list("train" = train,
              "test" = test))
}

########################
# Instalação de pacotes
pacotes <- c(
  'tidyverse',  # Pacote básico de datawrangling
  'rpart',      # Biblioteca de árvores
  'rpart.plot', # Conjunto com Rpart, plota a parvore
  'gtools',     # funções auxiliares como quantcut,
  'Rmisc',      # carrega a função sumarySE para a descritiva
  'scales',     # importa paletas de cores
  'viridis',    # Escalas 'viridis' para o ggplot2
  'caret',       # Funções úteis para machine learning
  'AMR',
  'randomForest',
  'fastDummies',
  'rattle',
  'xgboost',
  'ggpubr',
  'titanic'
  
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#Bagging (bootstrapping aggregation) é um uso do bootstrapping 

#É um método ensemble - aggregation: combinação de (em geral uma média simples) das previsões de dois ou mais modelos previamente construídos
#A ideia é que ainda que cada modelo individualmente seja um "weak learner", a combinação deles gere um "strong learner", ou seja, um preditor melhor

#Queremos gerar preditores diferentes, mas que mirem para o mesmo alvo = variável resposta

# Buscar reprodutibilidade
set.seed(2360873)

# Gera 80% de 1´s e 20% de 2´s para separar as amostras

n <- sample(1:2, # vamos amostrar elementos do conjunto c(1,2)
            size=nrow(titanic), # O tamanho da amostragem é 891 (linhas)
            replace=TRUE, # Amostragem com reposição (de c(1,2))
            prob=c(0.8,0.2)) # A probabilidade de ser 1 é  80%, de ser 2 é 20%

#sample retira amostras
n %>%  table

######################################
# Dividir amostras de treino e teste #

# Amostra de treino: n==1 (os 80%)
treino <- titanic[n==1,]

# Amostra de teste: n==2 (os 20%)
teste <- titanic[n==2,]


######################################
# Treinar a Random Forest            #

# Semente aleatória para buscar a reprodutibilidade
set.seed(2360873)

# medir tempo de execução (iniciar o cronometro)
start_time <- Sys.time()

# Rodar o algoritmo
treino_rf <- randomForest::randomForest(
  Survived ~ ., #Variavel resposta survided explicada por todas as outras variaveis da base
  data = treino,  #base de dados 
  ntree = 50, #numero de árvores
  mtry = 3, #hiperparametro que pega 3 variáveis aleatórias da base
  importance = T) #calculo da importância da variável

# parar o cronometro
end_time <- Sys.time()
end_time - start_time

print(treino_rf)
############################
# Avaliar o modelo         #

#type prob me devolve a probabilidade 
#predict sozinho devolve a classificação 

# Base de treino
avalia <- function(modelo, nome_modelo="modelo"){
  p_treino <- predict(modelo, treino, type='prob') # Probabilidade predita
  c_treino <- predict(modelo, treino)  # Classificação
  
  #Base de teste
  p_teste <- predict(modelo, teste, type='prob')
  c_teste <- predict(modelo, teste)
  
  # Data frame de avaliação (Treino)
  aval_treino <- data.frame(obs = treino$Survived, #evento observado
         pred = c_treino, #evento predito - classificação
         Y = p_treino[,2], #probabilidade de sobreviver (evento)
         N = 1-p_treino[,2] #probabilidade de não sobreviver (não evento)
  )
  
  # Data frame de avaliação (Teste)
  aval_teste <- data.frame(obs=teste$Survived, 
                              pred=c_teste,
                              Y = p_teste[,2],
                              N = 1-p_teste[,2]
  )
  
  #área da curva ROC 
  #TwoClassSummary só aceita dataframe
  tcs_treino <- caret::twoClassSummary(aval_treino, 
                                       lev=levels(aval_treino$obs))
  #indica a base e os levels, níveis da variável resposta 
  tcs_teste <- caret::twoClassSummary(aval_teste, 
                                      lev=levels(aval_teste$obs))
  ##########################
  ###### Curva ROC #########
  
  CurvaROC <- ggplot2::ggplot(aval_teste, aes(d = obs, m = Y, colour='1')) + 
    plotROC::geom_roc(n.cuts = 0, color="blue") +
    plotROC::geom_roc(data=aval_treino,
                      aes(d = obs, m = Y, colour='1'),
                      n.cuts = 0, color = "red") +
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    theme(legend.position = "none") +
    ggtitle(paste("Curva ROC | ", nome_modelo, " | AUC-treino=",
                  percent(tcs_treino[1]),
                  "| AUC_teste = ",
                  percent(tcs_teste[1]))
    )
  
  print('Avaliação base de treino')
  print(tcs_treino)
  print('Avaliação base de teste')
  print(tcs_teste)
  CurvaROC
}
avalia(treino_rf, nome_modelo="Random Forest")
#Percebemos que temos overfitting: 99% de acurácia no treino e 85% na base teste
#AUC treino de 98% e AUC teste de 85%


# Crossvalidation - k fold
###########################################
# Usando o Caret para fazer o grid-search #

tempo_ini <- Sys.time()

# number: number of folds for training
# repeats: keep the number for training

# O objeto gerado por trainControl vai controlar o algoritmo 
# como vai funcionar o treino do modelo que iremos fazer 

controle <- caret::trainControl(
  method='repeatedcv', # Solicita como método um K-Fold com repetições
  number=6, # Número de FOLDS (o k do k-fold)
  repeats=3, # Número de repetições
  search='grid', # especifica o grid-search
  summaryFunction = twoClassSummary, # Função de avaliação de performance, a mesma que calcula a curva roc
  classProbs = TRUE # Necessário para calcular a curva ROC
)

# agora vamos especificar o grid
# vamos variar um mtry, numero de variaveis que o modelo vai testar pra gente, no caso de 1 até 10

grid <- base::expand.grid(.mtry=c(1:10))

# Vamos treinar todos os modelos do grid-search com cross-validation
gridsearch_rf <- caret::train(Survived ~ ., #Fórmula (todas as variáveis)
                 data = treino,# Base de dados
                 method = 'rf', # Random-forest
                 metric='ROC', # Escolhe o melhor por essa métrica
                 trControl = controle, # Parâmetros de controle do algoritmo
                 ntree=100,  # Numero de árvores
                 tuneGrid = grid)   # Percorre o grid especificado aqui

#Grid search é uma "grade" de possibilidades de hiperparâmetros

#4 (number) * 2 (repeats) * 10 (combinações do grid, nº variáveis) -> 80 modelos que o caret fará para nós, baseando-se no k-fold e suas repetições, e no grid
# 8 mil árvores - porque cada modelo consiste em 100 árvores (ntree)


print(gridsearch_rf)
plot(gridsearch_rf)

tempo_fim <- Sys.time()
tempo_fim - tempo_ini


###################################
# Avaliar o modelo tunado         #

avalia(gridsearch_rf, nome_modelo='RF Tunado')

#Bases pequenas não nos dá muita margem para fazermos um grid mais sofisticado
#Possivelmente teríamos um resultado melhor se aumentarmos o numero de folds, essencialmente, aumentaríamos a base de treinamento

#Bagging x Boosting
# A principal diferença entre esses métodos de aprendizagem é o método de treinamento. No bagging, os cientistas de dados melhoram a precisão de aprendizes fracos treinando vários deles ao mesmo tempo em vários conjuntos de dados. Em contraste, o boosting treina os aprendizes fracos um após o outro.


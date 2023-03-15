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


#########################################
###          Bootstrapping            ### 
#########################################

#É um modelo ensemble: mistura de modelos já existentes
#Um método de agregação simples e poderoso que consiste em obter a média de várias previsões

#O bootstrapping avalia a média e, em vez de alterarmos o algoritmo, alteramos a base utilizando o mesmo algoritmo

#Temos 1 conjunto de dados de tamanho N e queremos estimar o erro padrão de um parâmetro (ex: média)
#1: Retiramos uma amostra aleatória de tamanho N da base
#2: Calculamos esse parâmetro e armazenamos essa informação
#3: Repetimos isso várias vezes (M vezes)
#4: Podemos calculas a média e o erro padrão do estimador

# Gerando os dados
set.seed(2360873)
L=1000
dados = rnorm(L)**2 #gerando dados aleatórios com 1000 obs

# o quantil amostral é:
quantile(dados, .75)
#percentil 75% - 3 quartil

# Definindo o número de amostras bootstrap
M=10000 #numero de repeticoes das amostras

# Inicializando um vetor que conterá as médias
estimativas <- vector(length=M)

# Realizar M amostras dos dados e calcular a média em cada uma delas

for (i in 1:M){
  estimativas[i] <- quantile(sample(x = dados, 
                                    size=L, #num de obs da amostra, linhas
                                    replace=TRUE), .75) #3º quartil
}
#retirando amostras com reposição da nossa tabela original, fazendo isso 10 mil vezes e guardando no vetor estimativas

#Vamos avaliar o intervalo de confiança
#Calcular os quantis
estimativas %>% 
  quantile(c(.025, .975)) %>%  #2,5% e 97%
  round(3) #3 casas decimais
#O terceiro quartil deve estar entre esse intervalo
#intervalo de confiança de 95%
#o terceiro quartil deve estar entre o intervalo de confiança: 1295 e 1536

data.frame(estimativas) %>% head
#transformando em dataframe 

#histograma com as estimativas do percentil 75%
p <- data.frame(estimativas) %>%
  ggplot( aes(x=estimativas, fill='q3')) +
  geom_histogram(bins=30, alpha=0.9) +
  ggtitle("Distribuição bootstrap do percentil 75%") +
  scale_fill_viridis_d(direction = -1, begin=0, end=.75) +
  theme_bw() + 
  theme(
    plot.title = element_text(size=12)
  )
p

#Retira várias amostras com reposição, faz alguma coisa com elas e agrega o resultado de alguma forma

#Para apagar as variáveis e liberar memória
rm(list=ls())
gc()


######################################################

#########################################
###        Boosting manual            ### 
#########################################

#O boosting nada mais é que uma "correção sequencial de erros" em busca do modelo mais assertivo possível
#Ou seja, utilizamos os resíduos de cada predição para construir uma nova árvore de forma repetida até minimizarmos ao máximo o erro geral, aumentando a acurácia

#A variável resposta de uma iteração é o "erro" da anterior 

#Gradient boosting é um boosting com árvores, com alguns hiperparâmetros que controlam o algoritmo 

#####
# Gerando os dados
# x é uma sequencia de valores entre 0 e 1 - 1000 valores
x <- seq(0,1, length.out=1000)
x %>% hist #distribuicao bem uniforme

# y segue uma relação quadrática
a <- 0
b <- 10
c <- -10 #negativo = concavidade para baixo

set.seed(2360873)
y <- a + b*x + c*x**2 + rnorm(length(x), mean=0, sd=.1)
#distribuição normal = média é 0 e desvio padrão 1

df <- data.frame(x, y)

p0 <- ggplot(df, aes(x,y)) + 
  geom_point(aes(colour='Observado')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.85, name = "Valor") +
  theme(legend.position="bottom",
        legend.spacing.x = unit(0, 'cm'))
p0
#grafico do valor observado
#agora vamos calcular o valor esperado

########################
# Construindo a árvore #
tree <- rpart(y~x, # y vai ser explicado pelo x
              data=df, #base de dados
              control=rpart.control(maxdepth = 2, cp=0))
#profundidade igual a 2
#4 folhas

# Plotando a árvore
paleta = scales::viridis_pal(begin=.75, end=1)(20)
rpart.plot::rpart.plot(tree,
                       box.palette = paleta) # Paleta de cores

# Valores preditos
df['p'] = predict(tree, df) #valor predito
df['r'] = df$y - df$p #resíduo usado no boosting
#o real - o predito

# Valores esperados e observados
boost0_O_vs_E <- ggplot(df, aes(x,y)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,p, colour='Esperado')) + #faz a escadinha em cima do x e do p
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  # guides(colour = guide_legend(label.position = "bottom")) +
  labs(title="Valores observados vs esperados") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

boost0_O_vs_E

# Gráfico de resíduos -> o que iremos tentar ajustas
boost0_res <- ggplot(df, aes(x,r)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Resíduo')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  theme_bw() +
  theme(legend.position="bottom") +
  labs(title="Gráfico de resíduos") +
  scale_y_continuous(name= "r") +
  scale_x_continuous(name= "x")

boost0_res
#em tese se há padrão, conseguimos melhorar o modelo, melhorar o r quadrado da árvore

#para plotar os gráficos lado a lado
#dados originais, árvore e residuos
ggpubr::ggarrange(boost0_O_vs_E, boost0_res, 
                  labels = c("A", "B"),
                  ncol = 2, nrow = 1)

# Primeira iteração de boosting manual
# outra árvore, com a variável resíduo como resposta e a variável explicativa continua o x
#variável resíduo sendo explicada pelo x
tree1 <- rpart(r~x, 
               data=df, #mesma base de dados
               control=rpart.control(maxdepth = 2, cp=0))
tree1

df['p1'] = predict(tree1, df) # Predito da árvore neste passo -> aqui nosso modelo vai tentar adivinhar o RESIDUO e não o Y

df['P1'] = df$p + df$p1 # Predito do boosting (acumulado) -> vai acumular nossa primeira predicao (y~x) com a realizada agora (r~x)

#Essa predicao tenta acertar o y -> porque o residuo é o valor real de y menos o predito, então y seria o valor do resiuo + o predito

df['r1'] = df$r - df$p1 # resíduo do boosting -> residuo do residuo


# Gráfico da primeira iteração de boosting manual
# O QUE O MODELO ESTÁ FAZENDO
# primeiramente faremos o residuo junto com a previsão 1
boost1_r_vs_E <- ggplot(df, aes(x,r)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,p1, colour='Esperado')) + #Ploting - predito
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Variável resposta neste passo") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

boost1_r_vs_E

# O QUE ACONTECE COM O MODELO FINAL 
# utilizando o predito do boosting acumulado, vai acumular nossa primeira predicao (y~x) com a realizada do resíduo (r~x: P1) -> da o valor observado Y

boost1_O_vs_E <- ggplot(df, aes(x,y)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,P1, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Observado vs Esperado (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

# Gráfico de resíduos
boost1_r <- ggplot(df, aes(x,r1)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Resíduo')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Gráfico de resíduos (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "r") +
  scale_x_continuous(name= "x")


ggpubr::ggarrange(boost1_r_vs_E, boost1_O_vs_E, boost1_r,
                  ncol = 3, nrow = 1)

#cada degrau da escadinha do valor esperado é uma folha

#####
# Terceira iteração do boosting
# residuo da árvore 2 do boosting sendo explicada pelo x
tree2 <- rpart(r1~x, 
               data=df,
               control=rpart.control(maxdepth = 2, cp=0))

df['p2'] = predict(tree2, df) # predito da árvore tree2
df['P2'] = df$P1 + df$p2      # predito do boosting neste passo, vamos sempre somando as predições -> acumulado das predições (tanto a original do y, quanto a dos dois erros)

#df['P2'] = df$P1 + df$p2 * 0,5 -> para usar o learning rate 

df['r2'] = df$r1 - df$p2      # resíduo da árvore neste passo (resíduo do boosting)
# df['r2'] = df$y - df$P2     # O mesmo que a linha acima


# Gráfico da primeira iteração de boosting manual
# O QUE O MODELO ESTÁ FAZENDO
boost2_r_vs_E <- ggplot(df, aes(x,r1)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,p2, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Variável resposta neste passo") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y(i)") +
  scale_x_continuous(name= "x")


# O QUE ACONTECE COM O MODELO FINAL
boost2_O_vs_E <- ggplot(df, aes(x,y)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Observado')) +
  geom_path(aes(x,P2, colour='Esperado')) + #Ploting
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Observado vs Esperado (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "y") +
  scale_x_continuous(name= "x")

# Gráfico de resíduos
boost2_r <- ggplot(df, aes(x,r2)) + 
  geom_point(alpha=.7, size=.5, aes(colour='Resíduo')) +
  scale_color_viridis(discrete=TRUE, begin=0, end=.8, name = "Dado: ") +
  labs(title="Gráfico de resíduos (final)") +
  theme_bw() +
  theme(legend.position="bottom") +
  scale_y_continuous(name= "r") +
  scale_x_continuous(name= "x")

boost2_O_vs_E

ggpubr::ggarrange(boost2_r_vs_E, boost2_O_vs_E, boost2_r,
                  # labels = c("A", "B"),
                  ncol = 3, nrow = 1)

#se fizermos muitos boostings, exageradamente, dará overffiting!

#É ai que entra o "Learning Rate": ele diminui o impacto de cada iteração


#Para apagar as variáveis e liberar memória
rm(list=ls())
gc()


################################################
####            XGBoosting                  ####
################################################


#Nada mais é que uma implementação famosa do Gradient Boosting

set.seed(2360873)

# Gera 80% de 1´s e 20% de 2´s para separar as amostras
titanic %>% head

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


#Parâmetros do trainControl
#number: number of folds or resampling iterations
#repeats: for repeated k-fold cross-validation, the number of complete sets of folds to compute

#vamos fazer um k-fold com 10 folds 
controle <- caret::trainControl(
  "cv", #cross validation
  number = 10, #10 partições 
  summaryFunction = twoClassSummary, # Função de avaliação de performance
  classProbs = TRUE # Necessário para calcular a curva ROC
)
#se não colocarmos o summaryfunction e o classprobs = true, ele faz o controle com a acurácia

modelo <- caret::train(
  Survived ~., #variavel survived sendo explicada por todas as outras
  data = treino, #base de treino
  method = "xgbTree", #xgboosting
  trControl = controle, #parametros do controle acima, kfold
  tuneGrid = NULL,
  verbosity = 0)

modelo
#ja trouxe um grid search para nós
#parametro eta = learning rate
#max depth = profundidade
#nrounds = numero de arvores (no exemplo ele teste 50, 100 e 150 arvores)
#colsample bytree = é o mtry da random forest, proporcao de variaveis que ele vai amostrar em cada round
#subsample = amostra um pouco menor q a original; é tipo um pasting

#nrounds é o numero de árvores, no exemplo é sempre 50,100 ou 150
#eta = learning rate, se diminuirmos ele, precisaremos de uma arvore maior, mais profunda

###################################
# Avaliar o XGBoosting            #
avalia(modelo, nome_modelo="XGBoosting")

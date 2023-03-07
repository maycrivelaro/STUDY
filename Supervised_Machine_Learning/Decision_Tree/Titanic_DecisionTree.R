#para "limpar" o R e a memória
rm(list = ls())
gc()

########################
# Instalação de pacotes
pacotes <- c('titanic',    # carrega a base original titanic_treino 
             'tidyverse',  # Pacote básico de datawrangling
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'caret',      # Funções úteis para machine learning
             'plotROC'     # Para plotar a curva ROC
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

#############################  

#################################
# Objetivo: Classificar passageiros sobreviventes de acordo somente com variáveis do registro deles
################################

#################################### 
# Breve análise descritiva #

titanic %>% head

# Vamos criar uma base temporária para manter a base original intacta
tmp <- titanic

#Criação da variável targete binária (0,1)
tmp$survived <- as.integer(titanic$Survived=="Y")
tmp %>% head

##########################################
# Função para fazer a análise descritiva #

# Vamos avaliar a distribuição de sobreviventes por cada variável X
# Sumarizamos então y por categoria de X e montamos um gráfico de perfis

descritiva <- function(var){
  # Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(tmp, measurevar="survived", groupvars=c(var))
  
  ggplot(tgc) + 
    # Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/891, fill=as.factor(tgc[,var]))) + 
    # Plota as barras de erro
    geom_errorbar(aes(x=tgc[,var], y=survived, ymin=survived-se, ymax=survived+se, colour='1'), width=.1) +
    # Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    # Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    # Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    # Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    # Remove a legenda
    theme(legend.position = "none") +
    # Rótulo dos eixos
    xlab(var) + ylab("Taxa de sobreviventes") + 
    # Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*891, name = "Frequencia"), labels = scales::percent)
}

#ymin=survived-se -> média de sobreviventes - o desvio padrão 


descritiva("Sex") #mulheres estão em menor número (frequência), porém possuem taxa de sobrevivência maior
descritiva("Pclass") #passageiros da primeira classe possuem tx de sobrevivência maior
descritiva("Embarked") #cidade de embarque C possui tx sobrevivência maior
descritiva("SibSp") #pessoa que esta sozinha sobrevive menos de quem está acompanhado de 1 ou 2 pessoas
descritiva("Parch") #se tem pais ou filhos a bordo, aparentemente quem esta sozinho tem menos chance de sobreviver

# Vamos categorizar as variáveis contínuas para analisar
# quantcut quebra a variável em quantis 

tmp$cat_age <- quantcut(tmp$Age, 20)
descritiva("cat_age")
#as faixas de idade variam bastante, porém crianças têm tx de sobrevivencia maior

#fare é o valor que a pessoa pagou no tkt
#quanto maior o valor do fare, maior a tx de sobrevivência 
tmp$cat_fare <- quantcut(tmp$Fare, 10)
descritiva("cat_fare")

# Listagem das variáveis com algumas características
tmp %>% str
tmp %>% glimpse

#variáveis qualitativas precisam estar transformadas em factor

#############################################
# Vamos construir a árvore de classificação #

# primeiro argumento é a variável resposta ~ depois as variáveis explicativas

arvore <- rpart::rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                       data=titanic,
                       parms = list(split = 'gini'), # podemos trocar para  'information'
                       method='class' # Essa opção indica que a resposta é qualitativa
)

#gini é o parâmetro padrão = DEFAULT

#########################
# Visualizando a árvore #

# Definindo uma paleta de cores
paleta = scales::viridis_pal(begin=.75, end=1)(20)

# Plotando a árvore
rpart.plot::rpart.plot(arvore,
                       box.palette = paleta) # Paleta de cores

##############################
# Avaliação básica da árvore #

# Predizendo com a árvore

# Probabilidade de sobreviver
prob = predict(arvore, titanic)
prob %>%  head

# Classificação dos sobreviventes, sim ou não?
class = prob[,2]>.5

#probabilidade do evento maior que 50% sobreviveu, se não não
#coluna 2 pq é onde está a probabilidade de sobrevivencia

class %>%  head
sum(class) #-> para ver quantos sobreviveram

# Matriz de confusão com tabela de frequência
# Vamos cruzar a classificação que predizemos com o que realmente aconteceu
tab <- table(class, titanic$Survived)
tab

#o nosso algoritmo são as linhas e a base do que realmente aconteceu as colunas
#nosso algoritmo diz que 295 sobreviveram e 596 não
#a base de dados diz que 342 sobreviveram e 549 não 

#o ideal é ter bastante gente em false/false e true/true (1,1 e 2,2)

sum(tab) #total da base

#vamo calcular a quantidade de acertos -> acurácia
acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

#83% de acurácia no nosso modelo 


#para ver a variável mais importante da árvore
arvore$variable.importance

#Sex, Fare e Pclass são as variáveis mais importantes para predizer o modelo!


###################################################
###################################################
# VALIDAÇÃO CRUZADA

# Agora iremos fazer uma nova Decision tree com cross validation e avaliação de overfitting

#"limpando" o R e a memória novamente
rm(list = ls())
gc()

library(titanic)
titanic %>% head

# Vamos separar a base em treinamento e teste #
set.seed(123) #funcionamento aleatório para minimizar as diferenças de resultado

#runif -> gera números aleatórios
#dim(tatanic)[1] é o número de linhas da base

#logo abaixo estamos gerando a quantidade de linhas da base titanic de forma aleatória entre zero e 1, false ou true, 75% de true 25% de false
#75% da base será o treino e 25% o teste 
bool_treino <- stats::runif(dim(titanic)[1])>.25

table(bool_treino)

treino <- titanic[bool_treino,] #vai me trazer só as linhas que tem TRUE
teste  <- titanic[!bool_treino,] #vai me trazer só as linhas que não tem TRUE


# Deixar a árvore ser feliz e crescer ao infinto e além
# ATENÇÂO! NÃO PLOTAR ESTA ÁRVORE! -> muito complexa

set.seed(123)
arvore <- rpart::rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                       data=treino,
                       method='class',
                       xval=5,
                       control = rpart.control(cp = 0, 
                                               minsplit = 1, 
                                               maxdepth = 30)
)

#control -> vai controlar a árvore, os hiperparâmetros
#rpart.control, cp = parâmetro de complexidade (o R tem por default 10%)
#minslip: minimo de observações
#maxdepth: profundidade máxima da árvore (2^30 folhas)

# Verificando a complexidade da árvore
arvore$frame
# importancia variaveis -> Fare, age e sex são as maiores
arvore$variable.importance

############################################
# Avaliar a árvore na base de treino com predict
p_treino = stats::predict(arvore, treino) #probabilidade 
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N")) #classificando, tudo que a coluna 2 (TRUE) for acima de 50% ele vai colocar Y se não, N

# Avaliar a árvore na base de teste com predict
p_teste = stats::predict(arvore, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

#cálculo da acurácia
tab <- table(c_treino, treino$Survived)
tab
acc <- (tab[1,1]+tab[2,2])/nrow(treino)
sprintf('Acurácia na base de treino: %s ', percent(acc)) #98%

tab <- table(c_teste, teste$Survived)
tab
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
sprintf('Acurácia na base de treino: %s ', percent(acc)) #79%


#Cálculamos a acurácia do modelo na base de TREINO
#Desenvolvemos o modelo e a árvore na base de TREINO, a base de TESTE possui registros separados

#Overfitting!!!!
#Desenvolvemos um modelo numa base e aplicamos em uma outra (proveniente da mesma população), porém ele não funcionou tão bem (acurácia baixa)

###############################
# Curva ROC                   #
###############################

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas

# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (N no caso): contém a probabilidade da classe 2
library(plotROC)

aval_treino <- data.frame(obs=treino$Survived, #evento observado
                          pred=c_treino,#evento que modelo classificou 
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

#Y: probabilidade do Y 
#N: probabilidade do N

aval_treino %>% head
#mesma cara da base de trino, porém com as VARS obs e pred

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))
#levels: indicar os níveis, que são os da variável resposta 
#Calcula curva ROC; sensibilidade; especificidade

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC
#curva bem rente ao eixo y -> pois como é a base de treino, a acurácia está muito perto dos 100%


############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret

aval_teste <- data.frame(obs=teste$Survived, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))
#agora notamos que a curva ROC, sens. e spec. diminuiram bastante

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC

#Bem diferente da base treino

#Problema de deixar a árvore crescer o tanto que ela quer é esse:
# a árvore vai ficar com uma profundidade muito grande e acabar achando alguma coisa que vai deixar alguém sozinho numa folha e não conseguiremos generalizar

#Como resolver isso? 


# Cross validation mais sofistcado 
##########################
# pós-poda (Grid Search) # - prunning
########################## 

tab_cp <- rpart::printcp(arvore) #cria uma tabela com possibilidades de CP, para cada um deles a função faz um k-fold (padrão é 10)
tab_cp

# o "error" é na base treino é o erro relativo
# 0 "xerror" é na base de teste, iremos usar ele -> quanto menor o erro melhor -> a idéia é utilizarmos o CP que apresenta menor erro

# utilizar o cp que miniza o erro relativo na base teste

plotcp(arvore)

#esse comando vai pegar o CP que minima a árvore (menor xerror)
tab_cp[which.min(tab_cp[,'xerror']),]
cp_min <- tab_cp[which.min(tab_cp[,'xerror']),'CP']
cp_min #objeto para salvar o CP gerado acima

#agora vamos podar a árvore com o CP minimo descoberto acima
set.seed(1)
arvore_poda <- rpart::rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                            data=treino,
                            method='class',
                            xval=0,
                            control = rpart.control(cp = cp_min, 
                                                    minsplit = 1, 
                                                    maxdepth = 30)
)

#novamente vamos avaliar a base de treino e teste
p_treino = stats::predict(arvore_poda, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))
p_teste = stats::predict(arvore_poda, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

#####
#Agora vamos fazer as curvas ROC novamente
aval_treino <- data.frame(obs=treino$Survived, 
                          pred=c_treino,
                          Y = p_treino[,2],
                          N = 1-p_treino[,2]
)

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))
#curva ROC ficou bem menor 

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")

CurvaROC

#gráfico já não está tão próximo do eixo Y  

############################################
# Avaliar a árvore na base de teste

# Vamos calcular a área da curva ROC com uma função no Caret
# A função é o twoClassSummary, que espera como entrada um dataframe com esse layout:
# obs: uma coluna contendo um fator com as classes observadas
# pred: fator com as classes preditas
# <classe 1> (Y no caso): contém a probabilidade da classe 1
# <classe 2> (Y no caso): contém a probabilidade da classe 2
aval_teste <- data.frame(obs=teste$Survived, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

# Podemos usar o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC
# curva ROC caiu um pouco

#Porém podemos notar q as duas curvas ROCS estão mais parecidas agora


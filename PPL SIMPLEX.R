#Pacotes utilizados
pack <- c('lpSolve','lpSolveAPI')
lapply(pack, require, character.only = TRUE)

#"lpSolve","lpSolveAPI" -> DIFERENÇA É A FORMA DE MONTAR A MATRIZ

# #Maximização
# Exemplos: maximização de lucros, receitas; maximização de atendimentos; maximização de produtividade e eficiência; 
# #FO = max  {7X1 + 3X2 + 2X3}  
# Sujeito a:  5X1 + 2X2 + 2X3 <= 19
#             2X1 + X2 + 2X3 <= 8
# Para:       X1 >= 0
#             X2 >= 0 
#             X3 >= 0

#Desenhando o modelo
FUNCAO_OBJ <- c(7,3,2)
RESTRICOES <- matrix(c(5,2,2,
                       2,1,2), ncol=3, byrow=T)

RESTRICOES_SINAL <- c("<=","<=")
RESTRIÇÕES_RESP <- c(19,8)

#Rodando o modelo usando a função lp()
MODELO <- lp("max", FUNCAO_OBJ,RESTRICOES,RESTRICOES_SINAL,RESTRIÇÕES_RESP,compute.sens = T)

#Observando a quantidade sugerida pelo modelo
MODELO$solution

#Logo, X1 = 3 e X2 = 2 e x3 = 0 para atingir o maior resultado possível

#Resultado do modelo
MODELO
#Solução ótima da função Objetivo: 27 (máximo de lucro obtido) 


# Minimização

# Exemplos: minimizar custos de produção; riscos de um investimento; minimizar tempo de espera; minimizar o custo de uma compra; minimização de estoques

# #FO = max  {7X1 + 3X2 + 2X3}  
# Sujeito a:  5X1 + 2X2 + 2X3 <= 19
#             2X1 + X2 + 2X3 <= 8
# Para:       X1 >= 0
#             X2 >= 0 
#             X3 >= 0


#Desenhando o modelo - matriz

FUNCAO_OBJ <- c(3,2,1)
RESTRICOES <- matrix(c(2,1,1,
                       1,5,1), ncol=3, byrow=T)

RESTRICOES_SINAL <- c(">=",">=")
RESTRIÇÕES_RESP <- c(10,15)

#Rodando o modelo usando a função lp()
MODELO <- lp("min", FUNCAO_OBJ,RESTRICOES,RESTRICOES_SINAL,RESTRIÇÕES_RESP,compute.sens = T)

#Observando a quantidade sugerida pelo modelo
MODELO$solution

#Logo, X1 = 0 e X2 = 1,25 e x3 = 8,75 para atingir o resultado que mais minimiza a função objetivo

#Resultado do modelo
MODELO
#Solução ótima da função Objetivo: 11.25 (mínimo obtido) 

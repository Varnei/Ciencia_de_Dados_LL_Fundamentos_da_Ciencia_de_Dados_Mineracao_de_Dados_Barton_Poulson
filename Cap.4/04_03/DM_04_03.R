# DM_04_03.R

# CARREGAR PACOTES #########################################

pacman::p_load("class")  # class contém a função kNN

# CARREGAR DADOS ###########################################

# Ler CSV
df <- read.csv("~/Desktop/ccdefault.csv", header = T)
colnames(df)
head(df)  # Mostrar seis primeiros casos

# NORMALIZAR DADOS #########################################

# Se os intervalos das variáveis forem muito diferentes,
# convém normalizar as variáveis, colocando-as em intervalos
# semelhantes. Use uma função personalizada por enquanto.

# Definir função para normalização de dados
normalize <- function(x) {
  norm <- ((x - min(x))/(max(x) - min(x)))
  return (norm)
}

# Aplicar função ao quadro de dados
# (mas não ao índice ou ao resultado)
dfn <- as.data.frame(lapply(df[, 2:24], normalize))
head(dfn)

# Colocar de volta a variável-resposta e renomear
dfn <- cbind(dfn, df[, 25])
names(dfn)[24] <- "DEFAULT"

# Verificar dados
colnames(dfn)
head(dfn)

# DIVIDIR DADOS ############################################

# Dividir dados em conjunto de treinamento (2/3)
# e conjunto de testes (1/3)
set.seed(2786)  # Semente aleatória
dfn.split <- sample(2, nrow(dfn), 
                     replace = TRUE,
                     prob = c(2/3, 1/3))

# Criar conjuntos de dados de treinamento e testes
# sem rótulos para os resultados.
# Usar apenas as primeiras 23 variáveis.
dfn.train <- dfn[dfn.split == 1, 1:23]
dfn.test  <- dfn[dfn.split == 2, 1:23]

# Criar rótulos para os resultados
dfn.train.labels <- dfn[dfn.split == 1, 24]
dfn.test.labels  <- dfn[dfn.split == 2, 24]

# COMPILAR E TESTAR CLASSIFICADOR ##########################

# Compilar classificador para dados de teste.
# k = número de vizinhos a comparar; n ímpar evita empates.
# Testar vários valores de k e verificar a precisão
# na tabela a seguir.
dfn.pred <- knn(train = dfn.train,
                test = dfn.test, 
                cl = dfn.train.labels,  # classe verdadeira
                k = 9)                  # n vizinhos

# Comparar resultado previsto com resultado observado
table(dfn.pred, dfn.test.labels)

# LIMPAR ###################################################

# Limpar espaço de trabalho
rm(list = ls()) 

# Descarregar pacotes
pacman::p_unload(class)

# Limpar console
cat("\014")  # CTRL+L

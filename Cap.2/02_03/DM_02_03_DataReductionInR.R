# .R

# INSTALAR E CARREGAR PACOTES ##############################

# Verificar se o pacman está instalado
# Se não estiver, instalá-lo
if (!require("pacman")) install.packages("pacman")
p_load(psych)        # Carregar pacote psych
p_depends(psych)     # Verificar dependências do psych
p_load(GPArotation)  # Instalar dependência

# Carregar pacote com conjunto de dados internos
library(datasets)

# CARREGAR DADOS ###########################################

# Ler big5.csv na área de trabalho
big5 <- read.csv("~/Desktop/big5.csv", header = T) 
colnames(big5)         # Obter nome das colunas
boxplot(big5[, 9:58])  # Diagr. de caixa das variáveis Big 5
b5 <- big5[, 9:58]     # Salvar apenas variáveis do Big 5

# ANÁLISE DE COMPONENTES PRINCIPAIS ########################

# Realizar PCA usando a função "principal" do "psych"
# Primeira PCA sem rotação, especificar 5 fatores
pc0 <- principal(b5, nfactors = 5)
# Verificar resultados
pc0

# Segunda PCA com rotação oblimin (oblíqua)
pc1 <- principal(b5, nfactors = 5, rotate = "oblimin")
# Verificar resultados
pc1

# INSPECIONAR RESULTADOS ###################################

# Gerar gráfico da posição das variáveis nos componentes
plot(pc1)

# LIMPAR ###################################################

# Limpar espaço de trabalho
rm(list = ls()) 

# Limpar pacotes
pacman::p_unload(psych, GPArotation)
detach("package:datasets", unload = TRUE)

# Limpar gráficos
dev.off() 

# Limpar console
cat("\014")  # CTRL+L

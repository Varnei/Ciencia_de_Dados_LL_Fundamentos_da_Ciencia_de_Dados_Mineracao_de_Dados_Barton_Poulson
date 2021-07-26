# DM_02_03.R

# INSTALAR E CARREGAR PACOTES ##############################

# Verificar se o pacman está instalado
# Se não estiver, instalá-lo
if (!require("pacman")) install.packages("pacman")
p_load(psych)        # Carregar pacote psych
p_depends(psych)     # Verificar dependências do psych
p_load(GPArotation)  # Instalar dependência

# CARREGAR DADOS ###########################################

# Ler big5.csv na área de trabalho
b5 <- read.csv("~/Desktop/b5.csv", header = T) 
colnames(b5)  # Obter nome das colunas
boxplot(b5)   # Diagramas de caixa das variáveis do Big 5

# ANÁLISE DE COMPONENTES PRINCIPAIS ########################

# Pode-se usar "prcomp" ou "princomp", mas prefiro fazer PCA
# com a função "principal" do pacote "psych".

# Primeira PCA sem rotação, especificar 5 fatores
pc0 <- principal(b5, nfactors = 5)
pc0  # Verificar resultados

# Segunda PCA com rotação oblimin (oblíqua)
pc1 <- principal(b5, nfactors = 5, rotate = "oblimin")
pc1  # Verificar resultados

# INSPECIONAR RESULTADOS ###################################

# Gerar gráfico da posição das variáveis nos componentes
plot(pc1)

# LIMPAR ###################################################

# Limpar espaço de trabalho
rm(list = ls()) 

# Limpar pacotes
p_unload(psych, GPArotation)

# Limpar gráficos
dev.off() 

# Limpar console
cat("\014")  # CTRL+L

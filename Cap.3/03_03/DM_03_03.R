# DM_03_03.R

# CARREGAR DADOS ###########################################

# Ler CSV
states <- read.csv("~/Desktop/ClusterData.csv", header = T)
colnames(states)

# Salvar apenas dados numéricos
st <- states[, 3:27]
row.names(st) <- states[, 2]
colnames(st)

# Apenas dados de pesquisas sobre esportes
sports <- st[, 8:11]
head(sports)

# AGRUPAMENTO ##############################################

# Criar matriz de distâncias
d <- dist(st)

# Agrupamento hierárquico
c <- hclust(d)
c # Informações sobre o agrupamento

# Gerar dendrograma dos agrupamentos
plot(c, main = "Cluster with All Searches and Personality")

# Ou aninhar comandos em uma linha (para dados esportivos)
plot(hclust(dist(sports)), main = "Sports Searches")

# LIMPAR ###################################################

# Limpar espaço de trabalho
rm(list = ls()) 

# Limpar gráficos
dev.off()

# Limpar console
cat("\014")  # CTRL+L

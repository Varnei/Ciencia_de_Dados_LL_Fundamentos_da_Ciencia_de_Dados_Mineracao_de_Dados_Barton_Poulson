# DM_02_02_AlgorithmsForDataReduction

# CARREGAR PACOTES #########################################

pacman::p_load(pcaMethods)
library(datasets)

# CRIAR DADOS ##############################################

idata <- iris[, 1:4]
nlpca(idata)

# LIMPAR ###################################################

# Limpar ambiente
rm(list = ls()) 

# Limpar pacotes
pacman::p_unload(pcaMethods)
detach("package:datasets", unload = TRUE)  # Para base

# Limpar gráficos
dev.off()  # Somente se HOUVER algum gráfico

# Limpar console
cat("\014")  # CTRL+L

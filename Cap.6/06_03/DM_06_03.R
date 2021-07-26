# DM_06_03.R

# INSTALAR E CARREGAR PACOTES ##############################

pacman::p_load(arules, arulesViz) 

# DADOS ####################################################

# Ler dados transacionais do pacote arules
data("Groceries")   # Carregar dados
?Groceries          # Ajuda sobre os dados
str(Groceries)      # Estrutura dos dados
summary(Groceries)  # Inclui os 5 itens mais frequentes

# REGRAS ###################################################

# Configurar suporte mínimo (minSup) como 0,001
# Configurar confiança mínima (minConf) como 0,75

rules <- apriori(Groceries, 
           parameter = list(supp = 0.001, conf = 0.75))

options(digits=2)
inspect(rules[1:10])

# GRÁFICOS #################################################

# Diagrama de dispersão de suporte x confiança
# (colorido pela estatística lift, ou elevação)
plot(rules)

# Gráfico das 20 principais regras
plot(rules[1:20], 
  method = "graph", 
  control = list(type = "items"))

# Gráfico de coordenadas paralelas das 20 principais regras
plot(rules[1:20], 
  method = "paracoord", 
  control = list(reorder = TRUE))

# Gráfico matricial de antecedentes e consequentes
plot(rules[1:20], 
     method = "matrix", 
     control = list(reorder = TRUE))

# Gráfico matricial agrupado de antecedentes e consequentes
plot(rules[1:20], method = "grouped")

# LIMPAR ###################################################

# Limpar espaço de trabalho
rm(list = ls()) 

# Limpar pacotes
pacman::p_unload(arules, arulesViz)

# Limpar gráficos
dev.off()

# Limpar console
cat("\014")  # CTRL+L

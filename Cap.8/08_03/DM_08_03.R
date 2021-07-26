# DM_08_03.R

# INSTALAR E CARREGAR PACOTES ##############################

pacman::p_load(pacman, depmixS4) 

# CARREGAR E EXAMINAR DADOS ################################

# Usaremos o conjunto de dados de amostra "speed"
# do depmixS4
data(speed)
str(speed)

# Gerar gráfico com os dados
plot(ts(speed[, 1:3]), main = "speed data")

# MODELAR DADOS ############################################

# Comparar modelos com diferentes números de estados ocultos

# Modelo 1:
# Resposta gaussiana-binomial combinada com 1 estado 
model1 <- depmix(list(rt ~ 1, corr ~ 1), 
            data = speed, 
            nstates = 1,
            family = list(gaussian(), 
              multinomial("identity")))
fm1 <- fit(model1, verbose = FALSE)

# Modelo 2: HMM com 2 estados e Pacc como covariável
model2 <- depmix(list(rt ~ 1, corr ~ 1), 
            data = speed, 
            nstates = 2,
            family = list(gaussian(), 
              multinomial("identity")), 
            transition = ~ scale(Pacc),
              ntimes=c(168, 134, 137))
fm2 <- fit(model2, verbose = FALSE)

# Modelo 3: HMM com 3 estados e Pacc como covariável 
model3 <- depmix(list(rt ~ 1,corr ~ 1), 
            data = speed, 
            nstates = 3,
            family = list(gaussian(), 
              multinomial("identity")), 
            transition = ~ scale(Pacc),
              ntimes=c(168, 134, 137))
fm3 <- fit(model3, verbose = FALSE)

# COMPARAR MODELOS #########################################

# Deseja-se o menor BIC (critério de informação bayesiano)
plot(1:3, c(BIC(fm1), BIC(fm2), BIC(fm3)),
  ty = "b", xlab = "Model", ylab = "BIC")

# LIMPAR ###################################################

# Limpar espaço de trabalho
rm(list = ls()) 

# Limpar pacotes
p_unload(all)  # Para complementos

# Limpar gráficos
dev.off()  # Somente se HOUVER algum gráfico

# Limpar console
cat("\014")  # CTRL+L

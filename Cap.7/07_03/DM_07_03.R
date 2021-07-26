# DM_07_03.R

# INSTALAR E CARREGAR PACOTES ##############################

pacman::p_load(lars, caret) # Importando bibliotecas

# DADOS ####################################################

# Importar os dados
data = read.csv("~/Desktop/winequality-red.csv")

# Definir grupos de variáveis
x <- as.matrix(data[-12])
y <- data[, 12]

# Seleção regressiva de características com o algoritmo de 
# eliminação recursiva de características (RFE, um método
# incorporado comumente usado com máquinas
# de vetores de suporte)
ctrl <- rfeControl(method = "repeatedcv",
          repeats = 5,
          verbose = TRUE,
          functions = lmFuncs)

# Isso demora um pouco.
rfe <- rfe(x, y ,
         sizes = c(1:11),
         rfeControl = ctrl)

# Ver resultados
rfe

# Manter características identificadas pela RFE
x <- as.matrix(data[rfe$optVariables])

# MODELOS ADICIONAIS #######################################

# Regressão stepwise convencional
stepwise <- lars(x,y, type = "stepwise")

# Stagewise: como a stepwise, mas com melhor generalização
forward <- lars(x,y, type = "forward.stagewise")

# LAR: Least Angle Regression, ou regressão de ângulo mínimo
lar <- lars(x,y, type= "lar")

# LASSO: Least Absolute Shrinkage and Selection Operator,
# ou mínimos absolutos reduzidos e operador de seleção
lasso <- lars(x, y, type = "lasso")

# Comparação de modelos
r2comp <- c(stepwise$R2[6], forward$R2[6], 
            lar$R2[6], lasso$R2[6]) 
names(r2comp) <- c("stepwise", "forward", "lar", "lasso") 
r2comp 

# LIMPAR ###################################################

# Limpar espaço de trabalho
rm(list = ls()) 

# Limpar pacotes
pacman::p_unload(lars, caret)

# Limpar console
cat("\014")  # CTRL+L

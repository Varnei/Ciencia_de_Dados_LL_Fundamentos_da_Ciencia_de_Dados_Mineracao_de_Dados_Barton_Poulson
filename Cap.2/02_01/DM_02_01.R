# DM_02_01_GoalsOfDataReduction

# CARREGAR PACOTES #########################################

install.packages("pacman")  # Instalar pacman se necessário

# Carregar/instalar rgl para gráf. interativos 3D c/ OpenGL;
# no Mac é carregado no X11/XQuartz 

pacman::p_load(rgl)

# CRIAR DADOS ##############################################

x <- runif(100, 0, 100)     # 100 pontos da distrib uniforme
y <- runif(100, 0, 80)      # O mesmo para os valores de y
z <- y + runif(100, 0, 20)  # Somar valores aleat p/ obter z
plot(x, y)
plot(x, z)
plot(y, z)

# Projeção de sombra com menos dimensões
plot3d(x, y, z)

# LIMPAR ###################################################

# Limpar ambiente
rm(list = ls()) 

# Limpar pacotes
pacman::p_unload(rgl)

# Limpar gráficos
dev.off()  # Somente se HOUVER algum gráfico

# Limpar console
cat("\014")  # CTRL+L

# DM_05_01_GoalsOfAnomalyDetection

library("ggplot2")

# ======================================================================
# Figure 02.02. A Better Bar Chart
# ======================================================================

data1 <- read.table(
  header = TRUE, # A primeira linha é o cabeçalho
  # Não há comentários nos dados
  text = '
     Level  Percent
     "No High School"    11
     "Some High School"  14
     "High School Grad"  30
     "Some College"      19
     "Associates Degree"  6
     "Bachelors Degree"  13
     "Grad Degree"        6
     "PhD"                2
  ')
data1$Level <- factor(data1$Level,
                      levels = c("No High School",
                                 "Some High School",
                                 "High School Grad",
                                 "Some College",
                                 "Associates Degree",
                                 "Bachelors Degree",
                                 "Grad Degree",
                                 "PhD"))
data1  # verificar dados

ggplot(data = data1, aes(x = Level, y = Percent)) + 
  geom_bar(stat = "identity", fill = "#1D76B5") +
  coord_flip() +
  ylab("Percent of US Adults") +
  theme(axis.title.y = element_blank()) +  # Sem título no eixo X
  theme(text = element_text(size = 24)) +
  theme(legend.position = "none")

ggsave("Figure 02.02. A Better Bar Chart.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.09. Boxplot.png
# ======================================================================

# Como nota, salve gráficos como 1165 x 500, pois esta é a proporção
# das imagens do modelo de PowerPoint que estou usando
# Para salvar, usar width = 13.5, height = 6, dpi = 200
# Mas para livro usar 12 x 6 @ 300

# Paleta de cores:
# - Vermelho:           firebrick3 (#136)
# - Branco:             white      (#1)
# - Preto:              black      (#24)
# - Castanho-amarelado: wheat      (#646)


# Este é um diagrama em caixa básico, para dados normais
x1 <- rnorm(500)     # Normal
x2 <- rchisq(100, 2) # Assimétrica
# Versão padrão do R
boxplot(x1, horizontal = TRUE)

# Diagrama em caixa ggplot2 para uma variável
ggplot(NULL, aes(x = 1, y = x2)) +
  geom_boxplot(fill = "firebrick3", outlier.colour = "firebrick3",
               size = 1, outlier.size = 5) +
  coord_flip() +
  theme(axis.title.x = element_blank()) +  # Sem título no eixo X
  theme(axis.title.y = element_blank()) +  # Sem título no eixo Y
  theme(axis.text.x = element_blank()) +   # Sem marcações/rótulos no eixo X
  theme(axis.text.y = element_blank())     # Sem marcações/rótulos no eixo Y

# Nos títulos também: usar este
ggplot(NULL, aes(x = 1, y = x2)) +
  geom_boxplot(fill = "firebrick3", outlier.colour = "firebrick3",
               size = 1, outlier.size = 5) +
  coord_flip() +
  theme(axis.title.y = element_blank()) +  # Sem título no eixo X
  theme(axis.text.y = element_blank()) +   # Sem marcações/rótulos no eixo X
  ylab("Scores on Outcome Variable") +  # Título no eixo Y
  theme(text = element_text(size = 24))

# Este comando salva como PNG com 300 DPI
ggsave("Figure 02.09. Boxplot.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.10. Boxplot Anatomy.png
# ======================================================================

x2 <- rchisq(100, 2) # Dados assimétricos

# Nos títulos também: usar este
ggplot(NULL, aes(x = 1, y = x2)) +
  geom_boxplot(fill = "#E38942",
               outlier.colour = "#E38942",
               size = 1,
               outlier.size = 5) +
  coord_flip() +
  theme(axis.title.y = element_blank()) +  # Sem título no eixo X
  theme(axis.text.y = element_blank()) +   # Sem marcações/rótulos no eixo X
  ylab("Scores on Outcome Variable") +  # Título no eixo Y
  theme(text = element_text(size = 24))

# Este comando salva como PNG com 300 DPI
ggsave("Figure 02.09. Boxplot.png", width = 12, height = 6, dpi = 300)

# A versão antiga não usa ggplot2
# Isso gera dados assimétricos usando distribuição uma qui-quadrado e
# acrescenta um gráfico de pontos irregulares sobre o diagrama em caixa
# Consulte http://rss.acs.unt.edu/Rdoc/library/stats/html/Chisquare.html
x2 <- rchisq(100, 2) # Qui-q mais fácil de usar: n, df, ncp
boxplot(x2, horizontal = TRUE)
stripchart(x2, add = TRUE, method = "jitter", col = "#E38942", pch = 20)

# ======================================================================
# Figure 02.11. Side-by-Side Boxplots.png
# ======================================================================

# Vários diagramas em caixa
mbdf <- data.frame(group = rep(c(1:5), each = 200), value = rnorm(1000))
str(mbdf)
ggplot(mbdf, aes(factor(group), value)) +
  geom_boxplot(fill = "firebrick3", outlier.colour = "firebrick3",
               size = 1, outlier.size = 3) +
  coord_flip() +
  theme(axis.title.y = element_blank()) +  # Sem título no eixo X
  theme(axis.text.y = element_blank()) +   # Sem marcações/rótulos no eixo X
  ylab("Scores on Outcome Variable") +  # Título no eixo Y
  theme(text = element_text(size = 24))

# Este comando salva como PNG com 300 DPI
ggsave("Figure 02.11. Side-by-Side Boxplots.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.12. Bell Curve.png
# ======================================================================

# FORMATO
# Traçar distribuição normal padrão usando ggplot2 (apenas a linha)
p <- ggplot(data.frame(x = c(-3, 3)), aes(x = x))
p + stat_function(fun = dnorm, size = 3, colour = "firebrick3") +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Relative Frequency") +
  theme(text = element_text(size = 24))

ggsave("Figure 02.12. Bell Curve.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.13. Histogram of Normal Distribution.png
# ======================================================================

# Traçar distribuição normal padrão usando ggplot2 (histograma)
xnorm <- rnorm(10000)
ggplot(NULL, aes(x = xnorm)) + 
  geom_histogram(binwidth = 0.5, fill = "firebrick3", colour = "white") +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Frequency") +
  theme(text = element_text(size = 24))

ggsave("Figure 02.13. Histogram of Normal Distribution.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# Ou traçar distribuição normal usando métodos padrão
x <- seq(-4, 4, length=200)
# y <- 1/sqrt(2*pi)*exp(-x^2/2) # Método manual
y1 <- dnorm(x) # Função interna usando padrões
# Ou especificar valores mas verificar amplitude no gráfico
# y1 <- dnorm(x, mean=100, sd=15) Pode-se especificar M e DP
# Usar lwd = 2 para gráfico pequeno, lwd = 5 para grande
plot(x, y1, type="l", lwd=5, col="red")
plot(x, y1, type="l", lwd=5, col="red", axes = F,
     xlab = NULL, ylab = NULL, frame.plot = F)

# ======================================================================
# Figure 02.14. Uniform Distribution.png
# ======================================================================

# Distribuição uniforme
x2 <- runif(10000, min = -4, max = 4)
ggplot(NULL, aes(x = x2)) + 
  geom_histogram(binwidth = 0.5,
                 fill = "firebrick3",
                 colour = "white") +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Frequency") +
  theme(text = element_text(size = 24))
ggsave("Figure 02.14. Uniform Distribution.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.15. U-Shaped Distribution.png
# ======================================================================

# Distribuição em forma de U
# Pode-se usar distribuição beta com alfa = beta = 0,5
# Consulte http://en.wikipedia.org/wiki/Beta_distribution
x3 <- rbeta(5000, 0.5, 0.5)

# Histograma de distribuição em U usando ggplot2 proper
# Usar NULL porque não vou usar um quadro de dados, apenas um vetor
ggplot(NULL, aes(x = x3)) + 
  geom_histogram(binwidth = 0.05,
                 fill = "firebrick3",
                 colour = "white") +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Frequency") +
  theme(text = element_text(size = 24))
ggsave("Figure 02.15. U-Shaped Distribution.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# MODAS
# Distribuições unimodais

# ======================================================================
# Figure 02.16. Unimodal Distribution.png
# ======================================================================

# Isto é apenas uma repetição da Figura 02.13
# Traçar distribuição normal padrão usando ggplot2 (histograma)
xnorm <- rnorm(10000)
ggplot(NULL, aes(x = xnorm)) + 
  geom_histogram(binwidth = 0.5,
                 fill = "firebrick3",
                 colour = "white") +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Frequency") +
  theme(text = element_text(size = 24))

ggsave("Figure 02.16. Unimodal Distribution.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.17. Bimodal Distribution.png
# ======================================================================

library("reshape2")
# - Distribuições individuais
nd1 <- c(rnorm(1000, mean = 100, sd = 10), 150, 150, 150, 150, 145, 160, 149)
nd2 <- 

# - Combinação em quadros de dados, dois por vez
# 1 e 2
df1   <- data.frame(nd1)
# melt12 <- melt(df12)
hist(df1)
ggplot(df1, aes(x = value)) + 
  geom_histogram(binwidth = 5, fill = "firebrick3", colour = "white") +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Frequency") +
  theme(text = element_text(size = 24))

ggsave("Figure 02.17. Bimodal Distribution.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.18. Bimodal Distributions as Separate Distributions.png
# ======================================================================

# 1 e 3 com curvas sobrepostas para cada distribuição
ggplot(melt12, aes(x = value)) + 
  geom_histogram(aes(y = ..density.. * 2),
                 binwidth = 5,
                 fill = "gray",
                 colour = "white") +
  stat_function(fun = dnorm,
                colour = "firebrick3",
                size = 1.5,
                arg = list(mean = 100, sd = 10)) +
  stat_function(fun = dnorm,
                colour = "blue",
                size = 1.5,
                arg = list(mean = 150, sd = 15)) +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Relative Frequency") +
  theme(text = element_text(size = 24))

ggsave(paste("Figure 02.18. Bimodal Distributions as ",
             "Separate Distributions.png"),
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.19. Multimodal Distribution.png
# ======================================================================

# - Combinação dos três
df123   <- data.frame(nd1, nd2, nd3)
melt123 <- melt(df123)
hist(melt123$value)
ggplot(melt123, aes(x = value)) + 
  geom_histogram(binwidth = 5, fill = "firebrick3", colour = "white") +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Relative Frequency") +
  theme(text = element_text(size = 24))

ggsave("Figure 02.19. Multimodal Distribution.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.20. Multimodal Distributions as Combined Distributions.png
# ======================================================================

# Os 3 com curvas sobrepostas para cada distribuição
ggplot(melt123, aes(x = value)) + 
  geom_histogram(aes(y = ..density.. * 3),
                 binwidth = 5, fill = "gray",
                 colour = "white") +
  stat_function(fun = dnorm,
                colour = "firebrick3",
                size = 1.5,
                arg = list(mean = 100, sd = 10)) +
  stat_function(fun = dnorm,
                colour = "blue",
                size = 1.5,
                arg = list(mean = 150, sd = 15)) +
  stat_function(fun = dnorm, 
                colour = "green4", 
                size = 1.5, 
                arg = list(mean = 250, sd = 30)) +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Relative Frequency") +
  theme(text = element_text(size = 24))

ggsave(paste("Figure 02.20. Multimodal Distributions as ",
             "Combined Distributions.png"),
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.21. Positively Skewed Distribution.png
# ======================================================================

# ASSIMETRIA
# Gerar dados assimétricos com distribuições Beta
# Assimétrica à direita
skewright <- rbeta(10000, 1.5, 8)

ggplot(NULL, aes(x = skewright)) + 
  geom_histogram(binwidth = 0.025,
                 fill = "#1D76B5",
                 colour = "white") +
  # geom_point(aes(x = .8, y = 0), size = 10, color = "#E38942") +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Frequency") +
  theme(text = element_text(size = 24))

ggsave("Figure 02.21. Positively Skewed Distribution.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 11.01 Scatterplot from Google Correlate.png
# ======================================================================

google <- read.csv("~/Desktop/google.csv", header = T)

ggplot(google, aes(modern.dance, nba)) + 
  # Linha de regressão
  geom_smooth(method="lm",
              color = "gray70",
              size = 2,
              fill=NA,
              fullrange=TRUE) + 
  # Pontos
  geom_point(size=5, 
             color="#1D76B5", 
             fill="#1D76B5", 
             shape=21) +
  # Opções
  xlab("Searches for \"modern dance\"") +
  ylab("Searches for \"NBA\"") +
  theme(text = element_text(size = 24))

# ======================================================================
# Figure 02.22. Negatively Skewed Distribution.png
# ======================================================================

# Assimétrica à esquerda
skewleft <- rbeta(10000, 8, 1.5)
ggplot(NULL, aes(x = skewleft)) + 
  geom_histogram(binwidth = 0.025,
                 fill = "firebrick3",
                 colour = "white") +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Frequency") +
  theme(text = element_text(size = 24))

ggsave("Figure 02.22. Negatively Skewed Distribution.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.23. Platykurtic Distribution.png
# ======================================================================

# A Figura 02.23. Mesokurtic Distribution (Distribuição mesocúrtica) 
# é uma cópia da distribuição normal.
library("psych")
library("reshape2")

# Platicúrtica
# - Distribuições individuais
pk1 <- rnorm(10000, mean = 0, sd = 1)
pk2 <- rnorm(10000, mean = 2, sd = 1)
pk3 <- rnorm(10000, mean = 4, sd = 1)
pk4 <- rnorm(10000, mean = 6, sd = 1)
pk5 <- rnorm(10000, mean = 8, sd = 1)
# - Combinação em quadros de dados, dois por vez
pkdf   <- data.frame(pk1, pk2, pk3, pk4, pk5)
meltpk <- melt(pkdf)

ggplot(meltpk, aes(x = value)) + 
  geom_histogram(binwidth = 1, fill = "firebrick3", colour = "white") +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Frequency") +
  theme(text = element_text(size = 24))

ggsave("Figure 02.23. Platykurtic Distribution.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 02.25. Leptokurtic Distribution.png
# ======================================================================

# Leptocúrtica
# - Distribuições individuais
k1 <- rnorm(10000, mean = 0, sd = 1)
k2 <- rnorm(10000, mean = 0, sd = 10)
# - Combinação em quadros de dados, dois por vez
kdf1   <- data.frame(k1, k2)
meltk1 <- melt(kdf1)

ggplot(meltk1, aes(x = value)) + 
  geom_histogram(binwidth = 2, fill = "firebrick3", colour = "white") +
  xlab("Scores on Outcome Variable") +  # Título no eixo Y
  ylab("Frequency") +
  theme(text = element_text(size = 24))

ggsave("Figure 02.25. Leptokurtic Distribution.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)


# Curvas em forma de sino do Capítulo 03
p <- ggplot(data.frame(x = c(-4, 4)), aes(x = x))
p + stat_function(fun = dnorm, colour = "firebrick3",
size = 1.5, arg = list(mean = -1, sd = 1)) +
  stat_function(fun = dnorm, colour = "blue", size = 1.5,
arg = list(mean = 1, sd = 1)) +
  theme(axis.title.x = element_blank()) +  # Sem título no eixo X
  theme(axis.title.y = element_blank()) +  # Sem título no eixo Y
  theme(axis.text.x = element_blank()) +   # Sem marcações/rótulos eixo Y
  theme(axis.text.y = element_blank())     # Sem marcações/rótulos eixo Y
ggsave("twoBellCurves.png", width = 13.5, height = 6, dpi = 200)

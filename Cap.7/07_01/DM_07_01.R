# Introduction to Data Analysis, Ch 11: Correlation and Regression

# Para PowerPoint, salve os gráficos como 1165 x 500, usando
# width = 13.5, height = 6, dpi = 200.
# Mas para livros use 12 x 6 @ 300

# Paleta de cores:
# - Vermelho:           firebrick3 (#136)
# - Branco:             white      (#1)
# - Preto:              black      (#24)
# - Castanho-amarelado: wheat      (#646)

# Use o ggplot2 sempre que possível
library("ggplot2")

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
             color="firebrick3", 
             fill="firebrick3", 
             shape=21) +
  # Opções
  annotate("text", x = 4.922, y = 2.15, label = "Utah",
           color = "black", size = 8) +
  xlab("Searches for \"modern dance\"") +
  ylab("Searches for \"NBA\"") +
  theme(text = element_text(size = 24))

ggsave("Figure 11.01 Scatterplot from Google Correlate.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 11.02 Correlations for Scatterplots.png
# ======================================================================

require("MASS")

# 1: r = 1,00
sigma1 <- matrix(c(1, 1, 1, 1), 2, 2)
scatter1 <- as.data.frame(mvrnorm(n = 100, mu = rep(0, 2), sigma1))

# 2. r = 0,75
sigma2 <- matrix(c(1, .75, .75, 1), 2, 2)
scatter2 <- as.data.frame(mvrnorm(n = 100, mu = rep(0, 2), sigma2))

# 3. r = 0,50
sigma3 <- matrix(c(1, .5, .5, 1), 2, 2)
scatter3 <- as.data.frame(mvrnorm(n = 100, mu = rep(0, 2), sigma3))

# 4. r = 0,25
sigma4 <- matrix(c(1, .25, .25, 1), 2, 2)
scatter4 <- as.data.frame(mvrnorm(n = 100, mu = rep(0, 2), sigma4))

# 5. r = 0,00
sigma5 <- matrix(c(1, 0, 0, 1), 2, 2)
scatter5 <- as.data.frame(mvrnorm(n = 100, mu = rep(0, 2), sigma5))

# 6. r = -0,25
sigma6 <- matrix(c(1, -.25, -.25, 1), 2, 2)
scatter6 <- as.data.frame(mvrnorm(n = 100, mu = rep(0, 2), sigma6))

# 7. r = -0,50
sigma7 <- matrix(c(1, -.5, -.5, 1), 2, 2)
scatter7 <- as.data.frame(mvrnorm(n = 100, mu = rep(0, 2), sigma7))

# 8. r = -0,75
sigma8 <- matrix(c(1, -.75, -.75, 1), 2, 2)
scatter8 <- as.data.frame(mvrnorm(n = 100, mu = rep(0, 2), sigma8))

# 9. r = -1,00
sigma9 <- matrix(c(1, -1, -1, 1), 2, 2)
scatter9 <- as.data.frame(mvrnorm(n = 100, mu = rep(0, 2), sigma9))

# Agora combinar quadros de dados
# Combinar quadros de dados com cbind()
sps <- cbind(scatter1, scatter2, scatter3,
             scatter4, scatter5, scatter6,
             scatter7, scatter8, scatter9)

# Esvaziar quadro de dados
sp <- data.frame()  # sp = "scatterplot"
# scatterplot = diagrama de dispersão


# Preencher quadro de dados sp
rnames <- c("r = +1.00", "r = +0.75", "r = +0.50",
           "r = +0.25", "r = 0.00", "r = -0.25",
           "r = -0.50", "r = -0.75", "r = -1.00")
for(i in 1:9)
  sp <- rbind(sp, 
                data.frame(set = rnames[i], 
                           x = sps[, i * 2 - 1], 
                           y = sps[, i * 2]))

# Gerar gráfico com os dados
ggplot(sp, aes(x, y)) + 
  # Linha de regressão
  geom_smooth(method="lm",
              color = "gray70",
              size = 2,
              fill = NA,
              fullrange = TRUE) + 
  # Pontos
  geom_point(size=3, 
             color = "firebrick3", 
             fill = "firebrick3", 
             shape = 21) +
  # Definir escala
  theme(text = element_text(size = 18)) +
  # Quatro painéis
  facet_wrap(~set, ncol = 3)

ggsave("Figure 11.02 Correlations for Scatterplots.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 11.03 Anscombes Quartet.png
# ======================================================================

# Com base em https://gist.github.com/amoeba/7576126

library(ggplot2)

# Esvaziar quadro de dados
anscombe_m <- data.frame()

# Preencher quadro de dados
# ("Anscombe" é um conjunto de dados integrado)
setname <- c("Set 1", "Set 2", "Set 3", "Set 4")
for(i in 1:4)
  anscombe_m <- rbind(anscombe_m, 
                      data.frame(set = setname[i], 
                                 x = anscombe[, i], 
                                 y = anscombe[, i + 4]))

# Gerar gráfico com os dados
ggplot(anscombe_m, aes(x, y)) + 
  # Linha de regressão
  geom_smooth(method = "lm",
              color = "gray70",
              size = 2,
              fill = NA,
              fullrange = TRUE) + 
  # Pontos
  geom_point(size = 5, 
             color = "firebrick3", 
             fill = "firebrick3", 
             shape = 21) +
  # Definir escala
  scale_x_continuous(limits = c(0, 20), 
                     breaks = seq(0, 20, 5)) +
  theme(text = element_text(size = 18)) +
  # Quatro painéis
  facet_wrap(~set, ncol = 2)

ggsave("Figure 11.03 Anscombes Quartet.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 11.04. Scatterplot with Regression Line.png
# ======================================================================

ggplot(cars, aes(speed, dist)) + 
  # Linha de regressão
  geom_smooth(method = "lm",
              color = "gray70",
              size = 2,
              fill = NA,
              fullrange = TRUE) +
  # Pontos
  geom_point(size = 5, 
             color = "#E38942", 
             fill = "#E38942", 
             shape = 21) +
  # Opções
  xlab("Speed of Car [MPH]") +
  ylab("Distance to Stop [Feet]") +
  theme(text = element_text(size = 24))

ggsave("Figure 11.04. Scatterplot with Regression Line.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 11.05. Scatterplot with Residuals.png
# ======================================================================

# Gerar resíduos
carfit <- lm(dist ~ speed, data = cars)

# Salvar novo quadro de dados
cars2 <- as.data.frame(cbind(carfit$model$speed,
                             carfit$model$dist,
                             carfit$fitted.values))
colnames(cars2) <- c("Speed", "Distance", "Predicted")
cars2


ggplot(cars2, aes(Speed, Distance)) + 
  # Linhas de resíduos
  geom_segment(aes(x = Speed,
                   xend = Speed,
                   y = Predicted,
                   yend = Distance,
                   color = "#E38942", 
                   fill = "#E38942", 
                   size = 1)) +
  # Linha de regressão
  geom_smooth(method="lm",
              color = "gray70",
              size = 2,
              fill = NA,
              fullrange = TRUE) + 
  # Pontos
  geom_point(size = 5, 
             color = "#E38942", 
             fill = "#E38942", 
             shape = 21) +
  # Opções
  xlab("Speed of Car [MPH]") +
  ylab("Distance to Stop [Feet]") +
  theme(text = element_text(size = 24)) +
  theme(legend.position = "none")

ggsave("Figure 11.05. Scatterplot with Residuals.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

# ======================================================================
# Figure 11.06. Outliers in Regression.png
# ======================================================================

# Importar dados
outliers <- read.csv("~/Desktop/outliers.csv", header = T)
# Definir manualmente a ordem dos fatores para controlar
# a ordem nos painéis
outliers$Set <- factor(outliers$Set,
                       levels = c("No Outliers",
                                  "Outlier on X",
                                  "Outlier on Y",
                                  "Bivariate Outlier"))

# Gerar gráfico com os dados
ggplot(outliers, aes(X, Y)) + 
  # Linha de regressão
  geom_smooth(method = "lm",
              color = "gray70",
              size = 2,
              fill = NA,
              fullrange = TRUE) + 
  # Pontos
  scale_color_manual(values = c("gray50", "firebrick3")) +  
  scale_fill_manual(values = c("gray50", "firebrick3")) +  
  geom_point(size=5,
             shape=21,
             aes(color = factor(Outlier),
                 fill = factor(Outlier))) + 
  # Definir escala
  theme(text = element_text(size = 18)) +
  # Quatro painéis
  facet_wrap(~Set, ncol=2) +
  theme(legend.position = "none")

ggsave("Figure 11.06. Outliers in Regression.png",
#        width = 12, height = 6, dpi = 300)
       width = 13.5, height = 6, dpi = 200)

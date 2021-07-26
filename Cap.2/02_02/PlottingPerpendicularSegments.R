# Arquivo:    PlottingPerpendicularSegments.R
# Curso:  
# Capítulo: 
# Seção: 
# Autor:  Barton Poulson, datalab.cc, @bartonpoulson
# Data:    2016-05-06

# Fonte: http://j.mp/1Oh11e1
# Parte sobre resíduos: http://j.mp/1XcHAdC

# DEFINIR FUNÇÃO PARA SEGMENTOS PERPENDICULARES ############

# Função para segmentos perpendiculares
# Fonte: http://j.mp/1Oh1fSi

perp.segment.coord <- function(x0, y0, lm.mod){
  #encontra o ponto final de um segmento perpendicular
  #do ponto (x0,y0) até a linha definida por lm.mod
  #como y=a+b*x
  
  a <- coef(lm.mod)[1]  #interseção
  b <- coef(lm.mod)[2]  #inclinação
  x1 <- (x0+b*y0-a*b)/(1+b^2)
  y1 <- a + b*x1
  list(x0=x0, y0=y0, x1=x1, y1=y1)
}

# GERAR DADOS ##############################################

set.seed(1846)
x <- runif(25, 0, 100)
y <- x + rnorm(25, 0, 7)

# GERAR GRÁFICO DE DADOS COM TENDÊNCIA #####################

mod1 <- lm(y ~ x)
plot(x, y, 
     # xlim = c(min(x) - 5, max(x) + 5), 
     # ylim = c(min(y) - 10, max(y) + 10),
     xlim = c(0, 100), 
     ylim = c(0, 100),
     pch = 19, cex = 1.5)
abline(mod1, lwd = 2)

## Acrescentar linhas perpendiculares ######################

ss <- perp.segment.coord(x, y, mod1)
segments(x0 = ss$x0, x1 = ss$x1,
         y0 = ss$y0, y1 = ss$y1, 
         col = "red", lwd = 2)
# Também é possível fazer:
# do.call(segments, ss)
# mas não é possível modificar as cores, etc.
points(x, y, pch = 19, cex = 1.5)  # Plotar pontos de novo

# GERAR GRÁFICO COM DADOS APÓS REMOÇÃO DA TENDÊNCIA ########

# Gerar resíduos
y.res <- resid(mod1)

# Regressão com resíduos
# mod2 <- lm(y.res ~ x)
plot(x, y.res, 
     # xlim = c(min(x) - 5, max(x) + 5), 
     # ylim = c(min(y) - 10, max(y) + 10),
     xlim = c(0, 100), 
     ylim = c(-50, 50),
     pch = 19, cex = 1.5)
# abline(mod2, lwd = 2)
abline(h = 0, lwd = 2)

# calcular resíduos e valores previstos
# res <- signif(residuals(mod1), 5)
# pre <- predict(mod2)

# plotar distâncias entre os pontos e a linha de regressão
segments(x, y.res, 
         x, 0, 
         col = "red", lwd = 2)
points(x, y.res, pch = 19, cex = 1.5)

# LIMPAR ###################################################

# Limpar ambiente
rm(list = ls()) 

# Limpar gráficos
dev.off()  # But only if there IS a plot

# Limpar console
cat("\014")  # CTRL+L

# Esvaziar a mente :)

# DM_05_03.R

# INSTALAR E CARREGAR PACOTES ##############################

pacman::p_load(ggplot2, grid, gridExtra, robustbase) 

# DADOS ####################################################

# Importar os dados
data = read.csv("~/Desktop/AnomalyData.csv")

# Estrutura dos dados
str(data)

# Transformar variáveis em fatores
data$PsychRegions = as.factor(data$PsychRegions)
data$region = as.factor(data$region)
data$division = as.factor(data$division)

# VALORES DISCREPANTES UNIVARIADOS #########################

# Uso de diagramas em caixa para cada variável separadamente

# data.science (ciência de dados)
u01 <- qplot(data = data, y = data.science, x = 1, 
         geom = "boxplot", outlier.colour = "#E38942",
         xlim = c(0, 2), xlab=NULL, ylab = NULL, 
         main="data.science") +
       geom_text(aes(label = ifelse(data.science %in% 
         boxplot.stats(data.science)$out,
         as.character(state_code), "")), hjust = 1.5)
u01

# cluster.analysis (análise de agrupamentos)
u02 <- qplot(data = data,y = cluster.analysis, x = 1, 
         geom = "boxplot", outlier.colour = "#E38942",
         xlim = c(0, 2), xlab = NULL, ylab = NULL,
         main = "cluster.analysis") +
       geom_text(aes(label = ifelse(cluster.analysis %in% 
         boxplot.stats(cluster.analysis)$out,
         as.character(state_code), "")), hjust = 1.5)
u02

# college (faculdade)
u03 <- qplot(data = data, y = college, x = 1, 
         geom = "boxplot", outlier.colour = "#E38942",
         xlim = c(0, 2), xlab = NULL, ylab = NULL, 
         main="college") +
       geom_text(aes(label = ifelse(college %in% 
         boxplot.stats(college)$out,
         as.character(state_code), "")), hjust = 1.5)
u03

# startup
u04 <- qplot(data = data, y = startup, x = 1, 
         geom = "boxplot", outlier.colour = "#E38942",
         xlim = c(0, 2), xlab = NULL, ylab = NULL, 
         main="startup") +
       geom_text(aes(label = ifelse(startup %in% 
         boxplot.stats(startup)$out,
         as.character(state_code), "")), hjust = 1.5)
u04

# entrepreneur (empreendedor)
u05 <- qplot(data = data, y = entrepreneur, x = 1, 
         geom = "boxplot", outlier.colour = "#E38942",
         xlim = c(0, 2), xlab = NULL, ylab = NULL, 
         main="entrepreneur") +
       geom_text(aes(label = ifelse(entrepreneur %in% 
         boxplot.stats(entrepreneur)$out,
         as.character(state_code), "")), hjust = 1.5)
u05

# ceo
u06 <- qplot(data = data, y = ceo, x = 1, 
         geom = "boxplot", outlier.colour = "#E38942",
         xlim = c(0, 2), xlab = NULL, ylab = NULL, 
         main="ceo") +
       geom_text(aes(label = ifelse(ceo %in% 
         boxplot.stats(ceo)$out,
         as.character(state_code), "")), hjust = 1.5)
u06

# mortgage (hipoteca)
u07 <- qplot(data = data, y = mortgage, x = 1, 
         geom = "boxplot", outlier.colour = "#E38942",
         xlim = c(0, 2), xlab = NULL, ylab = NULL, 
         main="mortgage") +
       geom_text(aes(label = ifelse(mortgage %in% 
         boxplot.stats(mortgage)$out,
         as.character(state_code), "")), hjust = 1.5)
u07

# nba
u08 <- qplot(data = data, y = nba, x = 1, 
         geom = "boxplot", outlier.colour = "#E38942",
         xlim = c(0, 2), xlab = NULL, ylab = NULL, 
         main="nba") +
       geom_text(aes(label = ifelse(nba %in% 
         boxplot.stats(nba)$out,
         as.character(state_code), "")), hjust = 1.5)
u08

# royal.family (família real)
u09 <- qplot(data = data, y = royal.family, x = 1, 
         geom = "boxplot", outlier.colour = "#E38942",
         xlim = c(0, 2), xlab = NULL, ylab = NULL, 
         main="royal.family") +
       geom_text(aes(label = ifelse(royal.family %in% 
         boxplot.stats(royal.family)$out,
         as.character(state_code), "")), hjust = 1.5)
u09

# Neuroticism (neuroticismo)
u10 <- qplot(data = data, y = Neuroticism, x = 1, 
         geom = "boxplot", outlier.colour = "#E38942",
         xlim = c(0, 2), xlab = NULL, ylab = NULL, 
         main="Neuroticism") +
       geom_text(aes(label = ifelse(Neuroticism %in% 
         boxplot.stats(Neuroticism)$out,
         as.character(state_code), "")), hjust = 1.5)
u10

# Gerar um gráfico com as 10 juntas
grid.arrange(u01, u02, u03, u04, u05,
             u06, u07, u08, u09, u10,
             nrow = 2, 
             top = "Boxplots: Univariate outliers")

# VALORES DISCREPANTES BIVARIADOS ##########################

# data.science x cluster.analysis
# (ciência de dados x análise de agrupamentos)
b1 <- qplot(data = data, 
        x = data.science,
        y = cluster.analysis,
        main = "data.science vs cluster.analysis") +
      stat_ellipse(level = .99, color = "#E38942") +
      geom_text(aes(label =
      ifelse((data.science>1.8 | cluster.analysis>1.6),
        as.character(state_code), "")), 
        hjust = 1.5)
b1 

# mortgage x ceo (hipoteca x CEO)
b2 <- qplot(data = data,
        x = mortgage,
        y = ceo, 
        main = "mortgage vs ceo") +
      stat_ellipse(level = .99, color = "#E38942") +
      geom_text(aes(label =
        ifelse(ceo > 2,
        as.character(state_code), "")), 
        hjust = 1.5)
b2

# modern.dance vs Openness (dança moderna x abertura)
b3 <- qplot(data = data,
        x = modern.dance, 
        y = Openness,
        main = "modern.dance vs Openness") +
      stat_ellipse(level = .99,color = "#E38942") +
      geom_text(aes(label =
        ifelse((modern.dance > 2 | Openness < 30),
        as.character(state_code),"")), 
        hjust = 1.5)
b3

# fifa x nba
b4 <- qplot(data = data,
        x = fifa,
        y = nba, 
        main = "fifa vs nba") +
      stat_ellipse(level = .99, color = "#E38942") +
      geom_text(aes(label =
        ifelse(fifa > 2,
        as.character(state_code), "")), 
        hjust = 1.5)
b4

# subaru x escalade
b5 <- qplot(data = data,
        x = subaru,
        y = escalade,
        main = "subaru vs escalade") +
      stat_ellipse(level = .99, color = "#E38942") +
      geom_text(aes(label =
        ifelse(subaru > 2.5,
        as.character(state_code), "")), 
        hjust = 1.5)
b5

# unicorn x obfuscation (unicórnio x ofuscação)
b6 <- qplot(data = data,
        x = unicorn,
        y = obfuscation,
        main = "unicorn vs obfuscation") +
      stat_ellipse(level = .99, color = "#E38942") +
      geom_text(aes(label =
        ifelse((unicorn > 2 | obfuscation > 2),
        as.character(state_code), "")), 
        hjust = 1.5)
b6

# Conscientiousness x Extroversion
# (Conscienciosidade x Extroversão)
b7 <- qplot(data = data,
        x = Conscientiousness,
        y = Extraversion,
        main = "Conscientiousness vs Extraversion") +
      stat_ellipse(level = .99, color = "#E38942") 
b7

# college x royal.family (faculdade x família real)
b8 <- qplot(data = data,
        x = college,
        y = royal.family,
        main = "college vs royal.family") + 
     stat_ellipse(level = .99, color = "#E38942") 
b8

# Gerar um gráfico com as 8 juntas
grid.arrange(b1, b2, b3, b4, b5, b6, b7, b8,
  nrow = 2, top = "Bivariate outliers")

# VALORES DISCREPANTES MULTIVARIADOS #######################

# Medir a distância geral entre um caso e outro usando a
# distância de Mahalanobis e uma medida robusta de distância

# Criar conjunto de dados apenas com variáveis quantitativas
mcd = covMcd(data[-c(1, 2, 28, 29, 30)])

par(mfrow = c(1, 2))
# distância de Mahalanobis x distância robusta
plot(mcd, 
     which = "dd", 
     labels.id = as.character(data$state_code))
# Gráfico quantil-quantil com distância robusta
plot(mcd,
     which = "qqchi2",
     labels.id = as.character(data$state_code))


# LIMPAR ###################################################

# Limpar espaço de trabalho
rm(list = ls()) 

# Limpar pacotes
pacman::p_unload(ggplot2, grid, gridExtra, robustbase)

# Limpar gráficos
dev.off()  # Somente se HOUVER algum gráfico

# Limpar console
cat("\014")  # CTRL+L

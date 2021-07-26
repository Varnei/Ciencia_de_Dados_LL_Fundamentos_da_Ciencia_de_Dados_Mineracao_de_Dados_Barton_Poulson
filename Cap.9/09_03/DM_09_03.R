# DM_09_03.R

# INSTALAR E CARREGAR PACOTES ##############################

pacman::p_load(pacman, tm, SnowballC, dplyr)

# IMPORTAR DADOS ###########################################

# Não é preciso especificar o caminho do arquivo se ele 
# estiver no mesmo diretório ou pasta do script do R. 
# Além disso, os metadados já foram retirados do início
# e do fim dos arquivos de texto.

# "Jane Eyre", de Charlotte Brontë, publicado em 1847
bookJE <- readLines('JaneEyre.txt')

# "Wuthering Heights" ("O Morro dos Ventos Uivantes",
# em português), de Emily Brontë, também publicado em 1847
bookWH <- readLines('WutheringHeights.txt')

# CORPUS DE JANE EYRE ######################################

# Nota: a maioria das operações leva algum tempo.
# É importante aguardar o término antes de seguir adiante.

# Corpus preliminar
corpusJE <- Corpus(VectorSource(bookJE)) %>%
            tm_map(removePunctuation) %>%
            tm_map(removeNumbers) %>%
            tm_map(content_transformer(tolower)) %>%
            tm_map(removeWords, stopwords("english")) %>%
            tm_map(stripWhitespace) %>%
            tm_map(stemDocument)

# Criar matrizes termo-documento + eliminar termos esparsos
tdmJE <- DocumentTermMatrix(corpusJE) %>%
         removeSparseTerms(1 - (5/length(corpusJE)))

# Calcular e classificar por frequência das palavras
word.freqJE <- sort(colSums(as.matrix(tdmJE)), 
                 decreasing = T)

# Criar tabela de frequências
tableJE <- data.frame(word = names(word.freqJE), 
             absolute.frequency = word.freqJE, 
             relative.frequency = 
             word.freqJE/length(word.freqJE))

# Eliminar as palavras dos nomes das linhas
rownames(tableJE) <- NULL

# Mostrar as 10 palavras mais comuns
head(tableJE, 10)

# Exportar as 1000 palavras mais comuns para arquivos CSV
write.csv(tableJE[1:1000, ], "JE_1000.csv")

# CORPUS DE WUTHERING HEIGHTS ##############################

corpusWH <- Corpus(VectorSource(bookWH)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument)
tdmWH <- DocumentTermMatrix(corpusWH) %>%
  removeSparseTerms(1 - (5/length(corpusWH)))
word.freqWH <- sort(colSums(as.matrix(tdmWH)), 
  decreasing = T)
tableWH <- data.frame(word = names(word.freqWH), 
  absolute.frequency = word.freqWH, 
  relative.frequency = word.freqWH/length(word.freqWH))
rownames(tableWH) <- NULL
head(tableWH, 10)
write.csv(tableWH[1:1000, ], "WH_1000.csv")

# PALAVRAS MAIS DISTINTAS ##################################

# Configurar número de dígitos da saída
options(digits = 2)

# Comparar frequência relativa (por subtração)
bronte <- tableJE %>%
            merge(tableWH, by = "word") %>%
            mutate(dProp = 
              relative.frequency.x -
              relative.frequency.y,
              dAbs = abs(dProp)) %>%
           arrange(desc(dAbs)) %>%
           rename(JE.freq = absolute.frequency.x,
             JE.prop = relative.frequency.x,
             WH.freq = absolute.frequency.y,
             WH.prop = relative.frequency.y)

# Mostrar os 10 termos mais distintos
head(bronte, 10)

# Visualizar tabela com todos os resultados
View(bronte)  

# Salvar tabela completa como CSV
write.csv(bronte, "bronte_table.csv")

# LIMPAR ###################################################

# Limpar espaço de trabalho
rm(list = ls()) 

# Limpar pacotes
p_unload(pacman, tm, SnowballC, dplyr)

# Limpar console
cat("\014")  # CTRL+L

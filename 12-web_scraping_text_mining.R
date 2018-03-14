##########################################################################
# Jose Cajide - @jrcajide
# Master Data Science: web scrapping
##########################################################################

rm(list=ls()) 
cat("\014")

library(tidyverse)
library(rvest)
library(googleLanguageR)
library(stringr)


#----------------------------------------------------------------------------
# stringr
#----------------------------------------------------------------------------

"28.028 Madrid" %>% str_to_upper()
"28.028 Madrid" %>% str_split(" ")
"28.028 Madrid" %>% str_split(boundary("word"))
"28.028 Madrid" %>% str_count(boundary("word"))


"28.028 Madrid" %>% str_sub(0, 6)
"28.028 Madrid" %>% str_replace('\\.', '')
"28.028 Madrid" %>% str_sub(0, 6) %>% str_replace('\\.', '') %>% as.factor()


#----------------------------------------------------------------------------
# rvest
#----------------------------------------------------------------------------


url_madrid <- "http://resultados.elpais.com/elecciones/2011/municipales/12/28/79.html"
html_madrid <- read_html(url_madrid)

partidos <- html_madrid %>% html_nodes(".nombrePartido") %>% html_text()
concejales <- html_madrid %>% html_nodes(".tipoNumeroElectos") %>% html_text() %>% as.numeric
votos <- html_madrid %>% html_nodes(".tipoNumeroVotos") %>% html_text() %>% as.numeric

madrid <- data_frame(partidos, concejales, votos)
madrid


madrid <- html_madrid %>% html_node("#tablaVotosPartidos") %>% html_table()
names(madrid) <- c("partidos", "concejales", "votos", "porcentaje")
madrid

library(treemap)
library(viridis)

treemap(madrid, 
        index=c("partidos"), 
        vSize="votos", 
        type="index",
        border.lwds=.3,
        border.col="#FFFFFF",
        palette=viridis(15))



#----------------------------------------------------------------------------
# All together now!
#----------------------------------------------------------------------------

url_libro <- "https://www.amazon.es/Cincuenta-Sombras-Grey-L-James/product-reviews/1101910461/ref=cm_cr_getr_d_show_all?showViewpoints=1&pageNumber=1&reviewerType=all_reviews"

browseURL(url_libro)

html_libro <- url_libro %>% 
  read_html()

opiniones <- html_libro %>% 
  html_nodes(".review-text") %>% 
  html_text()

head(opiniones, 3)

# Ejercicio: obten la puntuación dada por cada usuario.
# Deberás obtener sólo las opiniones correspondientes a las anteriores opiniones. 
# Estas se encuentran dentro de una etiqueta HTML con una clase "a-icon-star" 
# que a su vez están dentro de un div con id "cm_cr-review_list"
estrellas <- html_libro %>% 
  html_nodes("#cm_cr-review_list") %>% 
  html_nodes(".a-icon-star") %>% 
  html_text() %>% 
  str_sub(0, 3) %>% 
  str_replace(',', '.') %>% 
  as.numeric()


# Modificamos la URL para simplificar. Movemos pageNumber al final 
url <- "https://www.amazon.es/Cincuenta-Sombras-Grey-L-James/product-reviews/1101910461/ref=cm_cr_getr_d_show_all?ie=UTF8&reviewerType=all_reviews&pageNumber="
num_paginas <- 30
opiniones_amazon <- NULL

for (j in 1: num_paginas){
  
  html <- read_html(paste0(url, j)) 
  
  opinion <- html %>% 
    html_nodes(".review-text") %>% 
    html_text()
  
  estrellas <- html %>% 
    html_nodes("#cm_cr-review_list") %>% 
    html_nodes(".a-icon-star") %>% 
    html_text() %>% 
    str_sub(0, 3) %>% 
    str_replace(',', '.') %>% 
    as.numeric()
  
  opiniones_amazon <- rbind(opiniones_amazon, data.frame('pagina'=j, 'opinion'=opinion, 'estrellas' = estrellas))
}

str(opiniones_amazon)

opiniones_amazon$opinion <- as.character(opiniones_amazon$opinion)

opiniones_amazon %>% as_data_frame() 

# opiniones_amazon <- read_csv('data/opiniones_amazon_50_sombras.csv')

# Calcular el sentimiento de cada opinión

gl_auth('cpb100-gcp-api-language-kschool.json')

# sentimiento <- read_rds('data/sentimiento.rds')
# head(sentimiento)
sentimiento <- lapply(opiniones_amazon$opinion, function(t) gl_nlp(t))

puntuaciones_producto <- sapply(sentimiento, function(t) t$documentSentiment$score)

opiniones_amazon$puntuacion <- puntuaciones_producto

opiniones_amazon$sentimiento <- 'Neutro'
opiniones_amazon$sentimiento <- ifelse(opiniones_amazon$puntuacion > .2, "Positivo", opiniones_amazon$sentimiento)
opiniones_amazon$sentimiento <- ifelse(opiniones_amazon$puntuacion < -.2, "Negativo", opiniones_amazon$sentimiento)

opiniones_amazon %>% arrange(desc(sentimiento)) %>% View()

#----------------------------------------------------------------------------
# Un poco de estadística
#----------------------------------------------------------------------------

cor(opiniones_amazon$estrellas, opiniones_amazon$puntuacion)

modelo <- lm(puntuacion ~ estrellas, data=opiniones_amazon)

summary( modelo )

# ¿En que opiniones hay más discordancia entre el voto del usuario y el sentimiento asignado?
# ¿Son coherentes las opiniones de los usuarios?

errores_modelo <- resid(modelo)

boxplot(errores_modelo)

errores_destacados <- boxplot(errores_modelo, plot = F)$out

names(errores_destacados)

opiniones_amazon[names(errores_destacados), ]



#----------------------------------------------------------------------------
# Scrapping + translate
#----------------------------------------------------------------------------

article_url <- "https://itb.dk/news/fremtidens-kompetencer/itu-%C3%A5bner-danmarks-f%C3%B8rste-uddannelse-i-data-science"

results <- read_html(article_url) %>% # read html
  html_node(css = ".node-content") %>%  
  html_text %>% 
  gl_translate(format = "html", target = 'es') %>% 
  dplyr::select(translatedText)
  
results 

#----------------------------------------------------------------------------
# Text mining
#----------------------------------------------------------------------------

# install.packages('tidytext')
library(tidytext)

# Diccionarios en inglés
get_sentiments("nrc")

# traduccion <- read_rds('data/traduccion.rds')
traduccion <- lapply(opiniones_amazon$opinion, function(t) gl_translate(t, target = "en")$translatedText)

traduccion[[2]]

traduccion
head(unlist(traduccion, recursive = TRUE))

opiniones_amazon$en <- unlist(traduccion)
head(opiniones_amazon)

opiniones_amazon %>% head %>% View


text_df <- opiniones_amazon %>% 
  as_data_frame() %>% 
  mutate(line = row_number(), 
         text = en) %>% 
  select(line, text)

# Tokenization
# Line number each word came from
# Punctuation has been stripped
# Converts the tokens to lowercase
text_df <- text_df %>%
  unnest_tokens(word, text)

data(stop_words)

text_df <- text_df %>%
  anti_join(stop_words)

text_df %>%
  count(word, sort = TRUE)

text_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() + 
  theme_minimal()

# Term frequency
# Ejercicio: 
comment_words <- text_df %>%
  count(word, sort = TRUE) %>%
  ungroup() %>% 
  mutate(total = sum(n))

ggplot(comment_words, aes(n/total)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.01) 

freq_by_rank <- comment_words %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)


freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`)) + 
  geom_line(size = 1, show.legend = FALSE, color="darkblue") + 
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Zipf’s law.", 
       subtitle = "The frequency that a word appears is inversely proportional to its rank") + 
  theme_minimal()

# The idea of tf-idf is to find the important words for the content of each document by decreasing the weight for commonly used words 
# and increasing the weight for words that are not used very much in a collection or corpus of documents
# Calculating tf-idf attempts to find the words that are important (i.e., common) in a text, but not too common


# tf: term frequency
# idf: inverse document frequency, which decreases the weight for commonly used words and 
#  increases the weight for words that are not used very much in a collection of documents
#  idf = ln(n docs / n docs containg the term)
# tf_idf: the frequency of a term adjusted for how rarely it is used
tf_idf <- text_df %>%
  count(word, line, sort = TRUE) %>%
  bind_tf_idf(word, line, n) %>% 
  arrange(desc(tf_idf))



tf_idf %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  head(15) %>%
  ggplot(aes(word, tf_idf)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()


# Latent Dirichlet allocation
# Every document is a mixture of topics
# Every topic is a mixture of words
# LDA is a mathematical method for estimating both of these at the same time: 
# finding the mixture of words that is associated with each topic, while also determining the mixture of topics that describes each document.
# install.packages('topicmodels')
library(topicmodels)
library(tm)
comments_dtm <- tf_idf %>%
  cast_dtm(line, word, n)

inspect(comments_dtm)

findAssocs(comments_dtm, 'book', 0.4)

comments_lda <- LDA(comments_dtm, k = 2, control = list(seed = 1234))
comments_lda

# per-topic-per-word probabilities
topics <- tidy(comments_lda, matrix = "beta")
topics


top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



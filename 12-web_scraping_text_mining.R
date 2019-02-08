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
estrellas <- 
  
  



# Ejercicio: Extraer las opiniones de las 30 primeras páginas.
# El resultado será un dataframe con  pagina, opinion, estrellas
# pagina opinion                                                                                                                          estrellas
# 1 no se el libro no es para mi así que no lo he leído.pero voy a valorar físicamente al libro, es pequeñito como de 15 cm y tiene…         4

# Modificamos la URL para simplificar. Movemos pageNumber al final 
url <- "https://www.amazon.es/Cincuenta-Sombras-Grey-L-James/product-reviews/1101910461/ref=cm_cr_getr_d_show_all?ie=UTF8&reviewerType=all_reviews&pageNumber="














# opiniones_amazon <- read_csv('data/opiniones_amazon_50_sombras.csv')

# Vamos a filtrar las que no tengan iun numero de palabras minimo

opiniones_amazon <- opiniones_amazon %>%  
  mutate(n_words = stringr::str_count(opinion, ' ') ) %>% 
  filter(n_words > 50)

# opiniones_amazon <- read_csv('data/opiniones_amazon_50_sombras.csv')


# Calcular el sentimiento de cada opinión

gl_auth('cpb100-162913-eb077c530ef4.json')

# sentimiento <- read_rds('data/sentimiento.rds')
# head(sentimiento)
sentimiento <- lapply(opiniones_amazon$opinion, function(t) gl_nlp(t))

# str(sentimiento)

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

article_url <- "https://itb.dk/maerkesager/privacy-og-sikkerhed/seks-nye-nationale-cyberstrategier-mangler-sammenhaeng/"

# Ejercicio: Extraer el texto del articlo y traducirlo a espanol.
# Usa la funcion gl_translate()

results <- 
  
  
  
  
  

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



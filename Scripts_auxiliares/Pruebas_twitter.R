#Pruebas Twitter
#extraccion datos de Twitter
library(rtweet)
library(tidyverse)
library(knitr)
library("devtools")
library("twitteR")
library("httr")
library("ROAuth")
library("tm")
library("SnowballC")

# Identificación y obtención de tokens
appname <- "Fred23"
key     <- "js00YxGt2JFv80pkUm4Sua2gV"
secret  <- "XOztfHJOQH8ZbP6kjp9r497QyWtifYjMY1q12TBkC1BA5KlmFv"
acces_t <- "296864741-fvCPDE9Hqj4xXQeFtutfj05oLoD5tWUlHECLqzf2"
access_s <- "vAlJvVZkdZvqnAhebuG7NfxpJEtXQqv4ibhpn2H11yTJT"

twitter_token <- create_token(app = appname, consumer_key = key,
                              consumer_secret = secret,
                              access_token = acces_t ,access_secret = access_s)


#extraigo data
ney <- search_tweets("Neymar",n=18000,retryonratelimit=TRUE,lang = "en")

data <- data.frame(Neymar=ney$text)

#preprocesamiento
#convierto texo del tweet a objeto corpus
corpus <- Corpus(VectorSource(data$Neymar))
length(corpus)

#ingreso a la informacion en el objeto
content(corpus[[1]])

#cambio letras mayusculas a minusculas
corpus1 <- tm_map(corpus,tolower)

#accedo a informacion del nuevo objeto
content(corpus1[[1]])[1]

#quito puntuacion
corpus1 <- tm_map(corpus1,removePunctuation)

#accedo a informacion del nuevo objeto
content(corpus1[[1]])[1]

#quito stop words
#primeras palabras
stopwords("english")[1:10]
stopwords("spanish")[1:10]

#quito stopwords
#existen problemas con caracter "’", lo quite manualmente
corpus1 <- tm_map(corpus1,removeWords,c(stopwords("english"),"Neymar","’"))

#accedo a informacion del nuevo objeto
content(corpus1[[1]])[1]

#modifico palabras a su raiz
corpus1 <- tm_map(corpus1,stemDocument)

#accedo a informacion del nuevo objeto
content(corpus1[[1]])[1]

#creo matriz de frecuencias
#creo nuevo corpus con data limpia
corpus2 <- Corpus(VectorSource(as.character(content(corpus1))))

frecuencies <- DocumentTermMatrix(corpus2)

#veo matriz
inspect(frecuencies[800:805,505:515])

#encuento frecuencia
findFreqTerms(frecuencies,lowfreq = 50)

#reduzco matriz
sparse <- removeSparseTerms(frecuencies,0.995)
sparse

#convierto matriz a dataframe
tweetsSparse <- as.data.frame(as.matrix(sparse))

colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))


#es necesario evaluar cada tweet inicial como positivo o negativo
#antes de continuar

library(tidytext)
sentimientos <- get_sentiments(lexicon = "bing")
head(sentimientos)
sentimientos <- sentimientos %>%
  mutate(valor = if_else(sentiment == "negative", -1, 1))


#usar corpus 1 o usar matriz de frecuencia pero solo el nombre de las columnas
content(corpus1[[1]])[1]

da <- as.data.frame(as.character(content(corpus1)))

for(i in 1:nrow(da)){
  da[i,1] <- as.character(da[i,1])
  
}
names(da) <- "texto"

#uso funcion tokenizar
limpiar_tokenizar(texto = da[1,1])

da1 <- da %>% mutate(texto_tokenizado = map(.x = texto,
                                                   .f = limpiar_tokenizar))

da1 %>% select(texto_tokenizado) %>% head()


#accedo a datos
da1 %>% slice(2) %>% select(texto_tokenizado) %>% pull()


#analisis exploratorio
#proceso de unnest
da2 <- da1 %>% select(-texto) %>% unnest()
da2 <- da2 %>% rename(token = texto_tokenizado)
head(da2) 

tweetsSparse$sentiment <- data$sent




#maquina de soporte vectorial
#dividir data en un 80% entrenamiento y un 20% para evaluar
library(caTools)

set.seed(12)


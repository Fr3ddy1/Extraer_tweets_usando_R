#EXTRACCION DE TWEETS
#1) USANDO API DE TWITTER
#A) BUSCANDO UN USUARIO EN ESPECIFICO
library(rtweet)


# Identificación y obtención de tokens
appname <- "Fred23"
key     <- "js00YxGt2JFv80pkUm4Sua2gV"
secret  <- "XOztfHJOQH8ZbP6kjp9r497QyWtifYjMY1q12TBkC1BA5KlmFv"
acces_t <- "296864741-fvCPDE9Hqj4xXQeFtutfj05oLoD5tWUlHECLqzf2"
access_s <- "vAlJvVZkdZvqnAhebuG7NfxpJEtXQqv4ibhpn2H11yTJT"

twitter_token <- create_token(app = appname, consumer_key = key,
                              consumer_secret = secret,
                              access_token = acces_t ,access_secret = access_s)


datos_new <- get_timeline(user = "@elonmusk", n = 3200, parse = TRUE,
                          check = TRUE, include_rts = FALSE)

datos <- datos_new[,-c(17:28,30,31,69,70,71)]

datos$text[1:5]

#B) BUSCANDO UNA PALABRA CUALQUIERA
apple <- search_tweets("apple",n=1000,type = "recent")
apple$text[1:10]

#2) USANDO GOOGLE SHEETS
tweets <- read.csv(paste0(getwd(),"/Datos/tweets.csv"))








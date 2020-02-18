#Analisis de sentimientos con R
library("devtools")
library("twitteR")
library("httr")
library("ROAuth")
library("tm")
library("SnowballC")
setwd("~/Desktop")

#setup_twitter_oauth(consumer_key = "js00YxGt2JFv80pkUm4Sua2gV",
#                    consumer_secret = "XOztfHJOQH8ZbP6kjp9r497QyWtifYjMY1q12TBkC1BA5KlmFv",
#                    access_token = "296864741-fvCPDE9Hqj4xXQeFtutfj05oLoD5tWUlHECLqzf2",
#                    access_secret = "vAlJvVZkdZvqnAhebuG7NfxpJEtXQqv4ibhpn2H11yTJT")

#apple <- searchTwitter("apple",n=50)

#leo base de datos de Tweets que baje usando google Drive
tweets <- read.csv("tweets.csv")

#tomo 1era fila para colocar nombre a columnas
a <- tweets[1,]
for(i in 1:length(a)){
  a[[i]] <- as.character(a[[i]])
}

#pongo nombre a columnas
names(tweets) <- a
tweets <- tweets[-1,]

data <- data.frame(texto=as.character(tweets$`Tweet Text`))

#creo columna de 1 y -1 
f <- rbinom(length(data$texto),1,0.5)
f[which(f==0)] <- -1

data$sent <- f 

table(data$sent)

#preprocesamiento
#convierto texo del tweet a objeto corpus
corpus <- Corpus(VectorSource(data$texto))
length(corpus)

#ingreso a la informacion en el objeto
content(corpus[[20]])

#cambio letras mayusculas a minusculas
corpus1 <- tm_map(corpus,tolower)

#realizo transformacion de data anterios
corpus1 <- tm_map(corpus1,PlainTextDocument)

#accedo a informacion del nuevo objeto
content(corpus1[[1]])[20]

#quito puntuacion
corpus1 <- tm_map(corpus1,removePunctuation)

#accedo a informacion del nuevo objeto
content(corpus1[[1]])[20]

#quito stop words
#primeras palabras
stopwords("english")[1:10]
stopwords("spanish")[1:10]

#quito stopwords
#existen problemas con caracter "’", lo quite manualmente
corpus1 <- tm_map(corpus1,removeWords,c(stopwords("english"),"apple","’"))

#accedo a informacion del nuevo objeto
content(corpus1[[1]])[20]

#modifico palabras a su raiz
corpus1 <- tm_map(corpus1,stemDocument)

#accedo a informacion del nuevo objeto
content(corpus1[[1]])[20]

#creo matriz de frecuencias
#creo nuevo corpus con data limpia
corpus2 <- Corpus(VectorSource(as.character(content(corpus1[[1]]))))

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
tweetsSparse$sentiment <- data$sent


#maquina de soporte vectorial
#dividir data en un 80% entrenamiento y un 20% para evaluar
library(caTools)

set.seed(12)

split <- sample.split(tweetsSparse$sentiment,SplitRatio = 0.8)

#creo data de entrenamiento y data de evaluacion
trainSparse <- subset(tweetsSparse,split==TRUE)
testSparse <- subset(tweetsSparse,split==FALSE)
testSparse$sentiment <- as.factor(testSparse$sentiment)

table(testSparse$sentiment)
#precision inicial, para tweets positivos
346/660

#paquete para algoritmo de Machine Learning
library(caret)
library(e1071)

#ejecuto modelo
SVM <- svm(as.factor(sentiment)~.,data = trainSparse)

summary(SVM)

#realizo predicciones
predictSVM <- predict(SVM,newdata = testSparse)

#realizo prueba de la prediccion
confusionMatrix(predictSVM,testSparse$sentiment)


#nube de palabras
library(wordcloud)

positive <- subset(tweetsSparse,tweetsSparse$sentiment==1)
positive$sentiment <- NULL

positivas <- as.data.frame(colSums(positive))
positivas$words <- row.names(positivas)
colnames(positivas) <- c("freq","word")

wordcloud(positivas$word,positivas$freq,random.order = FALSE,colors = brewer.pal(8,"Dark2"),max.words = 300)




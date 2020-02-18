#analisis de sentimientos
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


#Introduccion
#extraccion datos de Twitter
library(rtweet)
library(tidyverse)
library(knitr)

# Identificación y obtención de tokens
appname <- "Fred23"
key     <- "js00YxGt2JFv80pkUm4Sua2gV"
secret  <- "XOztfHJOQH8ZbP6kjp9r497QyWtifYjMY1q12TBkC1BA5KlmFv"
acces_t <- "296864741-fvCPDE9Hqj4xXQeFtutfj05oLoD5tWUlHECLqzf2"
access_s <- "vAlJvVZkdZvqnAhebuG7NfxpJEtXQqv4ibhpn2H11yTJT"

twitter_token <- create_token(app = appname, consumer_key = key,
                              consumer_secret = secret,
                              access_token = acces_t ,access_secret = access_s)

#funcion para extraer tweets
extraccion_tweets <- function(usuario, maxtweets = 100, output_file_name = NULL){
  # Esta función extrae los tweets publicados por un usuario y los almacena en
  # un fichero csv. Si existe un fichero con el mismo nombre, lo lee, concatena
  # los nuevos tweets y lo sobrescribe.
  #
  # Argumentos:
  #   usuario: identificador del usuario de twitter
  #   maxtweets: número de tweets que se recuperan
  #   output_file_name: nombre del fichero de escritura
  
  # Si no se especifica el nombre del archivo de almacenamiento, se crea un
  # nombre por defecto
  if(is.null(output_file_name)){
    output_file_name <- paste0("datos_tweets_", usuario, ".csv")
  }
  
  # Si no existe el fichero de almacenamiento, se crea uno nuevo con los
  # resultados de la primera recuperación
  if(!(output_file_name %in% list.files())){
    datos_new <- get_timeline(user = usuario, n = maxtweets, parse = TRUE,
                              check = TRUE, include_rts = FALSE)
    datos_new <- datos_new[,-c(17:28,30,31,69,70,71)]
    write_csv(x = datos_new, path = output_file_name, col_names = TRUE)
    print("Nuevo fichero creado")
  }else{
    # Se leen los datos antiguos
    datos_old <- read_csv(file = output_file_name)
    # Se identifica el último Id recuperado
    ultimo_id <- tail(datos_old, 1)["status_id"] %>% pull()
    # Para no recuperar de nuevo el último tweet de la consulta anterior
    # se incrementa en 1 el Id
    ultimo_id = ultimo_id + 1
    # Para que no haya errores de compatibilidad, se convierten todas las
    # columnas numéricas a character
    datos_old <- map_if(.x = datos_old, .p = is.numeric, .f = as.character)
    # Extracción de nuevos tweets
    datos_new <- get_timeline(user = usuario, n = maxtweets, max_id = ultimo_id,
                              parse = TRUE, check = TRUE, include_rts = FALSE)
    datos_new <- datos_new[,-c(17:28,30,31,69,70,71)]
    datos_new <- map_if(.x = datos_new, .p = is.numeric, .f = as.character)
    
    # Concatenación de los datos nuevos, viejos y escritura en disco
    datos_concatenados <- bind_rows(datos_old, datos_new)
    write_csv(x = datos_concatenados, path = output_file_name, col_names = TRUE)
    print(paste("Número total de tweets:", nrow(datos_concatenados)))
    print(paste("Número de tweets nuevos:", nrow(datos_new)))
  }
}


extraccion_tweets(usuario  = "@elonmusk", maxtweets  = 3200)
extraccion_tweets(usuario  = "@BillGates", maxtweets  = 5000)
extraccion_tweets(usuario  = "@mayoredlee", maxtweets  = 200)

#carga de datos
#data obtenida de Github, tienen 35 variables
tweets_elon       <- read_csv(file = "/Users/freddytapia/Documents/datos_tweets_@elonmusk.csv",
                              col_names = TRUE)
tweets_BillGates  <- read_csv(file = "/Users/freddytapia/Documents/datos_tweets_@BillGates.csv",
                              col_names = TRUE)
tweets_mayoredlee <- read_csv(file = "/Users/freddytapia/Documents/datos_tweets_@mayoredlee.csv",
                              col_names = TRUE)

#mi data donde elimine algunas columnas, tiene 90 variables
tweets_elon_1      <- read_csv(file = "datos_tweets_@elonmusk.csv",
                              col_names = TRUE)

# Se unen todos los tweets en un único dataframe
tweets <- bind_rows(tweets_elon, tweets_BillGates, tweets_mayoredlee)

tweets %>% group_by(screen_name) %>% summarise(numero_tweets = n()) 

#De entre toda la información disponible, en este análisis únicamente 
#se emplea: autor del tweet, fecha de publicación, identificador del 
#tweet y contenido.

# Selección de variables
tweets <- tweets %>% select(screen_name, created_at, status_id, text)

# Se renombran las variables con nombres más prácticos
tweets <- tweets %>% rename(autor = screen_name, fecha = created_at,
                            texto = text, tweet_id = status_id)
head(tweets)


#limpieza de texto y tokenizacion

limpiar_tokenizar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  # Tokenización por palabras individuales
  nuevo_texto <- str_split(nuevo_texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  nuevo_texto <- keep(.x = nuevo_texto, .p = function(x){str_length(x) > 1})
  return(nuevo_texto)
}

test = "Esto es 1 ejemplo de l'limpieza de6 TEXTO  https://t.co/rnHPgyhx4Z @JoaquinAmatRodrigo #textmining"
limpiar_tokenizar(texto = test)

#aplico funcion anterior a la data principal
# Se aplica la función de limpieza y tokenización a cada tweet
tweets <- tweets %>% mutate(texto_tokenizado = map(.x = texto,
                                                   .f = limpiar_tokenizar))
tweets %>% select(texto_tokenizado) %>% head()

#accedo a datos
tweets %>% slice(2) %>% select(texto_tokenizado) %>% pull()


#analisis exploratorio
#proceso de unnest
tweets_tidy <- tweets %>% select(-texto) %>% unnest()
tweets_tidy <- tweets_tidy %>% rename(token = texto_tokenizado)
head(tweets_tidy) 

#distribucion temporal de los tweets
library(lubridate)

ggplot(tweets, aes(x = as.Date(fecha), fill = autor)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "5 month") +
  labs(x = "fecha de publicación", y = "número de tweets") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

#
tweets_mes_anyo <- tweets %>% mutate(mes_anyo = format(fecha, "%Y-%m"))
tweets_mes_anyo %>% group_by(autor, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = autor)) +
  geom_line(aes(group = autor)) +
  labs(title = "Número de tweets publicados", x = "fecha de publicación",
       y = "número de tweets") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")


#frecuencia de palabras
#Total de palabras utilizadas por cada usuario
tweets_tidy %>% group_by(autor) %>% summarise(n = n()) 

tweets_tidy %>%  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw() 

#Palabras distintas utilizadas por cada usuario
tweets_tidy %>% select(autor, token) %>% distinct() %>%  group_by(autor) %>%
  summarise(palabras_distintas = n()) 

#
tweets_tidy %>% select(autor, token) %>% distinct() %>%
  ggplot(aes(x = autor)) + geom_bar() + coord_flip() + theme_bw()


#Longitud media de los tweets por usuario
tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>% 
  group_by(autor) %>% summarise(media_longitud = mean(longitud),
                                                                                                                                sd_longitud = sd(longitud))

#
tweets_tidy %>% group_by(autor, tweet_id) %>% summarise(longitud = n()) %>%                      group_by(autor) %>%
  summarise(media_longitud = mean(longitud),
            sd_longitud = sd(longitud)) %>%
  ggplot(aes(x = autor, y = media_longitud)) +
  geom_col() +
  geom_errorbar(aes(ymin = media_longitud - sd_longitud,
                    ymax = media_longitud + sd_longitud)) +
  coord_flip() + theme_bw()

#Palabras más utilizadas por usuario
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>% print(n=30)

#Stop words
lista_stopwords <- c('me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves',
                     'you','your', 'yours', 'yourself', 'yourselves', 'he', 'him','his',
                     'himself', 'she', 'her', 'hers', 'herself', 'it', 'its', 'itself',
                     'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which',
                     'who', 'whom', 'this', 'that', 'these', 'those', 'am', 'is', 'are',
                     'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had',
                     'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the', 'and',
                     'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at',
                     'by', 'for', 'with', 'about', 'against', 'between', 'into',
                     'through', 'during', 'before', 'after', 'above', 'below', 'to',
                     'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under',
                     'again', 'further', 'then', 'once', 'here', 'there', 'when',
                     'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more',
                     'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own',
                     'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will',
                     'just', 'don', 'should', 'now', 'd', 'll', 'm', 'o', 're', 've',
                     'y', 'ain', 'aren', 'couldn', 'didn', 'doesn', 'hadn', 'hasn',
                     'haven', 'isn', 'ma', 'mightn', 'mustn', 'needn', 'shan',
                     'shouldn', 'wasn', 'weren', 'won', 'wouldn','i')
# Se añade el término amp al listado de stopwords
lista_stopwords <- c(lista_stopwords, "amp")

# Se filtran las stopwords
tweets_tidy <- tweets_tidy %>% filter(!(token %in% lista_stopwords))


#Representación gráfica de las frecuencias
tweets_tidy %>% group_by(autor, token) %>% count(token) %>% group_by(autor) %>%
  top_n(10, n) %>% arrange(autor, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = autor)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~autor,scales = "free", ncol = 1, drop = TRUE)


#Word Clouds
library(wordcloud)
library(RColorBrewer)

wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 400, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}

df_grouped <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  group_by(autor) %>% mutate(frecuencia = n / n()) %>%
  arrange(autor, desc(frecuencia)) %>% nest() 

walk2(.x = df_grouped$autor, .y = df_grouped$data, .f = wordcloud_custom)


#correlacion entre usuarios por palabras utilizadas
library(gridExtra)
library(scales)

tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = NA, drop = TRUE)

cor.test(~ mayoredlee + elonmusk, method = "pearson", data = tweets_spread)

#
cor.test(~ BillGates + elonmusk, data = tweets_spread)


#
p1 <- ggplot(tweets_spread, aes(elonmusk, mayoredlee)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p2 <- ggplot(tweets_spread, aes(elonmusk, BillGates)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

grid.arrange(p1, p2, nrow = 1)

#
palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor=="elonmusk") %>%
                                       select(token), tweets_tidy %>% filter(autor=="mayoredlee") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre Elon Musk y Ed Lee", palabras_comunes)


#
palabras_comunes <- dplyr::intersect(tweets_tidy %>% filter(autor=="elonmusk") %>%
                                       select(token), tweets_tidy %>% filter(autor=="BillGates") %>%
                                       select(token)) %>% nrow()
paste("Número de palabras comunes entre Elon Musk y Bill Gates", palabras_comunes)

#comparacion en el uso de las palabras
# Pivotaje y despivotaje
tweets_spread <- tweets_tidy %>% group_by(autor, token) %>% count(token) %>%
  spread(key = autor, value = n, fill = 0, drop = TRUE)
tweets_unpivot <- tweets_spread %>% gather(key = "autor", value = "n", -token)

# Selección de los autores elonmusk y mayoredlee
tweets_unpivot <- tweets_unpivot %>% filter(autor %in% c("elonmusk",
                                                         "mayoredlee"))
# Se añade el total de palabras de cada autor
tweets_unpivot <- tweets_unpivot %>% left_join(tweets_tidy %>%
                                                 group_by(autor) %>%
                                                 summarise(N = n()),
                                               by = "autor")
# Cálculo de odds y log of odds de cada palabra
tweets_logOdds <- tweets_unpivot %>%  mutate(odds = (n + 1) / (N + 1))
tweets_logOdds <- tweets_logOdds %>% select(autor, token, odds) %>% 
  spread(key = autor, value = odds)
tweets_logOdds <- tweets_logOdds %>%  mutate(log_odds = log(elonmusk/mayoredlee),
                                             abs_log_odds = abs(log_odds))
# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de Elon Musk. Esto es así porque el ratio sea ha
# calculado como elonmusk/mayoredlee.
tweets_logOdds <- tweets_logOdds %>%
  mutate(autor_frecuente = if_else(log_odds > 0,
                                   "@elonmusk",
                                   "@mayoredlee"))
tweets_logOdds %>% arrange(desc(abs_log_odds)) %>% head() 

#Representación de las 30 palabras más diferenciadas
tweets_logOdds %>% group_by(autor_frecuente) %>% top_n(15, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = autor_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (@elonmusk / mayoredlee)") +
  coord_flip() + 
  theme_bw()


#relacion entre palabras
library(tidytext)
limpiar <- function(texto){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el texto a minúsculas
  nuevo_texto <- tolower(texto)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_texto <- str_replace_all(nuevo_texto,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_texto <- str_replace_all(nuevo_texto,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_texto <- str_replace_all(nuevo_texto,"[\\s]+", " ")
  return(nuevo_texto)
}

bigramas <- tweets %>% mutate(texto = limpiar(texto)) %>%
  select(texto) %>%
  unnest_tokens(input = texto, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
bigramas  %>% count(bigrama, sort = TRUE)

# Separación de los bigramas 
bigrams_separados <- bigramas %>% separate(bigrama, c("palabra1", "palabra2"),
                                           sep = " ")
head(bigrams_separados)

# Filtrado de los bigramas que contienen alguna stopword
bigrams_separados <- bigrams_separados  %>%
  filter(!palabra1 %in% lista_stopwords) %>%
  filter(!palabra2 %in% lista_stopwords)

# Unión de las palabras para formar de nuevo los bigramas
bigramas <- bigrams_separados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Nuevo contaje para identificar los bigramas más frecuentes
bigramas  %>% count(bigrama, sort = TRUE) %>% print(n = 20)

#
library(igraph)
library(ggraph)
graph <- bigramas %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>% 
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 18) %>% graph_from_data_frame(directed = FALSE)
set.seed(123)

plot(graph, vertex.label.font = 2,
     vertex.label.color = "black",
     vertex.label.cex = 0.7, edge.color = "gray85")


#
ggraph(graph = graph) +
geom_edge_link(colour = "gray70")  +
  geom_node_text(aes(label = name), size = 4) +
  theme_bw()


#term frequency
# Número de veces que aparece cada término por tweet
tweets_tf <- tweets_tidy %>% group_by(tweet_id, token) %>% summarise(n = n())

# Se añade una columna con el total de términos por tweet
tweets_tf <- tweets_tf %>% mutate(total_n = sum(n))

# Se calcula el tf
tweets_tf <- tweets_tf %>% mutate(tf = n / total_n )
head(tweets_tf)

#Inverse Document Frequency
total_documentos = tweets_tidy$tweet_id %>% unique() %>% length()
total_documentos

# Número de documentos en los que aparece cada término
tweets_idf <- tweets_tidy %>% distinct(token, tweet_id) %>% group_by(token) %>%
  summarise(n_documentos = n())

# Cálculo del idf
tweets_idf <- tweets_idf %>% mutate(idf = n_documentos/ total_documentos) %>%
  arrange(desc(idf))
head(tweets_idf)

#Term Frequency - Inverse Document Frequency

tweets_tf_idf <- left_join(x = tweets_tf, y = tweets_idf, by = "token") %>% ungroup()
tweets_tf_idf <- tweets_tf_idf %>% mutate(tf_idf = tf * idf)
tweets_tf_idf %>% select(-tweet_id) %>% head() %>% kable()

#clasificacion de tweets
#Separación de los datos en entrenamiento y test
tweets_elon_ed <- tweets %>% filter(autor %in% c("elonmusk", "mayoredlee"))
set.seed(123)
train <- sample(x = 1:nrow(tweets_elon_ed), size = 0.8 * nrow(tweets_elon_ed))
tweets_train <- tweets_elon_ed[train, ]
tweets_test  <- tweets_elon_ed[-train, ]

#
table(tweets_train$autor) / length(tweets_train$autor)
table(tweets_test$autor) / length(tweets_test$autor)

#Vectorización tf-idf
library(quanteda)
texto <- paste0("Esto es 1 ejemplo de l'limpieza de6 TEXTO",
                "https://t.co/rnHPgyhx4Z @JoaquinAmatRodrigo #textmining")

matriz_tfidf <- dfm(x = texto, what = "word", remove_numbers = TRUE,
                    remove_punct = TRUE, remove_symbols = TRUE,
                    remove_separators = TRUE, remove_twitter = FALSE,
                    remove_hyphens = TRUE, remove_url = FALSE)
colnames(matriz_tfidf)

#comparacion con funcion anterior
limpiar_tokenizar(texto = texto)

paste(limpiar_tokenizar(texto = texto), collapse = " ")

# Limpieza y tokenización de los documentos de entrenamiento
tweets_train$texto <- tweets_train$texto %>% map(.f = limpiar_tokenizar) %>%
  map(.f = paste, collapse = " ") %>% unlist()

# Creación de la matriz documento-término
matriz_tfidf_train <- dfm(x = tweets_train$texto, remove = lista_stopwords)

# Se reduce la dimensión de la matriz eliminando aquellos términos que 
# aparecen en menos de 5 documentos. Con esto se consigue eliminar ruido.
matriz_tfidf_train <- dfm_trim(x = matriz_tfidf_train, min_docfreq = 5)

# Conversión de los valores de la matriz a tf-idf
matriz_tfidf_train <- tfidf(matriz_tfidf_train, scheme_tf = "prop",
                            scheme_df = "inverse")

matriz_tfidf_train

# Limpieza y tokenización de los documentos de test
tweets_test$texto <- tweets_test$texto %>% map(.f = limpiar_tokenizar) %>%
  map(.f = paste, collapse = " ") %>% unlist()
# Identificación de las dimensiones de la matriz de entrenamiento
# Los objetos dm() son de clase S4, se accede a sus elementos mediante @
dimensiones_matriz_train <- matriz_tfidf_train@Dimnames$features
# Conversión de vector a diccionario pasando por lista
dimensiones_matriz_train <- as.list(dimensiones_matriz_train)
names(dimensiones_matriz_train) <- unlist(dimensiones_matriz_train)
dimensiones_matriz_train <- dictionary(dimensiones_matriz_train)

# Proyección de los documentos de test
matriz_tfidf_test <- dfm(x = tweets_test$texto,
                         dictionary = dimensiones_matriz_train)
matriz_tfidf_test <- tfidf(matriz_tfidf_test, scheme_tf = "prop",
                           scheme_df = "inverse")


matriz_tfidf_test

all(colnames(matriz_tfidf_test) == colnames(matriz_tfidf_train))


#Modelo SVM lineal

library(e1071)
modelo_svm <- svm(x = matriz_tfidf_train, y = as.factor(tweets_train$autor),
                  kernel = "linear", cost = 1, scale = TRUE,
                  type = "C-classification")
modelo_svm

#Predicciones
predicciones <- predict(object = modelo_svm, newdata = matriz_tfidf_test)

#Error de predicción
# Matriz de confusión
table(observado = tweets_test$autor, predicho = predicciones)

# Error de clasificación
clasificaciones_erroneas <- sum(tweets_test$autor != predicciones)
error <- 100 * mean(tweets_test$autor != predicciones)
paste("Número de clasificaciones incorrectas =", clasificaciones_erroneas)

paste("Porcentaje de error =", round(error,2), "%")


#Optimización de hiperparámetros
set.seed(369)
svm_cv <- tune("svm", train.x =  matriz_tfidf_train,
               train.y = as.factor(tweets_train$autor),
               kernel = "linear", 
               ranges = list(cost = c(0.1, 0.5, 1, 2.5, 5)))
summary(svm_cv)

#
ggplot(data = svm_cv$performances, aes(x = cost, y = error)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = error - dispersion, ymax = error + dispersion)) +
  theme_bw()

svm_cv$best.parameters

#reajuste del modelo con el nuevo hiperparametro
modelo_svm <- svm(x = matriz_tfidf_train, y = as.factor(tweets_train$autor),
                  kernel = "linear", cost = 0.5, scale = TRUE)

predicciones <- predict(object = modelo_svm, newdata = matriz_tfidf_test)
table(observado = tweets_test$autor, predicho = predicciones)

#
clasificaciones_erroneas <- sum(tweets_test$autor != predicciones)
error <- 100 * mean(tweets_test$autor != predicciones)
paste("Número de clasificaciones incorrectas =", clasificaciones_erroneas)

paste("Porcentaje de error =", round(error,2), "%")

#analisis de sentimientos
library(tidytext)
sentimientos <- get_sentiments(lexicon = "bing")
head(sentimientos)

#reclasificacion
sentimientos <- sentimientos %>%
  mutate(valor = if_else(sentiment == "negative", -1, 1))

#Sentimiento promedio de cada tweet
tweets_sent <- inner_join(x = tweets_tidy, y = sentimientos,
                          by = c("token" = "word"))

#
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  head()

#Porcentaje de tweets positivos, negativos y neutros por autor
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(autor) %>%
  summarise(positivos = 100 * sum(sentimiento_promedio > 0) / n(),
            neutros = 100 * sum(sentimiento_promedio == 0) / n(),
            negativos = 100 * sum(sentimiento_promedio  < 0) / n())

#
tweets_sent %>% group_by(autor, tweet_id) %>%
  summarise(sentimiento_promedio = sum(valor)) %>%
  group_by(autor) %>%
  summarise(positivos = 100*sum(sentimiento_promedio > 0) / n(),
            neutros = 100*sum(sentimiento_promedio == 0) / n(),
            negativos = 100*sum(sentimiento_promedio  < 0) / n()) %>%
  ungroup() %>%
  gather(key = "sentimiento", value = "valor", -autor) %>%
  ggplot(aes(x = autor, y = valor, fill = sentimiento)) + 
  geom_col(position = "dodge", color = "black") + coord_flip() +
  theme_bw()

#Evolución de los sentimientos en función del tiempo
library(lubridate)
tweets_sent %>% mutate(anyo = year(fecha),
                       mes = month(fecha),
                       anyo_mes = ymd(paste(anyo, mes, sep="-"),truncated=2)) %>%
  group_by(autor, anyo_mes) %>%
  summarise(sentimiento = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = anyo_mes, y = sentimiento, color = autor)) +
  geom_point() + 
  geom_smooth() + 
  labs(x = "fecha de publicación") +
  facet_wrap(~ autor, ncol = 1) +
  theme_bw() +
  theme(legend.position = "none")

#web scrapping 
library(tidyverse)
library(rvest)

# Scrapping de la web twitter
# =============================================================================
twitter_BillGate <- read_html("https://twitter.com/billgates?lang=es")
tweets <- twitter_BillGate %>% html_nodes(".js-tweet-text-container") %>%
  html_text()
length(tweets)

#
library(RSelenium)
driver <- rsDriver()


#rD <- rsDriver(verbose = FALSE,port=4567L)
#remDr <- rD$client
#remDr$close()

#
driver$client$navigate(url = "https://twitter.com/billgates?lang=es")

# Se ejecuta escape para salir del formulario de inicio de sesión
driver$client$sendKeysToActiveElement(sendKeys = list(key = "escape"))
driver$client$sendKeysToActiveElement(sendKeys = list(key = selKeys$escape))

# Se avanza en la página para que se cargue su contenido iteración a iteración
for(i in 1:20){
  driver$client$sendKeysToActiveElement(sendKeys = list(key = "end"))
  # Se deja un tiempo para que cargue
  Sys.sleep(0.5) 
}

# Se recupera el contenido de la página
source_pagina <- driver$client$getPageSource()

# Se cierra la ventana y el servidor
driver$client$closeWindow()
driver$client$closeServer()

# Se lee el contenido html con read_html()
library(rvest)
twitter_BillGate <- read_html(source_pagina[[1]])
tweets <- twitter_BillGate %>% html_nodes(".js-tweet-text-container") %>%
  html_text()
length(tweets)


#....................................

###
library(tidytext)
library(magrittr)
library(dplyr)
sentimientos <- get_sentiments(lexicon = "bing")
head(sentimientos)

#paso valores a 1 o -1
sentimientos <- sentimientos %>%
  mutate(valor = if_else(sentiment == "negative", -1, 1))


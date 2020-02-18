#Analisis de sentimientos R
#Cargo librerias
library(tidyverse)
library(tidytext)
library(tm)
library(lubridate)
library(zoo)
library(scales)

#defino un tema
tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))


#Importo datos
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/tuits_candidatos.csv",
              "tuits_candidatos.csv")

#leo data
tuits <- read.csv("tuits_candidatos.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

#Descargo lexico afin
download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              "lexico_afinn.en.es.csv")

#leo datos
afinn <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = F, fileEncoding = "latin1") %>% 
  tbl_df()

#preparo la data
tuits <- 
  tuits %>%
  separate(created_at, into = c("Fecha", "Hora"), sep = " ") %>%
  separate(Fecha, into = c("Dia", "Mes", "Periodo"), sep = "/",
           remove = FALSE) %>%
  mutate(Fecha = dmy(Fecha),
         Semana = week(Fecha) %>% as.factor(),
         text = tolower(text)) %>%
  filter(Periodo == 2018)


#convierto twits en palabras
tuits_afinn <- 
  tuits %>%
  unnest_tokens(input = "text", output = "Palabra") %>%
  inner_join(afinn, ., by = "Palabra") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positiva", "Negativa")) %>% 
  rename("Candidato" = screen_name)

#agrupo
tuits <-
  tuits_afinn %>%
  group_by(status_id) %>%
  summarise(Puntuacion_tuit = mean(Puntuacion)) %>%
  left_join(tuits, ., by = "status_id") %>% 
  mutate(Puntuacion_tuit = ifelse(is.na(Puntuacion_tuit), 0, Puntuacion_tuit)) %>% 
  rename("Candidato" = screen_name)

#Explorando los datos, medias por día

# Total
tuits_afinn %>%
  count(Candidato)

# Únicas
tuits_afinn %>% 
  group_by(Candidato) %>% 
  distinct(Palabra) %>% 
  count()

#
map(c("Positiva", "Negativa"), function(sentimiento) {
  tuits_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(Candidato) %>%
    count(Palabra, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Palabra, n, fill = Candidato) +
    geom_col() +
    facet_wrap("Candidato", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento) +
    tema_graf
})
  

#filtro
tuits_afinn <-
  tuits_afinn %>%
  filter(Palabra != "no") 


#sentimientos por dia
tuits_afinn_fecha <-
  tuits_afinn %>%
  group_by(status_id) %>%
  mutate(Suma = mean(Puntuacion)) %>%
  group_by(Candidato, Fecha) %>%
  summarise(Media = mean(Puntuacion))


#grafico
tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = Candidato) +
  geom_line() +
  tema_graf +
  theme(legend.position = "top")

#separo graficos
tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = Candidato) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(Candidato~.) +
  tema_graf +
  theme(legend.position = "none")

#Uso regresión LOESS
tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = Candidato) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf


#
tuits_afinn %>%
  ggplot() +
  aes(Fecha, Puntuacion, color = Candidato) +
  geom_smooth(method = "loess", fill = NA) +
  tema_graf

#
tuits_afinn %>%
  ggplot() +
  aes(Fecha, Puntuacion, color = Candidato) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "loess", fill = NA) +
  facet_wrap(~Candidato) +
  tema_graf

#regresión lineal
tuits_afinn_fecha %>%
  ggplot() +
  aes(Fecha, Media, color = Candidato) +
  geom_point(color = "#E5E5E5") + 
  geom_smooth(method = "lm", fill = NA) +
  facet_wrap(~Candidato) +
  tema_graf

#Usando la media móvil
tuits_afinn_fecha %>%
  group_by(Candidato) %>%
  mutate(MediaR = rollmean(Media, k = 3, align = "right", na.pad = TRUE)) %>%
  ggplot() +
  aes(Fecha, MediaR, color = Candidato) +
  geom_hline(yintercept = 0, alpha = .35) +
  geom_line() +
  facet_grid(Candidato~.) +
  tema_graf

#Comparando sentimientos positivos y negativos
tuits_afinn %>%
  count(Candidato, Tipo) %>%
  group_by(Candidato) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Candidato, Proporcion, fill = Tipo) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  tema_graf +
  theme(legend.position = "top")

#
tuits_afinn %>%
  group_by(Candidato, Fecha) %>%
  count(Tipo) %>%
  mutate(Proporcion = n / sum(n)) %>%
  ggplot() +
  aes(Fecha, Proporcion, fill = Tipo) +
  geom_col(width = 1) +
  facet_grid(Candidato~.) +
  scale_y_continuous(labels = percent_format()) +
  scale_x_date(expand = c(0, 0)) +
  tema_graf +
  theme(legend.position = "top")

#Bloxplots
tuits %>%
  ggplot() +
  aes(Candidato, Puntuacion_tuit, fill = Candidato) +
  geom_boxplot() +
  tema_graf

#
tuits %>%
  mutate(Mes = factor(Mes)) %>% 
  ggplot() +
  aes(Mes, Puntuacion_tuit, fill = Candidato) +
  geom_boxplot(width = 1) +
  facet_wrap(~Candidato) +
  tema_graf +
  theme(legend.position = "none")


#Usando densidades
tuits %>%
  ggplot() +
  aes(Puntuacion_tuit, color = Candidato) +
  geom_density() +
  facet_wrap(~Candidato) +
  tema_graf


#
tuits %>%
  ggplot() +
  aes(Puntuacion_tuit, color = Candidato) +
  geom_density() +
  facet_grid(Candidato~Mes) +
  tema_graf




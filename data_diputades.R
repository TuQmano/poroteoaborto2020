library(rvest)
#library(xml2)
library(tidyverse)
library(glue)
library(janitor)
library(polite)
library(fs)
library(lubridate)


spawn_progressbar <- function(x, .name = .pb, .times = 1) {
  .name <- substitute(.name)
  n <- nrow(x) * .times
  eval(substitute(.name <<- dplyr::progress_estimated(n)))
  x
}

link <- 'https://www.diputados.gov.ar/diputados/listadip.html'

polite::bow(link)
#5 sec delay

nodes <- read_html(link)

table_df <- nodes %>% 
  html_nodes('table') %>% 
  html_table(.) %>% 
  tibble(.) %>%
  unnest(cols = c(.) )

table_df <- table_df %>%
  mutate(link = nodes %>%
           html_nodes('table') %>% 
           html_nodes('td') %>% 
           html_nodes("a") %>% 
           html_attr("href"))

get_datos_personales <- function(link,.pb){
  .pb$tick()$print()
  absolute_link = glue('https://www.diputados.gov.ar/diputados/{link}')

  diputade_nodes <- read_html(absolute_link)
  
  datos_personales <- diputade_nodes %>% 
    html_nodes(xpath = '//*[@id="principal-interno"]/div[2]/div[2]/div[1]') %>% 
    html_text() %>% 
    str_replace_all(.,'\\s', ' ')
  
  profesion <- str_extract(datos_personales, '(?<=Profesión:).*(?=Fecha de Nac)') %>% 
    str_trim()
  
  fecha_nacimiento <- str_extract(datos_personales, '(?<=Fecha de Nac.:).*\\d+/\\d+/\\d+') %>% 
    str_trim()
  
  mail <- diputade_nodes %>% 
    html_nodes('a') %>% 
    polite::html_attrs_dfr() %>% 
    filter(href=='mailto:target=') %>% 
    pull(.text)

  Sys.sleep(5)
  tibble('profesion'=profesion, 'fecha_nacimiento' = fecha_nacimiento,'mail'=mail )
}

safe_get_datos_personales <- safely(.f = get_datos_personales,otherwise = NA,quiet = FALSE)

safe_get_datos_personales(table_df$link[1])

diputades_df <- table_df %>% 
  spawn_progressbar %>% 
mutate(datos_personales = map(link, safe_get_datos_personales,.pb) )

diputades_df <- diputades_df %>%
  unnest(cols = c(datos_personales)) %>% 
  select(-Foto,-Despacho,-Interno,-datos_personales)

calc_age <- function(birthDate, refDate = Sys.Date()) {

  period <- as.period(interval(birthDate, refDate),
                      unit = "year")
  period$year
}

diputades_df <- diputades_df %>% 
  mutate(fecha_nacimiento = lubridate::dmy(fecha_nacimiento),
         edad = calc_age(fecha_nacimiento))



saveRDS(diputades_df,'data/data_diputades.rds')

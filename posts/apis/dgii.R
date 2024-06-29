library(httr) # vamos usar este paquete para usar el protocolo HTTP
library(tidyverse)

get_precios_combustible <- function(date) {
  link <- paste0("https://api.digital.gob.do/v1/fuels?date=", date)
  
  precios <- httr::GET(link) 
  
  
  respuesta <- httr::content(x = precios) # httr nos lee el content del objeto que generamos
  resultados <- respuesta[["data"]]
  
  data <- dplyr::bind_rows(resultados)
  
  return(data)
}

get_precios_combustible(date = "2023-01-16")

lista_fechas <- seq.Date(from = as.Date("2010-01-09"), to = as.Date("2022-04-02"), by = "week")

historico <- lapply(lista_fechas, get_precios_combustible)

historico_df <- dplyr::bind_rows(historico)

gasolina <- historico_df |> 
  dplyr::filter(stringr::str_detect(name, "Gasolina")) |>
  tidyr::separate_wider_delim(cols = name, 
                              delim = " ", 
                              names = c("combustible", "tipo"))

gasolina |> 
  mutate(date = as.Date(date)) |>
  ggplot(aes(x = date, y = price)) +
  geom_col() +
  facet_wrap(~tipo, scales = "free_y")



repuesta <- httr::GET("https://api.digital.gob.do/v1/fuels?",
                      query = list(
                        # date = "2024-01-16",
                        since = "2010-01-02",
                        until = "2023-01-16"))

conten <- httr::content(repuesta)

combustibles <- conten["data"] |> 
  bind_rows() |> 
  mutate(
    price = as.numeric(price),
    date = as.Date(date)) |> 
  filter(price != 0)



combustibles |> 
  ggplot(aes(x = date, y = price)) +
  geom_col() +
  facet_wrap(~name, scales = "free_y")

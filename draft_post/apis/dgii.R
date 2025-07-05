library(httr) 
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

lista_fechas <- seq.Date(from = as.Date("2010-01-09"), to = as.Date("2024-11-16"), by = "week")

historico <- lapply(lista_fechas, get_precios_combustible)

historico_df <- dplyr::bind_rows(historico)

gasolina <- historico_df |> 
  dplyr::mutate(
    date = as.Date(date),
    year = lubridate::year(date),
    month = lubridate::month(date),
    price = as.numeric(price)) |>
  dplyr::filter(stringr::str_detect(name, "Gasolina")) |>
  tidyr::separate_wider_delim(cols = name, 
                              delim = " ", 
                              names = c("combustible", "tipo"))
#  Vamos a calcular la variación porcentual de los precios de la gasolina
var_gasolina <- gasolina |> 
  dplyr::group_by(tipo) |> 
  dplyr::arrange(date) |> 
  dplyr::mutate(
    price_change = (price - dplyr::lag(price)) / dplyr::lag(price) * 100) |> 
  dplyr::ungroup()

# Gráficamos la variación porcentual de los precios de la gasolina
var_gasolina |> 
  ggplot(aes(x = date, y = price_change, color = tipo)) +
  geom_line() +
  labs(title = "Variación porcentual de los precios de la gasolina",
       x = "Fecha", y = "Variación porcentual") +
  theme_minimal()

gasolina |> 
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

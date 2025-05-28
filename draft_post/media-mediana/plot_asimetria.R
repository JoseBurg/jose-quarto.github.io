library(moments)
library(gridExtra)


# Configurar semilla para reproducibilidad
set.seed(123)

# Crear datos con asimetría positiva (cola hacia la derecha)
# Distribución Chi-cuadrado con pocos grados de libertad
asimetria_positiva <- data.frame(
  valor = rchisq(1000, df = 3)
)

# Crear datos con asimetría negativa (cola hacia la izquierda)
# Usar la inversa de una distribución con asimetría positiva
asimetria_negativa <- data.frame(
  valor = max(asimetria_positiva$valor) - asimetria_positiva$valor
)

plot_asimetria <- function(data, text_title){
  ggplot2::ggplot(data, ggplot2::aes(x = valor)) +
    ggplot2::geom_density(bins = 30, fill = "#e0fbfc", color = "black") +
    ggplot2::labs(title = paste0("Distribución con Asimetría ", text_title),x = "Valor",y = "Frecuencia") +
    ggplot2::geom_vline(
      xintercept = mean(data$valor), linetype = 2, size = 1, 
      colour = "red") +
    ggplot2::geom_vline(
      xintercept = median(data$valor), linetype = 9, size = 1, 
      colour = "darkred") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))

}
 
plot_asimetria(asimetria_positiva, text_title = "Positiva")
plot_asimetria(asimetria_negativa, text_title = "Negativa")

# Opcionalmente, mostrar ambos gráficos juntos
grid.arrange(grafico_positivo, grafico_negativo, ncol = 1)

# Calcular y mostrar los coeficientes de asimetría (skewness)

cat("Coeficiente de asimetría positiva:", skewness(asimetria_positiva$valor), "\n")
cat("Coeficiente de asimetría negativa:", skewness(asimetria_negativa$valor), "\n")

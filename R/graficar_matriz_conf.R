#' Graficar Matriz de Confusión con ggplot2
#'
#' Esta función genera un gráfico a partir de una matriz de confusión producida con el paquete \code{caret},
#' diferenciando visualmente los aciertos (en verde) de los errores (en rojo). Se normaliza la intensidad de los colores
#' para resaltar la magnitud de las frecuencias.
#'
#' @param conf_matrix Objeto de clase \code{confusionMatrix} generado con \code{caret}.
#' @param titulo Cadena de texto con el título del gráfico.
#'
#' @return Un objeto \code{ggplot} que contiene el gráfico de la matriz de confusión.
#'
#' @examples
#' \dontrun{
#' library(caret)
#' # Ejemplo de uso:
#' predictions <- factor(c("A", "B", "A", "A", "B"))
#' references  <- factor(c("A", "A", "A", "B", "B"))
#' conf_matrix <- confusionMatrix(predictions, references)
#' graficar_matriz_conf(conf_matrix, "Matriz de Confusión")
#' }
#'
#' @import ggplot2
#' @import caret
#' @export
graficar_matriz_conf <- function(conf_matrix, titulo) {
  conf_matrix_plot <- as.data.frame(conf_matrix$table)

  # Definir los valores de color (positivos = verde, negativos = rojo)
  conf_matrix_plot$FillValue <- ifelse(
    conf_matrix_plot$Prediction == conf_matrix_plot$Reference,
    conf_matrix_plot$Freq,   # Aciertos en verde
    -conf_matrix_plot$Freq   # Errores en rojo
  )

  # Máximos independientes para aciertos y errores
  max_aciertos <- max(conf_matrix_plot$Freq[conf_matrix_plot$FillValue > 0], na.rm = TRUE)
  max_fallos <- max(abs(conf_matrix_plot$Freq[conf_matrix_plot$FillValue < 0]), na.rm = TRUE)

  # Normalización con una intensidad mínima ajustada
  normalize_intensity <- function(value, max_value) {
    if (max_value == 0) return(0.2)  # Evitar divisiones por cero
    scaled_value <- value / max_value  # Normalizar entre 0 y 1
    return(0.22 + 0.78 * scaled_value)   # Reescalar entre 0.22 y 1
  }

  conf_matrix_plot$FillIntensity <- ifelse(
    conf_matrix_plot$FillValue > 0,
    normalize_intensity(conf_matrix_plot$FillValue, max_aciertos),  # Aciertos
    ifelse(conf_matrix_plot$FillValue < 0,
           -normalize_intensity(abs(conf_matrix_plot$FillValue), max_fallos),  # Fallos
           0)  # Valor 0 será blanco
  )

  grafico <- ggplot(conf_matrix_plot, aes(Prediction, Reference, fill = FillIntensity)) +
    geom_tile() +
    geom_text(aes(label = Freq), color = "black") +
    scale_fill_gradient2(
      low = "#F95454",
      mid = "white",
      high = "#A8CD89",
      midpoint = 0,
      name = NULL,
      limits = c(-1, 1),
      breaks = c(-1, 0, 1),
      labels = c("Más fallos", "Neutro", "Más aciertos")
    ) +
    labs(
      title = titulo,
      subtitle = paste("Accuracy:", round(conf_matrix$overall[1], 4)),
      x = "Predicho",
      y = "Real"
    ) +
    theme_minimal()

  return(grafico)
}

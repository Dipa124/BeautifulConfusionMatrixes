test_that("graficar_matriz_conf works", {
  predictions <- factor(c("A", "B", "A", "A", "B"))
  references  <- factor(c("A", "A", "A", "B", "B"))
  conf_matrix <- confusionMatrix(predictions, references)
  graficar_matriz_conf(conf_matrix, "Matriz de Confusión")
})

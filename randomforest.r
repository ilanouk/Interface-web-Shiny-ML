#Fonction RF

getPrecision_Recall_FScore <- function(mat_conf) {
  tp <- mat_conf[2, 2]
  fp <- mat_conf[1, 2]
  fn <- mat_conf[2, 1]
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  fscore <- 2 * (precision * recall) / (precision + recall)
  
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("Fscore:", fscore, "\n")
  
  return(c(precision, recall, fscore))
}


fonctionRF <- function(donnees, interet) {
  set.seed(123)
  donnees[[interet]] <- as.factor(donnees[[interet]])
  
  ind <- sample(2, nrow(donnees), replace = TRUE, prob = c(0.7, 0.3))
  data_train <- donnees[ind == 1, ]
  data_test <- donnees[ind == 2, ]
  
  rf <- randomForest(as.formula(paste(interet, "~ .")), data = data_train, ntree = 5000, mtry = 2)
  print(rf)
  cat("\n\n")
  
  pred <- predict(rf, data_test)
  mat_conf <- table(observed = data_test[[interet]], predicted = pred)
  cat("Matrice de confusion sur de nouvelles données:\n\n")
  print(mat_conf)
  
  return(list(rf, data_train, data_test, mat_conf))
}


afficheROC <- function(rf, donnees_test, interet) {
  pred_prob <- predict(rf, newdata = donnees_test, type = "prob")
  pred_positive <- pred_prob[, 2]
  true_labels <- donnees_test[[interet]]
  perf <- prediction(pred_positive, true_labels)
  auc <- performance(perf, "auc")
  cat("AUC =", auc@y.values[[1]], "\n")
  
  pred3 <- performance(perf, "tpr", "fpr")
  
  plot(pred3, main = "ROC Curve pour le Random Forest", col = 2, lwd = 2)
  abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")
}

obtenirFeaturesImportanceRF <- function(donnees_rf){
  #Extraction des scores d'importance des variables
  importance_rf <- varImp(donnees_rf)
  
  #Création d'un data frame avec les noms des variables et les scores d'importance
  i_scores <- data.frame(
    var = rownames(importance_rf),
    Overall = runif(nrow(importance_rf)),
    Importance = importance_rf$Overall  # À ajuster en fonction de la structure de importance_rf
  )
  
  #Convertir 'var' en facteur
  i_scores$var <- as.factor(i_scores$var)
  
  #Création d'un graphique à barres horizontales
  i_horizontal <- ggplot(data = i_scores) +
    geom_bar(
      stat = "identity",
      mapping = aes(x = Overall, y = var, fill = var),
      show.legend = FALSE,
      width = 0.5  #Ajuster la largeur selon les besoins
    ) +
    labs(x = NULL, y = NULL) +
    theme_minimal()
  
  plot(i_horizontal)
}

executerRandomForest <- function(donnees, param_interet){
  model_rf <- fonctionRF(donnees, param_interet )
  
  res <- getPrecision_Recall_FScore(model_rf[[4]])
  
  output$resultats_modele_rf <- renderText({
    paste("Precision:", res[1], "\n",
          "Recall:", res[2], "\n",
          "Fscore:", res[3], "\n")
  })
  
  output$courbe_roc_rf <- renderPlot({
    afficheROC(model_rf[[1]], model_rf[[3]], param_interet)
  })
  
  output$features_imp_rf <- renderPlot({
    obtenirFeaturesImportanceRF(model_rf[[1]])
  })
}

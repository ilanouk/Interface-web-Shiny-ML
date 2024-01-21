#Fonction RF

getPrecision_Recall_FScore_rf <- function(mat_conf) {
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


fonctionRF <- function(donnees, interet, is_oversample) {
  set.seed(123)
  
  rf = NULL
  data_test <- NULL
  data_train <- NULL
  
  #on recupère l'index du parametre d'interet
  index_X = which(colnames(donnees) == interet)
  
  donnees[[interet]] <- as.factor(donnees[[interet]])
  
  
  datas <- equilibrerClasses_rf(donnees, interet, index_X, is_oversample)
  data_train <- datas[[1]]
  data_test <- datas[[2]]
  
  
  rf <- randomForest(as.formula(paste(interet, "~ .")), data = data_train, ntree = 5000, mtry = 2)
  print(rf)
  cat("\n\n")
  
  pred <- predict(rf, data_test)
  mat_conf <- table(observed = data_test[[interet]], predicted = pred)
  cat("Matrice de confusion sur de nouvelles données:\n\n")
  print(mat_conf)
  
  return(list(rf, data_train, data_test, mat_conf))
}


equilibrerClasses_rf <- function(data_interet, interet, index_X, is_oversample){
  data_test <- NULL
  data_train <- NULL
  
  #On calcule les proportions de classes
  class_proportions <- prop.table(table(data_interet[[interet]]))
  
  #On vérifie si la différence de proportion dépasse le seuil
  #Si c'est le cas, la technique de sur-échantillonnage ROSE (Random Over-Sampling Examples) 
  # peut régler les prbolèmes de déséquilibre de classe
  if (is_oversample == TRUE) {
    print("Déséquilibre de classe :")
    
    #Diviser le jeu de données en ensembles d'entraînement et de test de manière stratifiée
    indices <- createDataPartition(data_interet[[interet]], p = 0.7, list = FALSE)
    p_data_train <- data_interet[indices, ]
    data_test <- data_interet[-indices, ]
    
    
    #Afficher les proportions de classes dans les ensembles d'entraînement et de test
    print(prop.table(table(p_data_train[[interet]])))
    print(prop.table(table(data_test[[interet]])))
    
    
    print(p_data_train)
    
    cat('\n')
    
    print(data_test)
    
    #Appliquer la génération synthétique de données avec ROSE sur le jeu d'entraînement
    classe_formula <- as.formula(paste(interet, "~ ."))
    
    rose_train <- ROSE(classe_formula, data = p_data_train)
    data_train <- rose_train$data
    
    cat('\n')
    print(data_train)
    cat('\n')
    
    for (col in names(data_interet[, -index_X])) {
      #Vérifie si au moins une valeur dans notre data d'origine était décimale
      has_decimal <- any(data_interet[[col]] %% 1 != 0)
      
      if (has_decimal) {
        #Arrondir les valeurs au décimal près dans notre data_train
        data_train[[col]] <- round(data_train[[col]], digits = 1)
      }
      else{
        #Arrondir les valeurs à des entiers dans notre data_train
        data_train[[col]] <- round(data_train[[col]])
      }
    }
    
    #Prendre la valeur absolue de toutes les valeurs
    data_train[, -index_X] <- abs(data_train[, -index_X])
    
    
    print(data_train)
    
    
    #Afficher les nouvelles proportions de classes dans le jeu d'entraînement après ROSE
    print(prop.table(table(data_train[[interet]])))
    
  } else {
    # Si la différence de proportion n'est pas significative, effectuer une division normale sans stratification
    indices <- createDataPartition(data_interet[[interet]], p = 0.7, list = FALSE)
    data_train <- data_interet[indices, ]
    data_test <- data_interet[-indices, ]
    
    # Afficher les proportions de classes dans les ensembles d'entraînement et de test
    print(prop.table(table(data_train[[interet]])))
    print(prop.table(table(data_test[[interet]])))
    
  }
  
  return(list(data_train,data_test))
}


afficheROC_rf <- function(rf, donnees_test, interet) {
  pred_prob <- predict(rf, newdata = donnees_test, type = "prob")
  pred_positive <- pred_prob[, 2]
  true_labels <- donnees_test[[interet]]
  pred <- prediction(pred_positive, true_labels)
  
  #AUC
  perf <- performance(pred, "auc")
  auc <- perf@y.values[[1]]
  cat("AUC = ", auc)
  
  #TPR FPR
  perf_tpr_fpr <- performance(pred, "tpr","fpr")
  
  plot(perf_tpr_fpr, main = "ROC Curve pour le Random Forest", col = 2, lwd = 2)
  abline(a = 0, b = 1, lwd = 2, lty = 2, col = "gray")
  text(0.5, 0.3, paste("AUC =", round(auc, 2)), adj = c(0.5, 0.5), col = "black", cex = 1.5)
}

afficheMatriceConfusion_rf <- function(mat_conf){
  
  tn <- mat_conf[1, 1]  # True Negatives
  fp <- mat_conf[1, 2]  # False Positives
  fn <- mat_conf[2, 1]  # False Negatives
  tp <- mat_conf[2, 2]  # True Positives
  
  Observed <- factor(c(0, 0, 1, 1))
  Predicted <- factor(c(0, 1, 0, 1))
  
  Observed <- factor(Observed, levels = c(1, 0))
  Predicted <- factor(Predicted, levels = c(0, 1))
  
  Y <- c(tn, fp, fn, tp)
  df <- data.frame(Observed, Predicted, Y)
  
  ggplot(data =  df, mapping = aes(x = Predicted, y = Observed)) +
    geom_tile(aes(fill = Y), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1, size = 12) +
    scale_fill_gradient(low = "#D6EAF8", high = "#2E86C1") +
    theme_bw() + theme(legend.position = "none",
                       axis.text.x = element_text(size = 16),
                       axis.text.y = element_text(size = 16),
                       axis.title.x = element_text(size = 16),
                       axis.title.y = element_text(size = 16))
  
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

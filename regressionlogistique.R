#install.packages("caTools")
#install.packages("caret")
#install.packages("e1071")
#install.packages("ROCR")
#install.packages("pROC")
#install.packages("plotly")

library(caTools)
library(caret)
library(e1071)
library(ROCR)
library(pROC)
library(plotly)


########################################
#Obtenir les variables dependantes (binaires) et independantes (n_val > 2)
########################################


obtenirVariables_dependantes_independantes <- function(donnees){
  #On trie les variables potentielles dépendantes et potentielles indépendantes
  variables_dependantes_potentielles <- character(0)
  variables_independantes_potentielles <- character(0)
  
  for (col in names(donnees)) {
    unique_vals <- unique(donnees[[col]])
    
    #On vérifie si la variable a seulement deux niveaux (binaire)
    if (length(unique_vals) == 2) {
      variables_dependantes_potentielles <- c(variables_dependantes_potentielles, col)
    }
    else{
      variables_independantes_potentielles <- c(variables_independantes_potentielles, col)
    }
    
    
  }
  
  #On affiche les variables potentielles dépendantes
  print(variables_dependantes_potentielles)
  cat('\n\n')
  #On affiche les variables potentielles indépendantes
  print(variables_independantes_potentielles)
  
  return(list(variables_dependantes_potentielles,variables_independantes_potentielles))

}

##########################################


fonctionRegressionLogistique<- function(donnees, interet, var_independante_1, var_independante_2) {
  set.seed(123)
  
  #Faire de la variable dependante un facteur (categorique)
  #donnees[[interet]] pour donnees$interet
  donnees[[interet]] <- as.factor(donnees[[interet]])
  
  data_interet <- donnees[, c(interet, var_independante_1, var_independante_2)]
  
  #echantillonnage trainset et testset
  indices <- createDataPartition(data_interet[[interet]], p = 0.7, list = FALSE)
  data_train <- data_interet[indices, ]
  data_test <- data_interet[-indices, ]
  
  #construction du modele
  my_formula <- reformulate(c(var_independante_1, var_independante_2), response = interet)
  modele <- glm(formula = my_formula, data = data_train, family = "binomial")
  
  print(modele)
  
  cat("\n\n")
  
  #prediction
  pred <- predict(modele, data_test)
  
  #Définir un seuil
  seuil <- 0.5
  
  #Convertir les probabilités en classes en fonction du seuil
  classes_predites <- ifelse(pred >= seuil, 2, 1)
  
  #Matrice de confusion
  mat_conf <- table(observed = data_test[[interet]], predicted = classes_predites)
  cat("Matrice de confusion sur de nouvelles données:\n\n")
  print(mat_conf)
  
  return(list(modele, data_train, data_test, mat_conf))
}



getPrecision_Recall_FScore <- function(mat_conf) {
  #On extrait les valeurs de la matrice de confusion
  tp <- mat_conf[2, 2]  # True Positives
  fp <- mat_conf[1, 2]  # False Positives
  fn <- mat_conf[2, 1]  # False Negatives
  
  print(tp)
  print(fp)
  print(fn)
  
  # Calculer la précision, le rappel et le F-score
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  fscore <- 2 * (precision * recall) / (precision + recall)
  
  
  # Afficher les résultats
  cat("Precision:", precision, "\n")
  cat("Recall:", recall, "\n")
  cat("Fscore:", fscore, "\n")
  
  return(c(precision,recall,fscore))
}


afficheROCRegressionLogistique <- function(modele,donnees_test,interet){
  
  #Prédiction des probabilités avec le modèle svm sur le jeu de données de test
  pred_prob <- predict(modele, newdata = donnees_test, type = "response")
  
  print(pred_prob)
  
  #Extraction des probabilités associées à la classe positive (2ème colonne)
  pred_positive <- pred_prob
  
  #Obtention des vraies étiquettes (valeurs de la variable cible) à partir du jeu de données de test
  true_labels <- donnees_test[[interet]]
  
  #Création d'un objet de performance en utilisant les probabilités prédites et les vraies étiquettes
  perf <- prediction(pred_positive, true_labels)
  
  #AUC
  auc <- performance(perf, "auc")
  cat("AUC = ", auc@y.values[[1]])
  
  #TPR FPR
  pred3 <- performance(perf, "tpr","fpr")
  
  #ROC curve
  plot(pred3,main="ROC Curve pour la Régression Logistique",col=2,lwd=2)
  abline(a=0,b=1,lwd=2,lty=2,col="gray")
}

obtenirFeaturesImportanceRegL <- function(reglog){
  #Extraction des scores d'importance des variables
  importance_log <- varImp(reglog)
  
  #Création d'un data frame avec les noms des variables et les scores d'importance
  i_scores <- data.frame(
    var = rownames(importance_log),
    Overall = runif(nrow(importance_log)),
    Importance = importance_log$Overall  # À ajuster en fonction de la structure de importance_rf
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

visualisationRegressionLogistique <- function(modele, interet, var_independante_1, var_independante_2) {
  # Utilisation des données de test du modèle
  data_test <- modele[[3]]
  
  # Prédiction des probabilités avec le modèle
  proba <- predict(modele[[1]], newdata = data_test, type = "response")
  
  # Création d'un graphique en 3D avec les données de test
  plot_ly(data_test, x = ~data_test[[var_independante_1]], 
          y = ~data_test[[var_independante_2]], 
          z = ~proba,
          color = ~data_test[[interet]],
          colors = c("blue", "red"),
          type = "scatter3d", mode = "markers") %>%
    layout(scene = list(xaxis = list(title = var_independante_1),
                        yaxis = list(title = var_independante_2),
                        zaxis = list(title = paste("Probabilité de", interet))))
}

executerRegressionLogistique <- function(donnees, param_interet, param_independant_1, param_independant_2){
  
  model_log <- fonctionRegressionLogistique(donnees, param_interet, param_independant_1, param_independant_2)
  
  #Modèle
  donnees_log <- model_log[[1]]
  
  #Train_set
  donnees_train <- model_log[[2]]
  
  #Test_set
  donnees_test <- model_log[[3]]
  
  #Matrice de confusion
  mat_c <- model_log[[4]]
  
  #Precision, Recall, F1-score
  res <- getPrecision_Recall_FScore(mat_c)
  
  output$resultats_modele_rlog <- renderText({
    paste("Precision:", res[1], "\n",
          "Recall:", res[2], "\n",
          "Fscore:", res[3], "\n")
  })
  
  output$courbe_roc_rlog <- renderPlot({
    afficheROCRegressionLogistique(model_log[[1]], model_log[[3]], param_interet)
  })
  
  output$nuage_points_3d <- renderPlotly({
    visualisationRegressionLogistique(model_log, param_interet, param_independant_1, param_independant_2)
  })
  
  output$features_imp_rlog <- renderPlot({
    obtenirFeaturesImportanceRegL(donnees_log)
  })
  
}



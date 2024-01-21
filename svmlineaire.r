getPrecision_Recall_FScore_svm <- function(mat_conf) {
  #On extrait les valeurs de la matrice de confusion
  tp <- mat_conf[2, 2]  # True Positives
  fp <- mat_conf[1, 2]  # False Positives
  fn <- mat_conf[2, 1]  # False Negatives
  
  
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

#Fonction permettant l'application du modèle svm lineaire
# parametres : le dataset, le paramètre d'interet

fonctionSVM_lineaire <- function(donnees, interet, is_oversample) {
  set.seed(123)
  
  SVM <- NULL
  
  #on recupère l'index du parametre d'interet
  index_X = which(colnames(donnees) == interet)
  
  #Faire de la variable dependante un facteur (categorique)
  #donnees[[interet]] pour donnees$interet
  donnees[[interet]] <- as.factor(donnees[[interet]])
  
  
  datas <- equilibrerClasses_svm(donnees, interet, index_X, is_oversample)
  data_train <- datas[[1]]
  data_test <- datas[[2]]
  
  
  #Etape importante dans de nombreux algorithmes d'apprentissage automatique,
  # y compris les machines à vecteurs de support (SVM). Cette pratique vise à normaliser
  # les caractéristiques du jeu de données, c'est-à-dire à les ramener à une échelle commune
  
  #Feature Scaling sauf sur le parametre d'interet
  data_train[,-index_X] <- scale(data_train[,-index_X])
  data_test[,-index_X] <- scale(data_test[,-index_X])
  
  #construction du modele
  SVM = svm(as.formula(paste(interet, "~ .")), data = data_train, type = 'C-classification', kernel = 'linear')
  print(SVM)
  
  cat("\n\n")
  
  #prediction
  pred <- predict(SVM, data_test)
  
  #Matrice de confusion
  mat_conf <- table(observed = data_test[[interet]], predicted = pred)
  cat("Matrice de condusion sur de nouvelles données:\n\n")
  print(mat_conf)
  
  return(list(SVM, data_train, data_test, mat_conf))
}


equilibrerClasses_svm <- function(data_interet, interet, index_X, is_oversample){
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


afficheROC_SVM <- function(SVM,donnees_test,interet, type){
  
  #Prédiction des probabilités avec le modèle svm sur le jeu de données de test
  pred_prob <- predict(SVM, newdata = donnees_test, type = "prob")
  
  #Extraction des probabilités associées à la classe positive (2ème colonne)
  pred_positive <- as.numeric(levels(pred_prob))[pred_prob]
  
  #Obtention des vraies étiquettes (valeurs de la variable cible) à partir du jeu de données de test
  true_labels <- donnees_test[[interet]]
  
  #Création d'un objet de performance en utilisant les probabilités prédites et les vraies étiquettes
  pred <- prediction(pred_positive, true_labels)
  
  #AUC
  perf <- performance(pred, "auc")
  auc <- perf@y.values[[1]]
  cat("AUC = ", auc)
  
  #TPR FPR
  perf_tpr_fpr <- performance(pred, "tpr","fpr")
  
  if(type=="linéaire"){
    #ROC curve
    plot(perf_tpr_fpr,main="ROC Curve pour le SVM Linéaire",col=2,lwd=2)
    abline(a=0,b=1,lwd=2,lty=2,col="gray")
  }
  
  else if(type=="radiale"){
    #ROC curve
    plot(perf_tpr_fpr,main="ROC Curve pour le SVM Radiale",col=2,lwd=2)
    abline(a=0,b=1,lwd=2,lty=2,col="gray")
  }
  text(0.5, 0.3, paste("AUC =", round(auc, 2)), adj = c(0.5, 0.5), col = "black", cex = 1.5)
}

afficheMatriceConfusionSVM <- function(mat_conf){
  
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
    ggtitle("SVM Linéaire") +
    geom_tile(aes(fill = Y), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1, size = 12) +
    scale_fill_gradient(low = "#D6EAF8", high = "#2E86C1") +
    theme_bw() + theme(legend.position = "none",
                       axis.text.x = element_text(size = 16),
                       axis.text.y = element_text(size = 16),
                       axis.title.x = element_text(size = 16),
                       axis.title.y = element_text(size = 16))
  
}

obtenirFeaturesImportanceSVM <- function(svmod, interet){
  
  #Dans le contexte des modèles SVM , les coefficients négatifs pour certaines variables indiquent
  # l'impact inverse de ces variables sur la classe prédite. Cela signifie que lorsque la valeur
  # d'une variable avec un coefficient négatif augmente, la probabilité d'appartenir à la classe positive diminue.
  
  
  #On extrait les noms des variables (sauf la variable d'intérêt, et l'intercept (notion statistique d'equilibrage))
  coefs_svm <- coef(svmod)[-1]
  var_names <- setdiff(names(coefs_svm), interet)
  
  poids_normalises <- coefs_svm / sqrt(sum(coefs_svm^2))
  
  #Création d'un data frame avec les noms des variables et les scores d'importance
  var_importance <- data.frame(
    var = var_names,
    Importance = poids_normalises
  )
  
  #Création d'un graphique à barres horizontales
  i_horizontal <- ggplot(data = var_importance) +
    geom_bar(
      stat = "identity",
      mapping = aes(x = Importance, y = var, fill = var),
      show.legend = FALSE,
      width = 0.5  # Adjust the width as needed
    ) +
    ggtitle("SVM Linéaire") +
    labs(x = NULL, y = NULL) +
    theme_minimal()
  
  plot(i_horizontal)
  
}

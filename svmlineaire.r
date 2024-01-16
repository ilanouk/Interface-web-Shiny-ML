getPrecision_Recall_FScore <- function(mat_conf) {
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

fonctionSVM_lineaire <- function(donnees, interet) {
  SVM = NULL
  set.seed(123)
  
  #on recupère l'index du parametre d'interet
  index_X = which(colnames(donnees) == interet)
  
  #Faire de la variable dependante un facteur (categorique)
  #donnees[[interet]] pour donnees$interet
  donnees[[interet]] <- as.factor(donnees[[interet]])
  
  #echantillonnage trainset et testset
  ind <- sample(2, nrow(donnees), replace = TRUE, prob=c(0.7, 0.3))
  data_train <- donnees[ind == 1,]
  data_test <- donnees[ind == 2,]
  
  #Etape importante dans de nombreux algorithmes d'apprentissage automatique,
  # y compris les machines à vecteurs de support (SVM). Cette pratique vise à normaliser
  # les caractéristiques du jeu de données, c'est-à-dire à les ramener à une échelle commune
  
  #Feature Scaling sauf sur le parametre d'interet
  data_train[-index_X] <- scale(data_train[-index_X])
  data_test[-index_X] <- scale(data_test[-index_X])
  
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



afficheROC_SVM <- function(SVM,donnees_test,interet, type){
  
  #Prédiction des probabilités avec le modèle svm sur le jeu de données de test
  pred_prob <- predict(SVM, newdata = donnees_test, type = "prob")
  
  #Extraction des probabilités associées à la classe positive (2ème colonne)
  pred_positive <- as.numeric(levels(pred_prob))[pred_prob]
  
  #Obtention des vraies étiquettes (valeurs de la variable cible) à partir du jeu de données de test
  true_labels <- donnees_test[[interet]]
  
  #Création d'un objet de performance en utilisant les probabilités prédites et les vraies étiquettes
  perf <- prediction(pred_positive, true_labels)
  
  #AUC
  auc <- performance(perf, "auc")
  cat("AUC = ", auc@y.values[[1]])
  
  #TPR FPR
  pred3 <- performance(perf, "tpr","fpr")
  
  if(type=="linéaire"){
    #ROC curve
    plot(pred3,main="ROC Curve pour le SVM Linéaire",col=2,lwd=2)
    abline(a=0,b=1,lwd=2,lty=2,col="gray")
  }
  
  else if(type=="radiale"){
    #ROC curve
    plot(pred3,main="ROC Curve pour le SVM Radiale",col=2,lwd=2)
    abline(a=0,b=1,lwd=2,lty=2,col="gray")
  }
}

obtenirFeaturesImportanceSVM <- function(svmod, interet){
  
  #Dans le contexte des modèles SVM , les coefficients négatifs pour certaines variables indiquent
  # l'impact inverse de ces variables sur la classe prédite. Cela signifie que lorsque la valeur
  # d'une variable avec un coefficient négatif augmente, la probabilité d'appartenir à la classe positive diminue.
  
  
  #On extrait les noms des variables (sauf la variable d'intérêt, et l'intercept (notion statistique d'equilibrage))
  coefs_svm <- coef(svmod)[-1]
  var_names <- setdiff(names(coefs_svm), param_interet)
  
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

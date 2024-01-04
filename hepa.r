# Chargement du package nécessaire
install.packages("readr")
library(readr)

# Spécifie le chemin vers le fichier hepatitis.data
chemin_fichier <- "hepatitis.data"

# Charge les données depuis le fichier
donnees <- read.table(chemin_fichier, header = TRUE, sep = ",")
head(donnees)

# Convertir les colonnes chr en numériques
colonnes_chr <- sapply(donnees, is.character)
donnees[, colonnes_chr] <- sapply(donnees[, colonnes_chr], as.numeric)

# Convertir la colonne catégorielle en facteur
donnees$X1 <- as.factor(donnees$X1)


# Remplacer les "?" par NA dans les colonnes numériques
donnees[donnees == "?"] <- NA

# Afficher la structure mise à jour
str(donnees)



# Affiche les valeurs manquantes
print("Valeurs manquantes par colonne :")
print(colSums(is.na(donnees)))

# Affiche les valeurs manquantes par ligne
print("Valeurs manquantes par ligne :")
print(rowSums(is.na(donnees)))



# Exemple : Résumé statistique d'une variable qualitative
summary(donnees)



moyenne_X_dot <- mean(donnees$X., na.rm = TRUE)
moyenne_X85 <- mean(donnees$X85, na.rm = TRUE)

donnees$X.[is.na(donnees$X.)] <- moyenne_X_dot
donnees$X85[is.na(donnees$X85)] <- moyenne_X85

donnees <- na.omit(donnees)
summary(donnees)



# Sélectionner uniquement les colonnes quantitatives
colonnes_quantitatives <- sapply(donnees, is.numeric)
donnees_quantitatives <- donnees[, colonnes_quantitatives]

# Créer la boîte à moustaches pour toutes les variables quantitatives
boxplot(donnees_quantitatives, las = 2, main = "Boxplot des variables quantitatives")




# Créer un nuage de points pour chaque paire de variables quantitatives, en distinguant les groupes définis par X1
par(mfrow = c(3, 2))  # Ajuste le nombre de lignes et de colonnes selon le nombre de variables quantitatives

for (i in 1:(ncol(donnees_quantitatives) - 1)) {
  for (j in (i + 1):ncol(donnees_quantitatives)) {
    plot(donnees_quantitatives[, i], donnees_quantitatives[, j],
         col = donnees$X1, pch = 16,
         xlab = names(donnees_quantitatives)[i], ylab = names(donnees_quantitatives)[j],
         main = paste("Nuage de points entre", names(donnees_quantitatives)[i], "et", names(donnees_quantitatives)[j]))
  }
}




# Créer un diagramme en barres entre deux variables
barplot(table(donnees$X1, donnees$X2.8), col = c("skyblue", "orange"),
        main = "Diagramme en Barre entre X1 et X2.8", xlab = "X1", ylab = "Fréquence", beside = TRUE, legend = rownames(table(donnees$X1, donnees$X2.8)))
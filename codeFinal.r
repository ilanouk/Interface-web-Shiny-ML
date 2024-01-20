# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("randomForest")
# install.packages("caTools")
# install.packages("caret") #confusionMatrix
# install.packages("yardstick")
# install.packages("ggplot2")
# install.packages("ROCR")
# install.packages("e1071")
# install.packages("pROC")
# install.packages("plotly")

source('utils.r')

#Modèles
source('randomforest.r')
source('regressionlogistique.r')
source('svmlineaire.r')
source('svmradiale.r')



library(shiny)
library(ggplot2)
library(dplyr)
library(randomForest)
library(caTools)
library(caret)
library(yardstick)
library(ROCR)
library(e1071)
library(pROC)
library(plotly)


# Interface utilisateur Shiny
ui <- fluidPage(
  titlePanel("Analyse Exploratoire - Choix de Variables"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fichier", "Choisir le fichier .data"),
      selectInput("colonne1", "Choisir la première variable :", ""),
      selectInput("colonne2", "Choisir la deuxième variable :", ""),
      selectInput("type_viz", "Choisir le type de visualisation :", c("Comparaison de Deux Variables", "Visualisation d'une Seule Variable")),
      selectInput("interet", "Variable d'intérêt :", ""),
      actionButton("analyser", "Lancer l'analyse"),
      actionButton("button_to_NA", "Lancer le Preprocessing")
      
    ),
    
    mainPanel(
      tabsetPanel(
                    tabPanel("Donnéees" ,tableOutput("myDataTable")
        ),
        tabPanel( "Preproprecessing",
                  tabsetPanel(
                    textInput("string_to_replace","Entrer le string que vous voulez remplacer par des NA"),
                    selectInput("numericMethod", "Choose Method for Numeric Variables to replace NA:",
                                choices = c("None","Mean", "Median")),
                    selectInput("categoricalMethod", "Choose Method for Categorical Variables to replace NA:",
                                choices = c("None","Most Frequent", "Least Frequent")),
                    checkboxInput('do_normalisation', 'Voulez vous normaliser le dataset',value=FALSE)
                    
                  )
        ),
        tabsetPanel(
          tabPanel("Comparaison de Deux Variables", 
                   tabsetPanel(
                     tabPanel("Nuage de Points", plotOutput("nuage_points")),
                     tabPanel("Boxplot", plotOutput("boxplot_tab")),
                     tabPanel("Tableau Récapitulatif", tableOutput("tableau_recap"))
                   )
          ),
          tabPanel("Visualisation d'une Seule Variable", 
                   tabsetPanel(
                     tabPanel("Diagramme en Barre", plotOutput("barplot")),
                     tabPanel("Boîte à Moustaches", plotOutput("boxplot")),
                     tabPanel("Courbe des Fréquences Cumulées", plotOutput("courbe_freq_cumulee")),
                     tabPanel("Tableau Statistique", tableOutput("tableau_stats"))
                   )
          ),
          tabPanel("RandomForest",
                   tabsetPanel(
                     tabPanel("Modélisation",
                               actionButton("lancer_modele_rf", "Lancer le modèle RandomForest"),
                               textOutput("resultats_modele_rf"),
                               plotOutput("courbe_roc_rf")
                     ),
                     tabPanel("Matrice de confusion", plotOutput("mat_conf_rf")),
                     tabPanel("Features importance", plotOutput("features_imp_rf"))
                   )
          ),
          tabPanel("SVM",
                   tabsetPanel(
                     tabPanel("Modélisation",
                               actionButton("lancer_modele_svm", "Lancer le modèle SVM"),
                               textOutput("resultats_modele_svm"),
                               plotOutput("courbe_roc_svm"),
                               textOutput("resultats_modele_svmr"),
                               plotOutput("courbe_roc_svmr")
                     ),
                     tabPanel("Matrice de confusion",
                              tabPanel("SVM Linéaire", plotOutput("mat_conf_svm")),
                              tabPanel("SVM Radiale", plotOutput("mat_conf_svmr"))
                     ),
                     tabPanel("Features importance",
                              tabPanel("SVM Linéaire",plotOutput("features_imp_svm")),
                              tabPanel("SVM Radiale",plotOutput("features_imp_svmr"))
                     )
                   )
          ),
          tabPanel("Régression Logistique", 
                   tabsetPanel(
                     tabPanel("Modélisation",
                              actionButton("lancer_modele_rlog", "Lancer le modèle Régression Logistique"),
                              textOutput("resultats_modele_rlog"),
                              plotOutput("courbe_roc_rlog")
                     ),
                     tabPanel("Matrice de confusion", plotOutput("mat_conf_rlog")),
                     tabPanel("Features importance", plotOutput("features_imp_rlog")),
                     tabPanel("Visualisation 3D", plotlyOutput("nuage_points_3d"))
                   )
          )
        )
      )
    )
  )
)

# Serveur Shiny
server <- function(input, output, session) {
  
  donnees <- reactiveValues(df = NULL)
  
  observeEvent(input$fichier, {
    req(input$fichier)
    df <- read.table(input$fichier$datapath, header = TRUE, sep = ",")
    # You might want to perform any additional processing here
    # Update the reactive dataframe
    donnees$df <- df
  })
  observe({
    req(input$fichier)
    
    # Mettre à jour les choix des variables lorsque le fichier est chargé
    col_choices <- names(donnees$df)
    updateSelectInput(session, "colonne1", choices = col_choices)
    updateSelectInput(session, "colonne2", choices = col_choices)
    updateSelectInput(session, "interet", choices = col_choices)
    
  })
  
  observeEvent(input$analyser, {
    req(input$colonne1)
    
    ###### VISU DOUBLE #######
    
    if (input$type_viz == "Comparaison de Deux Variables") {
      req(input$colonne2)
      str(donnees$df)
      # Créer le nuage de points
      output$nuage_points <- renderPlot({
        options(scipen=999)
        x.var <- input$colonne1
        y.var <- input$colonne2
        
        plot(x = donnees$df[, x.var], y = donnees$df[, y.var], col = as.factor(donnees$df[, y.var]),
             las = 2, cex.axis = 0.7,
             main = paste(y.var, "en fonction de", x.var),
             xlab = x.var, ylab = y.var, cex.lab = 1.2)
        
        options(scipen=0)
      })
      
      tableau_recap <- as.data.frame(t(sapply(donnees$df[, c(input$colonne1, input$colonne2), drop = FALSE], summary)))
      
      # Renommer les colonnes
      colnames(tableau_recap) <- paste(rep(c(""), each = 1), colnames(tableau_recap), sep = "")
      
      # Afficher le tableau récapitulatif
      output$tableau_recap <- renderTable({
        print(tableau_recap)
      })
      
      # Créer la visualisation en fonction du choix de l'utilisateur
      gg <- ggplot(donnees$df, aes_string(x = input$colonne1, y = input$colonne2)) +
        geom_point() +
        labs(title = paste("Nuage de Points entre", input$colonne1, "et", input$colonne2),
             x = input$colonne1, y = input$colonne2) +
        theme_minimal()
      
      # Afficher la visualisation
      output$graphique <- renderPlot({
        print(gg)
      })
      
      ## Créer les boîtes à moustaches côte à côte
      output$boxplot_tab <- renderPlot({
        par(mfrow = c(1, 2))  # Diviser la zone graphique en 2 colonnes
        
        # Boxplot pour input$colonne1
        boxplot(donnees$df[[input$colonne1]], col = grey(0.8),
                main = paste("Boîte à Moustaches pour", input$colonne1),
                ylab = input$colonne1, las = 1)
        rug(donnees$df[[input$colonne1]], side = 2)
        
        # Boxplot pour input$colonne2
        boxplot(donnees$df[[input$colonne2]], col = grey(0.8),
                main = paste("Boîte à Moustaches pour", input$colonne2),
                ylab = input$colonne2, las = 1)
        rug(donnees$df[[input$colonne2]], side = 2)
      })
      
      
      
      ####### VISU SIMPLE #######
      
    } else if (input$type_viz == "Visualisation d'une Seule Variable") {
      
      # Créer le diagramme en barre
      barplot_gg <- ggplot(donnees$df, aes_string(x = input$colonne1)) +
        geom_bar() +
        labs(title = paste("Diagramme en Barre pour", input$colonne1),
             x = input$colonne1, y = "Count") +
        theme_minimal()
      
      # Afficher le diagramme en barre
      output$barplot <- renderPlot({
        print(barplot_gg)
      })
      
      # Créer la boîte à moustaches
      output$boxplot <- renderPlot({
        boxplot(donnees$df[[input$colonne1]], col = grey(0.8),
                main = paste("Boîte à Moustaches pour", input$colonne1),
                ylab = input$colonne1, las = 1)
        rug(donnees$df[[input$colonne1]], side = 2)
      })
      
      # Créer la courbe des fréquences cumulées
      freq_cumulee_gg <- ggplot(donnees$df, aes_string(x = input$colonne1)) +
        stat_ecdf(geom = "step", pad = FALSE) +
        labs(title = paste("Courbe des Fréquences Cumulées pour", input$colonne1),
             x = input$colonne1, y = "Fréquence Cumulée") +
        theme_minimal()
      
      # Afficher la courbe des fréquences cumulées
      output$courbe_freq_cumulee <- renderPlot({
        print(freq_cumulee_gg)
      })
      
      # Créer le tableau statistique
      stats_table <- summary(donnees$df[, input$colonne1, drop = FALSE])
      
      # Afficher le tableau statistique
      output$tableau_stats <- renderTable({
        print(stats_table)
      })
    }
  })
  
  ###### MODELE #######
  
  observeEvent(input$lancer_modele_rf, {
    
    model_rf <- fonctionRF(donnees$df, input$interet )
    
    res <- getPrecision_Recall_FScore(model_rf[[4]])
    
    output$resultats_modele_rf <- renderText({
      paste("Precision:", res[1], "\n",
            "Recall:", res[2], "\n",
            "Fscore:", res[3], "\n")
    })
    
    output$courbe_roc_rf <- renderPlot({
      afficheROC(model_rf[[1]], model_rf[[3]], input$interet)
    })
    
    output$mat_conf_rf <- renderPlot({
      afficheMatriceConfusion(model_rf[[4]])
    })
    
    output$features_imp_rf <- renderPlot({
      obtenirFeaturesImportanceRF(model_rf[[1]])
    })
  })
  
  
  
  
  observeEvent(input$lancer_modele_svm, {
    str(donnees$df)
    #SVM Linéaire
    model_svm <- fonctionSVM_lineaire(donnees$df, input$interet)
    
    #SMV Radial
    model_svmr <- fonctionSVM_radial(donnees$df, input$interet)
    
    res_l <- getPrecision_Recall_FScore(model_svm[[4]])
    res_r <- getPrecision_Recall_FScore(model_svmr[[4]])
    
    output$resultats_modele_svm <- renderText({
      paste("Precision:", res_l[1], "\n",
            "Recall:", res_l[2], "\n",
            "Fscore:", res_l[3], "\n")
    })
    
    output$resultats_modele_svmr <- renderText({
      paste("Precision:", res_r[1], "\n",
            "Recall:", res_r[2], "\n",
            "Fscore:", res_r[3], "\n")
    })
    
    output$courbe_roc_svm <- renderPlot({
      afficheROC_SVM(model_svm[[1]], model_svm[[3]], input$interet, "linéaire")
    })
    
    output$courbe_roc_svmr <- renderPlot({
      afficheROC_SVM(model_svmr[[1]], model_svmr[[3]], input$interet, "radiale")
    })
    
    output$mat_conf_svm <- renderPlot({
      afficheMatriceConfusionSVM(model_svm[[4]])
    })
    
    output$mat_conf_svmr <- renderPlot({
      afficheMatriceConfusionSVMR(model_svmr[[4]])
    })
    
    output$features_imp_svm <- renderPlot({
      obtenirFeaturesImportanceSVM(model_svm[[1]], input$interet)
    })
    
    output$features_imp_svmr <- renderPlot({
      obtenirFeaturesImportanceSVMR(model_svmr[[1]], input$interet)
    })
    
    
  })
  
  
  #REGRESSION LOGISTIQUE
  observeEvent(input$lancer_modele_rlog, {
    
    model_log <- fonctionRegressionLogistique(donnees$df, input$interet, input$colonne1, input$colonne2)
    
    #Modèle
    donnees_log <- model_log[[1]]
    
    #Train_set
    donnees_train <- model_log[[2]]
    
    #Test_set
    donnees_test <- model_log[[3]]
    
    #Matrice de confusion
    mat_c_rlog <- model_log[[4]]
    
    
    #Precision, Recall, F1-score
    res <- getPrecision_Recall_FScore_rlog(mat_c_rlog)
    
    output$resultats_modele_rlog <- renderText({
      paste("Precision:", res[1], "\n",
            "Recall:", res[2], "\n",
            "Fscore:", res[3], "\n")
    })
    
    output$courbe_roc_rlog <- renderPlot({
      afficheROCRegressionLogistique(model_log[[1]], model_log[[3]], input$interet)
    })
    
    output$features_imp_rlog <- renderPlot({
      obtenirFeaturesImportanceRegL(donnees_log)
    })
    
    output$nuage_points_3d <- renderPlotly({
      visualisationRegressionLogistique(model_log, input$interet, input$colonne1, input$colonne2)
    })
    
    output$mat_conf_rlog <- renderPlot({
      afficheMatriceConfusionRLOG(mat_c_rlog)
    })
    
    
  })
  
  # Print les données
  observeEvent(input$button_to_NA, {
    df <- donnees$df

    df <- (replace_by_NA(df,input$string_to_replace))
    #Remplacement de tout les NA
    df <- replace_missing_values(df,input$numericMethod, input$categoricalMethod)
    df <- mutate_all(df, as.numeric)

    donnees$df <- df
    #print(class_diff(df,input$interet))
    output$myDataTable <- renderTable(donnees$df)
    
  })
  output$myDataTable <- renderTable(donnees$df)
  
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)

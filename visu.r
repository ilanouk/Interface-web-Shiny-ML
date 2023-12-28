# Installer les packages Shiny et ggplot2 s'ils ne sont pas déjà installés
# install.packages("shiny")
# install.packages("ggplot2")

library(shiny)
library(ggplot2)

# Interface utilisateur Shiny
ui <- fluidPage(
  titlePanel("Analyse Exploratoire - Choix de Variables"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fichier", "Choisir le fichier .data"),
      selectInput("colonne1", "Choisir la première variable :", ""),
      selectInput("colonne2", "Choisir la deuxième variable (optionnel) :", ""),
      selectInput("type_viz", "Choisir le type de visualisation :", c("Comparaison de Deux Variables", "Visualisation d'une Seule Variable")),
      actionButton("analyser", "Lancer l'analyse")
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.type_viz == 'Comparaison de Deux Variables'",
        plotOutput("graphique")
      ),
      conditionalPanel(
        condition = "input.type_viz == 'Visualisation d'une Seule Variable'",
        plotOutput("barplot"),
        plotOutput("boxplot"),
        tableOutput("tableau_stats")
      )
    )
  )
)

# Serveur Shiny
server <- function(input, output, session) {
  
  donnees <- reactive({
    req(input$fichier)
    read.table(input$fichier$datapath, header = TRUE, sep = ",")
  })
  
  observe({
    req(input$fichier)
    
    # Mettre à jour les choix des variables lorsque le fichier est chargé
    col_choices <- names(donnees())
    updateSelectInput(session, "colonne1", choices = col_choices)
    updateSelectInput(session, "colonne2", choices = col_choices)
  })
  
  observeEvent(input$analyser, {
    req(input$colonne1)
    
    if (input$type_viz == "Comparaison de Deux Variables") {
      req(input$colonne2)
      
      # Créer la visualisation en fonction du choix de l'utilisateur
      gg <- ggplot(donnees(), aes_string(x = input$colonne1, y = input$colonne2)) +
        geom_point() +
        labs(title = paste("Nuage de Points entre", input$colonne1, "et", input$colonne2),
             x = input$colonne1, y = input$colonne2) +
        theme_minimal()
      
      # Afficher la visualisation
      output$graphique <- renderPlot({
        print(gg)
      })
      
    } else if (input$type_viz == "Visualisation d'une Seule Variable") {
      
      # Créer le diagramme en barre
      barplot_gg <- ggplot(donnees(), aes_string(x = input$colonne1)) +
        geom_bar() +
        labs(title = paste("Diagramme en Barre pour", input$colonne1),
             x = input$colonne1, y = "Count") +
        theme_minimal()
      
      # Afficher le diagramme en barre
      output$barplot <- renderPlot({
        print(barplot_gg)
      })
      
      # Créer la boîte à moustaches
      boxplot_gg <- ggplot(donnees(), aes_string(y = input$colonne1)) +
        geom_boxplot() +
        labs(title = paste("Boîte à Moustaches pour", input$colonne1),
             x = "", y = input$colonne1) +
        theme_minimal()
      
      # Afficher la boîte à moustaches
      output$boxplot <- renderPlot({
        print(boxplot_gg)
      })
      
      # Créer le tableau statistique
      stats_table <- summary(donnees()[, input$colonne1, drop = FALSE])
      
      # Afficher le tableau statistique
      output$tableau_stats <- renderTable({
        print(stats_table)
      })
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)

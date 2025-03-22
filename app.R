# Auteur : Axelle Bourez
# Date : 2025-01-31
# Licence : MIT (voir fichier LICENSE pour les détails)
# Description : Heatmap interactive

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(heatmaply)
library(plotly)
library(tibble)

ui <- fluidPage(
  titlePanel("Heatmap Interactive"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choisir un fichier Excel", accept = c(".xlsx")),
      uiOutput("select_sheet"),
      uiOutput("select_samples"),
      uiOutput("select_annotations"),
      selectInput("cor_method", "Méthode de corrélation", 
                  choices = c("pearson", "spearman", "kendall"),
                  selected = "pearson"),
      selectInput("clust_method", "Méthode de clustering", 
                  choices = c("ward.D", "ward.D2", "complete", "average", "1 - correlation"),
                  selected = "ward.D"),
      selectInput("color_palette", "Palette de couleurs", 
                  choices = c("Blues", "RdBu", "Viridis", "Plasma", "Magma"),
                  selected = "Blues"),
      downloadButton("download_png", "Télécharger en PNG"),
      downloadButton("download_pdf", "Télécharger en PDF")
    ),
    mainPanel(
      plotlyOutput("heatmap_plot", height = "700px")
    )
  )
)

server <- function(input, output, session) {
  
  dataset <- reactive({
    req(input$file)
    as.data.frame(readxl::read_excel(input$file$datapath, sheet = input$sheet_selected))
  })
  
  output$select_sheet <- renderUI({
    req(input$file)
    sheets <- excel_sheets(input$file$datapath)
    selectInput("sheet_selected", "Sélectionner une feuille", choices = sheets, selected = sheets[1])
  })
  
  output$select_samples <- renderUI({
    req(dataset())
    checkboxGroupInput("samples_selected", "Sélectionner les échantillons", 
                       choices = colnames(dataset()), 
                       selected = colnames(dataset())[-1])
  })
  
  output$select_annotations <- renderUI({
    req(dataset())
    checkboxGroupInput("annotations_selected", "Sélectionner la colonne d'annotation (gènes, catégories...)", 
                       choices = colnames(dataset()), 
                       selected = colnames(dataset())[1])
  })
  
  heatmap_data <- reactive({
    req(input$samples_selected, input$annotations_selected)
    df <- dataset()
    
    # Sélectionner la colonne d'annotation et les colonnes d'échantillons
    df_selected <- df[, c(input$annotations_selected, input$samples_selected), drop = FALSE]
    
    # Remplacer les valeurs manquantes par 0
    df_selected[] <- lapply(df_selected, function(x) {
      x[is.na(x)] <- 0
      x
    })
    
    # Si des duplications existent dans les noms de lignes, les agréger
    row_names <- df_selected[[input$annotations_selected[1]]]
    if(any(duplicated(row_names))) {
      df_selected <- df_selected %>%
        group_by(across(all_of(input$annotations_selected))) %>%
        summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
        as.data.frame()
      row_names <- df_selected[[input$annotations_selected[1]]]
    }
    
    # Assigner les noms de lignes et convertir en matrice
    rownames(df_selected) <- row_names
    df_numeric <- df_selected[, input$samples_selected, drop = FALSE]
    df_numeric[] <- lapply(df_numeric, function(x) as.numeric(as.character(x)))
    df_numeric <- as.matrix(df_numeric)
    
    # Supprimer les lignes à variance nulle
    variances <- apply(df_numeric, 1, var)
    if(any(variances == 0)){
      df_numeric <- df_numeric[variances != 0, , drop = FALSE]
    }
    
    return(df_numeric)
  })
  
  output$heatmap_plot <- renderPlotly({
    req(heatmap_data())
    
    dist_method <- if (input$clust_method == "1 - correlation") {
      function(x) as.dist(1 - cor(t(x), method = input$cor_method))
    } else {
      function(x) dist(x, method = "euclidean")
    }
    
    heatmaply(
      heatmap_data(),
      colors = switch(input$color_palette,
                      "Blues" = blues9,
                      "RdBu" = RColorBrewer::brewer.pal(11, "RdBu"),
                      "Viridis" = viridis::viridis(10),
                      "Plasma" = viridis::plasma(10),
                      "Magma" = viridis::magma(10)),
      scale = "row",
      dendrogram = "both",
      distfun = dist_method,
      hclustfun = function(x) hclust(x, method = ifelse(input$clust_method == "1 - correlation", "complete", input$clust_method)),
      key.title = "Corrélation",
      margins = c(80, 80),
      main = "Heatmap Interactive",
      xlab = "Échantillons",
      ylab = input$annotations_selected[1]
    )
  })
  
  generate_ggplot_heatmap <- function() {
    df <- heatmap_data()
    df_long <- as.data.frame(as.table(df))
    colnames(df_long) <- c("Gène", "Échantillon", "Valeur")
    
    ggplot(df_long, aes(x = Échantillon, y = Gène, fill = Valeur)) +
      geom_tile() +
      scale_fill_gradientn(colors = switch(input$color_palette,
                                           "Blues" = blues9,
                                           "RdBu" = RColorBrewer::brewer.pal(11, "RdBu"),
                                           "Viridis" = viridis::viridis(10),
                                           "Plasma" = viridis::plasma(10),
                                           "Magma" = viridis::magma(10))) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Heatmap", x = "Échantillons", y = "Gènes", fill = "Valeur")
  }
  
  output$download_png <- downloadHandler(
    filename = function() { "heatmap.png" },
    content = function(file) {
      ggsave(file, plot = generate_ggplot_heatmap(), device = "png", width = 10, height = 8, dpi = 300)
    }
  )
  
  output$download_pdf <- downloadHandler(
    filename = function() { "heatmap.pdf" },
    content = function(file) {
      ggsave(file, plot = generate_ggplot_heatmap(), device = "pdf", width = 10, height = 8)
    }
  )
}

shinyApp(ui, server)

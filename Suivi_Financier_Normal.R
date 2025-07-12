library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(plotly)
library(viridis)
library(DT)

chemin_budget <- "C:/Users/HP/Desktop/Indicateurs_Rshiny/suivi_financier.xlsx"
#chemin_decaissement <- "C:/Users/HP/Desktop/Projet_02/Normal/data/Base.xlsx"
chemin<- "C:/Users/HP/Desktop/Projet_02/Normal/data/Base.xlsx"

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "📊 Suivi Financier PUDC 2025"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Repartion du Budget", tabName = "budget", icon = icon("pie-chart")),
      menuItem("Décaissements", tabName = "decaissements", icon = icon("money-bill")),
      menuItem("Exécution budgétaire", tabName = "execution", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
    /* HEADER */
    .main-header .logo {
      background-color: #0B2545 !important; /* Bleu foncé */
      color: white !important;
      font-weight: bold;
      font-size: 22px;
      text-align: center;
    }
    .main-header .navbar {
      background-color: #0B2545 !important;
    }
    /* SIDEBAR foncé */
    .main-sidebar {
      background-color: #0B2545 !important;
    }
    .main-sidebar .sidebar .sidebar-menu .active > a {
      background-color: #061930 !important; /* Bleu très foncé */
      color: white !important;
    }
    /* BODY */
    .content-wrapper, .right-side {
      background-color: #f0f4f8 !important;
      padding-bottom: 50px;
    }
    /* TITRES BOX EN BLEU FONCÉ */
    .box-header.with-border {
      background-color: #0B2545 !important;
      color: white !important;
      font-weight: 700;
      font-size: 16px;
    }
    /* BOX FILTRES */
    .box.box-primary {
      background-color: #0B2545 !important;
      color: white !important;
      font-weight: bold;
      font-size: 16px;
      border-top-color: #061930 !important;
    }
    /* selectInput filtres */
    .form-control {
      border-radius: 6px !important;
      border: 1.5px solid #061930 !important;
      padding: 6px 10px !important;
      color: white !important;
      background-color: #0B2545 !important;
      font-weight: 600 !important;
    }
    /* Fonds graphiques - Budget (bleu clair pastel) */
    .budget-graph-box {
      background-color: #D6E6F5 !important;
    }
    /* Fonds graphiques - Décaissements (vert clair pastel) */
    .decaissements-graph-box {
      background-color: #D9F0E3 !important;
    }
    /* Fonds graphiques - Exécution (rouge clair pastel) */
    .execution-graph-box {
      background-color: #F5D6D6 !important;
    }
  "))
    )
    ,
    tabItems(
      # Budget
      tabItem(tabName = "budget",
              fluidRow(
                column(width = 12,
                       box(width = NULL, title = "Filtres Budget", status = "primary", solidHeader = TRUE,
                           selectInput("volet_budget", "Composante (Volet) :", choices = NULL)
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(width = NULL, title = "Budget total par sous-composante", status = "primary", solidHeader = TRUE,
                           class = "budget-graph-box",
                           plotlyOutput("barplot_souscompo", height = 450)
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(width = NULL, title = "Répartition du budget par source", status = "primary", solidHeader = TRUE,
                           class = "budget-graph-box",
                           plotlyOutput("donut_source", height = 300)
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(width = NULL, title = "Budget par sous-composante et source", status = "primary", solidHeader = TRUE,
                           class = "budget-graph-box",
                           plotlyOutput("barplot_detail", height = 500)
                       )
                ),
                column(width = 12,
                       box(width = NULL, title = "Budget total par composante", status = "primary", solidHeader = TRUE,
                           class = "budget-graph-box",
                           plotlyOutput("barplot_composante", height = 400)
                       )
                )
              )
      ),
      # Décaissements
      tabItem(tabName = "decaissements",
              fluidRow(
                column(width = 12,
                       box(width = NULL, title = "Filtres Décaissements", status = "primary", solidHeader = TRUE,
                           selectInput("projet_dec", "Projet :", choices = NULL)
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       tabsetPanel(
                         tabPanel(
                                  fluidRow(
                                    column(12,
                                           box(width = NULL, title = "Montants décaissés par trimestre", status = "primary", solidHeader = TRUE,
                                               class = "decaissements-graph-box",
                                               plotlyOutput("barplot_trimestres_dec", height = 400))
                                    ),
                                    column(12,
                                           box(width = NULL, title = "Montants décaissés par source", status = "primary", solidHeader = TRUE,
                                               class = "decaissements-graph-box",
                                               plotlyOutput("donut_dec", height = 400))
                                    ),
                                    column(12,
                                           box(width = NULL, title = "Budget global vs Montant décaissé", status = "primary", solidHeader = TRUE,
                                               class = "decaissements-graph-box",
                                               plotlyOutput("comparaison_dec", height = 400))
                                    )
                                  )
                         )
                       )
                )
              )
      ),
      # Exécution
      tabItem(tabName = "execution",
              fluidRow(
                column(width = 12,
                       box(width = NULL, title = "Filtres Exécution budgétaire", status = "primary", solidHeader = TRUE,
                           selectInput("projet_exec", "Projet :", choices = NULL)
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(width = NULL, title = "Budget vs Montant réalisé par projet", status = "primary", solidHeader = TRUE,
                           class = "execution-graph-box",
                           plotlyOutput("barplot_budget_vs_realise", height = 350)),
                       box(width = NULL, title = "Taux d'exécution budgétaire moyen par projet", status = "primary", solidHeader = TRUE,
                           class = "execution-graph-box",
                           plotlyOutput("barplot_taux_execution", height = 350)),
                       box(width = NULL, title = "Montants réalisés par trimestre par projet", status = "primary", solidHeader = TRUE,
                           class = "execution-graph-box",
                           plotlyOutput("barplot_trimestres_exec", height = 350)),
                       box(width = NULL, title = "Répartition des montants réalisés par source", status = "primary", solidHeader = TRUE,
                           class = "execution-graph-box",
                           plotlyOutput("donut_exec", height = 350))
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # --- Repartition de Budget par projet ---
  data_budget <- reactive({
    df <- read_excel(chemin_budget, sheet = "Normal")
    colnames(df)[1:6] <- c("Volet", "Sous_composante", "Source_financement", "Budget_FCFA",
                           "Total_Sous_composante_FCFA", "Total_Composantes_FCFA")
    df <- tidyr::fill(df, Volet, Sous_composante, .direction = "down")
    df <- df %>%
      mutate(across(c(Budget_FCFA, Total_Sous_composante_FCFA, Total_Composantes_FCFA),
                    ~as.numeric(gsub("[^0-9.-]", "", as.character(.)))))
    df <- df %>% filter(tolower(Volet) != "total")
    return(df)
  })
  
  observe({
    volets <- unique(data_budget()$Volet)
    updateSelectInput(session, "volet_budget", choices = c("Tous", volets), selected = "Tous")
  })
  
  output$barplot_souscompo <- renderPlotly({
    df <- data_budget()
    df_totaux <- df %>% filter(!is.na(Total_Sous_composante_FCFA) & Total_Sous_composante_FCFA > 0)
    if (!is.null(input$volet_budget) && input$volet_budget != "Tous") {
      df_totaux <- df_totaux %>% filter(Volet == input$volet_budget)
    }
    req(nrow(df_totaux) > 0)
    df_totaux <- df_totaux %>% arrange(desc(Total_Sous_composante_FCFA))
    plot_ly(df_totaux, x = ~Sous_composante, y = ~Total_Sous_composante_FCFA, type = "bar",
            text = ~format(Total_Sous_composante_FCFA, big.mark = " ", scientific = FALSE),
            textposition = "auto",
            marker = list(color = viridis(nrow(df_totaux)), line = list(color = "black", width = 1))
    ) %>% layout(title = paste("Budget total par sous-composante",
                               ifelse(input$volet_budget == "Tous", "pour toutes les composantes",
                                      paste("pour", input$volet_budget))),
                 xaxis = list(title = "Sous-composante", tickangle = -45),
                 yaxis = list(title = "Budget total (FCFA)"))
  })
  
  output$donut_source <- renderPlotly({
    df <- data_budget()
    df_filtre <- if (is.null(input$volet_budget) || input$volet_budget == "Tous") df else df %>% filter(Volet == input$volet_budget)
    df_sum <- df_filtre %>% group_by(Source_financement) %>%
      summarise(Budget_total = sum(Budget_FCFA, na.rm = TRUE)) %>%
      arrange(desc(Budget_total))
    req(nrow(df_sum) > 0)
    plot_ly(df_sum, labels = ~Source_financement, values = ~Budget_total, type = "pie", hole = 0.6,
            textinfo = "label+percent", insidetextorientation = "radial",
            marker = list(colors = viridis(nrow(df_sum)))) %>%
      layout(title = paste("Répartition budget par source",
                           ifelse(input$volet_budget == "Tous", "pour toutes les composantes",
                                  paste("pour", input$volet_budget))))
  })
  
  output$barplot_detail <- renderPlotly({
    df <- data_budget()
    df_filtre <- if (is.null(input$volet_budget) || input$volet_budget == "Tous") df else df %>% filter(Volet == input$volet_budget)
    df_sum <- df_filtre %>% group_by(Sous_composante, Source_financement) %>%
      summarise(Budget_total = sum(Budget_FCFA, na.rm = TRUE)) %>%
      arrange(desc(Budget_total))
    req(nrow(df_sum) > 0)
    plot_ly(df_sum, x = ~Sous_composante, y = ~Budget_total, color = ~Source_financement, type = "bar",
            text = ~format(Budget_total, big.mark = " ", scientific = FALSE), textposition = "auto") %>%
      layout(title = paste("Budget par sous-composante et source",
                           ifelse(input$volet_budget == "Tous", "toutes composantes",
                                  paste("pour", input$volet_budget))),
             xaxis = list(title = "Sous-composante", tickangle = -45),
             yaxis = list(title = "Budget (FCFA)"),
             barmode = "stack")
  })
  
  output$barplot_composante <- renderPlotly({
    df <- data_budget()
    df_totaux <- df %>% filter(!is.na(Total_Sous_composante_FCFA) & Total_Sous_composante_FCFA > 0)
    df_sum <- df_totaux %>% group_by(Volet) %>%
      summarise(Budget_total = sum(Total_Sous_composante_FCFA, na.rm = TRUE)) %>%
      arrange(desc(Budget_total))
    req(nrow(df_sum) > 0)
    x_labels <- paste("Composante", seq_len(nrow(df_sum)))
    plot_ly(df_sum, x = ~x_labels, y = ~Budget_total, type = "bar",
            text = ~format(Budget_total, big.mark = " ", scientific = FALSE),
            textposition = "auto",
            marker = list(color = viridis(nrow(df_sum)), line = list(color = "black", width = 1))
    ) %>% layout(title = "Budget total par composante",
                 xaxis = list(title = "Composante", tickvals = x_labels, ticktext = x_labels),
                 yaxis = list(title = "Budget total (FCFA)"))
  })
  
  
  # Décaissements
  
  data_decaissement <- reactive({
    df <- read_excel(chemin, sheet = "DECAISSEMENT GLOBAUX 2025", skip = 1)
    colnames(df)[1:8] <- c("Projets", "SourceFinancement", "BudgetFCFA", "MontantDecaisse_31_12_2025",
                           "MontantDecaisse_T1_2025", "MontantDecaisse_T2_2025",
                           "MontantDecaisse_T3_2025", "MontantDecaisse_T4_2025")
    df <- tidyr::fill(df, Projets, .direction = "down")
    df <- df[!grepl("^TOTAL", df$Projets, ignore.case = TRUE), ]
    cols <- c("BudgetFCFA", "MontantDecaisse_31_12_2025", "MontantDecaisse_T1_2025",
              "MontantDecaisse_T2_2025", "MontantDecaisse_T3_2025", "MontantDecaisse_T4_2025")
    df[cols] <- lapply(df[cols], function(x) parse_number(as.character(x)))
    return(df)
  })
  
  observe({
    df <- data_decaissement()
    projets <- unique(df$Projets)
    projets <- projets[!is.na(projets)]
    projets <- projets[!grepl("^TOTAL", projets, ignore.case = TRUE)]
    projets <- sort(projets)
    updateSelectInput(session, "projet_dec", choices = c("Tous", projets), selected = "Tous")
  })
  
  data_filtrée_dec <- reactive({
    df <- data_decaissement()
    if (!is.null(input$projet_dec) && input$projet_dec != "Tous") {
      df <- df[df$Projets == input$projet_dec, ]
    }
    return(df)
  })
  
  output$donut_dec <- renderPlotly({
    df <- data_filtrée_dec()
    req(nrow(df) > 0)
    df_agg <- df %>% group_by(SourceFinancement) %>% summarise(Montant = sum(MontantDecaisse_31_12_2025, na.rm = TRUE))
    plot_ly(df_agg, labels = ~SourceFinancement, values = ~Montant, type = "pie", hole = 0.6,
            textinfo = "label+percent", insidetextorientation = "radial",
            marker = list(colors = viridis(nrow(df_agg)))) %>%
      layout(title = paste("Montants décaissés par source - Projet :", input$projet_dec))
  })
  
  output$comparaison_dec <- renderPlotly({
    df <- data_filtrée_dec()
    req(nrow(df) > 0)
    budget_total <- sum(df$BudgetFCFA, na.rm = TRUE)
    montant_decaissé <- sum(df$MontantDecaisse_31_12_2025, na.rm = TRUE)
    df_comp <- data.frame(Type = c("Budget global", "Montant décaissé"), Montant = c(budget_total, montant_decaissé))
    plot_ly(df_comp, x = ~Type, y = ~Montant, type = "bar",
            text = ~format(Montant, big.mark = " ", scientific = FALSE),
            textposition = "auto",
            marker = list(color = c("#1f77b4", "#ff7f0e"))) %>%
      layout(title = paste("Comparaison Budget global vs Montant décaissé - Projet :", input$projet_dec),
             yaxis = list(title = "Montant (FCFA)"))
  })
  
  output$barplot_trimestres_dec <- renderPlotly({
    df <- data_filtrée_dec()
    req(nrow(df) > 0)
    df_sum <- df %>% summarise(
      T1 = sum(MontantDecaisse_T1_2025, na.rm = TRUE),
      T2 = sum(MontantDecaisse_T2_2025, na.rm = TRUE),
      T3 = sum(MontantDecaisse_T3_2025, na.rm = TRUE),
      T4 = sum(MontantDecaisse_T4_2025, na.rm = TRUE)
    ) %>% tidyr::pivot_longer(everything(), names_to = "Trimestre", values_to = "Montant")
    plot_ly(df_sum, x = ~Trimestre, y = ~Montant, type = "bar",
            text = ~format(Montant, big.mark = " ", scientific = FALSE), textposition = "auto",
            marker = list(color = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"))) %>%
      layout(title = paste("Montants décaissés par trimestre - Projet :", input$projet_dec),
             yaxis = list(title = "Montant (FCFA)"), xaxis = list(title = "Trimestre"))
  })
  
  # Exécution budgétaire
  
  data_execution <- reactive({
    df <- read_excel(chemin, sheet = "Execution budgetaire PTBA 2025", skip = 1)
    colnames(df)[1:10] <- c("Projets", "SourceFinancement", "BudgetFCFA", "MontantRealise_T1", "TauxEx_T1",
                            "MontantRealise_T2", "TauxEx_T2", "MontantRealise_T3", "TauxEx_T3", "MontantRealise_T4", "TauxEx_T4")[1:ncol(df)]
    df <- tidyr::fill(df, Projets, .direction = "down")
    df <- df[!grepl("^TOTAL", df$Projets, ignore.case = TRUE), ]
    cols_numeric <- c("BudgetFCFA", "MontantRealise_T1", "MontantRealise_T2", "MontantRealise_T3", "MontantRealise_T4",
                      "TauxEx_T1", "TauxEx_T2", "TauxEx_T3", "TauxEx_T4")
    cols_numeric <- intersect(cols_numeric, names(df))
    df[cols_numeric] <- lapply(df[cols_numeric], function(x) parse_number(as.character(x)))
    return(df)
  })
  
  observe({
    df <- data_execution()
    projets <- unique(df$Projets)
    projets <- projets[!is.na(projets)]
    projets <- projets[!grepl("^TOTAL", projets, ignore.case = TRUE)]
    projets <- sort(projets)
    updateSelectInput(session, "projet_exec", choices = c("Tous", projets), selected = "Tous")
  })
  
  data_filtrée_exec <- reactive({
    df <- data_execution()
    if (!is.null(input$projet_exec) && input$projet_exec != "Tous") {
      df <- df[df$Projets == input$projet_exec, ]
    }
    return(df)
  })
  
  output$barplot_budget_vs_realise <- renderPlotly({
    df <- data_filtrée_exec()
    req(nrow(df) > 0)
    df_sum <- df %>% group_by(Projets) %>%
      summarise(
        Budget = first(BudgetFCFA),
        Realise = sum(MontantRealise_T1, MontantRealise_T2, MontantRealise_T3, MontantRealise_T4, na.rm = TRUE)
      ) %>% rename(Projet = Projets)
    df_long <- tidyr::pivot_longer(df_sum, cols = c("Budget", "Realise"), names_to = "Type", values_to = "Montant")
    plot_ly(df_long, x = ~Projet, y = ~Montant, color = ~Type, type = "bar",
            text = ~format(Montant, big.mark = " ", scientific = FALSE), textposition = "auto") %>%
      layout(title = "Budget vs Montant réalisé par projet",
             yaxis = list(title = "Montant (FCFA)"),
             xaxis = list(title = "Projets", tickangle = -45),
             barmode = "group")
  })
  
  output$barplot_taux_execution <- renderPlotly({
    df <- data_filtrée_exec()
    req(nrow(df) > 0)
    df_taux <- df %>% group_by(Projets) %>%
      summarise(TauxMoyen = mean(c_across(starts_with("TauxEx_")), na.rm = TRUE)) %>%
      rename(Projet = Projets)
    plot_ly(df_taux, x = ~Projet, y = ~TauxMoyen, type = "bar",
            text = ~paste0(round(TauxMoyen, 2), "%"), textposition = "auto",
            marker = list(color = viridis(nrow(df_taux)))) %>%
      layout(title = "Taux d'exécution budgétaire moyen par projet",
             yaxis = list(title = "Taux (%)"),
             xaxis = list(title = "Projets", tickangle = -45))
  })
  
  output$barplot_trimestres_exec <- renderPlotly({
    df <- data_filtrée_exec()
    req(nrow(df) > 0)
    df_long <- df %>%
      select(Projets, starts_with("MontantRealise_T")) %>%
      tidyr::pivot_longer(-Projets, names_to = "Trimestre", values_to = "Montant") %>%
      mutate(Trimestre = sub("MontantRealise_", "", Trimestre))
    plot_ly(df_long, x = ~Trimestre, y = ~Montant, color = ~Projets, type = "bar",
            text = ~format(Montant, big.mark = " ", scientific = FALSE), textposition = "auto") %>%
      layout(title = "Montants réalisés par trimestre par projet",
             yaxis = list(title = "Montant (FCFA)"),
             xaxis = list(title = "Trimestre"),
             barmode = "stack")
  })
  
  output$donut_exec <- renderPlotly({
    df <- data_filtrée_exec()
    req(nrow(df) > 0)
    df_agg <- df %>% group_by(SourceFinancement) %>% summarise(Montant = sum(BudgetFCFA, na.rm = TRUE))
    plot_ly(df_agg, labels = ~SourceFinancement, values = ~Montant, type = "pie", hole = 0.6,
            textinfo = "label+percent", insidetextorientation = "radial",
            marker = list(colors = viridis(nrow(df_agg)))) %>%
      layout(title = paste("Montants réalisés par source - Projet :", input$projet_exec))
  })
  
}

shinyApp(ui, server)

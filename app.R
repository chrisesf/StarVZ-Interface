library(shiny)
library(shinyjs)
library(shinyFiles)
library(ggplot2)
library(starvz)
library(tibble)
library(plotly)
library(dplyr)
library(tidyr)
library(fs)

source("plotly_panel.R")

#TODO passar tudo pra ingles

# --- UI Components ---
sidebar_ui <- function() {
  div(class = "sidebar",
      h3("Load Data"),
      shinyDirButton("data_dir", "Select Directory", "Choose the folder with the data"),
      verbatimTextOutput("selected_dir", placeholder = TRUE),
      
      fileInput("config_file", "Choose Configuration File", accept = c(".yaml", ".yml")),
      actionButton("load_data", "Load Data", class = "btn"),
      hr(),
      
      h3("Panels"),
      checkboxInput("st", "st", value = TRUE),
      checkboxInput("submitted", "Submitted", value = TRUE),
      checkboxInput("starpu", "Starpu", value = TRUE),
      checkboxInput("ready", "Ready", value = TRUE),
      
      h3("Features"),
      conditionalPanel("input.st == true",
                       checkboxInput("idleness", "Idleness", FALSE),
                       conditionalPanel("input.idleness == true",
                                        checkboxInput("idleness_all", "Idleness All", FALSE)
                       ),
                       checkboxInput("legend", "Legend", FALSE),
                       checkboxInput("makespan", "Makespan", FALSE),
                       checkboxInput("cpb", "CPB", FALSE),
                       checkboxInput("outliers", "Outliers", FALSE),
                       checkboxInput("tasks_active", "Tasks", FALSE),
                       conditionalPanel("input.tasks_active == true",
                                        numericInput("tasks_levels", "Levels", value = 3, min = 1, step = 1),
                                        textInput("tasks_list", "List (comma-separated)", value = "")
                       )
      ),
      
      conditionalPanel("input.starpu == true",
                       checkboxInput("cpb_mpi_active", "CPB MPI", value = FALSE)
      ),
      
      selectInput("labels", "Choose Labels:",
                  choices = c("ALL", "1CPU_per_NODE", "1GPU_per_NODE", "FIRST_LAST", "NODES_only", "NODES_1_in_10", "1CPU_1GPU", "ALL_nompi"),
                  selected = "FIRST_LAST"),
      
      actionButton("plotly_button", "Generate Interactive Plot", class = "btn"),
      actionButton("back_button", "Back to Static Plot", class = "btn", style = "display: none;")
  )
}

calculate_height <- function(input) {
  base_height <- 600
  max_height <- 1000
  step_size <- 100
  active_checkboxes <- sum(c(input$starpu, input$st, input$submitted, input$ready,
                             input$cpb, input$cpb_mpi_active, input$tasks_active, input$outliers))
  adjusted_height <- base_height + (active_checkboxes * step_size)
  min(max(adjusted_height, base_height), max_height)
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(includeCSS("www/style.css")),
  div(class = "container",
      sidebar_ui(),
      div(class = "main", uiOutput("plot_ui"))
  )
)

server <- function(input, output, session) {
  shinyDirChoose(input, "data_dir", roots = c(home = fs::path_home()), session = session)
  
  output$selected_dir <- renderText({
    req(input$data_dir)
    parseDirPath(c(home = fs::path_home()), input$data_dir)
  })
  
  rv <- reactiveValues(dado = NULL, data_dir = NULL, config_file = NULL, phase1_log = NULL)
  
  observeEvent(input$load_data, {
    req(input$data_dir, input$config_file)
    rv$data_dir <- parseDirPath(c(home = fs::path_home()), input$data_dir)
    rv$config_file <- input$config_file$datapath
    
    parquet_files <- fs::dir_ls(rv$data_dir, glob = "*.parquet")
    
    if (length(parquet_files) == 0) {
      showModal(modalDialog(
        title = "Arquivos não encontrados",
        "Nenhum arquivo .parquet foi encontrado. Deseja rodar o Phase 1?",
        footer = tagList(
          modalButton("Cancelar"),
          actionButton("run_phase1", "Rodar Phase 1")
        )
      ))
      return()
    }
    
    rv$dado <- starvz_read(paste0(rv$data_dir, "/"), rv$config_file, FALSE)
  })
  
  observeEvent(input$run_phase1, {
    removeModal()
    
    trace_files <- fs::dir_ls(rv$data_dir, glob = "*.trace")
    
    if (length(trace_files) == 0) {
      shinyjs::alert("Nenhum arquivo .trace foi encontrado. Não é possível rodar o Phase 1.")
      return()
    }
    
    withProgress(message = "Executando Phase 1...", value = 0.5, {
      result <- tryCatch({
        system2(
          command = "startvz",
          args = c("-1", "--use-paje-trace"),
          stdout = TRUE,
          stderr = TRUE,
          wd = rv$data_dir
        )
      }, error = function(e) {
        return(paste("Erro ao rodar startvz:", e$message))
      })
      
      rv$phase1_log <- paste(result, collapse = "\n")
    })
    
    parquet_files <- fs::dir_ls(rv$data_dir, glob = "*.parquet")
    
    if (length(parquet_files) > 0) {
      shinyjs::alert("Phase 1 rodado com sucesso! Clique em 'Load Data' novamente para carregar os .parquet.")
    } else {
      shinyjs::alert("Phase 1 foi executado, mas nenhum .parquet foi gerado. Verifique o log abaixo.")
    }
  })
  
  output$plot_ui <- renderUI({
    tagList(
      plotOutput("plot", width = "1100px", height = paste0(calculate_height(input), "px")),
      verbatimTextOutput("log")
    )
  })
  
  output$plot <- renderPlot({
    req(rv$dado)
    starvz_plot(rv$dado)
  })
  
  output$log <- renderText({
    rv$phase1_log
  })
}

shinyApp(ui = ui, server = server)

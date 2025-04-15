library(shiny)
library(shinyjs)
library(shinyFiles)
library(ggplot2)
library(starvz)
library(tibble)
library(plotly)

source("plotly_panel.R")

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Roboto:wght@300;400;700&display=swap"),
    tags$style(HTML("
    body {
      font-family: 'Roboto', sans-serif;
      background-color: #f8f9fa;
      color: #333;
      margin: 0;
      padding: 0;
    }

    .container {
      display: flex;
      flex-direction: row;
      gap: 0;
      padding: 0;
      margin: 0;
    }

    .sidebar {
      background-color: #2c3e50;
      color: white;
      padding: 30px;
      border-radius: 10px;
      box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
      width: 400px;
      min-height: 100vh;
      position: relative;
    }

    .sidebar h3 {
      border-bottom: 2px solid #3498db;
      padding-bottom: 5px;
      margin-bottom: 10px;
    }

    .main {
      flex-grow: 1;
      display: flex;
      flex-direction: column;
      gap: 30px;
      padding: 20px;
    }

    .btn {
      background-color: #3498db;
      color: white;
      border: none;
      border-radius: 5px;
      padding: 12px 18px;
      cursor: pointer;
      transition: background 0.3s ease;
    }

    .btn:hover {
      background-color: #2980b9;
    }

    input[type='checkbox'] {
      accent-color: #3498db;
    }

    select, input, .btn {
      margin-top: 8px;
      width: 100%;
    }

    .shiny-bound-output {
      padding: 10px;
      background: white;
      border-radius: 5px;
      box-shadow: 2px 2px 8px rgba(0, 0, 0, 0.1);
    }

    .shiny-plot-output {
      border-radius: 10px;
      box-shadow: 4px 4px 15px rgba(0, 0, 0, 0.15);
      flex-grow: 1;
      min-width: 1100px;
      min-height: 600px;
    }
  "))
  ),
  
  div(class = "container",
      div(class = "sidebar",
          h3("Carregar Dados"),
          shinyDirButton("data_dir", "Selecionar Diretório", "Escolha a pasta com os dados"),
          verbatimTextOutput("selected_dir", placeholder = TRUE),
          
          fileInput("config_file", "Escolher Arquivo de Configuração", 
                    accept = c(".yaml", ".yml")),
          
          actionButton("load_data", "Carregar Dados", class = "btn"),
          hr(),
          
          h3("Features"),
          
          conditionalPanel(
            condition = "input.st == true",  # Garantir que 'st' esteja ativado
            checkboxInput("idleness_all", "Ativar Idleness All", FALSE),
            checkboxInput("legend", "Ativar Legend", FALSE),
            checkboxInput("makespan", "Ativar Makespan", FALSE),
            checkboxInput("cpb", "Ativar CPB", FALSE),
            checkboxInput("outliers", "Ativar Outliers", FALSE),
            checkboxInput("tasks_active", "Ativar Tasks", FALSE),
            conditionalPanel(
              condition = "input.tasks_active == true",  # Apenas exibe se 'tasks_active' for true
              numericInput("tasks_levels", "Levels", value = 3, min = 1, step = 1),
              textInput("tasks_list", "List (separado por vírgula)", value = "a,b,c")
            ),
          ),
          
          conditionalPanel(
            condition = "input.starpu == true",
            checkboxInput("cpb_mpi_active", "Ativar CPB MPI", value = FALSE),
            # checkboxInput("outliers", "Ativar Outliers", value = FALSE)
          ),
          
          selectInput("labels", "Escolha os Labels:", 
                      choices = c("ALL", "1CPU_per_NODE", "1GPU_per_NODE", "FIRST_LAST", "NODES_only", "NODES_1_in_10", "1CPU_1GPU", "ALL_nompi"), 
                      selected = "FIRST_LAST"),
          
          h3("Panels"),
          
          checkboxInput("st", "Ativar st", value = TRUE),
          checkboxInput("submitted", "Ativar Submitted", value = TRUE),
          checkboxInput("starpu", "Ativar Starpu", value = TRUE),
          checkboxInput("ready", "Ativar Ready", value = TRUE),
          
          actionButton("plotly_button", "Gerar Gráfico Interativo", class = "btn"),
          actionButton("back_button", "Voltar para o Gráfico Normal", class = "btn", style = "display: none;")
      ),
      
      div(class = "main",
          uiOutput("plot_ui")
      )
  )
)


# Função para atualizar as configurações do dado
update_config <- function(dado, input) {
  dado$config$st <- list(
    active = input$st,
    labels = input$labels,
    legend = input$legend,
    makespan = input$makespan,
    cpb = input$cpb,
    idleness_all = input$idleness_all,
    cpb_mpi = list(active = input$cpb_mpi_active),
    tasks = list(
      active = input$tasks_active,
      levels = ifelse(is.null(input$tasks_levels) || input$tasks_levels == "", 3, as.integer(input$tasks_levels)),
      list = ifelse(is.null(input$tasks_list) || input$tasks_list == "", character(0), unlist(strsplit(input$tasks_list, split = ",")))
    ),
    outliers = input$outliers
  )
  
  dado$config$starpu$active <- input$starpu
  dado$config$submitted$active <- input$submitted
  dado$config$ready$active <- input$ready
  
  return(dado)
}

# Server
server <- function(input, output, session) {
  library(fs)
  
  # Inicializar as pastas no sistema
  shinyDirChoose(input, "data_dir", roots = c(home = fs::path_home()), session = session)
  
  # Observar diretório selecionado
  output$selected_dir <- renderText({
    req(input$data_dir)
    parseDirPath(c(home = fs::path_home()), input$data_dir)
  })
  
  # Variável reativa para armazenar os dados carregados
  rv <- reactiveValues(dado = NULL)
  
  # Carregar dados quando o botão é clicado
  observeEvent(input$load_data, {
    req(input$data_dir, input$config_file)
    
    # Caminho do diretório e arquivo de configuração
    data_dir <- parseDirPath(c(home = fs::path_home()), input$data_dir)
    config_file <- input$config_file$datapath
    
    # Carregar os dados usando starvz_read
    rv$dado <- starvz_read(paste0(data_dir, "/"), config_file, FALSE)
    
    # Configurações iniciais
    rv$dado$config$st$labels <- input$labels
    rv$dado$config$starpu$active <- input$starpu
    rv$dado$config$submitted$active <- input$submitted
    rv$dado$config$st$outliers <- input$outliers
  })
  
  # Calcular a altura ajustada
  calculate_height <- function(input) {
    base_height <- 600
    max_height <- 1000
    step_size <- 100
    
    active_checkboxes <- sum(c(input$starpu, input$st, input$submitted, input$ready, 
                               input$cpb, input$cpb_mpi_active, input$tasks_active, input$outliers))
    
    adjusted_height <- base_height + (active_checkboxes * step_size)
    
    return(min(max(adjusted_height, base_height), max_height))
  }
  
  # Renderizar a UI para o gráfico
  output$plot_ui <- renderUI({
    adjusted_height <- calculate_height(input)
    
    plotOutput("plot", width = "1100px", height = paste0(adjusted_height, "px"))
  })
  
  # Renderizar o gráfico com ggplot
  output$plot <- renderPlot({
    req(rv$dado)
    dado <- update_config(rv$dado, input)
    starvz_plot(dado)
  })
  
  # Gerar o gráfico com Plotly
  observeEvent(input$plotly_button, {
    req(rv$dado)
    dado <- update_config(rv$dado, input)
    dado$config$submitted$active <- FALSE
    dado$config$starpu$active <- FALSE
    dado$config$ready$active <- FALSE
    
    plotly_obj <- panel_st_plotly_native_v1(dado)
    
    output$plot_ui <- renderUI({
      plotlyOutput("plotly_plot", width = "100%", height = "800px")
    })
    
    output$plotly_plot <- renderPlotly({
      plotly_obj
    })
    
    shinyjs::show("back_button")
    shinyjs::hide("plotly_button")
  })
  
  # Voltar para o gráfico normal
  observeEvent(input$back_button, {
    adjusted_height <- calculate_height(input)
    
    output$plot_ui <- renderUI({
      plotOutput("plot", width = "100%", height = paste0(adjusted_height, "px"))
    })
    
    shinyjs::show("plotly_button")
    shinyjs::hide("back_button")
  })
}

# Executar a aplicação Shiny
shinyApp(ui = ui, server = server)

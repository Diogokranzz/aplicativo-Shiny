# app.R

# Carregar pacotes necessários
library(shiny)
library(ggplot2)
library(DT)

# Definir a UI
ui <- fluidPage(
  tags$head(
    # Adicionando CSS para centralização
    tags$style(HTML("
      body {
        background-color: #e0f7fa;  /* Cor de fundo suave */
        color: #333;
      }
      .container {
        padding: 20px;
        max-width: 800px;
        margin: 0 auto; /* Centraliza a div */
        background-color: white; /* Fundo branco para a área do conteúdo */
        border-radius: 8px; /* Bordas arredondadas */
        box-shadow: 0 2px 10px rgba(0, 0, 0, 0.1); /* Sombra suave */
      }
      .title {
        text-align: center;
        margin-bottom: 20px;
      }
      .sidebar {
        display: flex;
        flex-direction: column;
        align-items: center; /* Centraliza os inputs */
      }
      .sidebarPanel, .mainPanel {
        display: flex;
        flex-direction: column;
        align-items: center; /* Centraliza os elementos na sidebar e no main */
        justify-content: center; /* Alinha verticalmente */
      }
    "))
  ),
  
  div(class = "container",
      div(class = "title", 
          h1("Aplicativo Shiny")),
      
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          selectInput("dataset", "Escolha um conjunto de dados:",
                      choices = c("mtcars", "iris")),
          sliderInput("obs", "Número de observações:",
                      min = 1, max = 32, value = 10),
          
          # Filtro adicional para mtcars
          conditionalPanel(
            condition = "input.dataset == 'mtcars'",
            sliderInput("mpgFilter", "Filtrar por MPG:",
                        min = min(mtcars$mpg), max = max(mtcars$mpg), value = c(min(mtcars$mpg), max(mtcars$mpg))),
            selectInput("xvar", "Escolha variável X:", 
                        choices = c("wt" = "wt", "hp" = "hp")),
            selectInput("yvar", "Escolha variável Y:", 
                        choices = c("mpg" = "mpg", "qsec" = "qsec")),
            selectInput("color", "Escolha uma cor:", 
                        choices = c("blue", "red", "green", "purple")),
            actionButton("reset", "Resetar Filtros")
          )
        ),
        
        mainPanel(
          DTOutput("data"),
          plotOutput("plot", hover = hoverOpts("plot_hover")),
          verbatimTextOutput("hover_info"),
          downloadButton("downloadData", "Baixar Dados"),
          hr(),
          verbatimTextOutput("summary_stats")
        )
      )
  )
)

# Definir a lógica do servidor
server <- function(input, output, session) {
  
  # Cria uma função reativa para o conjunto de dados filtrados
  filtered_data <- reactive({
    dataset <- get(input$dataset)
    
    if (input$dataset == "mtcars") {
      dataset <- dataset[dataset$mpg >= input$mpgFilter[1] & dataset$mpg <= input$mpgFilter[2], ]
    }
    
    head(dataset, n = input$obs)
  })
  
  # Exibir o conjunto de dados selecionado
  output$data <- renderDT({
    datatable(filtered_data(), options = list(
      scrollX = TRUE,  # Permitir rolagem horizontal se necessário
      pageLength = 10,  # Número de linhas por página
      autoWidth = TRUE  # Largura automática para colunas
    ), width = '100%')  # Ajusta a largura da tabela
  })
  
  # Criar um gráfico com os dados selecionados
  output$plot <- renderPlot({
    dataset <- filtered_data()
    
    if (input$dataset == "mtcars") {
      ggplot(dataset, aes_string(x = input$xvar, y = input$yvar)) +
        geom_point(color = input$color, size = 3) +
        labs(title = "Gráfico de Dispersão: mtcars", x = input$xvar, y = input$yvar) +
        theme_minimal()
    } else {
      ggplot(dataset, aes_string(x = "Sepal.Width", y = "Sepal.Length")) +
        geom_point(color = input$color, size = 3) +
        labs(title = "Gráfico de Dispersão: iris", x = "Largura da Séssila", y = "Comprimento da Séssila") +
        theme_minimal()
    }
  })
  
  # Exibir informações de hover
  output$hover_info <- renderPrint({
    hover <- input$plot_hover
    if (!is.null(hover)) {
      dataset <- filtered_data()
      row_index <- round(hover$y)
      if (input$dataset == "mtcars") {
        info <- dataset[row_index, c(input$xvar, input$yvar)]
        paste("X:", info[1], "Y:", info[2])
      } else {
        paste("Largura:", hover$x, "Comprimento:", hover$y)
      }
    }
  })
  
  # Exibir estatísticas resumidas
  output$summary_stats <- renderPrint({
    dataset <- filtered_data()
    summary(dataset)
  })
  
  # Função para baixar os dados filtrados
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, "dados.csv", sep = "_")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Resetar filtros
  observeEvent(input$reset, {
    updateSliderInput(session, "mpgFilter", value = c(min(mtcars$mpg), max(mtcars$mpg)))
    updateSelectInput(session, "xvar", selected = "wt")
    updateSelectInput(session, "yvar", selected = "mpg")
    updateSelectInput(session, "color", selected = "blue")
  })
}

# Executar o aplicativo
shinyApp(ui = ui, server = server)
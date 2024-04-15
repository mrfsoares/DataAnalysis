
library(tidyverse)
library(ggplot2)
library(readxl)
library(shiny)
library(gridExtra)
library(shiny)
library(plotly)

# DADOS ========================================================================
df = readxl::read_xlsx('exp.xlsx', sheet='Resultado')

# teste plot
ggplot(df, aes(x=Ano, y=`Quilograma Líquido`)) + 
  geom_col()

# seleção de colunas
colunas = c('`Quilograma Líquido`', '`Valor FOB (US$)`')


# Dash =========================================================================
# Define a interface do usuário (UI)
ui <- fluidPage(
  titlePanel("Gráfico de Barras com ggplot2"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var_x", "Variável no Eixo X:", choices = 'Ano'),
      selectInput("var_y", "Variável no Eixo Y:", choices = colunas)
    ),
    
    mainPanel(
      plotOutput("barplot")
    )
  )
)

# Define a lógica do servidor
server <- function(input, output, session) {
  
  # Função para criar o gráfico de barras
  create_plot <- function() {
    ggplot(df, aes_string(x = input$var_x, y = input$var_y)) + 
      geom_col()
  }
  
  # Renderiza o gráfico
  output$barplot <- renderPlot({
    create_plot()
  })
}

# Cria o aplicativo Shiny
shinyApp(ui = ui, server = server)





# Dois plots --------------------------------------------------

# Define a interface do usuário (UI)
ui <- fluidPage(
  
  tags$head(
    tags$style(
      HTML("
      .custom-select {
        width: 100px; /* Defina a largura desejada para as caixas de seleção */
      }
      ")
    )
  ),
  
  titlePanel("Exportações - Mato Grosso do Sul"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("intervalo_x", "Ano:", min = min(df$Ano), max = max(df$Ano), value = c(min(df$Ano), max(df$Ano))),
      selectInput("var_y", "Variável:", choices = colunas, width = "200px")
    ),
    
    mainPanel(
      fluidRow(
        column(width = 6, plotOutput("plot1")),
        column(width = 6, plotOutput("plot2"))
      )
    )
  )
)

# Define a lógica do servidor
server <- function(input, output, session) {
  
  # Função para filtrar o conjunto de dados com base no intervalo de anos selecionado
  df2 <- reactive({
    subset(df, Ano >= input$intervalo_x[1] & Ano <= input$intervalo_x[2])
  })
  
  # Função para criar o gráfico de barras 1
  create_plot1 <- function() {
    ggplot(df2(), aes_string(x = "Ano", y = input$var_y)) + 
      geom_col(fill='skyblue') +
      labs(x = "", y = '', title = 'vs 1') +
      theme_minimal()
  }
  
  # Função para criar o gráfico de barras 2
  create_plot2 <- function() {
    ggplot(df2(), aes_string(x = "Ano", y = input$var_y)) + 
      geom_col(fill='#b2b3da') +
      labs(x = "", y = '', title = 'vs2') +
      theme_minimal()
  }
  
  # Renderiza o gráfico 1
  output$plot1 <- renderPlot({
    create_plot1()
  })
  
  # Renderiza o gráfico 2
  output$plot2 <- renderPlot({
    create_plot2()
  })
  
}

# Cria o aplicativo Shiny
shinyApp(ui = ui, server = server)




# Botom ======================================================================


library(tidyverse)
library(ggplot2)
library(readxl)
library(shiny)
library(gridExtra)
library(shiny)
library(plotly)

# DADOS ========================================================================
df = readxl::read_xlsx('exp.xlsx', sheet='Resultado')

# teste plot
ggplot(df, aes(x=Ano, y=`Quilograma Líquido`)) + 
  geom_col()

# seleção de colunas
colunas = c('`Quilograma Líquido`', '`Valor FOB (US$)`')

# Plots separados em abas ======================================================


# Define a interface do usuário (UI)
ui <- fluidPage(
  
  tags$head(
    tags$style(
      HTML("
      .custom-select {
        width: 100px; /* Defina a largura desejada para as caixas de seleção */
      }
      ")
    )
  ),
  
  titlePanel("Exportações - Mato Grosso do Sul"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("intervalo_x", "Ano:", min = min(df$Ano), max = max(df$Ano), value = c(min(df$Ano), max(df$Ano))),
      selectInput("var_y", "Variável:", choices = colunas, width = "200px")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Geral", plotlyOutput("plot1")),
        tabPanel('ANUAL', plotlyOutput("plot2"))
      )
    )
  )
)

# Define a lógica do servidor
server <- function(input, output, session) {
  
  # Função para filtrar o conjunto de dados com base no intervalo de anos selecionado
  df2 <- reactive({
    subset(df, Ano >= input$intervalo_x[1] & Ano <= input$intervalo_x[2])
  })
  
  # Função para criar o gráfico de barras 1
  create_plot1 <- function() {
    ggplot(df2(), aes_string(x = "Ano", y = input$var_y)) + 
      geom_col(fill='skyblue') +
      labs(x = "", y = '', title = 'vs 1') +
      theme_minimal()
  }
  
  # Função para criar o gráfico de barras 2
  create_plot2 <- function() {
    ggplot(df2(), aes_string(x = "Ano", y = input$var_y)) + 
      geom_col(fill='#b2b3da') +
      labs(x = "", y = '', title = 'vs2') +
      theme_minimal()
  }
  
  # Renderiza o gráfico 1
  output$plot1 <- renderPlotly({
    ggplotly(create_plot1())
  })
  
  # Renderiza o gráfico 2
  output$plot2 <- renderPlotly({
    ggplotly(create_plot2())
  })
  
}

# Cria o aplicativo Shiny
shinyApp(ui = ui, server = server)









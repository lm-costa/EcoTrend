library(shinylive)
library(httpuv)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(treemapify)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)
library(tidyverse)
library(httr)
library(writexl)

purrr::map(list.files('fun/',full.names = T),source)


header <- dashboardHeader(
  title = 'EcoTrend'
)

sidebar <- dashboardSidebar(
  sidebarMenu(

    # first page
    menuItem('Home', tabName = 'home',icon = icon('dashboard')),

    ## Pagina de estatisticas basicas
    menuItem("Estatisticas Básicas", tabName = 'basic',
             icon = icon('chart-bar')),
    selectInput(
      inputId = "select_stock",
      label = "Ativo",
      choices = c(
        "PETR4",
        "WIN25"
      ),
      selected = "PETR4",
      multiple = FALSE
    ),

    # Pagina de Previsão

    menuItem("Previsão de Preço", tabName = 'forescating',
             icon = icon('chart-line')),

    ## given options to make the plot
    textInput("cod_b3", "Código B3:", value = "PETR4"),
    numericInput('forecasted_min', "Previsão para (minutos):", value = 60),
    actionButton("run_model", "Iniciar Modelo")  # Adicionando o botão
  )
)

body <- dashboardBody(
  # My main page
  tabItems(
    tabItem(tabName = 'home',
            HTML("
              <h2><strong>Bem vindo ao EcoTrend</strong></h2>
              <p>Está é uma plataforma para previsão de preços das ações
              <strong>PETR4</strong> e <strong>WINM25</strong></p>
              <h3> Funcionalidades: </h3>
              <ul>
                <li><strong>Tendências</strong>: Aqui você encontra uma analise resumida dessas duas ações.</li>
                <li><strong>Previsão</strong>: Neste bloco, você pode fazer previsões para preços futuros baseados em IA.</li>
              </ul>
              <p>Use o menu à esquerda para acessar a seção de previsão de preços e comece a explorar os dados do mercado de forma inteligente e prática.</p>

          <p style='color: #888; font-size: 12px;'>
            <em>Aviso:</em> Este aplicativo tem fins exclusivamente educacionais e experimentais. As previsões aqui apresentadas não devem ser utilizadas como recomendação para compra ou venda de ativos financeiros. Tome decisões de investimento com base em múltiplas fontes e, se necessário, consulte profissionais qualificados.
          </p>
            ")
    ),

    ## Estatisticas page

    tabItem(
      tabName = 'basic',
      mainPanel(
        # sankey plot panel
        tabsetPanel(
          type='tabs',
          tabPanel(
            "Trend plot",
            plotOutput(
              'trend_plot'
            ),

          ),
          # data panel
          tabPanel(
            "Estatistica",
            DT::dataTableOutput('basic_data')
          )
        )
      )


    ),

    # Model Page
    tabItem(
      tabName = 'forescating',
      h2(
        plotOutput(
          'forecast_plot'
        )
      )

    )
    )
  )


# UI do aplicativo Shiny
ui <- dashboardPage(
  header,
  sidebar,
  body
)

# Server do aplicativo Shiny
server <- function(input, output) {
  # Trend
  ## plot output
  output$trend_plot <- renderPlot(
    trend_plot(cod_b3 = input$select_stock)
  )
  ## data output
  output$basic_data <-
    DT::renderDT(
      my_basic(cod_b3=input$select_stock)
    )
  ## Forecasting
  observeEvent(input$run_model, {  # Executa quando o botão for pressionado
    output$forecast_plot <- renderPlot({
      req(input$cod_b3, input$forecasted_min)
      load_data_and_train_model(input$cod_b3, input$forecasted_min)
    })
  })
}

# Roda o aplicativo Shiny
shinyApp(ui = ui, server = server)

# Packages
library(shiny)
library(bslib)
library(bsicons)
library(arrow)
library(dplyr)
library(ggplot2)
library(scales)

# Load data
sicom <- read_parquet(file = "dados_sicom.paquet")


ui <- page_sidebar(
  title = "Painel SICOM",
  sidebar = sidebar(
    
    selectInput( 
      inputId = "meio", 
      label = "Meio",
      choices = NULL, 
      multiple = TRUE  
    ),
    
    selectInput(
      inputId = "nome_veiculo", 
      label = "Nome do veículo",
      choices = NULL, 
      multiple = TRUE  
    )
  ),
  layout_column_wrap(
    width = 1/2, height = 200,
    value_box(
      title = "Valor total",
      value = textOutput("vtotal"),
      showcase = bs_icon("currency-dollar")
    ),
    value_box(
      title = "Ações",
      value = textOutput("nacoes"),
      showcase = bs_icon("cart-plus-fill")
    )
  ),
  card(
    card_header("Valor das ações"),
    plotOutput("p_valor_acoes")
  ),
  card(
    card_header("Quantidade de ações"),
    plotOutput("p_qtd_acoes")
  )
)

server <- function(input, output, session) {
  # Update choices
  updateSelectInput(
    session, "meio", 
    choices = sort(unique(sicom$meio))
  )
  
  nomes <- reactive({
    sicom %>%
      filter(meio %in% input$meio) %>%
      distinct(nome_do_veiculo) %>%
      arrange(nome_do_veiculo) %>%
      pull(nome_do_veiculo)
  })
  
  observe({
    updateSelectInput(
      session, "nome_veiculo",
      choices = nomes()
    )
  })
  
  
  output$p_valor_acoes <- renderPlot({
    req(input$nome_veiculo)
    
    sicom %>%
      filter(nome_do_veiculo %in% !!input$nome_veiculo) %>%
      group_by(nome_do_veiculo, ano_acao) %>%
      summarise(total = sum(valor_desembolso_anunciante_r, na.rm = TRUE)) %>%
      ungroup() %>%
      ggplot(aes(x = ano_acao, y = total, color = nome_do_veiculo)) +
      geom_line() +
      scale_y_continuous(labels = dollar_format(prefix = "R$ ", big.mark = ".", decimal.mark = ",")) + 
      theme_bw() +
      labs(x = "Ano da ação", y = "Valor desembolsado", color = "Veículo") +
      theme(legend.position = "bottom", legend.direction = "horizontal")
  })
  
  output$p_qtd_acoes <- renderPlot({
    req(input$nome_veiculo)
    
    sicom %>%
      filter(nome_do_veiculo %in% !!input$nome_veiculo) %>%
      group_by(nome_do_veiculo, ano_acao) %>%
      summarise(total = n()) %>%
      ungroup() %>%
      ggplot(aes(x = ano_acao, y = total, color = nome_do_veiculo)) +
      geom_line() +
      theme_bw() +
      labs(x = "Ano da ação", y = "Valor desembolsado") +
      labs(x = "Ano da ação", y = "Valor desembolsado", color = "Veículo") +
      theme(legend.position = "bottom", legend.direction = "horizontal")
  })
  
  output$vtotal <- renderText({
    req(input$nome_veiculo)
    
    sicom %>%
      filter(nome_do_veiculo %in% !!input$nome_veiculo) %>%
      summarise(total = sum(valor_desembolso_anunciante_r, na.rm = TRUE)) %>%
      pull(total) %>%
      dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
  })
  
  output$nacoes <- renderText({
    sicom %>%
      filter(nome_do_veiculo %in% !!input$nome_veiculo) %>%
      summarise(total = n()) %>%
      pull(total) %>%
      dollar(prefix = "", big.mark = ".", decimal.mark = ",")
  })
}

shinyApp(ui, server)

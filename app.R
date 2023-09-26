# Packages
library(shiny)
library(bslib)
library(bsicons)
library(arrow)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)

# Load data
sicom <- read_parquet(file = "dados_sicom.paquet")


ui <- page_sidebar(
  title = "Painel SICOM",
  sidebar = sidebar(
    
    selectizeInput( 
      inputId = "meio", 
      label = "Meio",
      choices = NULL, 
      multiple = TRUE  
    ),
    
    textInput(
      inputId = "starts_with", 
      label = "Nome começa com"
    ),
    
    selectizeInput(
      inputId = "nome_veiculo", 
      label = "Nome do veículo",
      choices = NULL, 
      multiple = TRUE  
    )
  ),
  layout_column_wrap(
    width = 1/2, height = 100, fill = FALSE,
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
    plotOutput("p_valor_acoes"), height = 300, fill = FALSE
  ),
  card(
    card_header("Quantidade de ações"),
    plotOutput("p_qtd_acoes"), height = 300, fill = FALSE
  ),
  card(
    card_header("Dias de veiculação"),
    plotOutput("p_dveic"), height = 300, fill = FALSE
  )
)

server <- function(input, output, session) {
  # Update choices
  updateSelectizeInput(
    session, "meio", 
    server = TRUE,
    choices = sort(unique(sicom$meio)),
    selected = sort(unique(sicom$meio))
    
  )
  
  nomes <- reactive({
    
    if(input$starts_with == ""){
      sicom %>%
        filter(meio %in% input$meio) %>%
        distinct(nome_do_veiculo) %>%
        arrange(nome_do_veiculo) %>%
        pull(nome_do_veiculo)
    } else {
      sicom %>%
        filter(meio %in% input$meio) %>%
        distinct(nome_do_veiculo) %>%
        filter(str_starts(tolower(nome_do_veiculo), tolower(input$starts_with))) %>%
        arrange(nome_do_veiculo) %>%
        pull(nome_do_veiculo)
    }
    
    
  })
  
  observe({
    updateSelectizeInput(
      session, "nome_veiculo",
      server = TRUE,
      choices = nomes()
    )
  })
  
  
  output$p_valor_acoes <- renderPlot({
    req(input$nome_veiculo)
    
    sicom %>%
      filter(nome_do_veiculo %in% !!input$nome_veiculo) %>%
      group_by(nome_do_veiculo, ano_acao) %>%
      summarise(total = sum(valor_desembolso_anunciante_r, na.rm = TRUE), .groups = "drop_last") %>%
      ungroup() %>%
      ggplot(aes(x = ano_acao, y = total, color = nome_do_veiculo)) +
      geom_line(lwd = .8) +
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
      summarise(total = n(), .groups = "drop_last") %>%
      ungroup() %>%
      ggplot(aes(x = ano_acao, y = total, color = nome_do_veiculo)) +
      geom_line(lwd = .8) +
      theme_bw() +
      labs(x = "Ano da ação", y = "Valor desembolsado", color = "Veículo") +
      theme(legend.position = "bottom", legend.direction = "horizontal")
  })
  
  output$p_dveic <- renderPlot({
    req(input$nome_veiculo)
    
    sicom %>%
      filter(nome_do_veiculo %in% !!input$nome_veiculo) %>%
      mutate(dias = as.numeric(data_termino_da_veiculacao - data_inicio_da_veiculacao)) %>%
      group_by(nome_do_veiculo, ano_acao) %>%
      summarise(total = sum(dias, na.rm = TRUE), .groups = "drop_last") %>%
      ungroup() %>%
      ggplot(aes(x = ano_acao, y = total, color = nome_do_veiculo)) +
      geom_line(lwd = .8) +
      theme_bw() +
      labs(x = "Ano da ação", y = "Dias", color = "Veículo") +
      theme(legend.position = "bottom", legend.direction = "horizontal")
  })
  
  output$vtotal <- renderText({
    req(input$nome_veiculo)
    
    sicom %>%
      filter(nome_do_veiculo %in% !!input$nome_veiculo) %>%
      summarise(total = sum(valor_desembolso_anunciante_r, na.rm = TRUE), .groups = "drop_last") %>%
      pull(total) %>%
      dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
  })
  
  output$nacoes <- renderText({
    sicom %>%
      filter(nome_do_veiculo %in% !!input$nome_veiculo) %>%
      summarise(total = n(), .groups = "drop_last") %>%
      pull(total) %>%
      dollar(prefix = "", big.mark = ".", decimal.mark = ",")
  })
}

shinyApp(ui, server)

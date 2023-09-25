# Packages
library(tidyverse)
library(janitor)
library(arrow)

# Files list
files_list <- list.files(
  path = "downloads/",
  pattern = "*.csv",
  full.names = TRUE
)

# Columns spec
spec_locale <- locale(encoding = "latin1", decimal_mark = ",", grouping_mark = ".")
date_format <- "%d/%m/%Y"
cspec <- cols(
  `IDN da veiculação` = col_double(),
  Meio = col_character(),
  `Nome do veículo` = col_character(),
  `CNPJ do veículo` = col_character(),
  `Cidade sede do veículo` = col_character(),
  `UF sede do veículo` = col_character(),
  `Órgão/Entidade` = col_character(),
  `Agência/Contratada` = col_character(),
  `CNPJ da Agência/Contratada` = col_character(),
  `Número da ação` = col_double(),
  `Nome da ação` = col_character(),
  `Data início da veiculação` = col_date(format = date_format),
  `Data término da veiculação` = col_date(format = date_format),
  `Valor negociado (R$)` = col_character(),
  `Valor desembolso anunciante (R$)` = col_character(),
  Grupo = col_character(),
  Editora = col_character(),
  Programa = col_character(),
  `Faixa horária` = col_character(),
  Rubrica = col_character(),
  `Tipo televisão` = col_character(),
  Formato = col_character(),
  `Tipo de mídia exterior` = col_character(),
  `Tipo de mídia exterior digital` = col_character(),
  `Abrangência da veiculação` = col_character(),
  `Ano ação` = col_double(),
  `Ano autorização` = col_double(),
  `Status da veiculação` = col_character()
)

dados_sicom <- tibble()
for(f in files_list){
  
  # Read file
  tmp <- read_delim(
    file = f, 
    delim = ";",
    locale = spec_locale, 
    col_types = cspec
  ) %>%
    # Standardize names
    clean_names() %>% 
    # Convert currency
    mutate(
      valor_negociado_r = parse_number(valor_negociado_r, locale = spec_locale),
      valor_desembolso_anunciante_r = parse_number(valor_desembolso_anunciante_r, locale = spec_locale)
    )
    
  # Bind rows
  dados_sicom <- bind_rows(dados_sicom, tmp)
}

# Save parquet
write_parquet(x = dados_sicom, sink = "dados_sicom.paquet")

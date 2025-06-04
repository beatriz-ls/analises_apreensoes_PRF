library(data.table)
library(ggplot2)
library(viridis)
library(highcharter)
library(dplyr)

# import data ------------------------------------------------------------------

data <- fread("data/prf_apreensoes.csv")

## Filtragens para as analises apenas do estado presente e 
data <- data[uf == "ES"]

data[, (c("uf", "estado")) := NULL]

municipios_estado_presente <- c("ARACRUZ", "CACHOEIRO DE ITAPEMIRIM", "CARIACICA", "COLATINA",
                     "LINHARES", "SAO MATEUS", "SERRA", "VILA VELHA", "VITORIA")

data_corte <- as.Date("2017-12-28")

data[, programa := ifelse(data_apreensao >= data_corte, 1, 0)]

data[, ano := year(data_apreensao)]

data[, ano_mes := format(data_apreensao, "%Y-%m")]

# Criar variável dummy para o Estado Presente
data[,estado_presente := factor(ifelse(municipio_ocorrencia %in% municipios_estado_presente,
                                       "Estado Presente", "Demais municípios"))]

# Valores acumulados -----------------------------------------------------------

## Gráfico área acumulada estado presente vs ES

# Agregação dos dados por ano e região (Estado Presente vs. Demais municípios)
dados_agregados <- data[, .(total = .N), by = .(ano, estado_presente)]

# Ordenar os dados por ano para garantir a visualização correta
setorder(dados_agregados, ano)

hchart(
  dados_agregados,
  "area",
  hcaes(x = ano, y = total, group = estado_presente),
  stacking = "normal"
) %>%
  hc_title(text = "Evolução Anual das Apreensões no ES") %>%
  hc_colors(viridis::viridis(2, option = "D")) %>%
  hc_xAxis(
    title = list(text = "Ano"),
    plotLines = list(
      list(
        color = "red",
        width = 2,
        value = 2018,
        dashStyle = "Dash",
        zIndex = 5,
        label = list(
          text = "Início do Programa",
          rotation = 90,
          align = "right",
          verticalAlign = "middle",
          style = list(color = "red", fontWeight = "bold")
        )
      )
    )
  ) %>%
  hc_yAxis(title = list(text = "Total de Apreensões"))


## Gráfico de densidade acumulada estado presente vs ES

data[, data_num := as.numeric(as.POSIXct(data_apreensao))]

dens_estado_presente <- data[estado_presente == "Estado Presente", {
  dens <- density(as.numeric(as.POSIXct(data_apreensao)))
  list(
    x = dens$x * 1000,
    y = dens$y
  )
}]

dens_espirito_santo <- data[, {
  dens <- density(as.numeric(as.POSIXct(data_apreensao)))
  list(
    x = dens$x * 1000,
    y = dens$y
  )
}]

### Cores viridis
cores <- viridisLite::viridis(2, option = "D")

### Converter para timestamp em milissegundos
data_corte_ms <- as.numeric(as.POSIXct(data_corte))

highchart() %>%
  hc_chart(type = "areaspline") %>%
  hc_title(text = "Distribuição temporal das apreensões (PRF - ES)") %>%
  hc_xAxis(type = "datetime", title = list(text = "Data da apreensão"),
           plotLines = list(
             list(
               color = "red",     # Cor da linha
               width = 2,         # Espessura
               value = data_corte_ms, # Posição (em timestamp ms)
               dashStyle = "Dash", # Estilo tracejado (opcional)
               zIndex = 5,
               label = list(
                 text = "Início do Programa", 
                 rotation = 90,
                 verticalAlign = "middle",
                 textAlign = "right",
                 style = list(color = "red", fontWeight = "bold")
               )
             )
             )
           )%>%
  hc_yAxis(title = list(text = "Densidade")) %>%
  hc_plotOptions(areaspline = list(fillOpacity = 0.5)) %>%
  hc_add_series(data = list_parse2(dens_data[estado_presente %in%
                                               c("Demais municípios", "Estado Presente"),
                                             .(x, y)]),
                name = "Espirito Santo",
                color = cores[2],
                type = "areaspline") %>%
  hc_add_series(data = list_parse2(dens_data[estado_presente == "Estado Presente", .(x, y)]),
                name = "Estado Presente",
                color = cores[1],
                type = "areaspline") %>%
  hc_tooltip(pointFormat = "<b>Densidade:</b> {point.y:.4f}") %>%
  hc_legend(title = list(text = "Município")) %>%
  hc_add_theme(hc_theme_flat())





## Gráficos de barras empilhada de número de apreensões

dados_agregados <- data[, .(total = .N), by = .(ano, estado_presente)]

hchart(
  dados_agregados, 
  "column", 
  hcaes(x = ano, y = total, group = estado_presente),
  stacking = "normal"
) %>%
  hc_title(text = "Apreensões por Ano no ES") %>%
  hc_colors(viridis(2, option = "D")) %>%
  hc_xAxis(title = list(text = "Ano")) %>%
  hc_yAxis(title = list(text = "Total de Apreensões"))

# Séries temporais -------------------------------------------------------------

## serie do total de todas apreensões

### Extrair ano e mês para agregação
data[, ano_mes := format(data_apreensao, "%Y-%m")]

### Calcular o total de apreensões por mês
dados_mensais <- data[, .(total_apreensoes = .N), by = .(ano_mes)]

### Ordenar por data
setorder(dados_mensais, ano_mes)

### Criar coluna de data completa (primeiro dia do mês) para o gráfico
dados_mensais[, data := as.Date(paste0(ano_mes, "-01"))]


hchart(dados_mensais, "line", hcaes(x = data, y = total_apreensoes)) %>%
  hc_title(text = "Total de Apreensões no ES por Mês") %>%
  hc_xAxis(
    title = list(text = "Mês/Ano"),
    type = "datetime",
    labels = list(format = "{value:%b/%Y}"),
    plotLines = list(
      list(
        color = "red",
        width = 2,
        value = datetime_to_timestamp(as.Date("2017-12-28")),
        dashStyle = "Dash",
        zIndex = 5,
        label = list(
          text = "Início do Programa", 
          rotation = 90,
          verticalAlign = "middle",
          textAlign = "right",
          style = list(color = "red", fontWeight = "bold")
        )
      )
    )
    
  ) %>%
  hc_yAxis(title = list(text = "Número de Apreensões")) %>%
  hc_tooltip(
    pointFormat = "<b>{point.total_apreensoes}</b> apreensões em <b>{point.x:%b/%Y}</b>"
  ) %>%
  hc_colors("#1f77b4")

## Serie de cada categoria de droga

data[, grupo_droga := fcase(
  tipo_apreensao %in% c("Cocaína/Cloridrato de Cocaína", "Crack"), "Cocaína e Derivados",
  tipo_apreensao %in% c("Maconha", "Pés de Maconha", "Haxixe"), "Cannabis e Derivados",
  tipo_apreensao %in% c("Anfetaminas", "Ecstasy", "Lança-perfume"), "Sintéticas/Estimulantes",
  tipo_apreensao %in% c("Barbitúricos", "Medicamentos"), "Farmacêuticos/Controlados",
  default = "Outros"  # Inclui LSD e demais (baixa frequência)
)]

### Calcular total de apreensões por mês e categoria
dados_mensais <- data[, .(total = .N), by = .(ano_mes, grupo_droga)]

# Criar coluna de data no primeiro dia do mês (para o eixo X)
dados_mensais[, data := as.Date(paste0(ano_mes, "-01"))]

setorder(dados_mensais, data)

# 2. Verificar a ordem (opcional)
head(dados_mensais[, .(data, ano_mes, grupo_droga, total)])

highchart() %>%
  hc_add_series(
    data = dados_mensais,
    type = "line",
    hcaes(x = data, y = total, group = grupo_droga),
    showInLegend = TRUE
  ) %>%
  hc_title(text = "Apreensões Mensais por Tipo de Droga (PRF-ES)") %>%
  hc_xAxis(
    type = "datetime",
    title = list(text = "Mês/Ano"),
    labels = list(format = "{value:%b/%Y}")  # Formato "Jan/2020"
  ) %>%
  hc_yAxis(title = list(text = "Total de Apreensões")) %>%
  hc_tooltip(
    pointFormat = "<b>{point.grupo_droga}:</b> {point.y} apreensões<br>Data: <b>{point.x:%b/%Y}</b>"
  ) %>%
  hc_colors(viridis::viridis(dados_mensais[, uniqueN(grupo_droga)]))


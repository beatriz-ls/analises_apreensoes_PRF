library(data.table)
library(trend)
library(dplyr)
library(sandwich)
library(lmtest)

# import data ------------------------------------------------------------------

data <- fread("data/prf_apreensoes.csv")

## Filtragens para as analises apenas do estado presente e 
data <- data[uf == "ES"]

data[, (c("uf", "estado")) := NULL]

municipios_estado_presente <- c("ARACRUZ", "CACHOEIRO DE ITAPEMIRIM", "CARIACICA",
                                "COLATINA", "LINHARES", "SAO MATEUS", "SERRA",
                                "VILA VELHA", "VITORIA")

data_corte <- as.Date("2017-12-28")

data[, programa := ifelse(data_apreensao >= data_corte, 1, 0)]

data[, ano := year(data_apreensao)]

data[, ano_mes := format(data_apreensao, "%Y-%m")]

data[, tempo := as.numeric(factor(ano_mes))]  # Converte ano_mes em 1, 2, 3...

data[, pos_intervencao := fifelse(
  programa == 1, 
  tempo - min(tempo[programa == 1]),
  0
)]

## Criar variável dummy para o Estado Presente
data[,estado_presente := factor(ifelse(municipio_ocorrencia %in% municipios_estado_presente,
                                       "Estado Presente", "Demais municípios"))]

# Teste de tendência pré-Programa (apenas cidades do estado presente) ----------

dados_agg <- data[, .(total_registros = .N), by = .(ano_mes, programa)]

mk_test <- dados_agg[, total_registros]

# Período ANTES do programa
if (dados_agg[programa == 0, .N] > 0) {
  mk_antes <- mk.test(dados_agg[programa == 0, total_registros])
  cat("\n--- Teste de Mann-Kendall (ANTES do programa) ---\n")
  print(mk_antes)
} else {
  cat("\nNenhum dado disponível no período 'Antes'.\n")
}

# Período DEPOIS do programa
if (dados_agg[programa == 1, .N] > 0) {
  mk_depois <- mk.test(dados_agg[programa == 1, total_registros])
  cat("\n--- Teste de Mann-Kendall (DEPOIS do programa) ---\n")
  print(mk_depois)
} else {
  cat("\nNenhum dado disponível no período 'Depois'.\n")
}

# Modelo Interruptes Time Serie ------------------------------------------------

## Pré processamneto para essa etapa
dados_agregados <- data[, .(
  total_apreensoes = .N,
  programa = first(programa),
  tempo = first(tempo),
  pos_intervencao = first(pos_intervencao)
), by = .(ano_mes, estado_presente)]

## Modelo
modelo_its <- lm(
  total_apreensoes ~ tempo + estado_presente + programa + 
    tempo:estado_presente + programa:estado_presente + 
    pos_intervencao:estado_presente,
  data = dados_agregados
)

## Verificando pressupostos

### Tendencia parelelas

dados_pre <- dados_agregados[programa == 0]
modelo_pre <- lm(total_apreensoes ~ tempo * estado_presente, data = dados_pre)
summary(modelo_pre)

### Autocorrelação

dwtest(modelo_its)

# Analise DiD ------------------------------------------------------------------

modelo_did <- lm(total_apreensoes ~ estado_presente + programa +
                   estado_presente:programa,
                 data = dados_agregados)


coeftest(modelo_did, vcov = vcovHC(modelo_did, type = "HC1"))  # Erros robustos

# Analise de sazonalidade ------------------------------------------------------


## serie do total de todas apreensões

### Extrair ano e mês para agregação
data[, ano_mes := format(data_apreensao, "%Y-%m")]

### Calcular o total de apreensões por mês
dados_mensais <- data[, .(total_apreensoes = .N), by = .(ano_mes)]

### Ordenar por data
setorder(dados_mensais, ano_mes)

### Criar coluna de data completa (primeiro dia do mês) para o gráfico
dados_mensais[, data := as.Date(paste0(ano_mes, "-01"))]


# Converter para série temporal mensal
serie_ts <- ts(dados_mensais$total_apreensoes, 
               start = c(as.integer(substr(dados_mensais$ano_mes[1], 1, 4)), 
                         as.integer(substr(dados_mensais$ano_mes[1], 6, 7))),
               frequency = 12)

# Plotar a série temporal
autoplot(serie_ts) + 
  ggtitle("Total de Apreensões Mensais") +
  xlab("Ano") + ylab("Quantidade") +
  theme_minimal()

# Teste de autocorrelação para verificar padrão sazonal
acf(serie_ts, main = "Função de Autocorrelação (ACF)")

# Teste de decomposição sazonal (opcional para visualização)
decomposicao <- stl(serie_ts, s.window = "periodic")
autoplot(decomposicao)


kruskal.test(total_apreensoes ~ ano_mes, data = dados_mensais)

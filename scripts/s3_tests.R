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

dados_agg <- data[, .(total_registros = .N, programa), by = ano_mes]

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
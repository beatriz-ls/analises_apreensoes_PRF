library(data.table)
library(trend)
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

# 
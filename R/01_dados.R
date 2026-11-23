## 01_dados.R
## Monta o painel mensal para refazer a parte de volumes do estudo do BC (2026), "Transmissão da
## Política Monetária por meio do Crédito", seção 4.
##
## Fontes (todas públicas):
##   - SGS/BCB: concessões dessazonalizadas pelo próprio BC (24439-24447), concessões brutas por
##     modalidade, taxas médias, prazos médios, Selic, IPCA, IBC-Br, desocupação (PNAD Contínua).
##   - Focus por reunião do Copom (Olinda, ExpectativasMercadoSelic) e histórico do Copom (API do
##     site do BCB).
##
## Os brutos ficam em data/ (fora do git). O script só baixa o que ainda não está lá.
##
## Saídas: output/painel_mensal.csv, output/surpresa_copom_reuniao.csv
## Rodar da pasta do estudo: Rscript R/01_dados.R

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(dplyr); library(readr); library(tidyr)
})

## ---- séries --------------------------------------------------------------------------------
series <- tribble(
  ~codigo, ~nome,
  # concessões dessazonalizadas pelo BC (R$ milhões, nominais)
  24439, "conc_total",   24440, "conc_pj",      24441, "conc_pf",
  24442, "conc_liv",     24443, "conc_liv_pj",  24444, "conc_liv_pf",
  24445, "conc_dir",     24446, "conc_dir_pj",  24447, "conc_dir_pf",
  # concessões brutas, para pesos e conferência
  20631, "concb_total",  20634, "concb_liv",    20685, "concb_dir",
  20635, "concb_liv_pj", 20662, "concb_liv_pf", 20686, "concb_dir_pj", 20698, "concb_dir_pf",
  # taxas médias (% a.a.)
  20714, "tx_total", 20717, "tx_liv", 20756, "tx_dir", 20715, "tx_pj", 20716, "tx_pf",
  20718, "tx_liv_pj", 20740, "tx_liv_pf", 20757, "tx_dir_pj", 20768, "tx_dir_pf",
  # prazos médios das concessões (meses)
  20855, "prazo_liv", 20895, "prazo_dir",
  # macro
  4189,  "selic",      # Selic acumulada no mês anualizada (% a.a.)
  433,   "ipca_var",   # IPCA, variação mensal (%)
  24364, "ibcbr",      # IBC-Br dessazonalizado
  24369, "desocup",    # taxa de desocupação, PNAD Contínua (%), desde mar/2012
  10777, "desocup_pme" # taxa de desocupação, PME (%), 2002 a fev/2016: cobre 2011-12 na réplica do BC
)

baixar_sgs <- function(codigo, tentativas = 4) {
  url <- sprintf("https://api.bcb.gov.br/dados/serie/bcdata.sgs.%d/dados?formato=json&dataInicial=01/01/2010&dataFinal=31/12/2026", codigo)
  for (i in seq_len(tentativas)) {
    r <- tryCatch(GET(url, timeout(60)), error = function(e) e)
    if (!inherits(r, "error") && status_code(r) == 200) {
      df <- tryCatch(fromJSON(content(r, as = "text", encoding = "UTF-8")), error = function(e) NULL)
      if (is.data.frame(df) && nrow(df) > 0)
        return(tibble(data = as.Date(df$data, "%d/%m/%Y"), valor = as.numeric(df$valor)))
    }
    message(sprintf("  SGS %d: tentativa %d falhou", codigo, i)); Sys.sleep(3)
  }
  stop(sprintf("SGS %d: falhou %d vezes", codigo, tentativas))
}

dir.create("data", showWarnings = FALSE); dir.create("output", showWarnings = FALSE)

baixar_json <- function(url, tentativas = 4) {
  for (i in seq_len(tentativas)) {
    r <- tryCatch(GET(url, timeout(180)), error = function(e) e)
    if (!inherits(r, "error") && status_code(r) == 200) {
      out <- tryCatch(fromJSON(content(r, as = "text", encoding = "UTF-8")), error = function(e) NULL)
      if (!is.null(out)) return(out)
    }
    message(sprintf("  tentativa %d falhou: %s", i, url)); Sys.sleep(3)
  }
  stop("falhou: ", url)
}

if (!file.exists("data/copom_historico.csv")) {
  message("baixando histórico do Copom")
  baixar_json("https://www.bcb.gov.br/api/servico/sitebcb/historicotaxasjuros")$conteudo %>%
    transmute(reuniao_n = as.integer(NumeroReuniaoCopom), extraordinaria = ReuniaoExtraordinaria,
              data_reuniao = as.Date(substr(DataReuniaoCopom, 1, 10)),
              inicio_vigencia = as.Date(substr(DataInicioVigencia, 1, 10)), meta_selic = MetaSelic) %>%
    write_csv("data/copom_historico.csv")
}
if (!file.exists("data/focus_selic_reuniao.csv")) {
  message("baixando Focus por reunião (pode demorar)")
  baixar_json(paste0("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/",
                     "ExpectativasMercadoSelic?$filter=baseCalculo%20eq%200",
                     "&$select=Data,Reuniao,Media,Mediana,DesvioPadrao,numeroRespondentes",
                     "&$top=1000000&$format=json"))$value %>%
    mutate(Data = as.Date(Data)) %>% write_csv("data/focus_selic_reuniao.csv")
}

ler_ou_baixar <- function(codigo) {
  arq <- file.path("data", sprintf("bcb_%d.csv", codigo))
  if (!file.exists(arq)) {
    message("baixando SGS ", codigo)
    write_csv(baixar_sgs(codigo), arq)
  }
  read_csv(arq, show_col_types = FALSE, col_types = cols(data = "D", valor = "d"))
}

painel <- series %>%
  mutate(dados = lapply(codigo, ler_ou_baixar)) %>%
  select(nome, dados) %>% unnest(dados) %>%
  mutate(data = as.Date(format(data, "%Y-%m-01"))) %>%
  pivot_wider(names_from = nome, values_from = valor) %>%
  arrange(data)

## ---- surpresa do Copom contra o Focus ------------------------------------------------------------
## Selic decidida menos a mediana do Focus para aquela reunião na última pesquisa antes da decisão
## (descartada se a pesquisa tem mais de 10 dias). Na linha de Leigh e Xu (2025), citados pelo BC.
copom <- read_csv("data/copom_historico.csv", show_col_types = FALSE) %>%
  mutate(data_reuniao = as.Date(data_reuniao), inicio_vigencia = as.Date(inicio_vigencia)) %>%
  filter(!is.na(meta_selic)) %>% arrange(data_reuniao, inicio_vigencia) %>%
  group_by(reuniao_n) %>% slice_tail(n = 1) %>% ungroup() %>%   # uma decisão por reunião
  arrange(data_reuniao) %>% mutate(meta_anterior = lag(meta_selic))
ordinarias <- copom %>% filter(!extraordinaria) %>%
  mutate(ano = as.integer(format(data_reuniao, "%Y"))) %>%
  group_by(ano) %>% mutate(k = row_number()) %>% ungroup() %>%
  mutate(rotulo = sprintf("R%d/%d", k, ano))
focus <- read_csv("data/focus_selic_reuniao.csv", show_col_types = FALSE) %>% mutate(Data = as.Date(Data))

surp <- ordinarias %>% rowwise() %>%
  mutate(focus_data = { d <- focus$Data[focus$Reuniao == rotulo & focus$Data < data_reuniao]
                        if (length(d)) max(d) else as.Date(NA) },
         focus_mediana = if (is.na(focus_data)) NA_real_ else
           focus$Mediana[focus$Reuniao == rotulo & focus$Data == focus_data][1]) %>%
  ungroup() %>%
  mutate(dias_antes = as.integer(data_reuniao - focus_data),
         surpresa = ifelse(dias_antes > 10, NA, meta_selic - focus_mediana))
write_csv(surp %>% select(reuniao_n, rotulo, data_reuniao, meta_anterior, meta_selic, focus_data,
                          dias_antes, focus_mediana, surpresa),
          "output/surpresa_copom_reuniao.csv")

surp_m <- surp %>% filter(!is.na(surpresa)) %>%
  mutate(data = as.Date(format(data_reuniao, "%Y-%m-01"))) %>%
  group_by(data) %>% summarise(surpresa = sum(surpresa), .groups = "drop")

painel <- painel %>% left_join(surp_m, by = "data") %>%
  mutate(surpresa = coalesce(surpresa, 0)) %>%
  filter(data >= as.Date("2011-03-01"))

## ---- transformações ------------------------------------------------------------------------------
painel <- painel %>%
  mutate(ipca_idx = 100 * cumprod(1 + ipca_var / 100),
         ipca_idx = ipca_idx / ipca_idx[data == as.Date("2025-06-01")],
         across(starts_with("conc_"), ~ 100 * log(.x / ipca_idx), .names = "l{.col}"),
         lipca = 100 * log(ipca_idx), libcbr = 100 * log(ibcbr),
         covid = as.integer(data >= as.Date("2020-03-01") & data <= as.Date("2022-12-01")),
         mes = factor(format(data, "%m")))

write_csv(painel, "output/painel_mensal.csv")

cat(sprintf("Painel: %s a %s, %d meses\n", min(painel$data), max(painel$data), nrow(painel)))
cat("Última observação não ausente por série:\n")
print(painel %>% summarise(across(c(conc_total, conc_dir_pj, tx_liv, desocup, selic, ibcbr),
                                  ~ max(data[!is.na(.x)]))) %>% t())
amostra_bc <- painel %>% filter(data <= as.Date("2025-06-01"))
cat(sprintf("\nMédia mensal das concessões brutas, mar/2011-jun/2025 (R$ bi): livre %.1f, direcionado %.1f, total %.1f\n",
            mean(amostra_bc$concb_liv) / 1e3, mean(amostra_bc$concb_dir) / 1e3, mean(amostra_bc$concb_total) / 1e3))
cat("BC, Tabela 4.1: livre 312,1; direcionado 41,0; total 353,2\n")
cat(sprintf("Meses com surpresa não nula (mar/2011-jun/2025): %d de %d\n",
            sum(amostra_bc$surpresa != 0), nrow(amostra_bc)))

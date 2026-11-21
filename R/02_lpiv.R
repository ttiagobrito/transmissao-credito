## 02_lpiv.R
## Multiplicador cumulativo por projeções locais com instrumento (Jordà e Taylor 2025), como na
## seção 4.2.2 do BC (2026):
##   mean(y_{t..t+h}) - y_{t-1} = m_h * [mean(i_{t..t+h}) - i_{t-1}] + controles + e
##   i = Selic; instrumento = surpresa do Copom contra o Focus (robustez declarada pelo BC, nota 41).
## Controles: p defasagens de Δy e Δi; nas concessões também Δdesocupação, ΔIPCA (log), ΔIBC-Br (log)
## e dummy de pandemia (mar/2020-dez/2022). Sem o IIE-Br (o BC diz que o resultado não muda sem ele).
## Erros Newey-West com h+1 defasagens.
##
## Além do que o BC reporta: F efetivo do primeiro estágio, intervalo de Anderson-Rubin (válido com
## instrumento fraco), MQO sem instrumento, teste da diferença direcionado − livre e soma em reais.
##
## Saídas: output/resultados_h18.csv, output/diferencas_h18.csv, output/soma_reais_h18.csv,
##         output/trajetoria.csv
## Rodar da pasta do estudo: Rscript R/02_lpiv.R

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(AER); library(sandwich); library(lmtest)
})

painel <- read_csv("output/painel_mensal.csv", show_col_types = FALSE) %>% arrange(data)
FIM_BC <- as.Date("2025-06-01")

source("R/funcoes.R")

## ---- 1. Réplica no horizonte de 18 meses ----------------------------------------------------
volumes <- c("lconc_total", "lconc_liv", "lconc_dir", "lconc_liv_pj", "lconc_liv_pf",
             "lconc_dir_pj", "lconc_dir_pf", "lconc_pj", "lconc_pf")
taxas   <- c("tx_total", "tx_liv", "tx_dir", "tx_liv_pj", "tx_liv_pf", "tx_dir_pj", "tx_dir_pf")

res <- bind_rows(
  lapply(volumes, function(v) estima_fim(painel, v, h = 18, macro = TRUE)),
  lapply(taxas,   function(v) estima_fim(painel, v, h = 18, macro = FALSE, grade = seq(-10, 10, by = 0.01))),
  # robustez: 4 defasagens; amostra até o último dado disponível
  lapply(c("lconc_total", "lconc_dir_pj", "lconc_liv_pj"), function(v) estima_fim(painel, v, h = 18, p = 4)),
  lapply(c("lconc_total", "lconc_dir_pj", "lconc_liv_pj"), function(v)
    estima_fim(painel, v, h = 18, fim = max(painel$data[!is.na(painel$conc_total)]))),
  # sem desocupação (amostra desde 2011, como o BC; desocupação PNADC só existe desde 2012)
  lapply(c("lconc_total", "lconc_dir_pj", "lconc_liv_pj"), function(v) {
    d <- painel; d$desocup <- 0; estima_fim(d, v, h = 18) %>% mutate(macro = NA)
  })
)
write_csv(res, "output/resultados_h18.csv")

## ---- 2. Diferença direcionado − livre na mesma regressão ------------------------------------------
painel <- painel %>% mutate(dif_pj = lconc_dir_pj - lconc_liv_pj, dif_pf = lconc_dir_pf - lconc_liv_pf,
                            dif_tot = lconc_dir - lconc_liv)
difs <- bind_rows(
  estima_fim(painel, "dif_pj", h = 18, y2 = "lconc_liv_pj"),
  estima_fim(painel, "dif_pf", h = 18, y2 = "lconc_liv_pf"),
  estima_fim(painel, "dif_tot", h = 18, y2 = "lconc_liv")
)
write_csv(difs, "output/diferencas_h18.csv")

## ---- 3. Em reais: soma dos quatro segmentos contra o total ------------------------------------------
media_bruta <- painel %>% filter(data <= FIM_BC) %>%
  summarise(lconc_total = mean(concb_total), lconc_liv_pj = mean(concb_liv_pj), lconc_liv_pf = mean(concb_liv_pf),
            lconc_dir_pj = mean(concb_dir_pj), lconc_dir_pf = mean(concb_dir_pf)) %>%
  pivot_longer(everything(), names_to = "serie", values_to = "media_rs_mi")
soma <- res %>% filter(p == 2, fim == FIM_BC, macro %in% TRUE, serie %in% media_bruta$serie) %>%
  select(serie, m_iv, ic_lo, ic_hi) %>% left_join(media_bruta, by = "serie") %>%
  mutate(participacao = media_rs_mi / media_rs_mi[serie == "lconc_total"],
         resposta_rs_bi = m_iv / 100 * media_rs_mi / 1e3,
         lo_rs_bi = ic_lo / 100 * media_rs_mi / 1e3, hi_rs_bi = ic_hi / 100 * media_rs_mi / 1e3)
write_csv(soma, "output/soma_reais_h18.csv")

## ---- 4. Trajetória h = 0..24 para a figura ---------------------------------------------------------
traj <- bind_rows(lapply(c("lconc_total", "lconc_liv_pj", "lconc_dir_pj", "lconc_liv_pf", "lconc_dir_pf"),
  function(v) bind_rows(lapply(0:24, function(h) estima_fim(painel, v, h = h, ar = FALSE)))))
write_csv(traj, "output/trajetoria.csv")

## ---- resumo no console -----------------------------------------------------------------------------
fmt <- function(df) df %>% transmute(serie, p, fim, n, inicio, m_iv = round(m_iv, 2),
  ic = sprintf("[%.2f, %.2f]", ic_lo, ic_hi), F_ef = round(F_efetivo, 1),
  AR = ifelse(ar_ilimitado, sprintf("[%.2f, %.2f]*", ar_lo, ar_hi), sprintf("[%.2f, %.2f]", ar_lo, ar_hi)),
  m_ols = round(m_ols, 2))
cat("\n== h = 18, amostra do BC (até jun/2025) ==\n"); print(fmt(res), n = 50, width = 200)
cat("\n== Diferença direcionado - livre (p.p. de variação %) ==\n"); print(fmt(difs), width = 200)
cat("\n== Em reais (R$ bi por mês, média de 18 meses, por +1 p.p. de Selic) ==\n")
print(soma %>% mutate(across(where(is.numeric), ~ round(.x, 3))), width = 200)
cat(sprintf("\nSoma dos 4 segmentos: %.2f bi; total estimado: %.2f bi\n",
            sum(soma$resposta_rs_bi[soma$serie != "lconc_total"]), soma$resposta_rs_bi[soma$serie == "lconc_total"]))
cat("* = conjunto de Anderson-Rubin encosta no limite da grade (provavelmente ilimitado)\n")

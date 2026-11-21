## 03_placebo.R
## Placebo de antecedência: a surpresa do Copom no mês t não pode "explicar" o que as concessões ou a
## Selic fizeram nos 12 meses anteriores. Forma reduzida: y_{t-1} - y_{t-13} contra z_t, com dummy de
## pandemia; Newey-West com 13 defasagens. Coeficiente grande e significante indicaria que a surpresa
## responde ao estado do crédito (o instrumento não seria exógeno).
## Saída: output/placebo.csv. Rodar da pasta do estudo.

suppressPackageStartupMessages({ library(dplyr); library(readr); library(sandwich) })
painel <- read_csv("output/painel_mensal.csv", show_col_types = FALSE) %>%
  arrange(data) %>% filter(data <= as.Date("2025-06-01"))

placebo <- function(v) {
  y <- painel[[v]]
  d <- tibble(data = painel$data, z = painel$surpresa, covid = painel$covid,
              passado = dplyr::lag(y, 1) - dplyr::lag(y, 13)) %>% na.omit()
  m <- lm(passado ~ z + covid, data = d)
  se <- sqrt(NeweyWest(m, lag = 13, prewhite = FALSE)["z", "z"])
  tibble(serie = v, n = nrow(d), coef_z = coef(m)["z"], se = se, t = coef(m)["z"] / se)
}
res <- bind_rows(lapply(c("lconc_total", "lconc_liv_pj", "lconc_liv_pf", "lconc_dir_pj", "lconc_dir_pf",
                          "tx_liv", "tx_dir", "selic"), placebo))
write_csv(res, "output/placebo.csv")
print(res %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

## Robustez: a mesma projeção local de 02_lpiv.R controlando também pelo crescimento dos 12 meses
## anteriores (y_{t-1} - y_{t-13}), que o placebo mostrou correlacionado com a surpresa.
source("R/funcoes.R")
painel_todo <- read_csv("output/painel_mensal.csv", show_col_types = FALSE) %>% arrange(data)
rob <- bind_rows(lapply(c("lconc_total", "lconc_liv_pj", "lconc_liv_pf", "lconc_dir_pj", "lconc_dir_pf"),
  function(v) bind_rows(estima_fim(painel_todo, v, h = 18), estima_fim(painel_todo, v, h = 18, c12 = TRUE))))
write_csv(rob, "output/robustez_c12.csv")
print(rob %>% transmute(serie, c12, n, m_iv = round(m_iv, 2), ic = sprintf("[%.2f, %.2f]", ic_lo, ic_hi),
                        F_ef = round(F_efetivo, 1)))

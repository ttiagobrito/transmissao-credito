## 07_replica_bc.R
## Réplica da seção 4.2.2 de BC (2026) na especificação que o estudo descreve (p. 45-46):
##   sum_{j=0}^{h} (w_{t+j} - w_{t-1}) = a + m_h sum_{j=0}^{h} (Selic_{t+j} - Selic_{t-1}) + X_t' G + e
##   MQ2E com a Selic acumulada instrumentada pela surpresa sup_t.
## Controles (p. 46): defasagens da variável dependente e da variação da Selic; nas concessões também
## defasagens da 1a diferença da desocupação, do log do IPCA, do log do IBC-Br e do log do IIE-Br, e
## dummy de pandemia (mar/2020-dez/2022). Número de defasagens pelo BIC. Newey-West.
##
## O que o estudo não especifica e foi escolhido aqui (rodar alternativas em robustez):
##   - BIC por horizonte, p de 1 a 12, calculado no MQO da própria equação com amostra fixa em 12 defasagens;
##   - defasagens da variável dependente em nível (alternativa: em diferença);
##   - desocupação: diferença da PNAD Contínua, e da PME antes de abr/2012;
##   - Selic: SGS 4189 (acumulada no mês, anualizada); Newey-West com h+1 defasagens;
##   - surpresa mensal = soma das surpresas das reuniões do mês.
## Instrumentos: (A) surpresa contra o Focus, 2011-2025 (robustez declarada pelo BC, nota 41);
##               (B) surpresa no DI futuro, como a principal do BC, só 2018-2025 (arquivos da B3);
##               (C) Focus na mesma amostra de B, para separar o efeito do instrumento do da amostra.
## Saídas: output/replica_bc_h18.csv, output/comparacao_surpresas.csv
## Rodar da pasta do estudo, depois de 01 e de py/06_surpresa_di.py.

source("R/funcoes.R")

painel <- read_csv("output/painel_mensal.csv", show_col_types = FALSE) %>% arrange(data)
# IIE-Br (FGV): baixar no portal FGV Dados (série 1428452, cadastro gratuito) e salvar em
# data_fgv/fgv_series_largo.csv com as colunas data (AAAA-MM-01) e iie_br. Sem o arquivo, a réplica
# roda sem esse controle, a variante que o BC declara não mudar o resultado (nota 41, p. 46).
ARQ_IIE <- "data_fgv/fgv_series_largo.csv"
TEM_IIE <- file.exists(ARQ_IIE)
fgv <- if (TEM_IIE) read_csv(ARQ_IIE, show_col_types = FALSE) %>% select(data, iie_br) else
  { warning("IIE-Br ausente: estimando sem o controle de incerteza"); tibble(data = as.Date(character()), iie_br = numeric()) }
di <- read_csv("output/surpresa_di_reuniao.csv", show_col_types = FALSE) %>%
  filter(!is.na(surpresa)) %>%
  mutate(data = as.Date(format(as.Date(data_reuniao), "%Y-%m-01"))) %>%
  group_by(data) %>% summarise(sup_di = sum(surpresa), .groups = "drop")
INI_DI <- as.Date("2018-02-01")

painel <- painel %>% left_join(fgv, by = "data") %>% left_join(di, by = "data") %>%
  mutate(sup_focus = surpresa,
         sup_di = ifelse(data >= INI_DI & data <= FIM_BC, coalesce(sup_di, 0), NA_real_),
         d_u = coalesce(desocup - lag(desocup), desocup_pme - lag(desocup_pme)),
         d_p = lipca - lag(lipca), d_a = libcbr - lag(libcbr),
         d_inc = 100 * (log(iie_br) - lag(log(iie_br))),
         d_selic = selic - lag(selic))

soma_frente <- function(x, h) {
  n <- length(x); sapply(seq_len(n), function(t) if (t > 1 && t + h <= n) sum(x[t:(t + h)] - x[t - 1]) else NA)
}

monta_bc <- function(df, y, h, p, macro, z, ylag = "nivel") {
  yy <- df[[y]]
  d <- tibble(data = df$data, lhs = soma_frente(yy, h), x = soma_frente(df$selic, h), z = df[[z]])
  base_y <- if (ylag == "nivel") yy else yy - dplyr::lag(yy)
  ctrl <- list(y = base_y, ds = df$d_selic)
  if (macro) ctrl <- c(ctrl, list(du = df$d_u, dp = df$d_p, da = df$d_a))
  if (macro && TEM_IIE) ctrl$dinc <- df$d_inc
  for (nm in names(ctrl)) for (l in seq_len(p)) d[[sprintf("%s_l%d", nm, l)]] <- dplyr::lag(ctrl[[nm]], l)
  if (macro) d$covid <- df$covid
  d
}

# pmax = 6: o BC não informa o máximo. Com 12, na amostra curta do DI (cerca de 60 meses) o BIC
# escolhe 10-12 e a regressão de concessões fica quase saturada. Na amostra longa o BIC escolhe 1.
escolhe_p <- function(df, y, h, macro, z, ylag, pmax = 6) {
  # BIC no MQO da equação, mesma amostra para todo p (a que sobra com pmax defasagens)
  amostra <- monta_bc(df, y, h, pmax, macro, z, ylag) %>% na.omit() %>% pull(data)
  bics <- sapply(seq_len(pmax), function(p) {
    d <- monta_bc(df, y, h, p, macro, z, ylag) %>% filter(data %in% amostra)
    fc <- paste(setdiff(names(d), c("data", "lhs", "z")), collapse = " + ")
    BIC(lm(as.formula(paste("lhs ~", fc)), data = d))
  })
  which.min(bics)
}

estima_bc <- function(df, y, h, macro, z, ylag = "nivel", ini = NULL, rodada = "") {
  dfx <- df %>% filter(data <= FIM_BC, if (is.null(ini)) TRUE else data >= ini)
  p <- escolhe_p(dfx, y, h, macro, z, ylag)
  d <- monta_bc(dfx, y, h, p, macro, z, ylag) %>% na.omit()
  ctrls <- setdiff(names(d), c("data", "lhs", "x", "z")); fc <- paste(ctrls, collapse = " + ")
  iv <- ivreg(as.formula(sprintf("lhs ~ x + %s | z + %s", fc, fc)), data = d)
  V <- NeweyWest(iv, lag = h + 1, prewhite = FALSE)
  fs <- lm(as.formula(sprintf("x ~ z + %s", fc)), data = d)
  Vf <- NeweyWest(fs, lag = h + 1, prewhite = FALSE)
  W <- model.matrix(as.formula(paste("~", fc)), data = d)
  rw <- function(v) as.numeric(lm.fit(W, v)$residuals)
  grade <- if (grepl("^tx_", y)) seq(-12, 12, by = 0.01) else seq(-60, 60, by = 0.05)
  ar_ok <- ar_conjunto(rw(d$lhs), rw(d$x), rw(d$z), L = h + 1, grade = grade)
  ar_ok[is.na(ar_ok)] <- FALSE
  b <- coef(iv)["x"]; se <- sqrt(V["x", "x"])
  tibble(rodada = rodada, serie = y, h = h, p_bic = p, ylag = ylag, n = nrow(d), k = ncol(W) + 1,
         inicio = min(d$data), fim_t = max(d$data),
         surpresas_nao_nulas = sum(d$z != 0), m = b, se = se, ic_lo = b - 1.96 * se, ic_hi = b + 1.96 * se,
         F_efetivo = (coef(fs)["z"] / sqrt(Vf["z", "z"]))^2,
         ar_lo = if (any(ar_ok)) min(grade[ar_ok]) else NA, ar_hi = if (any(ar_ok)) max(grade[ar_ok]) else NA,
         ar_ilimitado = any(ar_ok) && (ar_ok[1] || ar_ok[length(ar_ok)]))
}

taxas <- c("tx_total", "tx_dir", "tx_liv", "tx_dir_pj", "tx_pj", "tx_liv_pj", "tx_dir_pf", "tx_pf", "tx_liv_pf")  # Gráfico 4.1
volumes <- c("lconc_dir_pj", "lconc_dir_pf", "lconc_liv_pj", "lconc_liv_pf",                                # Gráfico 4.4
             "lconc_total", "lconc_liv", "lconc_dir")                                                       # não mostrados
bc_rep <- c(tx_liv = 1.05, tx_dir = 0.28, lconc_dir_pj = -5.3, lconc_dir_pf = -2.8, lconc_liv_pj = -0.3, lconc_liv_pf = -0.9)

roda <- function(rodada, z, ini = NULL, ylag = "nivel")
  bind_rows(lapply(taxas, function(v) estima_bc(painel, v, 18, FALSE, z, ylag, ini, rodada)),
            lapply(volumes, function(v) estima_bc(painel, v, 18, TRUE, z, ylag, ini, rodada)))

res <- bind_rows(
  roda("A_focus_2011", "sup_focus"),
  roda("B_di_2018", "sup_di", ini = INI_DI),
  roda("C_focus_2018", "sup_focus", ini = INI_DI),
  roda("A2_focus_2011_lag_dif", "sup_focus", ylag = "dif")
) %>% mutate(bc_reportado = unname(bc_rep[serie]))
write_csv(res, "output/replica_bc_h18.csv")

## comparação das duas surpresas, reunião a reunião, onde as duas existem
foc <- read_csv("output/surpresa_copom_reuniao.csv", show_col_types = FALSE) %>% select(data_reuniao, sup_focus = surpresa)
cmp <- read_csv("output/surpresa_di_reuniao.csv", show_col_types = FALSE) %>%
  select(data_reuniao, contrato, du, sup_di = surpresa, sup_di_bruta = surpresa_bruta) %>%
  left_join(foc %>% mutate(data_reuniao = as.Date(data_reuniao)), by = "data_reuniao")
write_csv(cmp, "output/comparacao_surpresas.csv")

cat("== Surpresas por reunião, fev/2018-jun/2025 ==\n")
cat(sprintf("reuniões: %d | não nulas DI: %d | não nulas Focus: %d | correlação: %.2f\n", nrow(cmp),
            sum(cmp$sup_di != 0, na.rm = TRUE), sum(cmp$sup_focus != 0, na.rm = TRUE),
            cor(cmp$sup_di, cmp$sup_focus, use = "complete.obs")))
cat("\n== h = 18 ==\n")
print(res %>% transmute(rodada, serie, p = p_bic, n, k, nz = surpresas_nao_nulas, m = round(m, 2),
                        ic = sprintf("[%.2f; %.2f]", ic_lo, ic_hi), F = round(F_efetivo, 1),
                        AR = sprintf("[%.1f; %.1f]%s", ar_lo, ar_hi, ifelse(ar_ilimitado, "*", "")), BC = bc_reportado),
      n = 100, width = 200)

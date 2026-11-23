## 05_documento.R
## Gera tabelas, números e figuras do documento de estudo tex/replicacao_secao4.tex: as duas
## regressões do MQ2E (primeiro estágio e segundo estágio) com todos os coeficientes, a forma
## reduzida, a identidade de Wald, um exemplo de montagem das variáveis e os quadros de resultado.
## Nenhum número do documento é digitado à mão: tudo sai daqui para tex/gerado/.
## Rodar da pasta do estudo, depois de 01 a 03: Rscript R/05_documento.R

source("R/funcoes.R")
suppressPackageStartupMessages(library(ggplot2))
dir.create("tex/gerado", recursive = TRUE, showWarnings = FALSE)

painel <- read_csv("output/painel_mensal.csv", show_col_types = FALSE) %>% arrange(data)
pb <- painel %>% filter(data <= FIM_BC)

## ---- formatação ------------------------------------------------------------------------------
num <- function(x, d = 3) {
  # "{,}" e \ensuremath{-} funcionam dentro e fora de fórmula (a vírgula em modo matemático ganha espaço)
  s <- gsub(".", "{,}", formatC(abs(x), format = "f", digits = d), fixed = TRUE)
  ifelse(is.na(x), "", ifelse(x < 0, paste0("\\ensuremath{-}", s), s))
}
mes <- function(d) format(as.Date(d), "%m/%Y")
macros <- character()
def <- function(nome, valor) macros <<- c(macros, sprintf("\\newcommand{\\%s}{%s}", nome, valor))
grava <- function(linhas, arq) writeLines(linhas, file.path("tex/gerado", arq), useBytes = TRUE)

rotulo_var <- c("(Intercept)" = "Constante", x = "$X^h_t$ (Selic, média de $t$ a $t+h$, menos $i_{t-1}$)",
                xhat = "$\\widehat{X}^h_t$ (valor previsto no 1º estágio)", z = "$z_t$ (surpresa do Copom)",
                covid = "Dummy de pandemia")
rot <- function(v) {
  if (v %in% names(rotulo_var)) return(rotulo_var[[v]])
  m <- regmatches(v, regexec("^(dy|di|dy2|dU|dP|dA)_l([0-9])$", v))[[1]]
  base <- c(dy = "\\Delta y", di = "\\Delta i", dy2 = "\\Delta y^{(2)}", dU = "\\Delta u",
            dP = "\\Delta p", dA = "\\Delta a")[m[2]]
  sprintf("$%s_{t-%s}$", base, m[3])
}

## ---- 1. surpresas ------------------------------------------------------------------------------
surp <- read_csv("output/surpresa_copom_reuniao.csv", show_col_types = FALSE) %>%
  filter(data_reuniao >= as.Date("2011-03-01"), data_reuniao <= as.Date("2025-06-30"))
nz <- surp %>% filter(!is.na(surpresa), abs(surpresa) >= 0.05)
def("Nreunioes", nrow(surp %>% filter(!is.na(surpresa))))
def("Nsurpresas", nrow(nz))
def("Nmeses", nrow(pb))
def("Nmesessurp", sum(pb$surpresa != 0))
grava(c("\\begin{tabular}{llrrrr}", "\\toprule",
        "Reunião & Data & Selic anterior & Mediana Focus & Decisão & Surpresa (p.p.) \\\\", "\\midrule",
        sprintf("%s & %s & %s & %s & %s & %s \\\\", nz$rotulo, format(nz$data_reuniao, "%d/%m/%Y"),
                num(nz$meta_anterior, 2), num(nz$focus_mediana, 2), num(nz$meta_selic, 2), num(nz$surpresa, 2)),
        "\\bottomrule", "\\end{tabular}"), "tab_surpresas.tex")

## exemplo de uma reunião (a primeira surpresa de alta do ciclo de 2021)
ex <- nz %>% filter(format(data_reuniao, "%Y") == "2021") %>% slice(1)
def("ExReuniao", ex$rotulo); def("ExData", format(ex$data_reuniao, "%d/%m/%Y"))
def("ExFocusData", format(ex$focus_data, "%d/%m/%Y")); def("ExAnterior", num(ex$meta_anterior, 2))
def("ExMediana", num(ex$focus_mediana, 2)); def("ExDecisao", num(ex$meta_selic, 2))
def("ExSurpresa", num(ex$surpresa, 2))

## ---- 2. dados: conferência com a Tabela 4.1 -------------------------------------------------------
def("MediaLivre", num(mean(pb$concb_liv) / 1e3, 1)); def("MediaDir", num(mean(pb$concb_dir) / 1e3, 1))
def("MediaTotal", num(mean(pb$concb_total) / 1e3, 1))
def("ParticDir", num(100 * mean(pb$concb_dir) / mean(pb$concb_total), 1))

## ---- 3. exemplo de montagem das variáveis da projeção local (taxa do crédito livre, h = 18) --------
h <- 18
d_ex <- monta(pb, "tx_liv", h = h, p = 2, macro = FALSE)
n <- nrow(pb); mf <- function(x) sapply(seq_len(n), function(t) if (t + h <= n) mean(x[t:(t + h)]) else NA)
aux <- tibble(data = pb$data, z = pb$surpresa, i_ant = dplyr::lag(pb$selic), i_med = mf(pb$selic),
              y_ant = dplyr::lag(pb$tx_liv), y_med = mf(pb$tx_liv)) %>%
  mutate(X = i_med - i_ant, Y = y_med - y_ant)
sel <- aux %>% filter(data >= ex$data_reuniao - 62, data <= ex$data_reuniao + 95)
stopifnot(isTRUE(all.equal(sel$Y, d_ex$lhs[match(sel$data, d_ex$data)])),
          isTRUE(all.equal(sel$X, d_ex$x[match(sel$data, d_ex$data)])))
grava(c("\\begin{tabular}{lrrrrrrr}", "\\toprule",
        "Mês $t$ & $z_t$ & $i_{t-1}$ & $\\bar{i}_{t,t+18}$ & $X^{18}_t$ & $y_{t-1}$ & $\\bar{y}_{t,t+18}$ & $Y^{18}_t$ \\\\",
        "\\midrule",
        sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\", mes(sel$data), num(sel$z, 2), num(sel$i_ant, 2),
                num(sel$i_med, 2), num(sel$X, 2), num(sel$y_ant, 2), num(sel$y_med, 2), num(sel$Y, 2)),
        "\\bottomrule", "\\end{tabular}"), "tab_exemplo_variaveis.tex")

## ---- 4. as duas regressões, a forma reduzida e a identidade de Wald --------------------------------
duas_regressoes <- function(y, macro, sufixo) {
  d <- estima_dados(pb, y, h = 18, p = 2, macro = macro)
  ctrls <- setdiff(names(d), c("data", "lhs", "x", "z")); fc <- paste(ctrls, collapse = " + ")
  L <- 19
  fs <- lm(as.formula(sprintf("x ~ z + %s", fc)), data = d); Vfs <- NeweyWest(fs, lag = L, prewhite = FALSE)
  rf <- lm(as.formula(sprintf("lhs ~ z + %s", fc)), data = d); Vrf <- NeweyWest(rf, lag = L, prewhite = FALSE)
  d$xhat <- fitted(fs)
  ss <- lm(as.formula(sprintf("lhs ~ xhat + %s", fc)), data = d); Vss <- NeweyWest(ss, lag = L, prewhite = FALSE)
  iv <- ivreg(as.formula(sprintf("lhs ~ x + %s | z + %s", fc, fc)), data = d)
  Viv <- NeweyWest(iv, lag = L, prewhite = FALSE)
  ols <- lm(as.formula(sprintf("lhs ~ x + %s", fc)), data = d); Vols <- NeweyWest(ols, lag = L, prewhite = FALSE)
  stopifnot(abs(coef(ss)["xhat"] - coef(iv)["x"]) < 1e-8,
            abs(coef(rf)["z"] / coef(fs)["z"] - coef(iv)["x"]) < 1e-8)
  tabela <- function(m, V, primeiro) {
    b <- coef(m); se <- sqrt(diag(V)); ordem <- c(primeiro, setdiff(names(b), c(primeiro, "(Intercept)")), "(Intercept)")
    c("\\begin{tabular}{lrrr}", "\\toprule", "Regressor & Coeficiente & Erro-padrão NW & $t$ \\\\", "\\midrule",
      sprintf("%s & %s & %s & %s \\\\", sapply(ordem, rot), num(b[ordem]), num(se[ordem]), num(b[ordem] / se[ordem], 2)),
      "\\midrule", sprintf("\\multicolumn{4}{l}{$N$ = %d meses (%s a %s); $R^2$ = %s} \\\\", nrow(d),
                           mes(min(d$data)), mes(max(d$data)), num(summary(m)$r.squared, 3)),
      "\\bottomrule", "\\end{tabular}")
  }
  grava(tabela(fs, Vfs, "z"), sprintf("tab_fs_%s.tex", sufixo))
  grava(tabela(rf, Vrf, "z"), sprintf("tab_rf_%s.tex", sufixo))
  grava(tabela(ss, Vss, "xhat"), sprintf("tab_ss_%s.tex", sufixo))
  s <- toupper(sufixo)
  def(paste0("PiFS", s), num(coef(fs)["z"])); def(paste0("PiRF", s), num(coef(rf)["z"]))
  def(paste0("Beta", s), num(coef(iv)["x"])); def(paste0("Wald", s), num(coef(rf)["z"] / coef(fs)["z"]))
  def(paste0("SEiv", s), num(sqrt(Viv["x", "x"]))); def(paste0("SEss", s), num(sqrt(Vss["xhat", "xhat"])))
  def(paste0("Fef", s), num((coef(fs)["z"] / sqrt(Vfs["z", "z"]))^2, 1))
  def(paste0("Beta", s, "ols"), num(coef(ols)["x"])); def(paste0("SEols", s), num(sqrt(Vols["x", "x"])))
  def(paste0("N", s), nrow(d)); def(paste0("Ini", s), mes(min(d$data)))
  def(paste0("ICl", s), num(coef(iv)["x"] - 1.96 * sqrt(Viv["x", "x"]), 2))
  def(paste0("ICh", s), num(coef(iv)["x"] + 1.96 * sqrt(Viv["x", "x"]), 2))

  ## ---- passo a passo do MQ2E pela residualização (Frisch-Waugh-Lovell) ----------------------------
  W <- model.matrix(as.formula(paste("~", fc)), data = d)
  rw <- function(v) as.numeric(lm.fit(W, v)$residuals)
  zt <- rw(d$z); Xt <- rw(d$x); Yt <- rw(d$lhs)
  zz <- sum(zt^2); zX <- sum(zt * Xt); zY <- sum(zt * Yt)
  stopifnot(abs(zY / zX - coef(iv)["x"]) < 1e-8, abs(zX / zz - coef(fs)["z"]) < 1e-8,
            abs(zY / zz - coef(rf)["z"]) < 1e-8)
  u_ok <- as.numeric(residuals(iv)); u_ss <- as.numeric(residuals(ss))
  ajust_ok <- d$lhs - u_ok
  stopifnot(max(abs(ajust_ok - as.numeric(cbind(d$x, W) %*% coef(iv)[c("x", colnames(W))]))) < 1e-8)
  # Newey-West à mão: S = sum_j w_j sum_t g_t g_{t-j}, g_t = z~_t u_t, w_j = 1 - j/(L+1)
  g <- zt * u_ok; n <- length(g)
  S <- sum(g^2) + 2 * sum(sapply(seq_len(L), function(j) (1 - j / (L + 1)) * sum(g[(j + 1):n] * g[1:(n - j)])))
  se_man <- sqrt(S) / abs(zX)
  stopifnot(abs(se_man - sqrt(Viv["x", "x"])) < 1e-6)
  se_hc <- sqrt(sum(g^2)) / abs(zX)                                     # sem autocorrelação
  se_hom <- sqrt(sum(u_ok^2) / (n - ncol(W) - 1) * zz) / abs(zX)          # homocedástico, sem autocorrelação
  def(paste0("ZZ", s), num(zz)); def(paste0("ZX", s), num(zX)); def(paste0("ZY", s), num(zY))
  def(paste0("Pman", s), num(zX / zz)); def(paste0("Rman", s), num(zY / zz)); def(paste0("Bman", s), num(zY / zX))
  def(paste0("Sman", s), num(S)); def(paste0("SEman", s), num(se_man)); def(paste0("SEhc", s), num(se_hc))
  def(paste0("SEhom", s), num(se_hom)); def(paste0("SSRok", s), num(sum(u_ok^2), 2)); def(paste0("SSRss", s), num(sum(u_ss^2), 2))
  def(paste0("Kw", s), ncol(W)); def(paste0("Lnw", s), L)
  if (sufixo == "tl") {
    sel <- which(d$data >= as.Date("2021-01-01") & d$data <= as.Date("2021-06-01"))
    grava(c("\\begin{tabular}{lrrrrrrr}", "\\toprule",
            "Mês $t$ & $z_t$ & $X^{18}_t$ & $\\widehat{X}^{18}_t$ & $Y^{18}_t$ & $\\hat\\beta X_t + W_t'\\hat\\gamma$ & $\\hat u_t$ (certo) & $\\hat e_t$ (2º estágio à mão) \\\\",
            "\\midrule",
            sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\", mes(d$data[sel]), num(d$z[sel], 2), num(d$x[sel], 2),
                    num(fitted(fs)[sel], 2), num(d$lhs[sel], 2), num(ajust_ok[sel], 2), num(u_ok[sel], 2), num(u_ss[sel], 2)),
            "\\bottomrule", "\\end{tabular}"), "tab_residuos_tl.tex")
    grava(c("\\begin{tabular}{lrrrr}", "\\toprule",
            "Mês $t$ & $\\tilde z_t$ & $\\tilde X_t$ & $\\tilde Y_t$ & $g_t=\\tilde z_t\\hat u_t$ \\\\", "\\midrule",
            sprintf("%s & %s & %s & %s & %s \\\\", mes(d$data[sel]), num(zt[sel], 3), num(Xt[sel], 3), num(Yt[sel], 3), num(g[sel], 3)),
            "\\bottomrule", "\\end{tabular}"), "tab_residualizado_tl.tex")
  }
}
# estima_dados: as mesmas linhas que estima() usa
estima_dados <- function(df, y, h, p, macro) monta(df, y, h, p, macro) %>% na.omit()
duas_regressoes("tx_liv", macro = FALSE, sufixo = "tl")        # taxa, crédito livre
duas_regressoes("lconc_dir_pj", macro = TRUE, sufixo = "cd")   # concessões, PJ direcionado

## ---- 5. quadros de resultado ------------------------------------------------------------------------
res <- read_csv("output/resultados_h18.csv", show_col_types = FALSE)
base <- res %>% filter(p == 2, fim == FIM_BC, (macro %in% TRUE) | grepl("^tx_", serie), !c12 %in% TRUE)
nomes <- c(tx_total = "Total", tx_liv = "Recursos livres", tx_dir = "Recursos direcionados",
           tx_liv_pj = "PJ, livres", tx_liv_pf = "PF, livres", tx_dir_pj = "PJ, direcionados", tx_dir_pf = "PF, direcionados",
           lconc_total = "Total", lconc_liv = "Recursos livres", lconc_dir = "Recursos direcionados",
           lconc_liv_pj = "PJ, livres", lconc_liv_pf = "PF, livres", lconc_dir_pj = "PJ, direcionados",
           lconc_dir_pf = "PF, direcionados", lconc_pj = "PJ, total", lconc_pf = "PF, total")
bc <- c(tx_liv = "1{,}05", tx_dir = "0{,}28", lconc_dir_pj = "\\ensuremath{-}5{,}3", lconc_dir_pf = "\\ensuremath{-}2{,}8",
        lconc_liv_pj = "\\ensuremath{-}0{,}3 (n.s.)", lconc_liv_pf = "\\ensuremath{-}0{,}9 (n.s.)")
quadro <- function(series, d = 2) {
  b <- base %>% filter(serie %in% series) %>% arrange(match(serie, series))
  c("\\begin{tabular}{lrrrrrl}", "\\toprule",
    "Série & Multiplicador & IC 95\\% & F efetivo & Anderson-Rubin & MQO & BC (2026) \\\\", "\\midrule",
    sprintf("%s & %s & [%s; %s] & %s & [%s; %s] & %s & %s \\\\", nomes[b$serie], num(b$m_iv, d), num(b$ic_lo, d),
            num(b$ic_hi, d), num(b$F_efetivo, 1), num(b$ar_lo, d), num(b$ar_hi, d), num(b$m_ols, d),
            ifelse(b$serie %in% names(bc), bc[b$serie], "só no gráfico")),
    "\\bottomrule", "\\end{tabular}")
}
grava(quadro(c("tx_total", "tx_liv", "tx_dir", "tx_liv_pj", "tx_liv_pf", "tx_dir_pj", "tx_dir_pf")), "tab_res_taxas.tex")
grava(quadro(c("lconc_total", "lconc_liv", "lconc_dir", "lconc_pj", "lconc_pf", "lconc_liv_pj", "lconc_liv_pf",
               "lconc_dir_pj", "lconc_dir_pf")), "tab_res_volumes.tex")

dif <- read_csv("output/diferencas_h18.csv", show_col_types = FALSE)
grava(c("\\begin{tabular}{lrrr}", "\\toprule", "Diferença (direcionado $-$ livre) & Multiplicador & IC 95\\% & F efetivo \\\\", "\\midrule",
        sprintf("%s & %s & [%s; %s] & %s \\\\", c("PJ", "PF", "Total"), num(dif$m_iv, 2), num(dif$ic_lo, 2), num(dif$ic_hi, 2),
                num(dif$F_efetivo, 1)), "\\bottomrule", "\\end{tabular}"), "tab_diferencas.tex")

rob <- res %>% filter(serie %in% c("lconc_total", "lconc_dir_pj", "lconc_liv_pj"), !c12 %in% TRUE) %>%
  mutate(espec = case_when(p == 4 ~ "4 defasagens", fim > FIM_BC ~ "Amostra até 09/2026",
                           is.na(macro) ~ "Sem desocupação, desde 2011", TRUE ~ "Principal")) %>%
  bind_rows(read_csv("output/robustez_c12.csv", show_col_types = FALSE) %>% filter(c12) %>%
              mutate(espec = "Controle: crescimento dos 12 meses anteriores")) %>%
  filter(serie %in% c("lconc_total", "lconc_dir_pj", "lconc_liv_pj")) %>%
  arrange(match(serie, c("lconc_total", "lconc_dir_pj", "lconc_liv_pj")),
          match(espec, c("Principal", "4 defasagens", "Amostra até 09/2026", "Sem desocupação, desde 2011",
                         "Controle: crescimento dos 12 meses anteriores")))
grava(c("\\begin{tabular}{llrrr}", "\\toprule", "Série & Especificação & Multiplicador & IC 95\\% & F efetivo \\\\", "\\midrule",
        sprintf("%s & %s & %s & [%s; %s] & %s \\\\", nomes[rob$serie], rob$espec, num(rob$m_iv, 2), num(rob$ic_lo, 2),
                num(rob$ic_hi, 2), num(rob$F_efetivo, 1)), "\\bottomrule", "\\end{tabular}"), "tab_robustez.tex")

plc <- read_csv("output/placebo.csv", show_col_types = FALSE)
nomes_plc <- c(nomes, selic = "Selic")
grava(c("\\begin{tabular}{lrrr}", "\\toprule", "Série ($y_{t-1}-y_{t-13}$) & Coeficiente de $z_t$ & Erro-padrão NW & $t$ \\\\", "\\midrule",
        sprintf("%s & %s & %s & %s \\\\", ifelse(grepl("^tx_", plc$serie), paste("Taxa,", nomes_plc[plc$serie]),
                                                ifelse(plc$serie == "selic", "Selic", paste("Concessões,", nomes_plc[plc$serie]))),
                num(plc$coef_z, 2), num(plc$se, 2), num(plc$t, 2)), "\\bottomrule", "\\end{tabular}"), "tab_placebo.tex")

soma <- read_csv("output/soma_reais_h18.csv", show_col_types = FALSE)
def("TotalRS", num(soma$resposta_rs_bi[soma$serie == "lconc_total"], 2))
def("TotalRSlo", num(soma$lo_rs_bi[soma$serie == "lconc_total"], 1)); def("TotalRShi", num(soma$hi_rs_bi[soma$serie == "lconc_total"], 1))

## ---- 6. figuras ----------------------------------------------------------------------------------------
azul <- "#2a78d6"; laranja <- "#eb6834"
tr <- bind_rows(lapply(c("tx_liv", "tx_dir"), function(v)
  bind_rows(lapply(0:24, function(hh) estima_fim(painel, v, h = hh, macro = FALSE, ar = FALSE))))) %>%
  mutate(serie = dplyr::recode(serie, tx_liv = "Recursos livres", tx_dir = "Recursos direcionados"))
g1 <- ggplot(tr, aes(h, m_iv, colour = serie, fill = serie)) +
  geom_hline(yintercept = c(0, 1), colour = "grey55", linewidth = 0.3, linetype = c("solid", "dashed")) +
  geom_ribbon(aes(ymin = ic_lo, ymax = ic_hi), alpha = 0.15, colour = NA) + geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c("Recursos livres" = azul, "Recursos direcionados" = laranja)) +
  scale_fill_manual(values = c("Recursos livres" = azul, "Recursos direcionados" = laranja)) +
  labs(x = "Horizonte h (meses)", y = "Multiplicador cumulativo", colour = NULL, fill = NULL) +
  theme_minimal(base_size = 11) + theme(legend.position = "top", panel.grid.minor = element_blank())
ggsave("tex/gerado/fig_traj_taxas.pdf", g1, width = 6.2, height = 3.6)

tv <- read_csv("output/trajetoria.csv", show_col_types = FALSE) %>%
  mutate(serie = factor(nomes[serie], levels = nomes[c("lconc_total", "lconc_liv_pj", "lconc_dir_pj", "lconc_liv_pf", "lconc_dir_pf")]))
g2 <- ggplot(tv, aes(h, m_iv)) + geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_ribbon(aes(ymin = ic_lo, ymax = ic_hi), fill = azul, alpha = 0.15) + geom_line(colour = azul, linewidth = 0.8) +
  facet_wrap(~serie, ncol = 3) + labs(x = "Horizonte h (meses)", y = "Multiplicador cumulativo (%)") +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave("tex/gerado/fig_traj_volumes.pdf", g2, width = 6.4, height = 4.0)

sp <- pb %>% select(data, selic, surpresa) %>% tidyr::pivot_longer(-data) %>%
  mutate(name = dplyr::recode(name, selic = "Selic (% a.a.)", surpresa = "Surpresa do Copom contra o Focus (p.p., soma no mês)"))
g3 <- ggplot(sp, aes(data, value)) + geom_hline(yintercept = 0, colour = "grey60", linewidth = 0.3) +
  geom_line(data = ~ subset(.x, grepl("^Selic", name)), colour = azul, linewidth = 0.8) +
  geom_col(data = ~ subset(.x, grepl("^Surpresa", name)), fill = laranja, width = 25) +
  facet_wrap(~name, ncol = 1, scales = "free_y") + labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank())
ggsave("tex/gerado/fig_surpresas.pdf", g3, width = 6.4, height = 4.2)

writeLines(macros, "tex/gerado/numeros.tex", useBytes = TRUE)
cat("Gerado em tex/gerado/:", length(list.files("tex/gerado")), "arquivos;", length(macros), "macros\n")

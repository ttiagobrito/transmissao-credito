## funcoes.R: monta a regressão de projeção local, estima o multiplicador cumulativo por MQ2E,
## o F efetivo, o conjunto de Anderson-Rubin e o MQO. Usado por 02_lpiv.R e 03_placebo.R.

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(AER); library(sandwich); library(lmtest)
})
FIM_BC <- as.Date("2025-06-01")

monta <- function(df, y, h, p, macro, y2 = NULL, c12 = FALSE) {
  # y2: segunda série cujas defasagens entram como controle (regressão da diferença)
  n <- nrow(df); i <- df$selic; yy <- df[[y]]
  media_frente <- function(x) sapply(seq_len(n), function(t) if (t + h <= n) mean(x[t:(t + h)]) else NA)
  out <- tibble(data = df$data,
                lhs = media_frente(yy) - dplyr::lag(yy),
                x   = media_frente(i) - dplyr::lag(i),
                z   = df$surpresa, covid = df$covid)
  dif <- function(v) v - dplyr::lag(v)
  ctrl <- list(dy = dif(yy), di = dif(i))
  if (!is.null(y2)) ctrl$dy2 <- dif(df[[y2]])
  if (macro) ctrl <- c(ctrl, list(dU = dif(df$desocup), dP = dif(df$lipca), dA = dif(df$libcbr)))
  for (nm in names(ctrl)) for (l in seq_len(p)) out[[sprintf("%s_l%d", nm, l)]] <- dplyr::lag(ctrl[[nm]], l)
  if (c12) out$c12 <- dplyr::lag(yy, 1) - dplyr::lag(yy, 13)
  if (!macro) out$covid <- NULL
  out
}

# Anderson-Rubin pela forma residualizada (Frisch-Waugh): para cada m, testa se z explica
# (lhs - m x) com variância de Newey-West (Bartlett, L defasagens). Rápido o bastante para uma grade.
ar_conjunto <- function(yt, xt, zt, L, grade) {
  n <- length(zt); zz <- sum(zt^2)
  w <- 1 - seq_len(L) / (L + 1)
  sapply(grade, function(m) {
    u <- yt - m * xt; cc <- sum(zt * u) / zz; g <- zt * (u - cc * zt)
    S <- sum(g^2) + 2 * sum(w * sapply(seq_len(L), function(j) sum(g[(j + 1):n] * g[1:(n - j)])))
    abs(cc / sqrt(S / zz^2)) < 1.96
  })
}

estima <- function(df, y, h, p = 2, macro = TRUE, fim = FIM_BC, y2 = NULL, grade = seq(-40, 40, by = 0.05),
                   ar = TRUE, c12 = FALSE) {
  d <- monta(df, y, h, p, macro, y2, c12) %>%
    filter(data <= fim - months_ajuste(h, fim, df)) %>% na.omit()
  ctrls <- setdiff(names(d), c("data", "lhs", "x", "z"))
  fc <- paste(ctrls, collapse = " + ")
  iv  <- ivreg(as.formula(sprintf("lhs ~ x + %s | z + %s", fc, fc)), data = d)
  V   <- NeweyWest(iv, lag = h + 1, prewhite = FALSE)
  ols <- lm(as.formula(sprintf("lhs ~ x + %s", fc)), data = d)
  Vo  <- NeweyWest(ols, lag = h + 1, prewhite = FALSE)
  fs  <- lm(as.formula(sprintf("x ~ z + %s", fc)), data = d)
  Vf  <- NeweyWest(fs, lag = h + 1, prewhite = FALSE)
  F_ef <- (coef(fs)["z"] / sqrt(Vf["z", "z"]))^2
  # Anderson-Rubin: m está no conjunto se z não explica (lhs - m x) a 5%
  if (ar) {
    W <- model.matrix(as.formula(sprintf("~ %s", fc)), data = d)
    res_w <- function(v) as.numeric(lm.fit(W, v)$residuals)
    ar_ok <- ar_conjunto(res_w(d$lhs), res_w(d$x), res_w(d$z), L = h + 1, grade = grade)
  } else ar_ok <- rep(FALSE, length(grade))
  ar_lo <- if (any(ar_ok)) min(grade[ar_ok]) else NA
  ar_hi <- if (any(ar_ok)) max(grade[ar_ok]) else NA
  ar_aberto <- any(ar_ok) && (ar_ok[1] || ar_ok[length(ar_ok)])
  b <- coef(iv)["x"]; se <- sqrt(V["x", "x"])
  tibble(serie = y, h = h, p = p, macro = macro, c12 = c12, fim = fim, n = nrow(d),
         inicio = min(d$data), m_iv = b, se_iv = se, ic_lo = b - 1.96 * se, ic_hi = b + 1.96 * se,
         F_efetivo = F_ef, ar_lo = ar_lo, ar_hi = ar_hi, ar_ilimitado = ar_aberto,
         m_ols = coef(ols)["x"], se_ols = sqrt(Vo["x", "x"]))
}

# a amostra de t termina quando t+h alcança o fim dos dados usados (fim)
months_ajuste <- function(h, fim, df) 0
estima_fim <- function(df, ..., fim = FIM_BC) estima(df %>% filter(data <= fim), ..., fim = fim)


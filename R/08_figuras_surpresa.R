## 08_figuras_surpresa.R: o que é uma surpresa, em figura.
##  (a) curva do DI futuro no dia de uma decisão e no pregão seguinte, com o contrato usado;
##  (b) surpresas do DI futuro e do Focus em todas as reuniões de fev/2018 a jun/2025.
## Paleta categórica da skill dataviz (#2a78d6, #eb6834), validada.
## Saídas: output/fig_surpresa_exemplo.png, output/fig_surpresas_di_focus.png
## Rodar da pasta do estudo, depois de py/06_surpresa_di.py e R/01_dados.R.

suppressPackageStartupMessages({ library(dplyr); library(readr); library(ggplot2) })
azul <- "#2a78d6"; laranja <- "#eb6834"; tinta <- "#0b0b0b"; tinta2 <- "#52514e"; fundo <- "#fcfcfb"
virg <- function(x, d = 2) gsub("-", "−", formatC(x, format = "f", digits = d, decimal.mark = ","))
tema <- theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = fundo, colour = NA), panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", colour = tinta), plot.subtitle = element_text(colour = tinta2),
        plot.caption = element_text(colour = tinta2, hjust = 0, size = 8.5), plot.title.position = "plot",
        plot.caption.position = "plot", legend.position = "top", legend.justification = "left")

di1 <- read_csv("data_b3/tratado/di1_ajustes.csv", show_col_types = FALSE)
surp <- read_csv("output/surpresa_di_reuniao.csv", show_col_types = FALSE)

## ---- (a) um exemplo: 17/03/2021 ------------------------------------------------------------------
ex <- surp %>% filter(data_reuniao == as.Date("2021-03-17"))
meses <- c(F = 1, G = 2, H = 3, J = 4, K = 5, M = 6, N = 7, Q = 8, U = 9, V = 10, X = 11, Z = 12)
venc <- function(tk) as.Date(sprintf("20%s-%02d-01", substr(tk, 5, 6), meses[substr(tk, 4, 4)]))
curva <- di1 %>% filter(data %in% c(ex$data_reuniao, ex$data_seguinte)) %>%
  mutate(vencimento = venc(ticker),
         dia = ifelse(data == ex$data_reuniao, "Fechamento do dia da decisão (17/03/2021)",
                      "Fechamento do dia seguinte (18/03/2021)")) %>%
  filter(vencimento <= as.Date("2022-04-01"))
escolhido <- curva %>% filter(ticker == ex$contrato)
ga <- ggplot(curva, aes(vencimento, taxa_ajuste, colour = dia)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2.2) +
  geom_point(data = escolhido, size = 4.2, shape = 21, fill = fundo, stroke = 1.4) +
  annotate("segment", x = venc(ex$contrato) + 12, xend = venc(ex$contrato) + 12, y = ex$termo_t, yend = ex$taxa_t1,
           colour = tinta, linewidth = 0.5, arrow = arrow(length = unit(0.15, "cm"), ends = "both")) +
  annotate("segment", x = venc(ex$contrato) + 14, xend = as.Date("2021-05-25"), y = (ex$termo_t + ex$taxa_t1) / 2,
           yend = 4.25, colour = tinta2, linewidth = 0.3) +
  annotate("label", x = as.Date("2021-05-25"), y = 4.55, hjust = 0, size = 3.4, colour = tinta, fill = fundo,
           label.size = 0,
           label = sprintf("Contrato %s (%d dias úteis)\nTaxa a termo em t: %s%%\nTaxa no dia seguinte: %s%%\nSurpresa: +%s p.p.",
                           ex$contrato, ex$du, virg(ex$termo_t, 3), virg(ex$taxa_t1, 3), virg(ex$surpresa, 3))) +
  scale_colour_manual(values = c(laranja, azul)) +
  scale_y_continuous(labels = function(x) virg(x, 1)) +
  labs(title = "O que é uma surpresa: o mercado reprecifica os juros depois do Copom",
       subtitle = "Taxas de ajuste dos contratos de DI futuro, por vencimento (% a.a.)",
       x = "Vencimento do contrato", y = NULL, colour = NULL,
       caption = paste0("O Copom subiu a Selic de 2,00% para 2,75% em 17/03/2021, depois do fechamento. ",
                        "O contrato usado é o primeiro com mais de 10 e menos de 40 dias úteis.\n",
                        "Fonte: B3 (arquivos BVBG.086). Cálculo como em BC (2026), p. 44.")) + tema
ggsave("output/fig_surpresa_exemplo.png", ga, width = 9, height = 5.6, dpi = 150, bg = fundo)

## ---- (b) todas as reuniões: DI futuro x Focus ---------------------------------------------------------
foc <- read_csv("output/surpresa_copom_reuniao.csv", show_col_types = FALSE) %>%
  transmute(data_reuniao = as.Date(data_reuniao), focus = surpresa)
todas <- surp %>% transmute(data_reuniao = as.Date(data_reuniao), di = surpresa_bruta) %>%
  left_join(foc, by = "data_reuniao")
gb <- ggplot(todas, aes(data_reuniao)) +
  annotate("rect", xmin = min(todas$data_reuniao) - 30, xmax = max(todas$data_reuniao) + 30,
           ymin = -0.05, ymax = 0.05, fill = "#e6e5e1", alpha = 0.8) +
  geom_hline(yintercept = 0, colour = tinta2, linewidth = 0.3) +
  geom_col(aes(y = di, fill = "DI futuro (variação da taxa no dia seguinte)"), width = 22) +
  geom_point(aes(y = focus, colour = "Focus (decisão menos mediana dos analistas)"), size = 2.4) +
  annotate("segment", x = as.Date("2021-10-27"), xend = as.Date("2021-10-27"), y = 0.29, yend = 0.265,
           colour = tinta2, linewidth = 0.3, arrow = arrow(length = unit(0.12, "cm"))) +
  annotate("text", x = as.Date("2021-10-27"), y = 0.30, label = "out/2021: sinais opostos", size = 3.2,
           colour = tinta2, hjust = 0.5, vjust = 0) +
  annotate("text", x = min(todas$data_reuniao), y = 0.065, label = "faixa de ±5 p.b.: o BC trata como zero",
           size = 3.1, colour = tinta2, hjust = 0) +
  scale_fill_manual(values = azul) + scale_colour_manual(values = laranja) +
  scale_y_continuous(labels = function(x) virg(x, 2)) +
  labs(title = "Surpresas do Copom, 2018 a 2025: mercado e analistas",
       subtitle = "Surpresa em cada reunião (p.p.)", x = NULL, y = NULL, fill = NULL, colour = NULL,
       caption = "Fontes: B3 (DI futuro); BCB (Focus, histórico do Copom). Cálculo como em BC (2026), p. 43–44.") + tema
ggsave("output/fig_surpresas_di_focus.png", gb, width = 9, height = 5, dpi = 150, bg = fundo)
cat("Salvo: output/fig_surpresa_exemplo.png e output/fig_surpresas_di_focus.png\n")

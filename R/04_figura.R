## 04_figura.R: figura do post. Resposta das concessões a +1 p.p. de Selic em 18 meses, por segmento:
## réplica com dados públicos (ponto + IC 95%) e o número reportado pelo BC (2026, Gráfico 4.4).
## Paleta: categórica padrão da skill dataviz (#2a78d6, #eb6834), validada (ΔE CVD 24,7).
## Saída: output/fig_concessoes_h18.png. Rodar da pasta do estudo.

suppressPackageStartupMessages({ library(dplyr); library(readr); library(ggplot2); library(scales) })

res <- read_csv("output/resultados_h18.csv", show_col_types = FALSE) %>%
  filter(p == 2, macro %in% TRUE, fim == as.Date("2025-06-01"), !c12 %in% TRUE)

ordem <- c(lconc_total = "Crédito total", lconc_liv_pj = "PJ, recursos livres",
           lconc_dir_pj = "PJ, recursos direcionados", lconc_liv_pf = "PF, recursos livres",
           lconc_dir_pf = "PF, recursos direcionados")
bc <- tibble(serie = c("lconc_liv_pj", "lconc_dir_pj", "lconc_liv_pf", "lconc_dir_pf"),
             valor = c(-0.3, -5.3, -0.9, -2.8))   # BC (2026), p. 51

d <- res %>% filter(serie %in% names(ordem)) %>%
  mutate(rotulo = factor(ordem[serie], levels = rev(ordem)))
bc <- bc %>% mutate(rotulo = factor(ordem[serie], levels = rev(ordem)))

azul <- "#2a78d6"; laranja <- "#eb6834"; tinta <- "#0b0b0b"; tinta2 <- "#52514e"; fundo <- "#fcfcfb"
virg <- label_number(accuracy = 0.1, decimal.mark = ",")
fmt_n <- function(x) gsub("-", "−", virg(x))

g <- ggplot(d, aes(y = rotulo)) +
  geom_vline(xintercept = 0, colour = tinta2, linewidth = 0.4) +
  geom_errorbar(aes(xmin = ic_lo, xmax = ic_hi, colour = "Réplica com dados públicos (IC de 95%)"),
                width = 0, linewidth = 0.9, orientation = "y") +
  geom_point(aes(x = m_iv, colour = "Réplica com dados públicos (IC de 95%)"), size = 3.2) +
  geom_point(data = bc, aes(x = valor, colour = "Número reportado pelo BC"), shape = 21, fill = fundo,
             size = 3.4, stroke = 1.4) +
  geom_label(aes(x = m_iv, label = fmt_n(m_iv)), nudge_y = 0.28, size = 3.4, colour = tinta2,
             fill = fundo, label.size = 0, label.padding = unit(0.12, "lines")) +
  geom_text(data = bc, aes(x = valor, label = fmt_n(valor)), nudge_y = -0.28, size = 3.4, colour = tinta2) +
  annotate("text", x = 4.2, y = "Crédito total", label = "BC não reporta", hjust = 0, size = 3.3,
           colour = tinta2, fontface = "italic") +
  scale_colour_manual(values = c("Réplica com dados públicos (IC de 95%)" = azul,
                                 "Número reportado pelo BC" = laranja),
                      breaks = c("Réplica com dados públicos (IC de 95%)", "Número reportado pelo BC"),
                      guide = guide_legend(override.aes = list(shape = c(16, 21), linetype = c(1, 0),
                                                               fill = fundo, size = 3))) +
  scale_x_continuous(labels = function(x) gsub("-", "−", x), breaks = seq(-10, 6, 2), limits = c(-10.6, 7.2)) +
  labs(title = "A Selic mais alta reduz o volume de crédito?",
       subtitle = paste0("Variação média das concessões nos 18 meses seguintes a uma Selic 1 p.p. mais alta (%).\n",
                         "Projeções locais com instrumento, como no BC (2026); surpresa do Copom medida contra o Focus."),
       x = NULL, y = NULL, colour = NULL,
       caption = paste0("Concessões reais dessazonalizadas (SGS 24439 a 24447), jun/2012 a jun/2025. O BC usa a surpresa no DI futuro e\n",
                        "diz que o resultado é robusto à surpresa contra o Focus (nota 41). Erros de Newey-West.\n",
                        "Fontes: BCB (SGS, Focus, Copom); BC (2026), Transmissão da Política Monetária por meio do Crédito, p. 51.\n",
                        "Elaboração: Tiago Müller de Brito. Código: github.com/ttiagobrito/transmissao-credito")) +
  theme_minimal(base_size = 13) +
  theme(plot.background = element_rect(fill = fundo, colour = NA),
        panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(colour = "#e6e5e1", linewidth = 0.3),
        axis.text.y = element_text(colour = tinta, size = 12), axis.text.x = element_text(colour = tinta2),
        plot.title = element_text(face = "bold", colour = tinta, size = 17),
        plot.subtitle = element_text(colour = tinta2, size = 11.5, lineheight = 1.15),
        plot.caption = element_text(colour = tinta2, size = 8.6, hjust = 0, lineheight = 1.15),
        plot.title.position = "plot", plot.caption.position = "plot",
        legend.position = "top", legend.justification = "left", legend.text = element_text(colour = tinta, size = 11),
        plot.margin = margin(18, 22, 14, 18))

ggsave("output/fig_concessoes_h18.png", g, width = 8, height = 8, dpi = 150, bg = fundo)
cat("Salvo: output/fig_concessoes_h18.png\n")

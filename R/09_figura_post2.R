## 09_figura_post2.R: figura do post 2. Concessões em 18 meses por +1 p.p. de Selic, por segmento,
## na especificação do BC: réplica com a surpresa no DI futuro (2018-2023), réplica com a surpresa
## pela Focus (2011-2023) e o número que o BC reporta (p. 51).
## Paleta categórica da skill dataviz (#2a78d6, #1baf7a, #eb6834), validada (ΔE CVD mínimo 9,2).
## Saída: output/fig_post2_concessoes.png. Rodar da pasta do estudo, depois de R/07_replica_bc.R.

suppressPackageStartupMessages({ library(dplyr); library(readr); library(ggplot2) })
azul <- "#2a78d6"; verde <- "#1baf7a"; laranja <- "#eb6834"; tinta <- "#0b0b0b"; tinta2 <- "#52514e"; fundo <- "#fcfcfb"
virg <- function(x, d = 1) gsub("-", "−", formatC(x, format = "f", digits = d, decimal.mark = ","))

ordem <- c(lconc_total = "Crédito total", lconc_liv_pj = "Empresas, recursos livres",
           lconc_dir_pj = "Empresas, recursos direcionados", lconc_liv_pf = "Famílias, recursos livres",
           lconc_dir_pf = "Famílias, recursos direcionados")
rot_di <- "Réplica, surpresa no DI futuro (2018 a 2023)"
rot_fo <- "Réplica, surpresa pela Focus (2011 a 2023)"
rot_bc <- "Número do BC (2026)"

r <- read_csv("output/replica_bc_h18.csv", show_col_types = FALSE) %>%
  filter(serie %in% names(ordem), rodada %in% c("A_focus_2011", "B_di_2018")) %>%
  mutate(versao = ifelse(rodada == "B_di_2018", rot_di, rot_fo),
         rotulo = factor(ordem[serie], levels = rev(ordem)),
         y = as.numeric(rotulo) + ifelse(versao == rot_di, 0.17, -0.17))
bc <- tibble(serie = c("lconc_liv_pj", "lconc_dir_pj", "lconc_liv_pf", "lconc_dir_pf"), m = c(-0.3, -5.3, -0.9, -2.8)) %>%
  mutate(rotulo = factor(ordem[serie], levels = rev(ordem)), y = as.numeric(rotulo), versao = rot_bc)
fraco <- r %>% filter(F_efetivo < 10, serie == "lconc_total")

g <- ggplot() +
  geom_vline(xintercept = 0, colour = tinta2, linewidth = 0.4) +
  geom_errorbar(data = r, aes(y = y, xmin = pmax(ic_lo, -16), xmax = pmin(ic_hi, 7), colour = versao),
                width = 0, linewidth = 0.9, orientation = "y") +
  geom_point(data = r, aes(y = y, x = m, colour = versao), size = 3) +
  geom_point(data = bc, aes(y = y, x = m, colour = versao), shape = 21, fill = fundo, size = 3.4, stroke = 1.4) +
  geom_text(data = r, aes(y = y, x = pmin(ic_hi, 7) + 0.3, label = virg(m)), hjust = 0, size = 3.2, colour = tinta2) +
  geom_text(data = fraco, aes(y = y, x = ic_lo - 0.4, label = sprintf("instrumento fraco (F = %s)", virg(F_efetivo))),
            hjust = 1, size = 3, colour = tinta2, fontface = "italic") +
  annotate("text", x = 4.6, y = 5.45, label = "BC não mostra", size = 3.1, colour = tinta2, fontface = "italic") +
  scale_colour_manual(values = setNames(c(azul, verde, laranja), c(rot_di, rot_fo, rot_bc)), breaks = c(rot_di, rot_fo, rot_bc),
                      guide = guide_legend(ncol = 1, override.aes = list(shape = c(16, 16, 21), fill = fundo, linetype = c(1, 1, 0)))) +
  scale_y_continuous(breaks = seq_along(levels(r$rotulo)), labels = levels(r$rotulo)) +
  scale_x_continuous(labels = function(x) gsub("-", "−", x), breaks = seq(-16, 6, 4), limits = c(-16.5, 9)) +
  labs(title = "A Selic mais alta reduz as concessões de crédito?",
       subtitle = "Variação média das concessões nos 18 meses seguintes a uma Selic 1 p.p. mais alta (%), com intervalo de 95%",
       x = NULL, y = NULL, colour = NULL,
       caption = paste0("Especificação do BC (2026), p. 45-46: projeções locais com instrumento, controles e defasagens pelo BIC.\n",
                        "A B3 só publica os arquivos de DI futuro desde 2018. Os anos indicam o mês t da projeção (até 12/2023).\n",
                        "Fontes: BCB (SGS, Focus, Copom), B3, FGV (IIE-Br); BC (2026). Elaboração: Tiago Müller de Brito.")) +
  theme_minimal(base_size = 12) +
  theme(plot.background = element_rect(fill = fundo, colour = NA), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(), axis.text.y = element_text(colour = tinta, size = 11),
        plot.title = element_text(face = "bold", colour = tinta, size = 15),
        plot.subtitle = element_text(colour = tinta2, size = 10.5), plot.caption = element_text(colour = tinta2, hjust = 0, size = 8.3),
        plot.title.position = "plot", plot.caption.position = "plot", legend.position = "top", legend.justification = "left",
        plot.margin = margin(16, 20, 12, 16))
ggsave("output/fig_post2_concessoes.png", g, width = 8.6, height = 7, dpi = 150, bg = fundo)
cat("Salvo: output/fig_post2_concessoes.png\n")

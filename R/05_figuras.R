# =============================================================================
# 05_figuras.R  ->  PDF em saidas/figuras (Corpo do Paper + Mapas + Mecanismos)
#
# FIGURAS GERADAS:
#   F1   f1_densidade.pdf                 Distribuicao da portabilidade + cortes de 2010
#   F2   f2_serie.pdf                     Serie nacional (corrente vs. fixa em 2010)
#   F2b  f2b_shiftshare_acumulado.pdf     Decomposicao shift-share acumulada ano a ano
#   F3   f3_composicao.pdf                Evolucao dos 3 grupos fixos no emprego formal
#   F3b  f3b_grandes_grupos_box.pdf       Distribuicao por Grande Grupo da CBO (Anexo)
#   F4   f4_mapa_flex_painel.pdf          Mapas de Flexibilidade (quintis + bordas de UF)
#        f4_mapa_flex_<janela>.pdf        Mapa individual por janela
#   F4b  f4b_mapa_T_D_painel.pdf          Mapas comparativos de Turbulencia (T) e Descasamento (D)
#   F5   f5_mapa_portab_painel.pdf        Mapas de Portabilidade regional (quintis + UF)
#        f5_mapa_portab_<janela>.pdf      Mapa individual por janela
#   F5b  f5b_mapa_grupo1_share.pdf        Mapa da participacao do Grupo 1 (Alta portab.)
#   F6   f6_componentes.pdf               Turbulencia (T) e Descasamento (D) no tempo
#   F6b  f6b_dispersao_T_D.pdf            Dispersao regional log(T) vs. log(D) (Anexo)
#   F7   f7_salario.pdf                   Portabilidade vs. Salario regional (com rotulos)
#   F8   f8_kaplan_meier.pdf              Curvas de Kaplan-Meier (24 meses, 3 grupos)
#   F8b  f8b_kaplan_meier_longo.pdf       Curvas de Kaplan-Meier de longo prazo (ate 84m / 7 anos)
#   F8c  f8c_acumulado_calendario.pdf     Retorno acumulado por ano-calendario (2014 a 2023)
#   F9   f9_destino.pdf                   Destino condicional de quem voltou (soma 100%)
#   F9b  f9b_destino_norm.pdf             Mudanca de setor observada vs. prevista (broadness)
#   F10  f10_retorno_24m.pdf              Taxa de retorno em 24m por coorte e grupo
#   F11  f11_km_extremos.pdf              Comparacao direta: Alta (G1) vs. Baixa (G3)
#   F12  f12_regressoes_coef.pdf          Coefplot das regressoes T12 (incluindo controle ln w0)
#   F13  f13_mecanismo_salario_duracao.pdf Gradiente Salario-Duracao por grupo (Sal. Reserva)
#   F14  f14_mecanismo_hazard_seguro.pdf  Hazard mensal por elegibilidade ao Seguro-Desemprego
#   F15  f15_mecanismo_salario_destino.pdf Variacao salarial por Grupo x Destino
# =============================================================================
source("config.R")
library(data.table)
library(ggplot2)

TINTA      <- "#1A1A1A"
COR_GRUPOS <- c("1" = "#1B365D", "2" = "#4A777A", "3" = "#C05621")
LTY_GRUPOS <- c("1" = "solid",   "2" = "dashed",  "3" = "twodash")
PAL_MAPA_5 <- c("#F1EEF6", "#BDC9E1", "#74A9CF", "#2B8CBE", "#045A8D")
PAL_QUENTE <- c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000")

tema_paper <- function(base = 10.5) {
  theme_minimal(base_size = base, base_family = "serif") +
    theme(
      text                = element_text(colour = TINTA),
      plot.title          = element_text(face = "bold", size = base + 1.5, hjust = 0),
      plot.subtitle       = element_text(size = base - 0.8, colour = "#404040",
                                         margin = margin(b = 8)),
      plot.caption        = element_text(size = base - 2, colour = "#595959", hjust = 0),
      plot.title.position = "plot",
      axis.text           = element_text(colour = TINTA, size = base - 1),
      axis.title          = element_text(colour = TINTA, size = base - 0.5),
      axis.line.x         = element_line(colour = TINTA, linewidth = 0.35),
      axis.ticks.x        = element_line(colour = TINTA, linewidth = 0.35),
      axis.ticks.length.x = unit(3, "pt"),
      panel.grid.minor    = element_blank(),
      panel.grid.major.x  = element_blank(),
      panel.grid.major.y  = element_line(colour = "#E6E6E6", linewidth = 0.3),
      strip.text          = element_text(colour = TINTA, face = "bold",
                                         size = base - 0.5, hjust = 0),
      legend.position     = "bottom",
      legend.title        = element_blank(),
      legend.key.width    = unit(22, "pt"),
      plot.margin         = margin(8, 14, 8, 8)
    )
}

salva_fig <- function(p, arq, w = 6.8, h = 4.2) {
  ggsave(file.path(DIR_FIG, arq), p, width = w, height = h)
  cat("figura:", arq, "\n")
}

broad    <- fread(file.path(DIR_DADOS, "broadness.csv"), colClasses = c(cbo = "character"))
base_cbo <- monta_base_cbo(broad, ANO_BASE, N_GRUPOS)
CORTES   <- attr(base_cbo, "cortes")
broad[, intersect(c("broadness_base", "grupo"), names(broad)) := NULL]
broad <- merge(broad, base_cbo[, .(cbo, broadness_base, grupo)], by = "cbo", all.x = TRUE)
A1 <- min(ANOS); A2 <- max(ANOS)
CRISES <- data.table(ini = c(2014.5, 2019.5), fim = c(2016.5, 2021.0))

rotulo_painel <- function(x) {
  chaves <- c(names(COORTES), "recessao", "pandemia", "total")
  rots   <- c(paste0(vapply(COORTES, function(C) as.character(C$ano), ""),
                     " — ", vapply(COORTES, function(C) C$fase, "")),
              "Recessão (2014–2016)", "Pandemia (2019–2021)", "Total acumulado")
  factor(x, chaves, rots)
}

# =============================================================================
# F1: Distribuicao do indice de portabilidade + linhas dos cortes de 2010
# =============================================================================
f1_dt <- broad[ano %in% c(ANO_BASE, 2016, A2)]
f1 <- ggplot(f1_dt, aes(broadness, weight = vinculos,
                        colour = factor(ano), linetype = factor(ano))) +
  geom_vline(xintercept = CORTES, linetype = "dotted", colour = "#595959", linewidth = 0.5) +
  geom_density(linewidth = 0.75, bw = 0.028) +
  scale_colour_manual(values = c("#1B365D", "#4A777A", "#C05621")) +
  scale_linetype_manual(values = c("solid", "dashed", "twodash")) +
  scale_x_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1),
                     labels = function(x) gsub(".", ",", sprintf("%.1f", x), fixed = TRUE)) +
  labs(title = "Distribuição da Portabilidade Ocupacional no Emprego Formal",
       subtitle = paste0("Densidade ponderada por vínculos. Linhas pontilhadas indicam os tercis de ",
                         ANO_BASE, " (", paste(gsub(".", ",", round(CORTES, 3), fixed = TRUE), collapse = " e "), ")"),
       x = "Índice de portabilidade ocupacional (broadness)", y = NULL) +
  tema_paper() + theme(axis.text.y = element_blank())
salva_fig(f1, "f1_densidade.pdf")

# =============================================================================
# F2 e F2b: Serie nacional e decomposicao Shift-Share acumulada desde 2010
# =============================================================================
s_corr <- broad[, .(valor = weighted.mean(broadness, vinculos),
                    serie = "Índice corrente (efeito total)"), by = ano]
s_fixa <- broad[, .(valor = weighted.mean(broadness_base, vinculos),
                    serie = paste0("Índice fixo em ", ANO_BASE, " (efeito composição puro)")), by = ano]
serie  <- rbind(s_corr, s_fixa)

f2 <- ggplot(serie, aes(ano, valor, colour = serie, linetype = serie)) +
  geom_rect(data = CRISES, inherit.aes = FALSE,
            aes(xmin = ini, xmax = fim, ymin = -Inf, ymax = Inf),
            fill = "#BFBFBF", alpha = 0.22) +
  geom_line(linewidth = 0.75) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = c("#1B365D", "#C05621")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  scale_x_continuous(breaks = seq(A1, A2, 2)) +
  scale_y_continuous(labels = function(x) gsub(".", ",", sprintf("%.3f", x), fixed = TRUE)) +
  labs(title = "Evolução da Portabilidade Média do Emprego Formal Privado",
       subtitle = "Áreas sombreadas: recessão de 2015–2016 e pandemia de 2020–2021",
       x = NULL, y = "Portabilidade média ponderada") +
  tema_paper()
salva_fig(f2, "f2_serie.pdf")

arq_sh <- file.path(DIR_DADOS, "broadness_shiftshare_anual.csv")
if (file.exists(arq_sh)) {
  sh_a <- fread(arq_sh)[ano >= ANO_BASE]
  sh_m <- melt(sh_a, id.vars = "ano",
               measure.vars = c("total", "entre_ocup", "dentro_ocup"),
               variable.name = "comp", value.name = "val")
  sh_m[, comp := factor(comp, c("total", "entre_ocup", "dentro_ocup"),
                        c("Variação total", "Entre ocupações (composição)", "Dentro da ocupação (dispersão)"))]
  f2b <- ggplot(sh_m, aes(ano, val, colour = comp, linetype = comp)) +
    geom_hline(yintercept = 0, colour = "#8C8C8C", linetype = "dotted", linewidth = 0.4) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 1.5) +
    scale_colour_manual(values = c("#1A1A1A", "#1B365D", "#C05621")) +
    scale_linetype_manual(values = c("solid", "dashed", "twodash")) +
    scale_x_continuous(breaks = seq(ANO_BASE, A2, 2)) +
    scale_y_continuous(labels = function(x) gsub(".", ",", sprintf("%+.4f", x), fixed = TRUE)) +
    labs(title = paste0("Decomposição Shift-Share Acumulada da Portabilidade (Base = ", ANO_BASE, ")"),
         subtitle = "Separação entre migração do emprego entre ocupações e variação setorial dentro de cada CBO",
         x = NULL, y = paste0("Variação acumulada desde ", ANO_BASE)) +
    tema_paper()
  salva_fig(f2b, "f2b_shiftshare_acumulado.pdf")
}

# =============================================================================
# F3 e F3b: Composicao dos 3 grupos fixos e distribuicao por Grande Grupo CBO
# =============================================================================
g3 <- broad[, .(vinculos = sum(vinculos)), by = .(ano, grupo)]
g3[, pct_vinc := 100 * vinculos / sum(vinculos), by = ano]
g3[, g_str := as.character(grupo)]

f3 <- ggplot(g3, aes(ano, pct_vinc, colour = g_str, linetype = g_str, group = g_str)) +
  geom_rect(data = CRISES, inherit.aes = FALSE,
            aes(xmin = ini, xmax = fim, ymin = -Inf, ymax = Inf),
            fill = "#BFBFBF", alpha = 0.20) +
  geom_hline(yintercept = 100 / N_GRUPOS, linetype = "dotted", colour = "#8C8C8C", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  geom_text(data = g3[ano == A2], aes(label = ROTULOS_CURTOS[g_str]),
            hjust = -0.12, size = 3.2, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = COR_GRUPOS, labels = ROTULOS_GRUPOS) +
  scale_linetype_manual(values = LTY_GRUPOS, labels = ROTULOS_GRUPOS) +
  scale_x_continuous(breaks = seq(A1, A2, 2), limits = c(A1, A2 + 2.3)) +
  scale_y_continuous(labels = function(x) paste0(gsub(".", ",", sprintf("%.1f", x), fixed = TRUE), "%")) +
  labs(title = "Participação dos Grupos de Portabilidade no Emprego Formal",
       subtitle = paste0("Ocupações fixadas nos 3 tercis de emprego de ", ANO_BASE,
                         " (33,3% cada em ", ANO_BASE, ")"),
       x = NULL, y = "Parcela do emprego formal privado (%)") +
  tema_paper() + theme(legend.position = "none")
salva_fig(f3, "f3_composicao.pdf")

if (file.exists(ARQ_GG)) {
  gg_dt <- fread(ARQ_GG, sep = ";", colClasses = "character", encoding = "Latin-1")
  setnames(gg_dt, 1:2, c("g1", "titulo"))
  gg_dt[, titulo := paste0(g1, " - ", trimws(titulo))]
  b_gg <- copy(broad[ano == ANO_BASE])[, g1 := substr(cbo, 1, 1)]
  b_gg <- merge(b_gg, gg_dt, by = "g1", all.x = TRUE)[!is.na(titulo)]
  res_gg <- b_gg[, {
    o <- order(broadness); x <- broadness[o]; w <- as.numeric(vinculos[o]); p <- cumsum(w) / sum(w)
    .(p25 = x[which(p >= 0.25)[1]], p50 = x[which(p >= 0.50)[1]],
      p75 = x[which(p >= 0.75)[1]], media = weighted.mean(broadness, vinculos))
  }, by = titulo][order(media)]
  res_gg[, titulo := factor(titulo, titulo)]

  f3b <- ggplot(res_gg, aes(y = titulo)) +
    geom_errorbarh(aes(xmin = p25, xmax = p75), height = 0.25, colour = "#4A777A", linewidth = 0.7) +
    geom_point(aes(x = p50, shape = "Mediana"), size = 2.6, colour = "#1B365D") +
    geom_point(aes(x = media, shape = "Média ponderada"), size = 2.6, colour = "#C05621") +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = function(x) gsub(".", ",", sprintf("%.1f", x), fixed = TRUE)) +
    labs(title = paste0("Portabilidade Ocupacional por Grande Grupo da CBO (", ANO_BASE, ")"),
         subtitle = "Média ponderada, mediana e intervalo interquartil (P25–P75) ponderados por vínculos",
         x = "Índice de portabilidade (broadness)", y = NULL) +
    tema_paper()
  salva_fig(f3b, "f3b_grandes_grupos_box.pdf", w = 7.6, h = 4.6)
}

# =============================================================================
# F4, F4b, F5 e F5b: Mapas em Quintis + Bordas Estaduais (UF) + Paineis
# =============================================================================
arq_flex <- file.path(DIR_DADOS, "flexibilidade.csv")
if (file.exists(arq_flex) &&
    requireNamespace("geobr", quietly = TRUE) &&
    requireNamespace("sf", quietly = TRUE)) {
  library(sf); library(geobr)
  flex  <- fread(arq_flex, colClasses = c(regiao = "character"))
  malha <- read_intermediate_region(year = 2020, simplified = TRUE)
  malha$regiao <- as.character(malha$code_intermediate)
  ufs   <- tryCatch(read_state(year = 2020, simplified = TRUE), error = function(e) NULL)

  corta_quintis <- function(v, dig = 1) {
    qs <- quantile(v, probs = seq(0, 1, 0.2), na.rm = TRUE)
    qs <- unique(qs)
    if (length(qs) < 3) return(factor(rep("Valor", length(v))))
    rots <- vapply(seq_len(length(qs) - 1), function(i) {
      a <- gsub(".", ",", sprintf(paste0("%.", dig, "f"), qs[i]), fixed = TRUE)
      b <- gsub(".", ",", sprintf(paste0("%.", dig, "f"), qs[i + 1]), fixed = TRUE)
      paste0(a, " a ", b)
    }, "")
    cut(v, breaks = qs, include.lowest = TRUE, labels = rots)
  }

  desenha_mapa <- function(sf_obj, col_cat, titulo, subtitulo, leg_titulo, arq,
                           paleta = PAL_MAPA_5, w = 6.5, h = 5.8) {
    p <- ggplot(sf_obj) +
      geom_sf(aes(fill = .data[[col_cat]]), colour = "white", linewidth = 0.10) +
      { if (!is.null(ufs)) geom_sf(data = ufs, fill = NA, colour = "#222222", linewidth = 0.28) } +
      scale_fill_manual(values = paleta, na.value = "#F5F5F5",
                        name = leg_titulo, drop = FALSE, na.translate = FALSE) +
      labs(title = titulo, subtitle = subtitulo) +
      theme_void(base_family = "serif", base_size = 10.5) +
      theme(
        text            = element_text(colour = TINTA),
        plot.title      = element_text(face = "bold", size = 12, hjust = 0),
        plot.subtitle   = element_text(size = 9.5, colour = "#404040", hjust = 0,
                                       margin = margin(b = 6)),
        strip.text      = element_text(face = "bold", size = 10.5, margin = margin(b = 4)),
        legend.position = "right",
        legend.title    = element_text(face = "bold", size = 9),
        legend.text     = element_text(size = 8.5),
        plot.margin     = margin(6, 6, 6, 6)
      )
    salva_fig(p, arq, w = w, h = h)
  }

  for (k in names(JANELAS)) {
    J  <- JANELAS[[k]]
    dm <- flex[nivel == "intermediaria" & t0 == J$t0 & t1 == J$t1,
               .(regiao, F_flex, T_turb, D_desc, portab_media, share_grupo1)]
    dm[, F_q := corta_quintis(F_flex, dig = 1)]
    dm[, P_q := corta_quintis(portab_media, dig = 3)]
    m_k <- merge(malha, dm, by = "regiao", all.x = TRUE)

    desenha_mapa(m_k, "F_q",
                 paste0("Flexibilidade Alocativa Regional (", J$t0, "–", J$t1, ")"),
                 "Regiões geográficas intermediárias do IBGE em quintis (fronteiras estaduais em destaque)",
                 "Flexibilidade (F)\n[Quintis]",
                 paste0("f4_mapa_flex_", k, ".pdf"))

    desenha_mapa(m_k, "P_q",
                 paste0("Portabilidade Ocupacional Média (", J$t0, ")"),
                 "Média regional ponderada pelos vínculos formais privados em quintis",
                 "Portabilidade\n[Quintis]",
                 paste0("f5_mapa_portab_", k, ".pdf"))
  }

  # Paineis comparativos lado a lado (Recessao vs. Pandemia)
  dm_all <- rbindlist(lapply(names(JANELAS), function(k) {
    J <- JANELAS[[k]]
    flex[nivel == "intermediaria" & t0 == J$t0 & t1 == J$t1,
         .(regiao, F_flex, T_turb, D_desc, portab_media, share_grupo1, janela = J$rotulo)]
  }))
  dm_all[, janela := factor(janela, vapply(JANELAS, function(J) J$rotulo, ""))]
  dm_all[, F_q_global := corta_quintis(F_flex, dig = 1)]
  dm_all[, P_q_global := corta_quintis(portab_media, dig = 3)]
  dm_all[, T_q_global := corta_quintis(T_turb, dig = 3)]
  dm_all[, D_q_global := corta_quintis(100 * D_desc, dig = 2)]
  m_painel <- merge(malha, dm_all, by = "regiao", allow.cartesian = TRUE)

  p_f4_painel <- ggplot(m_painel) +
    geom_sf(aes(fill = F_q_global), colour = "white", linewidth = 0.08) +
    { if (!is.null(ufs)) geom_sf(data = ufs, fill = NA, colour = "#222222", linewidth = 0.24) } +
    facet_wrap(~ janela, nrow = 1) +
    scale_fill_manual(values = PAL_MAPA_5, na.value = "#F5F5F5",
                      name = "Flexibilidade (F)", drop = FALSE, na.translate = FALSE) +
    labs(title = "Flexibilidade Alocativa nas Regiões Intermediárias: Recessão vs. Pandemia",
         subtitle = "Classificação unificada em quintis nas 133 regiões intermediárias do IBGE") +
    theme_void(base_family = "serif", base_size = 10.5) +
    theme(plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 9.5, colour = "#404040", margin = margin(b = 6)),
          strip.text = element_text(face = "bold", size = 10.5),
          legend.position = "bottom")
  salva_fig(p_f4_painel, "f4_mapa_flex_painel.pdf", w = 10, h = 5.4)

  p_f4b_turb <- ggplot(m_painel) +
    geom_sf(aes(fill = T_q_global), colour = "white", linewidth = 0.08) +
    { if (!is.null(ufs)) geom_sf(data = ufs, fill = NA, colour = "#222222", linewidth = 0.24) } +
    facet_wrap(~ janela, nrow = 1) +
    scale_fill_manual(values = PAL_QUENTE, na.value = "#F5F5F5",
                      name = "Turbulência (T)", drop = FALSE, na.translate = FALSE) +
    labs(title = "Turbulência Setorial (T) nas Regiões Intermediárias: Recessão vs. Pandemia",
         subtitle = "Fração do emprego formal realocada entre divisões CNAE em cada janela") +
    theme_void(base_family = "serif", base_size = 10.5) +
    theme(plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 9.5, colour = "#404040", margin = margin(b = 6)),
          strip.text = element_text(face = "bold", size = 10.5),
          legend.position = "bottom")
  salva_fig(p_f4b_turb, "f4b_mapa_T_D_painel.pdf", w = 10, h = 5.4)

  p_f5_painel <- ggplot(m_painel) +
    geom_sf(aes(fill = P_q_global), colour = "white", linewidth = 0.08) +
    { if (!is.null(ufs)) geom_sf(data = ufs, fill = NA, colour = "#222222", linewidth = 0.24) } +
    facet_wrap(~ janela, nrow = 1) +
    scale_fill_manual(values = PAL_MAPA_5, na.value = "#F5F5F5",
                      name = "Portabilidade média", drop = FALSE, na.translate = FALSE) +
    labs(title = "Portabilidade Ocupacional Regional: 2014 vs. 2019",
         subtitle = "Classificação unificada em quintis nas 133 regiões intermediárias do IBGE") +
    theme_void(base_family = "serif", base_size = 10.5) +
    theme(plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 9.5, colour = "#404040", margin = margin(b = 6)),
          strip.text = element_text(face = "bold", size = 10.5),
          legend.position = "bottom")
  salva_fig(p_f5_painel, "f5_mapa_portab_painel.pdf", w = 10, h = 5.4)

  if ("share_grupo1" %in% names(dm_all) && any(!is.na(dm_all$share_grupo1))) {
    dm_g1 <- dm_all[janela == JANELAS$recessao$rotulo]
    dm_g1[, G1_q := corta_quintis(100 * share_grupo1, dig = 1)]
    m_g1 <- merge(malha, dm_g1, by = "regiao", all.x = TRUE)
    desenha_mapa(m_g1, "G1_q",
                 "Participação de Ocupações de Alta Portabilidade (Grupo 1) por Região",
                 paste0("Percentual dos vínculos formais em ocupações do Grupo 1 (definido em ", ANO_BASE, ")"),
                 "Parcela G1 (%)\n[Quintis]",
                 "f5b_mapa_grupo1_share.pdf")
  }
} else message("geobr/sf ou flexibilidade.csv ausentes, mapas pulados")

# =============================================================================
# F6, F6b e F7: Componentes T e D no tempo, Dispersao T x D e Salario Regional
# =============================================================================
if (file.exists(arq_flex)) {
  flex <- fread(arq_flex, colClasses = c(regiao = "character"))
  td <- melt(flex[t1 == t0 + 1, .(T = mean(T_turb), D = mean(D_desc)), by = .(nivel, ano = t1)],
             id.vars = c("nivel", "ano"))
  td[, variable := factor(variable, c("T", "D"),
                          c("Turbulência Setorial (T)", "Descasamento Alocativo (D)"))]
  td[, nivel := factor(nivel, c("intermediaria", "uf"),
                       c("Região Intermediária (133)", "Unidade da Federação (27)"))]
  f6 <- ggplot(td, aes(ano, value, colour = nivel, linetype = nivel)) +
    geom_rect(data = CRISES, inherit.aes = FALSE,
              aes(xmin = ini, xmax = fim, ymin = -Inf, ymax = Inf),
              fill = "#BFBFBF", alpha = 0.20) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.4) +
    facet_wrap(~ variable, scales = "free_y", ncol = 1) +
    scale_colour_manual(values = c("#1B365D", "#C05621")) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    scale_x_continuous(breaks = seq(A1 + 1, A2, 2)) +
    labs(title = "Evolução Anual dos Componentes da Flexibilidade Alocativa",
         subtitle = "Média entre regiões para pares de anos consecutivos",
         x = NULL, y = NULL) +
    tema_paper()
  salva_fig(f6, "f6_componentes.pdf", h = 5.5)

  # F6b: Dispersao regional entre Descasamento (D) e Turbulencia (T) nas duas janelas
  td_j <- rbindlist(lapply(names(JANELAS), function(k) {
    J <- JANELAS[[k]]
    flex[nivel == "intermediaria" & t0 == J$t0 & t1 == J$t1 & D_desc > 0 & T_turb > 0,
         .(regiao, nome_inter, emprego_t0, T_turb, D_desc, F_flex, janela = J$rotulo)]
  }))
  if (nrow(td_j)) {
    f6b <- ggplot(td_j, aes(D_desc, T_turb)) +
      geom_point(aes(size = emprego_t0), shape = 21, fill = "#74A9CF",
                 colour = "#1B365D", stroke = 0.35, alpha = 0.75) +
      geom_smooth(method = "lm", se = TRUE, colour = "#C05621", fill = "#FDB863", alpha = 0.18, linewidth = 0.7) +
      facet_wrap(~ janela, scales = "free") +
      scale_x_log10() + scale_y_log10() +
      scale_size_area(max_size = 6, guide = "none") +
      labs(title = "Relação Regional entre Descasamento de Choques (D) e Turbulência Setorial (T)",
           subtitle = "Escala log-log nas 133 regiões intermediárias (círculos proporcionais ao emprego inicial)",
           x = "Descasamento alocativo (D, escala log)", y = "Turbulência setorial (T, escala log)") +
      tema_paper()
    salva_fig(f6b, "f6b_dispersao_T_D.pdf", w = 8.2, h = 4.4)
  }

  fr <- flex[nivel == "intermediaria" & t0 == JANELAS$recessao$t0 & !is.na(salario)]
  if (nrow(fr)) {
    r_cor <- cor(log(fr$salario), fr$portab_media, use = "complete.obs")
    setorder(fr, -emprego_t0)
    destaque <- head(fr, 8)
    f7 <- ggplot(fr, aes(salario, portab_media)) +
      geom_point(aes(size = emprego_t0), shape = 21, fill = "#74A9CF",
                 colour = "#1B365D", stroke = 0.35, alpha = 0.75) +
      geom_smooth(method = "lm", se = TRUE, colour = "#C05621",
                  fill = "#FDB863", alpha = 0.18, linewidth = 0.7) +
      geom_text(data = destaque, aes(label = nome_inter),
                size = 2.7, vjust = -0.8, check_overlap = TRUE, colour = TINTA) +
      scale_size_area(max_size = 7, guide = "none") +
      scale_x_log10(labels = function(x) gsub(".", ",", sprintf("%.1f", x), fixed = TRUE)) +
      scale_y_continuous(labels = function(x) gsub(".", ",", sprintf("%.3f", x), fixed = TRUE)) +
      labs(title = "Portabilidade Ocupacional e Salário Médio nas Regiões Intermediárias",
           subtitle = paste0("Ano de ", JANELAS$recessao$t0, ". Correlação linear (log salário): r = ",
                             gsub(".", ",", sprintf("%+.2f", r_cor), fixed = TRUE),
                             ". Círculos proporcionais ao emprego formal."),
           x = "Salário médio formal em salários mínimos (escala log)",
           y = "Portabilidade média da região") +
      tema_paper()
    salva_fig(f7, "f7_salario.pdf")
  }
}

# =============================================================================
# F8, F8b e F8c: Curvas de Kaplan-Meier (24m e longo prazo 84m) + Ano-Calendario
# =============================================================================
arq8 <- file.path(DIR_DADOS, "transicoes_km.csv")
if (file.exists(arq8)) {
  km_all <- fread(arq8)
  km_all[, g_str := as.character(grupo)]
  km_all[, rot_g := factor(g_str, names(ROTULOS_GRUPOS), ROTULOS_GRUPOS)]

  km24 <- km_all[coorte %in% names(COORTES) & mes <= HORIZ_COMUM]
  km24[, painel := rotulo_painel(coorte)]
  f8 <- ggplot(km24, aes(mes, voltou_ate, colour = g_str, linetype = g_str, group = g_str)) +
    geom_line(linewidth = 0.75) +
    facet_wrap(~ painel, nrow = 2) +
    scale_colour_manual(values = COR_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_linetype_manual(values = LTY_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_x_continuous(breaks = seq(0, HORIZ_COMUM, 6)) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    labs(title = "Retorno ao Emprego Formal em até 24 Meses (Kaplan-Meier)",
         subtitle = paste0("Parcela acumulada de volta ao emprego formal privado por grupo fixado em ", ANO_BASE),
         x = "Meses desde a demissão sem justa causa", y = NULL) +
    tema_paper()
  salva_fig(f8, "f8_kaplan_meier.pdf", w = 8.4, h = 5.6)

  km_longo <- km_all[coorte %in% names(COORTES) & mes <= HORIZ_LONGO]
  km_longo[, painel := rotulo_painel(coorte)]
  f8b <- ggplot(km_longo, aes(mes, voltou_ate, colour = g_str, linetype = g_str, group = g_str)) +
    geom_vline(xintercept = c(12, 24, 36, 48, 60, 72, 84),
               linetype = "dotted", colour = "#D9D9D9", linewidth = 0.35) +
    geom_line(linewidth = 0.75) +
    facet_wrap(~ painel, nrow = 2, scales = "free_x") +
    scale_colour_manual(values = COR_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_linetype_manual(values = LTY_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_x_continuous(breaks = seq(0, HORIZ_LONGO, 12)) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, 1)) +
    labs(title = "Trajetória Cumulativa de Longo Prazo do Retorno ao Emprego Formal",
         subtitle = "Acompanhamento plurianual de cada coorte de demitidos até 2023 (linhas verticais a cada 12 meses)",
         x = "Meses acumulados desde a demissão", y = NULL) +
    tema_paper()
  salva_fig(f8b, "f8b_kaplan_meier_longo.pdf", w = 8.6, h = 5.8)
}

arq_cal <- file.path(DIR_DADOS, "transicoes_calendario.csv")
if (file.exists(arq_cal)) {
  cal <- fread(arq_cal)
  cal_g <- cal[grupo > 0]
  cal_g[, g_str  := as.character(grupo)]
  cal_g[, painel := rotulo_painel(coorte)]

  f8c <- ggplot(cal_g, aes(ano_retorno, pct_acum, colour = g_str, linetype = g_str, group = g_str)) +
    geom_line(linewidth = 0.75) +
    geom_point(size = 1.4) +
    facet_wrap(~ painel, nrow = 2, scales = "free_x") +
    scale_colour_manual(values = COR_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_linetype_manual(values = LTY_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_x_continuous(breaks = 2014:2023) +
    scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
    labs(title = "Retorno Acumulado ao Emprego Formal por Ano-Calendário (2014–2023)",
         subtitle = "Parcela acumulada dos demitidos de cada coorte que já haviam voltado até dezembro de cada ano",
         x = "Ano-calendário de acompanhamento", y = NULL) +
    tema_paper() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  salva_fig(f8c, "f8c_acumulado_calendario.pdf", w = 8.8, h = 5.8)
}

# =============================================================================
# F9, F9b, F10 e F11: Destino, Normalizacao, Resumo 24m e Extremos
# =============================================================================
arq9 <- file.path(DIR_DADOS, "transicoes_composicao.csv")
if (file.exists(arq9)) {
  cp <- fread(arq9)
  if (!"pct_cond" %in% names(cp)) {
    cp[, pct_cond := fifelse(destino == "0 Nao voltou", NA_real_,
                             round(100 * N / sum(N[destino != "0 Nao voltou"]), 2)),
       by = .(coorte, grupo)]
  }
  cp_cond <- cp[coorte %in% names(COORTES) & destino != "0 Nao voltou"]
  cp_cond[, painel := rotulo_painel(coorte)]
  cp_cond[, rot_g  := factor(as.character(grupo), names(ROTULOS_CURTOS), ROTULOS_CURTOS)]
  cp_cond[, dest_rot := factor(destino,
    c("1 Mesma ocupacao, mesmo setor",
      "2 Mesma ocupacao, outro setor",
      "3 Mesma familia, outra ocupacao",
      "4 Outra ocupacao"),
    c("Mesma CBO, mesmo setor",
      "Mesma CBO, outro setor",
      "Mesma família CBO, outra ocup.",
      "Outra família ocupacional"))]

  f9 <- ggplot(cp_cond, aes(rot_g, pct_cond, fill = dest_rot)) +
    geom_col(width = 0.65, colour = "white", linewidth = 0.4) +
    facet_wrap(~ painel, nrow = 2) +
    scale_fill_manual(values = c("#1B365D", "#2B8CBE", "#74A9CF", "#D0D1E6")) +
    scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
    labs(title = "Destino Ocupacional e Setorial Condicional ao Retorno",
         subtitle = "Distribuição entre os trabalhadores reabsorvidos no mercado formal (soma 100%)",
         x = "Grupo de portabilidade da ocupação de origem", y = NULL) +
    tema_paper() + guides(fill = guide_legend(nrow = 2))
  salva_fig(f9, "f9_destino.pdf", w = 8.4, h = 5.8)
}

arq9b <- file.path(DIR_DADOS, "transicoes_destino_norm.csv")
if (file.exists(arq9b)) {
  n9 <- fread(arq9b)[coorte %in% names(COORTES)]
  n9[, painel := rotulo_painel(coorte)]
  n9[, rot_g  := factor(as.character(grupo), names(ROTULOS_CURTOS), ROTULOS_CURTOS)]
  n9_m <- melt(n9, id.vars = c("painel", "rot_g"),
               measure.vars = c("obs_outro_setor", "prev_broadness"),
               variable.name = "tipo", value.name = "pct")
  n9_m[, tipo := factor(tipo, c("obs_outro_setor", "prev_broadness"),
                        c("Mudança de setor observada (mesma CBO)",
                          "Previsto teoricamente pelo índice (broadness)"))]

  f9b <- ggplot(n9_m, aes(rot_g, pct, fill = tipo)) +
    geom_col(position = position_dodge(width = 0.68), width = 0.6, colour = "white", linewidth = 0.3) +
    facet_wrap(~ painel, nrow = 2) +
    scale_fill_manual(values = c("#1B365D", "#C05621")) +
    scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100)) +
    labs(title = "Mobilidade Intersetorial Observada vs. Prevista pelo Índice de Portabilidade",
         subtitle = "Trabalhadores que retornaram para a mesma ocupação (CBO de 6 dígitos)",
         x = "Grupo de portabilidade da ocupação de origem", y = NULL) +
    tema_paper()
  salva_fig(f9b, "f9b_destino_norm.pdf", w = 8.4, h = 5.6)
}

arq10 <- file.path(DIR_DADOS, "transicoes_duracao.csv")
if (file.exists(arq10)) {
  d10 <- fread(arq10)[coorte %in% names(COORTES)]
  d10[, g_str := as.character(grupo)]
  d10[, ordem := match(coorte, names(COORTES))]
  rot <- vapply(COORTES, function(C) as.character(C$ano), "")
  f10 <- ggplot(d10, aes(ordem, em_24m, colour = g_str, linetype = g_str, group = g_str)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.8) +
    geom_text(data = d10[ordem == max(ordem)], aes(label = ROTULOS_CURTOS[g_str]),
              hjust = -0.12, size = 3.1, fontface = "bold", show.legend = FALSE) +
    scale_colour_manual(values = COR_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_linetype_manual(values = LTY_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_x_continuous(breaks = seq_along(rot), labels = rot,
                       limits = c(1, length(rot) + 1.15)) +
    scale_y_continuous(labels = function(x) paste0(round(x), "%")) +
    labs(title = "Retorno ao Emprego Formal em até 24 Meses por Coorte de Demissão",
         subtitle = paste0("Por ano da demissão e grupo de portabilidade fixado em ", ANO_BASE),
         x = "Ano da demissão", y = NULL) +
    tema_paper() + theme(legend.position = "none")
  salva_fig(f10, "f10_retorno_24m.pdf", w = 6.8, h = 4.2)
}

if (file.exists(arq8)) {
  k2 <- fread(arq8)[coorte %in% names(COORTES) & mes <= HORIZ_COMUM & grupo %in% c(1, N_GRUPOS)]
  k2[, g_str := as.character(grupo)]
  k2[, painel := rotulo_painel(coorte)]
  f11 <- ggplot(k2, aes(mes, voltou_ate, colour = g_str, linetype = g_str, group = g_str)) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~ painel, nrow = 2) +
    scale_colour_manual(values = COR_GRUPOS[c("1", as.character(N_GRUPOS))],
                        labels = ROTULOS_GRUPOS[c("1", as.character(N_GRUPOS))]) +
    scale_linetype_manual(values = LTY_GRUPOS[c("1", as.character(N_GRUPOS))],
                          labels = ROTULOS_GRUPOS[c("1", as.character(N_GRUPOS))]) +
    scale_x_continuous(breaks = seq(0, HORIZ_COMUM, 6)) +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
    labs(title = "Velocidade de Reabsorção nos Extremos de Portabilidade",
         subtitle = "Comparação entre Alta portabilidade (G1) e Baixa portabilidade (G3)",
         x = "Meses desde a demissão", y = NULL) +
    tema_paper()
  salva_fig(f11, "f11_km_extremos.pdf", w = 8.4, h = 5.6)
}

# =============================================================================
# F12, F13, F14 e F15: Coefplot e Graficos dos Testes de Mecanismos
# =============================================================================
arq12 <- file.path(DIR_DADOS, "t12_coeficientes.csv")
if (file.exists(arq12)) {
  r12 <- fread(arq12)
  mod_list <- list(
    data.table(ano = r12$ano, modelo = "Retorno 12m (sem controles)",
               b = r12$b_12m_s, se = r12$se_12m_s),
    data.table(ano = r12$ano, modelo = "Retorno 12m (EF setor + mun.)",
               b = r12$b_12m_ef, se = r12$se_12m_ef)
  )
  if ("b_12m_w0" %in% names(r12)) {
    mod_list[[length(mod_list) + 1]] <- data.table(
      ano = r12$ano, modelo = "Retorno 12m (EF + controle log salário origem)",
      b = r12$b_12m_w0, se = r12$se_12m_w0)
  }
  if ("b_sal_ef" %in% names(r12) && any(!is.na(r12$b_sal_ef))) {
    mod_list[[length(mod_list) + 1]] <- data.table(
      ano = r12$ano, modelo = "Variação log salário (EF + log salário origem)",
      b = r12$b_sal_ef, se = r12$se_sal_ef)
  }
  cf_dt <- rbindlist(mod_list)[!is.na(b)]
  cf_dt[, `:=`(ic_inf = b - 1.96 * se, ic_sup = b + 1.96 * se)]
  cf_dt[, ano_f := factor(ano)]

  f12 <- ggplot(cf_dt, aes(ano_f, b, colour = modelo, shape = modelo)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "#404040", linewidth = 0.45) +
    geom_errorbar(aes(ymin = ic_inf, ymax = ic_sup),
                  position = position_dodge(width = 0.58), width = 0.22, linewidth = 0.55) +
    geom_point(position = position_dodge(width = 0.58), size = 2.4) +
    scale_colour_manual(values = c("#8C8C8C", "#4A777A", "#1B365D", "#C05621")) +
    scale_y_continuous(labels = function(x) gsub(".", ",", sprintf("%+.2f", x), fixed = TRUE)) +
    labs(title = "Efeito da Portabilidade Ocupacional sobre o Retorno e o Salário",
         subtitle = "Estimativas pontuais e intervalos de confiança de 95% (erros-padrão clusterizados por divisão CNAE)",
         x = "Coorte de demissão", y = "Coeficiente estimado da portabilidade") +
    tema_paper() + guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2))
  salva_fig(f12, "f12_regressoes_coef.pdf", w = 7.8, h = 4.8)
}

# F13: Gradiente Salario-Duracao (Teste do Salario de Reserva)
arq_sd <- file.path(DIR_DADOS, "transicoes_salario_duracao.csv")
if (file.exists(arq_sd)) {
  sd_dt <- fread(arq_sd)[coorte == "total" & meses <= 24]
  sd_dt[, g_str := as.character(grupo)]
  f13 <- ggplot(sd_dt, aes(meses, d_log_med, colour = g_str, linetype = g_str, group = g_str)) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "#737373", linewidth = 0.45) +
    geom_ribbon(aes(ymin = d_log_med - 1.96 * se_dlog, ymax = d_log_med + 1.96 * se_dlog,
                    fill = g_str), alpha = 0.14, colour = NA, show.legend = FALSE) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.6) +
    scale_colour_manual(values = COR_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_fill_manual(values = COR_GRUPOS) +
    scale_linetype_manual(values = LTY_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_x_continuous(breaks = seq(1, 24, 2)) +
    scale_y_continuous(labels = function(x) gsub(".", ",", sprintf("%+.3f", x), fixed = TRUE)) +
    labs(title = "Variação Salarial no Reemprego Segundo os Meses de Busca",
         subtitle = "Diferença média de log salário (novo vínculo menos vínculo perdido) com IC de 95%",
         x = "Meses até a readmissão no emprego formal",
         y = "Variação média de log salário") +
    tema_paper()
  salva_fig(f13, "f13_mecanismo_salario_duracao.pdf", w = 7.4, h = 4.6)
}

# F14: Hazard Discreto Mensal por Elegibilidade ao Seguro-Desemprego
arq_hz <- file.path(DIR_DADOS, "transicoes_hazard_tenure.csv")
if (file.exists(arq_hz)) {
  hz_dt <- fread(arq_hz)[coorte == "total" & mes <= 24]
  hz_dt[, g_str := as.character(grupo)]
  f14 <- ggplot(hz_dt, aes(mes, hazard_pct, colour = g_str, linetype = g_str, group = g_str)) +
    geom_vline(xintercept = 5.5, linetype = "dotted", colour = "#595959", linewidth = 0.5) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.4) +
    facet_wrap(~ faixa_seguro, nrow = 1) +
    scale_colour_manual(values = COR_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_linetype_manual(values = LTY_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_x_continuous(breaks = seq(0, 24, 3)) +
    scale_y_continuous(labels = function(x) paste0(gsub(".", ",", sprintf("%.1f", x), fixed = TRUE), "%")) +
    labs(title = "Taxa Mensal de Saída para o Emprego Formal (Hazard) e Seguro-Desemprego",
         subtitle = "Comparação entre demitidos sem elegibilidade (< 6m) e com 5 parcelas (>= 24m). Linha vertical no 5º/6º mês",
         x = "Mês após a demissão sem justa causa",
         y = "Probabilidade condicional de reentrada no mês (%)") +
    tema_paper()
  salva_fig(f14, "f14_mecanismo_hazard_seguro.pdf", w = 8.4, h = 4.6)
}

# F15: Variacao Salarial por Grupo x Tipo de Destino (Teste de Capital Humano Setorial)
arq_sdst <- file.path(DIR_DADOS, "transicoes_salario_destino.csv")
if (file.exists(arq_sdst)) {
  st_dt <- fread(arq_sdst)[coorte == "total" & destino != "0 Nao voltou"]
  st_dt[, rot_g := factor(as.character(grupo), names(ROTULOS_CURTOS), ROTULOS_CURTOS)]
  st_dt[, dest_rot := factor(destino,
    c("1 Mesma ocupacao, mesmo setor",
      "2 Mesma ocupacao, outro setor",
      "3 Mesma familia, outra ocupacao",
      "4 Outra ocupacao"),
    c("Mesma CBO,\nmesmo setor",
      "Mesma CBO,\noutro setor",
      "Mesma família CBO,\noutra ocupação",
      "Outra família\nocupacional"))]

  f15 <- ggplot(st_dt, aes(dest_rot, d_log_med, fill = as.character(grupo))) +
    geom_hline(yintercept = 0, linetype = "solid", colour = "#404040", linewidth = 0.4) +
    geom_col(position = position_dodge(width = 0.72), width = 0.65, colour = "white", linewidth = 0.3) +
    geom_errorbar(aes(ymin = d_log_med - 1.96 * se_dlog, ymax = d_log_med + 1.96 * se_dlog),
                  position = position_dodge(width = 0.72), width = 0.22, linewidth = 0.45, colour = TINTA) +
    scale_fill_manual(values = COR_GRUPOS, labels = ROTULOS_GRUPOS) +
    scale_y_continuous(labels = function(x) gsub(".", ",", sprintf("%+.3f", x), fixed = TRUE)) +
    labs(title = "Variação Salarial no Reemprego por Grupo de Portabilidade e Destino",
         subtitle = "Média de ln(w1) - ln(w0) entre reentrantes (painel acumulado com IC de 95%)",
         x = NULL, y = "Variação média de log salário") +
    tema_paper()
  salva_fig(f15, "f15_mecanismo_salario_destino.pdf", w = 7.6, h = 4.6)
}


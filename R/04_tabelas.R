# =============================================================================
# 04_tabelas.R  ->  fragmentos .tex em saidas/tabelas (Corpo + Mecanismos + Anexo)
#
# TABELAS DO CORPO DO ARTIGO:
#   T1   t1_amostra.tex                  Amostra anual da RAIS (2009-2023)
#   T2   t2_extremos.tex                 Ocupacoes mais e menos portateis
#   T3   t3_grandes_grupos.tex           Portabilidade por grande grupo da CBO
#   T4   t4_composicao_grupos.tex        Composicao do emprego nos 3 grupos fixos (2010)
#   T4b  t4b_ocupacoes_grupos.tex        Definicao e maiores ocupacoes dos 3 grupos
#   T5   t5_shiftshare.tex               Decomposicao shift-share por subperiodos
#   T6   t6_flexibilidade_<jan>.tex      Regioes intermediarias mais e menos flexiveis
#   T7   t7_correlacoes.tex              Correlacoes entre as medidas regionais
#   T8   t8_coortes.tex                  Construcao das coortes de demitidos
#   T9   t9_perfil.tex                   Perfil dos demitidos por grupo (3 grupos)
#   T10  t10_duracao.tex                 Retorno (6m, 12m, 24m, 36m, 48m, salario)
#   T10b t10b_retorno_24m.tex            Resumo de retorno em 24m e diferencial G1-G3
#   T10c t10c_acumulado_anos.tex         Retorno cumulativo ano a ano (2014 a 2023)
#   T10d t10d_acumulado_anos_grupos.tex  Retorno cumulativo ano a ano por grupo
#   T11  t11_destino_painel.tex          Destino condicional de quem voltou (painel)
#   T11b t11b_destino_norm.tex           Mudanca de setor observada vs. prevista
#   T12  t12_regressoes_resumo.tex       Painel consolidado (MPL 12m, MPL 12m c/ EF,
#                                        MPL 12m c/ EF + log w0, MPL 24m, Cox 500k, Salario)
#
# TABELAS DE TESTES DE MECANISMOS E ANEXO DESCRITIVO:
#   T13  t13_mecanismo_setor_salario.tex Penalidade salarial ao mudar de setor x portabilidade
#   T14  t14_mecanismo_reserva_seguro.tex Salario-duracao e subamostras do seguro-desemprego
#   T15  t15_mecanismo_choque_regional.tex Interacao portabilidade x choque setorial local
#   TA1  ta1_distribuicao_percentis.tex  Media, DP e percentis P10-P90 da portabilidade por ano
#   TA2  ta2_uf_resumo.tex               Resumo completo das 27 UFs (T, D, F, portab., salario)
#   TA3  ta3_salario_por_destino.tex     Variacao salarial e duracao por Grupo x Destino
# =============================================================================
source("config.R")
library(data.table)

le_cbo <- function(arq) {
  x <- fread(arq, sep = ";", colClasses = "character", encoding = "Latin-1")
  setnames(x, 1:2, c("cod", "titulo"))
  x[, titulo := trimws(titulo)][]
}

broad    <- fread(file.path(DIR_DADOS, "broadness.csv"), colClasses = c(cbo = "character"))
base_cbo <- monta_base_cbo(broad, ANO_BASE, N_GRUPOS)
broad[, intersect(c("broadness_base", "grupo"), names(broad)) := NULL]
broad <- merge(broad, base_cbo[, .(cbo, broadness_base, grupo)], by = "cbo", all.x = TRUE)
A1 <- min(ANOS); A2 <- max(ANOS)

fmt <- function(x, d = 3, sinal = FALSE) {
  xn <- as.numeric(x)
  mascara <- paste0(if (sinal) "%+." else "%.", d, "f")
  ifelse(is.na(xn) | is.nan(xn) | is.infinite(xn), "---",
         gsub(".", ",", sprintf(mascara, xn), fixed = TRUE))
}
mil <- function(x) {
  xn <- as.numeric(x)
  ifelse(is.na(xn) | is.nan(xn) | is.infinite(xn), "---",
         gsub("(?<=\\d)(?=(\\d{3})+$)", ".",
              format(round(xn), scientific = FALSE, trim = TRUE), perl = TRUE))
}
esc <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- "---"
  y <- gsub("&", "\\\\&", y)
  y <- gsub("%", "\\\\%", y)
  y <- gsub("_", "\\\\_", y)
  y
}

tex <- function(dt, arq, alinha, notas = NULL, grupo_col = NULL, cab_extra = NULL) {
  nms <- esc(names(dt))
  linhas <- character(0)
  prev_g <- NULL
  for (i in seq_len(nrow(dt))) {
    r <- unlist(lapply(dt[i], as.character), use.names = FALSE)
    if (!is.null(grupo_col) && i > 1) {
      g_atual <- as.character(dt[[grupo_col]][i])
      if (!is.na(g_atual) && !is.na(prev_g) && g_atual != prev_g) {
        linhas <- c(linhas, "\\addlinespace[3pt]")
      }
      prev_g <- g_atual
    } else if (!is.null(grupo_col) && i == 1) {
      prev_g <- as.character(dt[[grupo_col]][1])
    }
    linhas <- c(linhas, paste0(paste(esc(r), collapse = " & "), " \\\\"))
  }

  out <- c(
    "\\providecommand{\\toprule}{\\hline\\hline}",
    "\\providecommand{\\midrule}{\\hline}",
    "\\providecommand{\\bottomrule}{\\hline\\hline}",
    "\\providecommand{\\addlinespace}[1][]{}",
    "{\\small",
    "\\renewcommand{\\arraystretch}{1.12}",
    paste0("\\begin{tabular}{", alinha, "}"),
    "\\toprule",
    if (!is.null(cab_extra)) cab_extra else NULL,
    paste0(paste(nms, collapse = " & "), " \\\\"),
    "\\midrule",
    linhas,
    "\\bottomrule",
    "\\end{tabular}"
  )
  if (!is.null(notas)) {
    out <- c(out, "\\vspace{2pt}", paste0("\\par{\\footnotesize \\textit{Nota:} ", esc(notas), "}"))
  }
  out <- c(out, "}")
  writeLines(out, file.path(DIR_TAB, arq))
  cat("tabela:", arq, "\n")
}

# =============================================================================
# T1 e TA1: Amostra anual da RAIS e distribuicao de percentis (2009-2023)
# =============================================================================
t1 <- broad[, .(
  Ano                   = ano,
  `Vínculos privados`   = mil(sum(vinculos)),
  `Ocupações (CBO)`     = mil(.N),
  `Setores (média)`     = fmt(mean(n_setores), 1),
  `Portabilidade (ano)` = fmt(weighted.mean(broadness, vinculos), 3),
  `Portab. (fixa 2010)` = fmt(weighted.mean(broadness_base, vinculos), 3)
), by = ano][, ano := NULL]
tex(t1, "t1_amostra.tex", "lrrrrr",
    paste0("Vínculos formais ativos em 31/12 no setor privado (natureza jurídica > 2020). ",
           "A coluna 'Portabilidade (ano)' usa o índice calculado em cada ano; ",
           "'Portab. (fixa ", ANO_BASE, ")' mantém o índice de cada CBO congelado em ", ANO_BASE,
           ", isolando a mudança na composição ocupacional do emprego."))

arq_perc <- file.path(DIR_DADOS, "broadness_percentis.csv")
if (file.exists(arq_perc)) {
  pa <- fread(arq_perc)
  ta1 <- pa[, .(
    Ano          = ano,
    Vínculos     = mil(vinculos),
    Média        = fmt(media_pond, 3),
    `Desv. Pad.` = fmt(dp_pond, 3),
    P10          = fmt(p10, 3),
    P25          = fmt(p25, 3),
    Mediana      = fmt(p50, 3),
    P75          = fmt(p75, 3),
    P90          = fmt(p90, 3)
  )]
  tex(ta1, "ta1_distribuicao_percentis.tex", "lrrrrrrrr",
      "Estatísticas descritivas e percentis ponderados pelos vínculos formais privados ativos em 31/12.")
}

# =============================================================================
# T2: Extremos de portabilidade (ocupacoes com pelo menos 5.000 vinculos)
# =============================================================================
if (file.exists(ARQ_CBO)) {
  nmc <- le_cbo(ARQ_CBO)
  b2  <- merge(broad[ano == A2 & vinculos >= 5000], nmc[, .(cbo = cod, titulo)],
               by = "cbo", all.x = TRUE)
  setorder(b2, -broadness)
  cl <- function(x, g) x[, .(
    Extremo  = g,
    CBO      = cbo,
    Ocupação = titulo,
    Vínculos = mil(vinculos),
    Setores  = n_setores,
    Índice   = fmt(broadness, 3)
  )]
  tex(rbind(cl(head(b2, 10), "Mais portáteis"), cl(tail(b2, 10), "Menos portáteis")),
      "t2_extremos.tex", "llp{5.8cm}rrr",
      paste0("Ocupações com pelo menos 5.000 vínculos formais ativos em ", A2, "."),
      grupo_col = "Extremo")
}

# =============================================================================
# T3: Portabilidade por Grande Grupo da CBO
# =============================================================================
if (file.exists(ARQ_GG)) {
  gg <- le_cbo(ARQ_GG)
  broad[, g1 := substr(cbo, 1, 1)]
  t3 <- dcast(broad[ano %in% c(ANO_BASE, A2), .(
    m = weighted.mean(broadness, vinculos),
    v = sum(vinculos)
  ), by = .(g1, ano)], g1 ~ ano, value.var = c("m", "v"))
  t3 <- merge(t3, gg[, .(g1 = cod, titulo)], by = "g1", all.x = TRUE)
  setcolorder(t3, c("g1", "titulo"))
  t3[, Variacao := fmt(get(paste0("m_", A2)) - get(paste0("m_", ANO_BASE)), 3, sinal = TRUE)]
  for (j in grep("^m_", names(t3), value = TRUE)) t3[, (j) := fmt(get(j), 3)]
  for (j in grep("^v_", names(t3), value = TRUE)) t3[, (j) := mil(get(j))]
  setnames(t3, c("Cód.", "Grande grupo da CBO",
                 paste0("Índice ", ANO_BASE), paste0("Índice ", A2),
                 paste0("Vínculos ", ANO_BASE), paste0("Vínculos ", A2), "Dif."))
  tex(t3, "t3_grandes_grupos.tex", "lp{5.2cm}rrrrr",
      "Média ponderada pelos vínculos formais ativos em 31/12 de cada grande grupo.")
}

# =============================================================================
# T4 e T4b: Composicao do emprego nos 3 grupos fixos (2010) e maiores ocupacoes
# =============================================================================
comp_fix <- broad[, .(
  ocupacoes = .N,
  vinculos  = sum(vinculos)
), by = .(ano, grupo)]
comp_fix[, pct_vinc := 100 * vinculos / sum(vinculos), by = ano]

t4 <- dcast(comp_fix, ano ~ grupo, value.var = "pct_vinc")
t4[, Dif_G1_G3 := get("1") - get(as.character(N_GRUPOS))]
for (j in names(t4)[-1]) {
  sinal_j <- (j == "Dif_G1_G3")
  t4[, (j) := fmt(get(j), 1, sinal = sinal_j)]
}
nomes_g_cols <- c("G1: Alta (%)", "G2: Média (%)", "G3: Baixa (%)")[seq_len(N_GRUPOS)]
setnames(t4, c("Ano", nomes_g_cols, "Dif. G1 - G3 (p.p.)"))
tex(t4, "t4_composicao_grupos.tex", paste0("l", strrep("r", N_GRUPOS + 1)),
    paste0("Participação de cada grupo no emprego formal privado (%). A classificação de cada ",
           "ocupação (CBO) é fixada nos tercis de emprego de ", ANO_BASE,
           " (33,3% em cada grupo em ", ANO_BASE, "), de modo que a variação no tempo reflete ",
           "exclusivamente o crescimento relativo das ocupações de cada grupo."))

if (file.exists(ARQ_CBO)) {
  nmc <- le_cbo(ARQ_CBO)
  b_ref <- merge(copy(base_cbo), nmc[, .(cbo = cod, titulo)], by = "cbo", all.x = TRUE)
  setorder(b_ref, grupo, -vinculos_base)
  top_cbo <- b_ref[, head(.SD, 5), by = grupo][, .(
    Grupo            = ROTULOS_CURTOS[as.character(grupo)],
    CBO              = cbo,
    `Ocupação (CBO)` = titulo,
    `Vínculos 2010`  = mil(vinculos_base),
    `Setores 2010`   = n_setores_base,
    `Índice 2010`    = fmt(broadness_base, 3)
  )]
  tex(top_cbo, "t4b_ocupacoes_grupos.tex", "llp{5.5cm}rrr",
      paste0("As 5 maiores ocupações de cada um dos ", N_GRUPOS,
             " grupos de portabilidade definidos no ano-base (", ANO_BASE, ")."),
      grupo_col = "Grupo")
}

# =============================================================================
# T5: Decomposicao shift-share (subperiodos e acumulada)
# =============================================================================
calc_shiftshare <- function(y0, y1, rot) {
  sh <- broad[ano %in% c(y0, y1)]
  sh[, s := vinculos / sum(vinculos), by = ano]
  w <- dcast(sh, cbo ~ ano, value.var = c("s", "broadness"), fill = 0)
  s0 <- w[[paste0("s_", y0)]];         s1 <- w[[paste0("s_", y1)]]
  b0 <- w[[paste0("broadness_", y0)]]; b1 <- w[[paste0("broadness_", y1)]]
  e  <- sum((s1 - s0) * b0)
  dd <- sum(s0 * (b1 - b0))
  cx <- sum((s1 - s0) * (b1 - b0))
  tt <- e + dd + cx
  data.table(
    Período              = rot,
    `Entre ocupações`    = fmt(e, 4, sinal = TRUE),
    `Dentro da ocupação` = fmt(dd, 4, sinal = TRUE),
    `Termo cruzado`      = fmt(cx, 4, sinal = TRUE),
    `Variação total`     = fmt(tt, 4, sinal = TRUE),
    `% Entre ocup.`      = fmt(100 * e / tt, 1)
  )
}
t5 <- rbindlist(list(
  calc_shiftshare(ANO_BASE, 2014, paste0(ANO_BASE, "–2014 (Pré-recessão)")),
  calc_shiftshare(2014, 2019, "2014–2019 (Recessão e pós)"),
  calc_shiftshare(2019, A2, paste0("2019–", A2, " (Pandemia e pós)")),
  calc_shiftshare(ANO_BASE, A2, paste0(ANO_BASE, "–", A2, " (Desde o ano-corte)")),
  calc_shiftshare(A1, A2, paste0(A1, "–", A2, " (Série completa)"))
))
tex(t5, "t5_shiftshare.tex", "lrrrrr",
    paste0("Decomposição shift-share da variação da portabilidade média ponderada. ",
           "'Entre ocupações' mede a migração do emprego entre CBOs mantendo o índice inicial; ",
           "'Dentro da ocupação' mede a variação da dispersão setorial de cada CBO."))

# =============================================================================
# T6, T7 e TA2: Flexibilidade regional, correlacoes e tabela completa por UF
# =============================================================================
arq_flex <- file.path(DIR_DADOS, "flexibilidade.csv")
if (file.exists(arq_flex)) {
  flex <- fread(arq_flex, colClasses = c(regiao = "character"))
  for (k in names(JANELAS)) {
    J <- JANELAS[[k]]
    f <- flex[nivel == "intermediaria" & t0 == J$t0 & t1 == J$t1][order(-F_flex)]
    cf <- function(x, g) x[, .(
      Extremo             = g,
      Região              = nome_inter,
      UF                  = nome_uf,
      `Emprego inicial`   = mil(emprego_t0),
      `Turbulência (T)`   = fmt(T_turb, 4),
      `Descasamento (D)`  = fmt(D_desc, 5),
      `Flexibilidade (F)` = fmt(F_flex, 1),
      Portabilidade       = fmt(portab_media, 3)
    )]
    tex(rbind(cf(head(f, 10), "Mais flexíveis"), cf(tail(f, 10), "Menos flexíveis")),
        paste0("t6_flexibilidade_", k, ".tex"), "lllcrrrrr",
        paste0("Regiões geográficas intermediárias do IBGE, janela de ", J$t0, " a ", J$t1, "."),
        grupo_col = "Extremo")
  }

  t7 <- rbindlist(lapply(names(JANELAS), function(k) {
    J <- JANELAS[[k]]
    f <- flex[nivel == "intermediaria" & t0 == J$t0 & t1 == J$t1]
    data.table(
      Janela                    = J$rotulo,
      Regiões                   = nrow(f),
      `Corr(F, Portabilidade)`  = fmt(cor(f$F_flex, f$portab_media, use = "complete.obs"), 3),
      `Corr(F, log Salário)`    = fmt(cor(f$F_flex, log(f$salario), use = "complete.obs"), 3),
      `Corr(Portab., log Sal.)` = fmt(cor(f$portab_media, log(f$salario), use = "complete.obs"), 3),
      `Corr(T, D)`              = fmt(cor(f$T_turb, f$D_desc, use = "complete.obs"), 3)
    )
  }))
  tex(t7, "t7_correlacoes.tex", "lrrrrr",
      "Correlações lineares de Pearson entre as 133 regiões geográficas intermediárias do IBGE.")

  # TA2: Resumo das 27 Unidades da Federacao nas duas janelas (Anexo)
  uf_rec <- flex[nivel == "uf" & t0 == JANELAS$recessao$t0 & t1 == JANELAS$recessao$t1,
                 .(regiao, nome_uf, emp_2014 = emprego_t0, sal_2014 = salario,
                   port_2014 = portab_media, T_rec = T_turb, D_rec = D_desc, F_rec = F_flex)]
  uf_pan <- flex[nivel == "uf" & t0 == JANELAS$pandemia$t0 & t1 == JANELAS$pandemia$t1,
                 .(regiao, T_pan = T_turb, D_pan = D_desc, F_pan = F_flex)]
  if (nrow(uf_rec) && nrow(uf_pan)) {
    ta2 <- merge(uf_rec, uf_pan, by = "regiao", all.x = TRUE)[order(-F_rec)]
    tex(ta2[, .(
      UF                = nome_uf,
      `Vínculos (2014)` = mil(emp_2014),
      `Salário (SM)`    = fmt(sal_2014, 2),
      Portabilidade     = fmt(port_2014, 3),
      `T (14–19)`       = fmt(T_rec, 3),
      `F (14–19)`       = fmt(F_rec, 1),
      `T (19–22)`       = fmt(T_pan, 3),
      `F (19–22)`       = fmt(F_pan, 1)
    )], "ta2_uf_resumo.tex", "lrrrrrrr",
    "Indicadores de portabilidade ocupacional, turbulência setorial (T) e flexibilidade alocativa (F) por Unidade da Federação.")
  }
}

# =============================================================================
# T8, T9, T10, T10b, T10c, T10d: Coortes, Perfil e Duracao
# =============================================================================
arq8 <- file.path(DIR_DADOS, "transicoes_amostra.csv")
if (file.exists(arq8)) {
  a8 <- fread(arq8)
  a8[, pct_ret := 100 * sem_outro_emprego / uma_demissao]
  tex(a8[, .(
    Coorte                          = ano,
    Período                         = fase,
    `Trabalhadores no ano`          = mil(pessoas),
    `Uma demissão s/ justa causa`   = mil(uma_demissao),
    `Coorte final (s/ outro vínc.)` = mil(sem_outro_emprego),
    `Retenção (%)`                  = fmt(pct_ret, 1),
    `Horizonte (meses)`             = horizonte_meses
  )], "t8_coortes.tex", "llrrrrr",
  paste0("Coorte formada por trabalhadores do setor privado com uma única rescisão sem justa ",
         "causa por iniciativa do empregador (causadesli = 11) no ano e sem nenhum outro vínculo ",
         "formal ativo no mês do desligamento. Acompanhamento até dezembro de ", max(ANOS), "."))
}

arq9 <- file.path(DIR_DADOS, "transicoes_perfil.csv")
if (file.exists(arq9)) {
  p9 <- fread(arq9)
  p9[, rot_coorte := fifelse(coorte %in% names(COORTES), sub("^c", "", coorte), fase)]
  p9[, rot_grupo  := ROTULOS_CURTOS[as.character(grupo)]]
  tex(p9[, .(
    Coorte                  = rot_coorte,
    Grupo                   = rot_grupo,
    Demitidos               = mil(pessoas),
    `Idade (anos)`          = fmt(idade_media, 1),
    `Mulheres (%)`          = fmt(pct_mulher, 1),
    `Médio compl. ou + (%)` = fmt(pct_medio_ou_mais, 1),
    `Tenure (meses)`        = fmt(tempo_medio_meses, 1),
    `Salário origem (SM)`   = fmt(salario_medio_sm, 2)
  )], "t9_perfil.tex", "llrrrrrr",
  paste0("Características médias na ocupação de origem por grupo de portabilidade fixado em ",
         ANO_BASE, " (1 = Alta, 2 = Média, 3 = Baixa)."),
  grupo_col = "Coorte")
}

arq10 <- file.path(DIR_DADOS, "transicoes_duracao.csv")
if (file.exists(arq10)) {
  d10 <- fread(arq10)
  if (!"em_36m" %in% names(d10)) d10[, `:=`(em_36m = NA_real_, em_48m = NA_real_, em_60m = NA_real_)]
  d10[, rot_coorte := fifelse(coorte %in% names(COORTES), sub("^c", "", coorte), fase)]
  d10[, rot_grupo  := ROTULOS_CURTOS[as.character(grupo)]]

  tex(d10[, .(
    Coorte               = rot_coorte,
    Grupo                = rot_grupo,
    Demitidos            = mil(pessoas),
    `6m (%)`             = fmt(em_6m, 1),
    `12m (%)`            = fmt(em_12m, 1),
    `24m (%)`            = fmt(em_24m, 1),
    `36m (%)`            = fmt(em_36m, 1),
    `48m (%)`            = fmt(em_48m, 1),
    `Voltou total (%)`   = fmt(pct_voltou, 1),
    `Mediana (m)`        = fmt(mediana_mes, 0),
    `Migrou mun. (%)`    = fmt(pct_migrou, 1),
    `$\\Delta$ log sal.` = fmt(d_log_rem, 3, sinal = TRUE)
  )], "t10_duracao.tex", "llrrrrrrrrrr",
  paste0("Parcela acumulada de trabalhadores que retornaram ao emprego formal privado em até ",
         "6, 12, 24, 36 e 48 meses após a demissão e variação média de log salário dos reentrantes."),
  grupo_col = "Coorte")

  w24 <- dcast(d10, coorte + rot_coorte + fase ~ grupo, value.var = "em_24m")
  ord_c <- c(names(COORTES), "recessao", "pandemia", "total")
  w24[, ord := match(coorte, ord_c)]
  setorder(w24, ord)
  cols_g <- intersect(as.character(seq_len(N_GRUPOS)), names(w24))
  g_last <- tail(cols_g, 1)
  w24[, dif := get("1") - get(g_last)]
  for (j in cols_g) w24[, (j) := fmt(get(j), 1)]
  w24[, dif := fmt(dif, 1, sinal = TRUE)]
  t10b <- w24[, c("rot_coorte", "fase", cols_g, "dif"), with = FALSE]
  nomes_g_cols <- c("G1: Alta (%)", "G2: Média (%)", "G3: Baixa (%)")[seq_along(cols_g)]
  setnames(t10b, c("Coorte", "Período", nomes_g_cols, "Dif. G1 - G3 (p.p.)"))
  tex(t10b, "t10b_retorno_24m.tex", paste0("ll", strrep("r", length(cols_g) + 1)),
      paste0("Parcela de cada grupo de volta ao emprego formal privado em até ",
             HORIZ_COMUM, " meses após a demissão."))
}

arq_cal <- file.path(DIR_DADOS, "transicoes_calendario.csv")
if (file.exists(arq_cal)) {
  cal <- fread(arq_cal)
  w_cal <- dcast(cal[grupo == 0], ano_demissao + fase ~ ano_retorno, value.var = "pct_acum")
  anos_cols <- setdiff(names(w_cal), c("ano_demissao", "fase"))
  for (j in anos_cols) w_cal[, (j) := fmt(get(j), 1)]
  setnames(w_cal, c("Coorte", "Período", anos_cols))
  tex(w_cal, "t10c_acumulado_anos.tex", paste0("ll", strrep("r", length(anos_cols))),
      "Parcela acumulada (%) dos demitidos de cada coorte que já haviam retornado até dezembro de cada ano.")

  w_cal_g <- dcast(cal[grupo > 0], ano_demissao + grupo ~ ano_retorno, value.var = "pct_acum")
  w_cal_g[, Grupo := ROTULOS_CURTOS[as.character(grupo)]][, grupo := NULL]
  setcolorder(w_cal_g, c("ano_demissao", "Grupo", anos_cols))
  for (j in anos_cols) w_cal_g[, (j) := fmt(get(j), 1)]
  setnames(w_cal_g, c("Coorte", "Grupo", anos_cols))
  tex(w_cal_g, "t10d_acumulado_anos_grupos.tex", paste0("ll", strrep("r", length(anos_cols))),
      "Parcela acumulada (%) de retorno até dezembro de cada ano-calendário, aberta por coorte e grupo.",
      grupo_col = "Coorte")
}

# =============================================================================
# T11, T11b e TA3: Destino condicional, normalizacao e salario por destino
# =============================================================================
arq11 <- file.path(DIR_DADOS, "transicoes_composicao.csv")
if (file.exists(arq11)) {
  d11 <- fread(arq11)
  if (!"pct_cond" %in% names(d11)) {
    d11[, pct_cond := fifelse(destino == "0 Nao voltou", NA_real_,
                              round(100 * N / sum(N[destino != "0 Nao voltou"]), 2)),
        by = .(coorte, grupo)]
  }
  for (k in unique(d11$coorte)) {
    w11 <- dcast(d11[coorte == k & destino != "0 Nao voltou"],
                 grupo ~ destino, value.var = "pct_cond")
    w11[, Grupo := ROTULOS_CURTOS[as.character(grupo)]][, grupo := NULL]
    setcolorder(w11, "Grupo")
    for (j in names(w11)[-1]) w11[, (j) := fmt(get(j), 1)]
    setnames(w11, c("Grupo", "Mesma CBO, mesmo setor", "Mesma CBO, outro setor",
                    "Mesma família, outra CBO", "Outra família CBO")[seq_len(ncol(w11))])
    tex(w11, paste0("t11_destino_", k, ".tex"), paste0("l", strrep("r", ncol(w11) - 1)),
        "Distribuição percentual dos trabalhadores que retornaram ao emprego formal (linhas somam 100%).")
  }

  wp <- dcast(d11[destino != "0 Nao voltou"], coorte + fase + grupo ~ destino, value.var = "pct_cond")
  ord_c <- c(names(COORTES), "recessao", "pandemia", "total")
  wp[, ord := match(coorte, ord_c)]
  setorder(wp, ord, grupo)
  wp[, Coorte := fifelse(coorte %in% names(COORTES), sub("^c", "", coorte), fase)]
  wp[, Grupo  := ROTULOS_CURTOS[as.character(grupo)]]
  cols_dest <- grep("^[1-4] ", names(wp), value = TRUE)
  for (j in cols_dest) wp[, (j) := fmt(get(j), 1)]
  wp_out <- wp[, c("Coorte", "Grupo", cols_dest), with = FALSE]
  setnames(wp_out, c("Coorte", "Grupo",
                     "Mesma CBO, mesmo setor (%)",
                     "Mesma CBO, outro setor (%)",
                     "Mesma família CBO (%)",
                     "Outra ocupação (%)")[seq_len(ncol(wp_out))])
  tex(wp_out, "t11_destino_painel.tex", paste0("ll", strrep("r", length(cols_dest))),
      "Destino ocupacional e setorial condicional a ter voltado ao emprego formal (linhas somam 100%).",
      grupo_col = "Coorte")
}

arq11b <- file.path(DIR_DADOS, "transicoes_destino_norm.csv")
if (file.exists(arq11b)) {
  n11 <- fread(arq11b)
  ord_c <- c(names(COORTES), "recessao", "pandemia", "total")
  n11[, ord := match(coorte, ord_c)]
  setorder(n11, ord, grupo)
  n11[, Coorte := fifelse(coorte %in% names(COORTES), sub("^c", "", coorte), fase)]
  n11[, Grupo  := ROTULOS_CURTOS[as.character(grupo)]]
  t11b <- n11[, .(
    Coorte                       = Coorte,
    Grupo                        = Grupo,
    `Retornou à mesma CBO (%)`   = fmt(pct_mesma_cbo, 1),
    `Mudou setor — Obs. (%)`     = fmt(obs_outro_setor, 1),
    `Mudou setor — Previsto (%)` = fmt(prev_broadness, 1),
    `Razão Obs./Prev.`           = fmt(razao_obs_prev, 2)
  )]
  tex(t11b, "t11b_destino_norm.tex", "llrrrr",
      "Mudança de divisão CNAE observada vs. probabilidade teórica prevista pela broadness entre os que retornaram à mesma CBO.",
      grupo_col = "Coorte")
}

arq_sdst <- file.path(DIR_DADOS, "transicoes_salario_destino.csv")
if (file.exists(arq_sdst)) {
  sdst <- fread(arq_sdst)[coorte == "total"]
  sdst[, Grupo := ROTULOS_CURTOS[as.character(grupo)]]
  sdst[, Destino := sub("^[0-9] ", "", destino)]
  ta3 <- sdst[order(grupo, destino), .(
    Grupo                  = Grupo,
    `Destino no reemprego` = Destino,
    Reentrantes            = mil(n_obs),
    `Mediana duração (m)`  = fmt(meses_med, 1),
    `$\\Delta$ log sal.`   = fmt(d_log_med, 3, sinal = TRUE),
    `EP ($\\Delta$ log)`   = fmt(se_dlog, 4)
  )]
  tex(ta3, "ta3_salario_por_destino.tex", "llrrrr",
      "Duração mediana até o retorno e variação salarial média por grupo de portabilidade e tipo de transição (painel acumulado).",
      grupo_col = "Grupo")
}

# =============================================================================
# T12, T13, T14 e T15: Regressoes Principais e Testes de Mecanismos
# =============================================================================
N_COX   <- 500000
SEMENTE <- 1

if (requireNamespace("fixest", quietly = TRUE) && requireNamespace("survival", quietly = TRUE)) {
  library(fixest); library(survival)
  res_reg <- list()
  res_t13 <- list()
  res_t14 <- list()
  res_t15 <- list()

  est_str <- function(b, se) {
    if (is.na(b) || is.na(se) || se == 0) return("---")
    p <- 2 * pnorm(-abs(b / se))
    st <- ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))
    paste0(fmt(b, 4, sinal = TRUE), st, " (", fmt(se, 4), ")")
  }

  for (k in names(COORTES)) {
    arq <- file.path(DIR_DADOS, paste0("duracao_", k, ".csv"))
    if (!file.exists(arq)) next
    dd <- fread(arq, colClasses = c(div = "character", mun = "character"))

    if (!"log_rem0" %in% names(dd))
      dd[, log_rem0 := fifelse(!is.na(rem) & rem > 0, log(rem), NA_real_)]
    if (!"d_log_rem" %in% names(dd) && all(c("rem", "rem1") %in% names(dd)))
      dd[, d_log_rem := fifelse(voltou == 1 & rem > 0 & rem1 > 0, log(rem1) - log(rem), NA_real_)]
    if (!"mudou_setor" %in% names(dd))
      dd[, mudou_setor := fifelse(voltou == 1L, as.integer(grepl("outro setor|Outra|familia", destino)), NA_integer_)]

    # --- T12: Especificacoes principais (incluindo controle por log_rem0!) ----
    m1 <- feols(I(voltou == 1 & meses <= 12) ~ broadness, dd, cluster = ~div)
    m2 <- feols(I(voltou == 1 & meses <= 12) ~ broadness + idade + tempo +
                  factor(sexo) + factor(grauinstr) | div + mun, dd, cluster = ~div)
    m2w <- feols(I(voltou == 1 & meses <= 12) ~ broadness + log_rem0 + idade + tempo +
                   factor(sexo) + factor(grauinstr) | div + mun,
                 dd[!is.na(log_rem0)], cluster = ~div)
    m3 <- feols(I(voltou == 1 & meses <= 24) ~ broadness + log_rem0 + idade + tempo +
                  factor(sexo) + factor(grauinstr) | div + mun,
                dd[!is.na(log_rem0)], cluster = ~div)
    m4 <- if (sum(!is.na(dd$d_log_rem)) > 100) {
      feols(d_log_rem ~ broadness + log_rem0 + idade + tempo +
              factor(sexo) + factor(grauinstr) | div + mun,
            dd[!is.na(d_log_rem)], cluster = ~div)
    } else NULL

    modelos_et <- if (!is.null(m4)) list(m1, m2, m2w, m3, m4) else list(m1, m2, m2w, m3)
    etable(modelos_et,
           file = file.path(DIR_TAB, paste0("t12_binaria_", k, ".tex")),
           replace = TRUE, tex = TRUE,
           dict = c(broadness = "Portabilidade (2010)",
                    log_rem0  = "Log salário origem",
                    idade     = "Idade",
                    tempo     = "Tempo no emprego (meses)"))

    # Cox estratificado em amostra aleatoria de 500k
    set.seed(SEMENTE)
    am <- dd[sample(.N, min(.N, N_COX))]
    cx <- coxph(Surv(meses, voltou) ~ broadness + log_rem0 + idade + tempo + factor(sexo) +
                  factor(grauinstr) + strata(div), am[!is.na(log_rem0)])
    writeLines(c(paste0("Cox em amostra aleatoria de ", nrow(am), " de ", nrow(dd),
                        " observacoes (semente ", SEMENTE, ")"), "",
                 capture.output(summary(cx))),
               file.path(DIR_TAB, paste0("t12_cox_", k, ".txt")))

    # Cox na amostra cheia. Descomente se quiser rodar (leva varias horas por coorte).
    # cx_cheio <- coxph(Surv(meses, voltou) ~ broadness + log_rem0 + idade + tempo +
    #                     factor(sexo) + factor(grauinstr) + strata(div), dd[!is.na(log_rem0)])
    # writeLines(c(paste0("Cox na amostra cheia, ", nrow(dd), " observacoes"), "",
    #              capture.output(summary(cx_cheio))),
    #            file.path(DIR_TAB, paste0("t12_cox_cheia_", k, ".txt")))

    b1  <- coef(m1)["broadness"];  se1 <- se(m1)["broadness"]
    b2  <- coef(m2)["broadness"];  se2 <- se(m2)["broadness"]
    b2w <- coef(m2w)["broadness"]; se2w <- se(m2w)["broadness"]
    b3  <- coef(m3)["broadness"];  se3 <- se(m3)["broadness"]
    scx <- summary(cx)$coefficients["broadness", ]
    bcx <- scx["coef"];            secx <- scx["se(coef)"]
    bw  <- if (!is.null(m4)) coef(m4)["broadness"] else NA_real_
    sew <- if (!is.null(m4)) se(m4)["broadness"]   else NA_real_

    res_reg[[k]] <- data.table(
      coorte = k, ano = COORTES[[k]]$ano, fase = COORTES[[k]]$fase, n_obs = nrow(dd),
      b_12m_s = b1, se_12m_s = se1,
      b_12m_ef = b2, se_12m_ef = se2,
      b_12m_w0 = b2w, se_12m_w0 = se2w,
      b_24m_ef = b3, se_24m_ef = se3,
      b_cox = bcx, se_cox = secx,
      b_sal_ef = bw, se_sal_ef = sew,
      str_12m_s  = est_str(b1, se1),
      str_12m_ef = est_str(b2, se2),
      str_12m_w0 = est_str(b2w, se2w),
      str_24m_ef = est_str(b3, se3),
      str_cox    = est_str(bcx, secx),
      str_sal_ef = est_str(bw, sew)
    )

    # --- T13: Mecanismo 1 — Penalidade salarial ao mudar de setor (Neal, 1995)
    if (sum(!is.na(dd$d_log_rem) & !is.na(dd$mudou_setor)) > 100) {
      m_neal <- feols(d_log_rem ~ mudou_setor * broadness + log_rem0 + idade + tempo +
                        factor(sexo) + factor(grauinstr) | div + mun,
                      dd[!is.na(d_log_rem) & !is.na(mudou_setor)], cluster = ~div)
      cn <- names(coef(m_neal))
      ix_int <- grep("mudou_setor.*broadness|broadness.*mudou_setor", cn, value = TRUE)[1]
      res_t13[[k]] <- data.table(
        Coorte                      = COORTES[[k]]$ano,
        Período                     = COORTES[[k]]$fase,
        Reentrantes                 = mil(nobs(m_neal)),
        `Mudou de setor ($\\delta_1$)` = est_str(coef(m_neal)["mudou_setor"], se(m_neal)["mudou_setor"]),
        `Portabilidade ($\\delta_2$)`  = est_str(coef(m_neal)["broadness"], se(m_neal)["broadness"]),
        `Mudou $\\times$ Portab. ($\\delta_3$)` = est_str(coef(m_neal)[ix_int], se(m_neal)[ix_int])
      )
    }

    # --- T14: Mecanismo 2 e 3 — Salario-Duracao e Subamostras do Seguro-Desemprego
    m_sem_seg <- feols(I(voltou == 1 & meses <= 6) ~ broadness + log_rem0 + idade + tempo +
                         factor(sexo) + factor(grauinstr) | div + mun,
                       dd[tempo < 6 & !is.na(log_rem0)], cluster = ~div)
    m_com_seg <- feols(I(voltou == 1 & meses <= 6) ~ broadness + log_rem0 + idade + tempo +
                         factor(sexo) + factor(grauinstr) | div + mun,
                       dd[tempo >= 24 & !is.na(log_rem0)], cluster = ~div)
    m_dur_sal <- if (sum(!is.na(dd$d_log_rem)) > 100) {
      feols(d_log_rem ~ meses * broadness + log_rem0 + idade + tempo +
              factor(sexo) + factor(grauinstr) | div + mun,
            dd[voltou == 1 & meses <= 24 & !is.na(d_log_rem)], cluster = ~div)
    } else NULL
    ix_md <- if (!is.null(m_dur_sal)) grep("meses.*broadness|broadness.*meses", names(coef(m_dur_sal)), value = TRUE)[1] else NA
    res_t14[[k]] <- data.table(
      Coorte                            = COORTES[[k]]$ano,
      Período                           = COORTES[[k]]$fase,
      `Voltou 6m: s/ seguro (<6m)`      = est_str(coef(m_sem_seg)["broadness"], se(m_sem_seg)["broadness"]),
      `Voltou 6m: seguro 5p (>=24m)`    = est_str(coef(m_com_seg)["broadness"], se(m_com_seg)["broadness"]),
      `$\\Delta\\ln w$: Meses parado`   = if (!is.null(m_dur_sal)) est_str(coef(m_dur_sal)["meses"], se(m_dur_sal)["meses"]) else "---",
      `$\\Delta\\ln w$: Meses $\\times$ Portab.` = if (!is.null(m_dur_sal)) est_str(coef(m_dur_sal)[ix_md], se(m_dur_sal)[ix_md]) else "---"
    )

    # --- T15: Mecanismo 4 — Ponte Regional: Portabilidade x Choque Setorial Local
    if ("g_setor_regiao" %in% names(dd) && sum(!is.na(dd$g_setor_regiao)) > 500) {
      m_ch_v <- feols(I(voltou == 1 & meses <= 12) ~ broadness * g_setor_regiao + log_rem0 +
                        idade + tempo + factor(sexo) + factor(grauinstr) | div + mun,
                      dd[!is.na(g_setor_regiao) & !is.na(log_rem0)], cluster = ~div)
      m_ch_s <- feols(mudou_setor ~ broadness * g_setor_regiao + log_rem0 +
                        idade + tempo + factor(sexo) + factor(grauinstr) | div + mun,
                      dd[voltou == 1 & !is.na(g_setor_regiao) & !is.na(log_rem0)], cluster = ~div)
      ix_c1 <- grep("broadness.*g_setor|g_setor.*broadness", names(coef(m_ch_v)), value = TRUE)[1]
      ix_c2 <- grep("broadness.*g_setor|g_setor.*broadness", names(coef(m_ch_s)), value = TRUE)[1]
      res_t15[[k]] <- data.table(
        Coorte                                = COORTES[[k]]$ano,
        Período                               = COORTES[[k]]$fase,
        `Voltou 12m: Choque $g_{r,s}$`        = est_str(coef(m_ch_v)["g_setor_regiao"], se(m_ch_v)["g_setor_regiao"]),
        `Voltou 12m: Portab. $\\times g_{r,s}$` = est_str(coef(m_ch_v)[ix_c1], se(m_ch_v)[ix_c1]),
        `Mudou setor: Portab. $\\times g_{r,s}$`= est_str(coef(m_ch_s)[ix_c2], se(m_ch_s)[ix_c2])
      )
    }

    cat("regressoes e mecanismos:", k, "\n")
    rm(dd, am); gc()
  }

  if (length(res_reg)) {
    dt_reg <- rbindlist(res_reg)
    fwrite(dt_reg, file.path(DIR_DADOS, "t12_coeficientes.csv"))

    t12_res <- dt_reg[, .(
      Coorte                       = ano,
      Observações                  = mil(n_obs),
      `MPL 12m (s/ EF)`            = str_12m_s,
      `MPL 12m (c/ EF)`            = str_12m_ef,
      `MPL 12m (EF + $\\ln w_0$)`  = str_12m_w0,
      `MPL 24m (EF + $\\ln w_0$)`  = str_24m_ef,
      `Cox estrat. (log HR)`       = str_cox,
      `$\\Delta\\ln w$ (c/ EF)`    = str_sal_ef
    )]
    tex(t12_res, "t12_regressoes_resumo.tex", "lrrrrrrr",
        paste0("Coeficiente do índice de portabilidade ocupacional (fixado em ", ANO_BASE,
               ") e erro-padrão entre parênteses (clusterizado por divisão CNAE). ",
               "As colunas 'EF + ln w0' controlam pelo log do salário no vínculo perdido, idade, ",
               "tenure, sexo, escolaridade e efeitos fixos de divisão CNAE e município de origem."))
  }

  if (length(res_t13)) {
    tex(rbindlist(res_t13), "t13_mecanismo_setor_salario.tex", "llrrrr",
        paste0("Variável dependente: variação de log salário (ln w1 - ln w0) dos trabalhadores que ",
               "retornaram ao emprego formal. Testa se a portabilidade ocupacional atenua a perda ",
               "salarial associada à mudança de divisão CNAE (interação Mudou x Portabilidade > 0)."))
  }

  if (length(res_t14)) {
    tex(rbindlist(res_t14), "t14_mecanismo_reserva_seguro.tex", "llrrrr",
        paste0("As duas primeiras colunas comparam o efeito da portabilidade sobre o retorno em até 6 meses ",
               "entre demitidos sem elegibilidade ao seguro-desemprego (tenure < 6 meses) e com elegibilidade ",
               "máxima (tenure >= 24 meses, 5 parcelas). As duas últimas colunas estimam o gradiente entre ",
               "variação salarial e duração do desemprego interagido com a portabilidade."))
  }

  if (length(res_t15)) {
    tex(rbindlist(res_t15), "t15_mecanismo_choque_regional.tex", "llrrr",
        paste0("Interação entre o índice de portabilidade da ocupação de origem (fixado em ", ANO_BASE,
               ") e a taxa de crescimento anual do emprego formal no par (região intermediária, divisão CNAE) ",
               "no ano da demissão (g_{r,s})."))
  }
} else message("fixest ou survival ausentes, T12-T15 puladas")


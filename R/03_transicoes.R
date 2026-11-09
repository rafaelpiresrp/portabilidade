# =============================================================================
# 03_transicoes.R
# Tempo ate voltar ao emprego formal apos demissao, por grupo de portabilidade
# da ocupacao de origem (3 grupos fixos em ANO_BASE = 2010: 1=Alta, 2=Media, 3=Baixa).
# =============================================================================
source("config.R")
library(data.table)
library(survival)

USAR_CACHE_PASSOS_1_2 <- TRUE
arq_cache_coh <- file.path(DIR_DADOS, "cache_passo1_coh.rds")
arq_cache_adm <- file.path(DIR_DADOS, "cache_passo2_adm.rds")

so_digitos <- function(x) {
  y <- gsub("[^0-9]", "", as.character(x))
  y[is.na(y)] <- ""
  y
}
completa_zeros <- function(x, n) {
  i <- which(nchar(x) > 0 & nchar(x) < n)
  x[i] <- paste0(strrep("0", n - nchar(x[i])), x[i])
  x
}
codigo <- function(x, n) completa_zeros(so_digitos(x), n)

mil <- function(x) gsub("(?<=\\d)(?=(\\d{3})+$)", ".",
                        format(x, scientific = FALSE, trim = TRUE), perl = TRUE)

arquivos <- list.files(DIR_CSV, pattern = "_limpo\\.csv$")

le_ano <- function(a, ids = NULL) {
  COLS <- c("cpf_m",                                               # ID
            "naturjur", "causadesli", "mesdeslig", "dtadmissao",
            "ocup2002", "clascnae20", "municipio",
            "remmedia", "tempempr", "grauinstr", "sexo", "idade")
  fs <- arquivos[grepl(paste0("^[a-z]+", a, "_limpo\\.csv$"), arquivos)]
  partes <- list()
  for (f in fs) {
    cat("  ", a, f, "\n")
    dt <- fread(file.path(DIR_CSV, f), select = COLS)
    setnames(dt, "cpf_m", "id")                                    # ID
    dt[, id := as.character(id)]
    dt <- dt[!is.na(id) & id != ""]
    if (!is.null(ids)) dt <- dt[id %chin% ids]
    dt <- dt[naturjur > 2020]                                      # privado
    dt[, cbo := codigo(ocup2002, 6)]
    dt[, div := substr(codigo(clascnae20, 5), 1, 2)]
    dt[, mun := codigo(municipio, 6)]
    dt[, rem := as.numeric(gsub(",", ".", as.character(remmedia), fixed = TRUE))]
    dt[, tempo := as.numeric(gsub(",", ".", as.character(tempempr), fixed = TRUE))]
    s <- formatC(as.integer(so_digitos(dt$dtadmissao)),
                 width = 8, flag = "0", format = "d")
    dt[, m_adm := as.integer(substr(s, 5, 8))*12L + as.integer(substr(s, 3, 4))]
    dt[, mesdeslig := as.integer(mesdeslig)]
    dt[, m_fim := fifelse(mesdeslig %between% c(1L, 12L),
                          a*12L + mesdeslig, a*12L + 12L)]
    partes[[f]] <- dt[, .(id, cbo, div, mun, rem, tempo, m_adm, m_fim, mesdeslig,
                          causadesli = as.integer(causadesli),
                          grauinstr  = as.integer(grauinstr),
                          sexo = as.integer(sexo), idade = as.integer(idade))]
    rm(dt); gc()
  }
  rbindlist(partes)
}

# Carrega broadness.csv e monta a tabela de CBOs FIXA no ANO_BASE = 2010 (3 grupos)
broad    <- fread(file.path(DIR_DADOS, "broadness.csv"), colClasses = c(cbo = "character"))
base_cbo <- monta_base_cbo(broad, ANO_BASE, N_GRUPOS)
cat("Classificacao fixa carregada de ANO_BASE =", ANO_BASE,
    "| Cortes:", paste(round(attr(base_cbo, "cortes"), 4), collapse = " | "), "\n")

# De-para municipio -> regiao intermediaria e choque setorial local (se disponiveis do 02)
arq_dtb_csv <- file.path(DIR_DADOS, "depara_municipio_regiao.csv")
dtb_mun <- if (file.exists(arq_dtb_csv)) {
  unique(fread(arq_dtb_csv, colClasses = "character")[, .(mun, regiao = intermediaria)], by = "mun")
} else NULL

arq_choque <- file.path(DIR_DADOS, "choque_setorial_regiao.csv")
choque_rs <- if (file.exists(arq_choque)) {
  fread(arq_choque, colClasses = c(regiao = "character", div = "character"))
} else NULL

# =============================================================================
# Passo 1 e Passo 2: carrega do cache (ou monta do zero se nao existir)
# =============================================================================
if (USAR_CACHE_PASSOS_1_2 && file.exists(arq_cache_coh) && file.exists(arq_cache_adm)) {
  cat("\nCarregando cache dos Passos 1 e 2 do disco...\n")
  coh <- readRDS(arq_cache_coh)
  adm <- readRDS(arq_cache_adm)
  coh[, intersect(c("broadness", "grupo"), names(coh)) := NULL]
  coh <- merge(coh, base_cbo[, .(cbo, broadness = broadness_base, grupo)], by = "cbo")
  cat("Coortes carregadas:", mil(nrow(coh)), "demissoes |",
      mil(nrow(adm)), "admissoes posteriores\n")
} else {
  coortes <- list(); am <- list()
  
  for (nm in names(COORTES)) {
    C <- COORTES[[nm]]
    cat("\n#### coorte", nm, "- demitidos em", C$ano, "\n")
    v0 <- le_ano(C$ano)
    
    dd  <- v0[causadesli == 11 & mesdeslig %between% c(1L, 12L)]
    uma <- dd[, .N, by = id][N == 1, id]
    dd  <- dd[id %chin% uma]
    dd[, m_dem := C$ano*12L + mesdeslig]
    
    v1 <- merge(v0[, .(id, m_adm, m_fim)], dd[, .(id, m_dem)], by = "id",
                allow.cartesian = TRUE)
    n_at <- v1[m_adm <= m_dem & m_fim >= m_dem, .(n_ativos = .N), by = .(id, m_dem)]
    rm(v1); gc()
    dem <- merge(dd, n_at, by = c("id", "m_dem"))[n_ativos == 1]
    rm(dd, n_at); gc()
    
    dem <- merge(dem, base_cbo[, .(cbo, broadness = broadness_base, grupo)], by = "cbo")
    dem[, `:=`(coorte = nm, fase = C$fase, fim = C$ate*12L + 12L)]
    
    am[[nm]] <- data.table(coorte = nm, fase = C$fase, ano = C$ano,
                           pessoas           = v0[, uniqueN(id)],
                           uma_demissao      = length(uma),
                           sem_outro_emprego = nrow(dem),
                           horizonte_meses   = (C$ate - C$ano)*12L)
    print(am[[nm]])
    
    coortes[[nm]] <- dem[, .(id, coorte, fase, m_dem, fim, cbo, div, mun, rem,
                             broadness, grupo, grauinstr, sexo, idade, tempo)]
    rm(v0, uma, dem); gc()
  }
  coh <- rbindlist(coortes); rm(coortes); gc()
  fwrite(rbindlist(am), file.path(DIR_DADOS, "transicoes_amostra.csv"))
  saveRDS(coh, arq_cache_coh)
  
  ids     <- unique(coh$id)
  ANO_INI <- min(vapply(COORTES, function(C) C$ano, 0))
  ANO_FIM <- max(vapply(COORTES, function(C) C$ate, 0))
  cat("\n#### procurando admissoes de", mil(length(ids)),
      "pessoas,", ANO_INI, "a", ANO_FIM, "\n")
  
  m_min <- coh[, .(m_min = min(m_dem)), by = id]
  
  adm <- list()
  for (y in ANO_INI:ANO_FIM) {
    z <- le_ano(y, ids)[, .(id, m_adm, cbo, div, mun, rem)]
    z <- merge(z, m_min, by = "id")[m_adm > m_min][, m_min := NULL]
    adm[[as.character(y)]] <- unique(z)
    rm(z); gc()
  }
  rm(m_min); gc()
  adm <- unique(rbindlist(adm)); gc()
  cat("admissoes encontradas:", mil(nrow(adm)), "\n")
  saveRDS(adm, arq_cache_adm)
}

# =============================================================================
# Passo 3: primeiro retorno de cada pessoa, em cada coorte + mecanismos
# =============================================================================
dur       <- list()
comp      <- list()
norm      <- list()
km        <- list()
dsc       <- list()
cal       <- list()
sal_dur_l <- list()
haz_ten_l <- list()
sal_dst_l <- list()
tab_km_l  <- list()
suf_emp   <- list()

mediana_ponderada <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (!length(x)) return(NA_real_)
  o <- order(x)
  x <- x[o]; w <- w[o]
  p <- cumsum(as.numeric(w)) / sum(as.numeric(w))
  as.numeric(x[which(p >= 0.5)[1]])
}

for (nm in names(COORTES)) {
  C  <- COORTES[[nm]]
  cc <- coh[coorte == nm]
  j  <- merge(adm[id %chin% cc$id], cc[, .(id, m_dem)], by = "id")[m_adm > m_dem]
  setorder(j, id, m_adm)
  prim <- unique(j, by = "id"); rm(j); gc()
  
  d <- merge(cc, prim[, .(id, m_adm, cbo1 = cbo, div1 = div, mun1 = mun, rem1 = rem)],
             by = "id", all.x = TRUE)
  rm(prim); gc()
  
  d[, meses       := fifelse(is.na(m_adm), fim - m_dem, m_adm - m_dem)]
  d[, voltou      := as.integer(!is.na(m_adm))]
  d[, migrou      := fifelse(voltou == 1L & mun1 != mun, 1L, 0L)]
  d[, mudou_setor := fifelse(voltou == 1L, as.integer(div1 != div), NA_integer_)]
  d[, mesma_cbo   := fifelse(voltou == 1L, as.integer(cbo1 == cbo), NA_integer_)]
  d[, log_rem0    := fifelse(!is.na(rem) & rem > 0, log(rem), NA_real_)]
  d[, d_log_rem   := fifelse(voltou == 1L & !is.na(rem) & !is.na(rem1) & rem > 0 & rem1 > 0,
                             log(rem1) - log(rem), NA_real_)]
  
  # Atualiza regiao e choque setorial in-place (sem copiar a tabela na memoria!)
  d[, regiao := NA_character_]
  if (!is.null(dtb_mun)) {
    d[dtb_mun, regiao := i.regiao, on = "mun"]
  }
  d[, g_setor_regiao := NA_real_]
  if (!is.null(choque_rs)) {
    ch_ano <- unique(choque_rs[ano == C$ano, .(regiao, div, g_setor_regiao)], by = c("regiao", "div"))
    d[ch_ano, g_setor_regiao := i.g_setor_regiao, on = c("regiao", "div")]
  }
  
  HORIZ <- (C$ate - C$ano) * 12L
  
  # --- 1) Perfil dos demitidos por grupo (T9) --------------------------------
  dsc[[nm]] <- d[, .(
    pessoas           = .N,
    broadness_media   = round(mean(broadness, na.rm = TRUE), 3),
    idade_media       = mean(idade, na.rm = TRUE),
    pct_mulher        = 100 * mean(sexo == 2, na.rm = TRUE),
    pct_medio_ou_mais = 100 * mean(grauinstr >= 7, na.rm = TRUE),
    tempo_medio_meses = mean(tempo, na.rm = TRUE),
    salario_medio_sm  = mean(rem[which(rem > 0)], na.rm = TRUE)
  ), by = grupo][order(grupo)][, `:=`(coorte = nm, fase = C$fase)]
  
  # --- 2) Duracao do retorno em multiplos horizontes (T10, T10b, T10c) -------
  du <- d[, .(
    pessoas      = .N,
    pct_voltou   = round(100 * mean(voltou), 1),
    mediana_mes  = as.numeric(median(meses[which(voltou == 1L)], na.rm = TRUE)),
    em_6m        = round(100 * mean(voltou == 1L & meses <=  6), 1),
    em_12m       = round(100 * mean(voltou == 1L & meses <= 12), 1),
    em_24m       = round(100 * mean(voltou == 1L & meses <= 24), 1),
    em_36m       = round(100 * mean(voltou == 1L & meses <= 36), 1),
    em_48m       = round(100 * mean(voltou == 1L & meses <= 48), 1),
    em_60m       = round(100 * mean(voltou == 1L & meses <= 60), 1),
    em_84m       = round(100 * mean(voltou == 1L & meses <= 84), 1),
    pct_migrou   = round(100 * sum(migrou, na.rm = TRUE) / sum(voltou), 1),
    d_log_rem    = round(mean(d_log_rem, na.rm = TRUE), 4),
    d_log_rem_md = round(median(d_log_rem, na.rm = TRUE), 4)
  ), by = grupo][order(grupo)]
  if (HORIZ < 36) du[, em_36m := NA_real_]
  if (HORIZ < 48) du[, em_48m := NA_real_]
  if (HORIZ < 60) du[, em_60m := NA_real_]
  if (HORIZ < 84) du[, em_84m := NA_real_]
  du[, `:=`(coorte = nm, fase = C$fase, horizonte = HORIZ)]
  dur[[nm]] <- du
  cat("\n#### retorno,", nm, "\n"); print(dur[[nm]])
  
  # --- 3) Retorno acumulado por ano-calendario (via tabela leve, instantaneo)
  t_cal <- d[, .(peso = as.numeric(.N)), by = .(grupo, voltou, m_adm)]
  anos_cal <- C$ano:C$ate
  cal_g <- rbindlist(lapply(anos_cal, function(y_cal) {
    m_lim <- y_cal * 12L + 12L
    t_cal[, .(
      coorte       = nm,
      ano_demissao = C$ano,
      fase         = C$fase,
      ano_retorno  = y_cal,
      anos_apos    = y_cal - C$ano,
      pessoas      = sum(peso),
      pct_acum     = round(100 * sum(peso[which(voltou == 1L & m_adm <= m_lim)]) / sum(peso), 1)
    ), by = grupo]
  }))
  cal_tot <- rbindlist(lapply(anos_cal, function(y_cal) {
    m_lim <- y_cal * 12L + 12L
    t_cal[, .(
      coorte       = nm,
      ano_demissao = C$ano,
      fase         = C$fase,
      grupo        = 0L,
      ano_retorno  = y_cal,
      anos_apos    = y_cal - C$ano,
      pessoas      = sum(peso),
      pct_acum     = round(100 * sum(peso[which(voltou == 1L & m_adm <= m_lim)]) / sum(peso), 1)
    )]
  }))
  cal[[nm]] <- rbind(cal_tot, cal_g, use.names = TRUE)
  
  # --- 4) Para onde foi quem voltou (T11 e T11b normalizada) -----------------
  d[, destino := fifelse(voltou == 0L, "0 Nao voltou",
                         fifelse(cbo1 == cbo & div1 == div,           "1 Mesma ocupacao, mesmo setor",
                                 fifelse(cbo1 == cbo,                         "2 Mesma ocupacao, outro setor",
                                         fifelse(substr(cbo1, 1, 4) == substr(cbo, 1, 4), "3 Mesma familia, outra ocupacao",
                                                 "4 Outra ocupacao"))))]
  cp <- d[, .N, by = .(grupo, destino)]
  cp[, pct := round(100 * N / sum(N), 2), by = grupo]
  cp[, pct_cond := fifelse(destino == "0 Nao voltou", NA_real_,
                           round(100 * N / sum(N[destino != "0 Nao voltou"]), 2)),
     by = grupo]
  comp[[nm]] <- cp[, `:=`(coorte = nm, fase = C$fase)]
  
  norm[[nm]] <- d[voltou == 1L, .(
    voltou_total        = .N,
    mesma_cbo_n         = sum(cbo1 == cbo, na.rm = TRUE),
    pct_mesma_cbo       = round(100 * mean(cbo1 == cbo, na.rm = TRUE), 1),
    obs_outro_setor     = round(100 * sum(cbo1 == cbo & div1 != div, na.rm = TRUE) / sum(cbo1 == cbo, na.rm = TRUE), 1),
    prev_broadness      = round(100 * mean(broadness[which(cbo1 == cbo)], na.rm = TRUE), 1),
    obs_outro_setor_all = round(100 * mean(div1 != div, na.rm = TRUE), 1),
    prev_broadness_all  = round(100 * mean(broadness, na.rm = TRUE), 1)
  ), by = grupo][order(grupo)][, `:=`(
    razao_obs_prev = round(obs_outro_setor / prev_broadness, 2),
    coorte = nm,
    fase   = C$fase
  )]
  
  # --- 5) Insumos dos Testes de Mecanismos (F13, F14, F15 e TA3) -------------
  sal_dur_l[[nm]] <- d[voltou == 1L & meses %between% c(1L, 24L) & !is.na(d_log_rem), .(
    n_obs      = as.numeric(.N),
    soma_dlog  = sum(d_log_rem, na.rm = TRUE),
    soma_dlog2 = sum(d_log_rem^2, na.rm = TRUE),
    d_log_med  = mean(d_log_rem, na.rm = TRUE)
  ), by = .(grupo, meses)][, `:=`(coorte = nm, fase = C$fase)]
  
  d[, faixa_seguro := fifelse(tempo < 6, "Sem seguro (< 6 meses)",
                              fifelse(tempo >= 24, "Seguro 5 parcelas (>= 24 meses)",
                                      "Seguro 3-4 parcelas (6 a 23 meses)"))]
  t_seg <- d[faixa_seguro %in% c("Sem seguro (< 6 meses)", "Seguro 5 parcelas (>= 24 meses)"),
             .(peso = as.numeric(.N)), by = .(faixa_seguro, grupo, meses, voltou)]
  haz_ten_l[[nm]] <- copy(t_seg)[, `:=`(coorte = nm, fase = C$fase)]
  
  sal_dst_l[[nm]] <- d[voltou == 1L & !is.na(d_log_rem), .(
    n_obs      = as.numeric(.N),
    soma_dlog  = sum(d_log_rem, na.rm = TRUE),
    soma_dlog2 = sum(d_log_rem^2, na.rm = TRUE),
    d_log_med  = mean(d_log_rem, na.rm = TRUE),
    meses_med  = as.numeric(median(meses, na.rm = TRUE))
  ), by = .(grupo, destino)][, `:=`(coorte = nm, fase = C$fase)]
  
  # --- 6) Kaplan-Meier via tabela ponderada (exato e instantaneo) ------------
  t_km <- d[, .(peso = as.numeric(.N)), by = .(grupo, meses, voltou)]
  tab_km_l[[nm]] <- copy(t_km)[, coorte := nm]
  
  sf <- survfit(Surv(meses, voltou) ~ grupo, data = t_km, weights = peso)
  st <- rep(names(sf$strata), sf$strata)
  passos <- data.table(grupo = as.integer(sub(".*=", "", st)),
                       mes   = sf$time,
                       surv  = sf$surv)
  setorder(passos, grupo, mes)
  grade <- CJ(grupo = sort(unique(d$grupo)), mes = 0:HORIZ)
  curva <- passos[grade, on = .(grupo, mes), roll = TRUE]
  curva[is.na(surv), surv := 1]
  curva[, `:=`(fora = surv, voltou_ate = 1 - surv, surv = NULL,
               coorte = nm, fase = C$fase)]
  km[[nm]] <- curva
  
  # --- 7) Estatisticas suficientes por coorte-grupo para os paineis empilhados
  suf_emp[[nm]] <- d[, .(
    n_tot           = as.numeric(.N),
    s_broad         = sum(broadness, na.rm = TRUE),
    s_idade         = sum(as.numeric(idade), na.rm = TRUE),
    n_mulher        = as.numeric(sum(sexo == 2, na.rm = TRUE)),
    n_medio         = as.numeric(sum(grauinstr >= 7, na.rm = TRUE)),
    s_tempo         = sum(tempo, na.rm = TRUE),
    s_rem_pos       = sum(rem[which(rem > 0)], na.rm = TRUE),
    n_rem_pos       = as.numeric(sum(rem > 0, na.rm = TRUE)),
    n_voltou        = as.numeric(sum(voltou == 1L, na.rm = TRUE)),
    n_6m            = as.numeric(sum(voltou == 1L & meses <=  6, na.rm = TRUE)),
    n_12m           = as.numeric(sum(voltou == 1L & meses <= 12, na.rm = TRUE)),
    n_24m           = as.numeric(sum(voltou == 1L & meses <= 24, na.rm = TRUE)),
    n_36m           = as.numeric(sum(voltou == 1L & meses <= 36, na.rm = TRUE)),
    n_48m           = as.numeric(sum(voltou == 1L & meses <= 48, na.rm = TRUE)),
    n_60m           = as.numeric(sum(voltou == 1L & meses <= 60, na.rm = TRUE)),
    n_84m           = as.numeric(sum(voltou == 1L & meses <= 84, na.rm = TRUE)),
    n_migrou        = as.numeric(sum(migrou == 1L, na.rm = TRUE)),
    s_dlog          = sum(d_log_rem, na.rm = TRUE),
    n_dlog          = as.numeric(sum(!is.na(d_log_rem))),
    md_dlog         = as.numeric(median(d_log_rem, na.rm = TRUE)),
    n_mesma_cbo     = as.numeric(sum(voltou == 1L & cbo1 == cbo, na.rm = TRUE)),
    n_outro_set_cbo = as.numeric(sum(voltou == 1L & cbo1 == cbo & div1 != div, na.rm = TRUE)),
    s_broad_cbo     = sum(broadness[which(voltou == 1L & cbo1 == cbo)], na.rm = TRUE),
    n_outro_set_all = as.numeric(sum(voltou == 1L & div1 != div, na.rm = TRUE)),
    s_broad_voltou  = sum(broadness[which(voltou == 1L)], na.rm = TRUE)
  ), by = grupo][, coorte := nm]
  
  # Salva arquivo individual da coorte enriquecido para regressoes (T12 a T15)
  fwrite(d[, .(coorte, fase, grupo, cbo, broadness, meses, voltou, destino,
               mudou_setor, mesma_cbo, migrou, div, mun, regiao,
               grauinstr, sexo, idade, tempo, rem, log_rem0, rem1, d_log_rem,
               g_setor_regiao)],
         file.path(DIR_DADOS, paste0("duracao_", nm, ".csv")))
  rm(cc, d, cp, t_km, t_seg, t_cal); gc()
}

# =============================================================================
# Passo 4: Paineis Empilhados / Cumulativos (Recessao, Pandemia e Total)
# =============================================================================
suf_dt   <- rbindlist(suf_emp)
tab_km_a <- rbindlist(tab_km_l)
comp_a   <- rbindlist(comp)

blocos_emp <- list(
  recessao = list(filtro = c("c2014", "c2015", "c2016"),
                  nome = "Recessão (2014–2016)", horiz = 84L),
  pandemia = list(filtro = c("c2019", "c2020", "c2021"),
                  nome = "Pandemia (2019–2021)", horiz = 24L),
  total    = list(filtro = names(COORTES),
                  nome = "Total acumulado",      horiz = 24L)
)

for (bk in names(blocos_emp)) {
  B  <- blocos_emp[[bk]]
  sb <- suf_dt[coorte %chin% B$filtro]
  kb <- tab_km_a[coorte %chin% B$filtro, .(peso = sum(peso)), by = .(grupo, meses, voltou)]
  if (!nrow(sb)) next
  
  dsc[[bk]] <- sb[, .(
    pessoas           = sum(n_tot),
    broadness_media   = round(sum(s_broad) / sum(n_tot), 3),
    idade_media       = sum(s_idade) / sum(n_tot),
    pct_mulher        = 100 * sum(n_mulher) / sum(n_tot),
    pct_medio_ou_mais = 100 * sum(n_medio) / sum(n_tot),
    tempo_medio_meses = sum(s_tempo) / sum(n_tot),
    salario_medio_sm  = sum(s_rem_pos) / sum(n_rem_pos)
  ), by = grupo][order(grupo)][, `:=`(coorte = bk, fase = B$nome)]
  
  med_g <- kb[voltou == 1L, .(mediana_mes = mediana_ponderada(meses, peso)), by = grupo]
  du_b <- sb[, .(
    pessoas      = sum(n_tot),
    pct_voltou   = round(100 * sum(n_voltou) / sum(n_tot), 1),
    em_6m        = round(100 * sum(n_6m)  / sum(n_tot), 1),
    em_12m       = round(100 * sum(n_12m) / sum(n_tot), 1),
    em_24m       = round(100 * sum(n_24m) / sum(n_tot), 1),
    em_36m       = if (B$horiz >= 36) round(100 * sum(n_36m) / sum(n_tot), 1) else NA_real_,
    em_48m       = if (B$horiz >= 48) round(100 * sum(n_48m) / sum(n_tot), 1) else NA_real_,
    em_60m       = if (B$horiz >= 60) round(100 * sum(n_60m) / sum(n_tot), 1) else NA_real_,
    em_84m       = if (B$horiz >= 84) round(100 * sum(n_84m) / sum(n_tot), 1) else NA_real_,
    pct_migrou   = round(100 * sum(n_migrou) / sum(n_voltou), 1),
    d_log_rem    = round(sum(s_dlog) / sum(n_dlog), 4),
    d_log_rem_md = round(weighted.mean(md_dlog, n_dlog, na.rm = TRUE), 4)
  ), by = grupo]
  du_b <- merge(du_b, med_g, by = "grupo", all.x = TRUE)[order(grupo)]
  setcolorder(du_b, names(dur[[1]])[1:13])
  du_b[, `:=`(coorte = bk, fase = B$nome, horizonte = B$horiz)]
  dur[[bk]] <- du_b
  
  cp_b <- comp_a[coorte %chin% B$filtro, .(N = sum(N)), by = .(grupo, destino)]
  cp_b[, pct := round(100 * N / sum(N), 2), by = grupo]
  cp_b[, pct_cond := fifelse(destino == "0 Nao voltou", NA_real_,
                             round(100 * N / sum(N[destino != "0 Nao voltou"]), 2)),
       by = grupo]
  comp[[bk]] <- cp_b[, `:=`(coorte = bk, fase = B$nome)]
  
  norm[[bk]] <- sb[, .(
    voltou_total        = sum(n_voltou),
    mesma_cbo_n         = sum(n_mesma_cbo),
    pct_mesma_cbo       = round(100 * sum(n_mesma_cbo) / sum(n_voltou), 1),
    obs_outro_setor     = round(100 * sum(n_outro_set_cbo) / sum(n_mesma_cbo), 1),
    prev_broadness      = round(100 * sum(s_broad_cbo) / sum(n_mesma_cbo), 1),
    obs_outro_setor_all = round(100 * sum(n_outro_set_all) / sum(n_voltou), 1),
    prev_broadness_all  = round(100 * sum(s_broad_voltou) / sum(n_voltou), 1)
  ), by = grupo][order(grupo)][, `:=`(
    razao_obs_prev = round(obs_outro_setor / prev_broadness, 2),
    coorte = bk,
    fase   = B$nome
  )]
  
  sf <- survfit(Surv(meses, voltou) ~ grupo, data = kb, weights = peso)
  st <- rep(names(sf$strata), sf$strata)
  passos <- data.table(grupo = as.integer(sub(".*=", "", st)),
                       mes   = sf$time,
                       surv  = sf$surv)
  setorder(passos, grupo, mes)
  grade <- CJ(grupo = sort(unique(kb$grupo)), mes = 0:B$horiz)
  curva <- passos[grade, on = .(grupo, mes), roll = TRUE]
  curva[is.na(surv), surv := 1]
  curva[, `:=`(fora = surv, voltou_ate = 1 - surv, surv = NULL,
               coorte = bk, fase = B$nome)]
  km[[bk]] <- curva
}

# Consolida tabelas de mecanismos (F13, F14, F15 e TA3)
sd_all <- rbindlist(sal_dur_l)
sd_tot <- sd_all[, .(
  n_obs     = sum(n_obs),
  d_log_med = sum(soma_dlog) / sum(n_obs),
  se_dlog   = sqrt(pmax(0, (sum(soma_dlog2) / sum(n_obs) - (sum(soma_dlog) / sum(n_obs))^2)) / sum(n_obs)),
  coorte    = "total",
  fase      = "Total acumulado"
), by = .(grupo, meses)]
sd_all[, se_dlog := sqrt(pmax(0, (soma_dlog2 / n_obs - d_log_med^2)) / n_obs)]
fwrite(rbind(sd_all[, .(coorte, fase, grupo, meses, n_obs, d_log_med, se_dlog)], sd_tot),
       file.path(DIR_DADOS, "transicoes_salario_duracao.csv"))

hz_raw <- rbindlist(haz_ten_l)
hz_tot <- hz_raw[, .(peso = sum(peso)), by = .(faixa_seguro, grupo, meses, voltou)]
calc_hazard <- function(dt_h, nm_c) {
  estratos <- unique(dt_h[, .(faixa_seguro, grupo)])
  setorder(estratos, faixa_seguro, grupo)
  res_h <- list()
  for (k in seq_len(nrow(estratos))) {
    fs_k  <- estratos$faixa_seguro[k]
    gr_k  <- estratos$grupo[k]
    sub_e <- dt_h[faixa_seguro == fs_k & grupo == gr_k]
    n_r   <- vapply(1:24, function(m) sum(sub_e[meses >= m, peso]), numeric(1))
    d_e   <- vapply(1:24, function(m) sum(sub_e[meses == m & voltou == 1L, peso]), numeric(1))
    res_h[[k]] <- data.table(
      faixa_seguro = fs_k,
      grupo        = gr_k,
      mes          = 1:24,
      n_risco      = n_r,
      eventos      = d_e,
      hazard_pct   = round(100 * d_e / pmax(1, n_r), 2),
      coorte       = nm_c
    )
  }
  rbindlist(res_h)
}
hz_out <- calc_hazard(hz_tot, "total")
fwrite(hz_out, file.path(DIR_DADOS, "transicoes_hazard_tenure.csv"))

st_all <- rbindlist(sal_dst_l)
st_tot <- st_all[, .(
  n_obs     = sum(n_obs),
  d_log_med = sum(soma_dlog) / sum(n_obs),
  se_dlog   = sqrt(pmax(0, (sum(soma_dlog2) / sum(n_obs) - (sum(soma_dlog) / sum(n_obs))^2)) / sum(n_obs)),
  meses_med = round(weighted.mean(meses_med, n_obs, na.rm = TRUE), 1),
  coorte    = "total",
  fase      = "Total acumulado"
), by = .(grupo, destino)]
st_all[, se_dlog := sqrt(pmax(0, (soma_dlog2 / n_obs - d_log_med^2)) / n_obs)]
fwrite(rbind(st_all[, .(coorte, fase, grupo, destino, n_obs, d_log_med, se_dlog, meses_med)], st_tot),
       file.path(DIR_DADOS, "transicoes_salario_destino.csv"))

fwrite(rbindlist(dsc,  use.names = TRUE), file.path(DIR_DADOS, "transicoes_perfil.csv"))
fwrite(rbindlist(dur,  use.names = TRUE), file.path(DIR_DADOS, "transicoes_duracao.csv"))
fwrite(rbindlist(comp, use.names = TRUE), file.path(DIR_DADOS, "transicoes_composicao.csv"))
fwrite(rbindlist(norm, use.names = TRUE), file.path(DIR_DADOS, "transicoes_destino_norm.csv"))
fwrite(rbindlist(km,   use.names = TRUE), file.path(DIR_DADOS, "transicoes_km.csv"))
fwrite(rbindlist(cal,  use.names = TRUE), file.path(DIR_DADOS, "transicoes_calendario.csv"))

cat("\n--- % de volta em", HORIZ_COMUM, "meses, por coorte e grupo (1=Alta, 2=Media, 3=Baixa) ---\n")
print(dcast(rbindlist(dur, use.names = TRUE), coorte + fase ~ grupo, value.var = "em_24m"))
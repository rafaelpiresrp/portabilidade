# =============================================================================
# 01_broadness.R
# Broadness (amplitude de setores) de cada ocupacao, um valor por ano, 2009-2023
#
# Para cada ano t e cada ocupacao o (CBO 2002, 6 digitos):
#   p_os,t      = vinculos da ocupacao o no setor s / vinculos da ocupacao o
#   broadness_o = 1 - soma_s (p_os,t)^2
#
# Setor s = divisao da CNAE 2.0 (2 primeiros digitos da classe).
# O indice e nacional: junta todos os municipios, uma linha por ocupacao-ano.
#
# Grupos: 3 tercis de emprego de ANO_BASE = 2010 (N_GRUPOS = 3).
# Tanto o grupo (1 = Alta, 2 = Media, 3 = Baixa) quanto a broadness_base de
# cada CBO ficam FIXOS em ANO_BASE para todos os anos.
#
# Alem disso, calcula e grava a decomposicao shift-share acumulada ano a ano
# desde ANO_BASE (broadness_shiftshare_anual.csv) e as estatisticas de
# percentis anuais (broadness_percentis.csv) para o anexo descritivo.
#
# ATALHO INTELIGENTE:
# Se saidas/dados/broadness.csv ja existir no disco, nao rele os CSV brutos da
# RAIS: apenas atualiza os 3 grupos fixos em 2010 e gera as tabelas derivadas
# em 1 segundo. Para forcar a releitura bruta, mude RECALCULAR_DO_ZERO <- TRUE.
# =============================================================================
source("config.R")
library(data.table)

RECALCULAR_DO_ZERO <- FALSE
arq_broad <- file.path(DIR_DADOS, "broadness.csv")

COLS <- c("empem3112",    # vinculo ativo em 31/12
          "ocup2002",     # CBO 2002
          "clascnae20",   # CNAE 2.0, classe
          "naturjur")     # natureza juridica

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

if (file.exists(arq_broad) && !RECALCULAR_DO_ZERO) {
  cat("broadness.csv ja existe em disco; reutilizando contagens e atualizando os",
      N_GRUPOS, "grupos fixos no ANO_BASE =", ANO_BASE, "...\n")
  broad <- fread(arq_broad, colClasses = c(cbo = "character"))
  broad[, cbo := codigo(cbo, 6)]
  broad <- broad[, .(ano, cbo, vinculos, n_setores, broadness)]
} else {
  arquivos <- list.files(DIR_CSV, pattern = "_limpo\\.csv$")
  lista <- list()

  for (a in ANOS) {
    fs <- arquivos[grepl(paste0("^[a-z]+", a, "_limpo\\.csv$"), arquivos)]
    if (!length(fs)) { message("sem arquivo para ", a, ", pulando"); next }

    parciais <- list()
    for (f in fs) {
      cat(a, f, "... ")
      dt <- fread(file.path(DIR_CSV, f), select = COLS)

      # 1) Vinculo ativo em 31/12 e setor privado (naturjur > 2020)
      dt <- dt[empem3112 == 1 & naturjur > 2020]

      # 2) Ocupacao com 6 digitos e setor = divisao CNAE 2.0
      dt[, cbo := codigo(ocup2002, 6)]
      dt[, div := substr(codigo(clascnae20, 5), 1, 2)]

      # 3) So codigos validos (tira ignorado, vazio e zerado)
      dt <- dt[nchar(cbo) == 6 & cbo != "000000" & nchar(div) == 2 & div != "00"]

      # 4) Agrega aqui e joga a base fora
      parciais[[f]] <- dt[, .(n = .N), by = .(cbo, div)]
      cat(mil(nrow(parciais[[f]])), "pares\n")
      rm(dt); gc()
    }

    os <- rbindlist(parciais)[, .(n = sum(n)), by = .(cbo, div)]
    rm(parciais); gc()

    os[, p := n / sum(n), by = cbo]
    lista[[as.character(a)]] <- os[, .(vinculos  = sum(n),
                                       n_setores = .N,
                                       broadness = 1 - sum(p^2)), by = cbo][, ano := a]
    rm(os); gc()
    cat("  ", a, "ok:", nrow(lista[[as.character(a)]]), "ocupacoes\n")
  }

  broad <- rbindlist(lista)
  setcolorder(broad, c("ano", "cbo", "vinculos", "n_setores", "broadness"))
}

# --- 1) Grupos e broadness FIXOS por ocupacao no ANO_BASE (2010) --------------
base_cbo <- monta_base_cbo(broad, ANO_BASE, N_GRUPOS)
CORTES   <- attr(base_cbo, "cortes")
cat("\nCortes de", ANO_BASE, "(", N_GRUPOS, "grupos ):",
    paste(round(CORTES, 4), collapse = " | "), "\n")

broad <- merge(broad, base_cbo[, .(cbo, broadness_base, grupo)], by = "cbo", all.x = TRUE)
broad[, grupo_corrente := as.integer(as.character(
  cut(broadness, c(-Inf, CORTES, Inf), labels = rev(seq_len(N_GRUPOS)))
))]

setorder(broad, ano, cbo)
fwrite(broad, arq_broad)
fwrite(base_cbo, file.path(DIR_DADOS, "broadness_base_cbo.csv"))
fwrite(data.table(quantil = seq_len(N_GRUPOS - 1) / N_GRUPOS, corte = CORTES),
       file.path(DIR_DADOS, "broadness_cortes.csv"))

# --- 2) Composicao por grupo no tempo -----------------------------------------
comp <- broad[, .(
  ocupacoes       = .N,
  vinculos        = sum(vinculos),
  faixa_min_base  = min(broadness_base, na.rm = TRUE),
  faixa_max_base  = max(broadness_base, na.rm = TRUE),
  broadness_media = weighted.mean(broadness, vinculos),
  broad_base_med  = weighted.mean(broadness_base, vinculos)
), by = .(ano, grupo)]
comp[, pct_vinc := round(100 * vinculos / sum(vinculos), 2), by = ano]
setorder(comp, ano, grupo)
fwrite(comp, file.path(DIR_DADOS, "broadness_grupos.csv"))

# --- 3) Serie anual da decomposicao Shift-Share em relacao ao ANO_BASE --------
sh_all <- copy(broad)
sh_all[, s := vinculos / sum(vinculos), by = ano]
b_ref <- sh_all[ano == ANO_BASE, .(cbo, s0 = s, b0 = broadness)]
sh_anual <- rbindlist(lapply(sort(unique(sh_all$ano)), function(y) {
  at <- sh_all[ano == y, .(cbo, s1 = s, b1 = broadness)]
  m  <- merge(b_ref, at, by = "cbo", all = TRUE)
  m[is.na(s0), s0 := 0]; m[is.na(s1), s1 := 0]
  m[is.na(b0), b0 := 0]; m[is.na(b1), b1 := 0]
  e  <- sum((m$s1 - m$s0) * m$b0)
  dd <- sum(m$s0 * (m$b1 - m$b0))
  cx <- sum((m$s1 - m$s0) * (m$b1 - m$b0))
  data.table(ano = y, ano_base = ANO_BASE,
             entre_ocup = e, dentro_ocup = dd, cruzado = cx, total = e + dd + cx)
}))
fwrite(sh_anual, file.path(DIR_DADOS, "broadness_shiftshare_anual.csv"))

# --- 4) Estatisticas descritivas e percentis ponderados por ano (Anexo TA1) ---
quantil_pond <- function(x, w, probs) {
  o <- order(x); x <- x[o]; w <- as.numeric(w[o])
  p <- cumsum(w) / sum(w)
  sapply(probs, function(q) x[which(p >= q)[1]])
}
perc_anual <- broad[, {
  qs <- quantil_pond(broadness, vinculos, c(0.10, 0.25, 0.50, 0.75, 0.90))
  mu <- weighted.mean(broadness, vinculos)
  sd_w <- sqrt(sum((vinculos / sum(vinculos)) * (broadness - mu)^2))
  .(vinculos = sum(vinculos), ocupacoes = .N,
    media_pond = mu, dp_pond = sd_w,
    p10 = qs[1], p25 = qs[2], p50 = qs[3], p75 = qs[4], p90 = qs[5])
}, by = ano]
fwrite(perc_anual, file.path(DIR_DADOS, "broadness_percentis.csv"))

cat("\n--- Parcela do emprego por grupo FIXO em", ANO_BASE, "(1 = Alta portabilidade, %) ---\n")
print(dcast(comp, ano ~ grupo, value.var = "pct_vinc"))


# =============================================================================
# 02_flexibilidade.R
# Turbulencia, descasamento e flexibilidade alocativa por regiao, 2009-2023
#
# Entre t0 e t1, para cada regiao j:
#   a_s = participacao do setor s no emprego da regiao
#   T   = 1/2 * soma_s |a_s,t1 - a_s,t0|
#   g_s = e1/e0 - 1  (setor sem emprego em t0 nao tem taxa e fica fora do D)
#   g_j = crescimento do emprego total da regiao
#   D   = soma_s (g_s - g_j)^2 * a_s,t0   (peso do setor em t0)
#   F   = T / D
#
# Setor = divisao da CNAE 2.0. Regiao = geografica intermediaria e UF.
#
# NOVIDADES NESTA VERSAO:
#   1) Usa os 3 grupos fixos em ANO_BASE (2010) e grava share_grupo1,
#      share_grupo2 e share_grupo3 por regiao.
#   2) Grava choque_setorial_regiao.csv: taxa de crescimento anual do emprego
#      g_{r,s,t} para cada (regiao intermediaria, divisao CNAE, ano), usada no
#      script 03/04 para conectar a parte regional com as transicoes individuais!
#   3) Salva cache das contagens municipais (cache_02_contagens.rds) para que
#      mudancas futuras em ANO_BASE ou JANELAS rodem em segundos sem reler CSVs.
# =============================================================================
source("config.R")
library(data.table)
library(readxl)

USAR_CACHE_02 <- TRUE
arq_cache_02  <- file.path(DIR_DADOS, "cache_02_contagens.rds")

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

# --- de-para do IBGE. A DTB traz 7 digitos (com verificador), a RAIS usa 6 ----
dtb <- as.data.table(read_excel(ARQ_DTB, skip = 6))
print(names(dtb))
pega <- function(p) {
  g <- grep(p, names(dtb), value = TRUE)
  if (!length(g)) stop("coluna nao encontrada: ", p)
  g[1]
}
dtb <- dtb[, .(
  mun           = substr(as.character(get(pega("Completo"))), 1, 6),
  intermediaria = as.character(get(pega("Geogr.fica Intermedi.ria$"))),
  nome_inter    = as.character(get(pega("Nome Regi.o Geogr.fica Intermedi.ria"))),
  uf            = as.character(get(pega("^UF$"))),
  nome_uf       = as.character(get(pega("Nome_UF")))
)]
dtb <- unique(dtb, by = "mun")
fwrite(dtb, file.path(DIR_DADOS, "depara_municipio_regiao.csv"))

broad    <- fread(file.path(DIR_DADOS, "broadness.csv"), colClasses = c(cbo = "character"))
base_cbo <- monta_base_cbo(broad, ANO_BASE, N_GRUPOS)

COLS <- c("municipio", "empem3112", "ocup2002", "clascnae20", "naturjur", "remmedia")

mil <- function(x) gsub("(?<=\\d)(?=(\\d{3})+$)", ".",
                        format(x, scientific = FALSE, trim = TRUE), perl = TRUE)

if (USAR_CACHE_02 && file.exists(arq_cache_02)) {
  cat("Carregando cache de contagens regionais de", arq_cache_02, "...\n")
  cache02 <- readRDS(arq_cache_02)
  cr      <- cache02$cr
  reg_cbo <- cache02$reg_cbo
  rm(cache02); gc()
} else {
  arquivos <- list.files(DIR_CSV, pattern = "_limpo\\.csv$")
  lista_cr <- list()
  lista_rc <- list()

  for (a in ANOS) {
    fs <- arquivos[grepl(paste0("^[a-z]+", a, "_limpo\\.csv$"), arquivos)]
    if (!length(fs)) { message("sem arquivo para ", a, ", pulando"); next }

    pa <- list(); pb <- list()
    for (f in fs) {
      cat(a, f, "... ")
      dt <- fread(file.path(DIR_CSV, f), select = COLS)
      dt <- dt[empem3112 == 1 & naturjur > 2020]
      dt[, mun := codigo(municipio, 6)]
      dt[, div := substr(codigo(clascnae20, 5), 1, 2)]
      dt[, cbo := codigo(ocup2002, 6)]
      dt[, rem := as.numeric(gsub(",", ".", as.character(remmedia), fixed = TRUE))]

      pa[[f]] <- dt[nchar(mun) == 6 & nchar(div) == 2 & div != "00",
                    .(n = .N, soma_rem = sum(rem, na.rm = TRUE)), by = .(mun, div)]
      pb[[f]] <- dt[nchar(mun) == 6 & nchar(cbo) == 6 & cbo != "000000",
                    .(n = .N), by = .(mun, cbo)]

      cat(mil(nrow(pa[[f]])), "mun-div | ",
          mil(nrow(pb[[f]])), "mun-cbo\n")
      rm(dt); gc()
    }

    lista_cr[[as.character(a)]] <-
      rbindlist(pa)[, .(n = sum(n), soma_rem = sum(soma_rem)), by = .(mun, div)][, ano := a]
    rm(pa); gc()

    # Colapsa mun-cbo para (intermediaria, uf, cbo) para ficar super leve no cache
    m <- rbindlist(pb)[, .(n = sum(n)), by = .(mun, cbo)]
    rm(pb); gc()
    m <- merge(m, dtb[, .(mun, regiao = intermediaria, uf)], by = "mun")[!is.na(regiao)]
    lista_rc[[as.character(a)]] <- m[, .(n = sum(n)), by = .(regiao, uf, cbo)][, ano := a]
    rm(m); gc()
  }

  cr      <- rbindlist(lista_cr); rm(lista_cr); gc()
  reg_cbo <- rbindlist(lista_rc); rm(lista_rc); gc()
  saveRDS(list(cr = cr, reg_cbo = reg_cbo), arq_cache_02)
}

# --- 1) Portabilidade media por regiao intermediaria e UF ---------------------
m_reg <- merge(reg_cbo, base_cbo[, .(cbo, broadness_base, grupo)], by = "cbo")
m_reg <- merge(m_reg, broad[, .(ano, cbo, broadness_corr = broadness)],
               by = c("ano", "cbo"), all.x = TRUE)

pm_inter <- m_reg[, .(
  portab_media      = weighted.mean(broadness_base, n, na.rm = TRUE),
  portab_media_corr = weighted.mean(broadness_corr, n, na.rm = TRUE),
  share_grupo1      = sum(n[grupo == 1]) / sum(n),
  share_grupo2      = sum(n[grupo == 2]) / sum(n),
  share_grupo3      = sum(n[grupo == 3]) / sum(n)
), by = .(ano, regiao)]

pm_uf <- m_reg[, .(
  portab_media      = weighted.mean(broadness_base, n, na.rm = TRUE),
  portab_media_corr = weighted.mean(broadness_corr, n, na.rm = TRUE),
  share_grupo1      = sum(n[grupo == 1]) / sum(n),
  share_grupo2      = sum(n[grupo == 2]) / sum(n),
  share_grupo3      = sum(n[grupo == 3]) / sum(n)
), by = .(ano, regiao = uf)]

pm <- rbind(
   copy(pm_inter)[, nivel := "intermediaria"],
  copy(pm_uf)[, nivel := "uf"]
)
fwrite(pm_inter, file.path(DIR_DADOS, "portabilidade_regiao.csv"))
rm(m_reg); gc()

# --- 2) Emprego, salario e choque setorial local (regiao x divisao CNAE) ------
cr <- merge(cr, dtb[, .(mun, intermediaria, uf)], by = "mun", all.x = TRUE)
p <- cr[is.na(intermediaria), sum(n)]
cat("vinculos sem regiao:", mil(p), "(", round(100 * p / sum(cr$n), 3), "%)\n")
cr <- cr[!is.na(intermediaria)]

sal_inter <- cr[, .(emprego = sum(n), salario = sum(soma_rem) / sum(n), nivel = "intermediaria"),
                by = .(ano, regiao = intermediaria)]
sal_uf    <- cr[, .(emprego = sum(n), salario = sum(soma_rem) / sum(n), nivel = "uf"),
                by = .(ano, regiao = uf)]
sal <- rbind(sal_inter, sal_uf)
fwrite(sal_inter[, .(ano, regiao, emprego, salario)], file.path(DIR_DADOS, "salario_regiao.csv"))

# Choque setorial anual g_{r,s,t} na regiao intermediaria (ponte com o script 03!)
rs_ano <- cr[, .(emp = sum(n)), by = .(regiao = intermediaria, div, ano)]
setorder(rs_ano, regiao, div, ano)
rs_ano[, emp_ant := shift(emp, 1L, type = "lag"), by = .(regiao, div)]
rs_ano[, ano_ant := shift(ano, 1L, type = "lag"), by = .(regiao, div)]
rs_ano[, g_setor_regiao := fifelse(!is.na(emp_ant) & ano == ano_ant + 1L & emp_ant >= 10,
                                   (emp - emp_ant) / emp_ant, NA_real_)]
fwrite(rs_ano[, .(ano, regiao, div, emp, emp_ant, g_setor_regiao)],
       file.path(DIR_DADOS, "choque_setorial_regiao.csv"))
rm(rs_ano); gc()

# --- 3) T, D, F ---------------------------------------------------------------
par_indices <- function(dd, chave, t0, t1) {
  w <- dcast(dd[ano %in% c(t0, t1), .(e = sum(n)), by = c(chave, "div", "ano")],
             as.formula(paste(chave, "+ div ~ ano")), value.var = "e", fill = 0)
  setnames(w, c(as.character(t0), as.character(t1)), c("e0", "e1"))
  w[, `:=`(tot0 = sum(e0), tot1 = sum(e1)), by = c(chave)]
  w <- w[tot0 > 0 & tot1 > 0]
  if (!nrow(w)) return(NULL)
  w[, `:=`(a0 = e0 / tot0, a1 = e1 / tot1)]
  w[, g_s := fifelse(e0 > 0, e1 / e0 - 1, NA_real_)]
  w[, g_j := tot1 / tot0 - 1]
  o <- w[, .(
    emprego_t0 = tot0[1],
    emprego_t1 = tot1[1],
    n_setores  = sum(e0 > 0 | e1 > 0),
    cresc      = g_j[1],
    T_turb     = 0.5 * sum(abs(a1 - a0)),
    D_desc     = sum((g_s - g_j)^2 * a0, na.rm = TRUE)
  ), by = c(chave)]
  o[, `:=`(F_flex = fifelse(D_desc > 0, T_turb / D_desc, NA_real_), t0 = t0, t1 = t1)]
  setnames(o, chave, "regiao")
  o[]
}

pares <- c(lapply(head(ANOS, -1), function(y) c(y, y + 1)),
           lapply(JANELAS, function(J) c(J$t0, J$t1)))
res <- list()
for (nv in c("intermediaria", "uf"))
  for (pp in pares) {
    o <- par_indices(cr, nv, pp[1], pp[2])
    if (!is.null(o)) { o[, nivel := nv]; res[[paste(nv, pp[1], pp[2])]] <- o }
  }
flex <- rbindlist(res, use.names = TRUE)

nomes_inter <- unique(dtb[, .(nivel = "intermediaria", regiao = intermediaria, nome_inter, nome_uf)])
nomes_uf    <- unique(dtb[, .(nivel = "uf", regiao = uf, nome_inter = nome_uf, nome_uf)])
nomes_reg   <- rbind(nomes_inter, nomes_uf)

flex <- merge(flex, nomes_reg, by = c("nivel", "regiao"), all.x = TRUE)
flex <- merge(flex, pm[, .(nivel, regiao, t0 = ano, portab_media, portab_media_corr,
                           share_grupo1, share_grupo2, share_grupo3)],
              by = c("nivel", "regiao", "t0"), all.x = TRUE)
flex <- merge(flex, sal[, .(nivel, regiao, t0 = ano, salario)],
              by = c("nivel", "regiao", "t0"), all.x = TRUE)
setorder(flex, nivel, t0, regiao)
fwrite(flex, file.path(DIR_DADOS, "flexibilidade.csv"))

print(flex[t1 == t0 + 1, .(
  regioes   = .N,
  T_med     = round(mean(T_turb), 4),
  D_med     = round(mean(D_desc), 5),
  F_mediana = round(median(F_flex, na.rm = TRUE), 1)
), by = nivel])


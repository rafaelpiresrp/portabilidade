# =============================================================================
# 09_escada_decomposicao.R
#
# Duas verificacoes da escada em regressao (07), no painel B (resto da regiao
# crescendo), com G1 e G3 das 6 coortes e salario de origem positivo:
#
# (a) Decomposicao, no degrau de queda relativa >= 10 pp (amostra da figura do 08):
#     a diferenca G1 - G3 com uma peca da Equacao (1) de cada vez.
#       1 sem controles
#       2 controles individuais (log_rem0, idade, tempo, sexo, escolaridade)
#       3 + EF coorte
#       4 + EF coorte e divisao CNAE
#       5 + EF coorte e municipio (sem divisao)
#       6 + EF coorte, divisao e municipio (= especificacao do 07)
#
# (b) Interacao, na amostra toda do painel B: testa se a diferenca G1 - G3 cresce
#     com a queda relativa do setor, numa regressao so.
#       faixas: Y = G1 + G1 x faixa + faixa + controles | div + mun + coorte
#               (faixas do 06; referencia: setor que cresceu ao menos tanto quanto
#               o resto da regiao)
#       binaria: Y = G1 + G1 x 1[queda >= 10 pp] + 1[queda >= 10 pp] + controles | ...
#
# Y: voltou em ate 6 e 12 meses; variacao de log salario (so quem voltou).
# Erro-padrao clusterizado por divisao CNAE. Coeficientes x 100.
#
# Saidas: saidas/dados/escada_decomposicao.csv e escada_interacao.csv
# =============================================================================
source("config.R")
library(data.table)
library(fixest)

# 1. Choque do setor vs. resto da regiao (mesmo calculo do 06) ----------------
cat("1) Calculando choques do setor vs. resto da regiao...\n")
ch <- fread(file.path(DIR_DADOS, "choque_setorial_regiao.csv"),
            colClasses = c(regiao = "character", div = "character"))
ch[, `:=`(emp_reg_tot     = sum(emp, na.rm = TRUE),
          emp_ant_reg_tot = sum(emp_ant, na.rm = TRUE)), by = .(regiao, ano)]
ch[, `:=`(emp_outros     = emp_reg_tot - emp,
          emp_ant_outros = emp_ant_reg_tot - emp_ant)]
ch[, g_outros := fifelse(emp_ant_outros >= 50,
                         (emp_outros - emp_ant_outros) / emp_ant_outros, NA_real_)]
ch[, dif_setor_regiao := g_setor_regiao - g_outros]

# 2. Le as 6 coortes: so G1 e G3, regioes cujo resto cresceu ------------------
cat("2) Lendo as 6 coortes...\n")
colunas <- c("grupo", "meses", "voltou", "div", "mun", "regiao",
             "grauinstr", "sexo", "idade", "tempo", "log_rem0", "d_log_rem")
partes <- list()
for (k in names(COORTES)) {
  arq_k <- file.path(DIR_DADOS, paste0("duracao_", k, ".csv"))
  if (!file.exists(arq_k)) next
  C <- COORTES[[k]]
  cat("   lendo", k, "(", C$ano, ")...\n")

  d <- fread(arq_k, select = colunas,
             colClasses = c(div = "character", mun = "character", regiao = "character"))
  d <- d[grupo %in% c(1L, 3L) & !is.na(log_rem0)]

  ch_ano <- unique(ch[ano == C$ano, .(regiao, div, g_setor_regiao, g_outros, dif_setor_regiao)],
                   by = c("regiao", "div"))
  d[ch_ano, `:=`(g_setor  = i.g_setor_regiao,
                 g_outros = i.g_outros,
                 dif_sr   = i.dif_setor_regiao), on = c("regiao", "div")]
  d <- d[!is.na(g_setor) & !is.na(g_outros) & g_outros > 0]
  d[, `:=`(coorte = C$ano, regiao = NULL, g_setor = NULL, g_outros = NULL)]

  partes[[k]] <- d
  rm(d); gc()
}
d <- rbindlist(partes)
rm(partes, ch); gc()

# Variaveis (NA conta como "nao voltou", como no 06) e faixas do 06
d[, `:=`(
  g1       = as.integer(grupo == 1L),
  r6       = as.integer((voltou == 1L & meses <=  6) %in% TRUE),
  r12      = as.integer((voltou == 1L & meses <= 12) %in% TRUE),
  queda10  = as.integer(dif_sr <= -0.10),
  faixa    = fifelse(dif_sr <= -0.20, "1_Queda_Idio_>=20pp",
             fifelse(dif_sr <= -0.15, "2_Queda_Idio_15a20pp",
             fifelse(dif_sr <= -0.10, "3_Queda_Idio_10a15pp",
             fifelse(dif_sr <= -0.05, "4_Queda_Idio_5a10pp",
             fifelse(dif_sr <   0.00, "5_Queda_Idio_0a5pp",
                                      "6_Setor_Acima_da_Regiao")))))
)]
d[, c("voltou", "meses", "grupo") := NULL]
cat("   observacoes (G1 e G3, resto da regiao crescendo):", format(nrow(d), big.mark = ","), "\n")

estima <- function(...) tryCatch(feols(...), error = function(e) {
  message("   falhou: ", conditionMessage(e)); NULL
})

coefs <- function(m, termos) {
  if (is.null(m)) return(NULL)
  ms <- if (inherits(m, "fixest_multi")) as.list(m) else list(m)
  rbindlist(lapply(ms, function(x) {
    ct <- coeftable(x)
    tt <- intersect(grep(termos, rownames(ct), value = TRUE), rownames(ct))
    data.table(y = as.character(x$fml[[2]]), termo = tt,
               b = 100 * ct[tt, 1], se = 100 * ct[tt, 2], n = nobs(x))
  }))
}

ctrl <- "g1 + log_rem0 + idade + tempo + factor(sexo) + factor(grauinstr)"

# 3. (a) Decomposicao no degrau >= 10 pp ----------------------------------------
cat("3) Decomposicao (queda relativa >= 10 pp)...\n")
s10 <- d$dif_sr <= -0.10
passos <- list(
  "1_sem_controles"            = c("g1",  ""),
  "2_controles_individuais"    = c(ctrl,  ""),
  "3_+EF_coorte"               = c(ctrl,  "coorte"),
  "4_+EF_coorte_divisao"       = c(ctrl,  "coorte + div"),
  "5_+EF_coorte_municipio"     = c(ctrl,  "coorte + mun"),
  "6_+EF_coorte_div_municipio" = c(ctrl,  "coorte + div + mun")
)
dec <- list()
for (p in names(passos)) {
  rhs <- passos[[p]][1]; fe <- passos[[p]][2]
  f_tx  <- as.formula(paste("c(r6, r12) ~", rhs, if (fe != "") paste("|", fe) else ""))
  f_sal <- as.formula(paste("d_log_rem ~", rhs, if (fe != "") paste("|", fe) else ""))
  cat("   ", p, "\n")
  m_tx  <- estima(f_tx,  d, subset = s10, cluster = ~div)
  m_sal <- estima(f_sal, d, subset = s10 & !is.na(d$d_log_rem), cluster = ~div)
  dec[[p]] <- rbind(coefs(m_tx, "^g1$"), coefs(m_sal, "^g1$"))[, passo := p]
  rm(m_tx, m_sal); gc()
}
dec <- rbindlist(dec)
fwrite(dec, file.path(DIR_DADOS, "escada_decomposicao.csv"))

# 4. (b) Interacao na amostra toda do painel B -----------------------------------
cat("4) Interacao G1 x queda relativa (painel B inteiro)...\n")
fe_full <- "| div + mun + coorte"
inter <- "+ i(faixa, g1, ref = '6_Setor_Acima_da_Regiao') + i(faixa, ref = '6_Setor_Acima_da_Regiao')"
f_fx_tx  <- as.formula(paste("c(r6, r12) ~", ctrl, inter, fe_full))
f_fx_sal <- as.formula(paste("d_log_rem ~", ctrl, inter, fe_full))
f_bi_tx  <- as.formula(paste("c(r6, r12) ~", ctrl, "+ g1:queda10 + queda10", fe_full))
f_bi_sal <- as.formula(paste("d_log_rem ~", ctrl, "+ g1:queda10 + queda10", fe_full))

termos <- "^g1$|^faixa::.*:g1$|^g1:queda10$"
int <- list()
m <- estima(f_fx_tx,  d, cluster = ~div);                          int[["faixas_tx"]]  <- coefs(m, termos); rm(m); gc()
m <- estima(f_fx_sal, d, subset = !is.na(d$d_log_rem), cluster = ~div); int[["faixas_sal"]] <- coefs(m, termos); rm(m); gc()
m <- estima(f_bi_tx,  d, cluster = ~div);                          int[["binaria_tx"]]  <- coefs(m, termos); rm(m); gc()
m <- estima(f_bi_sal, d, subset = !is.na(d$d_log_rem), cluster = ~div); int[["binaria_sal"]] <- coefs(m, termos); rm(m); gc()
int <- rbindlist(int, idcol = "modelo")
int[, modelo := sub("_(tx|sal)$", "", modelo)]
fwrite(int, file.path(DIR_DADOS, "escada_interacao.csv"))

# 5. Resumo ------------------------------------------------------------------------
cat("\n=========================================================================\n")
cat("(a) DECOMPOSICAO, queda relativa >= 10 pp: coeficiente de G1 (x 100)\n")
cat("=========================================================================\n")
dec[, est := sprintf("%.2f (%.2f)", b, se)]
print(dcast(dec, passo ~ y, value.var = "est"))
cat("\n=========================================================================\n")
cat("(b) INTERACAO: G1 na referencia e G1 x faixa (x 100)\n")
cat("=========================================================================\n")
int[, est := sprintf("%.2f (%.2f)", b, se)]
print(dcast(int, modelo + termo ~ y, value.var = "est"))
cat("\nConcluido:", file.path(DIR_DADOS, "escada_decomposicao.csv"), "e escada_interacao.csv\n")

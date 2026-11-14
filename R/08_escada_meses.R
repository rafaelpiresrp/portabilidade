# =============================================================================
# 08_escada_meses.R
#
# Versao ajustada da Figura de Kaplan-Meier do choque idiossincratico (06):
# diferenca G1 - G3 na fracao que ja voltou ao emprego formal, mes a mes
# (1 a 24 meses apos a demissao), com os mesmos controles e efeitos fixos do 07.
#
# Amostras (G1 e G3 das 6 coortes, resto da regiao crescendo):
#   - "2_Queda_Relativa_>=10pp": mesma amostra da curva de Kaplan-Meier do 06
#   - "0_Todos_Demitidos": referencia, sem condicao sobre a queda do setor
#
# Em cada mes k:
#   1[voltou ate k]_i = theta_k * G1_i + log_rem0 + idade + tempo + sexo + escolaridade
#                       + EF(divisao CNAE) + EF(municipio) + EF(coorte) + e_i
# com erro-padrao clusterizado por divisao CNAE.
#
# Ate 24 meses todos os demitidos sao observados (a coorte de 2021 e seguida ate
# dez/2023), entao nao ha censura e a especificacao "bruto" (sem controles)
# reproduz a diferenca entre as curvas de Kaplan-Meier do 06.
#
# Saida: saidas/dados/escada_meses.csv (coeficientes x 100, pontos percentuais).
# =============================================================================
source("config.R")
library(data.table)
library(fixest)

MESES <- 1:24

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
             "grauinstr", "sexo", "idade", "tempo", "log_rem0")
partes <- list()
for (k in names(COORTES)) {
  arq_k <- file.path(DIR_DADOS, paste0("duracao_", k, ".csv"))
  if (!file.exists(arq_k)) next
  C <- COORTES[[k]]
  cat("   lendo", k, "(", C$ano, ")...\n")

  d <- fread(arq_k, select = colunas,
             colClasses = c(div = "character", mun = "character", regiao = "character"))
  d <- d[grupo %in% c(1L, 3L)]

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

# Voltou ate o mes k (NA conta como "nao voltou", como no 06)
d[, g1 := as.integer(grupo == 1L)]
for (k in MESES) set(d, j = paste0("r", k), value = as.integer((d$voltou == 1L & d$meses <= k) %in% TRUE))
d[, c("voltou", "meses", "grupo") := NULL]
cat("   observacoes (G1 e G3, resto da regiao crescendo):", format(nrow(d), big.mark = "."), "\n")

# 3. Regressoes mes a mes ------------------------------------------------------
amostras <- list(
  "2_Queda_Relativa_>=10pp" = d$dif_sr <= -0.10,
  "0_Todos_Demitidos"       = rep(TRUE, nrow(d))
)
lhs <- paste0("c(", paste0("r", MESES, collapse = ", "), ")")
fml_bruto <- as.formula(paste(lhs, "~ g1"))
fml_ctrl  <- as.formula(paste(lhs, "~ g1 + log_rem0 + idade + tempo + factor(sexo) + factor(grauinstr) | div + mun + coorte"))

estima <- function(...) tryCatch(feols(...), error = function(e) {
  message("   falhou: ", conditionMessage(e)); NULL
})

extrai <- function(m, amostra, spec) {
  if (is.null(m)) return(NULL)
  rbindlist(lapply(as.list(m), function(x) {
    y   <- as.character(x$fml[[2]])
    obs <- obs(x)
    data.table(amostra = amostra, spec = spec, mes = as.integer(sub("r", "", y)),
               b        = 100 * unname(coef(x)["g1"]),
               se       = 100 * unname(se(x)["g1"]),
               n        = nobs(x),
               media_g3 = 100 * mean(d[[y]][obs][d$g1[obs] == 0L]))
  }))
}

res <- list()
for (a in names(amostras)) {
  s <- amostras[[a]]
  s[is.na(s)] <- FALSE
  cat("3)", a, "|", format(sum(s), big.mark = "."), "obs\n")
  m_b <- estima(fml_bruto, d, subset = s, cluster = ~div)
  res[[length(res) + 1]] <- extrai(m_b, a, "bruto"); rm(m_b); gc()
  m_c <- estima(fml_ctrl, d, subset = s & !is.na(d$log_rem0), cluster = ~div)
  res[[length(res) + 1]] <- extrai(m_c, a, "controles_EF"); rm(m_c); gc()
}
tab <- rbindlist(res)
fwrite(tab, file.path(DIR_DADOS, "escada_meses.csv"))

# 4. Resumo e conferencia com o Kaplan-Meier do 06 -------------------------------
cat("\n=========================================================================\n")
cat("DIFERENCA G1 - G3 MES A MES (x 100) E ERRO-PADRAO\n")
cat("=========================================================================\n")
tab[, est := sprintf("%.2f (%.2f)", b, se)]
print(dcast(tab, mes ~ amostra + spec, value.var = "est"))

arq_km <- file.path(DIR_DADOS, "km_choque_idiossincratico_comparado.csv")
if (file.exists(arq_km)) {
  km <- fread(arq_km)[recorte == "Choque_Idio_Bruto" & grupo %in% c(1L, 3L) & mes %in% MESES]
  km <- dcast(km, mes ~ grupo, value.var = "voltou_ate")
  km[, dif_km := `1` - `3`]
  conf <- merge(km[, .(mes, dif_km)],
                tab[amostra == "2_Queda_Relativa_>=10pp" & spec == "bruto", .(mes, bruto = round(b, 2))],
                by = "mes")
  cat("\nConferencia: 'bruto' vs. diferenca do Kaplan-Meier do 06 (devem coincidir ate ~0,01)\n")
  print(conf[, dif := round(bruto - dif_km, 3)])
}
cat("\nConcluido:", file.path(DIR_DADOS, "escada_meses.csv"), "\n")

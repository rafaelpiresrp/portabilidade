# =============================================================================
# 07_escada_regressao.R
#
# Versao em regressao da escada de choque idiossincratico do 06.
# Usa os mesmos degraus (queda relativa do setor >= 5, 10, 15 e 20 pp), as mesmas
# amostras dos paineis B (resto da regiao crescendo) e C (B e tempo < 6 meses,
# sem seguro-desemprego) e os mesmos controles da Equacao (1) do artigo
# (04_tabelas.R, especificacao "EF + ln w0").
#
# Em cada degrau, com G1 e G3 das 6 coortes empilhados, estima por MQO:
#   Y_i = theta * G1_i + log_rem0 + idade + tempo + sexo + escolaridade
#         + EF(divisao CNAE) + EF(municipio) + EF(coorte) + e_i
# com erro-padrao clusterizado por divisao CNAE. theta e a diferenca G1 - G3
# condicional aos controles.
#
# Y: voltou em ate 6, 12 e 24 meses; voltou em outro setor em ate 12 meses;
#    variacao de log salario (so quem voltou).
#
# A especificacao "bruto" (Y ~ G1, sem controles, amostra completa) reproduz as
# diferencas G1 - G3 do escada_choque_idiossincratico.csv e serve de conferencia.
#
# Saida: saidas/dados/escada_regressao.csv (coeficientes x 100: pontos
# percentuais nas taxas de retorno e pontos de log no salario).
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
colunas <- c("grupo", "meses", "voltou", "mudou_setor", "div", "mun", "regiao",
             "grauinstr", "sexo", "idade", "tempo", "log_rem0", "d_log_rem")
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

# Variaveis dependentes (NA conta como "nao voltou", como nas somas do 06)
d[, `:=`(
  g1      = as.integer(grupo == 1L),
  r6      = as.integer((voltou == 1L & meses <=  6) %in% TRUE),
  r12     = as.integer((voltou == 1L & meses <= 12) %in% TRUE),
  r24     = as.integer((voltou == 1L & meses <= 24) %in% TRUE),
  outro12 = as.integer((voltou == 1L & meses <= 12 & mudou_setor == 1L) %in% TRUE)
)]
cat("   observacoes (G1 e G3, resto da regiao crescendo):", format(nrow(d), big.mark = "."), "\n")

# 3. Regressoes por painel e degrau --------------------------------------------
paineis <- list(
  "B_Regiao_Crescendo"             = rep(TRUE, nrow(d)),
  "C_Sem_Seguro (<6m) + Crescendo" = d$tempo < 6
)
cortes <- c("0_Todos_Demitidos"       =  Inf,
            "1_Queda_Relativa_>=5pp"  = -0.05,
            "2_Queda_Relativa_>=10pp" = -0.10,
            "3_Queda_Relativa_>=15pp" = -0.15,
            "4_Queda_Relativa_>=20pp" = -0.20)

ctrl <- "g1 + log_rem0 + idade + tempo + factor(sexo) + factor(grauinstr) | div + mun + coorte"
fml_taxas <- as.formula(paste("c(r6, r12, r24, outro12) ~", ctrl))
fml_sal   <- as.formula(paste("d_log_rem ~", ctrl))

# Se um modelo falhar, registra o erro e segue para o proximo (nao perde a rodada)
estima <- function(...) tryCatch(feols(...), error = function(e) {
  message("   falhou: ", conditionMessage(e)); NULL
})

extrai <- function(m, painel, corte, spec) {
  if (is.null(m)) return(NULL)
  ms <- if (inherits(m, "fixest_multi")) as.list(m) else list(m)
  rbindlist(lapply(ms, function(x) {
    y   <- as.character(x$fml[[2]])
    obs <- obs(x)                                  # linhas de d usadas no modelo
    data.table(painel = painel, corte = corte, spec = spec, y = y,
               b        = 100 * unname(coef(x)["g1"]),
               se       = 100 * unname(se(x)["g1"]),
               n        = nobs(x),
               media_g3 = 100 * mean(d[[y]][obs][d$g1[obs] == 0L]))
  }))
}

res <- list()
for (p in names(paineis)) {
  for (ct in names(cortes)) {
    s <- paineis[[p]] & (is.infinite(cortes[[ct]]) | d$dif_sr <= cortes[[ct]])
    s[is.na(s)] <- FALSE
    cat("3)", p, "|", ct, "|", format(sum(s), big.mark = "."), "obs\n")

    # Bruto: reproduz a escada descritiva do 06
    m_b  <- estima(c(r6, r12, r24, outro12) ~ g1, d, subset = s, cluster = ~div)
    m_bs <- estima(d_log_rem ~ g1, d, subset = s & !is.na(d$d_log_rem), cluster = ~div)

    # Controles da Equacao (1) e efeitos fixos de divisao, municipio e coorte
    s_w  <- s & !is.na(d$log_rem0)
    m_c  <- estima(fml_taxas, d, subset = s_w, cluster = ~div)
    m_cs <- estima(fml_sal,   d, subset = s_w & !is.na(d$d_log_rem), cluster = ~div)

    res[[length(res) + 1]] <- extrai(m_b, p, ct, "bruto")
    res[[length(res) + 1]] <- extrai(m_bs, p, ct, "bruto")
    res[[length(res) + 1]] <- extrai(m_c, p, ct, "controles_EF")
    res[[length(res) + 1]] <- extrai(m_cs, p, ct, "controles_EF")
    rm(m_b, m_bs, m_c, m_cs); gc()
  }
}
tab <- rbindlist(res)
fwrite(tab, file.path(DIR_DADOS, "escada_regressao.csv"))

# 4. Resumo e conferencia com a escada descritiva --------------------------------
cat("\n=========================================================================\n")
cat("ESCADA EM REGRESSAO: coeficiente de G1 (x 100) e erro-padrao\n")
cat("=========================================================================\n")
tab[, est := sprintf("%.2f (%.2f)", b, se)]
print(dcast(tab, painel + corte ~ spec + y, value.var = "est"))

arq_esc <- file.path(DIR_DADOS, "escada_choque_idiossincratico.csv")
if (file.exists(arq_esc)) {
  esc <- fread(arq_esc)
  esc[, painel := fifelse(grepl("^B_", Bloco), "B_Regiao_Crescendo",
                  fifelse(grepl("^C_", Bloco), "C_Sem_Seguro (<6m) + Crescendo", NA_character_))]
  conf <- merge(esc[!is.na(painel), .(painel, corte, Dif_6m_G1_G3, Dif_12m_G1_G3, Dif_Outro12m)],
                dcast(tab[spec == "bruto" & y %in% c("r6", "r12", "outro12")],
                      painel + corte ~ y, value.var = "b"),
                by = c("painel", "corte"))
  conf[, dif_max := pmax(abs(Dif_6m_G1_G3 - r6), abs(Dif_12m_G1_G3 - r12),
                         abs(Dif_Outro12m - outro12))]
  cat("\nConferencia: 'bruto' vs. escada do 06 (diferencas devem ser < 0,01)\n")
  print(conf[, .(painel, corte, Dif_12m_G1_G3, r12 = round(r12, 2), dif_max = round(dif_max, 3))])
}
cat("\nConcluido:", file.path(DIR_DADOS, "escada_regressao.csv"), "\n")

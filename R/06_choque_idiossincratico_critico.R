# =============================================================================
# 06_choque_idiossincratico_critico.R
#
# Testa onde os mais portateis (G1) voltam mais rapido que os menos portateis (G3):
#   1) Escada de severidade do choque idiossincratico no setor (quando o resto
#      da regiao cresce, g_outros > 0, mas o setor do trabalhador cai >= 5%,
#      >= 10%, >= 15%, >= 20%);
#   2) Remocao do vies de composicao (Seguro-Desemprego e Salario de Reserva):
#      - Subamostra homogenea (sem seguro-desemprego, tempo < 6m, e salario 1-2 SM);
#      - Reponderacao exata por celulas de (faixa salarial inicial x faixa de tenure),
#        comparando G1, G2 e G3 como se tivessem o MESMO salario e tenure iniciais!
#   3) Velocidade de retorno por canal (Competing Risks: retorno em outro setor
#      vs. mesmo setor);
#   4) Exportacao de todas as bases leves para Python em saidas/dados_python/.
# =============================================================================
source("config.R")
library(data.table)
library(survival)

DIR_PY <- file.path(RAIZ, "saidas", "dados_python")
dir.create(DIR_PY, showWarnings = FALSE, recursive = TRUE)

# 1. Calcula o choque do setor (g_setor_regiao) e o crescimento do resto da regiao (g_outros)
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
fwrite(ch, file.path(DIR_DADOS, "choques_decompostos_regiao_setor.csv"))

# 2. Le as 6 coortes e agrega por celula fina de:
#    (escada de choque x faixa de salario inicial x faixa de tenure x grupo)
cat("2) Processando as 6 coortes nas faixas de choque e composicao inicial...\n")
partes_cel <- list()
partes_km  <- list()

for (k in names(COORTES)) {
  arq_k <- file.path(DIR_DADOS, paste0("duracao_", k, ".csv"))
  if (!file.exists(arq_k)) next
  C <- COORTES[[k]]
  cat("   lendo", k, "(", C$ano, ")...\n")

  d <- fread(arq_k, colClasses = c(cbo = "character", div = "character",
                                   mun = "character", regiao = "character"))
  if (!"mudou_setor" %in% names(d))
    d[, mudou_setor := fifelse(voltou == 1L, as.integer(grepl("outro setor|Outra|familia", destino)), NA_integer_)]

  ch_ano <- unique(ch[ano == C$ano, .(regiao, div, g_setor_regiao, g_outros, dif_setor_regiao)],
                   by = c("regiao", "div"))
  d[ch_ano, `:=`(g_setor = i.g_setor_regiao,
                 g_outros = i.g_outros,
                 dif_sr   = i.dif_setor_regiao), on = c("regiao", "div")]
  d <- d[!is.na(g_setor) & !is.na(g_outros)]

  # Faixas de tenure (elegibilidade ao seguro-desemprego) e salario inicial (em SM)
  d[, fx_tenure := fifelse(tempo < 6,  "1_Sem_Seguro (<6m)",
                   fifelse(tempo < 12, "2_Tenure_6a11m",
                   fifelse(tempo < 24, "3_Tenure_12a23m",
                                       "4_Seguro_Max (>=24m)")))]

  d[, fx_sal0 := fifelse(is.na(rem) | rem <= 0, "0_Sem_Info",
                 fifelse(rem <= 1.25, "1_Ate_1.25_SM",
                 fifelse(rem <= 2.00, "2_1.25_a_2_SM",
                 fifelse(rem <= 3.50, "3_2_a_3.5_SM",
                                      "4_Acima_3.5_SM"))))]

  # Escada de Choque Idiossincratico (varios pontos de corte transparentes):
  # Olha a diferenca (g_setor - g_outros): quanto o setor caiu A MAIS que a regiao
  d[, escada_idio := fifelse(dif_sr <= -0.20, "1_Queda_Idio_>=20pp",
                     fifelse(dif_sr <= -0.15, "2_Queda_Idio_15a20pp",
                     fifelse(dif_sr <= -0.10, "3_Queda_Idio_10a15pp",
                     fifelse(dif_sr <= -0.05, "4_Queda_Idio_5a10pp",
                     fifelse(dif_sr <   0.00, "5_Queda_Idio_0a5pp",
                                              "6_Setor_Acima_da_Regiao")))))]

  # Indica se o resto da regiao estava crescendo (ha vagas fora do setor!)
  d[, regiao_crescendo := as.integer(g_outros > 0)]

  partes_cel[[k]] <- d[, .(
    coorte          = k,
    ano             = C$ano,
    n_tot           = as.numeric(.N),
    n_voltou        = as.numeric(sum(voltou == 1L, na.rm = TRUE)),
    n_6m            = as.numeric(sum(voltou == 1L & meses <=  6, na.rm = TRUE)),
    n_12m           = as.numeric(sum(voltou == 1L & meses <= 12, na.rm = TRUE)),
    n_24m           = as.numeric(sum(voltou == 1L & meses <= 24, na.rm = TRUE)),
    # Retorno rapido VIA OUTRO SETOR (canal de portabilidade intersetorial!)
    n_outro_6m      = as.numeric(sum(voltou == 1L & meses <=  6 & mudou_setor == 1L, na.rm = TRUE)),
    n_outro_12m     = as.numeric(sum(voltou == 1L & meses <= 12 & mudou_setor == 1L, na.rm = TRUE)),
    n_outro_24m     = as.numeric(sum(voltou == 1L & meses <= 24 & mudou_setor == 1L, na.rm = TRUE)),
    # Retorno no MESMO SETOR
    n_mesmo_12m     = as.numeric(sum(voltou == 1L & meses <= 12 & mudou_setor == 0L, na.rm = TRUE)),
    n_mudou_setor   = as.numeric(sum(voltou == 1L & mudou_setor == 1L, na.rm = TRUE)),
    n_migrou        = as.numeric(sum(migrou == 1L, na.rm = TRUE)),
    s_dlog          = sum(d_log_rem, na.rm = TRUE),
    n_dlog          = as.numeric(sum(!is.na(d_log_rem)))
  ), by = .(escada_idio, regiao_crescendo, fx_tenure, fx_sal0, grupo)]

  # Guarda contagem para curva de Kaplan-Meier nos choques idiossincraticos fortes (dif_sr <= -0.10)
  partes_km[[k]] <- d[dif_sr <= -0.10, .(peso = as.numeric(.N)),
                      by = .(regiao_crescendo, fx_tenure, fx_sal0, grupo, meses, voltou)]
  rm(d); gc()
}

cel <- rbindlist(partes_cel)
km_b <- rbindlist(partes_km)

# =============================================================================
# DIAGNOSTICO 1: Escada de Cortes do Choque Idiossincratico (Bruto vs. Resto da Regiao Crescendo)
# =============================================================================
# Mostra cortes cumulativos: Todos, Queda >= 5pp, >= 10pp, >= 15pp, >= 20pp
cortes_lista <- list(
  "0_Todos_Demitidos"       = unique(cel$escada_idio),
  "1_Queda_Relativa_>=5pp"  = c("1_Queda_Idio_>=20pp", "2_Queda_Idio_15a20pp", "3_Queda_Idio_10a15pp", "4_Queda_Idio_5a10pp"),
  "2_Queda_Relativa_>=10pp" = c("1_Queda_Idio_>=20pp", "2_Queda_Idio_15a20pp", "3_Queda_Idio_10a15pp"),
  "3_Queda_Relativa_>=15pp" = c("1_Queda_Idio_>=20pp", "2_Queda_Idio_15a20pp"),
  "4_Queda_Relativa_>=20pp" = c("1_Queda_Idio_>=20pp")
)

monta_escada <- function(dt_in, titulo_bloco) {
   rbindlist(lapply(names(cortes_lista), function(nm_c) {
    sub <- dt_in[escada_idio %in% cortes_lista[[nm_c]]]
    ag <- sub[, .(
      n_tot       = sum(n_tot),
      ret_6m      = round(100 * sum(n_6m)        / sum(n_tot), 2),
      ret_12m     = round(100 * sum(n_12m)       / sum(n_tot), 2),
      ret_24m     = round(100 * sum(n_24m)       / sum(n_tot), 2),
      outro_12m   = round(100 * sum(n_outro_12m) / sum(n_tot), 2),
      mesmo_12m   = round(100 * sum(n_mesmo_12m) / sum(n_tot), 2),
      d_log_rem   = round(sum(s_dlog)            / sum(n_dlog), 4)
    ), by = grupo][order(grupo)]
    w <- dcast(ag[, .(corte = nm_c, grupo, ret_6m, ret_12m, ret_24m, outro_12m)],
               corte ~ grupo, value.var = c("ret_6m", "ret_12m", "ret_24m", "outro_12m"))
    w[, `:=`(
      Bloco           = titulo_bloco,
      Demitidos       = sum(ag$n_tot),
      Dif_6m_G1_G3    = round(ret_6m_1    - ret_6m_3, 2),
      Dif_12m_G1_G3   = round(ret_12m_1   - ret_12m_3, 2),
      Dif_24m_G1_G3   = round(ret_24m_1   - ret_24m_3, 2),
      Dif_Outro12m    = round(outro_12m_1 - outro_12m_3, 2)
    )]
    w[, .(Bloco, corte, Demitidos,
          ret_6m_1, ret_6m_3, Dif_6m_G1_G3,
          ret_12m_1, ret_12m_3, Dif_12m_G1_G3,
          outro_12m_1, outro_12m_3, Dif_Outro12m)]
  }))
}

esc_geral  <- monta_escada(cel, "A_Todas_Regioes")
esc_cresc  <- monta_escada(cel[regiao_crescendo == 1], "B_Regiao_Crescendo (Choque Idio Puro)")
esc_semseg <- monta_escada(cel[fx_tenure == "1_Sem_Seguro (<6m)" & regiao_crescendo == 1],
                           "C_Sem_Seguro_Desemprego (<6m) + Regiao_Crescendo")
esc_sal1   <- monta_escada(cel[fx_sal0 == "1_Ate_1.25_SM" & regiao_crescendo == 1],
                           "D_Mesmo_Salario_Inicial (<=1.25 SM) + Regiao_Crescendo")

tab_escada <- rbindlist(list(esc_geral, esc_cresc, esc_semseg, esc_sal1))
fwrite(tab_escada, file.path(DIR_DADOS, "escada_choque_idiossincratico.csv"))

cat("\n=========================================================================\n")
cat("DIAGNOSTICO 1 e 2: ESCADA DE CORTES DO CHOQUE IDIOSSINCRATICO (G1 vs G3)\n")
cat("  ret_6m_1 / ret_12m_1 = % que voltou (total) em 6m e 12m no G1 (Alta) vs G3 (Baixa)\n")
cat("  outro_12m_1 / 3      = % que voltou EM OUTRO SETOR em 12m no G1 vs G3\n")
cat("=========================================================================\n")
print(tab_escada)

# =============================================================================
# DIAGNOSTICO 3: Reponderacao Exata (DFL) — G1, G2 e G3 com o MESMO Salario e Tenure!
# =============================================================================
# Para eliminar o efeito "G1 ganha mais e tem mais seguro-desemprego", aplicamos
# os pesos da distribuicao de (fx_tenure x fx_sal0) do Grupo 1 sobre os Grupos 2 e 3!
pesos_g1 <- cel[fx_sal0 != "0_Sem_Info", .(
  w_g1 = sum(n_tot[grupo == 1])
), by = .(escada_idio, regiao_crescendo, fx_tenure, fx_sal0)]

cel_p <- merge(cel[fx_sal0 != "0_Sem_Info"], pesos_g1,
               by = c("escada_idio", "regiao_crescendo", "fx_tenure", "fx_sal0"))
cel_p[, `:=`(
  tx_6m       = n_6m / n_tot,
  tx_12m      = n_12m / n_tot,
  tx_24m      = n_24m / n_tot,
  tx_outro12m = n_outro_12m / n_tot,
  dlog_m      = fifelse(n_dlog > 0, s_dlog / n_dlog, NA_real_)
)]

esc_dfl <- rbindlist(lapply(names(cortes_lista), function(nm_c) {
  sub <- cel_p[regiao_crescendo == 1 & escada_idio %in% cortes_lista[[nm_c]]]
  ag <- sub[, .(
    ret_6m_ajust    = round(100 * weighted.mean(tx_6m,       w_g1, na.rm = TRUE), 2),
    ret_12m_ajust   = round(100 * weighted.mean(tx_12m,      w_g1, na.rm = TRUE), 2),
    ret_24m_ajust   = round(100 * weighted.mean(tx_24m,      w_g1, na.rm = TRUE), 2),
    outro_12m_ajust = round(100 * weighted.mean(tx_outro12m, w_g1, na.rm = TRUE), 2),
    dlog_ajust      = round(weighted.mean(dlog_m,            w_g1, na.rm = TRUE), 4)
  ), by = grupo][order(grupo)]
  w <- dcast(ag[, .(corte = nm_c, grupo, ret_6m_ajust, ret_12m_ajust, ret_24m_ajust, dlog_ajust)],
             corte ~ grupo, value.var = c("ret_6m_ajust", "ret_12m_ajust", "ret_24m_ajust", "dlog_ajust"))
  w[, `:=`(
    Dif_6m_G1_G3  = round(ret_6m_ajust_1  - ret_6m_ajust_3, 2),
    Dif_12m_G1_G3 = round(ret_12m_ajust_1 - ret_12m_ajust_3, 2),
    Dif_24m_G1_G3 = round(ret_24m_ajust_1 - ret_24m_ajust_3, 2),
    Dif_dlogw     = round(dlog_ajust_1    - dlog_ajust_3, 4)
  )]
  w[, .(corte,
        ret_6m_ajust_1, ret_6m_ajust_3, Dif_6m_G1_G3,
        ret_12m_ajust_1, ret_12m_ajust_3, Dif_12m_G1_G3,
        dlog_ajust_1, dlog_ajust_3, Dif_dlogw)]
}))
fwrite(esc_dfl, file.path(DIR_DADOS, "escada_choque_reponderado_DFL.csv"))

cat("\n=========================================================================\n")
cat("DIAGNOSTICO 3: REPONDERADO PARA MESMO SALARIO INICIAL E MESMO TENURE (DFL)\n")
cat("  (Compara G1 e G3 sob a MESMA distribuicao de salario inicial e seguro-desemprego!)\n")
cat("=========================================================================\n")
print(esc_dfl)

# Salva tambem curvas de Kaplan-Meier (bruta e para mesmo salario/sem seguro) no choque idiossincratico
km_cresc <- km_b[regiao_crescendo == 1, .(peso = sum(peso)), by = .(grupo, meses, voltou)]
sf1 <- survfit(Surv(meses, voltou) ~ grupo, data = km_cresc, weights = peso)
st1 <- rep(names(sf1$strata), sf1$strata)
p1  <- data.table(grupo = as.integer(sub(".*=", "", st1)), mes = sf1$time, surv = sf1$surv)
setorder(p1, grupo, mes)
c1  <- p1[CJ(grupo = 1:3, mes = 0:36), on = .(grupo, mes), roll = TRUE]
c1[is.na(surv), surv := 1]; c1[, `:=`(voltou_ate = round(100 * (1 - surv), 2), recorte = "Choque_Idio_Bruto")]

km_homog <- km_b[regiao_crescendo == 1 & fx_sal0 %in% c("1_Ate_1.25_SM", "2_1.25_a_2_SM") &
                 fx_tenure == "1_Sem_Seguro (<6m)", .(peso = sum(peso)), by = .(grupo, meses, voltou)]
sf2 <- survfit(Surv(meses, voltou) ~ grupo, data = km_homog, weights = peso)
st2 <- rep(names(sf2$strata), sf2$strata)
p2  <- data.table(grupo = as.integer(sub(".*=", "", st2)), mes = sf2$time, surv = sf2$surv)
setorder(p2, grupo, mes)
c2  <- p2[CJ(grupo = 1:3, mes = 0:36), on = .(grupo, mes), roll = TRUE]
c2[is.na(surv), surv := 1]; c2[, `:=`(voltou_ate = round(100 * (1 - surv), 2), recorte = "Choque_Idio_Homogeneo_SemSeguro_Ate2SM")]

fwrite(rbind(c1, c2), file.path(DIR_DADOS, "km_choque_idiossincratico_comparado.csv"))

# =============================================================================
# Exporta todos os CSVs leves para saidas/dados_python/
# =============================================================================
arqs_py <- list.files(DIR_DADOS, pattern = "\\.csv$")
arqs_py <- arqs_py[!grepl("^duracao_c", arqs_py)]   # exclui apenas os 6 CSVs gigantes individuais
for (f in arqs_py) {
  file.copy(file.path(DIR_DADOS, f), file.path(DIR_PY, f), overwrite = TRUE)
}
for (f_ext in c(ARQ_CBO, ARQ_GG)) {
  if (file.exists(f_ext)) file.copy(f_ext, file.path(DIR_PY, basename(f_ext)), overwrite = TRUE)
}
cat("\nConcluido!", length(arqs_py), "arquivos CSV leves copiados para:", DIR_PY, "\n")
# =============================================================================
# config.R  -  caminhos, parametros e classificacao fixa no ano-base (2010).
# Unico arquivo de configuracao a editar.
# =============================================================================
RAIZ    <- "D:/Pessoal/RPires_LMongold/rafaelpires/portabilidade"
DIR_CSV <- "D:/Bases/MTE/Vinc/arquivos-csv/"

DIR_EXT   <- file.path(RAIZ, "dados", "externos")
DIR_DADOS <- file.path(RAIZ, "saidas", "dados")
DIR_TAB   <- file.path(RAIZ, "saidas", "tabelas")
DIR_FIG   <- file.path(RAIZ, "saidas", "figuras")
for (d in c(DIR_EXT, DIR_DADOS, DIR_TAB, DIR_FIG))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

ANOS     <- 2009:2023
ANO_BASE <- 2010     # ano-corte base (censo/pre-crises) que fixa indice e grupos
N_GRUPOS <- 3        # 3 tercis de emprego em ANO_BASE (1 = mais portatil)

# Rotulos padronizados dos 3 grupos de portabilidade
ROTULOS_GRUPOS <- c(
  "1" = "Alta portabilidade (G1)",
  "2" = "Média portabilidade (G2)",
  "3" = "Baixa portabilidade (G3)"
)
ROTULOS_CURTOS <- c(
  "1" = "1 - Alta",
  "2" = "2 - Média",
  "3" = "3 - Baixa"
)

# --- flexibilidade alocativa (02) --------------------------------------------
JANELAS <- list(
  recessao = list(t0 = 2014, t1 = 2019, rotulo = "Recessão (2014–2019)"),
  pandemia = list(t0 = 2019, t1 = 2022, rotulo = "Pandemia (2019–2022)")
)

# --- coortes de demitidos (03) -----------------------------------------------
COORTES <- list(
  c2014 = list(ano = 2014, ate = 2023, fase = "Véspera da recessão"),
  c2015 = list(ano = 2015, ate = 2023, fase = "Recessão"),
  c2016 = list(ano = 2016, ate = 2023, fase = "Recessão"),
  c2019 = list(ano = 2019, ate = 2023, fase = "Véspera da pandemia"),
  c2020 = list(ano = 2020, ate = 2023, fase = "Pandemia"),
  c2021 = list(ano = 2021, ate = 2023, fase = "Pandemia")
)

HORIZ_COMUM <- 24    # meses do horizonte comum a todas as coortes (2 anos)
HORIZ_LONGO <- 84    # meses do horizonte longo para coortes de 2014-2016 (7 anos)

ARQ_DTB <- file.path(DIR_EXT, "RELATORIO_DTB_BRASIL_MUNICIPIO.xls")
ARQ_CBO <- file.path(DIR_EXT, "CBO2002 - Ocupacao.csv")
ARQ_GG  <- file.path(DIR_EXT, "CBO2002 - Grande Grupo.csv")

# =============================================================================
# Funcao auxiliar: classifica cada ocupacao (CBO) de forma FIXA no ANO_BASE
# =============================================================================
# Evita dois problemas metodologicos:
#   1) Ocupacoes trocando de grupo ao longo do tempo;
#   2) Endogeneidade ao usar a broadness de um ano de crise (ex.: 2016) como
#      regressor na propria coorte demitida em 2016.
# Cada CBO recebe a broadness_base e o grupo (1 = Alta, 2 = Media, 3 = Baixa)
# definidos pelos tercis ponderados por vinculos em ANO_BASE (2010). Se alguma
# CBO rara nao existia em ANO_BASE, usa-se o primeiro ano em que ela aparece.
monta_base_cbo <- function(broad, ano_base = ANO_BASE, n_grupos = N_GRUPOS) {
  b0 <- broad[ano == ano_base][order(broadness)]
  if (!nrow(b0))
    stop("ANO_BASE (", ano_base, ") sem ocupacoes em broadness.csv.")
  b0[, acum := cumsum(vinculos) / sum(vinculos)]
  cortes <- sapply(seq_len(n_grupos - 1) / n_grupos,
                   function(q) b0[acum >= q, broadness][1])
  if (anyNA(cortes)) stop("Cortes com NA em ANO_BASE = ", ano_base)

  # Prioriza o registro de ANO_BASE; se ausente, pega a primeira aparicao da CBO
  b_ord <- broad[order(cbo, ano != ano_base, ano)]
  base_cbo <- unique(b_ord, by = "cbo")[, .(
    cbo,
    ano_ref        = ano,
    broadness_base = broadness,
    vinculos_base  = vinculos,
    n_setores_base = n_setores
  )]
  base_cbo[, grupo := as.integer(as.character(
    cut(broadness_base, c(-Inf, cortes, Inf), labels = rev(seq_len(n_grupos)))
  ))]
  attr(base_cbo, "cortes") <- cortes
  base_cbo[]
}

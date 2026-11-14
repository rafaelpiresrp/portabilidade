# Le os resultados dos scripts 06 a 09 e escreve as tabelas da escada de choque
# idiossincratico (tab/) e os dados da figura mes a mes (fig/dados). Nao calcula
# nada sobre microdados: so formata o que os scripts em R ja salvaram.
#
# Uso: python3 scripts/gera_escada_regressao.py ../saidas/dados tab fig/dados
import sys
from pathlib import Path

import pandas as pd
from scipy import stats

DADOS, TAB, FIGD = (Path(a) for a in sys.argv[1:4])

# Erro-padrao clusterizado por divisao CNAE: valor-p pela t com G - 1 graus de
# liberdade, como no fixest (87 divisoes).
GL = 86
T975 = stats.t.ppf(0.975, GL)
DEGRAUS = ["0_Todos_Demitidos", "1_Queda_Relativa_>=5pp", "2_Queda_Relativa_>=10pp",
           "3_Queda_Relativa_>=15pp", "4_Queda_Relativa_>=20pp"]
ROT_DEGRAU = ["All", "5 or more", "10 or more", "15 or more", "20 or more"]


def num(x, d=2):
    s = f"{x:,.{d}f}"
    return s.replace("-", "$-$")


def estrelas(b, se):
    p = 2 * stats.t.sf(abs(b / se), GL)
    return "***" if p < 0.01 else "**" if p < 0.05 else "*" if p < 0.10 else ""


def coef(b, se):
    return num(b) + estrelas(b, se), f"({se:.2f})"


def nota(texto):
    return ("\\begin{tablenotes}[flushleft]\\footnotesize\n\\item \\textit{Note:} " + texto +
            "\n\\end{tablenotes}\n\\end{threeparttable}\n}\n")


# 1. Medias por degrau (painel B) ----------------------------------------------
esc = pd.read_csv(DADOS / "escada_choque_idiossincratico.csv")
b = esc[esc.Bloco.str.startswith("B_")].set_index("corte").loc[DEGRAUS]
linhas = [f"{r} & {int(x.Demitidos):,} & {x.ret_6m_1:.1f} & {x.ret_6m_3:.1f} & {x.ret_12m_1:.1f} & {x.ret_12m_3:.1f} \\\\"
          for r, (_, x) in zip(ROT_DEGRAU, b.iterrows())]
(TAB / "t_escada_medias.tex").write_text(
    "{\\footnotesize\n\\begin{threeparttable}\n\\begin{tabular}{lrrrrr}\n\\toprule\n"
    " & & \\multicolumn{2}{c}{Within 6 months (\\%)} & \\multicolumn{2}{c}{Within 12 months (\\%)} \\\\\n"
    "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\n"
    "Relative decline & Dismissed & G1 & G3 & G1 & G3 \\\\\n\\midrule\n" + "\n".join(linhas) +
    "\n\\bottomrule\n\\end{tabular}\n" +
    nota("Workers dismissed in regions whose rest grew in the year of separation, six cohorts pooled. Each row "
         "pools workers whose origin sector grew at least the indicated number of percentage points less than the "
         "rest of the intermediate region (\\hyperref[eq:choque_leave_one_out]{Equation~(\\ref*{eq:choque_leave_one_out})}); "
         "the ``All'' row imposes no restriction. G1 and G3 are the high- and low-portability groups. "
         "``Dismissed'' adds up the three groups. Source: RAIS, authors' calculations."))

# 2. Regressao por degrau (07): paineis B e C -----------------------------------
reg = pd.read_csv(DADOS / "escada_regressao.csv")
reg = reg[reg.spec == "controles_EF"]
ys = ["r6", "r12", "r24", "outro12", "d_log_rem"]
corpo = []
for painel, titulo in [("B_Regiao_Crescendo", "A. Rest of the region expanding"),
                       ("C_Sem_Seguro (<6m) + Crescendo", "B. Rest of the region expanding, tenure under 6 months")]:
    corpo.append(f"\\multicolumn{{7}}{{l}}{{\\textit{{{titulo}}}}} \\\\")
    for rot, c in zip(ROT_DEGRAU, DEGRAUS):
        x = reg[(reg.painel == painel) & (reg.corte == c)].set_index("y")
        cs = [coef(x.loc[y, "b"], x.loc[y, "se"]) for y in ys]
        corpo.append(f"{rot} & {int(x.loc['r6', 'n']):,} & " + " & ".join(c[0] for c in cs) + " \\\\")
        corpo.append(" & & " + " & ".join(c[1] for c in cs) + " \\\\")
    corpo.append("\\addlinespace[3pt]")
corpo = corpo[:-1]
(TAB / "t_escada_reg.tex").write_text(
    "{\\footnotesize\n\\setlength{\\tabcolsep}{5pt}\n\\begin{threeparttable}\n\\begin{tabular}{lrrrrrr}\n\\toprule\n"
    " & & \\multicolumn{3}{c}{Reemployed within} & \\shortstack{In another sector\\\\within 12 months} & $\\Delta \\ln w$ \\\\\n"
    "\\cmidrule(lr){3-5}\n"
    "Relative decline & Observations & 6 months & 12 months & 24 months & & \\\\\n"
    "& & (1) & (2) & (3) & (4) & (5) \\\\\n\\midrule\n" + "\n".join(corpo) +
    "\n\\bottomrule\n\\end{tabular}\n" +
    nota("Each cell is the coefficient of the Group 1 indicator in a separate regression of "
         "\\hyperref[eq:escada_reg]{Equation~(\\ref*{eq:escada_reg})}, estimated on Group 1 and Group 3 workers at "
         "each rung, multiplied by 100: percentage points in columns (1) to (4) and log points in column (5), which "
         "uses only reemployed workers. Standard errors clustered by CNAE division in parentheses. Panel B keeps "
         "workers dismissed with less than six months of tenure, not eligible for unemployment insurance. "
         "``Observations'' refers to columns (1) to (4). *** $p<0.01$, ** $p<0.05$, * $p<0.10$. "
         "Source: RAIS, authors' calculations."))

# 3. Interacao (09b) ---------------------------------------------------------------
it = pd.read_csv(DADOS / "escada_interacao.csv")
ys3 = ["r6", "r12", "d_log_rem"]
termos = [("faixas", "g1", "G1 (sector grew at least as much as the region)"),
          ("faixas", "faixa::5_Queda_Idio_0a5pp:g1", "G1 $\\times$ decline of 0 to 5"),
          ("faixas", "faixa::4_Queda_Idio_5a10pp:g1", "G1 $\\times$ decline of 5 to 10"),
          ("faixas", "faixa::3_Queda_Idio_10a15pp:g1", "G1 $\\times$ decline of 10 to 15"),
          ("faixas", "faixa::2_Queda_Idio_15a20pp:g1", "G1 $\\times$ decline of 15 to 20"),
          ("faixas", "faixa::1_Queda_Idio_>=20pp:g1", "G1 $\\times$ decline of 20 or more"),
          ("binaria", "g1", "G1 (decline of less than 10)"),
          ("binaria", "g1:queda10", "G1 $\\times$ decline of 10 or more")]
corpo = ["\\multicolumn{4}{l}{\\textit{A. Bands of relative decline}} \\\\"]
for i, (m, t, rot) in enumerate(termos):
    if i == 6:
        corpo += ["\\addlinespace[3pt]", "\\multicolumn{4}{l}{\\textit{B. Single cutoff at 10 points}} \\\\"]
    x = it[(it.modelo == m) & (it.termo == t)].set_index("y")
    cs = [coef(x.loc[y, "b"], x.loc[y, "se"]) for y in ys3]
    corpo.append(f"{rot} & " + " & ".join(c[0] for c in cs) + " \\\\")
    corpo.append(" & " + " & ".join(c[1] for c in cs) + " \\\\")
n_tx = int(it[(it.modelo == "faixas") & (it.y == "r6")].n.iloc[0])
n_sal = int(it[(it.modelo == "faixas") & (it.y == "d_log_rem")].n.iloc[0])
corpo += ["\\midrule", f"Observations & {n_tx:,} & {n_tx:,} & {n_sal:,} \\\\"]
(TAB / "t_escada_interacao.tex").write_text(
    "{\\footnotesize\n\\begin{threeparttable}\n\\begin{tabular}{lrrr}\n\\toprule\n"
    " & \\multicolumn{2}{c}{Reemployed within} & $\\Delta \\ln w$ \\\\\n\\cmidrule(lr){2-3}\n"
    " & 6 months & 12 months & \\\\\n\\midrule\n" + "\n".join(corpo) +
    "\n\\bottomrule\n\\end{tabular}\n" +
    nota("Coefficients of \\hyperref[eq:escada_int]{Equation~(\\ref*{eq:escada_int})}, multiplied by 100, "
         "estimated on all Group 1 and Group 3 workers dismissed in regions whose rest grew. The relative decline is "
         "the number of percentage points by which the origin sector grew less than the rest of the region. Each "
         "model also includes the band (or cutoff) indicators, the controls and the fixed effects of "
         "\\hyperref[eq:escada_reg]{Equation~(\\ref*{eq:escada_reg})}. Standard errors clustered by CNAE division "
         "in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.10$. Source: RAIS, authors' calculations."))

# 4. Decomposicao (09a) ------------------------------------------------------------
dec = pd.read_csv(DADOS / "escada_decomposicao.csv")
passos = [("1_sem_controles", "", "", ""),
          ("2_controles_individuais", "Yes", "", ""),
          ("3_+EF_coorte", "Yes", "Yes", ""),
          ("4_+EF_coorte_divisao", "Yes", "Yes", "Yes"),
          ("5_+EF_coorte_municipio", "Yes", "Yes", ""),
          ("6_+EF_coorte_div_municipio", "Yes", "Yes", "Yes")]
mun = {"5_+EF_coorte_municipio": "Yes", "6_+EF_coorte_div_municipio": "Yes"}
corpo = []
for i, (p, ctl, coo, div) in enumerate(passos, 1):
    x = dec[dec.passo == p].set_index("y")
    cs = [coef(x.loc[y, "b"], x.loc[y, "se"]) for y in ys3]
    corpo.append(f"({i}) & {ctl} & {coo} & {div} & {mun.get(p, '')} & " + " & ".join(c[0] for c in cs) + " \\\\")
    corpo.append(" & & & & & " + " & ".join(c[1] for c in cs) + " \\\\")
n_tx = int(dec[dec.y == "r6"].n.iloc[0]); n_sal = int(dec[dec.y == "d_log_rem"].n.iloc[0])
corpo += ["\\midrule", f"Observations & & & & & {n_tx:,} & {n_tx:,} & {n_sal:,} \\\\"]
(TAB / "t_escada_decomp.tex").write_text(
    "{\\footnotesize\n\\setlength{\\tabcolsep}{5pt}\n\\begin{threeparttable}\n\\begin{tabular}{lccccrrr}\n\\toprule\n"
    " & \\multicolumn{4}{c}{Included} & \\multicolumn{2}{c}{Reemployed within} & $\\Delta \\ln w$ \\\\\n"
    "\\cmidrule(lr){2-5}\\cmidrule(lr){6-7}\n"
    " & Controls & Cohort & Division & Municipality & 6 months & 12 months & \\\\\n\\midrule\n" + "\n".join(corpo) +
    "\n\\bottomrule\n\\end{tabular}\n" +
    nota("Coefficient of the Group 1 indicator, multiplied by 100, for workers dismissed from sectors that grew at "
         "least 10 percentage points less than the rest of the region, in regions whose rest grew, with a positive "
         "origin wage. Each row adds to "
         "\\hyperref[eq:escada_reg]{Equation~(\\ref*{eq:escada_reg})} the indicated set of controls (log origin wage, "
         "age, tenure, sex and schooling) and fixed effects (cohort, origin CNAE division and origin municipality). "
         "Row (5) leaves out the division fixed effects and row (6) is the full specification. Standard errors "
         "clustered by CNAE division in parentheses. *** $p<0.01$, ** $p<0.05$, * $p<0.10$. "
         "Source: RAIS, authors' calculations."))

# 5. Dados da figura mes a mes (08) ------------------------------------------------
mes = pd.read_csv(DADOS / "escada_meses.csv")
x = mes[(mes.amostra == "2_Queda_Relativa_>=10pp") & (mes.spec == "controles_EF")].sort_values("mes")
out = pd.DataFrame({"mes": [0] + x.mes.tolist(),
                    "b": [0.0] + x.b.round(3).tolist(),
                    "lo": [0.0] + (x.b - T975 * x.se).round(3).tolist(),
                    "hi": [0.0] + (x.b + T975 * x.se).round(3).tolist()})
out.to_csv(FIGD / "escada_meses.dat", sep=" ", index=False)
print("ok: t_escada_medias, t_escada_reg, t_escada_interacao, t_escada_decomp, escada_meses.dat")

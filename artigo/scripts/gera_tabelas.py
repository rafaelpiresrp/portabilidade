# Monta as tabelas do artigo (tab/*.tex) a partir dos CSVs e das tabelas .tex
# que os scripts 03 e 04 salvaram. Nenhuma estimativa e refeita aqui.
import re, sys
import numpy as np
import pandas as pd

D, T, OUT = sys.argv[1], sys.argv[2], sys.argv[3]   # dados, tabelas do 04, saida

def num(v, d=1, sinal=False):
    if v is None or (isinstance(v, float) and np.isnan(v)):
        return "--"
    s = f"{v:+.{d}f}" if sinal else f"{v:.{d}f}"
    s = s.replace(".", ",")
    return s.replace("-", "$-$").replace("+", "") if (sinal or v < 0) else s

def mil(v):
    return f"{int(round(v)):,}".replace(",", ".")

def le_tex(nome):
    linhas, dentro = [], False
    for l in open(f"{T}/{nome}.tex", encoding="utf-8"):
        l = l.strip()
        if l.startswith("\\midrule"):
            dentro = True
            continue
        if l.startswith("\\bottomrule"):
            break
        if dentro and "&" in l:
            linhas.append([c.strip() for c in l.rstrip("\\").split("&")])
    return linhas

def coef_ep(cel):
    """'+0,0456*** (0,0118)' -> ('0,0456***', '(0,0118)') com sinal tipografico."""
    m = re.match(r"([+-]?)([\d,]+)(\**)\s*\(([\d,]+)\)", cel)
    s, v, est, ep = m.groups()
    return (("$-$" if s == "-" else "") + v + est, f"({ep})")

FONTE = "Fonte: RAIS, elaboração dos autores."

# espaco entre colunas das tabelas que nao cabem em 16 cm com o padrao de 6pt
SEP = {"t_destino": "3.6pt", "t_escada": "4.5pt", "t_flex_rec": "5pt", "t_flex_pan": "5pt",
       "t_seguro": "4.5pt", "t_reg_12m": "5pt", "t_reg_sal": "5pt", "t_reg_cox": "5pt"}

def escreve(nome, spec, cab, linhas, nota=None, tam="\\small", larg=None, sep=None, fonte=True):
    sep = sep or SEP.get(nome)
    with open(f"{OUT}/{nome}.tex", "w", encoding="utf-8") as f:
        f.write(f"{{{tam}\n" + (f"\\setlength{{\\tabcolsep}}{{{sep}}}\n" if sep else ""))
        f.write(f"\\begin{{threeparttable}}\n\\begin{{tabular}}{{{spec}}}\n\\toprule\n")
        f.write(cab + "\n\\midrule\n")
        for l in linhas:
            f.write(l if isinstance(l, str) else " & ".join(l) + " \\\\")
            f.write("\n")
        f.write("\\bottomrule\n\\end{tabular}\n")
        txt = (nota or "") + ((" " + FONTE) if fonte else "")
        if txt.strip():
            f.write("\\begin{tablenotes}[flushleft]\\footnotesize\n\\item \\textit{Nota:} "
                    + txt.strip() + "\n\\end{tablenotes}\n")
        f.write("\\end{threeparttable}\n}\n")

G = {1: "Alta (G1)", 2: "Média (G2)", 3: "Baixa (G3)"}
COORTES = ["c2014", "c2015", "c2016", "c2019", "c2020", "c2021"]
ESP = "\\addlinespace[3pt]"

# ------------------------------------------------------------------ ocupacoes
r = le_tex("t4b_ocupacoes_grupos")
L = []
for i, x in enumerate(r):
    g = int(x[0][0])
    if i and i % 5 == 0:
        L.append(ESP)
    oc = x[2].replace("Trabalhador de serviços de limpeza e conservação de áreas públicas",
                      "Limpeza e conservação de áreas públicas")
    oc = oc.replace(" (rotas regionais e internacionais)", "").replace(", em geral", "")
    L.append([G[g] if i % 5 == 0 else "", oc, x[3], x[4], x[5]])
escreve("t_ocupacoes", "llrrr",
        "Grupo & Ocupação & Vínculos & Setores & Índice \\\\", L,
        "Cinco maiores ocupações de cada grupo em 2010, pelo número de vínculos privados ativos em 31 de dezembro. ``Setores'' é o número de divisões da CNAE em que a ocupação tem ao menos um vínculo; o índice é o da Equação~\\eqref{eq:portabilidade}.")

# ------------------------------------------------------------------ grandes grupos
nomes = {"1": "Dirigentes e gerentes", "2": "Profissionais das ciências e das artes",
         "3": "Técnicos de nível médio", "4": "Serviços administrativos",
         "5": "Serviços e vendedores do comércio", "6": "Agropecuária, florestal e pesca",
         "7": "Produção industrial (GG 7)", "8": "Produção industrial (GG 8)",
         "9": "Reparação e manutenção"}
L = []
for x in le_tex("t3_grandes_grupos"):
    if x[0] in nomes:
        dif = x[6].replace("-", "$-$").replace("+", "")
        L.append([nomes[x[0]], x[4], x[5], x[2], x[3], dif])
escreve("t_grandes_grupos", "lrrrrr",
        " & \\multicolumn{2}{c}{Vínculos} & \\multicolumn{3}{c}{Índice médio} \\\\\n"
        "\\cmidrule(lr){2-3}\\cmidrule(lr){4-6}\n"
        "Grande grupo da CBO & 2010 & 2023 & 2010 & 2023 & Variação \\\\", L,
        "Média do índice de cada ano ponderada pelos vínculos ativos em 31 de dezembro. "
        "Os grandes grupos 7 e 8 têm o mesmo nome na CBO (trabalhadores da produção de bens e serviços industriais).")

# ------------------------------------------------------------------ shift-share
ss = pd.read_csv(f"{D}/broadness_shiftshare_anual.csv").set_index("ano")
L = []
for x in le_tex("t5_shiftshare")[:3]:
    per = x[0].split(" (")[0].replace("–", " a ")
    L.append([per] + [c.replace("-", "$-$").replace("+", "") for c in x[1:5]])
escreve("t_shiftshare", "lrrrr",
        "Período & Entre ocupações & Dentro da ocupação & Termo cruzado & Total \\\\", L,
        "Decomposição da variação do índice médio pela Equação~\\eqref{eq:shift_share}, com pesos e índices do ano inicial de cada período. "
        "As ocupações sem vínculos no ano inicial entram com peso e índice zero nesse ano, de modo que toda a sua contribuição aparece no termo cruzado.")

# ------------------------------------------------------------------ regioes
fl = pd.read_csv(f"{D}/flexibilidade.csv", dtype={"regiao": str})
it = fl[fl.nivel == "intermediaria"]
L = []
for rot, (a, b) in {"2014 a 2019": (2014, 2019), "2019 a 2022": (2019, 2022)}.items():
    w = it[(it.t0 == a) & (it.t1 == b)]
    for i, (v, nome, d) in enumerate([("T_turb", "Turbulência ($T$)", 3), ("D_desc", "Descasamento ($D$)", 3),
                                      ("F_flex", "Flexibilidade ($F$)", 2),
                                      ("portab_media", "Portabilidade média", 3)]):
        q = w[v].quantile([.1, .25, .5, .75, .9])
        L.append([rot if i == 0 else "", nome, num(w[v].mean(), d)] + [num(z, d) for z in q])
    L.append(ESP)
escreve("t_regioes_resumo", "llrrrrrr",
        "Janela & Indicador & Média & P10 & P25 & Mediana & P75 & P90 \\\\", L[:-1],
        "Estatísticas entre as 133 regiões intermediárias, sem ponderação. A portabilidade média usa o índice de 2010 de cada ocupação e o emprego do ano inicial de cada janela.")

L = []
for rot, (a, b) in {"2014 a 2019": (2014, 2019), "2019 a 2022": (2019, 2022)}.items():
    w = it[(it.t0 == a) & (it.t1 == b)].copy()
    w["ls"], w["le"] = np.log(w.salario), np.log(w.emprego_t0)
    c = w[["F_flex", "T_turb", "D_desc", "portab_media", "ls", "le"]].corr()
    L.append([rot, num(c.loc["F_flex", "portab_media"], 2), num(c.loc["F_flex", "ls"], 2),
              num(c.loc["F_flex", "le"], 2), num(c.loc["portab_media", "ls"], 2),
              num(c.loc["portab_media", "le"], 2), num(c.loc["T_turb", "D_desc"], 2)])
escreve("t_correlacoes", "lrrrrrr",
        " & \\multicolumn{3}{c}{Flexibilidade com} & \\multicolumn{2}{c}{Portabilidade com} & \\\\\n"
        "\\cmidrule(lr){2-4}\\cmidrule(lr){5-6}\n"
        "Janela & Portab. & Log salário & Log emprego & Log salário & Log emprego & $T$ com $D$ \\\\", L,
        "Correlações de Pearson entre as 133 regiões intermediárias, sem ponderação. Portabilidade média, salário médio e emprego são os do ano inicial de cada janela.")

for k, (a, b) in {"rec": (2014, 2019), "pan": (2019, 2022)}.items():
    w = it[(it.t0 == a) & (it.t1 == b)].sort_values("F_flex", ascending=False)
    L = []
    for j, parte in enumerate([w.head(10), w.tail(10)]):
        for i, (_, x) in enumerate(parte.iterrows()):
            L.append([("Mais flexíveis" if j == 0 else "Menos flexíveis") if i == 0 else "",
                      x.nome_inter, x.nome_uf, mil(x.emprego_t0), num(x.T_turb, 3), num(x.D_desc, 3),
                      num(x.F_flex, 2), num(x.portab_media, 3)])
        if j == 0:
            L.append(ESP)
    escreve(f"t_flex_{k}", "lllrrrrr",
            " & Região & UF & Emprego inicial & $T$ & $D$ & $F$ & Portab. \\\\", L,
            f"Regiões intermediárias ordenadas pela flexibilidade alocativa na janela de {a} a {b}.",
            tam="\\footnotesize", sep="3.5pt")

uf = fl[fl.nivel == "uf"]
a = uf[(uf.t0 == 2014) & (uf.t1 == 2019)].set_index("regiao")
b = uf[(uf.t0 == 2019) & (uf.t1 == 2022)].set_index("regiao")
L = []
for reg, x in a.sort_values("F_flex", ascending=False).iterrows():
    y = b.loc[reg]
    L.append([x.nome_uf, mil(x.emprego_t0), num(x.salario, 2), num(x.portab_media, 3),
              num(x.T_turb, 3), num(x.D_desc, 3), num(x.F_flex, 2),
              num(y.T_turb, 3), num(y.D_desc, 3), num(y.F_flex, 2)])
escreve("t_uf", "lrrrrrrrrr",
        " & \\multicolumn{3}{c}{2014} & \\multicolumn{3}{c}{2014 a 2019} & \\multicolumn{3}{c}{2019 a 2022} \\\\\n"
        "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}\\cmidrule(lr){8-10}\n"
        "UF & Vínculos & Salário (SM) & Portab. & $T$ & $D$ & $F$ & $T$ & $D$ & $F$ \\\\", L,
        "Unidades da Federação ordenadas pela flexibilidade alocativa de 2014 a 2019.", tam="\\footnotesize")

nt = pd.read_csv(f"{OUT}/../fig/dados/natureza_tabela.csv")
L = [[str(int(x.ano)), num(x.cresc, 1, True), num(x.resto_cresce, 1), num(x.cai_resto_cresce, 1),
      num(x.cai_resto_cai, 1), num(x.idio5, 1), num(x.idio10, 1), num(x.idio20, 1)] for _, x in nt.iterrows()]
escreve("t_natureza", "lrrrrrrr",
        " & & & \\multicolumn{2}{c}{Setor em queda, restante} & \\multicolumn{3}{c}{Restante em expansão, queda relativa de} \\\\\n"
        "\\cmidrule(lr){4-5}\\cmidrule(lr){6-8}\n"
        "Ano & \\shortstack{Variação do\\\\emprego (\\%)} & \\shortstack{Restante em\\\\expansão} & em expansão & em queda & 5 ou mais & 10 ou mais & 20 ou mais \\\\", L,
        "Parcela do emprego formal privado do ano anterior (\\%) em cada tipo de célula região intermediária $\\times$ divisão CNAE. "
        "A primeira coluna é a variação do emprego formal privado no ano, e as demais são parcelas do emprego do ano anterior. O restante é a soma dos demais setores da mesma região. A segunda coluna é a parcela do emprego em regiões cujo restante cresceu. "
        "As três últimas colunas, em pontos percentuais de queda relativa, usam a definição da Equação~\\eqref{eq:choque_leave_one_out}.", tam="\\footnotesize")

# ------------------------------------------------------------------ indice por ano (apendice)
pc = pd.read_csv(f"{D}/broadness_percentis.csv")
gr = pd.read_csv(f"{D}/broadness_grupos.csv").pivot(index="ano", columns="grupo", values="pct_vinc")
L = [[str(int(x.ano)), mil(x.vinculos), mil(x.ocupacoes), num(x.media_pond, 3), num(x.dp_pond, 3),
      num(x.p10, 3), num(x.p25, 3), num(x.p50, 3), num(x.p75, 3), num(x.p90, 3),
      num(gr.loc[x.ano, 1], 1), num(gr.loc[x.ano, 2], 1), num(gr.loc[x.ano, 3], 1)] for _, x in pc.iterrows()]
escreve("t_indice_ano", "lrrrrrrrrrrrr",
        " & & & \\multicolumn{7}{c}{Índice de portabilidade} & \\multicolumn{3}{c}{Grupos (\\%)} \\\\\n"
        "\\cmidrule(lr){4-10}\\cmidrule(lr){11-13}\n"
        "Ano & Vínculos & Ocup. & Média & DP & P10 & P25 & P50 & P75 & P90 & G1 & G2 & G3 \\\\", L,
        "Vínculos privados ativos em 31 de dezembro. Média, desvio-padrão e percentis ponderados pelos vínculos. "
        "As três últimas colunas trazem a parcela do emprego em cada grupo fixado em 2010.", tam="\\footnotesize")

r = le_tex("t2_extremos")
L = []
for i, x in enumerate(r):
    if i == 10:
        L.append(ESP)
    L.append([("Mais portáteis" if i < 10 else "Menos portáteis") if i % 10 == 0 else "",
              x[2].replace(" (pdi)", "").replace(" - ", "-"), x[3], x[4], x[5]])
escreve("t_extremos", "lp{8.2cm}rrr", " & Ocupação & Vínculos & Setores & Índice \\\\", L,
        "Ocupações com pelo menos 5.000 vínculos em 2023, índice de 2023.", tam="\\footnotesize")

# ------------------------------------------------------------------ coortes e perfil
am = pd.read_csv(f"{D}/transicoes_amostra.csv")
L = [[str(x.ano), x.fase, mil(x.pessoas), mil(x.uma_demissao), mil(x.sem_outro_emprego),
      str(x.horizonte_meses)] for _, x in am.iterrows()]
L.append(ESP)
L.append(["Total", "", "", mil(am.uma_demissao.sum()), mil(am.sem_outro_emprego.sum()), ""])
escreve("t_coortes", "llrrrr",
        "Coorte & Período & Trabalhadores no ano & Uma dispensa & Coorte final & Meses \\\\", L,
        "``Trabalhadores no ano'' conta quem teve ao menos um vínculo privado no ano. ``Uma dispensa'' conta quem teve uma "
        "única dispensa sem justa causa. A coorte final exclui quem tinha outro vínculo privado ativo no mês do "
        "desligamento. ``Meses'' é o tempo máximo de acompanhamento, até dezembro de 2023.")

pf = pd.read_csv(f"{D}/transicoes_perfil.csv")
L = []
for co, rot in [("total", "Seis coortes"), ("recessao", "2014 a 2016"), ("pandemia", "2019 a 2021")]:
    for g in (1, 2, 3):
        x = pf[(pf.coorte == co) & (pf.grupo == g)].iloc[0]
        L.append([rot if g == 1 else "", G[g], mil(x.pessoas), num(x.broadness_media, 3), num(x.idade_media, 1),
                  num(x.pct_mulher, 1), num(x.pct_medio_ou_mais, 1), num(x.tempo_medio_meses, 1),
                  num(x.salario_medio_sm, 2)])
    L.append(ESP)
escreve("t_perfil", "llrrrrrrr",
        " & Grupo & Demitidos & Índice & Idade & \\shortstack{Mulheres\\\\(\\%)} & \\shortstack{Médio\\\\completo (\\%)} & \\shortstack{Meses no\\\\vínculo} & \\shortstack{Salário\\\\(SM)} \\\\",
        L[:-1], "Médias das características do trabalhador e do vínculo encerrado. ``Médio completo'' inclui quem tem escolaridade acima do ensino médio. O salário é a remuneração média do ano do desligamento, em salários mínimos.",
        tam="\\footnotesize")

# ------------------------------------------------------------------ retorno
du = pd.read_csv(f"{D}/transicoes_duracao.csv")
L = []
for co in COORTES + ["total"]:
    for g in (1, 2, 3):
        x = du[(du.coorte == co) & (du.grupo == g)].iloc[0]
        L.append([("Seis coortes" if co == "total" else co[1:]) if g == 1 else "", G[g], mil(x.pessoas),
                  num(x.em_6m, 1), num(x.em_12m, 1), num(x.em_24m, 1), num(x.pct_voltou, 1),
                  str(int(x.mediana_mes)), num(x.pct_migrou, 1), num(x.d_log_rem, 3)])
    L.append(ESP)
escreve("t_retorno", "llrrrrrrrr",
        " & & & \\multicolumn{4}{c}{Reempregados em até (\\%)} & & & \\\\\n\\cmidrule(lr){4-7}\n"
        "Coorte & Grupo & Demitidos & 6 m & 12 m & 24 m & \\shortstack{Até\\\\dez/2023} & \\shortstack{Mediana\\\\(meses)} & \\shortstack{Outro\\\\mun. (\\%)} & $\\Delta \\ln w$ \\\\",
        L[:-1],
        "Parcela dos demitidos com novo vínculo formal privado em cada horizonte, sem correção para censura, que só atinge "
        "a coluna ``Até dez/2023''. A mediana é o número de meses até o reemprego. ``Outro mun.'' é a parcela dos "
        "reempregados em município diferente do de origem. $\\Delta \\ln w$ é a variação média do log do salário, em salários mínimos, entre os reempregados.",
        tam="\\footnotesize")

# ------------------------------------------------------------------ regressoes (resumo)
L = []
for x in le_tex("t12_regressoes_resumo"):
    cs = [coef_ep(c) for c in x[2:]]
    L.append([x[0], x[1]] + [c[0] for c in cs])
    L.append(["", ""] + [c[1] for c in cs])
escreve("t_regressoes", "lrrrrrrr",
        " & & \\multicolumn{3}{c}{Reemprego em 12 meses} & Em 24 meses & Cox & $\\Delta \\ln w$ \\\\\n"
        "\\cmidrule(lr){3-5}\n"
        "Coorte & Observações & (1) & (2) & (3) & (4) & (5) & (6) \\\\", L,
        "Cada célula é o coeficiente do índice de portabilidade de 2010 em uma regressão separada, com erro-padrão entre "
        "parênteses, agrupado por divisão CNAE nas colunas (1) a (4) e (6). Como o índice varia de 0 a 1, o coeficiente "
        "é a diferença entre uma ocupação concentrada em um único setor e uma ocupação distribuída por todos. (1) sem controles. (2) idade, tempo no vínculo, sexo, escolaridade e efeitos fixos de "
        "divisão CNAE e município de origem. (3), (4) e (6) acrescentam o log do salário no vínculo perdido. (5) modelo de "
        "Cox estratificado por divisão CNAE, em amostra de 500 mil demitidos por coorte. "
        "*** $p<0{,}01$, ** $p<0{,}05$, * $p<0{,}10$.", tam="\\footnotesize")

# ------------------------------------------------------------------ destino
cp = pd.read_csv(f"{D}/transicoes_composicao.csv")
dn = pd.read_csv(f"{D}/transicoes_destino_norm.csv")
ordem = ["1 Mesma ocupacao, mesmo setor", "2 Mesma ocupacao, outro setor",
         "3 Mesma familia, outra ocupacao", "4 Outra ocupacao"]
L = []
for co, rot in [("total", "Seis coortes"), ("recessao", "2014 a 2016"), ("pandemia", "2019 a 2021")]:
    for g in (1, 2, 3):
        w = cp[(cp.coorte == co) & (cp.grupo == g)].set_index("destino")
        x = dn[(dn.coorte == co) & (dn.grupo == g)].iloc[0]
        L.append([rot if g == 1 else "", G[g]] + [num(w.loc[o, "pct_cond"], 1) for o in ordem] +
                 [num(x.obs_outro_setor, 1), num(x.prev_broadness, 1), num(x.razao_obs_prev, 2)])
    L.append(ESP)
escreve("t_destino", "llrrrrrrr",
        " & & \\multicolumn{4}{c}{Destino dos reempregados (\\%)} & \\multicolumn{3}{c}{Reempregados na mesma ocupação} \\\\\n"
        "\\cmidrule(lr){3-6}\\cmidrule(lr){7-9}\n"
        " & Grupo & \\shortstack{Mesma ocup.,\\\\mesmo setor} & \\shortstack{Mesma ocup.,\\\\outro setor} & "
        "\\shortstack{Mesma\\\\família} & \\shortstack{Outra\\\\família} & "
        "\\shortstack{Mudou de\\\\setor (\\%)} & \\shortstack{Previsto\\\\(\\%)} & \\shortstack{Obs./\\\\prev.} \\\\",
        L[:-1],
        "As quatro primeiras colunas somam 100\\% entre os reempregados. Ocupação é o código da CBO a seis dígitos, família "
        "é o código a quatro dígitos e setor é a divisão da CNAE. As três últimas colunas consideram só quem se reempregou na "
        "mesma ocupação: a parcela que mudou de setor, a parcela prevista se a nova vaga fosse sorteada entre todos os "
        "vínculos da ocupação no país (a média de $m_{o,2010}$ nesse conjunto) e a razão entre as duas.", tam="\\footnotesize")

L = []
for co in COORTES:
    for g in (1, 2, 3):
        w = cp[(cp.coorte == co) & (cp.grupo == g)].set_index("destino")
        x = dn[(dn.coorte == co) & (dn.grupo == g)].iloc[0]
        L.append([co[1:] if g == 1 else "", G[g]] + [num(w.loc[o, "pct_cond"], 1) for o in ordem] +
                 [num(x.pct_mesma_cbo, 1), num(x.obs_outro_setor, 1), num(x.prev_broadness, 1), num(x.razao_obs_prev, 2)])
    L.append(ESP)
escreve("t_destino_coorte", "llrrrrrrrr",
        " & & \\multicolumn{4}{c}{Destino dos reempregados (\\%)} & \\multicolumn{4}{c}{Reempregados na mesma ocupação} \\\\\n"
        "\\cmidrule(lr){3-6}\\cmidrule(lr){7-10}\n"
        "Coorte & Grupo & \\shortstack{Mesma ocup.,\\\\mesmo setor} & \\shortstack{Mesma ocup.,\\\\outro setor} & "
        "\\shortstack{Mesma\\\\família} & \\shortstack{Outra\\\\família} & \\shortstack{Parcela\\\\(\\%)} & "
        "\\shortstack{Mudou de\\\\setor (\\%)} & \\shortstack{Previsto\\\\(\\%)} & \\shortstack{Obs./\\\\prev.} \\\\",
        L[:-1], "Mesmas definições da Tabela~\\ref{tab:destino}, por coorte.", tam="\\footnotesize")

# ------------------------------------------------------------------ escada
es = pd.read_csv(f"{D}/escada_choque_idiossincratico.csv")
es["b"] = es.Bloco.str[0]
deg = ["Todos", "5 ou mais", "10 ou mais", "15 ou mais", "20 ou mais"]
blocos = {"A": "A. Todas as regiões", "B": "B. Restante da região em expansão",
          "C": "C. Restante da região em expansão, menos de 6 meses no vínculo"}
L = []
for b, rot in blocos.items():
    L.append(f"\\multicolumn{{10}}{{l}}{{\\textit{{{rot}}}}} \\\\")
    for i, (_, x) in enumerate(es[es.b == b].iterrows()):
        L.append([deg[i], mil(x.Demitidos), num(x.ret_6m_1, 1), num(x.ret_6m_3, 1), num(x.Dif_6m_G1_G3, 1, True),
                  num(x.ret_12m_1, 1), num(x.ret_12m_3, 1), num(x.Dif_12m_G1_G3, 1, True),
                  num(x.outro_12m_1, 1), num(x.outro_12m_3, 1)])
    L.append(ESP)
escreve("t_escada", "lrrrrrrrrr",
        " & & \\multicolumn{3}{c}{Reemprego em 6 meses (\\%)} & \\multicolumn{3}{c}{Reemprego em 12 meses (\\%)} & "
        "\\multicolumn{2}{c}{Em outro setor (\\%)} \\\\\n"
        "\\cmidrule(lr){3-5}\\cmidrule(lr){6-8}\\cmidrule(lr){9-10}\n"
        "Queda relativa & Demitidos & G1 & G3 & Dif. & G1 & G3 & Dif. & G1 & G3 \\\\", L[:-1],
        "Cada linha reúne os demitidos das seis coortes cujo setor de origem cresceu, no ano do desligamento, pelo menos o "
        "número indicado de pontos percentuais a menos que o restante da região intermediária "
        "(Equação~\\eqref{eq:choque_leave_one_out}); a linha ``Todos'' não impõe restrição. G1 e G3 são os grupos de alta e "
        "de baixa portabilidade, e ``Dif.'' é a diferença entre eles em pontos percentuais. As duas últimas colunas trazem "
        "a parcela dos demitidos reempregada em até 12 meses em divisão CNAE diferente da de origem. ``Demitidos'' soma os três grupos.", tam="\\footnotesize")

df = pd.read_csv(f"{D}/escada_choque_reponderado_DFL.csv")
L = [[deg[i], num(x.ret_6m_ajust_1, 1), num(x.ret_6m_ajust_3, 1), num(x.Dif_6m_G1_G3, 1, True),
      num(x.ret_12m_ajust_1, 1), num(x.ret_12m_ajust_3, 1), num(x.Dif_12m_G1_G3, 1, True),
      num(x.dlog_ajust_1, 3), num(x.dlog_ajust_3, 3), num(x.Dif_dlogw, 3, True)] for i, (_, x) in enumerate(df.iterrows())]
escreve("t_dfl", "lrrrrrrrrr",
        " & \\multicolumn{3}{c}{Reemprego em 6 meses (\\%)} & \\multicolumn{3}{c}{Reemprego em 12 meses (\\%)} & "
        "\\multicolumn{3}{c}{$\\Delta \\ln w$} \\\\\n"
        "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}\\cmidrule(lr){8-10}\n"
        "Queda relativa & G1 & G3 & Dif. & G1 & G3 & Dif. & G1 & G3 & Dif. \\\\", L,
        "Mesma amostra do painel B da Tabela~\\ref{tab:escada}, sem os demitidos sem salário de origem positivo. As médias "
        "do Grupo 3 são recalculadas com os pesos do Grupo 1 nas 16 células de tempo no vínculo e salário inicial "
        "(Equação~\\eqref{eq:dfl_reweight}), dentro de cada faixa de queda relativa, de modo que os dois grupos têm a mesma "
        "composição nessas dimensões. ``Dif.'' em pontos percentuais nos dois primeiros blocos e em pontos de log no terceiro.",
        tam="\\footnotesize")

# ------------------------------------------------------------------ seguro e salario
hz = pd.read_csv(f"{D}/transicoes_hazard_tenure.csv")
n1 = hz[hz.mes == 1].pivot(index="faixa_seguro", columns="grupo", values="n_risco")
tot = pf[pf.coorte == "total"].set_index("grupo").pessoas
L = []
for f, rot in [("Sem seguro (< 6 meses)", "Menos de 6 meses"),
               ("Seguro 5 parcelas (>= 24 meses)", "24 meses ou mais")]:
    w = hz[hz.faixa_seguro == f]
    for i, g in enumerate((1, 2, 3)):
        x = w[w.grupo == g].set_index("mes")
        n0 = x.n_risco.iloc[0]
        ac = lambda m: 100 * x.eventos.loc[:m].sum() / n0
        L.append([rot if i == 0 else "", G[g], mil(n0), num(100 * n0 / tot[g], 1),
                  num(x.hazard_pct.loc[1:5].mean(), 2), num(x.hazard_pct.loc[6:8].mean(), 2),
                  num(ac(6), 1), num(ac(12), 1)])
    L.append(ESP)
escreve("t_seguro", "llrrrrrr",
        " & & & & \\multicolumn{2}{c}{Taxa mensal (\\%)} & \\multicolumn{2}{c}{Reempregados (\\%)} \\\\\n"
        "\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}\n"
        "Tempo no vínculo & Grupo & Demitidos & \\% do grupo & Meses 1 a 5 & Meses 6 a 8 & 6 m & 12 m \\\\", L[:-1],
        "Seis coortes reunidas. A taxa mensal é a parcela dos que ainda não tinham novo vínculo no início do mês e se reempregam naquele mês, "
        "em média simples dos meses indicados. Quem tem menos de 6 meses no vínculo não tem direito ao seguro-desemprego; quem tem 24 ou mais tem direito a cinco parcelas.", tam="\\footnotesize")

r13, r14 = le_tex("t13_mecanismo_setor_salario"), le_tex("t14_mecanismo_reserva_seguro")
L = []
for a, b in zip(r13, r14):
    cs = [coef_ep(c) for c in a[3:6]] + [coef_ep(c) for c in b[4:6]]
    L.append([a[0], a[2]] + [c[0] for c in cs])
    L.append(["", ""] + [c[1] for c in cs])
escreve("t_salario_mec", "lrrrrrr",
        " & & \\multicolumn{3}{c}{Mudança de setor} & \\multicolumn{2}{c}{Tempo fora} \\\\\n"
        "\\cmidrule(lr){3-5}\\cmidrule(lr){6-7}\n"
        "Coorte & Reempregados & Mudou & Portab. & Mudou $\\times$ portab. & Meses & Meses $\\times$ portab. \\\\", L,
        "Variável dependente: $\\Delta \\ln w_i$ dos reempregados. Duas regressões por coorte. Na primeira entram o "
        "indicador de mudança de divisão CNAE, o índice de portabilidade e a interação. Na segunda, os meses até o reemprego "
        "e a interação com o índice (o coeficiente do índice não é mostrado). Controles e efeitos fixos da coluna (6) da "
        "Tabela~\\ref{tab:regressoes}. Erros-padrão agrupados por divisão CNAE. *** $p<0{,}01$, ** $p<0{,}05$, * $p<0{,}10$.",
        tam="\\footnotesize")

L = []
for b in r14:
    cs = [coef_ep(c) for c in b[2:4]]
    L.append([b[0]] + [c[0] for c in cs])
    L.append([""] + [c[1] for c in cs])
escreve("t_seguro_reg", "lrr",
        "Coorte & Menos de 6 meses no vínculo & 24 meses ou mais \\\\", L,
        "Coeficiente do índice de portabilidade em regressões do reemprego em até 6 meses, estimadas separadamente para "
        "demitidos sem direito ao seguro-desemprego e com direito a cinco parcelas. Controles e efeitos fixos da "
        "coluna (3) da Tabela~\\ref{tab:regressoes}. Erros-padrão agrupados por divisão CNAE.",
        tam="\\footnotesize", larg="0.7\\textwidth")

sd = le_tex("ta3_salario_por_destino")
dest = {"Mesma ocupacao, mesmo setor": "Mesma ocupação, mesmo setor",
        "Mesma ocupacao, outro setor": "Mesma ocupação, outro setor",
        "Mesma familia, outra ocupacao": "Mesma família, outra ocupação",
        "Outra ocupacao": "Outra família de ocupações"}
L = []
for i, x in enumerate(sd):
    if i and i % 4 == 0:
        L.append(ESP)
    L.append([G[int(x[0][0])] if i % 4 == 0 else "", dest[x[1]], x[2], x[3],
              x[4].replace("-", "$-$"), x[5]])
escreve("t_salario_destino", "llrrrr",
        "Grupo & Destino & Reempregados & Mediana (meses) & $\\Delta \\ln w$ & Erro-padrão \\\\", L,
        "Seis coortes reunidas. Mediana dos meses até o reemprego e média de $\\Delta \\ln w_i$ em cada destino.",
        tam="\\footnotesize")

# ------------------------------------------------------------------ regressoes completas (apendice)
def limpa(c):
    c = c.strip()
    if not c:
        return ""
    est = ""
    m = re.search(r"\$\^\{(\*+)\}\$", c)
    if m:
        est = m.group(1)
        c = c[:m.start()]
    par = c.startswith("(")
    c = c.strip("()$ ")
    m = re.match(r"(-?[\d.]+)\\times 10\^\{(-?\d+)\}", c)
    v = float(m.group(1)) * 10 ** int(m.group(2)) if m else float(c.replace(",", ""))
    s = f"{abs(v):.4f}".replace(".", ",")
    s = ("$-$" if v < 0 else "") + s
    return f"({s})" if par else s + est

def le_binaria(co):
    linhas = [l.rstrip().rstrip("\\").rstrip() for l in open(f"{T}/t12_binaria_{co}.tex", encoding="utf-8")]
    res, i = {}, 0
    while i < len(linhas):
        c = [z.strip() for z in linhas[i].split("&")]
        chave = c[0]
        if chave in ("Portabilidade (2010)", "Idade", "Tempo no emprego (meses)", "factor(sexo)2",
                     "Log salário origem") or chave.startswith("factor(grauinstr)"):
            ep = [z.strip() for z in linhas[i + 1].split("&")]
            res[chave] = ([limpa(z) for z in c[1:]], [limpa(z) for z in ep[1:]])
            i += 1
        elif chave in ("Observations", "R$^2$"):
            res[chave] = c[1:]
        i += 1
    return res

bin_ = {co: le_binaria(co) for co in COORTES}
rot = [("Portabilidade (2010)", "Índice de portabilidade (2010)"), ("Log salário origem", "Log do salário de origem"),
       ("Idade", "Idade"), ("Tempo no emprego (meses)", "Meses no vínculo"), ("factor(sexo)2", "Mulher"),
       ("factor(grauinstr)5", "Fundamental completo"), ("factor(grauinstr)7", "Médio completo"),
       ("factor(grauinstr)9", "Superior completo")]
for col, nome, titulo in [(2, "t_reg_12m", "reemprego em 12 meses"), (4, "t_reg_sal", "$\\Delta \\ln w$")]:
    L = []
    for ch, r_ in rot:
        L.append([r_] + [bin_[co][ch][0][col] for co in COORTES])
        L.append([""] + [bin_[co][ch][1][col] for co in COORTES])
    L.append("\\midrule")
    L.append(["Observações"] + [bin_[co]["Observations"][col].replace(",", ".") for co in COORTES])
    L.append(["$R^2$"] + [f"{float(bin_[co]['R$^2$'][col]):.3f}".replace(".", ",") for co in COORTES])
    escreve(nome, "lrrrrrr", " & 2014 & 2015 & 2016 & 2019 & 2020 & 2021 \\\\", L,
            "Mínimos quadrados com efeitos fixos de divisão CNAE e de município de origem. A escolaridade entra como "
            "onze categorias da RAIS; a tabela mostra três delas, em relação a analfabetos. Erros-padrão agrupados por "
            "divisão CNAE entre parênteses. *** $p<0{,}01$, ** $p<0{,}05$, * $p<0{,}10$.", tam="\\footnotesize", sep="3.5pt")

def le_cox(co):
    res = {}
    for l in open(f"{T}/t12_cox_{co}.txt", encoding="utf-8"):
        p = l.split()
        if l.startswith("  n="):
            m = re.search(r"n= (\d+), number of events= (\d+)", l)
            res["n"], res["ev"] = int(m.group(1)), int(m.group(2))
        if len(p) >= 6 and p[0] in ("broadness", "log_rem0", "idade", "tempo", "factor(sexo)2",
                                    "factor(grauinstr)5", "factor(grauinstr)7", "factor(grauinstr)9") \
                and p[0] not in res and re.match(r"-?\d", p[1]) and "e" in p[1]:
            b, se, z = float(p[1]), float(p[3]), abs(float(p[4]))
            est = "***" if z > 2.576 else "**" if z > 1.96 else "*" if z > 1.645 else ""
            res[p[0]] = (("$-$" if b < 0 else "") + f"{abs(b):.4f}".replace(".", ",") + est,
                         "(" + f"{se:.4f}".replace(".", ",") + ")")
        if l.startswith("Concordance"):
            res["conc"] = l.split("=")[1].split()[0].replace(".", ",")
    return res

cx = {co: le_cox(co) for co in COORTES}
rc = [("broadness", "Índice de portabilidade (2010)"), ("log_rem0", "Log do salário de origem"), ("idade", "Idade"),
      ("tempo", "Meses no vínculo"), ("factor(sexo)2", "Mulher"), ("factor(grauinstr)5", "Fundamental completo"),
      ("factor(grauinstr)7", "Médio completo"), ("factor(grauinstr)9", "Superior completo")]
L = []
for ch, r_ in rc:
    L.append([r_] + [cx[co][ch][0] for co in COORTES])
    L.append([""] + [cx[co][ch][1] for co in COORTES])
L.append("\\midrule")
L.append(["Observações"] + [mil(cx[co]["n"]) for co in COORTES])
L.append(["Retornos observados"] + [mil(cx[co]["ev"]) for co in COORTES])
L.append(["Concordância"] + [cx[co]["conc"] for co in COORTES])
escreve("t_reg_cox", "lrrrrrr", " & 2014 & 2015 & 2016 & 2019 & 2020 & 2021 \\\\", L,
        "Coeficientes (log da razão de riscos) do modelo de Cox estratificado por divisão CNAE de origem, em amostra "
        "aleatória de 500 mil demitidos por coorte; as observações sem salário de origem positivo ficam de fora. "
        "*** $p<0{,}01$, ** $p<0{,}05$, * $p<0{,}10$.", tam="\\footnotesize", sep="3.5pt")
print("tabelas ok")

# Converte as tabelas geradas por gera_tabelas.py (rotulos em portugues) para a
# versao em ingles do artigo: numeros no padrao ingles, notas reescritas em
# ingles e rotulos/cabecalhos traduzidos.
# Uso (dentro de artigo/): python3 scripts/traduz_tabelas.py tab
import glob, re, sys

PASTA = sys.argv[1] if len(sys.argv) > 1 else "tab"

def remissao(m):
    nome, lab = m.group(1), m.group(2)
    if nome.startswith("Equa"):
        return "\\hyperref[%s]{%s~(\\ref*{%s})}" % (lab, nome, lab)
    return "\\hyperref[%s]{%s~\\ref*{%s}}" % (lab, nome, lab)

FONTE = " Source: RAIS, authors' calculations."
EST = r" *** $p<0.01$, ** $p<0.05$, * $p<0.10$."
REG = (r"Least squares with origin CNAE division and municipality fixed effects. Schooling enters as the "
       r"eleven RAIS categories; the table shows three of them, relative to illiterate workers. "
       r"Standard errors clustered by CNAE division in parentheses." + EST)
NOTAS = {
 "t_coortes": r"``Workers in the year'' counts those with at least one private job in the year. ``One dismissal'' counts those with a single dismissal without cause. The final cohort excludes workers with another private job active in the month of separation and those without a valid occupation code. ``Months'' is the follow-up through December 2023 for workers dismissed in December of the cohort year, the shortest in each cohort.",
 "t_correlacoes": r"Unweighted Pearson correlations across the 133 intermediate regions. Average portability, average wage and employment refer to the initial year of each window.",
 "t_destino": r"The first four columns add up to 100\% of reemployed workers. Occupation is the six-digit CBO code, family is the four-digit code and sector is the CNAE division. The last three columns consider only workers reemployed in the same occupation: the share that changed sector, the share predicted if the new job were drawn at random from all jobs of the occupation in the country (the mean of $m_{o,2010}$ in this set), and the ratio of the two.",
 "t_destino_coorte": r"Same definitions as in [[Table|tab:destino]], by cohort.",
 "t_dfl": r"Same sample as panel B of [[Table|tab:escada]], excluding workers without a positive origin wage. Group 3 means are recomputed with Group 1 weights in the 16 cells of tenure and initial wage ([[Equation|eq:dfl_reweight]]), within each band of relative decline, so that the two groups have the same composition along these dimensions. The first two blocks give the share reemployed within 6 and 12 months, and the third the change in log wages. ``Diff.'' is in percentage points in the first two blocks and in log points in the third.",
 "t_escada": r"Each row pools workers from the six cohorts whose origin sector grew, in the year of separation, at least the indicated number of percentage points less than the rest of the intermediate region ([[Equation|eq:choque_leave_one_out]]); the ``All'' row imposes no restriction. The first two blocks give the share reemployed within 6 and 12 months. G1 and G3 are the high- and low-portability groups, and ``Diff.'' is the difference between them in percentage points. The last two columns give the share of dismissed workers reemployed within 12 months in a CNAE division other than the origin one. ``Dismissed'' adds up the three groups.",
 "t_extremos": r"Occupations with at least 5,000 jobs in 2023; 2023 index.",
 "t_flex_rec": r"Intermediate regions ranked by allocative flexibility in the 2014 to 2019 window.",
 "t_flex_pan": r"Intermediate regions ranked by allocative flexibility in the 2019 to 2022 window.",
 "t_grandes_grupos": r"Average of each year's index weighted by jobs active on December 31. Major groups 7 and 8 have the same name in the CBO (workers in the production of industrial goods and services).",
 "t_indice_ano": r"Private jobs active on December 31. Mean, standard deviation and percentiles weighted by jobs. The last three columns give the share of employment in each group, with groups fixed in 2010.",
 "t_natureza": r"Share of the previous year's private formal employment (\%) in each type of intermediate region $\times$ CNAE division cell. The first column is the change in private formal employment during the year, and the others are shares of the previous year's employment. The rest of the region is the sum of the other sectors in the same region. The second column is the share of employment in regions whose rest grew. The two columns under ``Declining sector'' split those sectors by whether the rest of the region grew or fell. The last three columns refer to regions whose rest grew and give the share of employment in sectors with at least the indicated relative decline, in percentage points, as defined in [[Equation|eq:choque_leave_one_out]].",
 "t_ocupacoes": r"Five largest occupations in each group in 2010, by number of private jobs active on December 31. ``Sectors'' is the number of CNAE divisions in which the occupation has at least one job; the index is that of [[Equation|eq:portabilidade]].",
 "t_perfil": r"Means of the characteristics of the worker and of the lost job. ``High school'' includes workers with schooling above high school. The wage is the average monthly earnings in the year of separation, in minimum wages (MW).",
 "t_reg_12m": REG,
 "t_reg_sal": REG,
 "t_reg_cox": r"Coefficients (log hazard ratios) of the Cox model stratified by origin CNAE division, in a random sample of 500 thousand dismissed workers per cohort; observations without a positive origin wage are excluded." + EST,
 "t_regioes_resumo": r"Unweighted statistics across the 133 intermediate regions. Average portability uses each occupation's 2010 index and employment in the initial year of each window.",
 "t_regressoes": r"Each cell is the coefficient of the 2010 portability index in a separate regression, with the standard error in parentheses, clustered by CNAE division in columns (1) to (4) and (6). Since the index ranges from 0 to 1, the coefficient is the difference between an occupation concentrated in a single sector and one spread across all sectors. (1) No controls. (2) Age, tenure, sex, schooling, and origin CNAE division and municipality fixed effects. (3), (4) and (6) add the log wage in the lost job. (5) Cox model stratified by CNAE division, in a sample of 500 thousand dismissed workers per cohort." + EST,
 "t_retorno": r"Share of dismissed workers with a new private formal job at each horizon, without correction for censoring, which affects only the ``By Dec 2023'' column. The median is the number of months until reemployment. ``Other mun.'' is the share of reemployed workers in a municipality other than the origin one. $\Delta \ln w$ is the average change in log wages, in minimum wages, among reemployed workers.",
 "t_salario_destino": r"Six cohorts pooled. Median months until reemployment and mean of $\Delta \ln w_i$ in each destination.",
 "t_salario_mec": r"Dependent variable: $\Delta \ln w_i$ of reemployed workers. Two regressions per cohort. The first includes the indicator of a change in CNAE division, the portability index and their interaction. The second includes months until reemployment and its interaction with the index (the index coefficient is not shown). Controls and fixed effects as in column (6) of [[Table|tab:regressoes]]. Standard errors clustered by CNAE division." + EST,
 "t_seguro": r"Six cohorts pooled. The monthly rate is the share of workers without a new job at the start of the month who are reemployed in that month, as a simple average over the months indicated. Workers with less than 6 months of tenure are not eligible for unemployment insurance; those with 24 months or more are entitled to five installments.",
 "t_seguro_reg": r"Coefficient of the portability index in regressions of reemployment within 6 months, estimated separately for dismissed workers not eligible for unemployment insurance and for those entitled to five installments. Controls and fixed effects as in column (3) of [[Table|tab:regressoes]]. Standard errors clustered by CNAE division.",
 "t_shiftshare": r"Decomposition of the change in the average index by [[Equation|eq:shift_share]], with weights and indices from the initial year of each period. Occupations with no jobs in the initial year enter with zero weight and index in that year, so that their whole contribution appears in the cross term.",
 "t_uf": r"States ranked by allocative flexibility from 2014 to 2019.",
}
# frases exatas (cabecalhos e rotulos), aplicadas das mais longas para as mais curtas
T = [
 # cabecalhos
 (r"Coorte & Período & Trabalhadores no ano & Uma dispensa & Coorte final & Meses", r"Cohort & Period & Workers in the year & One dismissal & Final cohort & Months"),
 (r"{Flexibilidade com}", r"{Flexibility with}"), (r"{Portabilidade com}", r"{Portability with}"),
 (r"Janela & Portab. & Log salário & Log emprego & Log salário & Log emprego & $T$ com $D$", r"Window & Portab. & Log wage & Log employment & Log wage & Log employment & $T$ with $D$"),
 (r"{Destino dos reempregados (\%)}", r"{Destination of reemployed workers (\%)}"),
 (r"{Reempregados na mesma ocupação}", r"{Reemployed in the same occupation}"),
 (r"\shortstack{Mesma ocup.,\\mesmo setor}", r"\shortstack{Same occ.,\\same sector}"),
 (r"\shortstack{Mesma ocup.,\\outro setor}", r"\shortstack{Same occ.,\\other sector}"),
 (r"\shortstack{Mesma\\família}", r"\shortstack{Same\\family}"), (r"\shortstack{Outra\\família}", r"\shortstack{Other\\family}"),
 (r"\shortstack{Mudou de\\setor (\%)}", r"\shortstack{Changed\\sector (\%)}"),
 (r"\shortstack{Previsto\\(\%)}", r"\shortstack{Predicted\\(\%)}"), (r"\shortstack{Obs./\\prev.}", r"\shortstack{Obs./\\pred.}"),
 (r"\shortstack{Parcela\\(\%)}", r"\shortstack{Share\\(\%)}"),
 (r"{Reemprego em 6 meses (\%)}", r"{Within 6 months (\%)}"),
 (r"{Reemprego em 12 meses (\%)}", r"{Within 12 months (\%)}"),
 (r"{Reemprego em 12 meses}", r"{Reemployment within 12 months}"),
 (r"\multicolumn{2}{c}{Em outro setor (\%)}", r"\multicolumn{2}{c}{\shortstack{In another\\sector (\%)}}"),
 (r"\textit{A. Todas as regiões}", r"\textit{A. All regions}"),
 (r"\textit{C. Restante da região em expansão, menos de 6 meses no vínculo}", r"\textit{C. Rest of the region expanding, tenure under 6 months}"),
 (r"\textit{B. Restante da região em expansão}", r"\textit{B. Rest of the region expanding}"),
 (r"& Ocupação & Vínculos & Setores & Índice", r"& Occupation & Jobs & Sectors & Index"),
 (r"& Região & UF & Emprego inicial &", r"& Region & State & Initial employment &"),
 (r"{Vínculos}", r"{Jobs}"), (r"{Índice médio}", r"{Average index}"),
 (r"Grande grupo da CBO & 2010 & 2023 & 2010 & 2023 & Variação", r"CBO major group & 2010 & 2023 & 2010 & 2023 & Change"),
 (r"{Índice de portabilidade}", r"{Portability index}"), (r"{Grupos (\%)}", r"{Groups (\%)}"),
 (r"Ano & Vínculos & Ocup. & Média & DP &", r"Year & Jobs & Occ. & Mean & SD &"),
 (r"{Setor em queda, restante}", r"{Declining sector}"),
 (r"{Restante em expansão, queda relativa de}", r"{Relative decline}"),
 (r"Ano & \shortstack{Variação do\\emprego (\%)} & \shortstack{Restante em\\expansão} & em expansão & em queda &",
  r"Year & \shortstack{Employment\\change (\%)} & \shortstack{Rest of region\\expanding} & \shortstack{rest\\expanding} & \shortstack{rest\\declining} &"),
 (r"& Grupo & Demitidos & Índice & Idade & \shortstack{Mulheres\\(\%)} & \shortstack{Médio\\completo (\%)} & \shortstack{Meses no\\vínculo} & \shortstack{Salário\\(SM)}",
  r"& Group & Dismissed & Index & Age & \shortstack{Women\\(\%)} & \shortstack{High\\school (\%)} & \shortstack{Tenure\\(months)} & \shortstack{Wage\\(MW)}"),
 (r"Janela & Indicador & Média & P10 & P25 & Mediana & P75 & P90", r"Window & Indicator & Mean & P10 & P25 & Median & P75 & P90"),
 (r"& Em 24 meses &", r"& Within 24 months &"),
 (r"Coorte & Observações &", r"Cohort & Observations &"),
 (r"{Reempregados em até (\%)}", r"{Reemployed within (\%)}"),
 (r"Coorte & Grupo & Demitidos &", r"Cohort & Group & Dismissed &"),
 (r"Coorte & Grupo &", r"Cohort & Group &"), (r" & Grupo & ", r" & Group & "),
 (r"\shortstack{Até\\dez/2023}", r"\shortstack{By\\Dec 2023}"),
 (r"\shortstack{Mediana\\(meses)}", r"\shortstack{Median\\(months)}"),
 (r"\shortstack{Outro\\mun. (\%)}", r"\shortstack{Other\\mun. (\%)}"),
 (r"Grupo & Destino & Reempregados & Mediana (meses) & $\Delta \ln w$ & Erro-padrão", r"Group & Destination & Reemployed & Median (months) & $\Delta \ln w$ & Std. error"),
 (r"{Mudança de setor}", r"{Sector change}"), (r"{Tempo fora}", r"{Time out of work}"),
 (r"Coorte & Reempregados & Mudou & Portab. & Mudou $\times$ portab. & Meses & Meses $\times$ portab.",
  r"Cohort & Reemployed & Changed & Portab. & Changed $\times$ portab. & Months & Months $\times$ portab."),
 (r"{Taxa mensal (\%)}", r"{Monthly rate (\%)}"), (r"{Reempregados (\%)}", r"{Reemployed (\%)}"),
 (r"Tempo no vínculo & Grupo & Demitidos & \% do grupo & Meses 1 a 5 & Meses 6 a 8 &", r"Tenure & Group & Dismissed & \% of group & Months 1 to 5 & Months 6 to 8 &"),
 (r"Coorte & Menos de 6 meses no vínculo & 24 meses ou mais", r"Cohort & Tenure under 6 months & 24 months or more"),
 (r"Período & Entre ocupações & Dentro da ocupação & Termo cruzado & Total", r"Period & Between occupations & Within occupations & Cross term & Total"),
 (r"UF & Vínculos & Salário (SM) & Portab.", r"State & Jobs & Wage (MW) & Portab."),
 (r"Grupo & Ocupação & Vínculos & Setores & Índice", r"Group & Occupation & Jobs & Sectors & Index"),
 (r"Queda relativa &", r"Relative decline &"), (r"& Demitidos &", r"& Dismissed &"),
 # rotulos de linhas
 (r"Véspera da recessão", r"Pre-recession"), (r"Véspera da pandemia", r"Pre-pandemic"),
 (r"Recessão", r"Recession"), (r"Pandemia", r"Pandemic"),
 (r"Seis coortes", r"Six cohorts"), (r"Alta (G1)", r"High (G1)"), (r"Média (G2)", r"Medium (G2)"), (r"Baixa (G3)", r"Low (G3)"),
 (r"Mais portáteis", r"Most portable"), (r"Menos portáteis", r"Least portable"),
 (r"Mais flexíveis", r"Most flexible"), (r"Menos flexíveis", r"Least flexible"),
 (r"Índice de portabilidade (2010)", r"Portability index (2010)"), (r"Log do salário de origem", r"Log origin wage"),
 (r"Meses no vínculo", r"Tenure (months)"), (r"Fundamental completo", r"Completed elementary school"),
 (r"Médio completo", r"Completed high school"), (r"Superior completo", r"Completed college"),
 (r"Observações", r"Observations"), (r"Concordância", r"Concordance"), (r"Retornos observados", r"Reemployments observed"),
 (r"Idade", r"Age"), (r"Mulher", r"Female"),
 (r"Turbulência ($T$)", r"Turbulence ($T$)"), (r"Descasamento ($D$)", r"Mismatch ($D$)"),
 (r"Flexibilidade ($F$)", r"Flexibility ($F$)"), (r"Portabilidade média", r"Average portability"),
 (r"Mesma família, outra ocupação", r"Same family, other occupation"), (r"Mesma ocupação, mesmo setor", r"Same occupation, same sector"),
 (r"Mesma ocupação, outro setor", r"Same occupation, other sector"), (r"Outra família de ocupações", r"Other occupational family"),
 (r"24 meses ou mais", r"24 months or more"), (r"Menos de 6 meses", r"Under 6 months"),
 (r"Agropecuária, florestal e pesca", r"Agriculture, forestry and fishing"), (r"Dirigentes e gerentes", r"Directors and managers"),
 (r"Produção industrial (GG 7)", r"Industrial production (group 7)"), (r"Produção industrial (GG 8)", r"Industrial production (group 8)"),
 (r"Profissionais das ciências e das artes", r"Professionals in the sciences and arts"), (r"Reparação e manutenção", r"Repair and maintenance"),
 (r"Serviços administrativos", r"Administrative services"), (r"Serviços e vendedores do comércio", r"Service and sales workers"),
 (r"Técnicos de nível médio", r"Mid-level technicians"),
 # ocupacoes
 (r"Alimentador de linha de produção", r"Production line feeder"), (r"Assistente administrativo", r"Administrative assistant"),
 (r"Auxiliar de escritório", r"Office clerk"), (r"Cozinheiro geral", r"Cook"), (r"Faxineiro", r"Cleaner"),
 (r"Limpeza e conservação de áreas públicas", r"Public area cleaner"), (r"Motorista de caminhão", r"Truck driver"),
 (r"Operador de caixa", r"Cashier"), (r"Pedreiro", r"Bricklayer"), (r"Porteiro de edifícios", r"Building doorman"),
 (r"Recepcionista", r"Receptionist"), (r"Servente de obras", r"Construction laborer"),
 (r"Trabalhador agropecuário em geral", r"General farmworker"), (r"Vendedor de comércio varejista", r"Retail salesperson"),
 (r"Vigilante patrimonial", r"Security guard"),
 (r"Agente de proteção de aviação civil", r"Civil aviation security agent"), (r"Analista de recursos humanos", r"Human resources analyst"),
 (r"Cobrador de transportes coletivos (exceto trem)", r"Public transport fare collector (except rail)"),
 (r"Comissário de vôo", r"Flight attendant"), (r"Economista financeiro", r"Financial economist"),
 (r"Engenheiro de segurança do trabalho", r"Occupational safety engineer"), (r"Escrivão extra-judicial", r"Registry office clerk"),
 (r"Escrevente", r"Notary clerk"), (r"Gerente de pesquisa, desenvolvimento e inovação", r"Research, development and innovation manager"),
 (r"Gerente de produção e operações", r"Production and operations manager"),
 (r"Gerente de projetos e serviços de manutenção", r"Maintenance projects and services manager"),
 (r"Gerente de recursos humanos", r"Human resources manager"), (r"Instrutor de auto-escola", r"Driving instructor"),
 (r"Professor de ensino superior na área de orientação educacional", r"University lecturer in educational guidance"),
 (r"Professor de geografia do ensino fundamental", r"Geography teacher, elementary school"),
 (r"Professor de matemática no ensino médio", r"Mathematics teacher, high school"),
 (r"Professor de química no ensino médio", r"Chemistry teacher, high school"),
 (r"Técnico de manutenção elétrica", r"Electrical maintenance technician"),
 (r"Técnico de planejamento de produção", r"Production planning technician"),
 (r"Administrador", r"Administrator"),
]
T.sort(key=lambda x: -len(x[0]))
# tabelas que precisam de menos espaco entre colunas em ingles
SEP = {"t_retorno": "5.2pt", "t_seguro": "4pt"}
usados = set()
arqs = sorted(glob.glob(f"{PASTA}/*.tex"))
for f in arqs:
    nome = f.split("/")[-1][:-4]
    s = open(f, encoding="utf-8").read()
    # 1. numeros: milhar com ponto -> virgula; decimal com virgula -> ponto
    s = re.sub(r"(?<![\d.,])(\d{1,3}(?:\.\d{3})+)(?!\d)", lambda m: m.group(1).replace(".", "§"), s)
    s = re.sub(r"(\d)\{,\}(\d)", r"\1.\2", s)
    s = re.sub(r"(\d),(\d)", r"\1.\2", s)
    s = s.replace("§", ",")
    # 2. nota
    assert nome in NOTAS, nome
    nota = re.sub(r"\[\[([^|\]]+)\|([^\]]+)\]\]", remissao, NOTAS[nome])
    s, n = re.subn(r"\\item \\textit\{Nota:\}.*", lambda m: "\\item \\textit{Note:} " + nota + FONTE, s)
    assert n == 1, (nome, n)
    # 3. textos
    for i, (a, b) in enumerate(T):
        if a in s:
            s = s.replace(a, b); usados.add(i)
    s = re.sub(r"(\d{4}) a (\d{4})", r"\1 to \2", s)
    s = re.sub(r"(\d+) ou mais", r"\1 or more", s)
    s = re.sub(r"(?<![A-Za-zÀ-ú])Todos(?![A-Za-zÀ-ú])", "All", s)
    s = re.sub(r"(?<![A-Za-zÀ-ú])Dif\.", "Diff.", s)
    s = re.sub(r"(^|\n)Grupo &", r"\1Group &", s)
    if nome in SEP:
        if "\\setlength{\\tabcolsep}" in s:
            s = re.sub(r"\\setlength\{\\tabcolsep\}\{[\d.]+pt\}", lambda m: "\\setlength{\\tabcolsep}{%s}" % SEP[nome], s, count=1)
        else:
            s = s.replace("\\begin{threeparttable}", "\\setlength{\\tabcolsep}{%s}\n\\begin{threeparttable}" % SEP[nome], 1)
    open(f, "w", encoding="utf-8").write(s)
print(len(arqs), "tabelas traduzidas")

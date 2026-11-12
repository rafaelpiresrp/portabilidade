# Le os CSVs de resultado e escreve os arquivos .dat usados pelas figuras em
# pgfplots (fig/dados). Nao calcula nada novo sobre microdados: so reorganiza
# o que os scripts 01 a 05 ja salvaram.
import sys
import numpy as np
import pandas as pd

D = sys.argv[1]          # pasta com os CSVs
OUT = sys.argv[2]        # fig/dados

def salva(df, nome, **kw):
    df.to_csv(f"{OUT}/{nome}.dat", sep=" ", index=False, na_rep="nan",
              float_format="%.5f", **kw)

# ---------------------------------------------------------------- indice
broad = pd.read_csv(f"{D}/broadness.csv", dtype={"cbo": str})
base = pd.read_csv(f"{D}/broadness_base_cbo.csv", dtype={"cbo": str})

# distribuicao do emprego de 2010 por faixa do indice (largura 0,025)
b10 = broad[broad.ano == 2010]
bins = np.arange(0, 1.0001, 0.025)
h, _ = np.histogram(b10.broadness, bins=bins, weights=b10.vinculos)
salva(pd.DataFrame({"x": bins[:-1] + 0.0125, "pct": 100 * h / h.sum()}), "dist2010")

# media anual: indice do ano e indice fixo de 2010
m = broad.drop(columns=[c for c in ["broadness_base", "grupo"] if c in broad.columns])
m = m.merge(base[["cbo", "broadness_base"]], on="cbo", how="left")
ser = m.groupby("ano").apply(lambda x: pd.Series({
    "anual": np.average(x.broadness, weights=x.vinculos),
    "fixo": np.average(x.broadness_base, weights=x.vinculos)})).reset_index()
b0 = ser.loc[ser.ano == 2010].iloc[0]
ser["composicao"] = ser.fixo - b0.fixo        # mudanca no peso das ocupacoes
ser["dentro"] = ser.anual - ser.fixo          # mudanca do indice de cada ocupacao
ser["total"] = ser.anual - b0.anual
salva(ser, "serie")

ss = pd.read_csv(f"{D}/broadness_shiftshare_anual.csv")
salva(ss[["ano", "entre_ocup", "dentro_ocup", "cruzado", "total"]], "shiftshare")

gr = pd.read_csv(f"{D}/broadness_grupos.csv")
salva(gr.pivot(index="ano", columns="grupo", values="pct_vinc")
        .rename(columns=lambda c: f"g{c}").reset_index(), "grupos")

pc = pd.read_csv(f"{D}/broadness_percentis.csv")
salva(pc[["ano", "p10", "p25", "p50", "p75", "p90", "media_pond"]], "percentis")

# ---------------------------------------------------------------- regioes
fl = pd.read_csv(f"{D}/flexibilidade.csv", dtype={"regiao": str})
it = fl[fl.nivel == "intermediaria"]
an = it[it.t1 == it.t0 + 1].groupby("t1")[["T_turb", "D_desc"]].median().reset_index()
an.columns = ["ano", "T", "D"]
salva(an, "td_anual")

for nome, (t0, t1) in {"rec": (2014, 2019), "pan": (2019, 2022)}.items():
    w = it[(it.t0 == t0) & (it.t1 == t1)].copy()
    w["lsal"] = np.log(w.salario)
    salva(w[["regiao", "T_turb", "D_desc", "F_flex", "portab_media", "salario",
             "lsal", "emprego_t0"]], f"regioes_{nome}")

# natureza do choque: emprego em celulas regiao x setor que encolheram,
# separado pelo que aconteceu com o resto da regiao. Mesma definicao do
# 06_choque_idiossincratico_critico.R: g_setor_regiao do 02 (minimo de 10
# vinculos no setor) e resto da regiao com minimo de 50 vinculos.
c = pd.read_csv(f"{D}/choque_setorial_regiao.csv", dtype={"regiao": str, "div": str})
tot = c.groupby(["regiao", "ano"]).agg(er=("emp", "sum"), ea=("emp_ant", "sum")).reset_index()
c = c.merge(tot, on=["regiao", "ano"])
c["r1"] = c.er - c.emp
c["r0"] = c.ea - c.emp_ant
c["gr"] = np.where(c.r0 >= 50, (c.r1 - c.r0) / c.r0, np.nan)
c["g"] = c.g_setor_regiao
c = c.dropna(subset=["g", "gr"])
c["d"] = c.g - c.gr

def nat(x):
    w = x.emp_ant
    W = w.sum()
    return pd.Series({
        "cai_resto_cresce": 100 * w[(x.g < 0) & (x.gr > 0)].sum() / W,
        "cai_resto_cai": 100 * w[(x.g < 0) & (x.gr <= 0)].sum() / W,
        "idio5": 100 * w[(x.d <= -.05) & (x.gr > 0)].sum() / W,
        "idio10": 100 * w[(x.d <= -.10) & (x.gr > 0)].sum() / W,
        "idio20": 100 * w[(x.d <= -.20) & (x.gr > 0)].sum() / W,
        "resto_cresce": 100 * w[x.gr > 0].sum() / W,
        "cresc": 100 * (x.emp.sum() / x.emp_ant.sum() - 1)})
nt = c.groupby("ano").apply(nat).reset_index()
salva(nt, "natureza")
nt.round(1).to_csv(f"{OUT}/natureza_tabela.csv", index=False)

# ---------------------------------------------------------------- retorno
km = pd.read_csv(f"{D}/transicoes_km.csv")
for co in km.coorte.unique():
    w = km[km.coorte == co].pivot(index="mes", columns="grupo", values="voltou_ate") * 100
    w.columns = [f"g{c}" for c in w.columns]
    salva(w.reset_index(), f"km_{co}")

cal = pd.read_csv(f"{D}/transicoes_calendario.csv")
w = cal[cal.grupo == 0].pivot(index="ano_retorno", columns="coorte", values="pct_acum")
salva(w.reset_index().rename(columns={"ano_retorno": "ano"}), "calendario")

co = pd.read_csv(f"{D}/t12_coeficientes.csv")
w = co[["ano", "b_12m_w0", "se_12m_w0", "b_24m_ef", "se_24m_ef", "b_cox", "se_cox",
        "b_sal_ef", "se_sal_ef"]].copy()
w["pos"] = range(1, len(w) + 1)
for k in ["12m_w0", "24m_ef", "cox", "sal_ef"]:
    w[f"ic_{k}"] = 1.96 * w[f"se_{k}"]
salva(w, "coef")

cp = pd.read_csv(f"{D}/transicoes_composicao.csv")
w = cp[(cp.coorte == "total") & (cp.destino != "0 Nao voltou")]
w = w.pivot(index="grupo", columns="destino", values="pct_cond")
w.columns = ["mesma_mesmo", "mesma_outro", "familia", "outra"]
salva(w.reset_index(), "destino")

# ---------------------------------------------------------------- escada
es = pd.read_csv(f"{D}/escada_choque_idiossincratico.csv")
es["degrau"] = es.corte.str[0].astype(int)
es["bloco"] = es.Bloco.str[0]
for b in "ABCD":
    salva(es[es.bloco == b].drop(columns=["Bloco", "corte", "bloco"]), f"escada_{b}")
df = pd.read_csv(f"{D}/escada_choque_reponderado_DFL.csv")
df["degrau"] = df.corte.str[0].astype(int)
df["dlog1"] = 100 * df.dlog_ajust_1
df["dlog3"] = 100 * df.dlog_ajust_3
df["difw"] = 100 * df.Dif_dlogw
salva(df.drop(columns=["corte"]), "escada_dfl")

kc = pd.read_csv(f"{D}/km_choque_idiossincratico_comparado.csv")
for r, nome in [("Choque_Idio_Bruto", "bruto"),
                ("Choque_Idio_Homogeneo_SemSeguro_Ate2SM", "homog")]:
    w = kc[kc.recorte == r].pivot(index="mes", columns="grupo", values="voltou_ate")
    w.columns = [f"g{c}" for c in w.columns]
    w["dif"] = w.g1 - w.g3
    salva(w.reset_index(), f"kmchoque_{nome}")

# ---------------------------------------------------------------- seguro e salario
hz = pd.read_csv(f"{D}/transicoes_hazard_tenure.csv")
for f, nome in [("Seguro 5 parcelas (>= 24 meses)", "seg5"), ("Sem seguro (< 6 meses)", "semseg")]:
    w = hz[hz.faixa_seguro == f].pivot(index="mes", columns="grupo", values="hazard_pct")
    w.columns = [f"g{c}" for c in w.columns]
    salva(w.reset_index(), f"hazard_{nome}")

sd = pd.read_csv(f"{D}/transicoes_salario_duracao.csv")
w = sd[sd.coorte == "total"].copy()
w["d"] = 100 * w.d_log_med
w = w.pivot(index="meses", columns="grupo", values="d")
w.columns = [f"g{c}" for c in w.columns]
salva(w.reset_index(), "salario_duracao")

st = pd.read_csv(f"{D}/transicoes_salario_destino.csv")
w = st[st.coorte == "total"].copy()
w["d"] = 100 * w.d_log_med
w = w.pivot(index="destino", columns="grupo", values="d")
w.columns = [f"g{c}" for c in w.columns]
w["pos"] = range(1, 5)
salva(w.reset_index(drop=True), "salario_destino")
print("ok")

# ---------------------------------------------------------------- contexto
# PIB (SIDRA 5932, taxa acumulada em quatro trimestres) e taxa de desocupacao
# (SIDRA 4099), baixados em ext/. Trimestre q do ano a fica em a + (q - 0.5)/4.
import json, os
EXT = os.path.join(os.path.dirname(OUT.rstrip("/")), "..", "ext")
if os.path.exists(f"{EXT}/pib.json"):
    a = pd.DataFrame(json.load(open(f"{EXT}/pib.json"))[1:])
    a["v"] = pd.to_numeric(a.V, errors="coerce")
    a["x"] = a.D3C.str[:4].astype(int) + (a.D3C.str[4:].astype(int) - 0.5) / 4
    nomes = {"Agropecuária - total": "agro", "Indústria - total": "ind",
             "Serviços - total": "serv", "PIB a preços de mercado": "pib"}
    a["s"] = a.D4N.map(nomes)
    salva(a.pivot(index="x", columns="s", values="v").reset_index(), "ctx_pib")
    b = pd.DataFrame(json.load(open(f"{EXT}/pnad.json"))[1:])
    b["v"] = pd.to_numeric(b.V, errors="coerce")
    b["x"] = b.D3C.str[:4].astype(int) + (b.D3C.str[4:].astype(int) - 0.5) / 4
    salva(b[["x", "v"]].rename(columns={"v": "desoc"}), "ctx_pnad")
# emprego formal privado em 31/12 (RAIS), em milhoes, no fim de cada ano
e = pc[["ano", "vinculos"]].copy()
e["x"] = e.ano + 1.0
e["emp"] = e.vinculos / 1e6
salva(e[["x", "ano", "emp"]], "ctx_rais")

# ---------------------------------------------------------------- regioes: nivel e variacao
bz = it[it.t1 == it.t0 + 1].pivot(index="regiao", columns="t0", values="emprego_t0")
sz = it[it.t1 == it.t0 + 1].pivot(index="regiao", columns="t0", values="salario")
pz = it[it.t1 == it.t0 + 1].pivot(index="regiao", columns="t0", values="portab_media")
rg = pd.DataFrame({"emp2014": bz[2014], "sal2014": sz[2014], "portab2014": pz[2014],
                   "var1416": 100 * (bz[2016] / bz[2014] - 1),
                   "var1920": 100 * (bz[2020] / bz[2019] - 1),
                   "var1921": 100 * (bz[2021] / bz[2019] - 1)}).reset_index()
f1 = it[(it.t0 == 2014) & (it.t1 == 2019)].set_index("regiao")
f2 = it[(it.t0 == 2019) & (it.t1 == 2022)].set_index("regiao")
rg["F1419"] = rg.regiao.map(f1.F_flex)
rg["F1922"] = rg.regiao.map(f2.F_flex)
rg["T1419"] = rg.regiao.map(f1.T_turb)
rg["D1419"] = rg.regiao.map(f1.D_desc)
rg["tercil_sal"] = pd.qcut(rg.sal2014, 3, labels=[1, 2, 3]).astype(int)
rg["raio"] = 1.1 + 4.6 * np.sqrt(rg.emp2014 / rg.emp2014.max())
rg.to_csv(f"{OUT}/regioes_nivel.csv", index=False)
for k in (1, 2, 3):
    salva(rg[rg.tercil_sal == k], f"regioes_sal{k}")
salva(rg, "regioes_todas")
print(rg[["emp2014", "sal2014", "var1416", "var1920", "var1921", "F1419"]].describe().round(2))
print(rg[["portab2014", "F1419", "F1922", "sal2014", "emp2014", "var1416", "var1920"]].assign(
    le=np.log(rg.emp2014), ls=np.log(rg.sal2014)).corr(method="spearman").round(2))
print(rg.groupby("tercil_sal")[["portab2014", "F1419", "F1922", "sal2014", "emp2014", "var1416", "var1920"]].median().round(3))

# Escreve os mapas como caminhos TikZ (fig/dados/mapa_*.tex), com legendas em ingles.
# Malha: regioes intermediarias de 2020 do geobr (versao simplificada), a mesma
# usada no 05_figuras.R. As classes sao quintis calculados sobre as duas
# janelas juntas, como nos paineis do 05_figuras.R.
import sys, json, math
import numpy as np
import pandas as pd
import geopandas as gpd
import shapely

D, OUT = sys.argv[1], sys.argv[2]
MALHA = f"{OUT}/malha_regioes.geojson"
K = 0.196                       # cm por grau
COSLAT = math.cos(math.radians(14))
LON0, LAT0 = -74.0, -33.8

def carrega_malha(bruto=None):
    if bruto:
        g = gpd.read_file(bruto)
        g["regiao"] = g.code_intermediate.astype(int).astype(str)
        g["uf"] = g.code_state.astype(int).astype(str)
        g = g[["regiao", "uf", "geometry"]]
        try:
            g["geometry"] = shapely.coverage_simplify(g.geometry.values, 0.04)
        except Exception:
            g["geometry"] = g.geometry.simplify(0.04, preserve_topology=True)
        g.to_file(MALHA, driver="GeoJSON", coordinate_precision=3)
    return gpd.read_file(MALHA)

def aneis(geom, area_min=0.02):
    polys = list(geom.geoms) if geom.geom_type == "MultiPolygon" else [geom]
    maior = max(p.area for p in polys)
    for p in polys:
        if p.area < area_min and p.area < maior:
            continue                       # ilhas muito pequenas
        yield p.exterior.coords
        for i in p.interiors:
            yield i.coords

def caminho(geom):
    partes = []
    for co in aneis(geom):
        pts = [f"({(x - LON0) * K * COSLAT:.3f},{(y - LAT0) * K:.3f})" for x, y in co[:-1]]
        partes.append("--".join(pts) + "--cycle")
    return " ".join(partes)

bruto = sys.argv[3] if len(sys.argv) > 3 else None
g = carrega_malha(bruto)
g["regiao"] = g.regiao.astype(str)
g["uf"] = g.uf.astype(str)
g["tikz"] = g.geometry.apply(caminho)

# contorno das UFs
ufs = g.dissolve(by="uf")
with open(f"{OUT}/mapa_ufs.tex", "w") as f:
    for _, r in ufs.iterrows():
        f.write(f"\\draw[ufborda] {caminho(r.geometry)};\n")

# ----------------------------------------------------------------------------
# Cada mapa e um arquivo fig/dados/mapa_<nome>.tex com um \fill por regiao.
# Classes sequenciais (c1..c5) sao quintis; quando ha dois paineis do mesmo
# indicador, os quintis sao calculados com os dois juntos. A variacao do
# emprego usa classes fixas e cores divergentes (v1..v5).
# ----------------------------------------------------------------------------
fl = pd.read_csv(f"{D}/flexibilidade.csv", dtype={"regiao": str})
it = fl[fl.nivel == "intermediaria"]
rg = pd.read_csv(f"{OUT}/regioes_nivel.csv", dtype={"regiao": str}).set_index("regiao")
leg = []

def escreve_mapa(nome, serie, q, pref="c"):
    m = g.merge(serie.rename("v"), left_on="regiao", right_index=True, how="left")
    m["cl"] = np.clip(np.searchsorted(q, m.v, side="right"), 1, len(q) - 1)
    with open(f"{OUT}/mapa_{nome}.tex", "w") as f:
        for _, r in m.iterrows():
            est = "csem" if pd.isna(r.v) else f"{pref}{int(r.cl)}"
            f.write(f"\\fill[{est}] {r.tikz};\n")

def legenda(nome, q, dig, esc=1.0, abertos=False):
    for i in range(len(q) - 1):
        a, b = fmt(q[i] * esc, dig), fmt(q[i + 1] * esc, dig)
        if abertos and i == 0:
            txt = f"below {b}"
        elif abertos and i == len(q) - 2:
            txt = f"above {a}"
        else:
            txt = f"{a} to {b}"
        leg.append(f"\\expandafter\\def\\csname leg{nome}{'abcde'[i]}\\endcsname{{{txt}}}")

def fmt(v, dig):
    s = f"{abs(v):.{dig}f}"
    return ("$-$" if v < 0 else "") + s

def quintis(*series):
    return np.quantile(pd.concat(series).dropna(), [0, .2, .4, .6, .8, 1])

q = quintis(rg.emp2014);    escreve_mapa("emp", rg.emp2014, q);       legenda("emp", q, 0, 1e-3)
q = quintis(rg.sal2014);    escreve_mapa("sal", rg.sal2014, q);       legenda("sal", q, 2)
q = quintis(rg.portab2014); escreve_mapa("portab", rg.portab2014, q); legenda("portab", q, 3)
q = quintis(rg.F1419, rg.F1922)
escreve_mapa("flex_rec", rg.F1419, q); escreve_mapa("flex_pan", rg.F1922, q); legenda("flex", q, 2)
for var, nome, dig in [("T_turb", "turb", 3), ("D_desc", "desc", 3)]:
    a = it[(it.t0 == 2014) & (it.t1 == 2019)].set_index("regiao")[var]
    b = it[(it.t0 == 2019) & (it.t1 == 2022)].set_index("regiao")[var]
    q = quintis(a, b)
    escreve_mapa(f"{nome}_rec", a, q); escreve_mapa(f"{nome}_pan", b, q); legenda(nome, q, dig)
qv = np.array([-1e9, -10, -5, 0, 5, 1e9])
escreve_mapa("var_rec", rg.var1416, qv, "v"); escreve_mapa("var_pan", rg.var1920, qv, "v")
for i, txt in enumerate(["below $-$10", "$-$10 to $-$5", "$-$5 to 0", "0 to 5", "above 5"]):
    leg.append(f"\\expandafter\\def\\csname legvar{'abcde'[i]}\\endcsname{{{txt}}}")
open(f"{OUT}/mapa_legendas.tex", "w").write("\n".join(leg) + "\n")
print("mapas ok", len(g), "regioes")

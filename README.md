# Occupational portability and reemployment after displacement

Code and aggregated results for the paper **Occupational Portability and Reemployment after Displacement: Aggregate and Idiosyncratic Shocks in Brazil's Regional Labor Markets**, by Rafael Pires de Oliveira and Carlos R. Azzoni (University of São Paulo).

Latest version of the paper: [`artigo/portabilidade.pdf`](artigo/portabilidade.pdf)

The paper uses the identified microdata of the Relação Anual de Informações Sociais (RAIS), 2009 to 2023. These data are restricted and are not in this repository. The repository has the code that processes them, the aggregated results that the code writes, and everything needed to rebuild the tables and figures of the paper from those results.

## Structure

| Path | Contents |
| --- | --- |
| `config.R` | Paths and parameters shared by the R scripts (base year 2010, three portability groups, cohorts, flexibility windows) |
| `R/` | Scripts that run on the identified RAIS microdata |
| `dados/externos/` | Public classifications: CBO 2002 occupations and major groups, IBGE municipality codes |
| `saidas/dados/` | Aggregated results written by the R scripts (CSV) |
| `saidas/tabelas/` | Tables written by `04_tabelas.R`, read when building the tables of the paper |
| `artigo/` | The paper: LaTeX source, latest PDF, figures, tables and the scripts that build them |

## R scripts

| Script | What it does | Main outputs in `saidas/dados/` |
| --- | --- | --- |
| `01_broadness.R` | Portability index of each occupation by year, groups fixed in 2010, shift-share decomposition | `broadness*.csv` |
| `02_flexibilidade.R` | Turbulence, mismatch and allocative flexibility by intermediate region and state; employment growth by region, sector and year | `flexibilidade.csv`, `choque_setorial_regiao.csv`, `portabilidade_regiao.csv`, `salario_regiao.csv`, `depara_municipio_regiao.csv` |
| `03_transicoes.R` | Cohorts of workers dismissed without cause (2014, 2015, 2016, 2019, 2020, 2021) and their reemployment through December 2023 | `transicoes_*.csv` |
| `04_tabelas.R` | Linear probability, Cox and wage-change regressions, and tables | `t12_coeficientes.csv`, `saidas/tabelas/` |
| `05_figuras.R` | Figures in ggplot2 (the figures of the paper are built in `artigo/`) | `saidas/figuras/` |
| `06_choque_idiossincratico_critico.R` | Reemployment by relative decline of the origin sector (leave-one-out) and reweighting by wage and tenure | `escada_*.csv`, `km_choque_idiossincratico_comparado.csv` |

To run them, set `RAIZ` (this folder) and `DIR_CSV` (the RAIS files) in `config.R` and run the scripts in order from the repository root, for example `Rscript R/01_broadness.R`. Packages: `data.table`, `readxl`, `survival`, `fixest`, and `ggplot2`, `sf` and `geobr` for `05_figuras.R`.

`03_transicoes.R` also writes worker-level files (`duracao_c*.csv`, one row per dismissed worker, 1 to 2 GB each), which `06_choque_idiossincratico_critico.R` reads. They stay on the server and are excluded by `.gitignore`. All files in `saidas/` are aggregates by occupation, sector, region, cohort or portability group.

## Rebuilding the paper

Requirements: Python 3 with `pandas`, `numpy`, `geopandas` and `shapely`, and a TeX distribution with `pgfplots`. From `artigo/`:

```
python3 scripts/prepara_dados.py ../saidas/dados fig/dados        # series for the figures
python3 scripts/gera_mapas.py ../saidas/dados fig/dados           # maps, drawn in TikZ
python3 scripts/gera_tabelas.py ../saidas/dados ../saidas/tabelas tab
python3 scripts/traduz_tabelas.py tab                             # English labels and number format
bash scripts/compila_figuras.sh                                   # fig/*.tex -> fig/*.pdf
pdflatex portabilidade.tex                                        # three times
```

`gera_tabelas.py` writes the tables with Portuguese labels, as in the first version of the paper, and `traduz_tabelas.py` converts them to the English version. `ext/` has the GDP and unemployment series downloaded from SIDRA (IBGE tables 5932 and 4099), and `fig/dados/malha_regioes.geojson` has simplified boundaries of the 133 intermediate regions, from the `geobr` package. The figure sources (`fig/*.tex`) and their compiled PDFs are both in the repository, so the paper also compiles without running the scripts.

#!/bin/bash
# Compila cada figura (fig/f*.tex) em PDF. Uso: scripts/compila_figuras.sh [f01 f02 ...]
cd "$(dirname "$0")/../fig" || exit 1
alvos=("$@"); [ ${#alvos[@]} -eq 0 ] && alvos=(f*.tex)
for f in "${alvos[@]}"; do
  f=$(ls ${f%.tex}*.tex 2>/dev/null | head -1); [ -z "$f" ] && continue
  if pdflatex -interaction=nonstopmode -halt-on-error "$f" > /tmp/fig_$$.log 2>&1; then
    echo "ok   $f"
  else
    echo "ERRO $f"; grep -A6 "^!" /tmp/fig_$$.log | head -20
  fi
done
rm -f *.aux *.log /tmp/fig_$$.log

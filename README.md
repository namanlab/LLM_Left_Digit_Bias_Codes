# Ninety-Nine Cents: Price-Ending Bias in Language Models

Replication code and data for "Ninety-Nine Cents: Price-Ending Bias in Language Models Is Learned, Not Perceived".

## Repository structure

```
data/
  panels.zip        Processed experiment panels (one CSV per arm)
  tags.zip          Rendered price-tag images for the vision experiment

notebooks/
  01_collect.ipynb   Data collection: queries model APIs and writes raw logs

analysis/
  build_panels.py    Converts raw API logs into tidy per-arm panels
  analyse.R          Produces all estimates, figures, tables, and LaTeX macros
  corpus_endings.py  Counts price-ending frequencies in public corpora (infini-gram API)
  make_tag_figure.py Assembles rendered price tags into a single figure

output/
  figures/           All paper figures (PDF)
  tables/            All paper tables (LaTeX fragments) and numbers.tex macros
  *.csv              Intermediate estimate files
```

## Requirements

**Python** (>=3.9): `openai`, `anthropic`, `google-generativeai`, `Pillow`, `pandas`

**R** (>=4.1): `data.table`, `ggplot2`

## Reproducing the analysis

1. **Unzip the data.** Extract `data/panels.zip` into `data/panels/`.

2. **Run the analysis.**
   ```bash
   Rscript analysis/analyse.R
   ```
   This reads every panel in `data/panels/`, runs block bootstrap inference (400 replications, fixed seed), and writes all figures to `output/figures/` and all tables to `output/tables/`. The file `output/tables/numbers.tex` contains every in-text number used in the paper.

3. **Corpus frequencies** (optional, requires internet):
   ```bash
   python analysis/corpus_endings.py
   ```

## Key results

- Corrected left-digit effect (core sweep, E1): -3.25 pp [-5.64, -0.99]
- Matched design estimate (E0): -6.14 pp [-8.36, -3.86]
- Stated-convention bump: ~36 pp shift from a single sentence
- 21 models tested across 683,559 experimental cells

## Licence

MIT

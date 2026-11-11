# Ninety-Nine Cents: Price-Ending Bias in Language Models Tracks Learned Conventions

Replication code and data for the paper (revised version). The analysis runs offline in about two minutes, with no API key and no GPU, and reproduces every table, figure and in-text number.

## Contents

```
data/
  panels.zip                    Experiment panels, one CSV per experiment (input to the analysis)
  corpus_endings.csv            Raw counts of "$D.CC" price strings in three public corpora (E5)
  ending_frequency_profile.csv  Share of each price ending in those corpora (read by the analysis)
  tags.zip                      Rendered price-tag images shown to the vision models (E7)

analysis/
  analyse.R           All estimates, tables, figures and the in-text numbers
  corpus_endings.py   Corpus counts (infini-gram API) and the ending profile
  build_panels.py     Rebuilds the panels from raw call logs (logs not included)
  make_tag_figure.py  Assembles Figure 5 from the rendered tags

notebooks/
  01_collect.ipynb    Data collection: queries the model APIs and writes the panels

output/
  tables/             LaTeX tables, plus numbers.tex with every number used in the text
  figures/            All figures (PDF)
  *.csv               Estimates in tidy form
```

## Reproducing the results

```bash
unzip data/panels.zip -d data/          # creates data/panels/E0.csv ... E9.csv
Rscript analysis/analyse.R              # writes output/
```

The script reads the 13 panels and the corpus profile, estimates every experiment, and writes all tables to `output/tables/`, all figures to `output/figures/` (except Figure 5, see below) and the estimates to `output/*.csv`. Every number in the text of the paper is a macro in `output/tables/numbers.tex`.

Tested with R 4.5.3, data.table 1.18.2.1, ggplot2 4.0.2 and patchwork 1.3.2 on a laptop with 16 GB of memory: about two minutes, peak memory about 1.2 GB. patchwork is needed for the multi-panel Figures 1, 3 and 7.

Options:

- `--boot=N` sets the number of bootstrap resamples (default 400).
- `--out=DIR` writes to another folder instead of `output/`.
- `--copyto=DIR1,DIR2` copies the figures and tables into other folders after the run.

Inference uses block bootstraps with a fixed seed (set at the top of `analyse.R`): 400 resamples for experiment-level estimates, 133 for condition-level and model-level estimates and the E10 ending effects, and 200 for the E7 cents-size slope. With the default settings, the tables and estimate files match those in `output/` exactly.

### Key numbers to check

| Quantity | Macro in `numbers.tex` | Value |
|---|---|---|
| Matched design (E0): leftmost digit changes vs. does not | `Mroll`, `MrollLo`, `MrollHi` | -6.14 [-8.36, -3.86] |
| Conservative bound (E1): step 10 minus the mean placebo step | `SpecNaive`, `SpecNaiveLo`, `SpecNaiveHi` | -2.50 [-3.91, -1.28] |
| Roundness-corrected estimate (E1) | `E1LD`, `E1Lo`, `E1Hi` | -3.25 [-5.64, -0.99] |
| Prompt phrasing (E2), pooled | `E2LD`, `E2Lo`, `E2Hi` | -8.91 [-11.04, -6.93] |
| Stated convention (E10): effect of .33 when it is named | `BumpP3333`, `BumpP3333Lo`, `BumpP3333Hi` | 36.02 [29.79, 42.31] |
| Ending profile vs. corpus frequency (Spearman) | `CorpusRho` | 0.333 |
| Cells and models | `NCells`, `NModels` | 683,559 and 21 |

## Experiments, data files and outputs

| Code | Experiment | Data | Paper tables and figures |
|---|---|---|---|
| E1 | Core sweep | `panels/E1.csv` | Tables 7, 8, 37, 38 (rows 2 to 5), 40, 42; Figures 1, 3(b), 7(a), 11 |
| E1f | Newest models | `panels/E1f.csv` | Table 9 |
| E1b | Scaled prices | `panels/E1b.csv` | Tables 10, 11 |
| E2 | Prompt phrasing | `panels/E2.csv` | Tables 12, 13, 14; Figure 8 |
| E3 | Non-price quantities | `panels/E3.csv` | Tables 15, 16; Figure 4 |
| E4 | Currency and locale | `panels/E4.csv` | Tables 17, 18; Figure 9 |
| E5 | Corpus counts | `corpus_endings.csv`, `ending_frequency_profile.csv` | Tables 30, 37, 38 (row 4); Figure 3 |
| E6 | Number formatting | `panels/E6.csv` | Tables 19, 20 |
| E7 | Rendered price tags | `panels/E7.csv`, `tags.zip` | Tables 21, 22, 23; Figures 5, 6, 10 |
| E8 | Explicit reasoning | `panels/E8.csv` | Tables 24, 25 |
| E9 | Arithmetic probes | `panels/E9.csv` | Table 26 |
| E10 | Stated convention | `panels/E10.csv` | Tables 27, 28, 29; Figure 2 |
| E0 | Matched design | `panels/E0.csv` | Tables 31, 32, 33, 38 (row 1); Figure 7(b) |
| E0b | Digit position | `panels/E0b.csv` | Tables 34, 35, 36 |
| All | | all panels | Tables 1, 39, 41, 43, 44, 45 |

Tables 2 to 6 (products, personas, prompt templates, models and experiments) are written directly in the paper.

### Paper table or figure to output file

| Paper | File |
|---|---|
| Table 1 | `output/tables/coverage.tex` |
| Table 7 | `output/tables/tab_E1_model.tex` |
| Table 8 | `output/tables/by_model.tex` |
| Table 9 | `output/tables/tab_E1f_model.tex` |
| Table 10 | `output/tables/tab_E1b_cond.tex` |
| Table 11 | `output/tables/tab_E1b_model.tex` |
| Table 12 | `output/tables/tab_E2_cond.tex` |
| Table 13 | `output/tables/tab_E2_model.tex` |
| Table 14 | `output/tables/tab_E2_heterogeneity.tex` |
| Table 15 | `output/tables/tab_E3_cond.tex` |
| Table 16 | `output/tables/tab_E3_model.tex` |
| Table 17 | `output/tables/tab_E4_cond.tex` |
| Table 18 | `output/tables/tab_E4_model.tex` |
| Table 19 | `output/tables/tab_E6_cond.tex` |
| Table 20 | `output/tables/tab_E6_model.tex` |
| Table 21 | `output/tables/vision.tex` |
| Table 22 | `output/tables/tab_E7_model.tex` |
| Table 23 | `output/tables/vision_trend.tex` |
| Table 24 | `output/tables/tab_E8_cond.tex` |
| Table 25 | `output/tables/tab_E8_model.tex` |
| Table 26 | `output/tables/arithmetic.tex` |
| Table 27 | `output/tables/installed.tex` |
| Table 28 | `output/tables/tab_E10_cond.tex` |
| Table 29 | `output/tables/tab_E10_model.tex` |
| Table 30 | `output/tables/corpus.tex` |
| Table 31 | `output/tables/tab_E0_cond.tex` |
| Table 32 | `output/tables/matched.tex` |
| Table 33 | `output/tables/matched_by_model.tex` |
| Table 34 | `output/tables/tab_E0b_cond.tex` |
| Table 35 | `output/tables/matched_e0b.tex` |
| Table 36 | `output/tables/matched_e0b_by_model.tex` |
| Table 37 | `output/tables/roundness_levels.tex` |
| Table 38 | `output/tables/roundness_specs.tex` |
| Table 39 | `output/tables/main_estimates.tex` |
| Table 40 | `output/tables/steps.tex` |
| Table 41 | `output/tables/robustness.tex` |
| Table 42 | `output/tables/loo.tex` |
| Table 43 | `output/tables/by_condition.tex` |
| Table 44 | `output/tables/by_condition2.tex` |
| Table 45 | `output/tables/multiplicity.tex` |
| Figure 1 | `output/figures/fig_walkthrough.pdf` |
| Figure 2 | `output/figures/fig_installed.pdf` |
| Figure 3 | `output/figures/fig_corpus.pdf` |
| Figure 4 | `output/figures/fig_E3_forest.pdf` |
| Figure 5 | `output/figures/fig_tags.pdf` (from `make_tag_figure.py`) |
| Figure 6 | `output/figures/fig_vision_by_model.pdf` |
| Figure 7 | `output/figures/fig_identification.pdf` |
| Figure 8 | `output/figures/fig_E2_forest.pdf` |
| Figure 9 | `output/figures/fig_E4_forest.pdf` |
| Figure 10 | `output/figures/fig_vision_by_model.pdf` |
| Figure 11 | `output/figures/fig_roundness.pdf` |

## Panel columns

One row is one cell: one prompt (one model, product, persona, price and experimental condition). For models without log-probabilities, a cell averages 12 sampled answers. A column that an experiment does not vary is constant or empty in that file.

| Column | Meaning |
|---|---|
| `experiment` | Experiment code (E0 to E10) |
| `model` | Model identifier, `provider/name` |
| `product`, `persona` | Product and persona IDs (paper, Appendices A.1 and A.2) |
| `template` | Prompt template, T01 to T12 (varied in E2; T01 elsewhere) |
| `currency` | Currency (varied in E4) |
| `rendering` | Number format (varied in E6): `plain` ("$12.99"), `spaced` ("$ 12.99"), `digitspace` ("$ 1 2 . 9 9"), `iso` ("USD 12.99"), `words` (currency written as a word) |
| `cents_cond` | Tag condition (varied in E7): `textonly`, `same` (cents at 100%), `r70`, `r50`, `r35`, `sup` |
| `domain` | Quantity (varied in E3); `price` elsewhere |
| `unit` | Price scale (varied in E1b): 1, 10, 100 |
| `probe` | Arithmetic probe (E9): `compare` or `magnitude` |
| `preamble_id` | Stated convention (E10 only): `none`, `p00`, `p33`, `p77` |
| `price` | Posted price (the posted quantity in E3) |
| `p_buy` | Purchase probability. For log-probability models, Equation 1 of the paper; for sampled models, the share of YES answers. In E9, the probability or share of a correct answer (`compare`) or the stated difference in cents (`magnitude`) |
| `p_sd` | Standard deviation across repeated draws (empty for single-call cells) |
| `n_obs` | Number of API calls behind the cell |
| `cost_usd` | API cost of those calls |
| `cot_tokens` | Number of reasoning tokens returned with the answer (0 in every released panel) |
| `ref_price` | The product's reference price |
| `anchor` | Dollar anchor b of the block (prices run from b.00 to (b+1).00) |
| `ending` | Cents of the price; 0 for both b.00 and (b+1).00, so `analyse.R` recovers the position in the block from `price - anchor` |
| `dollars` | Dollar part of the price |
| `decade_step`, `crosses_dollar`, `roundness` | For prices ending in a multiple of ten: the one-cent step that ends at this price, whether that step crosses a dollar, and the roundness score of the ending. `analyse.R` recomputes these from `price` and `anchor` |
| `deliberation` | The persona's deliberation level (Appendix A.2): `high`, `low`, `load` (P6), or `none` (P0) |
| `tier`, `family`, `rsn` | Model tier (Appendix A.4), model family label, and whether the model is a reasoning model |
| `stack` | API route: `openai` or `openrouter` |
| `mode` | How purchase probability was measured: `logprob`, `sample` (12 draws per cell) or `logprob_after_cot` |

## Corpus counts (E5)

`data/corpus_endings.csv` holds the counts of `$D.CC` for 10 dollar values and the 30 endings of the price grid in C4, Dolma and RedPajama, from the public infini-gram API; 21 of the 900 queries returned no count. `data/ending_frequency_profile.csv` gives, for each corpus, the share of each ending among the counted prices with the same dollar value, averaged over the dollar values.

```bash
python3 analysis/corpus_endings.py --profile-only   # rebuild the profile from the counts, offline
python3 analysis/corpus_endings.py                  # re-query the API and rebuild both files
```

Re-querying can return different counts if the indexes are updated; the files included here are the counts used in the paper. Both commands use only the Python standard library.

## Data collection (optional; requires API keys and incurs cost)

`notebooks/01_collect.ipynb` queries the models through the OpenAI and OpenRouter APIs and writes one panel per experiment to `data/panels/`. It reads keys from a file `api.txt` at the repository root, which is not included (for example, `OPENROUTER_API_KEY=...` and `OPENAI_API_KEY=...`, one per line). It uses `openai`, `anthropic`, `pandas`, `numpy`, `Pillow`, `matplotlib`, `tiktoken` and `tqdm`.

`analysis/build_panels.py` rebuilds the panels from the raw call logs in `data/raw/`; the raw logs are not included, and the panels in `data/panels.zip` are the input to the analysis. To rebuild Figure 5, extract the tags with `unzip data/tags.zip -d data/` and run `python3 analysis/make_tag_figure.py` (requires `matplotlib`).

## Changes since the submitted version

- New outputs: Table 14 (variation across prompt templates and across models in E2) and Figure 7 (the comparisons behind the three estimates of the dollar-boundary effect). Table 38 now reports five specifications; its core-sweep rows share the resamples of the main core-sweep estimate, it adds the mean-placebo bound, and it labels the largest-placebo contrast as not a bound.
- `analyse.R`: the `--out` and `--copyto` options, clearer table captions, notes and figure labels, a corrected label for the E6 `digitspace` format ("$ 1 2 . 9 9"), a corrected legend in the forest plots, and new macros. All other estimates are identical to the submitted version.
- `corpus_endings.py` now also writes the ending profile, and both corpus files are included, so the analysis runs offline.
- The panels are unchanged.

## Licence

MIT (see `LICENSE`).

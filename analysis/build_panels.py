#!/usr/bin/env python3
"""Rebuild the tidy per-arm panels from the raw call logs.

The collection notebook writes these panels as it goes; this is the same
aggregation as a standalone step, so the analysis can be rebuilt from
`data/raw/` without a live kernel.

One call log line is one API call. A panel cell is one (condition, price)
combination, averaging over the repeated draws that sampled models need.

    python analysis/build_panels.py [ARM ...]      # default: every arm found
"""
from __future__ import annotations

import json
import math
import statistics
import sys
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RAW = ROOT / "data" / "raw"
OUT = ROOT / "data" / "panels"

# Every field that distinguishes a condition. A field left out here silently averages
# two different conditions into one cell: that is how E10's four installed conventions
# went missing from the first build.
KEYCOLS = ["experiment", "model", "product", "persona", "template", "currency",
           "rendering", "cents_cond", "domain", "unit", "probe", "preamble_id", "price"]
META = ["ref_price", "anchor", "ending", "decade_step", "crosses_dollar", "roundness",
        "dollars", "deliberation", "tier", "family", "rsn", "stack", "mode"]
COLS = KEYCOLS + ["p_buy", "p_sd", "n_obs", "cost_usd", "cot_tokens"] + META


def build(arm: str) -> int:
    files = sorted((RAW / arm).glob("*.jsonl"))
    if not files:
        return 0
    vals: dict[tuple, list[float]] = defaultdict(list)
    cost: dict[tuple, float] = defaultdict(float)
    cot: dict[tuple, list[float]] = defaultdict(list)
    meta: dict[tuple, dict] = {}
    n_raw = n_err = 0
    for f in files:
        with open(f) as fh:
            for line in fh:
                try:
                    r = json.loads(line)
                except json.JSONDecodeError:
                    continue
                n_raw += 1
                p = r.get("p_buy")
                if r.get("error") is not None or p is None or (isinstance(p, float) and p != p):
                    n_err += 1
                    continue
                k = tuple(r.get(c) for c in KEYCOLS)
                vals[k].append(float(p))
                cost[k] += float(r.get("cost_usd") or 0.0)
                cot[k].append(float(r.get("cot_tokens") or 0.0))
                if k not in meta:
                    meta[k] = {c: r.get(c) for c in META}
    if not vals:
        print(f"  {arm}: {n_raw:,} calls, none usable")
        return 0

    def fmt(v):
        if v is None:
            return ""
        s = str(v)
        return '"' + s.replace('"', '""') + '"' if any(ch in s for ch in ',"\n') else s

    OUT.mkdir(parents=True, exist_ok=True)
    dest = OUT / f"{arm}.csv"
    with open(dest, "w") as fh:
        fh.write(",".join(COLS) + "\n")
        for k in sorted(vals, key=lambda t: tuple(str(x) for x in t)):
            v = vals[k]
            row = list(k) + [
                statistics.fmean(v),
                statistics.stdev(v) if len(v) > 1 else "",
                len(v), round(cost[k], 8), statistics.fmean(cot[k]),
            ] + [meta[k][c] for c in META]
            fh.write(",".join(fmt(x) for x in row) + "\n")
    print(f"  {arm}: {n_raw:,} calls, {n_err:,} unusable ({n_err/max(n_raw,1):.2%}), "
          f"{len(vals):,} cells, {sum(cost.values()):.3f} USD")
    return len(vals)


if __name__ == "__main__":
    arms = sys.argv[1:] or sorted(d.name for d in RAW.iterdir() if d.is_dir())
    total = sum(build(a) for a in arms)
    print(f"{total:,} cells across {len(arms)} arm(s) -> {OUT}")

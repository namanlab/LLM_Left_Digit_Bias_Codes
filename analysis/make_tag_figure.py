#!/usr/bin/env python3
"""Assemble the rendered price tags shown to the vision models into one figure.

The tags are cached by the collection notebook under data/tags/, keyed by a hash of
(price, product name, condition, currency). This reproduces that key, picks a product
for which every condition was rendered at both sides of a dollar boundary, and writes
one PDF for the paper and the design document.
"""
import hashlib
import re
import json
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import matplotlib.image as mpimg

ROOT = Path(__file__).resolve().parent.parent
TAGS = ROOT / "data" / "tags"
OUT = ROOT / "output" / "figures" / "fig_tags.pdf"
CONDS = [("same", "cents at 100%"), ("r70", "cents at 70%"),
         ("r50", "cents at 50%"), ("r35", "cents at 35%")]


def tag(price, name, cond):
    h = hashlib.md5(f"{price}|{name}|{cond}|USD".encode()).hexdigest()[:12]
    return TAGS / f"tag_{h}.png"


def main():
    nb = json.loads((ROOT / "notebooks" / "01_collect.ipynb").read_text())
    src = "".join("".join(c["source"]) for c in nb["cells"])
    names = dict(re.findall(r'Product\("(\w+)","([^"]+)"', src))

    chosen = None
    for pid, name in names.items():
        for d in range(2, 60):
            pair = [round(d + 0.99, 2), float(d + 1)]
            if all(tag(p, name, c).exists() for c, _ in CONDS for p in pair):
                chosen = (name, pair)
                break
        if chosen:
            break
    if chosen is None:
        raise SystemExit("no product has every condition cached on both sides of a dollar")

    name, pair = chosen
    fig, axes = plt.subplots(2, len(CONDS), figsize=(6.6, 3.5))
    for j, (cond, label) in enumerate(CONDS):
        for i, price in enumerate(pair):
            ax = axes[i, j]
            ax.imshow(mpimg.imread(tag(price, name, cond)))
            ax.set_xticks([]); ax.set_yticks([])
            for sp in ax.spines.values():
                sp.set_color("0.75")
            if i == 0:
                ax.set_title(label, fontsize=8.5)
            if j == 0:
                ax.set_ylabel(f"${price:.2f}", fontsize=9)
    fig.tight_layout(pad=0.4)
    OUT.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUT)
    for dest in ("paper", "plan"):
        d = ROOT / dest / "figures"
        if d.exists():
            fig.savefig(d / OUT.name)
    print(f"wrote {OUT} using {name} at ${pair[0]:.2f} and ${pair[1]:.2f}")


if __name__ == "__main__":
    main()

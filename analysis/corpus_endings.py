"""E5, corpus side: how often does each price ending appear in web text?

Uses the free public infini-gram API (no key, no GPU) over four corpora. Counts
"$D.CC" for a set of dollar values D and every ending CC in the experimental grid,
then aggregates to an ending-level frequency profile.

    python3 analysis/corpus_endings.py     ->  data/corpus_endings.csv
"""
import json, urllib.request, time, itertools, sys
from pathlib import Path

API = "https://api.infini-gram.io/"
INDEXES = {"dolma": "v4_dolma-v1_7_llama", "redpajama": "v4_rpj_llama_s4",
           "c4": "v4_c4train_llama"}
DOLLARS  = [3, 4, 5, 7, 9, 12, 15, 19, 24, 29]
ENDINGS  = sorted({d*10+w for d in range(10) for w in (0, 5, 9)})   # the 30 grid endings

def count(index, s, tries=4):
    for a in range(tries):
        try:
            r = urllib.request.Request(API, data=json.dumps(
                    {"index": index, "query_type": "count", "query": s}).encode(),
                    headers={"Content-Type": "application/json"})
            with urllib.request.urlopen(r, timeout=45) as resp:
                d = json.loads(resp.read())
            if "count" in d:
                return d["count"]
            return None
        except Exception:
            time.sleep(1.5 * (a + 1))
    return None

def main():
    rows = []
    total = len(INDEXES) * len(DOLLARS) * len(ENDINGS)
    n = 0
    for name, idx in INDEXES.items():
        for d, e in itertools.product(DOLLARS, ENDINGS):
            s = f"${d}.{e:02d}"
            c = count(idx, s)
            n += 1
            if n % 60 == 0:
                print(f"  {n}/{total}", flush=True)
            rows.append(dict(corpus=name, dollars=d, ending=e, query=s, count=c))
    out = Path(__file__).resolve().parent.parent / "data" / "corpus_endings.csv"
    import csv
    with open(out, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=["corpus", "dollars", "ending", "query", "count"])
        w.writeheader(); w.writerows(rows)
    print("wrote", out, f"({len(rows)} rows)")

if __name__ == "__main__":
    main()

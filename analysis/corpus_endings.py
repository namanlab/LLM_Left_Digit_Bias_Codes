"""E5, corpus side: how often does each price ending appear in web text?

Uses the free public infini-gram API (no key, no GPU) over three corpora (C4, Dolma,
RedPajama). Counts "$D.CC" for a set of dollar values D and every ending CC in the
experimental grid, then aggregates to an ending-level frequency profile.

    python3 analysis/corpus_endings.py                 # query the API, write both files
    python3 analysis/corpus_endings.py --profile-only  # rebuild the profile offline

    data/corpus_endings.csv            one row per corpus, dollar value and ending (raw counts)
    data/ending_frequency_profile.csv  one row per ending, read by analysis/analyse.R
"""
import csv, json, math, urllib.request, time, itertools, sys
from collections import defaultdict
from pathlib import Path

API = "https://api.infini-gram.io/"
INDEXES = {"dolma": "v4_dolma-v1_7_llama", "redpajama": "v4_rpj_llama_s4",
           "c4": "v4_c4train_llama"}
DOLLARS  = [3, 4, 5, 7, 9, 12, 15, 19, 24, 29]
ENDINGS  = sorted({d*10+w for d in range(10) for w in (0, 5, 9)})   # the 30 grid endings
DATA = Path(__file__).resolve().parent.parent / "data"

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

def write_profile(rows):
    # Share of each ending among the counted prices with the same dollar value, in
    # percent, averaged over dollar values. Queries that returned no count are skipped.
    corpora = sorted({r["corpus"] for r in rows})
    share = {}
    for name in corpora:
        per_dollar = defaultdict(dict)
        for r in rows:
            if r["corpus"] == name and r["count"] is not None:
                per_dollar[int(r["dollars"])][int(r["ending"])] = float(r["count"])
        acc = defaultdict(list)
        for m in per_dollar.values():
            t = sum(m.values())
            if t > 0:
                for e, c in m.items():
                    acc[e].append(100 * c / t)
        share[name] = {e: sum(v) / len(v) for e, v in acc.items()}
    out = DATA / "ending_frequency_profile.csv"
    with open(out, "w", newline="") as fh:
        w = csv.writer(fh)
        w.writerow(["ending"] + corpora + ["mean", "excess"])
        for e in ENDINGS:
            raw = [share[k].get(e, float("nan")) for k in corpora]
            vals = [round(v, 4) for v in raw]
            mean = round(sum(raw) / len(raw), 4)
            # log2 of the mean share relative to a uniform share over the 30 endings
            w.writerow([e] + vals + [mean, math.log2(mean / (100 / len(ENDINGS)))])
    print("wrote", out)

def read_counts():
    rows = []
    with open(DATA / "corpus_endings.csv") as fh:
        for r in csv.DictReader(fh):
            r["count"] = None if r["count"] in ("", "None") else float(r["count"])
            rows.append(r)
    return rows

def main():
    if "--profile-only" in sys.argv:
        write_profile(read_counts())
        return
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
    out = DATA / "corpus_endings.csv"
    with open(out, "w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=["corpus", "dollars", "ending", "query", "count"])
        w.writeheader(); w.writerows(rows)
    print("wrote", out, f"({len(rows)} rows)")
    write_profile(rows)

if __name__ == "__main__":
    main()

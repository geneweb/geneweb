#!/usr/bin/env python3
"""Benchmark gwd first-name searches on several imports of a same base.

Three imports of the same .gw file. Base names must not contain "_": gwd
reads "base_xxx" as the base "base" with the password or token "xxx".
  gwc -bd <bases> -o <base> <base>.gw
  gwc -bd <bases> -ngrams 1,1,1 -o <base>-ng1 <base>.gw
  gwc -bd <bases> -ngrams 2,2,2 -o <base>-ng2 <base>.gw

Timing runs: one gwd serves the three bases with the same options for all of
them, on a machine that serves nothing else meanwhile, without --debug and
--log (debug mode walks the heap and writes logs on every request). To time
as in production, load the three bases in memory:
  gwd --bd <bases> --cache-database <base>,<base>-ng1,<base>-ng2

Random queries (one file per draw, the seed is written in the file):
  cache_files -bd <bases> -ngram_queries q1.tsv <base>

Run and report:
  ngbench.py run --url http://localhost:2317 \
      --bases <base>,<base>-ng1,<base>-ng2 --queries q1.tsv --out r1.tsv
  ngbench.py report r1.tsv

Diagnosis only, not for timing: start gwd with "-debug -log gwd.log", run with
--rounds 1, then list the result counts of each request:
  awk '{sub(/\r$/, "")} /INFO  GWD/ {q = $NF}
       /-> [0-9]+ results/ {print q "\t" $0}' gwd.log

queries.tsv: one "group<TAB>query" per line, "#" lines ignored.
The first base of --bases is the reference of the report.
Default mode: p_order=on, the only search that uses the n-grams for queries
in another order; other gwd parameters can be given with --modes, e.g.
--modes "p_order=on,p_order=on&p_exact=off&fna=on".
Round 1 is reported apart: it includes the first reads of the base files from
disk; later rounds find them in the system file cache.
"""
import argparse
import csv
import re
import statistics
import sys
import time
import urllib.parse
import urllib.request

PERSON_RE = re.compile(
    r'[?&;](?:amp;)?p=([^&;"#]*)&(?:amp;)?n=([^&;"#]*)(?:&(?:amp;)?oc=(\d+))?'
)
MODES = ["p_order=on"]
FIELDS = ["round", "mode", "group", "query", "base", "seconds", "bytes", "persons"]


def fetch(url, timeout):
    start = time.perf_counter()
    with urllib.request.urlopen(url, timeout=timeout) as resp:
        body = resp.read()
    seconds = time.perf_counter() - start
    persons = len(set(PERSON_RE.findall(body.decode("utf-8", "replace"))))
    return seconds, len(body), persons


def read_queries(path):
    queries = []
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.rstrip("\r\n")
            if line and not line.startswith("#"):
                group, query = line.split("\t")[:2]
                queries.append((group, query.strip()))
    return queries


def run(args):
    queries = read_queries(args.queries)
    bases = args.bases.split(",")
    with open(args.out, "w", newline="", encoding="utf-8") as out:
        writer = csv.writer(out, delimiter="\t")
        writer.writerow(FIELDS)
        for rnd in range(1, args.rounds + 1):
            for mode in args.modes.split(","):
                for i, (group, query) in enumerate(queries, 1):
                    q = urllib.parse.quote(query)
                    for base in bases:
                        url = f"{args.url}/{base}?m=S&p={q}&{mode}"
                        try:
                            seconds, size, persons = fetch(url, args.timeout)
                        except Exception as e:
                            print(f"\n{base} {query!r}: {e}", file=sys.stderr)
                            seconds, size, persons = float("nan"), 0, -1
                        writer.writerow(
                            [rnd, mode, group, query, base,
                             f"{seconds:.4f}", size, persons])
                    out.flush()
                    print(f"\rround {rnd}/{args.rounds}  {mode}  "
                          f"{i}/{len(queries)}   ", end="", file=sys.stderr)
    print(file=sys.stderr)


def p95(values):
    values = sorted(values)
    return values[min(len(values) - 1, int(0.95 * len(values)))]


def fmt(values, f):
    return f"{f(values):.3f}" if values else "-"


def report(args):
    with open(args.results, encoding="utf-8") as f:
        rows = [r for r in csv.DictReader(f, delimiter="\t")
                if r["persons"] != "-1"]
    bases = list(dict.fromkeys(r["base"] for r in rows))
    ref = bases[0]
    first, warm, persons = {}, {}, {}
    for r in rows:
        key = (r["mode"], r["group"], r["query"], r["base"])
        seconds = float(r["seconds"])
        if r["round"] == "1":
            first[key] = seconds
        else:
            warm.setdefault(key, []).append(seconds)
        persons[key] = int(r["persons"])
    groups = list(dict.fromkeys((r["mode"], r["group"]) for r in rows))
    print(f"reference: {ref}; times in seconds, medians over the queries;"
          " warm time of a query = median of its rounds after the first")
    print("mode\tgroup\tbase\tqueries\tfirst\twarm\twarm_p95"
          "\tpersons\tmore\tfewer")
    for mode, group in groups:
        queries = list(dict.fromkeys(
            r["query"] for r in rows
            if r["mode"] == mode and r["group"] == group))
        for base in bases:
            keys = [(mode, group, q, base) for q in queries
                    if (mode, group, q, base) in persons]
            if not keys:
                continue
            firsts = [first[k] for k in keys if k in first]
            warms = [statistics.median(warm[k]) for k in keys if k in warm]
            more = fewer = 0
            for k in keys:
                rk = (mode, group, k[2], ref)
                if rk in persons:
                    more += persons[k] > persons[rk]
                    fewer += persons[k] < persons[rk]
            total = sum(persons[k] for k in keys)
            print(f"{mode}\t{group}\t{base}\t{len(keys)}"
                  f"\t{fmt(firsts, statistics.median)}"
                  f"\t{fmt(warms, statistics.median)}\t{fmt(warms, p95)}"
                  f"\t{total}\t{more}\t{fewer}")


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawTextHelpFormatter)
    sub = parser.add_subparsers(dest="cmd", required=True)
    p_run = sub.add_parser("run")
    p_run.add_argument("--url", default="http://localhost:2317")
    p_run.add_argument("--bases", required=True)
    p_run.add_argument("--queries", required=True)
    p_run.add_argument("--out", default="results.tsv")
    p_run.add_argument("--rounds", type=int, default=2)
    p_run.add_argument("--modes", default=",".join(MODES))
    p_run.add_argument("--timeout", type=float, default=300)
    p_report = sub.add_parser("report")
    p_report.add_argument("results")
    args = parser.parse_args()
    run(args) if args.cmd == "run" else report(args)


if __name__ == "__main__":
    main()
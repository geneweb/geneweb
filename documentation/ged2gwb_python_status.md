# Python ged2gwb — Status and Next Steps

## Summary

- Python `ged2gwb` works: it converts a GEDCOM file into a `.pkl` (Python pickle) database.
- The classic GeneWeb web UI is powered by the OCaml daemon `gwd`, which expects `.gwb` directories (OCaml binary format).
- To use the classic UI, you still need `gwd` (or a new reader/driver compatible with the `.pkl` output).

## What works now

- Convert a GEDCOM to `.pkl`:
  - `PYTHONPATH=src/python python -m ged2gwb --force --output tmp/test-ci-ged2gwb src/python/gedcom/tests/test_simple.ged`
  - Produces `tmp/test-ci-ged2gwb.pkl`.
- Load to validate:
  - `PYTHONPATH=src/python python -m ged2gwb --load tmp/test-ci-ged2gwb.pkl`

## Limitations

- `gwd` reads `<base>.gwb` (directory) and cannot read `.pkl`.
- Therefore, `.pkl` produced by Python is not directly usable by the classic web UI.

## Options to proceed

1. Keep `gwd` and generate `.gwb` (OCaml format) from Python.
2. Keep `gwd` and add a new driver that reads a neutral format (e.g., MessagePack/JSON) produced by Python.
3. Replace `gwd` with a Python web server that serves `.pkl` directly (new UI).

## Recommended direction

- To keep the classic UI: define a versioned, neutral schema (e.g., MessagePack) and implement an OCaml reader (new driver) in `gwd`.
- To move fast with pure Python: provide a minimal Python web UI for browsing `.pkl`.

## CI validation snippet

```bash
# Convert
PYTHONPATH=src/python \
python -m ged2gwb --force \
  --output tmp/test-ci-ged2gwb \
  src/python/gedcom/tests/test_simple.ged

# Load to verify
PYTHONPATH=src/python \
python -m ged2gwb --load tmp/test-ci-ged2gwb.pkl
```

## Takeaway

- Python `ged2gwb`: OK ✅
- Classic UI via `gwd`: needs `.gwb` or a new driver that can read the Python-produced format.

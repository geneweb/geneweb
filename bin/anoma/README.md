# gwwarn — GeneWeb warning-log analyzer

Reads a warning log produced when rebuilding a GeneWeb base and generates an
HTML report with statistics, "extreme" statistics, and per-warning-type sorted
unique lists of persons/families, each name linking to the person in the base.

## Build

Requires OCaml ≥ 4.10 (uses `List.concat_map`) and the `str` library
(bundled with the compiler).

    ocamlfind ocamlopt -package str -linkpkg gwwarn.ml -o gwwarn
    # or, without ocamlfind:
    ocamlopt str.cmxa gwwarn.ml -o gwwarn
    # or with dune (dune-project and dune files provided):
    dune build ./gwwarn.exe

## Usage

    gwwarn <basename> -bd <bases_dir> -ok <ignored_file> -in <log_file>
           [-out <report.html>] [-url <base_url>]

- `-bd`   bases directory; the configuration is read from `<bases_dir>/<basename>.cfg`
- `-ok`   ignored (verified) persons/families file
- `-in`   warning log file
- `-out`  output HTML report (default `<basename>_warnings.html`)
- `-url`  base URL for person links (default `http://localhost:2317/<basename>`)

A console summary of the statistics is also printed.

## Configuration file `<basename>.cfg`

One line per warning type, `WarningName=yes|no` (see `sample/demo.cfg` for the
full list). `yes` means the person/family lists for this warning appear in the
report. A type absent from the file defaults to `yes`. Lines starting with `#`
are comments.

## Ignored file

Lists persons/families whose warning has been explicitly verified.

    # person: codes M, B, D, E
    first_name.occ surname: M, B, D, E
    # family: codes D (duplicate), CO (children order), EO (events order)
    first_name.occ surname & first_name.occ surname: D, CO, EO
    # duplicate families may also be given by family ids:
    4143171 & 4143172: D

Codes and the warnings they suppress:

| Code | Warnings suppressed |
|------|---------------------|
| M | YoungForMarriage, OldForMarriage |
| B | ParentTooOld, ParentTooYoung |
| D (person) | DeadOld |
| E | BigAgeBetweenSpouses |
| D (family) | PossibleDuplicateFam, PossibleDuplicateFamHomonymous |
| CO | ChildrenNotInOrder, ChangedOrderOfChildren, CloseChildren, DistantChildren |
| EO | ChangedOrderOfFamilyEvents, FEventOrder, PEventOrder, ChangedOrderOfPersonEvents |

## Report contents

1. **Statistics** — per warning type: total, ignored (verified), remaining.
2. **Extreme statistics** — married at age ≤ 11, married at 12, died at
   100–109, 110–119, ≥ 120; each bucket is a button opening the person list.
3. **Persons / families per warning** — for every type set to `yes`: a button
   opening the sorted unique list; each `first_name.occ surname` links to
   `<base_url>?p=<first_name>&n=<surname>&oc=<occ>` (`oc` omitted when 0).
   Full original messages are available under a sub-button.
4. **Persons with several warnings** — persons appearing in ≥ 2 distinct
   warnings, with the messages.

## Limitations (inherent to the log format)

- `ParentTooOld` / `ParentTooYoung` print the same message; ages < 20 are
  classified as TooYoung. Same for `YoungForMarriage` / `OldForMarriage`
  (threshold 50).
- `CloseChildren` and `DistantChildren` print identical text; both are
  reported as CloseChildren (enabled if either is `yes`).
- `PEventOrder` / `FEventOrder` (and the P/F witness warnings) have identical
  shapes; they are reported under the P variant, enabled if either is `yes`.
- Exact duplicate messages (e.g. duplicate-family warnings printed once per
  direction) are deduplicated.

## Sample

`sample/` contains a demo log, configuration, and ignored file, plus the
report they produce (`demo_warnings.html`):

    gwwarn demo -bd sample -ok sample/demo.ok -in sample/demo.log

`test/harness.py` is a Python mirror of the same logic, used to validate the
parsing and to regenerate the demo report without an OCaml toolchain.
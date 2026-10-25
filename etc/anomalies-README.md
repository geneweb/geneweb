# Anomalies

The anomalies module flags entries in a GeneWeb base that are probably wrong —
a birth after a death, a parent younger than a child, a marriage dated before a
birth, and so on — and lets wizards triage them: review each one, mark the ones
that are in fact correct as *verified*, and keep only the genuine problems in
view. It replaces the old `gwwarn` plugin and is now a standard part of GeneWeb.

It is directly derived from the Perl tool developed for Roglo by Christian Gennerat
which was analysing the log traces produced by the `gwc` tool.

Nothing is ever changed in the base. The module only reads it and produces a
report; acting on an anomaly (fixing or confirming it) is left to the wizard.


## How a report is produced

The check is exactly the consistency check GeneWeb already runs when it builds a
base. Two things produce a report:

- **`gwc` and `ged2gwb`** write it automatically while building or importing a
  base, so a freshly built base already has its report.
- **`anoma`**, a small command-line tool, refreshes the report for a base that
  has been edited through `gwd` since it was built (run from a cron job on a
  large base, or from the gwsetup page).

The report is a snapshot: it reflects the base as it was when the check last
ran. Re-run `anoma` (or rebuild) to bring it up to date.


## Files

Everything lives in the base's configuration directory, `<base>.gwb/config/`:

- `anomalies.jsonl` — the report. JSON Lines: a header line (date, producer)
  followed by one line per anomaly, grouped by kind so that a per-kind page can
  read only the lines it needs.
- `verified.jsonl` — the consolidated list of anomalies that wizards have marked
  as verified. These are hidden from the report unless you ask to see them.
- `verified_d/` — pending verifications, one `<wizard>.jsonl` fragment per
  wizard. Each wizard's marks accumulate here until they are merged.
- `verified_d/archive/<timestamp>/` — fragments are moved here once merged, so
  the history of who verified what is kept.

Keeping the report and the verifications inside `<base>.gwb/` means a base backs
up as a single unit.


## The report in gwd (wizard only)

Open the report with `m=ANOMA`. It opens on a summary: a table of how many
anomalies of each kind were found and how many are already verified, with the
noisier data-entry checks folded into a separate "other checks" section.

From there you reach the per-kind lists. Each list is paginated and, where it
makes sense (ages), can be filtered by a range. Every anomaly shows the persons
involved (as links to edit them), the wizard most likely responsible, and a
button to mark it verified — or to cancel a verification.

A **responsible-wizard filter** sits at the top: *all*, *my anomalies*, the
wizards responsible for the most anomalies, and *unattributed* for the anomalies
no wizard could be tied to. Picking one narrows every count and list to that
wizard, which is how a wizard finds the entries that are theirs to look at.

The other modes, all wizard-only:

- `m=OK_ADD` — mark an anomaly as verified.
- `m=OK_DEL` — cancel a verification.
- `m=OK_FILE` — list the verified anomalies.
- `m=OK_FOLD` — consolidate: merge every wizard's pending fragments into
  `verified.jsonl` (a later cancellation wins over an earlier verification) and
  archive the fragments that were merged.


## Who is held responsible

For each anomaly the module tries to name the wizard most likely responsible, in
this order:

1. A `[[w:ident]]` **signature** in the sources of the persons (or, for a
   couple, the family) involved. The "sign" button of the update forms appends
   `[[w:ident]] (source) day/ROMAN-MONTH/year` to a source field; the first or
   last signer is taken, according to the base parameter below.
2. Failing a signature, the base **history**: the first or last wizard who
   touched one of the persons involved.
3. Failing both, the anomaly is **unattributed** (shown under its own filter).


## Base parameters (`.gwf`)

- `anomalies_responsible` — `first_signer` or `last_signer` (default
  `last_signer`): which signer, or which end of the history, names the
  responsible wizard.
- `anomalies_hidden` — a comma-separated list of kind names to fold into the
  "other checks" section instead of showing them up front. When absent, a
  built-in default hides the mostly cosmetic data-entry checks (order of
  children and events, undefined sex, possible duplicates, and the like).


## The `anoma` command

```
anoma [options] <base>
```

Checks `<base>`, writes `<base>.gwb/config/anomalies.jsonl`, then merges any
pending verifications.

Options:

- `-bd <DIR>` — the bases directory (defaults to the built-in base directory).
- `-fold` — only merge the pending verifications; skip the scan. Use this to
  consolidate without recomputing the report.

When standard error is a terminal, `anoma` shows a progress bar for each phase
(`check persons`, `check families`, `read history`); when its output is
redirected (for example by gwsetup), the bars are silent.

gwsetup exposes an **anoma** action that runs this tool on an existing base, so
a report can be refreshed from the browser without a rebuild.

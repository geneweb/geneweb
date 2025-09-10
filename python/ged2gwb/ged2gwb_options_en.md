# ged2gwb — Options and quick documentation (EN)

Short description
- Binary: ged2gwb (OCaml, part of Geneweb)
- Purpose: convert a GEDCOM file (.ged) into a GeneWeb database (.gwb). Parses GEDCOM, extracts persons/families/events, normalizes names/dates/charset and writes a GeneWeb database.
- Run where `bases/` is available or use `-bd` to set base dir.

Main features
- Full GEDCOM parsing (INDI, FAM, SOUR, NOTE, etc.)
- Normalization of first and last names (case rules, particles)
- Date handling (multiple calendars, numbered months)
- Options to force charset, load particles file, extract titles/public names
- Logging and consistency checks

Options (name — arg — type — default — description)
- -bd <DIR> — string — "." — Directory where `bases/` is located.
- -o <file> — string — derived from input, fallback "a" — Output database basename (.gwb).
- -f — flag — — Force remove existing database.
- -log <file> — string — — Redirect log trace to file.
- -lf — flag — off — Lowercase first names with title-case initials.
- -trackid — flag — — Print GEDCOM id -> GeneWeb id matches.
- -ls — flag — — Surnames to title-case; tries to keep particles lowercase.
- -us — flag — — Surnames to UPPERCASE.
- -fne <be> — string (2 chars) — — Two-character delimiters to extract usual first name (e.g. "()").
- -efn / -no_efn — flag / negation — — Extract first token as first name / cancel.
- -epn / -no_epn — flag / negation — — Extract public name from first-name field / cancel.
- -no_pit — flag — — Do not treat persons with titles as public.
- -tnd — flag — — Allow setting negative years on inconsistencies (e.g. birth after death).
- -no_nd — flag — — Do not interpret "-YEAR" as negative year.
- -nc — flag — — Disable consistency checks.
- -nopicture — flag — — Do not extract individual pictures.
- -udi x-y — string "x-y" — x,y integers — Set alive/dead thresholds when death undefined (e.g. 80-120).
- -uin — flag — — Put untreated GEDCOM tags into notes.
- -ds <string> — string — "" — Default source for persons/families lacking source.
- -dates_dm / -dates_md — mutually exclusive flags — — Interpret numeric-month dates as DM or MD.
- -rs_no_mention — flag — — Force family relation to NoMention (default Married).
- -charset [ANSEL|ASCII|MSDOS] — enum — — Force GEDCOM charset decoding.
- -particles <FILE> — string — — Load particles list from provided file.
- -reorg — flag — — Enable reorg mode.

Anonymous argument
- <ged> (optional): input GEDCOM file path.

Examples
- ged2gwb mytree.ged
- ged2gwb -o mydb -f -log convert.log file.ged
- ged2gwb -bd /opt/geneweb/bases -charset ANSEL file.ged

Notes
- -fne expects 2 chars exactly (open, close).
- UTF-8 may be auto-detected by BOM; -charset overrides detection.
- Particles/titles behavior depends on files in `bases/` or via -particles.
- Use -log to capture warnings and errors.

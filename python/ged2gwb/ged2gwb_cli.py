from dataclasses import dataclass, field
import argparse
import logging
import sys
from typing import Optional, Tuple, List

@dataclass
class Config:
    in_file: str = ""
    base_dir: str = "."
    out_file: str = "a"
    force: bool = False
    log_file: Optional[str] = None
    lowercase_first_names: bool = False
    track_ged2gw_id: bool = False
    case_surnames: str = "NoCase"  # "NoCase" | "LowerCase" | "UpperCase"
    first_names_brackets: Optional[Tuple[str, str]] = None
    extract_first_names: bool = False
    extract_public_names: bool = True
    no_public_if_titles: bool = False
    try_negative_dates: bool = False
    no_negative_dates: bool = False
    do_check: bool = True
    no_picture: bool = False
    alive_years: int = 80
    dead_years: int = 120
    untreated_in_notes: bool = False
    default_source: str = ""
    relation_status: str = "Married"  # or "NoMention"
    month_number_dates: str = "NoMonthNumberDates"  # DayMonthDates | MonthDayDates | ...
    charset: Optional[str] = None  # ANSEL | ASCII | MSDOS | MacIntosh | Utf8
    particles_file: Optional[str] = None
    reorg: bool = False
    # Keep original argv for possible later use
    argv: List[str] = field(default_factory=list)

def parse_udi(s: str) -> Tuple[int, int]:
    if '-' not in s:
        raise argparse.ArgumentTypeError("bad parameter for -udi, expected x-y")
    a, b = s.split('-', 1)
    a_val = int(a) if a != "" else 80
    b_val = int(b) if b != "" else max(a_val, 120)
    if b_val < a_val:
        b_val = a_val
    return a_val, b_val

def parse_fne(s: str) -> Tuple[str, str]:
    if len(s) != 2:
        raise argparse.ArgumentTypeError("-fne option must be followed by a 2 characters string")
    return (s[0], s[1])

def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(prog="ged2gwb.py", description="Ported CLI (options only) for ged2gwb")
    p.add_argument("infile", nargs="?", help="Input GED file (optional)")
    p.add_argument("-bd", "--base-dir", dest="base_dir", help="Where bases directory is installed", default=".")
    p.add_argument("-o", "--out", dest="out_file", help="Output database name (basename)", default="a")
    p.add_argument("-f", "--force", dest="force", action="store_true", help="Remove database if already existing")
    p.add_argument("-log", dest="log_file", help="Redirect log trace to this file")
    p.add_argument("-lf", dest="lowercase_first_names", action="store_true",
                   help="Convert first names to lowercase letters, initials uppercase")
    p.add_argument("-trackid", dest="track_ged2gw_id", action="store_true", help="Print gedcom id to gw id matches")
    group_case = p.add_mutually_exclusive_group()
    group_case.add_argument("-ls", dest="case_surnames", action="store_const", const="LowerCase",
                            help="Convert surnames to lowercase (titlecase)")
    group_case.add_argument("-us", dest="case_surnames", action="store_const", const="UpperCase",
                            help="Convert surnames to uppercase")
    p.add_argument("-fne", dest="fne", type=parse_fne,
                   help="When creating a person, first name part enclosed between two chars (2-chars string)")
    efng = p.add_mutually_exclusive_group()
    efng.add_argument("-efn", dest="extract_first_names", action="store_true",
                      help="Extract first name (first token) as person's first name")
    efng.add_argument("-no_efn", dest="extract_first_names", action="store_false",
                      help="Disable -efn")
    epng = p.add_mutually_exclusive_group()
    epng.add_argument("-epn", dest="extract_public_names", action="store_true",
                      help="Extract public names from first name part")
    epng.add_argument("-no_epn", dest="extract_public_names", action="store_false",
                      help="Disable -epn")
    p.add_argument("-no_pit", dest="no_public_if_titles", action="store_true", help="Do not consider persons with titles as public")
    p.add_argument("-tnd", dest="try_negative_dates", action="store_true", help="Try negative dates when inconsistency")
    p.add_argument("-no_nd", dest="no_negative_dates", action="store_true", help="Don't interpret -year as negative")
    p.add_argument("-nc", dest="do_check", action="store_false", help="No consistency check")
    p.add_argument("-nopicture", dest="no_picture", action="store_true", help="Don't extract individual picture")
    p.add_argument("-udi", dest="udi", type=parse_udi, help="x-y alive/dead thresholds")
    p.add_argument("-uin", dest="untreated_in_notes", action="store_true", help="Put untreated GEDCOM tags in notes")
    p.add_argument("-ds", dest="default_source", help="Set default source string", default="")
    dm_md = p.add_mutually_exclusive_group()
    dm_md.add_argument("-dates_dm", dest="month_number_dates", action="store_const", const="DayMonthDates",
                      help="Interpret month-numbered dates as day/month/year")
    dm_md.add_argument("-dates_md", dest="month_number_dates", action="store_const", const="MonthDayDates",
                      help="Interpret month-numbered dates as month/day/year")
    p.add_argument("-rs_no_mention", dest="relation_status", action="store_const", const="NoMention",
                   help="Force relation status to NoMention")
    p.add_argument("-charset", dest="charset", choices=["ANSEL", "ASCII", "MSDOS", "MACINTOSH", "UTF-8"],
                   help="Force given charset decoding")
    p.add_argument("-particles", dest="particles_file", help="Use given file as list of particles")
    p.add_argument("-reorg", dest="reorg", action="store_true", help="Mode reorg")
    return p

def parse_args(argv=None) -> Config:
    argv = sys.argv[1:] if argv is None else argv
    parser = build_parser()
    ns = parser.parse_args(argv)
    cfg = Config()
    cfg.argv = argv
    cfg.in_file = ns.infile or ""
    cfg.base_dir = ns.base_dir
    # out_file: remove extension if any, keep basename behavior
    out = ns.out_file or "a"
    cfg.out_file = out.split('/')[-1].split('.')[0]
    cfg.force = ns.force
    cfg.log_file = ns.log_file
    cfg.lowercase_first_names = bool(ns.lowercase_first_names)
    cfg.track_ged2gw_id = bool(ns.track_ged2gw_id)
    cfg.case_surnames = ns.case_surnames or "NoCase"
    cfg.first_names_brackets = ns.fne
    # extract flags default True in OCaml: set if explicitly False
    cfg.extract_first_names = bool(getattr(ns, "extract_first_names", False))
    # extract_public_names default True in OCaml; argparse default is None -> keep True unless false
    if getattr(ns, "extract_public_names", None) is False:
        cfg.extract_public_names = False
    elif getattr(ns, "extract_public_names", None) is True:
        cfg.extract_public_names = True
    cfg.no_public_if_titles = bool(ns.no_public_if_titles)
    cfg.try_negative_dates = bool(ns.try_negative_dates)
    cfg.no_negative_dates = bool(ns.no_negative_dates)
    cfg.do_check = bool(getattr(ns, "do_check", True))
    cfg.no_picture = bool(ns.no_picture)
    if getattr(ns, "udi", None):
        a, b = ns.udi
        cfg.alive_years = a
        cfg.dead_years = b
    cfg.untreated_in_notes = bool(ns.untreated_in_notes)
    cfg.default_source = ns.default_source or ""
    cfg.relation_status = ns.relation_status or "Married"
    cfg.month_number_dates = ns.month_number_dates or "NoMonthNumberDates"
    cfg.charset = ns.charset
    cfg.particles_file = ns.particles_file
    cfg.reorg = bool(ns.reorg)
    # setup logging early if requested
    if cfg.log_file:
        logging.basicConfig(filename=cfg.log_file, level=logging.INFO,
                            format='%(asctime)s %(levelname)s: %(message)s')
    else:
        logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
    return cfg

def main(argv=None):
    cfg = parse_args(argv)
    # Minimal action for now: print parsed configuration (will be used to drive further refactor)
    logging.info("Parsed configuration:")
    for k, v in cfg.__dict__.items():
        logging.info("  %s = %r", k, v)
    return cfg

if __name__ == "__main__":
    main()

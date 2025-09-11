from dataclasses import dataclass, field
import argparse
import logging
import sys
from typing import Optional, Tuple, List

@dataclass
class Config:
    infile: str = ""
    base_dir: str = "."
    out_file: str = "a"
    force: bool = False
    log_file: Optional[str] = None
    lowercase_first_names: bool = False
    trackid: bool = False
    case_surnames: str = "NoCase"  # NoCase | LowerCase | UpperCase
    fne: Optional[Tuple[str, str]] = None
    extract_first_names: Optional[bool] = None
    extract_public_names: Optional[bool] = None
    no_public_if_titles: bool = False
    try_negative_dates: bool = False
    no_negative_dates: bool = False
    month_number_dates: str = "NoMonthNumberDates"  # DayMonthDates | MonthDayDates
    untreated_in_notes: bool = False
    default_source: str = ""
    relation_status: str = "Married"  # Married | NoMention
    no_picture: bool = False
    do_check: bool = True
    udi: Optional[Tuple[int, int]] = None
    uin: bool = False
    dates_dm: bool = False
    dates_md: bool = False
    charset: Optional[str] = None
    particles_file: Optional[str] = None
    reorg: bool = False
    argv: List[str] = field(default_factory=list)

OPTIONS = [
    ("-bd", "Where bases/ is installed", True),
    ("-o", "Output database basename", True),
    ("-f", "Force remove existing database", True),
    ("-log", "Redirect log trace", True),
    ("-lf", "Lowercase first names (title-case initials)", True),
    ("-trackid", "Print gedcom id -> gw id matches", True),
    ("-ls", "Surnames -> title-case", True),
    ("-us", "Surnames -> UPPERCASE", True),
    ("-fne", "Two-char delimiters for usual first name", True),
    ("-efn / -no_efn", "Extract first token as first name / disable", True),
    ("-epn / -no_epn", "Extract public names / disable", True),
    ("-no_pit", "Don't consider persons with titles as public", True),
    ("-tnd", "Set negative dates on inconsistency", True),
    ("-no_nd", "Don't interpret -YEAR as negative", True),
    ("-nc", "Disable consistency checks", True),
    ("-nopicture", "Don't extract individual picture", True),
    ("-udi", "Alive/dead thresholds x-y", True),
    ("-uin", "Put untreated GEDCOM tags in notes", True),
    ("-ds", "Default source for persons/families", True),
    ("-dates_dm / -dates_md", "Interpret numeric-month dates DM/MD", True),
    ("-rs_no_mention", "Force family relation to NoMention", True),
    ("-charset", "Force GEDCOM charset decoding", True),
    ("-particles", "Particles list file", True),
    ("-reorg", "Enable reorg mode", True),
]

def parse_udi(s: str):
    if '-' not in s:
        raise argparse.ArgumentTypeError("bad parameter for -udi, expected x-y")
    a, b = s.split('-', 1)
    a_val = int(a) if a != "" else 80
    b_val = int(b) if b != "" else max(a_val, 120)
    if b_val < a_val:
        b_val = a_val
    return a_val, b_val

def parse_fne(s: str):
    if len(s) != 2:
        raise argparse.ArgumentTypeError("-fne option must be followed by a 2 characters string")
    return (s[0], s[1])

def build_parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(prog="ged2gwb.py", description="CLI for ged2gwb (partial port)")
    p.add_argument("infile", nargs="?", help="Input GED file (optional)")
    p.add_argument("-bd", dest="base_dir", help="Where bases directory is installed", default=".")
    p.add_argument("-o", dest="out_file", help="Output database name (basename)", default="a")
    p.add_argument("-f", dest="force", action="store_true", help="Remove database if already existing")
    p.add_argument("-log", dest="log_file", help="Redirect log trace to this file")
    p.add_argument("-lf", dest="lowercase_first_names", action="store_true",
                   help="Convert first names to lowercase letters, initials uppercase")
    p.add_argument("-trackid", dest="trackid", action="store_true", help="Print gedcom id to gw id matches")
    group_case = p.add_mutually_exclusive_group()
    group_case.add_argument("-ls", dest="case_surnames", action="store_const", const="LowerCase",
                            help="Convert surnames to lowercase (titlecase)")
    group_case.add_argument("-us", dest="case_surnames", action="store_const", const="UpperCase",
                            help="Convert surnames to uppercase")
    p.add_argument("-fne", dest="fne", type=parse_fne, help="2-char delimiters for usual first name")
    efng = p.add_mutually_exclusive_group()
    efng.add_argument("-efn", dest="extract_first_names", action="store_true", help="Extract first token as first name")
    efng.add_argument("-no_efn", dest="extract_first_names", action="store_false", help="Disable -efn")
    epng = p.add_mutually_exclusive_group()
    epng.add_argument("-epn", dest="extract_public_names", action="store_true", help="Extract public names from first name part")
    epng.add_argument("-no_epn", dest="extract_public_names", action="store_false", help="Disable -epn")
    p.add_argument("-no_pit", dest="no_public_if_titles", action="store_true", help="Do not consider persons with titles as public")
    p.add_argument("-tnd", dest="try_negative_dates", action="store_true", help="Try negative dates when inconsistency")
    p.add_argument("-no_nd", dest="no_negative_dates", action="store_true", help="Don't interpret -year as negative")
    p.add_argument("-nc", dest="do_check", action="store_false", help="No consistency check")
    p.add_argument("-nopicture", dest="no_picture", action="store_true", help="Don't extract individual picture")
    p.add_argument("-udi", dest="udi", type=parse_udi, help="x-y alive/dead thresholds")
    p.add_argument("-uin", dest="uin", action="store_true", help="Put untreated GEDCOM tags in notes")
    p.add_argument("-ds", dest="default_source", help="Set default source string", default="")
    dm_md = p.add_mutually_exclusive_group()
    dm_md.add_argument("-dates_dm", dest="dates_dm", action="store_true", help="Interpret month-numbered dates as day/month/year")
    dm_md.add_argument("-dates_md", dest="dates_md", action="store_true", help="Interpret month-numbered dates as month/day/year")
    p.add_argument("-rs_no_mention", dest="relation_status", action="store_const", const="NoMention", help="Force relation status to NoMention")
    p.add_argument("-charset", dest="charset", choices=["ANSEL", "ASCII", "MSDOS", "MACINTOSH", "UTF-8"], help="Force given charset decoding")
    p.add_argument("-particles", dest="particles_file", help="Use given file as list of particles")
    p.add_argument("-reorg", dest="reorg", action="store_true", help="Mode reorg")
    return p

def get_options_status():
    return [(opt, desc, impl) for opt, desc, impl in OPTIONS]

def parse_args(argv=None) -> Config:
    argv = sys.argv[1:] if argv is None else argv
    parser = build_parser()
    ns = parser.parse_args(argv)
    cfg = Config()
    cfg.argv = argv
    cfg.infile = ns.infile or ""
    cfg.base_dir = ns.base_dir
    cfg.out_file = (ns.out_file or "a").split('/')[-1].split('.')[0]
    cfg.force = bool(ns.force)
    cfg.log_file = ns.log_file
    cfg.lowercase_first_names = bool(ns.lowercase_first_names)
    cfg.trackid = bool(ns.trackid)
    cfg.case_surnames = ns.case_surnames or "NoCase"
    cfg.fne = ns.fne
    cfg.extract_first_names = getattr(ns, "extract_first_names", None)
    cfg.extract_public_names = getattr(ns, "extract_public_names", None)
    cfg.no_public_if_titles = bool(ns.no_public_if_titles)
    cfg.try_negative_dates = bool(ns.try_negative_dates)
    cfg.no_negative_dates = bool(ns.no_negative_dates)
    cfg.do_check = bool(getattr(ns, "do_check", True))
    cfg.no_picture = bool(ns.no_picture)
    cfg.udi = ns.udi
    cfg.uin = bool(ns.uin)
    cfg.default_source = ns.default_source or ""
    cfg.dates_dm = bool(ns.dates_dm)
    cfg.dates_md = bool(ns.dates_md)
    cfg.relation_status = ns.relation_status or "Married"
    cfg.charset = ns.charset
    cfg.particles_file = ns.particles_file
    cfg.reorg = bool(ns.reorg)
    if cfg.log_file:
        logging.basicConfig(filename=cfg.log_file, level=logging.INFO, format='%(asctime)s %(levelname)s: %(message)s')
    else:
        logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
    return cfg

def print_options_list():
    for opt, desc, impl in get_options_status():
        status = "Refactored" if impl else "Pending"
        print(f"{opt:20} - {desc} [{status}]")

def main(argv=None):
    cfg = parse_args(argv)
    logging.info("Configuration parsed:")
    for k, v in cfg.__dict__.items():
        logging.info("  %s = %r", k, v)
    print("\nAvailable options and refactor status:")
    print_options_list()
    return cfg

if __name__ == "__main__":
    main()

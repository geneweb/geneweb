"""Conversion options for GED2GWB."""

from dataclasses import dataclass
from pathlib import Path
from typing import Optional


@dataclass
class ConversionOptions:
    """Options for GEDCOM to GeneWeb conversion."""

    input_file: Path
    output_file: Path = Path("base.pkl")
    base_dir: Optional[Path] = None  # Base directory for databases

    charset: Optional[str] = None  # ANSEL, ASCII, MSDOS

    dates_dm: bool = False  # day/month/year
    dates_md: bool = False  # month/day/year
    no_nd: bool = False  # Don't interpret minus as negative year
    tnd: bool = False  # Set negative dates when inconsistency (e.g. birth after death)

    efn: bool = False  # Handle multiple first names
    epn: bool = False  # Handle public names
    no_efn: bool = False  # Cancel efn
    no_epn: bool = False  # Cancel epn
    fne: Optional[str] = None  # First name extraction pattern
    lf: bool = False  # Convert first names to lowercase with uppercase initials
    ls: bool = False  # Convert surnames to lowercase with uppercase initials
    us: bool = False  # Convert surnames to uppercase

    particles_file: Optional[Path] = None  # Custom particles file

    default_source: Optional[str] = None  # Default source for records without source

    rs_no_mention: bool = False  # Force relation status to NoMention

    no_pit: bool = False  # Don't consider persons with titles as public

    no_picture: bool = False  # Don't extract individual pictures

    udi: Optional[tuple] = None  # (x, y) years for undefined death

    uin: bool = False  # Put untreated GEDCOM tags in notes

    compress: bool = True  # Use gzip compression
    force: bool = False  # Overwrite existing files

    log_file: Optional[Path] = None
    verbose: bool = False
    track_id: bool = False  # Print gedcom id to gw id matches

    no_consistency_check: bool = False  # Skip consistency checks
    nc: bool = False  # No consistency check (alias)

    def __post_init__(self):
        if self.dates_dm and self.dates_md:
            raise ValueError("Cannot specify both --dates-dm and --dates-md")

        if self.efn and self.no_efn:
            raise ValueError("Cannot specify both --efn and --no-efn")

        if self.epn and self.no_epn:
            raise ValueError("Cannot specify both --epn and --no-epn")

        if self.udi and (not isinstance(self.udi, tuple) or len(self.udi) != 2):
            raise ValueError("--udi must be a tuple of (x, y) years")

    @classmethod
    def from_args(cls, args) -> "ConversionOptions":
        """Create options from command line arguments."""
        return cls(
            input_file=args.gedcom_file,
            output_file=args.output if args.output is not None else Path("a.pkl"),
            base_dir=args.base_dir,
            charset=args.charset,
            dates_dm=args.dates_dm,
            dates_md=args.dates_md,
            no_nd=args.no_nd,
            tnd=args.tnd,
            efn=args.efn,
            epn=args.epn,
            no_efn=args.no_efn,
            no_epn=args.no_epn,
            fne=args.fne,
            lf=args.lf,
            ls=args.ls,
            us=args.us,
            particles_file=args.particles,
            default_source=args.ds,
            rs_no_mention=args.rs_no_mention,
            no_pit=args.no_pit,
            no_picture=args.nopicture,
            udi=(
                args.udi
                if isinstance(args.udi, tuple)
                else (
                    (int(args.udi.split("-")[0]), int(args.udi.split("-")[1]))
                    if args.udi
                    else None
                )
            ),
            uin=args.uin,
            compress=args.compress,
            force=args.force,
            log_file=args.log,
            verbose=args.verbose,
            track_id=args.trackid,
            no_consistency_check=args.no_consistency_check or args.nc,
            nc=args.nc,
        )

    def get_gedcom_parser_options(self) -> dict:
        """Get options for GEDCOM parser."""
        return {"preserve_bom": True, "preserve_notes": not self.no_picture}

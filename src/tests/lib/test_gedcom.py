import lib.db.gedcom as gedcom
import pytest
import os


@pytest.fixture(scope="module")
def pwd():
    return os.path.dirname(__file__)


@pytest.fixture(scope="module")
def path(pwd):
    return f"{pwd}/test_data/uk.ged"


@pytest.fixture(scope="module")
def parsed_data(path) -> gedcom.GedcomData:
    return gedcom.GedcomParser.from_file(path)


def assert_uk_complete(uk_data: gedcom.GedcomData) -> None:
    assert uk_data is not None
    assert uk_data.header is not None
    assert uk_data.header.source is not None
    assert uk_data.header.source.version == "24.0.1.1252"
    assert uk_data.header.source.name == "Family Tree Maker for Windows"
    assert uk_data.header.source.corporation == "The Software MacKiev Company"
    assert uk_data.header.source.address == "30 Union Wharf\nBoston, MA 02109"
    assert uk_data.header.source.phone == "(617) 227-6681"
    assert len(uk_data.individuals) == 2_322
    assert len(uk_data.families) == 1115
    assert len(uk_data.multimedia) == 171
    assert len(uk_data.notes) == 141
    assert uk_data.repository is not None
    assert uk_data.submitter is not None
    assert len(uk_data.sources) == 91


def test_open_file(parsed_data: gedcom.GedcomData):
    assert_uk_complete(parsed_data)


def test_pickle(parsed_data: gedcom.GedcomData):
    import pickle

    data = pickle.dumps(parsed_data)
    parsed_2 = pickle.loads(data)
    assert_uk_complete(parsed_2)


def test_individual(parsed_data: gedcom.GedcomData):
    ind = parsed_data.individuals.get("@I570@")
    assert ind is not None
    assert isinstance(ind, gedcom.GEDIndividualRecord)
    assert ind.name == ["Elizabeth Ii /Windsor/"]
    assert ind.sex == "F"
    assert ind.birth is not None
    assert ind.birth.date.value == "21 APR 1926"
    assert ind.birth.date.year == 1926
    assert ind.birth.date.day == 21
    assert ind.birth.date.month == 4


def test_family(parsed_data: gedcom.GedcomData):
    fam = parsed_data.families.get("@F298@")
    assert fam is not None
    assert isinstance(fam, gedcom.GEDFamily)
    assert fam.wife.value == "@I570@"
    assert fam.husband.value == "@I571@"
    assert fam.children[0].value == "@I572@"
    assert fam.marriage.date is not None
    assert fam.marriage.date.value == "20 NOV 1947"


def test_address_and_place(parsed_data: gedcom.GedcomData):
    # Test place structure in birth events
    for k, ind in parsed_data.individuals.items():
        if ind.birth and ind.birth.place:
            assert isinstance(
                ind.birth.place, gedcom.GEDPlace
            ), f"{k} does not have a bp"


def test_event_details(parsed_data: gedcom.GedcomData):
    # Test individual event details (Elizabeth II)
    ind = parsed_data.individuals.get("@I570@")
    assert ind is not None

    # Test birth event
    assert ind.birth is not None
    assert isinstance(ind.birth, gedcom.GEDIndiEventDetail)
    assert ind.birth.date is not None
    assert ind.birth.date.year == 1926
    assert ind.birth.date.month == 4  # APR
    assert ind.birth.date.day == 21

    # Test family event details (Marriage)
    fam = parsed_data.families.get("@F382@")
    assert fam is not None
    assert fam.marriage is not None
    assert isinstance(fam.marriage, gedcom.GEDFamEventDetail)
    assert fam.marriage.date is not None
    assert fam.marriage.date.value == "11 SEP 1795"
    assert fam.marriage.date.year == 1795
    assert fam.marriage.date.month == 9  # SEP
    assert fam.marriage.date.day == 11
    assert len(fam.marriage.sources) == 2
    assert (
        fam.marriage.sources[0].page
        == "Source number: 11.000; Source type: Electronic Database; Number of Pages: 1; Submitter Code: FAI"
    )
    assert fam.marriage.sources[0].text == "Record for John Field"


def test_sources_and_repositories(parsed_data: gedcom.GedcomData):
    # Test that we have some sources
    assert len(parsed_data.sources) > 0

    # Get a source record
    for source_id, source in parsed_data.sources.items():
        assert isinstance(source, gedcom.GEDSourceRecord)
        # Test source record attributes
        if source.title:
            assert isinstance(source.title, str)
        if source.abbreviation:
            assert isinstance(source.abbreviation, str)
        if source.publication_facts:
            assert isinstance(source.publication_facts, str)
        if source.text:
            assert isinstance(source.text, str)

        # Test source citations
        if source.repositories:
            for repo in source.repositories:
                assert isinstance(repo, gedcom.GEDRepoCitation)
        break

    # Test source citations in individual records
    ind = parsed_data.individuals.get("@I570@")  # Elizabeth II
    assert ind is not None
    if ind.sources:
        for source in ind.sources:
            assert isinstance(source, gedcom.GEDSourceCitation)
            if source.quality:
                assert isinstance(source.quality, gedcom.GEDQuality)
                assert source.quality in [
                    gedcom.GEDQuality.UNRELIABLE,
                    gedcom.GEDQuality.QUESTIONABLE,
                    gedcom.GEDQuality.SECONDARY,
                    gedcom.GEDQuality.DIRECT,
                ]


def test_multimedia(parsed_data: gedcom.GedcomData):
    # Test multimedia records
    assert len(parsed_data.multimedia) > 0

    # Test multimedia links in individual records
    for ind in parsed_data.individuals.values():
        if ind.multimedia_links:
            for media in ind.multimedia_links:
                assert isinstance(media, gedcom.GEDMultimediaLink)
                if media.title:
                    assert isinstance(media.title, str)
                # Test crop if present
                if media.crop:
                    assert isinstance(media.crop, tuple)
                    assert len(media.crop) == 4
                    assert all(isinstance(x, int) for x in media.crop)
            break

    # Test multimedia records directly
    for multimedia_id, multimedia in parsed_data.multimedia.items():
        assert isinstance(multimedia, gedcom.GEDMultimediaRecord)
        # Test multimedia record attributes
        if multimedia.notes:
            assert isinstance(multimedia.notes, list)
            assert all(isinstance(note, gedcom.GEDNotes) for note in multimedia.notes)
        if multimedia.sources:
            assert isinstance(multimedia.sources, list)
            assert all(
                isinstance(source, gedcom.GEDSourceCitation)
                for source in multimedia.sources
            )
        break


def test_notes(parsed_data: gedcom.GedcomData):
    # Test notes in individual records
    ind = parsed_data.individuals.get("@I570@")  # Elizabeth II
    assert ind is not None

    if ind.notes:
        for note in ind.notes:
            assert isinstance(note, gedcom.GEDNotes)
            # Notes should have a string value
            assert isinstance(str(note), str)

    # Test note records
    assert parsed_data.notes is not None
    assert len(parsed_data.notes) > 0

    # Test a specific note record
    for note_id, note in parsed_data.notes.items():
        assert isinstance(note, gedcom.GEDNotesRecord)
        # Notes should have a string value
        assert isinstance(str(note), str)
        # Test sources if present
        if note.sources:
            for source in note.sources:
                assert isinstance(source, gedcom.GEDSourceCitation)
        break

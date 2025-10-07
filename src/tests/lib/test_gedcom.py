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
    assert isinstance(ind, gedcom.GEDIndividual)
    assert ind.name == ["Elizabeth Ii /Windsor/"]
    assert ind.sex == "F"
    assert ind.birth is not None
    assert ind.birth.date.value == "21 APR 1926"
    assert ind.birth.date.year == 1926
    assert ind.birth.date.day == 21
    assert ind.birth.date.month == 3


def test_family(parsed_data: gedcom.GedcomData):

    fam = parsed_data.families.get("@F298@")
    assert fam is not None
    assert isinstance(fam, gedcom.GEDFamily)
    assert fam.wife.value == "@I570@"
    assert fam.husband.value == "@I571@"
    assert fam.children[0].value == "@I572@"
    assert fam.marriage.date is not None
    assert fam.marriage.date.value == "20 NOV 1947"

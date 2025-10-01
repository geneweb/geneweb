"""Essential tests for consang core functionality."""

import pytest
from consang import ConsanguinityCalculator, ConsangCLI, GenewebDatabase


@pytest.mark.unit
@pytest.mark.consang
class TestConsangCore:
    """Core functionality tests."""

    def test_calculator_initialization(self):
        """Test calculator initializes correctly."""
        calc = ConsanguinityCalculator()
        assert calc.quiet_level == 0
        assert calc.fast_mode is False

    def test_cli_initialization(self):
        """Test CLI initializes correctly."""
        cli = ConsangCLI()
        assert cli.program_name == "consang"
        assert cli.parser is not None

    def test_cli_help_works(self):
        """Test help functionality."""
        cli = ConsangCLI()
        with pytest.raises(SystemExit) as exc_info:
            cli.run(['--help'])
        assert exc_info.value.code == 0

    def test_cli_error_handling(self):
        """Test basic error handling."""
        cli = ConsangCLI()

        exit_code = cli.run([])
        assert exit_code == 2

        exit_code = cli.run(['-h'])
        assert exit_code == 2

    def test_database_with_valid_path(self, mock_gwb_database):
        """Test database works with valid path."""
        db = GenewebDatabase(mock_gwb_database)
        assert db.database_path == mock_gwb_database
        assert db.total_persons >= 0

    def test_database_operations(self, mock_gwb_database):
        """Test essential database operations."""
        db = GenewebDatabase(mock_gwb_database)

        persons = db.get_all_persons()
        assert isinstance(persons, list)

        needing_calc = db.get_persons_needing_calculation()
        assert isinstance(needing_calc, list)

    def test_calculator_with_database(self, mock_gwb_database):
        """Test calculator works with database."""
        calc = ConsanguinityCalculator(quiet_level=2)
        db = GenewebDatabase(mock_gwb_database)

        result = calc.calculate(db)
        assert result.persons_processed >= 0
        assert result.calculation_time >= 0

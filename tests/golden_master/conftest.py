"""
Configuration pytest pour les tests Golden Master génériques
"""

import pytest
import json
from pathlib import Path

def pytest_addoption(parser):
    """Ajoute des options personnalisées à pytest"""
    parser.addoption(
        "--update-golden", 
        action="store_true", 
        default=False,
        help="Update golden masters with reference executable"
    )
    parser.addoption(
        "--test-config", 
        action="store", 
        default="test_config.json",
        help="Path to test configuration file"
    )

@pytest.fixture(scope="session")
def update_golden(request):
    """Fixture pour le mode mise à jour des golden masters"""
    return request.config.getoption("--update-golden")

@pytest.fixture(scope="session")
def test_config(request):
    """Fixture pour charger la configuration des tests"""
    config_path = request.config.getoption("--test-config")
    with open(config_path, 'r', encoding='utf-8') as f:
        return json.load(f)

def pytest_generate_tests(metafunc):
    """Génère dynamiquement les tests paramétrés"""
    if "test_case" in metafunc.fixturenames:
        config_path = metafunc.config.getoption("--test-config")
        with open(config_path, 'r', encoding='utf-8') as f:
            config = json.load(f)
        
        test_cases = config['test_cases']
        test_ids = [tc['name'] for tc in test_cases]
        
        metafunc.parametrize("test_case", test_cases, ids=test_ids)

def pytest_configure(config):
    """Configuration pytest"""
    config.addinivalue_line("markers", "slow: mark tests as slow")
    config.addinivalue_line("markers", "integration: mark integration tests") 
    config.addinivalue_line("markers", "golden_master: mark golden master tests")

def pytest_collection_modifyitems(config, items):
    """Modifie la collection de tests"""
    for item in items:
        # Ajoute automatiquement le marqueur golden_master
        if "golden_master" in item.nodeid:
            item.add_marker(pytest.mark.golden_master)
            item.add_marker(pytest.mark.integration)
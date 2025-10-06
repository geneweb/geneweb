def pytest_configure(config):
    """Configure pytest markers."""
    config.addinivalue_line("markers", "consang: Tests for consang binary")
    config.addinivalue_line("markers", "unit: Unit tests")
    config.addinivalue_line("markers", "integration: Integration tests")
    config.addinivalue_line("markers", "compatibility: Compatibility tests")
    config.addinivalue_line("markers", "performance: Performance tests")
    config.addinivalue_line("markers", "slow: Slow tests (>5s)")

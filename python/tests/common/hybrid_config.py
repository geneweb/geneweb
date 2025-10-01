"""Gestionnaire de configuration hybride YAML+JSON optimisé."""

import yaml
import json
from pathlib import Path
from typing import Dict, Any, Optional, List, Union
from dataclasses import dataclass, field
import os


@dataclass
class TestCase:
    """Représente un cas de test Golden Master."""
    name: str
    description: str
    args: List[str]
    expected_exit_code: int
    stdout_must_contain: List[str] = field(default_factory=list)
    stderr_must_contain: List[str] = field(default_factory=list)
    stdout_must_be_empty: bool = False
    stderr_must_be_empty: bool = False
    timeout: int = 30

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'TestCase':
        """Créer un TestCase à partir d'un dictionnaire."""
        return cls(
            name=data["name"],
            description=data.get("description", ""),
            args=data["args"],
            expected_exit_code=data["expected_exit_code"],
            stdout_must_contain=data.get("stdout_must_contain", []),
            stderr_must_contain=data.get("stderr_must_contain", []),
            stdout_must_be_empty=data.get("stdout_must_be_empty", False),
            stderr_must_be_empty=data.get("stderr_must_be_empty", False),
            timeout=data.get("timeout", 30)
        )


@dataclass
class BinaryConfig:
    """Configuration d'un binaire."""
    name: str
    description: str
    enabled: bool
    python_module: str
    ocaml_binary: str
    test_suites: Dict[str, Any] = field(default_factory=dict)


@dataclass
class GlobalConfig:
    """Configuration globale."""
    coverage_threshold: int = 85
    timeout_default: int = 10
    reports_dir: str = "tests/reports"
    parallel_jobs: int = 4
    retry_failed: int = 2
    python_path: str = "python"


class HybridConfigManager:
    """Gestionnaire de configuration hybride YAML+JSON."""

    def __init__(self, config_dir: Path = None):
        """Initialiser avec le répertoire de configuration."""
        if config_dir is None:
            config_dir = Path(__file__).parent.parent / "config"

        self.config_dir = config_dir
        self._main_config = None
        self._golden_data = None
        self._global_config = None

    def load_main_config(self) -> Dict[str, Any]:
        """Charger la configuration principale YAML."""
        if self._main_config is not None:
            return self._main_config

        yaml_file = self.config_dir / "main.yaml"

        if not yaml_file.exists():
            raise FileNotFoundError(f"Configuration YAML non trouvée: {yaml_file}")

        with open(yaml_file, 'r', encoding='utf-8') as f:
            self._main_config = yaml.safe_load(f)

        return self._main_config

    def load_golden_data(self) -> Dict[str, Any]:
        """Charger les données Golden Master JSON."""
        if self._golden_data is not None:
            return self._golden_data

        json_file = self.config_dir / "golden_data.json"

        if not json_file.exists():
            self._golden_data = {}
            return self._golden_data

        with open(json_file, 'r', encoding='utf-8') as f:
            self._golden_data = json.load(f)

        return self._golden_data

    def get_global_config(self) -> GlobalConfig:
        """Obtenir la configuration globale."""
        if self._global_config is not None:
            return self._global_config

        config = self.load_main_config()
        global_data = config.get("global", {})

        self._global_config = GlobalConfig(
            coverage_threshold=global_data.get("coverage_threshold", 85),
            timeout_default=global_data.get("timeout_default", 10),
            reports_dir=global_data.get("reports_dir", "tests/reports"),
            parallel_jobs=global_data.get("parallel_jobs", 4),
            retry_failed=global_data.get("retry_failed", 2),
            python_path=global_data.get("python_path", "python")
        )

        return self._global_config

    def get_binary_config(self, binary_name: str) -> Optional[BinaryConfig]:
        """Obtenir la configuration d'un binaire."""
        config = self.load_main_config()
        binaries = config.get("binaries", {})
        binary_data = binaries.get(binary_name)

        if not binary_data:
            return None

        return BinaryConfig(
            name=binary_name,
            description=binary_data.get("description", ""),
            enabled=binary_data.get("enabled", False),
            python_module=binary_data.get("python_module", ""),
            ocaml_binary=binary_data.get("ocaml_binary", ""),
            test_suites=binary_data.get("test_suites", {})
        )

    def is_binary_enabled(self, binary_name: str) -> bool:
        """Vérifier si un binaire est activé."""
        binary_config = self.get_binary_config(binary_name)
        return binary_config.enabled if binary_config else False

    def get_enabled_binaries(self) -> List[str]:
        """Obtenir la liste des binaires activés."""
        config = self.load_main_config()
        binaries = config.get("binaries", {})

        return [
            name for name, cfg in binaries.items()
            if cfg.get("enabled", False)
        ]

    def get_test_cases(self, binary_name: str) -> List[TestCase]:
        """Obtenir les cas de test pour un binaire."""
        golden_data = self.load_golden_data()
        binary_data = golden_data.get(binary_name, {})
        raw_cases = binary_data.get("test_cases", [])

        return [TestCase.from_dict(case_data) for case_data in raw_cases]

    def get_test_suite_config(self, binary_name: str, suite_name: str) -> Optional[Dict[str, Any]]:
        """Obtenir la configuration d'une suite de tests."""
        binary_config = self.get_binary_config(binary_name)
        if not binary_config:
            return None

        return binary_config.test_suites.get(suite_name)

    def is_test_suite_enabled(self, binary_name: str, suite_name: str) -> bool:
        """Vérifier si une suite de tests est activée."""
        suite_config = self.get_test_suite_config(binary_name, suite_name)
        return suite_config.get("enabled", False) if suite_config else False

    def get_pytest_options(self) -> List[str]:
        """Obtenir les options pytest."""
        config = self.load_main_config()
        return config.get("pytest", {}).get("options", [])

    def get_pytest_markers(self) -> List[str]:
        """Obtenir les marqueurs pytest."""
        config = self.load_main_config()
        return config.get("pytest", {}).get("markers", [])

    def setup_environment(self) -> None:
        """Configurer l'environnement de test."""
        config = self.load_main_config()
        env_config = config.get("environment", {})
        env_vars = env_config.get("env_vars", {})

        # Définir les variables d'environnement
        for key, value in env_vars.items():
            os.environ[key] = str(value)

    def validate_config(self) -> List[str]:
        """Valider la configuration et retourner les erreurs."""
        errors = []

        try:
            main_config = self.load_main_config()
        except Exception as e:
            errors.append(f"Erreur chargement YAML: {e}")
            return errors

        try:
            golden_data = self.load_golden_data()
        except Exception as e:
            errors.append(f"Erreur chargement JSON: {e}")

        # Valider la structure
        if "binaries" not in main_config:
            errors.append("Section 'binaries' manquante dans main.yaml")

        if "global" not in main_config:
            errors.append("Section 'global' manquante dans main.yaml")

        # Valider les binaires activés
        for binary_name in self.get_enabled_binaries():
            if binary_name not in golden_data:
                errors.append(f"Données Golden Master manquantes pour {binary_name}")

        return errors

    def get_summary(self) -> Dict[str, Any]:
        """Obtenir un résumé de la configuration."""
        try:
            main_config = self.load_main_config()
            golden_data = self.load_golden_data()
            enabled_binaries = self.get_enabled_binaries()

            summary = {
                "project": main_config.get("project", {}),
                "enabled_binaries": enabled_binaries,
                "total_test_cases": sum(
                    len(golden_data.get(binary, {}).get("test_cases", []))
                    for binary in enabled_binaries
                ),
                "test_suites": {}
            }

            for binary in enabled_binaries:
                binary_config = self.get_binary_config(binary)
                if binary_config:
                    enabled_suites = [
                        suite for suite, config in binary_config.test_suites.items()
                        if config.get("enabled", False)
                    ]
                    summary["test_suites"][binary] = enabled_suites

            return summary

        except Exception as e:
            return {"error": str(e)}


# Instance globale
hybrid_config = HybridConfigManager()

#!/usr/bin/env python3
"""
Golden Master Test Framework - Générique
Compare deux exécutables quelconques avec pytest
"""

import json
import os
import shutil
import subprocess
import tempfile
import difflib
import re
import glob
import pytest
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any, Set
import logging

logging.basicConfig(level=logging.INFO, format='%(levelname)s: %(message)s')
logger = logging.getLogger(__name__)

class GenericGoldenMaster:
    """Framework générique de Golden Master pour comparer deux exécutables"""
    
    def __init__(self, config_path: str = "test_config.json"):
        self.config_path = Path(config_path)
        self.config = self._load_config()
        self.base_dir = Path.cwd()
        self._setup_directories()
    
    def _load_config(self) -> Dict:
        """Charge la configuration depuis le fichier JSON"""
        try:
            with open(self.config_path, 'r', encoding='utf-8') as f:
                return json.load(f)
        except FileNotFoundError:
            raise FileNotFoundError(f"Config file not found: {self.config_path}")
        except json.JSONDecodeError as e:
            raise ValueError(f"Invalid JSON config: {e}")
    
    def _setup_directories(self):
        """Crée les répertoires nécessaires"""
        paths = self.config['test_config']['paths']
        for path_key, path_value in paths.items():
            full_path = self.base_dir / path_value
            full_path.mkdir(parents=True, exist_ok=True)
    
    def _build_command(self, executable_type: str, args: List[str], work_dir: Path) -> List[str]:
        """Construit la commande à exécuter"""
        exec_config = self.config['test_config']['executables'][executable_type]
        
        if exec_config['type'] == 'binary':
            cmd = [str(self.base_dir / exec_config['path'])]
        elif exec_config['type'] == 'script':
            interpreter = exec_config.get('interpreter', 'python3')
            script_path = self.base_dir / exec_config['path']
            cmd = [interpreter, str(script_path)]
        else:
            raise ValueError(f"Unknown executable type: {exec_config['type']}")
        
        # Ajoute les arguments
        cmd.extend(args)
        
        return cmd
    
    def _normalize_content(self, content: str) -> str:
        """Normalise le contenu pour la comparaison"""
        comparison = self.config['test_config']['comparison']
        
        # Supprime les patterns à ignorer
        for pattern in comparison.get('ignore_patterns', []):
            content = re.sub(pattern, '', content, flags=re.MULTILINE)
        
        # Normalisation
        if comparison.get('normalize_line_endings', True):
            content = content.replace('\r\n', '\n').replace('\r', '\n')
        
        if comparison.get('ignore_empty_lines', False):
            lines = [line for line in content.split('\n') if line.strip()]
            content = '\n'.join(lines)
        
        if comparison.get('normalize_whitespace', True):
            content = re.sub(r'[ \t]+', ' ', content)
            content = re.sub(r' *\n *', '\n', content)
        
        return content.strip()
    
    def _run_executable(self, executable_type: str, test_case: Dict, work_dir: Path) -> Dict[str, Any]:
        """Exécute un exécutable et capture les résultats"""
        env_config = self.config['test_config']['environment']
        timeout = env_config.get('timeout_seconds', 30)
        encoding = env_config.get('encoding', 'utf-8')
        
        # Construit la commande
        cmd = self._build_command(executable_type, test_case.get('args', []), work_dir)
        
        # Ajoute les fichiers d'entrée à la fin
        for input_file in test_case.get('input_files', []):
            cmd.append(str(work_dir / input_file))
        
        logger.info(f"Running {executable_type}: {' '.join(cmd)}")
        
        # Variables d'environnement
        env = os.environ.copy()
        env.update(env_config.get('env_vars', {}))
        
        try:
            result = subprocess.run(
                cmd,
                cwd=work_dir,
                capture_output=True,
                text=True,
                timeout=timeout,
                encoding=encoding,
                env=env
            )
            
            return {
                'stdout': result.stdout,
                'stderr': result.stderr,
                'returncode': result.returncode,
                'success': True,
                'error': None
            }
            
        except subprocess.TimeoutExpired:
            return {
                'stdout': '',
                'stderr': f'Timeout after {timeout} seconds',
                'returncode': -1,
                'success': False,
                'error': 'timeout'
            }
        except Exception as e:
            return {
                'stdout': '',
                'stderr': f'Execution error: {e}',
                'returncode': -1,
                'success': False,
                'error': str(e)
            }
    
    def _collect_all_generated_outputs(self, work_dir: Path, initial_files: set) -> Dict[str, str]:
        """Collecte TOUS les fichiers/dossiers générés après exécution"""
        output_files = {}
        
        # Parcourt récursivement TOUT le répertoire de travail
        for root, dirs, files in os.walk(work_dir):
            root_path = Path(root)
            
            # Traite tous les fichiers
            for filename in files:
                file_path = root_path / filename
                relative_path = file_path.relative_to(work_dir)
                
                # Ignore les fichiers d'entrée initiaux
                if str(relative_path) in initial_files:
                    continue
                
                relative_name = str(relative_path)
                
                try:
                    # Essaie d'abord de lire comme texte UTF-8
                    content = file_path.read_text(encoding='utf-8')
                    output_files[relative_name] = self._normalize_content(content)
                    logger.info(f"Captured text file: {relative_name} ({len(content)} chars)")
                except UnicodeDecodeError:
                    # Si c'est un fichier binaire, crée un hash
                    try:
                        import hashlib
                        with open(file_path, 'rb') as f:
                            binary_hash = hashlib.md5(f.read()).hexdigest()
                        output_files[relative_name] = f"BINARY_MD5:{binary_hash}"
                        logger.info(f"Captured binary file: {relative_name} (MD5: {binary_hash})")
                    except Exception as e:
                        logger.warning(f"Could not read binary file {file_path}: {e}")
                        output_files[relative_name] = f"ERROR: {e}"
                except Exception as e:
                    logger.warning(f"Could not read file {file_path}: {e}")
                    output_files[relative_name] = f"ERROR: {e}"
            
            # Traite tous les répertoires
            for dirname in dirs:
                dir_path = root_path / dirname
                relative_path = dir_path.relative_to(work_dir)
                relative_name = str(relative_path)
                
                try:
                    # Crée un hash du contenu du répertoire
                    dir_hash = self._hash_directory(dir_path)
                    output_files[relative_name] = f"DIRECTORY_HASH:{dir_hash}"
                    logger.info(f"Captured directory: {relative_name} (Hash: {dir_hash})")
                except Exception as e:
                    logger.warning(f"Could not hash directory {dir_path}: {e}")
                    output_files[relative_name] = f"ERROR: {e}"
        
        return output_files
    
    def _capture_initial_state(self, work_dir: Path) -> Set[str]:
        """Capture l'état initial du répertoire de travail"""
        initial_files = set()
        
        for root, dirs, files in os.walk(work_dir):
            root_path = Path(root)
            for filename in files:
                file_path = root_path / filename
                relative_path = file_path.relative_to(work_dir)
                initial_files.add(str(relative_path))
        
        logger.info(f"Initial state: {len(initial_files)} files")
        return initial_files
    
    def _hash_directory(self, dir_path: Path) -> str:
        """Crée un hash MD5 du contenu d'un répertoire"""
        import hashlib
        import os
        
        hash_md5 = hashlib.md5()
        
        # Parcourt récursivement tous les fichiers du répertoire
        for root, dirs, files in os.walk(dir_path):
            # Trie pour avoir un hash déterministe
            dirs.sort()
            files.sort()
            
            for filename in files:
                file_path = os.path.join(root, filename)
                rel_path = os.path.relpath(file_path, dir_path)
                
                # Ajoute le nom relatif du fichier au hash
                hash_md5.update(rel_path.encode('utf-8'))
                
                # Ajoute le contenu du fichier au hash
                try:
                    with open(file_path, 'rb') as f:
                        while chunk := f.read(8192):
                            hash_md5.update(chunk)
                except Exception as e:
                    # En cas d'erreur, ajoute l'erreur au hash
                    hash_md5.update(f"ERROR:{e}".encode('utf-8'))
        
        return hash_md5.hexdigest()
    
    def _collect_directory_manifests(self, work_dir: Path, patterns: List[str]) -> Dict[str, Dict[str, str]]:
        """Collecte le contenu des répertoires correspondants aux patterns.
        Retourne un dict: { dirname: { rel_path: content_or_binary_hash } }"""
        import hashlib
        import os
        manifests: Dict[str, Dict[str, str]] = {}
        
        for pattern in patterns:
            matches = glob.glob(str(work_dir / pattern))
            for match in matches:
                dir_path = Path(match)
                if not dir_path.is_dir():
                    continue
                dirname = dir_path.name
                manifest: Dict[str, str] = {}
                for root, dirs, files in os.walk(dir_path):
                    dirs.sort()
                    files.sort()
                    for filename in files:
                        file_path = Path(root) / filename
                        rel_path = str(file_path.relative_to(dir_path))
                        try:
                            data = file_path.read_bytes()
                            try:
                                text = data.decode('utf-8')
                                manifest[rel_path] = self._normalize_content(text)
                            except UnicodeDecodeError:
                                md5 = hashlib.md5(data).hexdigest()
                                manifest[rel_path] = f"BINARY_MD5:{md5}"
                        except Exception as e:
                            manifest[rel_path] = f"ERROR:{e}"
                manifests[dirname] = manifest
        return manifests
    
    def _setup_test_environment(self, test_case: Dict, work_dir: Path):
        """Prépare l'environnement de test"""
        test_data_dir = self.base_dir / self.config['test_config']['paths']['test_data_dir']
        
        # Copie les fichiers d'entrée
        for input_file in test_case.get('input_files', []):
            src = test_data_dir / input_file
            dst = work_dir / input_file
            
            if src.exists():
                shutil.copy2(src, dst)
            else:
                # Crée un fichier de test basique si pas trouvé
                self._create_sample_file(dst, input_file)
        
        # Exécute les commandes de setup
        for cmd in test_case.get('setup_commands', []):
            try:
                subprocess.run(cmd, shell=True, cwd=work_dir, check=True)
            except subprocess.CalledProcessError as e:
                logger.warning(f"Setup command failed: {cmd} - {e}")
    
    def _create_sample_file(self, file_path: Path, filename: str):
        """Crée un fichier d'exemple si le fichier de test n'existe pas"""
        if filename.endswith('.ged'):
            content = """0 HEAD
1 SOUR Generic Golden Master Test
1 GEDC
2 VERS 5.5
2 FORM LINEAGE-LINKED
1 CHAR UTF-8
0 @I1@ INDI
1 NAME Test /PERSON/
1 SEX M
1 BIRT
2 DATE 1 JAN 1950
0 TRLR"""
        else:
            content = f"# Test file: {filename}\nGeneric test content\n"
        
        file_path.write_text(content, encoding='utf-8')
        logger.info(f"Created sample file: {file_path}")
    
    def _save_golden_master(self, test_name: str, reference_result: Dict):
        """Sauvegarde le golden master avec structure séparée"""
        golden_dir = self.base_dir / self.config['test_config']['paths']['golden_dir']
        test_golden_dir = golden_dir / test_name
        test_golden_dir.mkdir(parents=True, exist_ok=True)
        
        # Crée les sous-répertoires pour l'organisation
        files_dir = test_golden_dir / 'files'
        files_dir.mkdir(exist_ok=True)
        
        # Sauvegarde SEULEMENT les métadonnées dans golden.json
        golden_data = {
            'stdout': reference_result['stdout'],
            'stderr': reference_result['stderr'], 
            'returncode': reference_result['returncode'],
            'success': reference_result['success'],
            'output_files_count': len(reference_result.get('output_files', {})),
            'dir_outputs_count': len(reference_result.get('dir_outputs', {}))
        }
        
        golden_file = test_golden_dir / 'golden.json'
        with open(golden_file, 'w', encoding='utf-8') as f:
            json.dump(golden_data, f, indent=2, ensure_ascii=False)
        
        # Sauvegarde les fichiers générés dans files/
        output_files = reference_result.get('output_files', {})
        for filename, content in output_files.items():
            safe_filename = filename.replace('/', '_')  # Évite les problèmes de path
            
            if content.startswith('BINARY_MD5:'):
                # Pour les binaires, sauvegarde juste le hash
                hash_file = files_dir / f"{safe_filename}.hash"
                hash_file.write_text(content, encoding='utf-8')
            elif content.startswith('DIRECTORY_HASH:'):
                # Pour les répertoires, sauvegarde juste le hash
                hash_file = files_dir / f"{safe_filename}.dir_hash"
                hash_file.write_text(content, encoding='utf-8')
            else:
                # Pour les fichiers texte, sauvegarde le contenu
                try:
                    content_file = files_dir / safe_filename
                    content_file.write_text(content, encoding='utf-8')
                except Exception as e:
                    logger.warning(f"Could not save output file {filename}: {e}")
        
        # Sauvegarde un index des fichiers pour référence
        file_index = {
            'text_files': [],
            'binary_files': [],
            'directories': []
        }
        
        for filename, content in output_files.items():
            if content.startswith('BINARY_MD5:'):
                file_index['binary_files'].append(filename)
            elif content.startswith('DIRECTORY_HASH:'):
                file_index['directories'].append(filename)
            else:
                file_index['text_files'].append(filename)
        
        index_file = test_golden_dir / 'file_index.json'
        with open(index_file, 'w', encoding='utf-8') as f:
            json.dump(file_index, f, indent=2, ensure_ascii=False)
                
        logger.info(f"Golden master saved for {test_name}: {len(file_index['text_files'])} text files, {len(file_index['binary_files'])} binary files, {len(file_index['directories'])} directories")
    
    def _load_golden_master(self, test_name: str) -> Optional[Dict]:
        """Charge le golden master depuis la structure séparée"""
        golden_dir = self.base_dir / self.config['test_config']['paths']['golden_dir']
        test_golden_dir = golden_dir / test_name
        golden_file = test_golden_dir / 'golden.json'
        
        if not golden_file.exists():
            return None
        
        try:
            # Charge les métadonnées principales
            with open(golden_file, 'r', encoding='utf-8') as f:
                golden_data = json.load(f)
            
            # Charge l'index des fichiers
            index_file = test_golden_dir / 'file_index.json'
            if index_file.exists():
                with open(index_file, 'r', encoding='utf-8') as f:
                    file_index = json.load(f)
            else:
                file_index = {'text_files': [], 'binary_files': [], 'directories': []}
            
            # Reconstruit output_files depuis les fichiers séparés
            output_files = {}
            files_dir = test_golden_dir / 'files'
            
            if files_dir.exists():
                # Charge les fichiers texte
                for filename in file_index.get('text_files', []):
                    safe_filename = filename.replace('/', '_')
                    content_file = files_dir / safe_filename
                    if content_file.exists():
                        try:
                            output_files[filename] = content_file.read_text(encoding='utf-8')
                        except Exception as e:
                            logger.warning(f"Could not load text file {filename}: {e}")
                            output_files[filename] = f"ERROR: {e}"
                
                # Charge les hashs de fichiers binaires
                for filename in file_index.get('binary_files', []):
                    safe_filename = filename.replace('/', '_')
                    hash_file = files_dir / f"{safe_filename}.hash"
                    if hash_file.exists():
                        try:
                            output_files[filename] = hash_file.read_text(encoding='utf-8')
                        except Exception as e:
                            logger.warning(f"Could not load binary hash {filename}: {e}")
                            output_files[filename] = f"ERROR: {e}"
                
                # Charge les hashs de répertoires
                for filename in file_index.get('directories', []):
                    safe_filename = filename.replace('/', '_')
                    hash_file = files_dir / f"{safe_filename}.dir_hash"
                    if hash_file.exists():
                        try:
                            output_files[filename] = hash_file.read_text(encoding='utf-8')
                        except Exception as e:
                            logger.warning(f"Could not load directory hash {filename}: {e}")
                            output_files[filename] = f"ERROR: {e}"
            
            # Ajoute les fichiers reconstruits aux données golden
            golden_data['output_files'] = output_files
            golden_data['dir_outputs'] = {}  # Pour compatibilité
            
            return golden_data
            
        except Exception as e:
            logger.error(f"Failed to load golden master for {test_name}: {e}")
            return None
    
    def _compare_results(self, test_name: str, golden: Dict, candidate: Dict) -> bool:
        """Compare les résultats entre golden et candidate"""
        success = True
        output_config = self.config['test_config']['output_capture']
        
        # Compare return code
        if output_config.get('return_code', True):
            if golden['returncode'] != candidate['returncode']:
                success = False
                logger.error(f"FAIL {test_name} - Return code mismatch: "
                           f"expected {golden['returncode']}, got {candidate['returncode']}")
        
        # Compare stdout
        if output_config.get('stdout', True):
            golden_stdout = self._normalize_content(golden['stdout'])
            candidate_stdout = self._normalize_content(candidate['stdout'])
            if golden_stdout != candidate_stdout:
                success = False
                logger.error(f"FAIL {test_name} - Stdout differs")
                self._print_diff('stdout', golden_stdout, candidate_stdout)
        
        # Compare stderr
        if output_config.get('stderr', True):
            golden_stderr = self._normalize_content(golden['stderr'])
            candidate_stderr = self._normalize_content(candidate['stderr'])
            if golden_stderr != candidate_stderr:
                success = False
                logger.error(f"FAIL {test_name} - Stderr differs")
                self._print_diff('stderr', golden_stderr, candidate_stderr)
        
        # Compare output files
        if output_config.get('files', True):
            golden_files = golden.get('output_files', {})
            candidate_files = candidate.get('output_files', {})
            
            all_files = set(golden_files.keys()) | set(candidate_files.keys())
            
            for filename in all_files:
                golden_content = golden_files.get(filename, '')
                candidate_content = candidate_files.get(filename, '')
                
                if golden_content != candidate_content:
                    success = False
                    logger.error(f"FAIL {test_name} - File {filename} differs")
                    
                    # Si c'est un hash de répertoire ou binaire, affiche les hashs
                    if (golden_content.startswith('DIRECTORY_HASH:') or golden_content.startswith('BINARY_MD5:') or
                        candidate_content.startswith('DIRECTORY_HASH:') or candidate_content.startswith('BINARY_MD5:')):
                        print(f"\n--- Hash diff for {filename} ---")
                        print(f"Golden:    {golden_content}")
                        print(f"Candidate: {candidate_content}")
                        print("--- End hash diff ---\n")
                    else:
                        # Sinon affiche un diff textuel
                        self._print_diff(filename, golden_content, candidate_content)
        
        # Compare directory outputs avec diff détaillé
        golden_dirs = golden.get('dir_outputs', {})
        candidate_dirs = candidate.get('dir_outputs', {})
        
        if golden_dirs or candidate_dirs:
            success &= self._compare_directory_outputs(test_name, golden_dirs, candidate_dirs)
        
        if success:
            logger.info(f"PASS {test_name}")
        
        return success
    
    def _compare_directory_outputs(self, test_name: str, golden_dirs: Dict, candidate_dirs: Dict) -> bool:
        """Compare les répertoires avec diff détaillé"""
        success = True
        all_dirs = set(golden_dirs.keys()) | set(candidate_dirs.keys())
        
        for dirname in all_dirs:
            golden_manifest = golden_dirs.get(dirname, {})
            candidate_manifest = candidate_dirs.get(dirname, {})
            
            if not golden_manifest and not candidate_manifest:
                continue
                
            if not golden_manifest:
                logger.error(f"FAIL {test_name} - Directory {dirname} only in candidate")
                success = False
                continue
                
            if not candidate_manifest:
                logger.error(f"FAIL {test_name} - Directory {dirname} only in golden")
                success = False
                continue
            
            # Compare les fichiers dans les répertoires
            all_files = set(golden_manifest.keys()) | set(candidate_manifest.keys())
            dir_success = True
            
            for filepath in all_files:
                golden_content = golden_manifest.get(filepath, '')
                candidate_content = candidate_manifest.get(filepath, '')
                
                if golden_content != candidate_content:
                    dir_success = False
                    logger.error(f"FAIL {test_name} - Directory {dirname}/{filepath} differs")
                    
                    # Si c'est un hash binaire, on affiche juste les hashs
                    if golden_content.startswith('BINARY_MD5:') or candidate_content.startswith('BINARY_MD5:'):
                        print(f"\n--- Binary file diff for {dirname}/{filepath} ---")
                        print(f"Golden:    {golden_content}")
                        print(f"Candidate: {candidate_content}")
                        print("--- End binary diff ---\n")
                    else:
                        # Sinon on affiche un diff textuel
                        self._print_diff(f"{dirname}/{filepath}", golden_content, candidate_content)
            
            if dir_success:
                logger.info(f"PASS {test_name} - Directory {dirname} matches")
            else:
                success = False
        
        return success
    
    def _print_diff(self, name: str, expected: str, actual: str):
        """Affiche les différences"""
        print(f"\n--- Diff for {name} ---")
        expected_lines = expected.splitlines(keepends=True)
        actual_lines = actual.splitlines(keepends=True)
        
        diff = difflib.unified_diff(
            expected_lines,
            actual_lines,
            fromfile=f"golden/{name}",
            tofile=f"candidate/{name}",
            lineterm=""
        )
        
        for line in diff:
            print(line.rstrip())
        print("--- End diff ---\n")
    
    def run_single_test(self, test_case: Dict, update_golden: bool = False) -> bool:
        """Exécute un test unique"""
        test_name = test_case['name']
        logger.info(f"Running test: {test_name}")
        
        with tempfile.TemporaryDirectory() as temp_dir:
            work_dir = Path(temp_dir) / test_case.get('working_dir', 'test')
            work_dir.mkdir(parents=True, exist_ok=True)
            
            # Setup de l'environnement de test
            self._setup_test_environment(test_case, work_dir)
            
            # Capture l'état initial
            initial_files = self._capture_initial_state(work_dir)
            
            # Exécute la version de référence
            reference_result = self._run_executable('reference', test_case, work_dir)
            
            if not reference_result['success']:
                logger.error(f"Reference executable failed for {test_name}: {reference_result['error']}")
                return False
            
            # Collecte TOUS les fichiers générés
            reference_result['output_files'] = self._collect_all_generated_outputs(work_dir, initial_files)
            reference_result['dir_outputs'] = self._collect_directory_manifests(work_dir, [])
            
            if update_golden:
                # Mode mise à jour des golden masters
                self._save_golden_master(test_name, reference_result)
                logger.info(f"Golden master updated for: {test_name}")
                return True
            
            # Charge le golden master
            golden = self._load_golden_master(test_name)
            if not golden:
                logger.error(f"No golden master found for {test_name}. Run with --update-golden first.")
                return False
            
            # Nettoie TOUS les fichiers générés pour le candidat
            current_files = self._capture_initial_state(work_dir)
            for file_rel_path in (current_files - initial_files):
                file_path = work_dir / file_rel_path
                if file_path.is_file():
                    file_path.unlink(missing_ok=True)
                elif file_path.is_dir():
                    shutil.rmtree(file_path, ignore_errors=True)
            
            # Nettoie aussi les répertoires générés
            for root, dirs, files in os.walk(work_dir, topdown=False):
                for dirname in dirs:
                    dir_path = Path(root) / dirname
                    if dir_path.relative_to(work_dir) not in [Path(f).parent for f in initial_files if '/' in f]:
                        try:
                            if not any(dir_path.iterdir()):  # Si le répertoire est vide
                                dir_path.rmdir()
                        except:
                            pass
            
            # Exécute la version candidate
            candidate_result = self._run_executable('candidate', test_case, work_dir)
            
            if not candidate_result['success']:
                logger.error(f"Candidate executable failed for {test_name}: {candidate_result['error']}")
                return False
            
            # Collecte TOUS les fichiers générés par le candidat
            candidate_result['output_files'] = self._collect_all_generated_outputs(work_dir, initial_files)
            candidate_result['dir_outputs'] = self._collect_directory_manifests(work_dir, [])
            
            # Compare les résultats
            return self._compare_results(test_name, golden, candidate_result)


# Tests pytest
class TestGenericGoldenMaster:
    
    @pytest.fixture(scope="class")
    def golden_master(self):
        return GenericGoldenMaster()
    
    def test_golden_master_comparison(self, golden_master, test_case, update_golden):
        """Test de comparaison Golden Master générique"""
        result = golden_master.run_single_test(test_case, update_golden=update_golden)
        assert result, f"Golden Master test failed for: {test_case['name']}"


def main():
    """Point d'entrée principal"""
    import argparse
    
    parser = argparse.ArgumentParser(description="Generic Golden Master Tests")
    parser.add_argument("--update-golden", action="store_true",
                       help="Update golden masters with reference executable")
    parser.add_argument("--test", help="Run specific test")
    parser.add_argument("--config", default="test_config.json",
                       help="Path to config file")
    parser.add_argument("--verbose", "-v", action="store_true",
                       help="Verbose mode")
    
    args = parser.parse_args()
    
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    golden_master = GenericGoldenMaster(args.config)
    
    if args.test:
        # Run specific test
        test_case = next((tc for tc in golden_master.config['test_cases'] 
                         if tc['name'] == args.test), None)
        if not test_case:
            logger.error(f"Test not found: {args.test}")
            return 1
        
        success = golden_master.run_single_test(test_case, args.update_golden)
        return 0 if success else 1
    
    else:
        # Run all tests
        total_tests = len(golden_master.config['test_cases'])
        passed_tests = 0
        
        for test_case in golden_master.config['test_cases']:
            if golden_master.run_single_test(test_case, args.update_golden):
                passed_tests += 1
        
        logger.info(f"Results: {passed_tests}/{total_tests} tests passed")
        return 0 if passed_tests == total_tests else 1


if __name__ == "__main__":
    exit(main())

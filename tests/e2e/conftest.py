"""
Configuration globale pour les tests E2E GeneWeb avec Selenium
"""
import pytest
import subprocess
import time
import os
import signal
from selenium import webdriver
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from pathlib import Path
import datetime


class GeneWebServer:
    """Gestionnaire pour le serveur GeneWeb"""

    def __init__(self, port=2317, base_name="galichet"):
        self.port = port
        self.base_name = base_name
        self.process = None
        self.base_url = f"http://localhost:{port}"

    def start(self):
        """Démarre le serveur GeneWeb"""
        # Chemin vers l'exécutable gwd (depuis le répertoire tests/e2e)
        gwd_path = "../../distribution/gw/gwd"

        if not os.path.exists(gwd_path):
            raise FileNotFoundError(f"GeneWeb server not found at {gwd_path}")

        # Commande pour démarrer le serveur
        cmd = [
            gwd_path,
            "-p", str(self.port),
            "-bd", "../../distribution/bases",
            "-hd", "../../distribution/gw"
        ]

        print(f"Starting GeneWeb server: {' '.join(cmd)}")
        self.process = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            preexec_fn=os.setsid if os.name != 'nt' else None
        )

        # Attendre que le serveur soit prêt
        self._wait_for_server()

    def stop(self):
        """Arrête le serveur GeneWeb"""
        if self.process:
            try:
                if os.name != 'nt':
                    os.killpg(os.getpgid(self.process.pid), signal.SIGTERM)
                else:
                    self.process.terminate()
                self.process.wait(timeout=10)
            except (subprocess.TimeoutExpired, ProcessLookupError):
                if os.name != 'nt':
                    os.killpg(os.getpgid(self.process.pid), signal.SIGKILL)
                else:
                    self.process.kill()
            finally:
                self.process = None

    def _wait_for_server(self, timeout=30):
        """Attend que le serveur soit prêt"""
        import requests

        start_time = time.time()
        while time.time() - start_time < timeout:
            try:
                response = requests.get(f"{self.base_url}/", timeout=5)
                if response.status_code == 200:
                    print("GeneWeb server is ready")
                    return
            except requests.exceptions.RequestException:
                pass
            time.sleep(1)

        raise TimeoutError("GeneWeb server failed to start within timeout")


@pytest.fixture(scope="session")
def geneweb_server():
    """Fixture pour le serveur GeneWeb"""
    server = GeneWebServer()
    server.start()
    yield server
    server.stop()


@pytest.fixture(scope="function")
def driver():
    """Fixture pour le driver Selenium"""
    chrome_options = Options()
    chrome_options.add_argument("--headless")  # Mode headless pour les tests
    chrome_options.add_argument("--no-sandbox")
    chrome_options.add_argument("--disable-dev-shm-usage")
    chrome_options.add_argument("--window-size=1280,720")
    chrome_options.add_argument("--disable-gpu")
    chrome_options.add_argument("--disable-extensions")
    chrome_options.add_argument("--remote-debugging-port=9222")

    # Essayer d'utiliser le driver par défaut (Selenium 4.15+ gère automatiquement)
    try:
        driver = webdriver.Chrome(options=chrome_options)
    except Exception as e:
        print(f"Erreur Chrome: {e}")
        # Fallback vers Firefox
        try:
            from selenium.webdriver.firefox.options import Options as FirefoxOptions
            firefox_options = FirefoxOptions()
            firefox_options.add_argument("--headless")
            driver = webdriver.Firefox(options=firefox_options)
            print("Utilisation de Firefox comme fallback")
        except Exception as e2:
            print(f"Erreur Firefox: {e2}")
            raise Exception("Aucun navigateur disponible")

    yield driver
    driver.quit()


@pytest.fixture(scope="function")
def page(driver, geneweb_server):
    """Fixture pour la page de test"""
    driver.get(geneweb_server.base_url)
    yield driver
    # Nettoyage automatique géré par la fixture driver


@pytest.fixture(scope="function")
def authenticated_page(driver, geneweb_server):
    """Fixture pour une page avec authentification wizard"""
    driver.get(f"{geneweb_server.base_url}/?m=WIZARD")

    # Remplir le formulaire de connexion (si nécessaire)
    # Note: Adaptez selon votre configuration d'authentification

    yield driver
    # Nettoyage automatique géré par la fixture driver


@pytest.fixture(scope="function")
def home_page(driver, geneweb_server):
    """
    POM fixture returning a HomePage instance built on the existing driver & server.
    Usage in tests: def test_x(home_page): home_page.go_list() ...
    """
    # local import to avoid import-time side effects
    from pages.home_page import HomePage

    hp = HomePage(driver, geneweb_server.base_url)
    hp.open()
    return hp


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_makereport(item, call):
    # store the report on the item for fixtures to inspect later
    outcome = yield
    rep = outcome.get_result()
    setattr(item, "rep_" + rep.when, rep)

@pytest.fixture(autouse=True)
def capture_artifacts_on_failure(request):
    """
    Après chaque test, si échec, essaye de récupérer:
      - screenshot via la fixture 'page' si disponible
      - page source HTML
    Les fichiers sont écrits sous tests/e2e/reports/artifacts
    """
    yield
    for when in ("setup", "call", "teardown"):
        rep = getattr(request.node, "rep_" + when, None)
        if rep and rep.failed:
            outdir = Path(request.config.rootpath) / "tests" / "e2e" / "reports" / "artifacts"
            outdir.mkdir(parents=True, exist_ok=True)
            ts = datetime.datetime.utcnow().strftime("%Y%m%dT%H%M%SZ")
            name = f"{request.node.name}.{when}.{ts}"
            # try to save screenshot and html if a 'page' fixture exists
            try:
                page = request.getfixturevalue("page")
            except Exception:
                page = None
            if page:
                try:
                    screenshot_path = outdir / f"{name}.png"
                    # save_screenshot or get_screenshot_as_file depending on driver wrapper
                    if hasattr(page, "save_screenshot"):
                        page.save_screenshot(str(screenshot_path))
                    elif hasattr(page, "get_screenshot_as_file"):
                        page.get_screenshot_as_file(str(screenshot_path))
                except Exception:
                    pass
                try:
                    html_path = outdir / f"{name}.html"
                    with open(html_path, "w", encoding="utf-8") as f:
                        f.write(page.page_source if hasattr(page, "page_source") else page.get_page_source())
                except Exception:
                    pass
            # also dump captured stdout/stderr from report if present
            try:
                logs_path = outdir / f"{name}.log"
                content = rep.longreprtext if hasattr(rep, "longreprtext") else str(rep)
                with open(logs_path, "w", encoding="utf-8") as f:
                    f.write(content)
            except Exception:
                pass

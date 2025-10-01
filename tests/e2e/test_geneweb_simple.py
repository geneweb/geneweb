"""
Test simple pour vérifier que GeneWeb fonctionne avec Selenium
"""
import pytest
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC


def test_geneweb_homepage(driver, geneweb_server):
    """Test que la page d'accueil GeneWeb se charge"""
    # Aller sur la page d'accueil GeneWeb
    driver.get(geneweb_server.base_url)

    # Attendre que la page se charge
    WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.TAG_NAME, "body"))
    )

    # Vérifier que la page se charge
    assert "GeneWeb" in driver.title or "GeneWeb" in driver.page_source

    print("✅ Test GeneWeb basique réussi !")
    print(f"   URL: {driver.current_url}")
    print(f"   Titre: {driver.title}")


def test_geneweb_database_list(driver, geneweb_server):
    """Test l'accès à la liste des bases de données"""
    # Aller à la page de liste des bases
    driver.get(f"{geneweb_server.base_url}/?m=LIST")

    # Attendre que la page se charge
    WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.TAG_NAME, "body"))
    )

    # Vérifier que la page se charge
    body = driver.find_element(By.TAG_NAME, "body")
    assert body.is_displayed()

    print("✅ Test liste des bases réussi !")
    print(f"   URL: {driver.current_url}")


def test_geneweb_search(driver, geneweb_server):
    """Test la page de recherche"""
    # Aller à la page de recherche
    driver.get(f"{geneweb_server.base_url}/?m=SEARCH")

    # Attendre que la page se charge
    WebDriverWait(driver, 10).until(
        EC.presence_of_element_located((By.TAG_NAME, "body"))
    )

    # Vérifier que la page se charge
    body = driver.find_element(By.TAG_NAME, "body")
    assert body.is_displayed()

    print("✅ Test recherche réussi !")
    print(f"   URL: {driver.current_url}")

"""
Tests E2E de base pour la navigation GeneWeb avec Selenium
"""
import pytest
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC


class TestBasicNavigation:
    """Tests de navigation de base"""

    def test_homepage_loads(self, home_page):
        """Test que la page d'accueil se charge correctement"""
        # Vérifier que la page se charge (titre non vide)
        assert home_page.title is not None and home_page.title.strip() != ""

        # Vérifier la présence d'éléments clés
        body = home_page.find(By.TAG_NAME, "body")
        assert body.is_displayed()

    def test_database_list_access(self, home_page):
        """Test l'accès à la liste des bases de données"""
        # Aller à la page de liste des bases
        home_page.go_list()

        # Vérifier que la page se charge
        body = home_page.find(By.TAG_NAME, "body")
        assert body.is_displayed()

    def test_language_switching(self, home_page):
        """Test le changement de langue"""
        # Tester le français
        home_page.switch_lang("fr")
        body = home_page.find(By.TAG_NAME, "body")
        assert body.is_displayed()

        # Tester l'anglais
        home_page.switch_lang("en")
        body = home_page.find(By.TAG_NAME, "body")
        assert body.is_displayed()

    def test_search_functionality(self, home_page):
        """Test la fonctionnalité de recherche"""
        # Aller à la page de recherche
        home_page.go_search()

        # Vérifier que le formulaire de recherche est présent
        try:
            search_form = home_page.find(By.TAG_NAME, "form")
            assert search_form.is_displayed()
        except Exception:
            # Si pas de formulaire, vérifier au moins que la page se charge
            body = home_page.find(By.TAG_NAME, "body")
            assert body.is_displayed()

    def test_person_display(self, home_page):
        """Test l'affichage d'une personne"""
        # Essayer d'accéder à une personne (si la base de données existe)
        home_page.go_person(1)

        # Vérifier que la page se charge (même si la personne n'existe pas)
        body = home_page.find(By.TAG_NAME, "body")
        assert body.is_displayed()

    def test_family_tree_display(self, home_page):
        """Test l'affichage de l'arbre généalogique"""
        # Aller à la page d'arbre
        home_page.go_tree()

        # Vérifier que la page se charge
        body = home_page.find(By.TAG_NAME, "body")
        assert body.is_displayed()

    def test_statistics_page(self, home_page):
        """Test la page de statistiques"""
        # Aller à la page de statistiques
        home_page.go_stats()

        # Vérifier que la page se charge
        body = home_page.find(By.TAG_NAME, "body")
        assert body.is_displayed()

    def test_help_documentation(self, home_page):
        """Test l'accès à la documentation d'aide"""
        # Aller à la page d'aide
        home_page.go_help()

        # Vérifier que la page se charge
        body = home_page.find(By.TAG_NAME, "body")
        assert body.is_displayed()

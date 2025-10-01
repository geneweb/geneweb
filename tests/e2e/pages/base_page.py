from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC

class BasePage:
    def __init__(self, driver, base_url):
        self.driver = driver
        # ensure no trailing slash ambiguity
        self.base_url = base_url.rstrip('/')

    def goto(self, path=""):
        """
        path examples:
         - "" -> base "/"
         - "?m=LIST" -> base + "?m=LIST"
         - "/some/path" -> base + "/some/path"
        """
        if not path:
            url = f"{self.base_url}/"
        elif path.startswith('?') or path.startswith('/'):
            url = f"{self.base_url}{path}"
        else:
            url = f"{self.base_url}/{path}"
        self.driver.get(url)
        return self

    def wait_for_body(self, timeout=5):
        WebDriverWait(self.driver, timeout).until(
            EC.presence_of_element_located((By.TAG_NAME, "body"))
        )

    def find(self, by, selector):
        return self.driver.find_element(by, selector)

    def find_all(self, by, selector):
        return self.driver.find_elements(by, selector)

    @property
    def title(self):
        return self.driver.title

    def save_screenshot(self, path):
        try:
            if hasattr(self.driver, "save_screenshot"):
                self.driver.save_screenshot(path)
        except Exception:
            pass

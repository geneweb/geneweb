from .base_page import BasePage
from selenium.webdriver.common.by import By


class HomePage(BasePage):
    def open(self):
        self.goto("")
        self.wait_for_body()
        return self

    def go_list(self):
        self.goto("?m=LIST")
        self.wait_for_body()
        return self

    def go_search(self):
        self.goto("?m=SEARCH")
        self.wait_for_body()
        return self

    def go_person(self, person_id=1):
        self.goto(f"?m=P&i={person_id}")
        self.wait_for_body()
        return self

    def go_tree(self):
        self.goto("?m=TREE")
        self.wait_for_body()
        return self

    def go_stats(self):
        self.goto("?m=STATS")
        self.wait_for_body()
        return self

    def go_help(self):
        self.goto("?m=HELP")
        self.wait_for_body()
        return self

    def switch_lang(self, lang_code):
        self.goto(f"?lang={lang_code}")
        self.wait_for_body()
        return self

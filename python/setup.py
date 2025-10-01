"""
Setup script pour le package consang Python.

Ce fichier configure comment notre code Python devient un package installable.
Il définit les métadonnées, dépendances, et commandes exécutables.
"""

from setuptools import setup, find_packages
import os

# Lire le README pour la description longue
readme_path = "README.md"
if os.path.exists(readme_path):
    with open(readme_path, "r", encoding="utf-8") as fh:
        long_description = fh.read()
else:
    long_description = "Python implementation of genealogical consanguinity calculator"

setup(
    # === IDENTIFICATION DU PACKAGE ===
    name="geneweb-consang",                    # Nom unique sur PyPI
    version="1.0.0",                           # Version du package
    author="Geneweb Team",                     # Auteur(s)
    description="Python implementation of genealogical consanguinity calculator",
    long_description=long_description,         # Description détaillée (README)
    long_description_content_type="text/markdown",

    # === DÉCOUVERTE AUTOMATIQUE DES PACKAGES ===
    packages=find_packages(),                  # Trouve automatiquement consang/ et geneweb_common/

    # === MÉTADONNÉES POUR PyPI ===
    classifiers=[
        "Development Status :: 4 - Beta",      # Statut de développement
        "Intended Audience :: Science/Research",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "Programming Language :: Python :: 3.10",
        "Programming Language :: Python :: 3.11",
        "Topic :: Scientific/Engineering :: Information Analysis",
    ],

    # === COMPATIBILITÉ PYTHON ===
    python_requires=">=3.8",                  # Version minimale de Python

    # === CRÉATION DE COMMANDES EXÉCUTABLES ===
    entry_points={
        "console_scripts": [
            "consang=consang.consang:main",
        ],
    },

    # === DÉPENDANCES ===
    install_requires=[
        # Pas de dépendances externes pour l'instant
    ],

    # === DÉPENDANCES DE DÉVELOPPEMENT ===
    extras_require={
        "dev": [
            "pytest>=7.0",                    # Tests unitaires
            "pytest-cov>=4.0",               # Couverture de code
            "black>=22.0",                    # Formatage automatique
            "flake8>=5.0",                    # Vérification style
            "mypy>=1.0",                      # Vérification types
        ],
    },
)

# UTILISATION :
#
# 1. Installation en mode développement :
#    pip install -e .
#    (Les modifications du code sont directement prises en compte)
#
# 2. Installation normale :
#    pip install .
#
# 3. Installation avec outils de développement :
#    pip install -e ".[dev]"
#
# 4. Création d'un package distribuable :
#    python setup.py sdist bdist_wheel
#
# 5. Test que la commande fonctionne :
#    consang --help

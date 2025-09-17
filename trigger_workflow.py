#!/usr/bin/env python3

import os
import subprocess
import datetime

def trigger_workflow():
    """Force le déclenchement du workflow en modifiant un fichier surveillé"""

    print("🔍 Diagnostic du workflow...")

    # Vérifier la branche actuelle
    result = subprocess.run(['git', 'branch', '--show-current'], capture_output=True, text=True)
    current_branch = result.stdout.strip()
    print(f"Branche actuelle: {current_branch}")

    # Vérifier les fichiers dans les paths surveillés
    paths_to_check = [
        'python/',
        'test/scena/',
        'test/fixtures/',
        '.github/workflows/golden-master-pipeline.yml'
    ]

    print(f"\n📁 Vérification des paths surveillés:")
    for path in paths_to_check:
        if os.path.exists(path):
            print(f"  ✅ {path}")
        else:
            print(f"  ❌ {path} (manquant)")

    # Créer un fichier trigger
    timestamp = datetime.datetime.now().isoformat()
    trigger_content = f"""# Workflow trigger file
# Created: {timestamp}
# This file is used to trigger the GitHub Actions workflow

print("Workflow triggered at {timestamp}")
"""

    trigger_file = "python/workflow_trigger.py"
    with open(trigger_file, 'w') as f:
        f.write(trigger_content)

    print(f"\n🚀 Fichier trigger créé: {trigger_file}")

    # Commit et push
    try:
        subprocess.run(['git', 'add', trigger_file], check=True)
        subprocess.run(['git', 'commit', '-m', f'trigger: force workflow execution at {timestamp}'], check=True)

        print("📤 Pushing to trigger workflow...")
        subprocess.run(['git', 'push'], check=True)

        print("✅ Workflow trigger envoyé!")
        print("🔗 Vérifiez sur GitHub: https://github.com/YOUR_USERNAME/YOUR_REPO/actions")

    except subprocess.CalledProcessError as e:
        print(f"❌ Erreur lors du commit/push: {e}")

    return trigger_file

if __name__ == "__main__":
    trigger_file = trigger_workflow()

    print(f"""
📋 Actions à vérifier:

1. Workflow GitHub Actions:
   - Allez sur GitHub → Actions
   - Cherchez "Golden Master Tests & Deployment" ou "Debug Workflow"
   - Vérifiez s'il se lance après le push

2. Si ça ne marche toujours pas:
   - Vérifiez que vous êtes sur la branche 'ged2gwb'
   - Essayez le lancement manuel depuis l'interface GitHub

3. Fichier créé: {trigger_file}
   - Supprimez-le après le test si vous voulez
""")

#!/bin/bash

set -e

APP_NAME="geneweb"
VERSION=$(awk -F\" '/^let ver =/ {print $2}' lib/version.txt 2>/dev/null || echo "7.0")
DMG_NAME="${APP_NAME}-${VERSION}.dmg"

echo "╔═══════════════════════════════════════════════════════╗"
echo "║          Création du DMG GeneWeb pour macOS           ║"
echo "╚═══════════════════════════════════════════════════════╝"
echo ""

if [ ! -d "${APP_NAME}.app" ]; then
    echo "❌ ${APP_NAME}.app non trouvé"
    echo "Lancez d'abord: ./create_bundle.sh"
    exit 1
fi

cleanup() {
    hdiutil detach /Volumes/"${APP_NAME}"* 2>/dev/null || true
    sleep 1
    rm -f temp*.dmg 2>/dev/null || true
}

trap cleanup EXIT
cleanup

rm -f "$DMG_NAME" "${DMG_NAME}.sha256" 2>/dev/null || true

echo "📦 Création du DMG : ${DMG_NAME}"
echo ""

STAGING_DIR=$(mktemp -d)
cp -R "${APP_NAME}.app" "$STAGING_DIR/"
ln -s /Applications "$STAGING_DIR/Applications"
ln -s "${APP_NAME}.app/Contents/Resources/geneweb_stop.command" "$STAGING_DIR/GeneWeb_stop.command"

# Ajouter après la création du fichier "Arrêter GeneWeb"

cat > "$STAGING_DIR/GeneWeb_install.command" << 'EOFINSTALL'
#!/bin/bash

clear
echo "╔═══════════════════════════════════════════════════════╗"
echo "║         Installation de GeneWeb pour macOS            ║"
echo "╚═══════════════════════════════════════════════════════╝"
echo ""

# Obtenir le chemin du DMG
DMG_PATH="$(cd "$(dirname "$0")" && pwd)"

echo "1️⃣  Vérification..."

if [ ! -d "$DMG_PATH/GeneWeb.app" ]; then
    echo "❌ GeneWeb.app non trouvé dans le DMG"
    exit 1
fi

echo "2️⃣  Installation dans /Applications..."

# Supprimer l'ancienne version si elle existe
if [ -d "/Applications/GeneWeb.app" ]; then
    echo "   → Suppression de l'ancienne version..."
    rm -rf /Applications/GeneWeb.app
fi

echo "3️⃣  Arrêt de GeneWeb si déjà lancé..."

# Vérifier si GeneWeb tourne
if pgrep -q "gwd\|gwsetup"; then
    echo "   → GeneWeb est en cours d'exécution, arrêt..."
    
    # Arrêt doux
    pkill gwd
    pkill gwsetup
    sleep 2
    
    # Arrêt forcé si nécessaire
    pkill -9 gwd 2>/dev/null || true
    pkill -9 gwsetup 2>/dev/null || true
    sleep 1
    
    # Vérifier que tout est arrêté
    if pgrep -q "gwd\|gwsetup"; then
        echo ""
        echo "❌ Impossible d'arrêter GeneWeb complètement"
        echo ""
        echo "Processus encore actifs:"
        pgrep -fl "gwd|gwsetup"
        echo ""
        echo "Veuillez:"
        echo "  1. Fermer la fenêtre Terminal de GeneWeb"
        echo "  2. Ou double-cliquer sur 'Arrêter GeneWeb.command'"
        echo "  3. Puis relancer cette installation"
        echo ""
        read -p "Appuyez sur Entrée pour quitter..."
        exit 1
    fi
    
    echo "   ✅ GeneWeb arrêté"
else
    echo "   ✅ GeneWeb n'est pas en cours d'exécution"
fi

# Copier la nouvelle version
echo "   → Copie de GeneWeb.app..."
cp -R "$DMG_PATH/GeneWeb.app" /Applications/

echo "4️⃣️  Suppression de la quarantaine..."
xattr -cr /Applications/GeneWeb.app 2>/dev/null || {
    echo ""
    echo "⚠️  Impossible de supprimer la quarantaine automatiquement"
    echo ""
    echo "Veuillez taper cette commande dans le Terminal:"
    echo "  xattr -cr /Applications/GeneWeb.app"
    echo ""
    read -p "Appuyez sur Entrée pour continuer..."
}

echo ""
echo "╔═══════════════════════════════════════════════════════╗"
echo "║  ✅ Installation terminée !                           ║"
echo "║                                                       ║"
echo "║  Vous pouvez maintenant :                             ║"
echo "║    1. Éjecter ce DMG                                  ║"
echo "║    2. Lancer GeneWeb depuis Applications              ║"
echo "╚═══════════════════════════════════════════════════════╝"
echo ""

sleep 3
EOFINSTALL

chmod +x "$STAGING_DIR/GeneWeb_install.command"

cat > "$STAGING_DIR/Lisez-moi.txt" << 'EOF'
╔═══════════════════════════════════════════════════════╗
║              GeneWeb pour macOS                       ║
╚═══════════════════════════════════════════════════════╝

INSTALLATION
════════════
1. Glissez GeneWeb.app vers Applications
2. Lancez GeneWeb depuis Applications
3. Le fichier ~/.geneweb/config définit plusieurs variables globales

PREMIER LANCEMENT
═════════════════
macOS affichera un avertissement :
→ Préférences Système → Confidentialité et Sécurité
→ Cliquez sur "Ouvrir quand même"

ARRÊT
═════
Pour arrêter proprement :
→ Ouvrez GeneWeb.app → Afficher le contenu du paquet
→ Double-cliquez sur geneweb_stop.command

SUPPORT
═══════
https://github.com/geneweb/geneweb
EOF

cat > "$STAGING_DIR/Read-me.txt" << 'EOF'
╔═══════════════════════════════════════════════════════╗
║              GeneWeb for macOS                        ║
╚═══════════════════════════════════════════════════════╝

INSTALLATION
════════════
1. Drag GeneWeb.app to Applications
2. Launch GeneWeb from Applications
3. The file ~/.geneweb/config defines several global variables 

FIRST LAUNCH
════════════
macOS will show a warning:
→ System Preferences → Privacy & Security
→ Click "Open Anyway"

STOP
════
To stop properly:
→ Open GeneWeb.app → Show Package Contents
→ Double-click geneweb_stop.command

SUPPORT
═══════
https://github.com/geneweb/geneweb
EOF

echo "Création du DMG..."
hdiutil create -volname "GeneWeb ${VERSION}" \
               -srcfolder "$STAGING_DIR" \
               -ov -format UDZO \
               -imagekey zlib-level=9 \
               "$DMG_NAME"

rm -rf "$STAGING_DIR"

CHECKSUM=$(shasum -a 256 "$DMG_NAME" | awk '{print $1}')
echo "$CHECKSUM  $DMG_NAME" > "${DMG_NAME}.sha256"

DMG_SIZE=$(ls -lh "$DMG_NAME" | awk '{print $5}')

echo ""
echo "╔═══════════════════════════════════════════════════════╗"
echo "║  ✅ DMG créé avec succès !                            ║"
echo "╠═══════════════════════════════════════════════════════╣"
echo "║  Fichier : ${DMG_NAME}"
echo "║  Taille  : ${DMG_SIZE}"
echo "║  SHA256  : ${CHECKSUM:0:16}..."
echo "╠═══════════════════════════════════════════════════════╣"
echo "║  Test: open ${DMG_NAME}"
echo "╚═══════════════════════════════════════════════════════╝"
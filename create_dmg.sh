#!/bin/bash

set -e

APP_NAME="GeneWeb"
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
ln -s "${APP_NAME}.app/Contents/Resources/stop_geneweb.command" "$STAGING_DIR/stop_geneweb.command"

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
→ Double-cliquez sur stop_geneweb.command

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
→ Double-click stop_geneweb.command

SUPPORT
═══════
https://github.com/geneweb/geneweb
EOF

echo "Création du DMG..."
hdiutil create -volname "${APP_NAME} ${VERSION}" \
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
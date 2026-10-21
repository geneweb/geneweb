#!/bin/bash

# Nécessite une image PNG de 1024x1024
# Vous pouvez créer une icône simple ou utiliser le logo GeneWeb

INPUT_IMAGE="hd/images/geneweb_icon.png"
ICONSET_DIR="geneweb.iconset"

if [ ! -f "$INPUT_IMAGE" ]; then
    echo "❌ Image source non trouvée : $INPUT_IMAGE"
    echo "Créez d'abord une image PNG de 1024x1024 pixels"
    exit 1
fi

# Créer le dossier iconset
mkdir -p "$ICONSET_DIR"

# Générer toutes les tailles requises
sips -z 16 16     "$INPUT_IMAGE" --out "${ICONSET_DIR}/icon_16x16.png"
sips -z 32 32     "$INPUT_IMAGE" --out "${ICONSET_DIR}/icon_16x16@2x.png"
sips -z 32 32     "$INPUT_IMAGE" --out "${ICONSET_DIR}/icon_32x32.png"
sips -z 64 64     "$INPUT_IMAGE" --out "${ICONSET_DIR}/icon_32x32@2x.png"
sips -z 128 128   "$INPUT_IMAGE" --out "${ICONSET_DIR}/icon_128x128.png"
sips -z 256 256   "$INPUT_IMAGE" --out "${ICONSET_DIR}/icon_128x128@2x.png"
sips -z 256 256   "$INPUT_IMAGE" --out "${ICONSET_DIR}/icon_256x256.png"
sips -z 512 512   "$INPUT_IMAGE" --out "${ICONSET_DIR}/icon_256x256@2x.png"
sips -z 512 512   "$INPUT_IMAGE" --out "${ICONSET_DIR}/icon_512x512.png"
sips -z 1024 1024 "$INPUT_IMAGE" --out "${ICONSET_DIR}/icon_512x512@2x.png"

# Convertir en .icns
iconutil -c icns "$ICONSET_DIR"

# Nettoyer
rm -rf "$ICONSET_DIR"

echo "✅ Icône créée et ajoutée au bundle"
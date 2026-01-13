#!/bin/bash

set -e

echo "╔═══════════════════════════════════════════════════════╗"
echo "║    Test du workflow de release (simulation locale)   ║"
echo "╚═══════════════════════════════════════════════════════╝"
echo ""

# Couleurs
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

# Compteurs
TESTS_PASSED=0
TESTS_FAILED=0

test_step() {
    local step_name="$1"
    local command="$2"
    
    echo -e "${YELLOW}▶ Testing: $step_name${NC}"
    
    if eval "$command"; then
        echo -e "${GREEN}  ✅ PASSED${NC}"
        ((TESTS_PASSED++))
        return 0
    else
        echo -e "${RED}  ❌ FAILED${NC}"
        ((TESTS_FAILED++))
        return 1
    fi
    echo ""
}

# Nettoyer l'environnement
echo "🧹 Nettoyage..."
pkill -9 gwd gwsetup 2>/dev/null || true
rm -rf GeneWeb.app GeneWeb-*.dmg* 2>/dev/null || true

echo ""
echo "═══════════════════════════════════════════════════════"
echo "1️⃣  Phase BUILD"
echo "═══════════════════════════════════════════════════════"
echo ""

# Test 1: Build distribution
test_step "make distrib" "make distrib > /tmp/make_distrib.log 2>&1"

# Test 2: Vérifier que distribution/ existe
test_step "Distribution directory exists" "[ -d 'distribution' ]"

# Test 3: Vérifier les exécutables
test_step "Executables present" "[ -f 'distribution/gw/gwd' ] && [ -f 'distribution/gw/gwsetup' ]"

echo ""
echo "═══════════════════════════════════════════════════════"
echo "2️⃣  Phase BUNDLE"
echo "═══════════════════════════════════════════════════════"
echo ""

# Test 4: Scripts exécutables
test_step "Scripts are executable" "[ -x 'create_bundle.sh' ] && [ -x 'create_dmg.sh' ]"

# Test 5: Créer le bundle
test_step "Create bundle" "./create_bundle.sh > /tmp/create_bundle.log 2>&1"

# Test 6: Vérifier la structure du bundle
test_step "Bundle structure" "
    [ -d 'GeneWeb.app/Contents/MacOS' ] && 
    [ -d 'GeneWeb.app/Contents/Resources' ] && 
    [ -d 'GeneWeb.app/Contents/Frameworks' ] &&
    [ -f 'GeneWeb.app/Contents/Info.plist' ]
"

# Test 7: Vérifier l'exécutable principal
test_step "Main executable" "[ -x 'GeneWeb.app/Contents/MacOS/GeneWeb' ]"

# Test 8: Vérifier les dylibs
test_step "Frameworks present" "[ -f 'GeneWeb.app/Contents/Frameworks/libgmp.10.dylib' ]"

# Test 9: Vérifier l'icône (si elle existe)
if [ -f "GeneWeb.icns" ]; then
    test_step "Icon present" "[ -f 'GeneWeb.app/Contents/Resources/GeneWeb.icns' ]"
fi

# Test 10: Tester le lancement du bundle
echo -e "${YELLOW}▶ Testing: Bundle launches${NC}"

# Nettoyer
pkill -9 gwd gwsetup 2>/dev/null || true
sleep 1

# Lancer
open GeneWeb.app 2>/dev/null
echo "  Waiting for startup (max 15s)..."

# Attendre avec timeout
for i in {1..15}; do
    if pgrep -q "gwd" && pgrep -q "gwsetup"; then
        echo -e "${GREEN}  ✅ PASSED - Started in ${i}s${NC}"
        ((TESTS_PASSED++))
        
        # Arrêter
        sleep 2
        pkill -9 gwd gwsetup
        sleep 1
        break
    fi
    sleep 1
    [ $i -eq 15 ] && echo -e "${RED}  ❌ FAILED - Timeout${NC}" && ((TESTS_FAILED++))
done

echo ""
echo "═══════════════════════════════════════════════════════"
echo "3️⃣  Phase DMG"
echo "═══════════════════════════════════════════════════════"
echo ""

# Test 11: Créer le DMG
test_step "Create DMG" "./create_dmg.sh > /tmp/create_dmg.log 2>&1"

# Test 12: Vérifier que le DMG existe
VERSION=$(awk -F\" '/^let ver =/ {print $2}' lib/version.txt)
DMG_NAME="GeneWeb-${VERSION}.dmg"
test_step "DMG file exists" "[ -f '$DMG_NAME' ]"

# Test 13: Vérifier le checksum
test_step "Checksum file exists" "[ -f '${DMG_NAME}.sha256' ]"

# Test 14: Vérifier le format du DMG
test_step "DMG is valid" "hdiutil verify '$DMG_NAME' > /dev/null 2>&1"

# Test 15: Monter le DMG et vérifier le contenu
echo -e "${YELLOW}▶ Testing: DMG contents${NC}"

# Fonction pour démonter proprement
cleanup_mounts() {
    for vol in /Volumes/GeneWeb*; do
        [ -d "$vol" ] && hdiutil detach "$vol" -force 2>/dev/null || true
    done
}

# Nettoyer d'abord
cleanup_mounts
sleep 1

# Monter le DMG
echo "  Mounting DMG..."
if ! hdiutil attach "$DMG_NAME" > /tmp/hdiutil_mount.log 2>&1; then
    echo -e "${RED}  ❌ FAILED - Could not mount DMG${NC}"
    echo "  Error:"
    cat /tmp/hdiutil_mount.log | sed 's/^/    /'
    ((TESTS_FAILED++))
else
    sleep 2
    
    # Trouver le point de montage
    MOUNT_POINT=$(mount | grep "$DMG_NAME" | awk '{print $3}')
    
    if [ -z "$MOUNT_POINT" ]; then
        # Fallback: chercher dans /Volumes
        MOUNT_POINT=$(find /Volumes -maxdepth 1 -name "GeneWeb*" -type d | head -1)
    fi
    
    if [ -z "$MOUNT_POINT" ] || [ ! -d "$MOUNT_POINT" ]; then
        echo -e "${RED}  ❌ FAILED - Mount point not found${NC}"
        echo "  Mounted volumes:"
        mount | grep -i geneweb | sed 's/^/    /'
        echo "  /Volumes contents:"
        ls -1 /Volumes | sed 's/^/    /'
        ((TESTS_FAILED++))
    else
        echo "  Mount point: $MOUNT_POINT"
        
        # Vérifier le contenu
        EXPECTED_FILES=(
            "GeneWeb.app"
            "GeneWeb_install.command"
            "GeneWeb_stop.command"
        )
        
        ALL_FOUND=true
        for file in "${EXPECTED_FILES[@]}"; do
            if [ ! -e "$MOUNT_POINT/$file" ]; then
                echo "  ❌ Missing: $file"
                ALL_FOUND=false
            fi
        done
        
        if $ALL_FOUND; then
            echo -e "${GREEN}  ✅ PASSED - DMG contents correct${NC}"
            echo "  Contents:"
            ls -1 "$MOUNT_POINT/" | sed 's/^/    /'
            ((TESTS_PASSED++))
        else
            echo -e "${RED}  ❌ FAILED - DMG contents incomplete${NC}"
            echo "  Found in DMG:"
            ls -la "$MOUNT_POINT/" 2>/dev/null | sed 's/^/    /'
            ((TESTS_FAILED++))
        fi
        
        # Démonter
        hdiutil detach "$MOUNT_POINT" -force 2>/dev/null || true
    fi
fi

# Cleanup final
cleanup_mounts
sleep 1
echo ""
# Test 16: Vérifier la taille du DMG
echo -e "${YELLOW}▶ Testing: DMG size reasonable${NC}"
DMG_SIZE=$(stat -f%z "$DMG_NAME")
DMG_SIZE_MB=$((DMG_SIZE / 1024 / 1024))

if [ "$DMG_SIZE_MB" -gt 50 ] && [ "$DMG_SIZE_MB" -lt 500 ]; then
    echo -e "${GREEN}  ✅ PASSED - Size: ${DMG_SIZE_MB}MB${NC}"
    ((TESTS_PASSED++))
else
    echo -e "${YELLOW}  ⚠️  WARNING - Size: ${DMG_SIZE_MB}MB (expected 50-500MB)${NC}"
    ((TESTS_FAILED++))
fi
echo ""

echo ""
echo "═══════════════════════════════════════════════════════"
echo "4️⃣  Phase VARIABLES (simulating GitHub Actions env)"
echo "═══════════════════════════════════════════════════════"
echo ""

# Simuler les variables GitHub Actions
echo "Simulating GitHub Actions environment variables:"
echo "  VERSION=$VERSION"
echo "  DMG_NAME=$DMG_NAME"
echo "  DMG_SHA256=${DMG_NAME}.sha256"
echo "  VERSIONED_ARCHIVE=geneweb-${VERSION}-macos-universal.zip"
echo ""

test_step "VERSION extracted correctly" "[ -n '$VERSION' ]"
test_step "DMG naming follows convention" "[[ '$DMG_NAME' =~ ^GeneWeb-[0-9] ]]"

echo ""
echo "═══════════════════════════════════════════════════════"
echo "📊 RÉSULTATS"
echo "═══════════════════════════════════════════════════════"
echo ""
echo -e "${GREEN}✅ Tests réussis: $TESTS_PASSED${NC}"
echo -e "${RED}❌ Tests échoués: $TESTS_FAILED${NC}"
echo ""

if [ $TESTS_FAILED -eq 0 ]; then
    echo -e "${GREEN}╔═══════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║  🎉 TOUS LES TESTS SONT PASSÉS !                     ║${NC}"
    echo -e "${GREEN}║                                                       ║${NC}"
    echo -e "${GREEN}║  Vous pouvez pusher en toute confiance               ║${NC}"
    echo -e "${GREEN}╚═══════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo "Fichiers générés:"
    ls -lh "$DMG_NAME"
    ls -lh "${DMG_NAME}.sha256"
    echo ""
    echo "Pour nettoyer:"
    echo "  rm -rf GeneWeb.app GeneWeb-*.dmg*"
    exit 0
else
    echo -e "${RED}╔═══════════════════════════════════════════════════════╗${NC}"
    echo -e "${RED}║  ⚠️  DES TESTS ONT ÉCHOUÉ                             ║${NC}"
    echo -e "${RED}║                                                       ║${NC}"
    echo -e "${RED}║  Corrigez les erreurs avant de pusher                ║${NC}"
    echo -e "${RED}╚═══════════════════════════════════════════════════════╝${NC}"
    echo ""
    echo "Logs disponibles:"
    echo "  /tmp/make_distrib.log"
    echo "  /tmp/create_bundle.log"
    echo "  /tmp/create_dmg.log"
    echo "  /tmp/geneweb_launch.log"
    exit 1
fi
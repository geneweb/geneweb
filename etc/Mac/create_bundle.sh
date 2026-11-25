#!/bin/bash

set -e

APP_NAME="GeneWeb"
BUNDLE_DIR="${APP_NAME}.app/Contents"
DISTRIB_DIR="distribution"

if [ ! -d "$DISTRIB_DIR" ]; then
    echo "❌ Distribution directory not found. Run 'make distrib' first."
    exit 1
fi

echo "╔═══════════════════════════════════════════════════════╗"
echo "║     Création du bundle GeneWeb.app pour macOS         ║"
echo "╚═══════════════════════════════════════════════════════╝"
echo ""

rm -rf "${APP_NAME}.app"

echo "📁 Création de l’applet (AppleScript, reste ouvert dans le Dock)..."
# L’applet reste active dans le Dock tant que GeneWeb tourne :
#   - Dock → clic droit → Quitter (ou ⌘Q) arrête gwd et gwsetup
#   - clic sur l’icône quand GeneWeb tourne : rouvre la page d’accueil
#   - si gwd s’arrête, l’applet quitte d’elle-même
APPLET_SRC=$(mktemp -t geneweb_applet).applescript
cat > "$APPLET_SRC" << 'EOFAPPLET'
on res(n)
	return POSIX path of (path to resource n)
end res

on run
	try
		do shell script quoted form of res("geneweb_launch.sh")
	on error
		-- lancement impossible ou annulé : le lanceur a déjà affiché le message
		quit
	end try
end run

on reopen
	try
		do shell script "open " & quoted form of res("START.htm")
	end try
end reopen

on idle
	try
		do shell script "pgrep -f " & quoted form of (res("gw") & "gwd")
	on error
		quit
	end try
	return 30
end idle

on quit
	try
		do shell script quoted form of res("geneweb_stop.command") & " --from-app"
	end try
	continue quit
end quit
EOFAPPLET
osacompile -s -o "${APP_NAME}.app" "$APPLET_SRC"
rm -f "$APPLET_SRC"
mkdir -p "${BUNDLE_DIR}"/{Frameworks,Resources}

echo "📦 Copie des ressources GeneWeb..."
cp -R "${DISTRIB_DIR}/gw" "${BUNDLE_DIR}/Resources/"
cp "${DISTRIB_DIR}"/*.txt "${BUNDLE_DIR}/Resources/" 2>/dev/null || true
cp "${DISTRIB_DIR}"/*.htm "${BUNDLE_DIR}/Resources/" 2>/dev/null || true
mkdir -p "${BUNDLE_DIR}/Resources/bases"

EXECUTABLES=(
    "anoma" "connex" "consang" "gwfixbase" "ged2gwb" "gwb2ged" 
    "cache_files" "gwc" "gwd" "gwdiff" "gwu" 
    "robot" "gwsetup" "update_nldb"
)

echo ""
echo "🔍 Analyse des dépendances dylib..."

get_dylibs() {
    local binary="$1"
    otool -L "$binary" 2>/dev/null | \
        tail -n +2 | \
        awk '{print $1}' | \
        grep -v "^/System" | \
        grep -v "^/usr/lib" | \
        grep -v "^\t"
}

find_dylib() {
    local lib_name="$1"
    
    local direct_paths=(
        "/opt/homebrew/opt/gmp/lib/libgmp.10.dylib"
        "/opt/homebrew/lib/$lib_name"
        "/usr/local/lib/$lib_name"
    )
    
    for path in "${direct_paths[@]}"; do
        if [ -f "$path" ]; then
            echo "$path"
            return 0
        fi
    done
    
    for base in /opt/homebrew/opt/*/lib /opt/homebrew/Cellar/*/lib; do
        if [ -f "$base/$lib_name" ]; then
            echo "$base/$lib_name"
            return 0
        fi
    done
    
    return 1
}

fix_dylib() {
    local lib_path="$1"
    local lib_name=$(basename "$lib_path")
    
    [ -f "${BUNDLE_DIR}/Frameworks/${lib_name}" ] && return
    
    if [[ "$lib_path" == *"@rpath"* ]]; then
        lib_name=$(basename "$lib_path")
        echo "  🔎 Recherche de ${lib_name}..."
        lib_path=$(find_dylib "$lib_name")
        if [ -z "$lib_path" ]; then
            echo "  ⚠️  ${lib_name} non trouvée"
            return
        fi
        echo "  → Trouvée: ${lib_path}"
    fi
    
    if [ ! -f "$lib_path" ]; then
        echo "  ⚠️  ${lib_path} non trouvée"
        return
    fi
    
    echo "  → ${lib_name}"
    cp "$lib_path" "${BUNDLE_DIR}/Frameworks/"
    chmod +w "${BUNDLE_DIR}/Frameworks/${lib_name}"
    install_name_tool -id "@rpath/${lib_name}" "${BUNDLE_DIR}/Frameworks/${lib_name}" 2>/dev/null || true
    
    for dep in $(get_dylibs "${BUNDLE_DIR}/Frameworks/${lib_name}"); do
        fix_dylib "$dep"
        local dep_name=$(basename "$dep")
        if [ -f "${BUNDLE_DIR}/Frameworks/${dep_name}" ]; then
            install_name_tool -change "$dep" "@loader_path/${dep_name}" \
                "${BUNDLE_DIR}/Frameworks/${lib_name}" 2>/dev/null || true
        fi
    done
}

for exe in "${EXECUTABLES[@]}"; do
    exe_path="${BUNDLE_DIR}/Resources/gw/${exe}"
    
    if [ ! -f "$exe_path" ]; then
        echo "  ⚠️  ${exe} non trouvé"
        continue
    fi
    
    echo "Analyse de ${exe}..."
    for lib in $(get_dylibs "$exe_path"); do
        fix_dylib "$lib"
    done
done

echo ""
echo "🔧 Mise à jour des chemins dans les exécutables..."

for exe in "${EXECUTABLES[@]}"; do
    exe_path="${BUNDLE_DIR}/Resources/gw/${exe}"
    [ ! -f "$exe_path" ] && continue
    
    echo "  ${exe}"
    
    for rpath in $(otool -l "$exe_path" 2>/dev/null | grep -A2 LC_RPATH | grep path | awk '{print $2}'); do
        install_name_tool -delete_rpath "$rpath" "$exe_path" 2>/dev/null || true
    done
    
    install_name_tool -add_rpath "@executable_path/../../Frameworks" "$exe_path" 2>/dev/null || true
    
    for lib in $(get_dylibs "$exe_path"); do
        lib_name=$(basename "$lib")
        if [ -f "${BUNDLE_DIR}/Frameworks/${lib_name}" ]; then
            install_name_tool -change "$lib" "@rpath/${lib_name}" "$exe_path" 2>/dev/null || true
        fi
    done
done

echo ""
echo "🔧 Vérification et correction de libgmp..."

if [ ! -f "${BUNDLE_DIR}/Frameworks/libgmp.10.dylib" ]; then
    if [ -f "/opt/homebrew/opt/gmp/lib/libgmp.10.dylib" ]; then
        echo "   → Copie forcée de libgmp.10.dylib"
        cp /opt/homebrew/opt/gmp/lib/libgmp.10.dylib "${BUNDLE_DIR}/Frameworks/"
        chmod +w "${BUNDLE_DIR}/Frameworks/libgmp.10.dylib"
        install_name_tool -id "@rpath/libgmp.10.dylib" "${BUNDLE_DIR}/Frameworks/libgmp.10.dylib"
    fi
fi

for exe in "${BUNDLE_DIR}"/Resources/gw/*; do
    [ ! -x "$exe" ] || [ ! -f "$exe" ] && continue
    
    if otool -L "$exe" 2>/dev/null | grep -q "libgmp"; then
        for rpath in $(otool -l "$exe" 2>/dev/null | grep -A2 LC_RPATH | grep path | awk '{print $2}'); do
            install_name_tool -delete_rpath "$rpath" "$exe" 2>/dev/null || true
        done
        
        install_name_tool -add_rpath "@executable_path/../../Frameworks" "$exe" 2>/dev/null || true
        
        for old_path in $(otool -L "$exe" 2>/dev/null | grep libgmp | awk '{print $1}'); do
            install_name_tool -change "$old_path" "@rpath/libgmp.10.dylib" "$exe" 2>/dev/null || true
        done
    fi
done

echo ""
echo "🔧 Vérification et correction de pcre2..."

# Détecter quelle version de pcre2 est nécessaire
PCRE2_NEEDED=$(otool -L "${BUNDLE_DIR}"/Resources/gw/gwd 2>/dev/null | grep pcre2 | head -1 | awk '{print $1}')

if [ -n "$PCRE2_NEEDED" ]; then
    PCRE2_FILE=$(basename "$PCRE2_NEEDED")
    echo "   Version nécessaire: $PCRE2_FILE"
    echo "   Chemin actuel: $PCRE2_NEEDED"
    
    if [ ! -f "${BUNDLE_DIR}/Frameworks/${PCRE2_FILE}" ]; then
        # Chercher pcre2 dans plusieurs emplacements
        PCRE2_FOUND=""
        
        for search_path in \
            "/opt/homebrew/opt/pcre2/lib/$PCRE2_FILE" \
            "/opt/homebrew/lib/$PCRE2_FILE" \
            "/opt/local/lib/$PCRE2_FILE" \
            "/usr/local/lib/$PCRE2_FILE"; do
            
            if [ -f "$search_path" ]; then
                PCRE2_FOUND="$search_path"
                break
            fi
        done
        
        # Recherche générique si pas trouvé
        if [ -z "$PCRE2_FOUND" ]; then
            PCRE2_FOUND=$(find /opt/homebrew /opt/local /usr/local -name "$PCRE2_FILE" 2>/dev/null | head -1)
        fi
        
        if [ -n "$PCRE2_FOUND" ] && [ -f "$PCRE2_FOUND" ]; then
            echo "   → Copie de $PCRE2_FILE depuis $PCRE2_FOUND"
            cp "$PCRE2_FOUND" "${BUNDLE_DIR}/Frameworks/"
            chmod +w "${BUNDLE_DIR}/Frameworks/${PCRE2_FILE}"
            install_name_tool -id "@rpath/${PCRE2_FILE}" "${BUNDLE_DIR}/Frameworks/${PCRE2_FILE}"
        else
            echo "   ❌ ERREUR: $PCRE2_FILE non trouvée !"
            echo "   Installez avec: brew install pcre2"
            exit 1
        fi
    fi
    
    # Corriger tous les exécutables
    for exe in "${BUNDLE_DIR}"/Resources/gw/*; do
        [ ! -x "$exe" ] || [ ! -f "$exe" ] && continue
        
        if otool -L "$exe" 2>/dev/null | grep -q "pcre2"; then
            # Nettoyer rpaths
            for rpath in $(otool -l "$exe" 2>/dev/null | grep -A2 LC_RPATH | grep path | awk '{print $2}'); do
                install_name_tool -delete_rpath "$rpath" "$exe" 2>/dev/null || true
            done
            
            # Ajouter rpath
            install_name_tool -add_rpath "@executable_path/../../Frameworks" "$exe" 2>/dev/null || true
            
            # Changer toutes les références pcre2 vers @rpath
            for old_path in $(otool -L "$exe" 2>/dev/null | grep pcre2 | awk '{print $1}'); do
                install_name_tool -change "$old_path" "@rpath/${PCRE2_FILE}" "$exe" 2>/dev/null || true
            done
            
            echo "   ✅ Corrigé: $(basename "$exe")"
        fi
    done
fi

echo ""
echo "📝 Création des scripts de lancement..."

cat > "${BUNDLE_DIR}/Resources/geneweb_launch.sh" << 'EOFMAIN'
#!/bin/bash
set -euo pipefail

BUNDLE_DIR="$(cd "$(dirname "$0")/.." && pwd)"

if [[ "$BUNDLE_DIR" == /Volumes/* ]]; then
    osascript -e 'display alert "GeneWeb" message "GeneWeb ne peut pas être lancé depuis le DMG.

1. Glissez GeneWeb.app vers Applications
2. Éjectez le DMG
3. Lancez GeneWeb depuis Applications

GeneWeb cannot run from the DMG: drag it to Applications first." as critical' >/dev/null 2>&1 || true
    exit 1
fi

# Charger la configuration utilisateur
CONFIG_FILE="$HOME/.geneweb/config"
mkdir -p "$HOME/.geneweb"

# Créer le fichier de config par défaut s'il n'existe pas
if [ ! -f "$CONFIG_FILE" ]; then
    cat > "$CONFIG_FILE" << 'EOFCONFIG'
# Configuration GeneWeb
# Décommentez et modifiez les lignes ci-dessous pour personnaliser

# Emplacement des bases de données
# Par défaut: dans l’application
# Exemples:
#   BASES_DIR="$HOME/Documents/GeneWeb/bases"
#   BASES_DIR="/Volumes/External/GeneWeb/bases"
#BASES_DIR="$HOME/Documents/GeneWeb/bases"

# Port pour gwd (par défaut 2317)
#GWD_PORT=2317

# Port pour gwsetup (par défaut 2316)
#GWSETUP_PORT=2316

# Langue (de, es, fr, it, lv, sv, en)
# Par défaut: détectée automatiquement
#GENEWEB_LANG=fr
EOFCONFIG
fi

# Lire la configuration (en évaluant les variables)
if [ -f "$CONFIG_FILE" ]; then
    # Lire ligne par ligne et évaluer les variables
    while IFS= read -r line || [ -n "$line" ]; do
        # Ignorer les commentaires et lignes vides
        [[ "$line" =~ ^[[:space:]]*# ]] && continue
        [[ -z "$line" ]] && continue
        # Évaluer et exporter la ligne
        eval "export $line"
    done < "$CONFIG_FILE"
fi

# Définir BASES_DIR (ordre de priorité: var env > config > défaut)
if [ -n "${GENEWEB_BASES_DIR:-}" ]; then
    BASES_DIR="$GENEWEB_BASES_DIR"
elif [ -n "${BASES_DIR:-}" ]; then
    # Évaluer les variables dans BASES_DIR (comme $HOME)
    eval BASES_DIR="$BASES_DIR"
else
    BASES_DIR="$BUNDLE_DIR/Resources/bases"
fi

LOG_FILE="/tmp/geneweb_launch.log"
exec > "$LOG_FILE" 2>&1

GW_DIR="$BUNDLE_DIR/Resources/gw"
START_PAGE="$BUNDLE_DIR/Resources/START.htm"

# Créer et vérifier le dossier bases
mkdir -p "$BASES_DIR" || {
    exec 1>&2
    echo ""
    echo "╔═══════════════════════════════════════════════════════╗"
    echo "║                    ERREUR                             ║"
    echo "╠═══════════════════════════════════════════════════════╣"
    echo "║  Impossible de créer le dossier des bases             ║"
    echo "║                                                       ║"
    echo "║  Dossier: $BASES_DIR"
    echo "║                                                       ║"
    echo "║  Vérifiez les permissions                             ║"
    echo "╚═══════════════════════════════════════════════════════╝"
    echo ""
    echo "Log complet: $LOG_FILE"
    sleep 5
    exit 1
}

detect_language() {
    # Priorité: GENEWEB_LANG (config) > LANG système
    if [ -n "${GENEWEB_LANG:-}" ]; then
        echo "$GENEWEB_LANG"
    else
        case "${LANG:-$(defaults read -g AppleLocale 2>/dev/null || true)}" in
            de*) echo "de" ;; es*) echo "es" ;; fr*) echo "fr" ;;
            it*) echo "it" ;; lv*) echo "lv" ;; sv*) echo "sv" ;;
            *)   echo "en" ;;
        esac
    fi
}

DETECTED_LANG=$(detect_language)

echo "  Langue: $DETECTED_LANG"

msg() {
    case "$1" in
        start)
            [[ "$DETECTED_LANG" = "fr" ]] && echo "🚀 Démarrage de GeneWeb..." || echo "🚀 Starting GeneWeb..."
            ;;
        ready)
            if [[ "$DETECTED_LANG" = "fr" ]]; then
                echo ""; echo "✅ GeneWeb est lancé !"; echo ""
                echo "🌐 Votre navigateur va s’ouvrir."
                echo "📁 Bases: $BASES_DIR"
                echo "🔌 Ports: gwd=$GWD_PORT, gwsetup=$GWSETUP_PORT"
                echo "❓ Options gwd: $GWD_OPTS"
                echo ""
                echo "📋 Logs disponibles :"
                echo "    gwsetup: $GWSETUP_LOG"
                echo "    gwd: $GWD_LOG"
                echo ""
                echo "Pour arrêter GeneWeb :"
                echo "  - Dock : clic droit sur GeneWeb → Quitter (ou ⌘Q)"
                echo "  - Script : Utilitaires/Arrêter GeneWeb.command"
            else
                echo ""; echo "✅ GeneWeb is running!"; echo ""
                echo "🌐 Your browser will open."
                echo "📁 Databases: $BASES_DIR"
                echo "🔌 Ports: gwd=$GWD_PORT, gwsetup=$GWSETUP_PORT"
                echo "❓ Options gwd: $GWD_OPTS"
                echo ""
                echo "📋 Logs available:"
                echo "    gwsetup: $GWSETUP_LOG"
                echo "    gwd: $GWD_LOG"
                echo ""
                echo "To stop GeneWeb:"
                echo "  - Dock: right-click GeneWeb → Quit (or ⌘Q)"
                echo "  - Script: Utilitaires/Arrêter GeneWeb.command"
            fi
            ;;
        stop)
            [[ "$DETECTED_LANG" = "fr" ]] && echo "🛑 Arrêt..." || echo "🛑 Stopping..."
            ;;
    esac
}

cleanup() {
    msg "stop"
    pkill -f "$GW_DIR/gwd" 2>/dev/null || true
    pkill -f "$GW_DIR/gwsetup" 2>/dev/null || true
    exit 1
}

# Affiche une alerte (le terminal n’est pas visible quand l’app est lancée depuis le Finder)
alert() {
    echo "❌ $1"
    # Échapper \ et " pour AppleScript
    local m="${1//\\/\\\\}"; m="${m//\"/\\\"}"
    osascript -e "display alert \"GeneWeb\" message \"$m\" as critical" >/dev/null 2>&1 || true
}

# PID du processus qui écoute sur un port (vide si libre)
port_owner() { lsof -nP -iTCP:"$1" -sTCP:LISTEN -t 2>/dev/null | head -1 || true; }
# Tous les PID qui écoutent sur un port (un serveur peut avoir des processus fils
# qui héritent de la socket)
port_pids() { lsof -nP -iTCP:"$1" -sTCP:LISTEN -t 2>/dev/null | sort -u | tr '\n' ' ' || true; }

# Définir les ports
GWD_PORT="${GWD_PORT:-2317}"
GWSETUP_PORT="${GWSETUP_PORT:-2316}"
GWD_OPTS="${GWD_OPTS:-}"

# Arrêter une éventuelle instance précédente de CE bundle (pas les autres gwd)
pkill -f "$GW_DIR/gwd" 2>/dev/null || true
pkill -f "$GW_DIR/gwsetup" 2>/dev/null || true
for i in 1 2 3 4 5; do
    [ -z "$(port_owner "$GWSETUP_PORT")" ] && [ -z "$(port_owner "$GWD_PORT")" ] && break
    sleep 1
done

# Pose une question avec deux boutons ; renvoie le libellé du bouton choisi
# (renvoie le 2e bouton si osascript échoue ou si l’utilisateur annule)
ask() {
    local m="${1//\\/\\\\}"; m="${m//\"/\\\"}"
    local r
    r=$(osascript -e "button returned of (display alert \"GeneWeb\" message \"$m\" as critical buttons {\"$3\", \"$2\"} default button \"$2\" cancel button \"$3\")" 2>/dev/null) || r="$3"
    echo "$r"
}

# Décrit les ports occupés ; remplit BUSY (texte) et BUSY_PIDS
check_ports() {
    BUSY=""; BUSY_PIDS=""
    local p pids pid var cmd
    for p in "$GWSETUP_PORT" "$GWD_PORT"; do
        pids=$(port_pids "$p")
        [ -z "${pids// /}" ] && continue
        [ "$p" = "$GWSETUP_PORT" ] && var=GWSETUP_PORT || var=GWD_PORT
        # Ne montrer que le(s) processus principal(aux) : ceux dont le parent
        # n’écoute pas lui-même sur le port (les workers de gwd sont des fils)
        local mains="" nchild=0 ppid
        for pid in $pids; do
            ppid=$(ps -o ppid= -p "$pid" 2>/dev/null | tr -d ' ' || true)
            case " $pids " in
                *" $ppid "*) nchild=$((nchild + 1)) ;;
                *) mains="$mains $pid" ;;
            esac
        done
        for pid in $mains; do
            cmd=$(ps -o user=,command= -p "$pid" 2>/dev/null | cut -c1-90 || true)
            BUSY="${BUSY}
• $p ($var) : PID $pid — ${cmd}"
        done
        if [ "$nchild" -gt 0 ]; then
            if [[ "$DETECTED_LANG" = "fr" ]]; then
                BUSY="${BUSY} (+ $nchild processus fils)"
            else
                BUSY="${BUSY} (+ $nchild child processes)"
            fi
        fi
        # Les principaux d’abord, les fils ensuite
        BUSY_PIDS="$BUSY_PIDS $mains $(for pid in $pids; do case " $mains " in *" $pid "*) ;; *) echo "$pid" ;; esac; done | tr '\n' ' ')"
    done
}

check_ports
if [ -n "$BUSY" ]; then
    if [[ "$DETECTED_LANG" = "fr" ]]; then
        Q="Port(s) déjà utilisé(s) par un autre programme :
${BUSY}

Voulez-vous arrêter ce(s) programme(s) et démarrer GeneWeb ?

(Sinon, vous pouvez choisir d’autres ports dans $CONFIG_FILE, par exemple GWSETUP_PORT=2326 et GWD_PORT=2327.)"
        B_KILL="Arrêter et démarrer"; B_QUIT="Annuler"
    else
        Q="Port(s) already in use by another program:
${BUSY}

Do you want to stop this program and start GeneWeb?

(Otherwise, you can choose other ports in $CONFIG_FILE, e.g. GWSETUP_PORT=2326 and GWD_PORT=2327.)"
        B_KILL="Stop and start"; B_QUIT="Cancel"
    fi
    echo "Ports occupés :${BUSY}"
    if [ "$(ask "$Q" "$B_KILL" "$B_QUIT")" = "$B_KILL" ]; then
        # TERM d’abord, puis KILL ; on relit les ports à chaque tour car
        # d’autres processus (fils) peuvent apparaître comme propriétaires
        for sig in TERM TERM TERM KILL KILL; do
            echo "→ kill -$sig :$BUSY_PIDS"
            kill -"$sig" $BUSY_PIDS 2>&1 || true
            sleep 1
            check_ports
            [ -z "$BUSY" ] && break
        done
        if [ -n "$BUSY" ]; then
            if [[ "$DETECTED_LANG" = "fr" ]]; then
                alert "Impossible d’arrêter le(s) programme(s) :${BUSY}

Il appartient peut-être à un autre utilisateur. Arrêtez-le manuellement ou changez de ports dans $CONFIG_FILE."
            else
                alert "Could not stop the program(s):${BUSY}

It may belong to another user. Stop it manually or change the ports in $CONFIG_FILE."
            fi
            exit 1
        fi
    else
        echo "→ Annulé par l’utilisateur"
        exit 1
    fi
fi

trap cleanup INT TERM EXIT

cd "$BASES_DIR"

msg "start"

# Définir les emplacements des logs (depuis config ou par défaut dans /tmp)
GWSETUP_LOG="${GWSETUP_LOG:-/tmp/geneweb_gwsetup.log}"
GWD_LOG="${GWD_LOG:-/tmp/geneweb_gwd.log}"

# Créer les répertoires des logs si nécessaire
mkdir -p "$(dirname "$GWSETUP_LOG")" 2>/dev/null || true
mkdir -p "$(dirname "$GWD_LOG")" 2>/dev/null || true

# Sauvegarder les anciens logs
[ -f "$GWSETUP_LOG" ] && mv "$GWSETUP_LOG" "${GWSETUP_LOG}.old" 2>/dev/null || true
[ -f "$GWD_LOG" ] && mv "$GWD_LOG" "${GWD_LOG}.old" 2>/dev/null || true

echo "=== Lancement de GeneWeb à $(date) ==="
echo "Configuration:"
echo "  BASES_DIR: $BASES_DIR"
echo "  GWD_PORT: $GWD_PORT"
echo "  GWSETUP_PORT: $GWSETUP_PORT"
echo "  GWD_OPTS: $GWD_OPTS"
echo "  Config file: $CONFIG_FILE"
echo "Logs:"
echo "  gwsetup: $GWSETUP_LOG"
echo "  gwd: $GWD_LOG"

# Lancer gwsetup et le détacher complètement
"$GW_DIR/gwsetup" -gd "$GW_DIR" -lang "$DETECTED_LANG" -p "$GWSETUP_PORT" > "$GWSETUP_LOG" 2>&1 &
GWSETUP_PID=$!
disown $GWSETUP_PID
sleep 3

if [ -z "$(port_owner "$GWSETUP_PORT")" ]; then
    [[ "$DETECTED_LANG" = "fr" ]] && alert "Échec du lancement de gwsetup (voir $GWSETUP_LOG)" || alert "gwsetup failed to start (see $GWSETUP_LOG)"
    echo "Voir les logs: $GWSETUP_LOG"
    cat "$GWSETUP_LOG"
    exit 1
fi

# Lancer gwd et le détacher complètement
"$GW_DIR/gwd" -bd "$BASES_DIR" -hd "$GW_DIR" -p "$GWD_PORT" $GWD_OPTS > "$GWD_LOG" 2>&1 &
GWD_PID=$!
disown $GWD_PID
sleep 3

if [ -z "$(port_owner "$GWD_PORT")" ]; then
    [[ "$DETECTED_LANG" = "fr" ]] && alert "Échec du lancement de gwd (voir $GWD_LOG)" || alert "gwd failed to start (see $GWD_LOG)"
    echo "Voir les logs: $GWD_LOG"
    cat "$GWD_LOG"
    exit 1
fi

msg "ready"

sleep 1
[[ -f "$START_PAGE" ]] && open "$START_PAGE" || open "http://127.0.0.1:$GWSETUP_PORT/"

# Désactiver le trap cleanup car les processus sont détachés
trap - INT TERM EXIT

# Rendre la main à l’applet (les serveurs continuent ; l’applet reste dans le Dock)
exit 0
EOFMAIN

chmod +x "${BUNDLE_DIR}/Resources/geneweb_launch.sh"

cat > "${BUNDLE_DIR}/Resources/gwd.command" << 'EOFGWD'
#!/bin/bash
set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GW_DIR="$SCRIPT_DIR/gw"
BASES_DIR="$SCRIPT_DIR/bases"
mkdir -p "$BASES_DIR"
echo "Starting gwd..."
pkill -f '/gwd' 2>/dev/null || true
sleep 0.5
cd "$BASES_DIR"
exec "$GW_DIR/gwd" -bd "$BASES_DIR" -hd "$GW_DIR"
EOFGWD

chmod +x "${BUNDLE_DIR}/Resources/gwd.command"

cat > "${BUNDLE_DIR}/Resources/gwsetup.command" << 'EOFSETUP'
#!/bin/bash
set -e
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
GW_DIR="$SCRIPT_DIR/gw"
case "${LANG:-}" in
    de*) LANG_ARG="de" ;; es*) LANG_ARG="es" ;; fr*) LANG_ARG="fr" ;;
    it*) LANG_ARG="it" ;; lv*) LANG_ARG="lv" ;; sv*) LANG_ARG="sv" ;;
    *)   LANG_ARG="en" ;;
esac
echo "Starting gwsetup..."
pkill -f '/gwsetup' 2>/dev/null || true
sleep 0.5
exec "$GW_DIR/gwsetup" -gd "$GW_DIR" -lang "$LANG_ARG"
EOFSETUP

chmod +x "${BUNDLE_DIR}/Resources/gwsetup.command"

cat > "${BUNDLE_DIR}/Resources/geneweb_stop.command" << 'EOFSTOP'
#!/bin/bash
# Arrête les serveurs lancés depuis un bundle GeneWeb.app (pas les gwd de
# développement). Appelé par l’applet avec --from-app quand on quitte GeneWeb ;
# lancé à la main, il fait aussi quitter l’applet du Dock.
PAT='/Contents/Resources/gw/'
echo "🛑 Arrêt de GeneWeb / Stopping GeneWeb..."
pkill -f "${PAT}gwd" 2>/dev/null
pkill -f "${PAT}gwsetup" 2>/dev/null
for i in 1 2 3 4 5; do
    pgrep -f "${PAT}gw(d|setup)" >/dev/null 2>&1 || break
    sleep 1
done
pkill -9 -f "${PAT}gwd" 2>/dev/null
pkill -9 -f "${PAT}gwsetup" 2>/dev/null
if [ "${1:-}" != "--from-app" ]; then
    # en arrière-plan : l’applet exécute alors son propre « on quit »
    osascript -e 'tell application id "org.geneweb.GeneWeb" to quit' >/dev/null 2>&1 &
    echo "✅ GeneWeb arrêté / GeneWeb stopped"
    sleep 2
fi
exit 0
EOFSTOP

chmod +x "${BUNDLE_DIR}/Resources/geneweb_stop.command"

echo ""
echo "📎 Création des utilitaires utilisateur..."

mkdir -p "${BUNDLE_DIR}/Resources/Utilitaires"

# 1. Arrêter GeneWeb
cp "${BUNDLE_DIR}/Resources/geneweb_stop.command" \
   "${BUNDLE_DIR}/Resources/Utilitaires/Arrêter GeneWeb.command"

# 2. Lancer seulement gwd
cat > "${BUNDLE_DIR}/Resources/Utilitaires/Lancer seulement gwd.command" << 'EOF'
#!/bin/bash
set -e
BUNDLE_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
GW_DIR="$BUNDLE_DIR/Resources/gw"
BASES_DIR="$BUNDLE_DIR/Resources/bases"
echo "🚀 Lancement de gwd..."
mkdir -p "$BASES_DIR"
cd "$BASES_DIR"
pkill -f '/gwd' 2>/dev/null || true
sleep 0.5
exec "$GW_DIR/gwd" -bd "$BASES_DIR" -hd "$GW_DIR"
EOF
chmod +x "${BUNDLE_DIR}/Resources/Utilitaires/Lancer seulement gwd.command"

# 3. Lancer seulement gwsetup
cat > "${BUNDLE_DIR}/Resources/Utilitaires/Lancer seulement gwsetup.command" << 'EOF'
#!/bin/bash
set -e
BUNDLE_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
GW_DIR="$BUNDLE_DIR/Resources/gw"
case "${LANG:-}" in
    de*) LANG_ARG="de" ;; es*) LANG_ARG="es" ;; fr*) LANG_ARG="fr" ;;
    it*) LANG_ARG="it" ;; lv*) LANG_ARG="lv" ;; sv*) LANG_ARG="sv" ;;
    *)   LANG_ARG="en" ;;
esac
echo "🚀 Lancement de gwsetup..."
pkill -f '/gwsetup' 2>/dev/null || true
sleep 0.5
exec "$GW_DIR/gwsetup" -gd "$GW_DIR" -lang "$LANG_ARG"
EOF
chmod +x "${BUNDLE_DIR}/Resources/Utilitaires/Lancer seulement gwsetup.command"

# 4. Ouvrir le dossier des bases
cat > "${BUNDLE_DIR}/Resources/Utilitaires/Ouvrir le dossier des bases.command" << 'EOF'
#!/bin/bash
BUNDLE_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
BASES_DIR="$BUNDLE_DIR/Resources/bases"
mkdir -p "$BASES_DIR"
open "$BASES_DIR"
EOF
chmod +x "${BUNDLE_DIR}/Resources/Utilitaires/Ouvrir le dossier des bases.command"

# 5. Voir les logs
cat > "${BUNDLE_DIR}/Resources/Utilitaires/Voir les logs.command" << 'EOF'
#!/bin/bash
BUNDLE_DIR="$(cd "$(dirname "$0")/../.." && pwd)"
BASES_DIR="$BUNDLE_DIR/Resources/bases"
echo "=== Logs GeneWeb ==="
echo ""
[ -f "/tmp/geneweb_launch.log" ] && echo "📋 Lancement:" && tail -50 /tmp/geneweb_launch.log && echo ""
[ -f "$BASES_DIR/gwd.log" ] && echo "📋 gwd:" && tail -30 "$BASES_DIR/gwd.log" && echo ""
[ -f "$BASES_DIR/gwsetup.log" ] && echo "📋 gwsetup:" && tail -30 "$BASES_DIR/gwsetup.log" && echo ""
echo "Appuyez sur une touche pour fermer..."
read -n 1
EOF
chmod +x "${BUNDLE_DIR}/Resources/Utilitaires/Voir les logs.command"

# 6. README
cat > "${BUNDLE_DIR}/Resources/Utilitaires/LISEZMOI.txt" << 'EOF'
╔═══════════════════════════════════════════════════════╗
║           Utilitaires GeneWeb                         ║
╚═══════════════════════════════════════════════════════╝

📝 Arrêter GeneWeb.command
   Arrête proprement gwd et gwsetup

🚀 Lancer seulement gwd.command
   Lance uniquement le serveur gwd

🚀 Lancer seulement gwsetup.command
   Lance uniquement l’interface de configuration

📁 Ouvrir le dossier des bases.command
   Ouvre le dossier de vos bases de données

📋 Voir les logs.command
   Affiche les logs pour diagnostic

Double-cliquez sur le script souhaité.
EOF

echo "   ✅ Utilitaires créés"

cat > "${BUNDLE_DIR}/Resources/README.txt" << 'EOFREADME'
GeneWeb pour macOS
==================

LANCEMENT : Double-cliquez sur GeneWeb.app
ARRÊT : Dock → clic droit sur GeneWeb → Quitter (ou ⌘Q)
LOGS : /tmp/geneweb_launch.log
SUPPORT : https://github.com/geneweb/geneweb
EOFREADME

VERSION="7.0"
if [ -f "lib/version.txt" ]; then
    VERSION=$(awk -F\" '/^let ver =/ {print $2}' lib/version.txt)
fi

PLIST="${BUNDLE_DIR}/Info.plist"
pb() { /usr/libexec/PlistBuddy -c "Set :$1 $2" "$PLIST" 2>/dev/null \
    || /usr/libexec/PlistBuddy -c "Add :$1 ${3:-string} $2" "$PLIST"; }
pb CFBundleIdentifier         org.geneweb.GeneWeb
pb CFBundleName               GeneWeb
pb CFBundleDisplayName        GeneWeb
pb CFBundleVersion            "${VERSION}"
pb CFBundleShortVersionString "${VERSION}"
pb CFBundleIconFile           GeneWeb
pb NSHighResolutionCapable    true bool
pb LSMinimumSystemVersion     10.13
pb NSHumanReadableCopyright   "Copyright © INRIA"

# Générer l’icône si elle existe
echo ""
echo "🎨 Génération de l’icône macOS…"

if [ ! -f "hd/images/geneweb_icon.png" ]; then
    echo "   ⚠️  geneweb_icon.png manquante (généré sans icône)"
else
    # Générer le .icns à la volée
    chmod +x ./etc/Mac/create_icon_set.sh
    ./etc/Mac/create_icon_set.sh
    
    if [ -f "geneweb.icns" ]; then
        mv "geneweb.icns" "${BUNDLE_DIR}/Resources/GeneWeb.icns"
        # L’applet d’osacompile porte son icône dans un catalogue d’assets
        # (Assets.car + CFBundleIconName), prioritaire sur CFBundleIconFile :
        # on le retire pour que macOS utilise GeneWeb.icns
        rm -f "${BUNDLE_DIR}/Resources/Assets.car" "${BUNDLE_DIR}/Resources/applet.icns"
        /usr/libexec/PlistBuddy -c "Delete :CFBundleIconName" "${BUNDLE_DIR}/Info.plist" 2>/dev/null || true
        echo "   ✅ Icône générée et ajoutée"
    else
        echo "   ❌ Échec génération icône"
    fi
fi

echo ""
echo "🧹 Nettoyage des fichiers système..."

# Supprimer les fichiers .DS_Store (cause des problèmes de permissions)
find "${APP_NAME}.app" -name ".DS_Store" -delete 2>/dev/null || true

# Supprimer les fichiers cachés macOS
find "${APP_NAME}.app" -name "._*" -delete 2>/dev/null || true

# Supprimer les fichiers de sauvegarde
find "${APP_NAME}.app" -name "*~" -delete 2>/dev/null || true
find "${APP_NAME}.app" -name "*.bak" -delete 2>/dev/null || true

# Supprimer les fichiers Python compilés
find "${APP_NAME}.app" -name "*.pyc" -delete 2>/dev/null || true
find "${APP_NAME}.app" -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true

# Fixer toutes les permissions
echo "   → Correction des permissions..."
chmod -R u+w "${APP_NAME}.app" 2>/dev/null || true
chmod -R go-w "${APP_NAME}.app" 2>/dev/null || true

# S’assurer que les exécutables sont exécutables
chmod +x "${BUNDLE_DIR}"/MacOS/* 2>/dev/null || true
chmod +x "${BUNDLE_DIR}"/Resources/gw/* 2>/dev/null || true
chmod +x "${BUNDLE_DIR}"/Resources/*.command 2>/dev/null || true

echo "   ✅ Nettoyage et permissions corrigés"

echo ""
echo "🔐 Signature finale…"

# Supprimer la quarantaine
xattr -cr "${APP_NAME}.app" 2>/dev/null || true

# Signer dans le bon ordre : dylibs -> exécutables -> bundle
echo "   → Signature des dylibs..."
for lib in "${BUNDLE_DIR}"/Frameworks/*.dylib; do
    [ -f "$lib" ] && codesign --force --sign - --timestamp=none "$lib" 2>/dev/null || true
done

echo "   → Signature des exécutables…"
for exe in "${BUNDLE_DIR}"/Resources/gw/*; do
    [ -x "$exe" ] && [ -f "$exe" ] && codesign --force --sign - --timestamp=none "$exe" 2>/dev/null || true
done

echo "   → Signature du bundle…"
codesign --force --deep --sign - --timestamp=none "${APP_NAME}.app" 2>/dev/null || true

# Forcer le Finder/Dock à relire l’icône
touch "${APP_NAME}.app"

echo ""
echo "✅ Bundle créé avec succès : ${APP_NAME}.app"
echo ""
echo "📊 Statistiques:"
NUM_EXES=$(ls -1 "${BUNDLE_DIR}"/Resources/gw/{connex,consang,gwd,gwc,gwu,gwsetup,ged2gwb,gwb2ged,update_nldb} 2>/dev/null | wc -l | tr -d ' ')
NUM_DYLIBS=$(ls -1 "${BUNDLE_DIR}"/Frameworks/*.dylib 2>/dev/null | wc -l | tr -d ' ')
echo "   Exécutables: ${NUM_EXES}"
echo "   Dylibs:      ${NUM_DYLIBS}"
echo ""
echo "🚀 Pour tester: open ${APP_NAME}.app"
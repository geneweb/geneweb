# ged2gwb — Options et documentation rapide

FR — Court descriptif
- Binaire : ged2gwb (OCaml, partie de Geneweb)
- But : convertir un fichier GEDCOM (.ged) en base GeneWeb (.gwb). Lit le GEDCOM, extrait personnes/familles/évènements, applique des règles de normalisation (noms, dates, charset...) et écrit une base GeneWeb.
- Emplacement conseillé d'exécution : répertoire projet contenant `bases/` ou via `-bd` pour indiquer le chemin.

EN — Short description
- Binary: ged2gwb (OCaml, part of Geneweb)
- Purpose: convert a GEDCOM file (.ged) into a GeneWeb database (.gwb). It parses GEDCOM, extracts persons/families/events, normalizes names/dates/charset and writes a GeneWeb database file.
- Run where `bases/` is available or use `-bd` to set base dir.

Fonctionnalités principales / Main features
- Parsing complet du GEDCOM (INDI, FAM, SOUR, NOTE, etc.)
- Extraction et normalisation des prénoms et noms (maj/min, particules)
- Détection et traitement des dates (divers calendriers, gestion des mois numérotés)
- Options pour forcer charset, importer fichiers de particules, titre/public name extraction
- Génération d'alertes/logs et contrôle de cohérence (consistency checks)

Options (liste, argument, type, défaut, FR / EN)
- -bd <DIR>
  Type: string — Défaut: "." si non fourni
  FR: répertoire où se trouvent les bases (bases/).
  EN: directory where the "bases" databases folder is installed.

- -o <file>
  Type: string — Défaut: derives from input; fallback "a"
  FR: nom de sortie (basename) pour la base .gwb. Doit contenir lettres, chiffres et '-'.
  EN: output database base name (will become .gwb). Allowed chars: a..z A..Z 0..9 -.

- -f
  Type: flag
  FR: force — supprime la base si elle existe déjà.
  EN: force remove existing database.

- -log <file>
  Type: string
  FR: redirige la trace de log vers ce fichier.
  EN: redirect log output to the file.

- -lf
  Type: flag — Défaut: off
  FR: normalise les prénoms en minuscules avec initiales en majuscule.
  EN: lowercase first names with title-case initials.

- -trackid
  Type: flag
  FR: imprime les correspondances GEDCOM id -> GeneWeb person id.
  EN: print GEDCOM id to GeneWeb id matches.

- -ls
  Type: flag
  FR: surnames -> title-case (essaye de préserver les particules).
  EN: surnames to title-case; attempts to keep particles lowercase.

- -us
  Type: flag
  FR: surnames en MAJUSCULES.
  EN: surnames to UPPERCASE.

- -fne <be>
  Type: string (exactement 2 caractères)
  FR: spécifie deux caractères ouvrant/fermant pour extraire le prénom usuel (ex. "()"). Doit être une chaîne de longueur 2.
  EN: two-character delimiters used to extract enclosed usual first name (e.g. "()"). Must be length 2.

- -efn / -no_efn
  Type: flag / negation
  FR: -efn active l'extraction du premier token comme prénom; -no_efn le désactive.
  EN: -efn extract first token as first name; -no_efn disables.

- -epn / -no_epn
  Type: flag / negation
  FR: -epn détecte les "public names" (titres, nombres romains...) et les sépare. -no_epn désactive.
  EN: -epn extract public names from given first-name field; -no_epn disables.

- -no_pit
  Type: flag
  FR: ne considère pas une personne ayant des titres comme "public".
  EN: do not treat persons with titles as public.

- -tnd
  Type: flag
  FR: essaye de transformer des dates incohérentes en années négatives (ex. naissance après décès).
  EN: try to set negative years when inconsistency is detected.

- -no_nd
  Type: flag
  FR: n'interprète pas une année précédée d'un signe '-' comme négative.
  EN: do not interpret a year prefixed by '-' as a negative year.

- -nc
  Type: flag
  FR: désactive les vérifications de cohérence (no consistency check).
  EN: disable consistency checks.

- -nopicture
  Type: flag
  FR: n'extrait pas les images individuelles (OBJE/FILE).
  EN: do not extract individual pictures.

- -udi x-y
  Type: string format "x-y" (x,y entiers ou vides)
  FR: définit seuils pour considérer une personne vivante/morte quand la date de décès est non définie. x = alive threshold, y = dead threshold. Ex: 80-120.
  EN: set alive/dead thresholds when death is undefined. Format x-y (integers).

- -uin
  Type: flag
  FR: place les tags GEDCOM non traités dans les notes.
  EN: put untreated GEDCOM tags into notes.

- -ds <string>
  Type: string
  FR: chaîne de source par défaut pour personnes/familles sans source.
  EN: default source string for persons/families without source.

- -dates_dm / -dates_md
  Type: flags mutuellement exclusifs
  FR: interprète les dates numériques "12/05/1912" comme jour/mois/année (dm) ou mois/jour/année (md). Si non défini, code tente de deviner et peut avertir.
  EN: interpret numeric-month dates as day/month/year (-dates_dm) or month/day/year (-dates_md).

- -rs_no_mention
  Type: flag
  FR: force relation de famille à NoMention (par défaut Married).
  EN: force family relation to NoMention (default is Married).

- -charset [ANSEL|ASCII|MSDOS]
  Type: string (enum)
  FR: force le décodage du charset GEDCOM (outrepasse l'en-tête). Valeurs supportées: ANSEL, ASCII, MSDOS.
  EN: force given GEDCOM charset decoding; overrides file header. Supported: ANSEL, ASCII, MSDOS.

- -particles <FILE>
  Type: string (fichier)
  FR: charge la liste des particules depuis le fichier fourni.
  EN: use provided file as list of particles.

- -reorg
  Type: flag
  FR: active le mode "reorg" (GWPARAM.reorg).
  EN: enable reorg mode.

Arguments anonymes
- <ged> (optionnel) : chemin vers le fichier GEDCOM d'entrée. Si absent, le programme peut lire stdin selon l'usage (vérifier l'appel).

Exemples rapides / Quick examples
- FR:
  - ged2gwb mytree.ged
    Convertit mytree.ged en mytree.gwb (par défaut).
  - ged2gwb -o mydb -f -log convert.log file.ged
    Force recréation de la base mydb.gwb, log dans convert.log.
  - ged2gwb -bd /opt/geneweb/bases -charset ANSEL file.ged

- EN:
  - ged2gwb mytree.ged
    Converts mytree.ged -> mytree.gwb.
  - ged2gwb -o mydb -f -log convert.log file.ged
    Force recreate mydb.gwb, log to convert.log.
  - ged2gwb -bd /opt/geneweb/bases -charset ANSEL file.ged

Remarques / Notes
- L'option -fne attend exactement deux caractères : premier = ouverture, second = fermeture (ex. '()' ou '""').
- Charset UTF-8 est détecté automatiquement si un BOM est présent; -charset force une valeur spécifique.
- Certains comportements (ex. extraction des titres, particules) dépendent de fichiers de configuration présents dans `bases/` ou fournis via -particles.
- Pour déboguer, utilisez -log pour capturer les warnings et erreurs.

Contact rapide
- Si tu veux que je génère : exemples de sortie, mapping des charset en Python, ou stubs pour la CLI Python (parsing d'options), dis lequel je dois produire ensuite.


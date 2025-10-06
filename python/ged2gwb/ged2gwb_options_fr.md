# ged2gwb — Options et documentation (FR)

Court descriptif
- Binaire : ged2gwb (OCaml, partie de Geneweb)
- But : convertir un fichier GEDCOM (.ged) en base GeneWeb (.gwb). Lit le GEDCOM, extrait personnes/familles/évènements, normalise noms/dates/charset et écrit une base GeneWeb.
- Exécuter dans un répertoire où `bases/` est accessible ou utiliser `-bd` pour indiquer le chemin.

Fonctionnalités principales
- Parsing GEDCOM (INDI, FAM, SOUR, NOTE, etc.)
- Normalisation des prénoms et noms (maj/min, particules)
- Traitement des dates (calendriers variés, mois numérotés)
- Options pour forcer charset, fichier de particules, extraction de titres/public name
- Logs et vérifications de cohérence

Options (nom — argument — type — défaut — description)
- -bd <DIR> — string — "." — Répertoire où se trouve `bases/`.
- -o <file> — string — dérivé du fichier d'entrée, fallback "a" — Nom de base de la base de sortie (.gwb).
- -f — flag — — Supprimer la base existante (force).
- -log <file> — string — — Rediriger la trace de log vers le fichier.
- -lf — flag — off — Met les prénoms en minuscules avec initiales en majuscule.
- -trackid — flag — — Imprime les correspondances GEDCOM id -> GeneWeb id.
- -ls — flag — — Surnoms en « title-case », tente de préserver les particules.
- -us — flag — — Surnoms en MAJUSCULES.
- -fne <be> — string (2 chars) — — Délimiteurs (deux caractères) pour extraire prénom usuel (ex. "()").
- -efn / -no_efn — flag / negation — — Extraire le premier token comme prénom / annuler.
- -epn / -no_epn — flag / negation — — Extraire public name depuis le champ prénom / annuler.
- -no_pit — flag — — Ne pas considérer une personne avec titre comme publique.
- -tnd — flag — — Permet dates négatives en cas d'incohérence (ex. naissance après décès).
- -no_nd — flag — — N'interprète pas "-YEAR" comme année négative.
- -nc — flag — — Désactive les vérifications de cohérence.
- -nopicture — flag — — N'extrait pas les images individuelles.
- -udi x-y — string format "x-y" — x,y entiers — Définit intervalles alive/dead (ex. 80-120).
- -uin — flag — — Met les tags GEDCOM non traités dans les notes.
- -ds <string> — string — "" — Source par défaut pour personnes/familles sans source.
- -dates_dm / -dates_md — flags mutuellement exclusifs — — Interprète dates numériques DM ou MD.
- -rs_no_mention — flag — — Force relation famille à NoMention (défaut Married).
- -charset [ANSEL|ASCII|MSDOS] — enum — — Force le décodage du charset GEDCOM.
- -particles <FILE> — string — — Fichier de particules à charger.
- -reorg — flag — — Active le mode reorg (GWPARAM.reorg).

Arguments anonymes
- <ged> (optionnel) : chemin vers le fichier GEDCOM d'entrée. Si absent, vérifier si le binaire lit stdin dans votre usage.

Exemples
- ged2gwb mytree.ged
- ged2gwb -o mydb -f -log convert.log file.ged
- ged2gwb -bd /opt/geneweb/bases -charset ANSEL file.ged

Remarques
- -fne exige exactement deux caractères (ex. '()' ou '""').
- UTF-8 peut être détecté via BOM; -charset force une valeur.
- Comportements liés aux particules/titres dépendent de fichiers dans `bases/` ou du fichier fourni via -particles.
- Utilisez -log pour capturer warnings / erreurs.

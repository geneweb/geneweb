
# Test Fixtures for ged2gwb Golden Master Tests

Ce dossier contient les fichiers GEDCOM de test utilisés pour les tests Golden Master de `ged2gwb`.

## Fichiers requis

Créez ces fichiers GEDCOM pour couvrir tous les cas de test :

- `sample.ged` - Fichier GEDCOM basique
- `dates_dm.ged` - Dates au format jour/mois
- `dates_md.ged` - Date au format mois/jour
- `with_sources.ged` - Fichier avec champs source
- `first_names.ged` - Noms avec parties GEDCOM
- `roman_numbers.ged` - Noms avec numéros romains
- `nobility.ged` - Noms avec particules de noblesse
- `names.ged` - Fichier pour test des noms
- `surnames.ged` - Fichier pour test des noms de famille
- `negative_dates.ged` - Dates négatives
- `titles.ged` - Personnes avec titres
- `with_pictures.ged` - GEDCOM avec images
- `relations.ged` - Relations familiales
- `inconsistent_dates.ged` - Dates incohérentes
- `death_intervals.ged` - Intervalles de décès
- `custom_tags.ged` - Tags GEDCOM personnalisés
- `complex.ged` - Fichier complexe pour tests combinés
- `particles.txt` - Liste de particules

## Format des fichiers GEDCOM

Chaque fichier doit être un GEDCOM valide 5.5.1 avec des cas spécifiques pour tester les différentes options.

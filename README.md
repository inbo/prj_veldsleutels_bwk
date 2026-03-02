# prj_veldsleutels_bwk

Eenvoudige toepassing om stapsgewijs door vragen te gaan in een veldsleutel om zo tot een habitatclassificatie te komen.

## Overzicht

Dit project bevat veldsleutels voor 5 hoofdhabitattypes:

1. **Bos** (bos) - Bossleutel
2. **Grasland** (grl) - Graslandsleutel
3. **Heide** (hei) - Heidesleutel
4. **Moeras** (mrs) - Moerassleutel
5. **Water** (wtr) - Watersleutel

## Structuur

- `source/main.R` - Hoofdscript voor het genereren van HTML-sleutels
- `source/functions/` - Functies voor data parsing, HTML rendering, en logische checks
- `data/` - Data configuratie voor Google Sheets
- `*.html` - Gegenereerde HTML veldsleutels

## Gebruik

Het R-script leest data uit Google Sheets, verwerkt deze, en genereert HTML-pagina's voor elke veldsleutel. De hoofdsleutels worden gedefinieerd in `source/main.R`

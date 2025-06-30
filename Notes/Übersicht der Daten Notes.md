# Analyse der Daten

Alle grafischen Daten findet ihr im GitHub-Repo unter **Grafiken**.

Im Datensatz geht es um eine Kampagne einer Bank, die Anleihen oder etwas in der Art verkaufen möchte.  
Jeder Eintrag entspricht einem Telefonat mit einem potenziellen Kunden.  
Es gibt eine Spalte *Outcome*, die angibt, ob der Anruf erfolgreich war und zu einem Verkauf geführt hat oder nicht.

## Demografische Analyse

Im Skript `Übersicht.R` werden die Grafiken der demografischen Eigenschaften des Datensatzes erstellt  
(zu finden im Ordner **Demografisch**).  
Darauf kann man kurz eingehen und bei Bedarf Auffälligkeiten beschreiben.

## Korrelationen & Erkenntnisse

Im Skript `Zusammenhänge.R` geht es um die Korrelationen zwischen demografischen Faktoren und der Abschlussrate:

- **Alter** zeigt eine Art Parabel:
  - 18–30-Jährige sowie Personen ab 50 Jahren zeigen höhere Abschlussraten.
- **Berufsgruppen**:
  - Studenten und Rentner haben die höchsten Abschlussraten.
- **Mobiltelefone** sind effizienter als Festnetz.
- **Bildung** spielt zwar eine Rolle, aber keine entscheidende.
- **Kreditausfälle** gab es keine im Datensatz – jedoch war die Abschlussrate höher, wenn bekannt war, dass es keine gab.
- **Wohnkredite** sind relativ irrelevant.
- **Familienstand** hat einen leichten Einfluss:
  - Singles schneiden etwas besser ab.
- **Privatkredite** zeigen einen ähnlichen Effekt wie der Familienstand.

**Beobachtung:**  
In Gruppen mit höherer Abschlussrate ist die Gesprächsdauer bei erfolgreichen Anrufen im Durchschnitt niedriger – was die Effizienz dieser Gruppen zusätzlich steigert.

## Vertriebsmetriken

- **Gesprächsdauer** korreliert mit der Erfolgsrate:
  - Bei über 1000 Sekunden liegt die Abschlussrate bei ca. 50 %.
- **Wochentag** ist relativ irrelevant.
- **Previous Outcome**:
  - Wenn jemand noch nie kontaktiert wurde, ist die Erfolgsrate niedriger.
  - Personen, die schon erfolglos kontaktiert wurden, haben eine höhere Erfolgsrate.
  - Daher sollten Leads regelmäßig recycled werden.
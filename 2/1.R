#Polytome Merkmale

#rohdaten = scan(file = "../Daten/browser.txt", what = "A")

rohdaten = scan(what = "A", text = "
Firefox Firefox IE Firefox IE IE IE Firefox IE
IE Chrome IE Chrome Firefox Firefox Chrome Firefox IE
Firefox Opera Chrome IE Firefox Firefox IE Chrome IE
IE IE IE IE Chrome Safari Safari Safari IE
Safari Safari Chrome Opera Opera Chrome Chrome Opera Firefox
Firefox Firefox Chrome Firefox Chrome IE Chrome Firefox Firefox
Firefox Chrome Chrome Chrome IE Chrome
")

browser = as.factor(rohdaten) #convert into a factor
#browser

sum(is.na(browser)) #check for missing values 

browser_abs = table(browser) #convert to table
#browser_abs

sum(browser_abs) # absolute Häufigkeit

browser_abs = sort(browser_abs, decreasing = TRUE) # nicht ordinal beliebig anordnen (absteigen hier)
#browser_abs

barplot(browser_abs, main = "Browserpopularität unter Mitarbeitern der Fa. X (N = 60)")
#Balkendiagramm am besten für absolute Häufigkeiten
# Titel und die Stichprobengröße (hier: “N = 60”) immer wichtig!!


# 1. Hypothese: Alle Browser werden gleich häufig genutzt -> jeder der 5 Browser wird mit der gleichen Wahrscheinlichkeit genutzt
#p = 1/5 = 0.2   ->   Bei N = 60    ->    p x N = 12
# Deshalb Referehnzlinie:
abline(h = 12, col = "red") # Chrome, FF & IE ähnlich häufig genutzt aber häufiger als als unter Gleichverteilung zu erwarten.
#Safari/Opera deutlich weniger als zu erwarten wäre

# 2. Hypothese: vergleich mit sonstiger Nutzungshäufigkeit. tabelle 2 Browser-Statistik für W3-Schools(http://www.w3schools.com/browsers/browsers_stats.asp)
# relative Häufigkeit ( absolutze Zahlen / Stichprobengröße) -> Prozent
browser_rel = prop.table(browser_abs)
#browser_rel

# gerundete Werte von W3-Schools in selber Reihenfolge:
w3stats = c(IE = 0.07, Firefox = 0.19, Chrome = 0.68,
            Safari = 0.04, Opera = 0.02)
#w3stats

# Beiden Zahlenreihen in eine Tabelle:
browser_tab = rbind(Firma = browser_rel, W3School = w3stats)
round(browser_tab, 2)

barplot(browser_tab, beside = TRUE, legend = TRUE,
        main = paste0("Vergleich der Browser-Nutzungsdaten\n",
                      "in Firma X mit jenen der Website W3Schools"),
        sub = "N (Firma) = 60")



# NICHT VERWENDEN WEIL SCHWER VERGLEICHBAR:
pie(browser_rel, main = "Browsernutzung in Firma X (N=60)")
pie(w3stats, main = "Browsernutzung bei W3Schools-Website")




#χ2 Test

# Stichproben können nur zufällig von den erwarteten Werten abweichen. Deshalb nicht auf Grundgesamtheit übertragbar.
# Auf Firmenbelegschaft bezogen falls beobachtete Abweichungen überzufällig dann kann man die Hypothese verwerfen.
#Beispiel: 5000 Mitarbeiter mit 5 verschiedene Merkmale entsprechend den 5 Browsern. Gleichverteilung:
Grundgesamtheit = rep(1 : 5, each = 1000)
table(Grundgesamtheit)

# 5 Stichproben in Größe 60
Stichproben = t(replicate(5, table(sample(Grundgesamtheit, 60)))) # sample erzeugt zufällige Teilmenge (Länge 60) des Vektors Grundgesamtheit. Table zählt Werte und replicate wiederholt das 5 mal. Tauschen von Zeilen und Spalten mit t()
colnames(Stichproben) = colnames(browser_tab)
Stichproben


barplot(Stichproben, beside = TRUE,
        main = paste0("Fünf Zufallsstichproben eines diskret\n",
                      "gleichverteilten Merkmals (N=60)"))
abline(h = 12, col = "red")

# rein zufällige Abweichungen sind sehr erheblich.

#Ansatz: statistische Hypothesentests: Abstandsmaß zwischen beobachteten und erwarteten Werten einer Verteilung.
#Wahrscheinlichkeit ermitteln dass die kennzahl rein zufällig einen mind. so hohen Wert (Worst Case) annimt. wie in Stichprobe
# -> P-Wert ist dieser sehr gering dann ist die Abweichung kein zufall.
# beobachtete oder erwarteten Werte sind falsch? Falls Stichprobe nicht verzerrt dann sind die beobachteten Werte gültig. Erwartete Werte von Ableitung der Hypothese müssen falsch sei.
# niedriger P-Wert widerlegt Hypothese (falsifiziert) Umkehert wenn P-Wert hoch dann sind Stichprobe und Hypothese im Einklang. Das heißt nicht dass die Hypothese stimmt!! Es kann einfach nicht gegenteiliges ausgesagt werden.


#Herleitung der x62 - Statistik:
# Oi = observed und Ei = erwartete Werte
# Summe der Differenzen: Summe 1 zu K ((Oi - Ei)^2 / Ei)

#Gleichverteilung:
browser_erw_gl = rep(60 / 5, 5)
browser_erw_gl

chisq_gl = sum((browser_abs - browser_erw_gl)^2 / browser_erw_gl)
chisq_gl

#Hypothese der Gleichheit mit W3-Schools:
browser_erw_W3 = 60 * w3stats
browser_erw_W3

chisq_W3 = sum((browser_abs - browser_erw_W3)^2 / browser_erw_W3)
chisq_W3


# Der χ2-Test:
# SInd werte noch im Rahmen des Zufalls oder zu hoch?
# χ2-Statistik theor. Wahrscheinlichkeitsverteilung. R-Funktion pchisq() -> Wahrscheinlichkeit dass χ2-Statistik einen kleineren Wert als vorgegeben annimmt.
# Für P-Wert gegenwahrscheinlichkeit notwendig dass die Statistik größer als Vorgabewert ist:

# Annahme: Gleichverteilung in der Grundgesamtheit
1 - pchisq(chisq_gl, 4) # (der Wert 4 ist ein Parameter der χ2-Verteilung – in der Regel einfach K − 1).
# Wahrscheinlichkeit dafür, unsere Stichprobe (oder eine “extremere” im Sinne: von der Erwartung abweichend) zu beobachten ca. 0.003


# Annahme: Verteilung wie W3-Statistik
1 - pchisq(chisq_W3, 4)
#In der Praxis vergleicht man P-Werte gerne mit einer Grenze (dem sog. “Signifikanzniveau”) von 0.05: liegt der P-Wert darunter, sagt man, die Abweichungen seien signifikant ( ..vom Zufall verschieden.).

#Das Signifikanzniveau wird willkürlich von der Person festgelegt, die den Test durchführt, und muss daher in Berichten immer angeführt werden.

#: Bei einem Wert von 0.05 beträgt es eben höchstens 5%, da man ja nurverwirft, falls der P-Wert darunter liegt.


#Einfacher:
chisq.test(browser_abs)
chisq.test(browser_abs, p = w3stats)

#Die Warnung beim zweiten Test weist darauf hin, dass die P-Werte für die χ2-Statistik streng
#genommen nur dann stimmen, falls die erwarteten Werte für alle Kategorien mindestens 5 be-
#tragen (was nicht auf alle Browser zutrifft). In solchen Fällen sollte man mit Schlussfolgerungen
#vorsichtig sein und beispielsweise versuchen, eine größere Stichprobe zu erhalten.

#(In diesem konkreten Fall ist allerdings die Abweichung so stark, dass das Problem nicht relevant ist.) Der
#etwas seltsame P-Wert liest sich: 6.67 × 10−15, also so gut wie 0 (dies ergibt sich manchmal aufgrund von Rundungsfehlern).




#Zusammenfassung: Durchführung eines statistischen Tests
#Um einen Hypothesentest durchzuführen, sind folgende Schritte notwendig:
#  1. Aufstellen von Null- und Alternativhypothese:
#  Die inhaltliche Fragestellung (z.B.: “Sind alle Browser gleich beliebt?”) wird in eine statisti-
#  sche übertragen (“Das Merkmal Browser folgt einer Gleichverteilung”). Die Alternative ist
#  meist die Negation der Aussage (“Das Merkmal Browser folgt keiner Gleichverteilung”).
# 2. Auswahl des Testvefahrens:
#  Im Beispiel der χ2-Test.
# 3. Festlegung des Signifikanzniveaus:
#  Meistens 0.05, andere typische Werte: 0.01 oder 0.1.
# 4. Ermittlung der Test-Statistik und ihres P-Werts:
#  Im Beispiel ist das die χ2-Statistik, deren P-Wert mit Hilfe der χ2-Verteilung ausgerechnet
#  wird. Die R-Funktion chisq.test() erledigt beides auf einmal.
# 5. Formaler Schluss:
#  Nun wird der P-Wert mit dem Signifikzniveau verglichen:
#  a) P-Wert kleiner als Signifikanzniveau: Die Nullhypothese wird verworfen. (Die Stich-
#                                                                                probe spricht gegen die Annahme über die Grundgesamtheit).
#  b) P-Wert größer (oder gleich) Signifikanzniveau: Die Nullhypothese wird beibehalten.
#  (Die Daten sprechen nicht gegen die Annahme über die Grundgesamtheit, beweisen
#  aber nicht deren Gültigkeit).
# 6. Inhaltliche Interpretation:
#  Übertragung des formalen Schlusses auf die inhaltliche Fragestellung, z.B.: “Die Ergeb-
#  nisse der Umfrage sprechen dagegen, dass unter allen Mitarbeitern alle Browser gleich
#beliebt sind.”)








# Gruppen- und Paarvergleiche
#oftwareentwicklungsfirma wird die Bugfixdauer für zwei Programmierer getrennt erhoben

meyer = scan(text = "44 37 36 42 47 36 39")
eckkrammer = scan(text = "35 34 29 46 34 21 29")

boxplot(meyer, eckkrammer, names = c("meyer", "eckkrammer"), horizontal = TRUE, main = "Vergleich der Bugfixdauer (in Min.)\n zweier Programmierer")

# An der relativen Position der Boxen, vor allem der Mediane (schwarzer Strich), ist abzulesen, dass sich die Lage der beiden Stichproben deutlich voneinander unterscheidet.

#Die genauen Kennzahlen erhalten wir mit den 5-Punkt-Zusammenfassungen:
summary(meyer)
sd(meyer)

summary(eckkrammer)
sd(eckkrammer)

#Die Mediane unterscheiden sich um 5 Minuten, die Mittelwerte sogar um rd. 7.5 Minuten.

#Der Mittelwert: wird berechnet, indem alle Werte summiert werden und danach die Summe durch die Anzahl der Werte dividiert wird.
#Der Median: kann berechnet werden, indem alle Zahlen in aufsteigender Reihenfolge aufgelistet werden und dann die Zahl in der Mitte dieser Verteilung ausgewählt wird.


# Der 2-Stichproben-t-Test

# Der Unterschoed bezieht sich auf die Stichproben. Besteht der Unterschied auch tatsächlich in der Grudngesamtheit?
# Falls beide gleich schnell dürfen sich die Stichproben nur zufällig voneinander unterscheiden.
# Daher kann mann Annahme (kein Unterschied zwischen Meyer und Eckkrammer) als widerlegt ansehenfalls die Abweichung so groß ist dass sie nicht zufällig zustande kommen konnte (bzw sehr unwahrscheinlich)
# die Stichproben wären quasi ein starker Gegenbeweis für unsere Annahme.

# 2-Stichproben-t-Test.
#Die Nullhypothese lautet, dass die Differenz der Gruppenmittel in der Grundgesamtheit 0 ist (Alternative: ungleich 0)
# Test ermitteln die Wahrscheinlichkeit dass beide Gruppen min so stark abweichen die die errechnete Differenz in beiden Stichproben (P-Wert)
# Ist diese sehr gering (unter Signifikanzniveau  0.05 ) verwerfen wir die Nullhypothese und müssen davon ausgehen dass beide Programmierer unterschiedlich sind.

# Falls nicht liefern die Daten keinen Hinweis dafür dass beide Programmierer unterschiedlich schnell sind.

t.test(meyer, eckkrammer)

# Der P-Wert (0.0467) liegt tatsächlich knapp unter 0.05, womit wir unterschiedliche Geschwindigkeiten beim Beheben von Bugs bei den beiden Programmierern nachgewiesen haben.




#Ein verwandtes Problem tritt auf, wenn Objekte oder Personen mehrfach gemessen werden,
#z.B. das Gewicht von Personen vor und nach einer Diät, oder die Arbeitsgeschwindigkeit von
#Mitarbeitern vor und nach einer Schulung oder der Einführung eines neuen IT-Systems, oder
#die getrennte Beurteilung von Klausuren durch jeweils zwei unabhängige Prüfer.
#In diesen Fällen könnte man zwar ebenfalls den 2-Stichproben-t-Test anwenden, jedoch würde man die
#Information “verschenken”, dass jeweils 2 Messungen von der selben Person bzw. der selben
#Klausur stammen. Ist dies nämlich bekannt, kann ein genaueres Verfahren angewandt werden:
#der gepaarte t-Test.

#Dieser kann vorhandene Unterschiede eher aufdecken als der “normale” 2-Stichproben-t-Test (natürlich nur, falls diese auch existieren)


#Nimmt man an dass beide Programmieren unabhängig dieselben Bugs gefixed haben liegen für jeden Bug (=beobachtungseinheit) zwei Messungen vor. Für jeden eine.
#Man betrachtet bnun die Differenzen der Dauer für jeden Bug

#Damit neutralisiert man den Effekt, dass jeder Bug grundsätzlich schwerer oder leichter zu fixen ist - bei einem
#schweren Bug werden beide Programmierer länger benötigen als bei einem leichten Bug. Die
#Differenz gibt dann wirklich nur die unterschiedliche Arbeitsgeschwindigkeit bei der beiden Pro-
#grammierern wieder. Bei einem 2-Stichproben-Test wären beide Effekte vermischt und würden
#das Ergebnis “verwaschen”. 


boxplot(meyer - eckkrammer, horizontal = TRUE)

#Zu erkennen ist, dass die Differenzen (speziell der Median) deutlich von 0 abweichen.

#Um einen Test durchzuführen, bildet man nun den Mittelwert diese Differenzen und führt
#einen klassischen 1-Stichproben-t-Test durch. Falls wir wieder annehmen, dass die Program-
#mierer gleich schnell sind, müsste diese Teststatistik bestenfalls zufällig von 0 abweichen.

t.test(meyer, eckkrammer, paired = TRUE)

#Der P-Wert (0.02) ist kleiner als das Signifikanzniveau von 0.05, also auch hier wird die An-
#nahme gleicher Geschwindigkeiten verworfen. Hätte man den nicht-gepaarten Test verwendet, wäre das Ergebnis allerdings deutlich knapper ausgefallen.



#Mittels wilcox.test() kann man sowohl den 2-Stichproben-Test wie auch den gepaar-
#ten Test in einer robusten Variante durchführen, welche wieder auf den Rängen statt auf
#den Orginalwerten basiert. Die Aufrufe sind analog zu t.test()

wilcox.test(meyer, eckkrammer)
wilcox.test(meyer, eckkrammer, paired = TRUE)
# Baseline Model

Für die Einträge in *filtered_data* mit Daten zwischen 2013-07-01 und 2013-07-31 wurde ein lineares Modell aufgestellt, welches die Variable *Umsatz* durch eine einfache Linearkombination der Variablen *Bewoelkung*, *Temperatur*, *Windgeschwindigkeit*, *Wettercode*, *Warengruppe*, *KielerWoche*, *holstein_spiel*, *thw_spiel*, *Ferien* und *flohmarkt* vorhersagt (keine Interaktionseffekte, die Variable *Feiertag* ist fehlerhaft).

Das lineare Modell ist signifikant (p \< e-15), wobei das adjustiertes R\^2 .06836 besträgt. Die Faktoren *Temperatur*, *Warengruppe*, *KielerWoche*, *holstein_spiel* und *Ferien* sind signifikant (die Variable *flohmarkt* hat ein p-Wert von ca. .011).

In der Regressionsdiagnostik weißt das Modell einen leichten Zusammenhang zwischen den vorhergesagten Werten und den Residuen auf. Weiterhin legt der Q-Q-Plot der Residuen eine deutliche Abweichung von der Normalverteilung nahe. Insgesamt ist die Qualität der Vorhersage durch das Modell sehr gering.

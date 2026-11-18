# ========================================================================
# TP données manquantes - Analyse sur cas complets
# ========================================================================
#
# Énoncé et explications : page « Analyse sur cas complets » du TP en ligne
# https://eusebe.github.io/missing/cc.html
#
# Exécutez le code ligne par ligne avec Ctrl+Entrée (Cmd+Entrée sur Mac).
# Vos modifications sont gardées dans votre navigateur.
# La correction est dans correction/correction.R.
# ========================================================================

## Préparation (déjà faite dans la page web)
aq <- airquality
aq$Month <- factor(aq$Month)
aq$R <- as.numeric(complete.cases(aq[, c("Ozone", "Solar.R", "Wind", "Temp")]))

# Ajustez le modèle linéaire expliquant Ozone par Solar.R, Wind et Temp,
# stockez-le dans un objet fit_cc, et affichez ses résultats avec summary().
## À vous :



# Combien d'observations ont réellement été utilisées pour ajuster le
# modèle ? (indice : nobs())
## À vous :



# Affichez les intervalles de confiance à 95 % des coefficients (indice :
# confint()).
## À vous :




# ========================================================================
# TP données manquantes - Imputation multiple avec mice
# ========================================================================
#
# Énoncé et explications : page « Imputation multiple avec mice » du TP en ligne
# https://eusebe.github.io/missing/mice.html
#
# Exécutez le code ligne par ligne avec Ctrl+Entrée (Cmd+Entrée sur Mac).
# Vos modifications sont gardées dans votre navigateur.
# La correction est dans correction/correction.R.
# ========================================================================

## Préparation (déjà faite dans la page web)
library(mice)
aq <- airquality
aq$Month <- factor(aq$Month)


# ------------------------------------------------------------------------
# Étape 1 : imputer
# ------------------------------------------------------------------------

# Créez une data.frame dat contenant les variables Ozone, Solar.R, Wind, Temp
# et Month.
## À vous :



# Lancez l'imputation avec mice() : 20 imputations (m = 20), une graine pour
# la reproductibilité (seed = 123), et printFlag = FALSE pour ne pas afficher
# le détail des itérations. Stockez le résultat dans un objet imp, puis
# affichez imp$method.
## À vous :




# ------------------------------------------------------------------------
# Vérifier les imputations
# ------------------------------------------------------------------------

# Vérifiez la convergence de l'algorithme avec plot(imp). On attend des
# courbes qui se mélangent, sans tendance.
## À vous :



# Comparez la distribution des valeurs observées (en bleu) et imputées (en
# rouge) avec densityplot(imp).
## À vous :




# ------------------------------------------------------------------------
# Étapes 2 et 3 : analyser et combiner
# ------------------------------------------------------------------------

# Ajustez le modèle linéaire sur chacun des 20 jeux imputés avec with(imp,
# lm(...)), stockez le résultat dans fit_mi, puis combinez les résultats avec
# pool() et affichez-les avec summary(..., conf.int = TRUE).
## À vous :



# Affichez les colonnes term, ubar, b, t et fmi de pooled$pooled.
## À vous :




# ------------------------------------------------------------------------
# Une fraction d'information manquante élevée ?
# ------------------------------------------------------------------------


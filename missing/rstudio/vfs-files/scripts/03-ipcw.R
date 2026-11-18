# ========================================================================
# TP données manquantes - Pondération IPCW
# ========================================================================
#
# Énoncé et explications : page « Pondération IPCW » du TP en ligne
# https://eusebe.github.io/missing/ipcw.html
#
# Exécutez le code ligne par ligne avec Ctrl+Entrée (Cmd+Entrée sur Mac).
# Vos modifications sont gardées dans votre navigateur.
# La correction est dans correction/correction.R.
# ========================================================================

## Préparation (déjà faite dans la page web)
library(sandwich)
library(lmtest)
aq <- airquality
aq$Month <- factor(aq$Month)
aq$R <- as.numeric(complete.cases(aq[, c("Ozone", "Solar.R", "Wind", "Temp")]))
fit_cc <- lm(Ozone ~ Solar.R + Wind + Temp, data = aq)


# ------------------------------------------------------------------------
# Étape 1 : modéliser la probabilité d'être complet
# ------------------------------------------------------------------------

# Ajustez un modèle de régression logistique expliquant R par Wind, Temp et
# Month sur l'ensemble des 153 jours. Stockez-le dans un objet mod_R et
# affichez ses résultats.
## À vous :




# ------------------------------------------------------------------------
# Étape 2 : calculer les poids
# ------------------------------------------------------------------------

# Calculez pour chaque jour la probabilité estimée d'être complet (indice :
# predict(..., type = "response")), stockez-la dans aq$p, puis calculez les
# poids aq$w = 1/aq$p.
## À vous :




# ------------------------------------------------------------------------
# Étape 3 : vérifier les poids
# ------------------------------------------------------------------------

# Décrivez la distribution des poids chez les cas complets (summary() et
# hist()), puis calculez la taille d'échantillon effective n_{eff} = (sum
# w_i)^2 / sum w_i^2.
## À vous :



# Comparez la répartition des mois dans l'échantillon entier, chez les cas
# complets non pondérés, et chez les cas complets pondérés. Le code
# ci-dessous n'a pas besoin d'être modifié (il utilise les poids aq$w
# calculés à l'étape 2).
cc <- aq$R == 1
round(cbind(
  echantillon = prop.table(table(aq$Month)),
  cas_complets = prop.table(table(aq$Month[cc])),
  cas_complets_ponderes = tapply(aq$w[cc], aq$Month[cc], sum) / sum(aq$w[cc])
), 3)


# ------------------------------------------------------------------------
# Étape 4 : ajuster le modèle d'analyse pondéré
# ------------------------------------------------------------------------

# Ajustez le modèle linéaire pondéré (option weights = w de lm()) et
# stockez-le dans fit_ipcw. Affichez ensuite les coefficients avec des
# erreurs standard robustes : coeftest(fit_ipcw, vcov = vcovHC(fit_ipcw, type
# = "HC0")).
## À vous :



# Comparez les coefficients obtenus avec ceux de l'analyse sur cas complets
# (coef(fit_cc)).
## À vous :




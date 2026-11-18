# ========================================================================
# TP données manquantes - Comparaison des trois approches
# ========================================================================
#
# Énoncé et explications : page « Comparaison des trois approches » du TP en ligne
# https://eusebe.github.io/missing/synthese.html
#
# Exécutez le code ligne par ligne avec Ctrl+Entrée (Cmd+Entrée sur Mac).
# Vos modifications sont gardées dans votre navigateur.
# La correction est dans correction/correction.R.
# ========================================================================

## Préparation (déjà faite dans la page web)
library(mice)
library(sandwich)
library(lmtest)
aq <- airquality
aq$Month <- factor(aq$Month)
aq$R <- as.numeric(complete.cases(aq[, c("Ozone", "Solar.R", "Wind", "Temp")]))
fit_cc <- lm(Ozone ~ Solar.R + Wind + Temp, data = aq)
mod_R <- glm(R ~ Wind + Temp + Month, data = aq, family = binomial)
aq$w <- 1 / predict(mod_R, type = "response")
fit_ipcw <- lm(Ozone ~ Solar.R + Wind + Temp, data = aq, weights = w)
imp <- mice(aq[, c("Ozone", "Solar.R", "Wind", "Temp", "Month")],
            m = 20, seed = 123, printFlag = FALSE)
pooled <- pool(with(imp, lm(Ozone ~ Solar.R + Wind + Temp)))

# Lancez le code ci-dessous, qui rassemble les coefficients et leurs erreurs
# standard dans un même tableau.
se_ipcw <- sqrt(diag(vcovHC(fit_ipcw, type = "HC0")))  # erreurs standard robustes
res_mi <- summary(pooled)

round(data.frame(
  coef_cc   = coef(fit_cc),
  se_cc     = coef(summary(fit_cc))[, "Std. Error"],
  coef_ipcw = coef(fit_ipcw),
  se_ipcw   = se_ipcw,
  coef_mi   = res_mi$estimate,
  se_mi     = res_mi$std.error
), 3)

# Répondez aux questions suivantes avant de dérouler les réponses.
# 1. Les trois approches donnent-elles des conclusions différentes sur
# l'effet du rayonnement solaire, du vent et de la température ?
#
# Votre réponse :
#

# 2. Pourquoi l'erreur standard du coefficient de Wind est-elle plus grande
# avec l'IPCW ?
#
# Votre réponse :
#

# 3. L'imputation multiple utilise 153 jours au lieu de 111. Pourquoi ses
# erreurs standard ne sont-elles pas beaucoup plus petites que celles de
# l'analyse sur cas complets ?
#
# Votre réponse :
#

# 4. Sur quelle hypothèse reposent ces trois analyses, et peut-on la
# vérifier ?
#
# Votre réponse :
#


# ========================================================================
# Script de correction : TP donnees manquantes
# ========================================================================

library(mice)
library(sandwich)
library(lmtest)

## Donnees fournies avec R
aq <- airquality
aq$Month <- factor(aq$Month)


# ========================================================================
# Les donnees et leurs valeurs manquantes
# ========================================================================

# --- Présentation des données

# Vous n'avez rien besoin d'écrire ici, appuyez juste sur le bouton 'Run Code'
# ou sur le raccourci clavier Ctrl+Enter
head(aq)
summary(aq)

# --- Combien de données manquantes ?

colSums(is.na(aq))
## Seules Ozone (37 NA) et Solar.R (7 NA) ont des valeurs manquantes

sum(complete.cases(aq))   # nombre de cas complets
mean(complete.cases(aq))  # proportion de cas complets
## Une analyse sur cas complets n'utiliserait que 111 jours sur 153 (73 %)

md.pattern(aq, rotate.names = TRUE)

# --- Le mécanisme est-il MCAR ?

aq$R <- as.numeric(complete.cases(aq[, c("Ozone", "Solar.R", "Wind", "Temp")]))
table(aq$R)

prop.table(table(aq$Month, aq$R), margin = 1)
## En juin, seuls 30 % des jours sont complets (contre 74 à 97 % les autres mois) :
## la probabilité d'être complet dépend du mois, qui est observé.
## Les données ne sont donc pas MCAR.

tapply(aq$Temp, aq$R, mean)
tapply(aq$Wind, aq$R, mean)
## Pas de différence notable sur la température et le vent


# ========================================================================
# Analyse sur cas complets
# ========================================================================

fit_cc <- lm(Ozone ~ Solar.R + Wind + Temp, data = aq)
summary(fit_cc)
## Remarquez la mention "(42 observations deleted due to missingness)"

nobs(fit_cc)
## 111 jours sur 153 : on a perdu 27 % de l'échantillon, y compris les 5 jours
## où l'ozone était observé mais pas le rayonnement solaire

confint(fit_cc)


# ========================================================================
# Ponderation IPCW
# ========================================================================

# --- Étape 1 : modéliser la probabilité d'être complet

mod_R <- glm(R ~ Wind + Temp + Month, data = aq, family = binomial)
summary(mod_R)
## Le modèle est ajusté sur tous les jours (R, Wind, Temp et Month sont
## observés pour tous). Seul le mois de juin est fortement associé à R.

# --- Étape 2 : calculer les poids

aq$p <- predict(mod_R, type = "response")
aq$w <- 1 / aq$p
head(aq[, c("Month", "R", "p", "w")])

# --- Étape 3 : vérifier les poids

w_cc <- aq$w[aq$R == 1]   # poids des cas complets
summary(w_cc)
hist(w_cc, breaks = 20, main = "Poids IPCW des cas complets", xlab = "Poids")
sum(w_cc)^2 / sum(w_cc^2) # taille d'échantillon effective
## Le poids maximal est inférieur à 4 (ce sont les jours complets de juin) :
## pas de poids extrême. La taille effective (~94) est assez proche du
## nombre de cas complets (111).

cc <- aq$R == 1
round(cbind(
  echantillon = prop.table(table(aq$Month)),
  cas_complets = prop.table(table(aq$Month[cc])),
  cas_complets_ponderes = tapply(aq$w[cc], aq$Month[cc], sum) / sum(aq$w[cc])
), 3)

# --- Étape 4 : ajuster le modèle d'analyse pondéré

fit_ipcw <- lm(Ozone ~ Solar.R + Wind + Temp, data = aq, weights = w)
## lm() exclut toujours les jours incomplets : seuls les cas complets,
## pondérés, contribuent à l'estimation
coeftest(fit_ipcw, vcov = vcovHC(fit_ipcw, type = "HC0"))

round(cbind(cas_complets = coef(fit_cc), ipcw = coef(fit_ipcw)), 3)
## Les estimations sont proches : le mois, qui explique la non-réponse,
## semble peu lié à l'ozone une fois pris en compte le rayonnement,
## le vent et la température.


# ========================================================================
# Imputation multiple avec mice
# ========================================================================

# --- Étape 1 : imputer

dat <- aq[, c("Ozone", "Solar.R", "Wind", "Temp", "Month")]
## Day est exclu : sinon mice l'utiliserait comme prédicteur

imp <- mice(dat, m = 20, seed = 123, printFlag = FALSE)
imp$method
## Par défaut, les variables continues sont imputées par Predictive Mean
## Matching ("pmm"). C'est adapté ici : l'ozone est positif et sa distribution
## est asymétrique, et pmm n'impute que des valeurs déjà observées.
## Les variables complètes ("") ne sont pas imputées.

# --- Vérifier les imputations

plot(imp)
## Moyenne et écart-type des valeurs imputées au fil des itérations,
## pour chacune des 20 imputations : pas de tendance, convergence correcte.

densityplot(imp)
## Les distributions imputées sont plausibles. Elles n'ont pas à être
## identiques aux distributions observées : sous MAR, les jours incomplets
## peuvent différer des jours complets.

# --- Étapes 2 et 3 : analyser et combiner

fit_mi <- with(imp, lm(Ozone ~ Solar.R + Wind + Temp))
pooled <- pool(fit_mi)
summary(pooled, conf.int = TRUE)
## Ces estimations utilisent les 153 jours, y compris ceux qui n'ont qu'une
## seule variable manquante

pooled$pooled[, c("term", "ubar", "b", "t", "fmi")]
## Selon les coefficients, environ 20 à 35 % de l'information est perdue
## à cause des données manquantes


# ========================================================================
# Comparaison des trois approches
# ========================================================================

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


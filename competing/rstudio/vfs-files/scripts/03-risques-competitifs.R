# ========================================================================
# TP survie causale et risques compétitifs - Partie 2 : risques compétitifs et estimands
# ========================================================================
#
# Énoncé et explications : page « Partie 2 : risques compétitifs et estimands » du TP en ligne
# https://eusebe.github.io/competing/partie2.html
#
# Exécutez le code ligne par ligne avec Ctrl+Entrée (Cmd+Entrée sur Mac).
# Vos modifications sont gardées dans votre navigateur.
# La base df.csv est dans le dossier de travail.
# La correction est dans correction/correction.R.
# ========================================================================

## Préparation (déjà faite dans la page web)
library(survival)
library(dplyr)
df <- read.csv("df.csv")
df_base <- df |>
  group_by(id) |>
  summarise(A = first(A), L1 = first(L1), L2 = first(L2), L3 = first(L3), L4 = first(L4),
            time = last(k), Y = last(Y), D = last(D), C = last(C)) |>
  ungroup()
haz_mod_Y <- glm(Y ~ A + k + I(k^2) + I(k^3) + A:k + A:I(k^2) + L1 + L2 + L3 + L4,
                 data = subset(df, D == 0 & C == 0), family = binomial)


# ------------------------------------------------------------------------
# Objectif
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# La forme de l'effet du temps est un choix de spécification
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Étape 1 - CIF naïve ou CIF correcte ?
# ------------------------------------------------------------------------

# Construisez la variable de statut à 3 niveaux (censore, evenement, deces),
# puis comparez l'incidence cumulée de Y obtenue en traitant D comme une
# censure (naïve) à celle obtenue par l'estimateur d'Aalen-Johansen (qui
# prend en compte D comme risque compétitif).
## Statut a 3 niveaux : "censore" (ni Y ni D), "evenement" (Y=1), "deces" (D=1)
## Le premier niveau du facteur doit etre l'etat de reference (censure)

## CIF naive : survfit(Surv(time, Y) ~ 1, data = df_base) puis 1 - survie
## CIF correcte : survfit(Surv(time, statut) ~ 1, data = df_base) (Aalen-Johansen)


# ------------------------------------------------------------------------
# Étape 2 - Estimand 1 : effet total
# ------------------------------------------------------------------------

# Ajustez le modèle de hazard de D (D ~ A + k + I(k^2) + L1 + L2 + L3 + L4,
# sur les lignes non censurées), puis écrivez une fonction gcomp_CIF(a_Y,
# a_D) qui prédit et chaîne les deux hazards pour chaque individu (sous A=a_Y
# pour le modèle de Y, sous A=a_D pour le modèle de D), et retourne la CIF
# moyenne à 36 mois.
## Modele de hazard de D : D ~ A + k + I(k^2) + L1 + L2 + L3 + L4, sur les lignes
## non censurees (subset(df, C == 0))

## gcomp_CIF <- function(a_Y, a_D) { ... }
## Chainer les deux hazards prediction par prediction, comme dans la formule
## ci-dessus, individu par individu, puis moyenner


# ------------------------------------------------------------------------
# Étape 3 - Estimand 2 : risque net
# ------------------------------------------------------------------------

# Écrivez cette version simplifiée, gcomp_CIF0(a_Y), qui ne modélise jamais
# D.
## gcomp_CIF0 <- function(a_Y) { comme gcomp_CIF, mais hazD force a 0 partout }
## À vous :



# Comparez cette différence de risque net à la différence d'effet total
# obtenue à l'Étape 2 : sont-elles identiques ?

# ------------------------------------------------------------------------
# Étape 4 - Estimand 3 : effet séparable
# ------------------------------------------------------------------------

# Avec gcomp_CIF, calculez gcomp_CIF(1, 0) - gcomp_CIF(0, 0) : le hazard de D
# reste fixé à A=0 dans les deux termes, seul celui de Y change de bras.
## gcomp_CIF(1, 0) - gcomp_CIF(0, 0)
## À vous :




# ------------------------------------------------------------------------
# Récapitulatif
# ------------------------------------------------------------------------

# Rassemblez les trois différences de risque à 36 mois (effet total, risque
# net, effet séparable) dans un seul tableau.

# ------------------------------------------------------------------------
# Discussion
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Ce qu'il faut retenir
# ------------------------------------------------------------------------


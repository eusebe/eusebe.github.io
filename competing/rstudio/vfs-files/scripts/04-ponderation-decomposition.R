# ========================================================================
# TP survie causale et risques compétitifs - Partie 3 : pondération (IPW) et décomposition
# ========================================================================
#
# Énoncé et explications : page « Partie 3 : pondération (IPW) et décomposition » du TP en ligne
# https://eusebe.github.io/competing/partie3.html
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
K <- 36


# ------------------------------------------------------------------------
# Objectif
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Étape 1 - Modèle de censure et poids IPCW
# ------------------------------------------------------------------------

# Modélisez Pr(C=1 | A, L) et calculez le poids IPCW cumulé (poids « effet
# total »).

# ------------------------------------------------------------------------
# Rappel : restreindre le modèle de censure à la période à risque
# ------------------------------------------------------------------------

## Modele de censure, restreint a la periode reellement a risque (k > 50) :
## C ~ A + L1 + L2 + L3 + L4, sur df
## Poids IPCW cumule (produit de 1/(1-p_cens) periode par periode, par individu)
## À vous :




# ------------------------------------------------------------------------
# Variante « classique » : censure uniforme sur tout le suivi
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Étape 2 - Poids « risque net » (censure × D)
# ------------------------------------------------------------------------

# Modélisez Pr(D=1 | A, L, k) (sur les lignes non censurées, sans exclure
# D=1 : c'est lui l'évènement modélisé), avec le même choix de dépendance
# temporelle qu'en Partie 2 (polynôme en k), et calculez w_net = w_total *
# 1/cumPredO où cumPredO est le produit cumulé de 1-Pr(D=1 | cdot).
## Modele : D ~ A + k + I(k^2) + L1 + L2 + L3 + L4, sur subset(df, C == 0)
## w_net <- w_total * 1 / cumPredO
## À vous :




# ------------------------------------------------------------------------
# Pourquoi « censure × D » et pas seulement D ?
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Étape 3 - Effet total et risque net, par IPW
# ------------------------------------------------------------------------

# Écrivez une fonction nonparam_hazard(w, a, event) qui calcule le hazard non
# paramétrique pondéré de event ("Y" ou "D") pour le bras a, et une fonction
# chain_CIF(hazY, hazD) qui chaîne ces hazards en CIF (comme dans la formule
# de la Partie 2). Utilisez-les pour calculer l'effet total (avec w_total,
# hazD inclus) et le risque net (avec w_net, sans hazD).
## nonparam_hazard <- function(w, a, event) { ... }
## chain_CIF <- function(hazY, hazD = NULL) { ... }
## À vous :




# ------------------------------------------------------------------------
# Étape 4 - Effet séparable, par IPW
# ------------------------------------------------------------------------

## predO_aD0 <- 1 - predict(modD, newdata = transform(df, A = 0), type = "response")
## predO_aY1 <- 1 - predict(modD, newdata = transform(df, A = 1), type = "response")
## w_sep <- (cumprod(predO_aD0) / cumprod(predO_aY1)) * w_total  (cumul par individu)
## À vous :




# ------------------------------------------------------------------------
# Étape 5 - Comparer g-formula et IPW
# ------------------------------------------------------------------------

# Comparez les trois différences de risque obtenues ici (IPW) à celles
# obtenues en Partie 2 (g-formula).

# ------------------------------------------------------------------------
# Étape 6 - Décomposer l'effet total en direct + indirect
# ------------------------------------------------------------------------

# Calculez gcomp_CIF(0, 1), puis les deux décompositions :
# gcomp_CIF(1,0)-gcomp_CIF(0,0) + gcomp_CIF(1,1)-gcomp_CIF(1,0), et
# gcomp_CIF(0,1)-gcomp_CIF(0,0) + gcomp_CIF(1,1)-gcomp_CIF(0,1). Vérifiez
# qu'elles tombent toutes deux exactement sur l'effet total de la Partie 2.

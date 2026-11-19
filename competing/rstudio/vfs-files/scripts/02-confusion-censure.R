# ========================================================================
# TP survie causale et risques compétitifs - Partie 1 : confusion et censure dépendante
# ========================================================================
#
# Énoncé et explications : page « Partie 1 : confusion et censure dépendante » du TP en ligne
# https://eusebe.github.io/competing/partie1.html
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


# ------------------------------------------------------------------------
# Objectif
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Une simplification provisoire
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Étape 1 - Score de propension et poids IPTW
# ------------------------------------------------------------------------

# Estimez le score de propension e(L) = Pr(A=1 | L) par régression logistique
# (sur L1 + L2 + L3 + L4), puis calculez les poids IPTW pour l'ATE.
## Modele de score de propension : A ~ L1 + L2 + L3 + L4
## Poids IPTW : 1 / e(L) si A=1, 1 / (1-e(L)) si A=0
## À vous :




# ------------------------------------------------------------------------
# Étape 2 - Kaplan-Meier pondéré (IPTW)
# ------------------------------------------------------------------------

# Calculez le Kaplan-Meier pondéré par w_iptw (toujours en traitant D et C
# comme des censures), et comparez la survie à 36 mois entre les deux
# groupes.
## survfit(..., weights = df_base$w_iptw)
## À vous :




# ------------------------------------------------------------------------
# Étape 3 - G-formula paramétrique
# ------------------------------------------------------------------------

# Ajustez un modèle logistique poolé sur les lignes de df où l'individu est
# encore à risque de Y, c'est-à-dire sans D ni C (subset(df, D == 0 & C ==
# 0)) : Y ~ A * k + L1 + L2 + L3 + L4.
## Modele de hazard poole : Y ~ A * k + L1 + L2 + L3 + L4,
## sur les lignes encore a risque de Y : subset(df, D == 0 & C == 0)
## À vous :



# Pour chaque individu, prédisez son hazard à chaque période sous A=1 puis
# sous A=0, et chaînez les (1-hazard) pour obtenir sa survie contrefactuelle
# individuelle à 36 mois. Moyennez ensuite sur tous les individus.
## Pour k = 1, ..., 36 : predire le hazard sous A=1 et sous A=0 pour chaque
## individu de df_base (avec ses valeurs de L1-L4), puis cumuler prod(1 - hazard)
## À vous :




# ------------------------------------------------------------------------
# Étape 4 - Censure dépendante : IPCW
# ------------------------------------------------------------------------

# Modélisez Pr(C=1 | A, L) par régression logistique poolée, puis calculez le
# poids IPCW cumulé pour chaque période de suivi.

# ------------------------------------------------------------------------
# Sur quelle période ajuster le modèle de censure ?
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# À 36 mois, les poids IPCW valent tous 1
# ------------------------------------------------------------------------

## Modele de censure, restreint a la periode reellement a risque (ici k > 50 :
## personne n'est censure avant, cf. range(df$k[df$C == 1]))
## Poids IPCW a la periode k pour l'individu i :
##   produit, de la 1ere periode a k, de 1 / (1 - p_C(A_i, L_i))
## À vous :




# ------------------------------------------------------------------------
# Variante « classique » : censure uniforme sur tout le suivi
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Étape 5 - Combiner IPTW et IPCW
# ------------------------------------------------------------------------

# Multipliez le poids IPTW (fixe, calculé à l'inclusion) et le poids IPCW
# (période par période), puis calculez le Kaplan-Meier pondéré sur les
# données au format « compteur » (Surv(k-1, k, Y)).
## Fusionner w_iptw (df_base) dans df, calculer le poids total = w_iptw * w_ipcw
## puis survfit(Surv(k-1, k, Y) ~ A, data = df, weights = poids_total)
## À vous :




# ------------------------------------------------------------------------
# Discussion - comparaison des trois approches
# ------------------------------------------------------------------------

# Comparez la différence de survie à 36 mois (S^(a=1)(36) - S^(a=0)(36))
# obtenue par l'analyse brute (Présentation), l'IPTW seul, la g-formula, et
# IPTW+IPCW combinés.

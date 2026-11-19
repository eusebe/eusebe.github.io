# ========================================================================
# TP survie causale et risques compétitifs - Présentation des données
# ========================================================================
#
# Énoncé et explications : page « Présentation des données » du TP en ligne
# https://eusebe.github.io/competing/presentation.html
#
# Exécutez le code ligne par ligne avec Ctrl+Entrée (Cmd+Entrée sur Mac).
# Vos modifications sont gardées dans votre navigateur.
# La base df.csv est dans le dossier de travail.
# La correction est dans correction/correction.R.
# ========================================================================

## Préparation (déjà faite dans la page web)
library(survival)
library(dplyr)
library(ggplot2)
df <- read.csv("df.csv")


# ------------------------------------------------------------------------
# Présentation
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Chargement et exploration
# ------------------------------------------------------------------------

# Chargez les données et affichez les premières lignes. Vérifiez les
# dimensions de la base, le nombre d'individus, et le nombre total
# d'évènements Y et D.
library(survival)
library(dplyr)
df <- read.csv("df.csv")

## Affichez les premières lignes, les dimensions, le nombre d'individus,
## le nombre de lignes où Y = 1 et le nombre de lignes où D = 1


# ------------------------------------------------------------------------
# Format person-period
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Construire une base à une ligne par individu
# ------------------------------------------------------------------------

# Créez df_base contenant : id, A, L1-L4 (valeurs à l'inclusion, constantes
# pour chaque individu), time (dernière valeur de k, c'est-à-dire le nombre
# de mois suivis), Y, D, C (statuts à la dernière période observée).
## Créez df_base : une ligne par individu, avec A, L1-L4 (à l'inclusion),
## time (derniere valeur de k), et les statuts finaux Y, D, C
## À vous :




# ------------------------------------------------------------------------
# Analyse brute (non ajustée)
# ------------------------------------------------------------------------

# Tracez les courbes de Kaplan-Meier de l'évènement Y selon A (traitez D et C
# comme des censures pour l'instant), puis extrayez la survie estimée à 36
# mois dans chaque groupe.
## Surv(time, Y) : Y = 1 si evenement d'interet, 0 sinon (censure, y compris D et C
## pour l'instant : c'est une simplification qu'on lèvera en Partie 2)
## À vous :



# Que constatez-vous ? Cette différence brute a-t-elle une interprétation
# causale ?

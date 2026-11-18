# ========================================================================
# TP données manquantes - Les données et leurs valeurs manquantes
# ========================================================================
#
# Énoncé et explications : page « Les données et leurs valeurs manquantes » du TP en ligne
# https://eusebe.github.io/missing/intro.html
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
# Présentation des données
# ------------------------------------------------------------------------

# Lancez le code ci-dessous pour avoir un premier aperçu des données.
# Vous n'avez rien besoin d'écrire ici, appuyez juste sur le bouton 'Run Code'
# ou sur le raccourci clavier Ctrl+Enter
head(aq)
summary(aq)


# ------------------------------------------------------------------------
# Combien de données manquantes ?
# ------------------------------------------------------------------------

# Comptez le nombre de valeurs manquantes de chaque variable (indice :
# combinez is.na() et colSums()).
## À vous :



# Combien de jours ont toutes leurs variables observées (cas complets) ?
# Quelle proportion de l'échantillon cela représente-t-il ? (indice :
# complete.cases())
## À vous :



# Lancez le code ci-dessous.
md.pattern(aq, rotate.names = TRUE)


# ------------------------------------------------------------------------
# Le mécanisme est-il MCAR ?
# ------------------------------------------------------------------------

# Créez une variable R dans aq, valant 1 si le jour est un cas complet pour
# les variables du modèle (Ozone, Solar.R, Wind, Temp) et 0 sinon. Affichez
# sa distribution avec table().
## À vous :



# Calculez la proportion de cas complets selon le mois (indice :
# prop.table(table(...), margin = 1)). Que constatez-vous ?
## À vous :



# Comparez la température et la vitesse du vent moyennes entre les jours
# complets et incomplets (indice : tapply()).
## À vous :




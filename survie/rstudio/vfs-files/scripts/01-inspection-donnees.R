# ========================================================================
# TP analyses de survie - Inspection de la base de données
# ========================================================================
#
# Énoncé et explications : page « Inspection de la base de données » du TP en ligne
# https://eusebe.github.io/survie/intro.html
#
# Exécutez le code ligne par ligne avec Ctrl+Entrée (Cmd+Entrée sur Mac).
# Vos modifications sont gardées dans votre navigateur.
# La base data_bone.csv est dans le dossier de travail.
# La correction est dans correction/correction.R.
# ========================================================================

## Préparation (déjà faite dans la page web)
library(survival)
library(dplyr)

bone <- read.csv2("data_bone.csv")
bone$dat_sel<-as.Date(bone$dat_sel, format="%d/%m/%Y")
bone$date_dc<-as.Date(bone$date_dc, format="%d/%m/%Y")
bone$ddn<-as.Date(bone$ddn, format="%d/%m/%Y")
bone$date_rec<-as.Date(bone$date_rec, format="%d/%m/%Y")
bone$date_fn<-as.Date(bone$date_fn, format="%d/%m/%Y")
bone$date_cc<-as.Date(bone$date_cc, format="%d/%m/%Y")
bone$date_ca<-as.Date(bone$date_ca, format="%d/%m/%Y")

# Vous pouvez écrire et lancer du code dans la boite ci-dessous. Pour le
# moment, n'ajoutez aucun code ci-dessous, cliquez juste sur le bouton Run
# Code.
# Ceci est un commentaire
# La base de données a déjà été importée à l'aide de la commande 
# read.csv2("data_bone.csv")
# Vous n'avez rien besoin d'écrire ici, appuyez juste sur le bouton 'Run Code' 
# ou sur le raccourci clavier Ctrl+Enter

str(bone)

# Essayez d'utiliser la fonction names(), avec le nom de la data.frame comme
# seul argument. Si vous êtes coincé·e, cliquez sur 'Montrez moi comment
# faire!' juste en dessous de la boite interactive (il y en aura une à chaque
# fois).
## À vous :



# Utilisez la fonction head(), à nouveau avec le nom de la data.frame comme
# seul argument.
## À vous :



# Ajoutez n = 10 comme second argument.
head(bone)


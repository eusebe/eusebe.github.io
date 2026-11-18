# ========================================================================
# TP analyses de survie - Comparaison de la survie de plusieurs groupes
# ========================================================================
#
# Énoncé et explications : page « Comparaison de la survie de plusieurs groupes » du TP en ligne
# https://eusebe.github.io/survie/comparaison.html
#
# Exécutez le code ligne par ligne avec Ctrl+Entrée (Cmd+Entrée sur Mac).
# Vos modifications sont gardées dans votre navigateur.
# La base data_bone.csv est dans le dossier de travail.
# La correction est dans correction/correction.R.
# ========================================================================

## Préparation (déjà faite dans la page web)
library(ggplot2)
library(ggsurvfit)
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

# délai de survie
bone$delsv<-difftime( bone$date_dc, bone$dat_sel,units="days")
bone$delsv<-ifelse(bone$etat==1, bone$delsv, difftime(bone$ddn, bone$dat_sel,units="days"))
fit_km <- survfit(Surv(delsv, etat) ~ 1, data = bone)
p <- ggsurvfit(fit_km)

# facteurs
bone$leuc.f <- factor(bone$leuc, 1:3, c("LAM B", "LAM C", "LAL"))


# ------------------------------------------------------------------------
# Survie globale par groupe
# ------------------------------------------------------------------------

## leuc.f a été préalablement générée avec la commande suivante :
## bone$leuc.f <- factor(bone$leuc, 1:3, c("LAM B", "LAM C", "LAL"))

table(bone$leuc, bone$leuc.f)

# Estimez la survie globale selon la variable leuc.f. Pour indiquer à R que
# l'on souhaite faire une estimation de Kaplan-Meïer par groupe, il faut
# remplacer ~ 1 par ~ nomvariable dans la formule utilisée avec la commande
# survfit. Vous stockerez le résultat dans un objet nommé fit_km_leuc avant
# d'afficher le résultat.
## À vous :



# Comme précédemment, utilisez les fonctions summary, plot et ggsurvfit.
## À vous :



fit_km_leuc2 <- survfit2(Surv(delsv, etat) ~ leuc.f, data = bone)
ggsurvfit(fit_km_leuc2)


# ------------------------------------------------------------------------
# Comparaison de la survie globale
# ------------------------------------------------------------------------

# Réalisez un test du logrank pour comparer la survie globale entre les
# groupes définis par la variable leuc.f.
## À vous :



# Affichez le résultat du test du logrank sur le graphique de Kaplan-Meïer.
# Vous utiliserez à nouveau la fonction ggsurvfit(), en y ajoutant la
# commande + add_pvalue(). Profitez-en pour ajouter également le nombre
# d'individus à risque.
## À vous :




# ========================================================================
# TP analyses de survie - Risques compétitifs
# ========================================================================
#
# Énoncé et explications : page « Risques compétitifs » du TP en ligne
# https://eusebe.github.io/survie/compet.html
#
# Exécutez le code ligne par ligne avec Ctrl+Entrée (Cmd+Entrée sur Mac).
# Vos modifications sont gardées dans votre navigateur.
# La base data_bone.csv est dans le dossier de travail.
# La correction est dans correction/correction.R.
# ========================================================================

## Préparation (déjà faite dans la page web)
library(ggplot2)
library(tidycmprsk)
library(survival)
library(dplyr)
library(ggsurvfit)

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
bone$fab.f <- factor(bone$fab, 0:1, c("Grade 4 ou 5 ou LMA ", "Autres"))
# bone$sexe.pat.f <- factor(bone$sexe.pat, 0:1, c("Femme", "Homme"))
bone$mtx.f <- factor(bone$mtx, 0:1, c("Pas de MTX", "MTX"))

fit_km_leuc <- survfit(Surv(delsv, etat) ~ leuc.f, data = bone)
fit_km_leuc2 <- survfit2(Surv(delsv, etat) ~ leuc.f, data = bone)

# délai de récidive
bone$delrec <- difftime(bone$date_rec, bone$dat_sel, units="days")
bone$delrec <-ifelse(bone$rec == 1, bone$delrec, difftime(bone$ddn, bone$dat_sel,units="days"))


# ------------------------------------------------------------------------
# Délai jusqu'à récidive, survie sans récidive
# ------------------------------------------------------------------------

head(bone[, c("delrec", "rec")])
summary(bone[, c("delrec", "rec")])

# Créez le critère composite 'délai jusqu'à récidive OU décès sans récidive',
# puis décrivez le avec la méthode de Kaplan-Meïer. La fonction pmin() pourra
# être très utile pour calculer le délai jusqu'au premier des deux
# évènements.
## À vous :



## Fonction d'incidence cumulée (F(t) = 1-S(t))
plot(survfit(Surv(deldfs, dfs) ~ 1, data = bone), fun = "F") 


# ------------------------------------------------------------------------
# Délai jusqu'à récidive ou décès sans récidive en risques compétitifs
# ------------------------------------------------------------------------

## À vous :




# ------------------------------------------------------------------------
# Incidence cumulée des évènements
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Approche 'naïve'
# ------------------------------------------------------------------------

plot(survfit(Surv(deldfs, compevent == "death without relapse") ~ 1, data = bone), 
     fun = "F", conf.int = FALSE)
lines(survfit(Surv(deldfs, compevent == "relapse") ~ 1, data = bone), 
      fun = "F", conf.int = FALSE, col = 2)


# ------------------------------------------------------------------------
# Approche correcte
# ------------------------------------------------------------------------

# Estimez les fonctions d'incidence cumulée des deux évènements en
# compétition à l'aide de la fonction cuminc(). Vous stockerez le résultat
# dans un objet appelé fit_cuminc.
library(tidycmprsk)

# Tracez les fonctions d'incidence estimées à l'aide de la fonction
# ggcuminc(). Par défaut, un seul évènement est représenté, mais vous pouvez
# ajouter l'option outcome = c("relapse", "death without relapse") pour
# obtenir les deux.
## À vous :




# ------------------------------------------------------------------------
# Modèle de Cox cause-spécifique
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# En deux commandes
# ------------------------------------------------------------------------

# Estimez un modèle de Cox cause-spécifique pour chacun des deux évènements.
# Vous ne mettrez qu'une seule variable explicative dans la formule : fab.f
# (classification des leucémies en deux catégories : 'Grade 4 ou 5 ou LMA' vs
# 'Autres').
## À vous :




# ------------------------------------------------------------------------
# En une seule commande
# ------------------------------------------------------------------------

coxph(Surv(deldfs, compevent) ~ fab.f, data = bone, id = num)

coxph(Surv(deldfs, compevent) ~ fab.f, data = bone, id = num, ties = "efron")


# ------------------------------------------------------------------------
# Modèle de Fine et Gray
# ------------------------------------------------------------------------

crr(Surv(deldfs, compevent) ~ fab.f, data = bone, failcode = c("relapse"))
crr(Surv(deldfs, compevent) ~ fab.f, data = bone, failcode = c("death without relapse"))


# ------------------------------------------------------------------------
# Comparaison des modèles causes-spécifiques et de sous-répartition
# ------------------------------------------------------------------------

ggcuminc(cuminc(Surv(deldfs, compevent) ~ fab.f, data = bone), 
         outcome = c("relapse", "death without relapse")) + 
  facet_grid(~ outcome)

# Remarque : le message Removed 2 rows... n'est pas une erreur. Au-delà du
# dernier temps de suivi du groupe 'Autres' (Jr max(bone$deldfs[bone$fab.f ==
# "Autres"])), l'incidence cumulée de ce groupe n'est plus estimable (plus
# aucun patient à risque) : les deux points correspondants (un par évènement)
# ne sont pas tracés.

# ========================================================================
# TP analyses de survie - Description de la survie
# ========================================================================
#
# Énoncé et explications : page « Description de la survie » du TP en ligne
# https://eusebe.github.io/survie/description.html
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


# ------------------------------------------------------------------------
# Construction du délai de survie
# ------------------------------------------------------------------------

# Evaluez la cohérence du contenu des variables date_dc et ddn pour les
# patients décédés, en vérifiant que les colonnes date_dc et ddn ont la même
# valeur chez eux.
## À vous :



# Quel est le contenu de la variable date_dc chez les patients vivants aux
# dernières nouvelles ?
## À vous :



# En utilisant la fonction difftime(), calculez le délai de suivi de chaque
# patient en jours, et stockez le dans la base de données dans une variable
# nommée delsv. La fonction a trois arguments : time1 (la première date),
# time2 (la seconde date), et units (utilisez "days").
## À vous :




# ------------------------------------------------------------------------
# Estimation de la survie globale par la méthode de Kaplan-Meïer
# ------------------------------------------------------------------------

# Lancez la commande ci-dessous.
# Vous n'avez rien besoin d'écrire ici, appuyez juste sur le bouton 'Run Code' 
# ou sur le raccourci clavier Ctrl+Enter
survfit(Surv(delsv, etat) ~ 1, data = bone)

# Stockez le résultat de la commande survfit() dans un objet appelé fit_km,
# puis appliquez la fonction summary() à cet objet.
## À vous :



# Ajoutez times = c(0, 365, 730) à la commande summary.
summary(fit_km)

# Utilisez la fonction plot() sur l'objet fit_km.
## À vous :



# Ajoutez les temps de censures (option mark.time = TRUE), et des labels
# informatifs aux axes (options xlab = "mon label" et ylab = "mon label")
## À vous :



# Le code ci-dessous n'a pas besoin d'être modifié. Conseil : utilisez le
# raccourci clavier Ctrl-Enter pour lancer ces commandes ligne après ligne et
# regarder le graphique produit à chaque fois (et non le bouton "Run Code"
# qui affichera tous les graphiques d'un seul coup).
library(ggsurvfit) ## Package à installer 
p <- ggsurvfit(fit_km)
p ## Courbe de base

p + add_censor_mark() ## Ajout des censures
p + add_confidence_interval() ## Ajout de l'intervalle de confiance
p + add_quantile() ## Ajout de la médiane de survie
p + add_risktable() ## Ajout du nombre d'individus exposés au risque et du 
                    ## nombre d'évènement
p + add_risktable(risktable_stats = "n.risk") ## Ajout du nombre d'individus 
                                              ## exposés au risque uniquement 
p + add_risktable(risktable_stats = "n.risk", times = seq(0, 2500, 500)) 
                                              ## Ajout du nombre d'individus
                                              ## exposés au risque uniquement,
                                              ## tous les 500 jours
p + scale_x_continuous(breaks = seq(0, 2500, 500)) ## Modification des temps 
                                                   ## représentés

# En vous basant sur le code ci-dessus qui explique chaque commande
# individuelle, construisez un graphique montrant à la fois les censures,
# l'intervalle de confiance, la médiane et le nombre d'individus exposés au
# risque
p <- ggsurvfit(fit_km) + ...


# ------------------------------------------------------------------------
# Indicateurs
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Médiane de survie
# ------------------------------------------------------------------------

fit_km


# ------------------------------------------------------------------------
# Suivi médian
# ------------------------------------------------------------------------

# Estimer le suivi médian
## À vous :



# Estimer les bornes du suivi (on utilisera notamment les fonctions max() et
# min())
## À vous :




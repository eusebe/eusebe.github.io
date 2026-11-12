# ========================================================================
# Script de correction : TP introduction aux analyses de survie
# ========================================================================

library(survival)
library(dplyr)
library(ggsurvfit)
library(tidycmprsk)

## Importation de la base de donnees au format csv
## Attention de modifier le chemin vers le dossier de telechargement !
bone <- read.csv2("data_bone.csv")
## Mise des dates au format "Date"
bone$dat_sel <- as.Date(bone$dat_sel, format = "%d/%m/%Y")
bone$date_dc <- as.Date(bone$date_dc, format = "%d/%m/%Y")
bone$ddn <- as.Date(bone$ddn, format = "%d/%m/%Y")
bone$date_rec <- as.Date(bone$date_rec, format = "%d/%m/%Y")
bone$date_fn <- as.Date(bone$date_fn, format = "%d/%m/%Y")
bone$date_cc <- as.Date(bone$date_cc, format = "%d/%m/%Y")
bone$date_ca <- as.Date(bone$date_ca, format = "%d/%m/%Y")


# ========================================================================
# Introduction : inspection de la base de donnees
# ========================================================================

# Ceci est un commentaire
# La base de données a déjà été importée à l'aide de la commande 
# read.csv2("data_bone.csv")
# Vous n'avez rien besoin d'écrire ici, appuyez juste sur le bouton 'Run Code' 
# ou sur le raccourci clavier Ctrl+Enter

str(bone)

# Copier-collez cette solution ci-dessus :
names(bone)

head(bone)

# Copier-collez cette solution ci-dessus :
head(bone, n = 10)


# ========================================================================
# Description de la survie
# ========================================================================

# --- Construction du délai de survie

## Avec les commandes R de base

bone_dc <- bone[bone$etat == 1, c("date_dc", "ddn")] ## Sélection des patients 
                                                     ## décédés et des colonnes 
                                                     ## "date_dc" et "ddn"
head(bone_dc) ## Premières lignes
tail(bone_dc) ## Dernières lignes
table(bone_dc$date_dc == bone_dc$ddn, useNA = "ifany") 
## Les dates de ces deux colonnes sont toutes identiques chez les individus décédés

## Même chose avec dplyr
# library(dplyr)
bone %>% 
  filter(etat == 1) %>%
  mutate(same_date = date_dc == ddn) %>%
  count(same_date)

bone_vv <- bone[bone$etat == 0, c("date_dc", "ddn")] ## Sélection des patients 
                                                     ## vivants et des colonnes 
                                                     ## "date_dc" et "ddn"
head(bone_vv)
tail(bone_vv)
table(bone_vv$date_dc, useNA = "ifany") ## Il n'y a que des NA. C'est normal !

## Même chose avec dplyr
# library(dplyr)
bone %>% 
  filter(etat == 0) %>%
  count(date_dc)

bone$delsv <- difftime(bone$ddn, bone$dat_sel, units = "days")
head(bone[, c("dat_sel", "ddn", "delsv", "etat")])
tail(bone[, c("dat_sel", "ddn", "delsv", "etat")])

## Même chose avec dplyr
# library(dplyr)
bone <- bone %>% 
  mutate(delsv = difftime(ddn, dat_sel, units = "days"))

# --- Estimation de la survie globale par la méthode de Kaplan-Meïer

# Vous n'avez rien besoin d'écrire ici, appuyez juste sur le bouton 'Run Code' 
# ou sur le raccourci clavier Ctrl+Enter
survfit(Surv(delsv, etat) ~ 1, data = bone)

fit_km <- survfit(Surv(delsv, etat) ~ 1, data = bone)
summary(fit_km)

summary(fit_km, times = c(0, 365, 730))

plot(fit_km)

plot(fit_km, mark.time = TRUE, xlab = "Temps en jours", 
     ylab = "Probabilité de survie globale")

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

p + add_censor_mark() + 
  add_confidence_interval() +
  add_quantile() + 
  add_risktable(risktable_stats = "n.risk", times = seq(0, 2500, 500)) +
  scale_x_continuous(breaks = seq(0, 2500, 500))

## Tips : seq(0, 2500, 500) : cette commande génère 
## les nombres 0, 500, 1000, ..., 2500

# --- Médiane de survie

fit_km

# --- Suivi médian

survfit(Surv(delsv, 1-etat) ~ 1, data = bone)

max(bone$delsv)
min(bone$delsv[bone$etat == 0])

## Même chose avec dplyr
# library(dplyr)
bone %>% summarize(max(delsv))
bone %>% filter(etat == 0) %>% summarize(min(delsv))


# ========================================================================
# Comparaison de la survie de plusieurs groupes
# ========================================================================

## Variables creees en debut de partie
bone$delsv <- difftime(bone$date_dc, bone$dat_sel,units = "days")
bone$delsv <- ifelse(bone$etat == 1, bone$delsv, 
                     difftime(bone$ddn, bone$dat_sel, units = "days"))
bone$leuc.f <- factor(bone$leuc, 1:3, c("LAM B", "LAM C", "LAL"))

# --- Survie globale par groupe

## leuc.f a été préalablement générée avec la commande suivante :
## bone$leuc.f <- factor(bone$leuc, 1:3, c("LAM B", "LAM C", "LAL"))

table(bone$leuc, bone$leuc.f)

fit_km_leuc <- survfit(Surv(delsv, etat) ~ leuc.f, data = bone)
fit_km_leuc

summary(fit_km_leuc, times = c(365*0:7))
plot(fit_km_leuc)
ggsurvfit(fit_km_leuc)

fit_km_leuc2 <- survfit2(Surv(delsv, etat) ~ leuc.f, data = bone)
ggsurvfit(fit_km_leuc2)

# --- Comparaison de la survie globale

survdiff(Surv(delsv, etat) ~ leuc.f, data = bone)

ggsurvfit(fit_km_leuc2) + 
  add_pvalue() +
  add_risktable(risktable_stats = "n.risk", times = seq(0, 2500, 500)) +
  scale_x_continuous(breaks = seq(0, 2500, 500))

## Tips : seq(0, 2500, 500) : cette commande génère 
## les nombres 0, 500, 1000, ..., 2500


# ========================================================================
# Modele de Cox
# ========================================================================

## Variables creees en debut de partie
bone$delsv <- difftime(bone$date_dc, bone$dat_sel,units = "days")
bone$delsv <- ifelse(bone$etat == 1, bone$delsv, 
                     difftime(bone$ddn, bone$dat_sel,units = "days"))
bone$leuc.f <- factor(bone$leuc, 1:3, c("LAM B", "LAM C", "LAL"))
bone$mtx.f <- factor(bone$mtx, 0:1, c("Pas de MTX", "MTX"))

# --- Modèle de Cox univariable

fit_cox_leuc <- coxph(Surv(delsv, etat) ~ leuc.f, data = bone)
fit_cox_leuc

summary(fit_cox_leuc)

# --- Hypothèse des hasards proportionnels

cox.zph(fit_cox_leuc, transform = "rank", terms = FALSE)

plot(cox.zph(fit_cox_leuc, transform = "identity", terms = FALSE))

## mtx.f a été préalablement générée avec la commande suivante :
# bone$mtx.f <- factor(bone$mtx, 0:1, c("Pas de MTX", "MTX"))
fit_cox_mtx <- coxph(Surv(delsv, etat) ~ mtx.f, data = bone)
fit_cox_mtx
cox.zph(fit_cox_mtx, transform = "rank", terms = FALSE)
plot(cox.zph(fit_cox_mtx, transform = "identity", terms = FALSE))

fit_km_mtx <- survfit2(Surv(delsv, etat) ~ mtx.f, data = bone)
ggsurvfit(fit_km_mtx) + 
  scale_x_continuous(breaks = seq(0, 2500, 500)) +
  add_risktable(risktable_stats = "n.risk", times = seq(0, 2500, 500))

# --- Modèle de Cox multivariable

fit_cox_leuc_age <- coxph(Surv(delsv, etat) ~ leuc.f + age.pat, data = bone)
summary(fit_cox_leuc_age)

fit_cox_age <- coxph(Surv(delsv, etat) ~ age.pat, data = bone)
anova(fit_cox_leuc_age, fit_cox_age)


# ========================================================================
# Risques competitifs
# ========================================================================

## Variables creees en debut de partie
bone$delsv <- difftime(bone$date_dc, bone$dat_sel,units = "days")
bone$delsv <- ifelse(bone$etat == 1, bone$delsv, 
                     difftime(bone$ddn, bone$dat_sel, units = "days"))
bone$leuc.f <- factor(bone$leuc, 1:3, c("LAM B", "LAM C", "LAL"))
bone$mtx.f <- factor(bone$mtx, 0:1, c("Pas de MTX", "MTX"))
bone$fab.f <- factor(bone$fab, 0:1, c("Grade 4 ou 5 ou LMA ", "Autres"))
bone$delrec <- difftime(bone$date_rec, bone$dat_sel, units = "days")
bone$delrec <- ifelse(bone$rec == 1, bone$delrec, 
                      difftime(bone$ddn, bone$dat_sel, units = "days"))

# --- Délai jusqu'à récidive, survie sans récidive

head(bone[, c("delrec", "rec")])
summary(bone[, c("delrec", "rec")])

## Calcul du délai jusqu'au premier évènement
bone$deldfs <- pmin(bone$delrec, bone$delsv) ## pour chaque patient, délai 
                                             ## minimal entre delrec et delsv
### Alternative : apply(bone[, c("delrec", "delsv")], 1, min)

## Evènement : récidive OU décès
bone$dfs <- ifelse(bone$rec == 1 | bone$etat == 1 , 1, 0)
plot(survfit(Surv(deldfs, dfs) ~ 1, data = bone)) ## Fonction de survie 
                                                  ## (comme d'habitude)

## Même chose avec dplyr
# library(dplyr)
bone <- bone %>% 
    rowwise() %>%
    mutate(
      deldfs = min(delrec, delsv), 
      dfs = case_when(
        rec == 1 | etat == 1 ~ 1, 
        rec == 0 & etat == 0 ~ 0
      )
    )

## Fonction d'incidence cumulée (F(t) = 1-S(t))
plot(survfit(Surv(deldfs, dfs) ~ 1, data = bone), fun = "F") 

# --- Délai jusqu'à récidive ou décès sans récidive en risques compétitifs

bone$compevent <- NA
bone$compevent[bone$etat == 0 & bone$rec == 0] <- 0
bone$compevent[bone$rec == 1] <- 1
bone$compevent[bone$etat == 1 & bone$rec == 0] <- 2
bone$compevent <- factor(bone$compevent, c(0, 1, 2), 
                         c("censor", "relapse", "death without relapse"))

## Même chose avec dplyr
# library(dplyr)
bone <- bone %>% 
  mutate(
    compevent = factor(case_when(
      etat == 0 & rec == 0 ~ "censor", 
      rec == 1 ~ "relapse", 
      etat == 1 & rec == 0 ~ "death without relapse"
    ))
  )

# --- Approche 'naïve'

plot(survfit(Surv(deldfs, compevent == "death without relapse") ~ 1, data = bone), 
     fun = "F", conf.int = FALSE)
lines(survfit(Surv(deldfs, compevent == "relapse") ~ 1, data = bone), 
      fun = "F", conf.int = FALSE, col = 2)

# --- Approche correcte

library(tidycmprsk)
fit_cuminc <- cuminc(Surv(deldfs, compevent) ~ 1, data = bone)
fit_cuminc

ggcuminc(fit_cuminc, outcome = c("relapse", "death without relapse"))

# --- En deux commandes

coxph(Surv(deldfs, compevent == "relapse") ~ fab.f, data = bone)
coxph(Surv(deldfs, compevent == "death without relapse") ~ fab.f, data = bone)

# --- En une seule commande

coxph(Surv(deldfs, compevent) ~ fab.f, data = bone, id = num)

# --- Modèle de Fine et Gray

crr(Surv(deldfs, compevent) ~ fab.f, data = bone, failcode = c("relapse"))
crr(Surv(deldfs, compevent) ~ fab.f, data = bone, failcode = c("death without relapse"))

# --- Comparaison des modèles causes-spécifiques et de sous-répartition

ggcuminc(cuminc(Surv(deldfs, compevent) ~ fab.f, data = bone), 
         outcome = c("relapse", "death without relapse")) + 
  facet_grid(~ outcome)


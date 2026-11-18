# ========================================================================
# TP analyses de survie - Modèle de Cox
# ========================================================================
#
# Énoncé et explications : page « Modèle de Cox » du TP en ligne
# https://eusebe.github.io/survie/cox.html
#
# Exécutez le code ligne par ligne avec Ctrl+Entrée (Cmd+Entrée sur Mac).
# Vos modifications sont gardées dans votre navigateur.
# La base data_bone.csv est dans le dossier de travail.
# La correction est dans correction/correction.R.
# ========================================================================

## Préparation (déjà faite dans la page web)
library(ggplot2)
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
# bone$sexe.pat.f <- factor(bone$sexe.pat, 0:1, c("Femme", "Homme"))
bone$mtx.f <- factor(bone$mtx, 0:1, c("Pas de MTX", "MTX"))

fit_km_leuc <- survfit(Surv(delsv, etat) ~ leuc.f, data = bone)
fit_km_leuc2 <- survfit2(Surv(delsv, etat) ~ leuc.f, data = bone)

# délais jusqu'à complication aigue / récidive (NA si pas d'évènement)
bone$delca <- as.numeric(difftime(bone$date_ca, bone$dat_sel, units = "days"))
bone$delrec_obs <- as.numeric(difftime(bone$date_rec, bone$dat_sel, units = "days"))

# Contournement d'un bug de survival::tmerge sous webR : la routine C
# Ctmerge2 lit hors limites (nid[-1]) et tdc() renvoie alors 0 partout.
# On la remplace par un équivalent en R (même résultat qu'en R natif).
local({
  tmerge2_r <- function(id, time1, nid, ntime) {
    vapply(seq_along(id), function(i) {
      k <- which(nid == id[i] & ntime <= time1[i])
      if (length(k)) max(k) else 0L
    }, integer(1))
  }
  f <- survival::tmerge
  b <- deparse(body(f))
  b <- sub(".Call(Ctmerge2,", "tmerge2_r(", b, fixed = TRUE)
  body(f) <- parse(text = b)[[1]]
  e <- new.env(parent = asNamespace("survival"))
  e$tmerge2_r <- tmerge2_r
  environment(f) <- e
  assign("tmerge", f, envir = globalenv())
})


# ------------------------------------------------------------------------
# Modèle de Cox univariable
# ------------------------------------------------------------------------

# Fittez un modèle de Cox évaluant l'effet de la variable leuc.f sur la
# survie globale, à l'aide de la fonction coxph(). Vous stockerez le résultat
# dans un objet appelé fit_cox_leuc avant d'afficher le résultat.
## À vous :



# Appliquez summary() à l'objet fit_cox_leuc.
## À vous :




# ------------------------------------------------------------------------
# Hypothèse des hasards proportionnels
# ------------------------------------------------------------------------

# Lancer la ligne de code suivante.
cox.zph(fit_cox_leuc, transform = "rank", terms = FALSE)

# Lancer la ligne de code suivante.
plot(cox.zph(fit_cox_leuc, transform = "identity", terms = FALSE))

# Fittez un modèle de Cox évaluant l'effet de la variable mtx.f sur la survie
# globale, stockez le résultat dans un objet nommé fit_cox_mtx, puis évaluez
# l'hypothèse des hasards proportionnels.
## À vous :



# Tracez maintenant la survie globale en fonction des catégories de la
# variable mtx.f. Pouvait-on s'attendre au résultat du test de Harrell ?
# Pourquoi ?
## À vous :




# ------------------------------------------------------------------------
# Modèle de Cox multivariable
# ------------------------------------------------------------------------

# Fittez un modèle de Cox évaluant l'effet des variables leuc.f ET age.pat
# sur la survie globale, et stockez le résultat dans un objet nommé
# fit_cox_leuc_age avant d'afficher le résultat détaillé.
## À vous :



# Testez la variable leuc.f à l'aide du test du rapport de vraisemblance.
## À vous :




# ------------------------------------------------------------------------
# Covariable dont la valeur dépend du temps
# ------------------------------------------------------------------------


# ------------------------------------------------------------------------
# Approche naïve
# ------------------------------------------------------------------------

# Fittez un modèle de Cox évaluant l'effet de la variable ca sur la survie
# globale, comme vous l'avez fait pour les autres variables.
## À vous :



# Lancer la ligne de code suivante.
summary(bone$delca)


# ------------------------------------------------------------------------
# Découpage des données
# ------------------------------------------------------------------------

# Lancer les lignes de code suivantes, qui créent une nouvelle data.frame
# bone_td, puis comparent, pour 4 patients, les données d'origine et les
# données découpées.
bone_td <- tmerge(bone[, c("num", "age.pat", "leuc.f")], bone, id = num,
                  deces = event(delsv, etat))
bone_td <- tmerge(bone_td, bone, id = num, ca_t = tdc(delca))
## Données d'origine : une ligne par patient
bone[bone$num %in% c(1, 2, 15, 18), c("num", "ca", "delca", "etat", "delsv")]
## Données découpées : une ligne par intervalle
bone_td[bone_td$num %in% c(1, 2, 15, 18), ]


# ------------------------------------------------------------------------
# Modèle de Cox avec une variable dépendante du temps
# ------------------------------------------------------------------------

# Fittez un modèle de Cox évaluant l'effet de la variable ca_t sur la survie
# globale, à partir de la data.frame bone_td, et comparez le HR obtenu à
# celui de l'approche naïve.
## À vous :




# ------------------------------------------------------------------------
# À vous : la récidive
# ------------------------------------------------------------------------

# Évaluez l'effet de la récidive sur la survie globale, d'abord avec
# l'approche naïve (variable rec), puis en tant que variable dépendante du
# temps. Pour cela, ajoutez à bone_td une variable rec_t à l'aide de tmerge()
# et tdc(), en vous inspirant du code précédent.
## À vous :




# ------------------------------------------------------------------------
# Modèle multivariable
# ------------------------------------------------------------------------

## À vous :




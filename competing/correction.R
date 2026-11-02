# ========================================================================
# Script de correction : TP survie causale, censure dependante, risques competitifs
# ========================================================================

library(survival)
library(dplyr)
library(cobalt)

# Chargement des donnees (format person-period : une ligne par individu et par
# periode k = 0, ..., 11)
df <- read.csv("df.csv")


# ========================================================================
# Presentation des donnees
# ========================================================================

# --- Chargement et exploration

library(survival)
library(dplyr)
df <- read.csv("df.csv")

head(df)
dim(df)                  # dimensions de la base
length(unique(df$id))    # nombre d'individus
sum(df$Y)                # nombre d'evenements d'interet (deces par cancer)
sum(df$D)                # nombre de deces par une autre cause (risque competitif)
df |> filter(id %in% unique(df$id)[1:2]) |> group_by(id) |>
  slice_head(n = 3) |> ungroup()  # 3 premiers mois des 2 premiers individus

# --- Construire la base baseline

## On resume df (format person-period) en une ligne par individu :
## - first() pour les covariables baseline (constantes pour un individu donne)
## - last() pour le temps de suivi total et les statuts finaux
df_base <- df |>
  group_by(id) |>
  summarise(
    A    = first(A),
    L1   = first(L1), L2 = first(L2), L3 = first(L3), L4 = first(L4),
    time = last(k),   # nombre de mois suivis
    Y    = last(Y),
    D    = last(D),
    C    = last(C)
  ) |>
  ungroup()

head(df_base)
nrow(df_base)  # doit valoir le nombre d'individus distincts

# --- Analyse brute (non ajustée)

km_brut <- survfit(Surv(time, Y) ~ A, data = df_base)

plot(km_brut,
     col  = c("#1D2769", "#AC182E"), lwd = 2,
     xlab = "Temps (mois)", ylab = "Probabilité de survie (sans Y)",
     main = "Kaplan-Meier brut selon A (D et C traités comme censure)")
legend("bottomleft",
       legend = c("A = 0 (placebo)", "A = 1 (DES)"),
       col = c("#1D2769", "#AC182E"), lwd = 2)

## Survie a 36 mois
summary(km_brut, times = 36)


# ========================================================================
# Partie 1 : confusion et censure dependante
# ========================================================================

# --- Étape 1 - Score de propension et poids IPTW

ps_mod <- glm(A ~ L1 + L2 + L3 + L4, data = df_base, family = binomial)
df_base$e_hat <- predict(ps_mod, type = "response")

## Poids IPTW pour l'ATE : inverse de la probabilite du traitement recu
df_base$w_iptw <- ifelse(df_base$A == 1,
                          1 / df_base$e_hat,
                          1 / (1 - df_base$e_hat))

summary(df_base$w_iptw)

# --- Étape 2 - Kaplan-Meier pondéré (IPTW)

km_iptw <- survfit(Surv(time, Y) ~ A, data = df_base, weights = w_iptw)
summary(km_iptw, times = 36)

# --- Étape 3 - G-formula paramétrique

## Modele de hazard discret, poole sur toutes les periodes ou l'individu est
## encore a risque de Y (ni D ni C survenu pendant la periode)
haz_mod <- glm(Y ~ A * k + L1 + L2 + L3 + L4,
               data = subset(df, D == 0 & C == 0), family = binomial)
summary(haz_mod)

K <- 36
predict_gform <- function(a_val) {
  surv <- rep(1, nrow(df_base))  # survie cumulee, une valeur par individu
  for (kk in 1:K) {
    newdata <- data.frame(A = a_val, k = kk,
                           L1 = df_base$L1, L2 = df_base$L2,
                           L3 = df_base$L3, L4 = df_base$L4)
    haz <- predict(haz_mod, newdata = newdata, type = "response")
    surv <- surv * (1 - haz)
  }
  surv  # = S^a(36) individuel, sous le scenario A = a_val
}

s1_gform <- predict_gform(1)  # survie individuelle predite sous A=1
s0_gform <- predict_gform(0)  # survie individuelle predite sous A=0

## Moyenne sur toute la population : estimation de S^a=1(36) et S^a=0(36)
c(S1 = mean(s1_gform), S0 = mean(s0_gform))

# --- À 36 mois, les poids IPCW valent tous 1

## Modele de la censure, restreint a la periode reellement a risque
late <- subset(df, k > 50)
cens_mod <- glm(C ~ A + L1 + L2 + L3 + L4, data = late, family = binomial)
df$p_cens <- 0
df$p_cens[df$k > 50] <- predict(cens_mod, newdata = df[df$k > 50, ], type = "response")

## Poids IPCW cumule : produit de 1/(1 - p_cens) depuis la 1ere periode jusqu'a la
## periode courante (chaque individu contribue une ligne par periode a risque)
df <- df |>
  group_by(id) |>
  mutate(w_ipcw = cumprod(1 / (1 - p_cens))) |>
  ungroup()

summary(df$w_ipcw)

# --- Étape 5 - Combiner IPTW et IPCW

df_w <- df |>
  left_join(df_base[, c("id", "w_iptw")], by = "id") |>
  mutate(poids_total = w_iptw * w_ipcw)

## Format "compteur" : chaque ligne est l'intervalle [k-1, k) avec son propre poids
km_combine <- survfit(Surv(k - 1, k, Y) ~ A, data = df_w, weights = poids_total)
summary(km_combine, times = 36)


# ========================================================================
# Partie 2 : risques competitifs et estimands
# ========================================================================

# --- Étape 1 - CIF naïve vs CIF correcte

df_base$statut <- factor(
  ifelse(df_base$Y == 1, "evenement", ifelse(df_base$D == 1, "deces", "censore")),
  levels = c("censore", "evenement", "deces")
)

## CIF naive (D traite comme une censure, comme en Partie 1)
km_naif <- survfit(Surv(time, Y) ~ 1, data = df_base)
cif_naive_36 <- 1 - summary(km_naif, times = 36)$surv

## CIF correcte (Aalen-Johansen, prend en compte D comme risque competitif)
aj <- survfit(Surv(time, statut) ~ 1, data = df_base)
s_aj <- summary(aj, times = 36)
cif_correcte_36 <- s_aj$pstate[, colnames(s_aj$pstate) == "evenement"]

c(naive = cif_naive_36, correcte = cif_correcte_36)

# --- Étape 2 - Estimand 1 : effet total

haz_mod_D <- glm(D ~ A + k + I(k^2) + L1 + L2 + L3 + L4,
                 data = subset(df, C == 0), family = binomial)

K <- 36
gcomp_CIF <- function(a_Y, a_D) {
  Spred <- rep(1, nrow(df_base)); CIF <- rep(0, nrow(df_base))
  for (kk in 1:K) {
    newD <- data.frame(A = a_D, k = kk, L1 = df_base$L1, L2 = df_base$L2,
                        L3 = df_base$L3, L4 = df_base$L4)
    newY <- data.frame(A = a_Y, k = kk, L1 = df_base$L1, L2 = df_base$L2,
                        L3 = df_base$L3, L4 = df_base$L4)
    hazD <- predict(haz_mod_D, newdata = newD, type = "response")
    hazY <- predict(haz_mod_Y, newdata = newY, type = "response")
    CIF   <- CIF + Spred * (1 - hazD) * hazY
    Spred <- Spred * (1 - hazD) * (1 - hazY)
  }
  mean(CIF)
}

## Effet total : les deux hazards varient ENSEMBLE avec A
effet_total <- gcomp_CIF(1, 1) - gcomp_CIF(0, 0)
effet_total

# --- Étape 3 - Estimand 2 : risque net

gcomp_CIF0 <- function(a_Y) {
  Spred <- rep(1, nrow(df_base)); CIF <- rep(0, nrow(df_base))
  for (kk in 1:K) {
    newY <- data.frame(A = a_Y, k = kk, L1 = df_base$L1, L2 = df_base$L2,
                        L3 = df_base$L3, L4 = df_base$L4)
    hazY <- predict(haz_mod_Y, newdata = newY, type = "response")
    CIF   <- CIF + Spred * hazY
    Spred <- Spred * (1 - hazY)
  }
  mean(CIF)
}

## Risque net : le modele de D n'est JAMAIS utilise
risque_net <- gcomp_CIF0(1) - gcomp_CIF0(0)
risque_net

# --- Étape 4 - Estimand 3 : effet séparable

## Effet separable : hazard de D FIXE a a_D = 0, seul celui de Y varie
effet_separable <- gcomp_CIF(1, 0) - gcomp_CIF(0, 0)
effet_separable

# --- Récapitulatif

data.frame(
  Estimand = c("Effet total", "Risque net", "Effet separable"),
  Difference_de_risque = round(c(effet_total, risque_net, effet_separable), 3)
)


# ========================================================================
# Partie 3 : ponderation (IPW) et decomposition
# ========================================================================

# --- Étape 1 - Modèle de censure et poids IPCW

late <- subset(df, k > 50)
modC <- glm(C ~ A + L1 + L2 + L3 + L4, data = late, family = binomial)
df$predC <- 1
df$predC[df$k > 50] <- 1 - predict(modC, newdata = df[df$k > 50, ], type = "response")
df <- df |>
  group_by(id) |>
  mutate(cumPredC = cumprod(predC)) |>
  ungroup()

w_total <- 1 / df$cumPredC          # poids "effet total" : censure seule
summary(w_total)

# --- Étape 2 - Poids "risque net" (censure × D)

modD <- glm(D ~ A + k + I(k^2) + L1 + L2 + L3 + L4,
            data = subset(df, C == 0), family = binomial)
df$predO <- 1 - predict(modD, newdata = df, type = "response")
df <- df |>
  group_by(id) |>
  mutate(cumPredO = cumprod(predO)) |>
  ungroup()

w_net <- w_total / df$cumPredO      # risque net : censure x D
summary(w_net)

# --- Étape 3 - Effet total et risque net, par IPW

nonparam_hazard <- function(w, a, event = c("Y", "D")) {
  event <- match.arg(event)
  sapply(1:K, function(kk) {
    if (event == "Y") { idx <- df$k==kk & df$A==a & df$C==0 & df$D==0; ev <- idx & df$Y==1 }
    else               { idx <- df$k==kk & df$A==a & df$C==0;          ev <- idx & df$D==1 }
    sum(w[ev]) / sum(w[idx])
  })
}
chain_CIF <- function(hazY, hazD = NULL) {
  S <- 1; cif <- 0
  for (kk in 1:K) {
    hD  <- if (!is.null(hazD)) hazD[kk] else 0
    cif <- cif + S * (1 - hD) * hazY[kk]
    S   <- S * (1 - hD) * (1 - hazY[kk])
  }
  cif
}

ipw_tot_1 <- chain_CIF(nonparam_hazard(w_total, 1, "Y"), nonparam_hazard(w_total, 1, "D"))
ipw_tot_0 <- chain_CIF(nonparam_hazard(w_total, 0, "Y"), nonparam_hazard(w_total, 0, "D"))
ipw_net_1 <- chain_CIF(nonparam_hazard(w_net, 1, "Y"))
ipw_net_0 <- chain_CIF(nonparam_hazard(w_net, 0, "Y"))

c(effet_total = ipw_tot_1 - ipw_tot_0, risque_net = ipw_net_1 - ipw_net_0)

# --- Étape 4 - Effet séparable, par IPW

predO_aD0 <- 1 - predict(modD, newdata = transform(df, A = 0), type = "response")
predO_aY1 <- 1 - predict(modD, newdata = transform(df, A = 1), type = "response")
w_sep <- (ave(predO_aD0, df$id, FUN = cumprod) / ave(predO_aY1, df$id, FUN = cumprod)) * w_total

## Somme ponderee directe (Stensrud et al., JASA 2022, eq. 11), sur les patients A=1
n_A1 <- sum(df$A[!duplicated(df$id)] == 1)
ipw_sep <- sum(sapply(1:K, function(kk) {
  idx <- df$k==kk & df$A==1 & df$C==0 & df$D==0
  sum(w_sep[idx & df$Y==1]) / n_A1
}))
effet_separable_ipw <- ipw_sep - ipw_tot_0
effet_separable_ipw


# ========================================================================
# Conclusion : comparaison des methodes
# ========================================================================

# --- Calcul de toutes les analyses (recapitulatif complet)

library(survival); library(dplyr)

## ---- Donnees ---------------------------------------------------------------
df <- read.csv("df.csv")
df_base <- df |>
  group_by(id) |>
  summarise(A = first(A), L1 = first(L1), L2 = first(L2), L3 = first(L3), L4 = first(L4),
            time = last(k), Y = last(Y), D = last(D), C = last(C)) |>
  ungroup()
K <- 36

## ---- Analyse brute (D et C traites comme censure) ---------------------------
km_brut <- survfit(Surv(time, Y) ~ A, data = df_base)
s36_brut <- summary(km_brut, times = 36)$surv

## ---- IPTW --------------------------------------------------------------------
ps_mod <- glm(A ~ L1 + L2 + L3 + L4, data = df_base, family = binomial)
df_base$e_hat <- predict(ps_mod, type = "response")
df_base$w_iptw <- ifelse(df_base$A == 1, 1 / df_base$e_hat, 1 / (1 - df_base$e_hat))
km_iptw <- survfit(Surv(time, Y) ~ A, data = df_base, weights = w_iptw)
s36_iptw <- summary(km_iptw, times = 36)$surv

## ---- G-formula (hazard de Y seul, = risque net) -------------------------------
## Partie 1 : modele simple (A * k)
haz_mod <- glm(Y ~ A * k + L1 + L2 + L3 + L4,
               data = subset(df, D == 0 & C == 0), family = binomial)
## Parties 2 et 3 : meme specification de la dependance temporelle que dans les
## diapos du cours (polynome de degre 3, avec interactions avec A) : cf. l'encart
## de la Partie 2 sur ce choix de specification.
haz_mod_Y <- glm(Y ~ A + k + I(k^2) + I(k^3) + A:k + A:I(k^2) + L1 + L2 + L3 + L4,
                 data = subset(df, D == 0 & C == 0), family = binomial)
predict_gform <- function(a_val, mod = haz_mod_Y) {
  surv <- rep(1, nrow(df_base))
  for (kk in 1:K) {
    newdata <- data.frame(A = a_val, k = kk, L1 = df_base$L1, L2 = df_base$L2,
                           L3 = df_base$L3, L4 = df_base$L4)
    haz <- predict(mod, newdata = newdata, type = "response")
    surv <- surv * (1 - haz)
  }
  surv
}
s36_gform <- c(mean(predict_gform(0, haz_mod)), mean(predict_gform(1, haz_mod)))

## ---- IPTW x IPCW --------------------------------------------------------------
## Modele de censure restreint a la periode reellement a risque (k > 50 : personne
## n'est censure avant, cf. Partie 1)
late_cens <- subset(df, k > 50)
cens_mod <- glm(C ~ A + L1 + L2 + L3 + L4, data = late_cens, family = binomial)
df$p_cens <- 0
df$p_cens[df$k > 50] <- predict(cens_mod, newdata = df[df$k > 50, ], type = "response")
df <- df |>
  group_by(id) |>
  mutate(w_ipcw = cumprod(1 / (1 - p_cens))) |>
  ungroup()
df_w <- df |>
  left_join(df_base[, c("id", "w_iptw")], by = "id") |>
  mutate(poids_total = w_iptw * w_ipcw)
km_combine <- survfit(Surv(k - 1, k, Y) ~ A, data = df_w, weights = poids_total)
s36_combine <- summary(km_combine, times = 36)$surv

## ---- CIF naive vs CIF correcte (Aalen-Johansen), risques competitifs ----------
df_base$statut <- factor(
  ifelse(df_base$Y == 1, "evenement", ifelse(df_base$D == 1, "deces", "censore")),
  levels = c("censore", "evenement", "deces")
)
km_naif_pop <- survfit(Surv(time, Y) ~ 1, data = df_base)
cif_naive_36 <- 1 - summary(km_naif_pop, times = 36)$surv
aj_pop <- survfit(Surv(time, statut) ~ 1, data = df_base)
s_aj_pop <- summary(aj_pop, times = 36)
cif_correcte_36 <- s_aj_pop$pstate[, colnames(s_aj_pop$pstate) == "evenement"]

## ---- Les 3 estimands par g-formula (deux hazards) -----------------------------
haz_mod_D <- glm(D ~ A + k + I(k^2) + L1 + L2 + L3 + L4,
                 data = subset(df, C == 0), family = binomial)
gcomp_CIF <- function(a_Y, a_D) {
  Spred <- rep(1, nrow(df_base)); CIF <- rep(0, nrow(df_base))
  for (kk in 1:K) {
    newD <- data.frame(A = a_D, k = kk, L1 = df_base$L1, L2 = df_base$L2,
                        L3 = df_base$L3, L4 = df_base$L4)
    newY <- data.frame(A = a_Y, k = kk, L1 = df_base$L1, L2 = df_base$L2,
                        L3 = df_base$L3, L4 = df_base$L4)
    hazD <- predict(haz_mod_D, newdata = newD, type = "response")
    hazY <- predict(haz_mod_Y, newdata = newY, type = "response")
    CIF   <- CIF + Spred * (1 - hazD) * hazY
    Spred <- Spred * (1 - hazD) * (1 - hazY)
  }
  mean(CIF)
}
g_tot_1 <- gcomp_CIF(1, 1); g_tot_0 <- gcomp_CIF(0, 0)
g_net_1 <- 1 - mean(predict_gform(1)); g_net_0 <- 1 - mean(predict_gform(0))
g_sep   <- gcomp_CIF(1, 0)
g01     <- gcomp_CIF(0, 1)

effet_total_g     <- g_tot_1 - g_tot_0
risque_net_g      <- g_net_1 - g_net_0
effet_separable_g <- g_sep - g_tot_0

## ---- Decomposition directe/indirecte -------------------------------------------
indirect_aY1 <- g_tot_1 - g_sep
indirect_aY0 <- g01 - g_tot_0
direct_aD1   <- g_tot_1 - g01
direct_aD0   <- g_sep - g_tot_0

## ---- Les 3 estimands par IPW ----------------------------------------------------
## Meme restriction que ci-dessus pour le modele de censure (k > 50)
late <- subset(df, k > 50)
modC <- glm(C ~ A + L1 + L2 + L3 + L4, data = late, family = binomial)
df$predC <- 1
df$predC[df$k > 50] <- 1 - predict(modC, newdata = df[df$k > 50, ], type = "response")
df <- df |> group_by(id) |> mutate(cumPredC = cumprod(predC)) |> ungroup()
w_total <- 1 / df$cumPredC

modD <- glm(D ~ A + k + I(k^2) + L1 + L2 + L3 + L4,
            data = subset(df, C == 0), family = binomial)
df$predO <- 1 - predict(modD, newdata = df, type = "response")
df <- df |> group_by(id) |> mutate(cumPredO = cumprod(predO)) |> ungroup()
w_net <- w_total / df$cumPredO

nonparam_hazard <- function(w, a, event = c("Y", "D")) {
  event <- match.arg(event)
  sapply(1:K, function(kk) {
    if (event == "Y") { idx <- df$k==kk & df$A==a & df$C==0 & df$D==0; ev <- idx & df$Y==1 }
    else               { idx <- df$k==kk & df$A==a & df$C==0;          ev <- idx & df$D==1 }
    sum(w[ev]) / sum(w[idx])
  })
}
chain_CIF <- function(hazY, hazD = NULL) {
  S <- 1; cif <- 0
  for (kk in 1:K) {
    hD  <- if (!is.null(hazD)) hazD[kk] else 0
    cif <- cif + S * (1 - hD) * hazY[kk]
    S   <- S * (1 - hD) * (1 - hazY[kk])
  }
  cif
}
ipw_tot_1 <- chain_CIF(nonparam_hazard(w_total, 1, "Y"), nonparam_hazard(w_total, 1, "D"))
ipw_tot_0 <- chain_CIF(nonparam_hazard(w_total, 0, "Y"), nonparam_hazard(w_total, 0, "D"))
ipw_net_1 <- chain_CIF(nonparam_hazard(w_net, 1, "Y"))
ipw_net_0 <- chain_CIF(nonparam_hazard(w_net, 0, "Y"))

predO_aD0 <- 1 - predict(modD, newdata = transform(df, A = 0), type = "response")
predO_aY1 <- 1 - predict(modD, newdata = transform(df, A = 1), type = "response")
w_sep <- (ave(predO_aD0, df$id, FUN = cumprod) / ave(predO_aY1, df$id, FUN = cumprod)) * w_total
n_A1 <- sum(df_base$A == 1)
ipw_sep <- sum(sapply(1:K, function(kk) {
  idx <- df$k==kk & df$A==1 & df$C==0 & df$D==0
  sum(w_sep[idx & df$Y==1]) / n_A1
}))

effet_total_ipw     <- ipw_tot_1 - ipw_tot_0
risque_net_ipw      <- ipw_net_1 - ipw_net_0
effet_separable_ipw <- ipw_sep - ipw_tot_0

# --- Récapitulatif : confusion et censure dépendante (Partie 1)

res1 <- data.frame(
  Analyse    = c("KM brut (non ajusté)", "IPTW", "G-formula (= risque net)", "IPTW × IPCW"),
  Ajustement = c("Aucun", "Score de propension (illustratif, essai randomisé)",
                 "Modèle de hazard", "Censure dépendante"),
  S36_A0     = round(c(s36_brut[1], s36_iptw[1], s36_gform[1], s36_combine[1]), 3),
  S36_A1     = round(c(s36_brut[2], s36_iptw[2], s36_gform[2], s36_combine[2]), 3),
  Diff       = round(c(diff(s36_brut), diff(s36_iptw), diff(s36_gform), diff(s36_combine)), 3)
)
knitr::kable(res1, align = "llrrr",
             col.names = c("Analyse", "Ajustement", "S(36) - A=0", "S(36) - A=1", "Δ survie"))

# --- Récapitulatif : risques compétitifs et estimands (Parties 2 et 3)

res2 <- data.frame(
  Estimand = c("Effet total", "Risque net", "Effet séparable"),
  `G-formula` = round(c(effet_total_g, risque_net_g, effet_separable_g), 3),
  `IPW`       = round(c(effet_total_ipw, risque_net_ipw, effet_separable_ipw), 3),
  check.names = FALSE
)
knitr::kable(res2, col.names = c("Estimand", "Différence de risque, g-formula",
                                  "Différence de risque, IPW"))

res3 <- data.frame(
  Decomposition = c("Direct(aD=0) + Indirect(aY=1)", "Indirect(aY=0) + Direct(aD=1)"),
  Somme = round(c(direct_aD0 + indirect_aY1, indirect_aY0 + direct_aD1), 3),
  `Effet total` = round(c(effet_total_g, effet_total_g), 3),
  check.names = FALSE
)
knitr::kable(res3, col.names = c("Décomposition", "Somme", "Effet total (g-formula)"))


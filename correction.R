# ========================================================================
# Script de correction : TP Inference causale en epidemiologie
# ========================================================================

library(survival)
library(dplyr)
library(cobalt)

# Chargement des donnees
df <- read.csv("df.csv")
df <- df |>
  group_by(id) |>
  mutate(A0 = first(A), L0 = first(L)) |>
  ungroup()


# ========================================================================
# Presentation des donnees
# ========================================================================

# --- Chargement et exploration

library(survival)
library(dplyr)
df <- read.csv("df.csv")

head(df)
dim(df)                    # dimensions de la base
length(unique(df$id))      # nombre d'individus
sum(df$D)                  # nombre total de décès
df[df$id %in% 1:3, ]       # 3 premiers individus (plusieurs lignes chacun)

# --- Analyse brute (non ajustée)

df <- df |>
  group_by(id) |>
  mutate(A0 = first(A)) |>
  ungroup()

km_brut <- survfit(Surv(T.start, T.stop, D) ~ A0, data = df)

plot(km_brut,
     col  = c("#1D2769", "#AC182E"), lwd = 2,
     xlab = "Temps (années)", ylab = "Probabilité de survie",
     main = "Kaplan-Meier brut selon A0 (non ajusté)")
legend("bottomleft",
       legend = c("A0 = 0 (non-exposés)", "A0 = 1 (exposés)"),
       col = c("#1D2769", "#AC182E"), lwd = 2)

## Survie à 3 ans
summary(km_brut, times = 3)


# ========================================================================
# Partie 1 : G-computation
# ========================================================================

# --- Étape 0 - Créer la base baseline

## On résume df (format long) en une ligne par individu :
## - first() pour les covariables mesurées à t=0
## - last() pour le temps de suivi total et le statut final
df_base <- df |>
  group_by(id) |>
  summarise(
    A0   = first(A),     # exposition initiale
    L0   = first(L),     # facteur de confusion initial
    X    = first(X),     # covariable initiale
    time = last(T.stop), # temps total de suivi
    D    = last(D)       # décès (0/1) : statut en fin de suivi
  )

head(df_base)
nrow(df_base)  # doit valoir le nombre d'individus distincts

# --- Étape 1 - Estimer les modèles de résultat

## Régression logistique sur D dans chaque groupe d'exposition séparément :
## on ajuste sur X et L0 pour contrôler la confusion
## (deux modèles séparés = équivalent à une interaction complète avec A0)
mod1 <- glm(D ~ X + L0,
            data   = df_base[df_base$A0 == 1, ],
            family = binomial)  # critère binaire -> famille binomiale (logit)

mod0 <- glm(D ~ X + L0,
            data   = df_base[df_base$A0 == 0, ],
            family = binomial)

summary(mod1)
summary(mod0)

# --- Étape 2 - Prédire les devenirs contrefactuels

## Probabilité de décès si tout le monde avait A0 = 1
y1 <- predict(mod1, newdata = df_base, type = "response")

## Probabilité de décès si tout le monde avait A0 = 0
y0 <- predict(mod0, newdata = df_base, type = "response")

head(data.frame(id = df_base$id, A0_obs = df_base$A0,
                pred_A0_1 = round(y1, 3),
                pred_A0_0 = round(y0, 3)))

# --- Étape 3 - Calculer l'effet causal moyen (ATE)

E_Y1 <- mean(y1)
E_Y0 <- mean(y0)

cat("E(D^1)                       =", round(E_Y1, 3), "\n")
cat("E(D^0)                       =", round(E_Y0, 3), "\n")
cat("ATE = E(D^1) - E(D^0)        =", round(E_Y1 - E_Y0, 3), "\n")
cat("RR  = E(D^1) / E(D^0)        =", round(E_Y1 / E_Y0, 3), "\n")

# ~~~ BONUS - Intervalle de confiance par bootstrap
# (section bonus : a faire chez vous)

set.seed(123)
B <- 200
ate_boot <- numeric(B)

for (b in 1:B) {
  ## Ré-échantillonnage avec remise (même taille que df_base)
  idx  <- sample(nrow(df_base), replace = TRUE)
  df_b <- df_base[idx, ]

  ## Réajuster les deux modèles sur l'échantillon bootstrap
  m1 <- glm(D ~ X + L0, data = df_b[df_b$A0 == 1, ], family = binomial)
  m0 <- glm(D ~ X + L0, data = df_b[df_b$A0 == 0, ], family = binomial)

  ## Prédire pour tous les individus du bootstrap sous chaque scénario
  y1_b <- predict(m1, newdata = df_b, type = "response")
  y0_b <- predict(m0, newdata = df_b, type = "response")

  ## Stocker l'ATE de cet échantillon bootstrap
  ate_boot[b] <- mean(y1_b) - mean(y0_b)
}

## L'ATE est celui estimé à l'Étape 3 (E_Y1 - E_Y0), pas la moyenne des bootstrap
cat("ATE    :", round(E_Y1 - E_Y0, 3), "\n")
cat("IC 95% :", round(quantile(ate_boot, c(0.025, 0.975)), 3), "\n")

# --- Étape 1 - Un modèle de Cox avec interaction

## Un seul modèle de Cox avec interaction A0 × covariables.
## Hypothèse : les deux groupes partagent le même risque de base h0(t).
## Deux modèles séparés lèveraient cette contrainte.
## Surv(time, D) : time = temps de suivi total, D = indicateur de décès
mod_cox <- coxph(Surv(time, D) ~ A0 * (X + L0), data = df_base)

summary(mod_cox)

# --- Étape 2 - Hazard cumulatif de base et prédicteurs linéaires individuels

## Bases contrefactuelles
df1 <- df_base; df1$A0 <- 1
df0 <- df_base; df0$A0 <- 0

## Hazard cumulatif de base unique (estimateur de Breslow)
## centered = FALSE : hazard de base au niveau de référence (X=0, L0=0, A0=0)
## suppressWarnings : Cox avec interaction génère un avertissement sans danger
bh <- suppressWarnings(basehaz(mod_cox, centered = FALSE))
head(bh)  # time = temps d'événement, hazard = H0(t) cumulé

## Prédicteurs linéaires individuels sous chaque scénario contrefactuel
lp1 <- predict(mod_cox, newdata = df1, type = "lp")
lp0 <- predict(mod_cox, newdata = df0, type = "lp")

head(data.frame(
  id         = df_base$id,
  A0_obs     = df_base$A0,
  lp_si_A1   = round(lp1, 3),
  lp_si_A0   = round(lp0, 3)
))

# --- Étape 3 - Courbes de survie individuelles et marginalisation

## 1. Grille de temps : tous les temps d'événement de df_base
t_grid <- sort(unique(df_base$time))

## 2. H0(t) unique évalué sur t_grid via une fonction en escalier
## stepfun crée une fonction en escalier : c(0, bh$hazard) indique que
## le hazard cumulatif vaut 0 avant le premier temps d'événement
H_fun  <- stepfun(bh$time, c(0, bh$hazard))
H_grid <- H_fun(t_grid)   # vecteur longueur T

## 3. Survie individuelle : S_i(t_j) = exp(-H0(t_j) * exp(lp_i))
##    outer(-exp(lp), H_grid)[i, j] = -exp(lp[i]) * H_grid[j]
S1_mat <- exp(outer(-exp(lp1), H_grid))  # matrice n × T, scénario a0=1
S0_mat <- exp(outer(-exp(lp0), H_grid))  # matrice n × T, scénario a0=0

## 4. Courbes marginales : moyenne sur les individus (ligne = individu)
S1_marg <- colMeans(S1_mat)
S0_marg <- colMeans(S0_mat)

head(data.frame(t     = round(t_grid, 2),
                S_a1  = round(S1_marg, 3),
                S_a0  = round(S0_marg, 3)))

# --- Étape 4 - Visualisation des courbes de survie contrefactuelles

## type = "s" : courbe en escalier, adaptée aux estimateurs de survie discrets
plot(t_grid, S1_marg, type = "s", col = "blue", lwd = 2,
     ylim = c(0, 1),
     xlab = "Temps (années)",
     ylab = "Probabilité de survie",
     main = "G-computation (Cox) - Courbes contrefactuelles")
lines(t_grid, S0_marg, type = "s", col = "red", lwd = 2)
legend("bottomleft",
       c("Scénario a₀=1 (tous exposés)",
         "Scénario a₀=0 (aucun exposé)"),
       col = c("blue", "red"), lty = 1, lwd = 2, bty = "n")

# --- Étape 5 - Différence de survie à [formule] ans

## t_grid ne contient pas nécessairement exactement t=3 : on prend
## le dernier temps d'événement inférieur ou égal à 3
idx3 <- max(which(t_grid <= 3))

S1_3 <- S1_marg[idx3]
S0_3 <- S0_marg[idx3]

cat("S^(a0=1)(3) =", round(S1_3, 3), "\n")
cat("S^(a0=0)(3) =", round(S0_3, 3), "\n")
cat("Différence de survie à 3 ans :", round(S1_3 - S0_3, 3), "\n")
cat("Rapport de survie à 3 ans    :", round(S1_3 / S0_3, 3), "\n")

# --- Intervalle de confiance par bootstrap (modèle de Cox)

set.seed(42)
B <- 200
S1_boot <- numeric(B)
S0_boot <- numeric(B)

for (b in 1:B) {
  ## 1. Ré-échantillonnage avec remise
  idx <- sample(nrow(df_base), replace = TRUE)
  db  <- df_base[idx, ]

  ## 2. Modèle de Cox avec interaction sur l'échantillon bootstrap
  m <- suppressWarnings(coxph(Surv(time, D) ~ A0 * (X + L0), data = db))

  ## 3. Bases contrefactuelles, hazard de base et prédicteurs linéaires
  db1 <- db; db1$A0 <- 1
  db0 <- db; db0$A0 <- 0
  bhb  <- suppressWarnings(basehaz(m, centered = FALSE))
  lp1b <- predict(m, newdata = db1, type = "lp")
  lp0b <- predict(m, newdata = db0, type = "lp")

  ## 4. H0(3) : dernier hazard cumulatif <= 3 (0 si aucun événement avant 3 ans)
  H_3b <- if (any(bhb$time <= 3)) tail(bhb$hazard[bhb$time <= 3], 1) else 0

  ## S_i(3) = exp(-H0(3) * exp(lp_i)), puis moyenne sur les individus
  S1_boot[b] <- mean(exp(-H_3b * exp(lp1b)))
  S0_boot[b] <- mean(exp(-H_3b * exp(lp0b)))
}

diff_boot <- S1_boot - S0_boot
## L'estimée est celle calculée avant le bootstrap (S1_3 - S0_3)
## Le bootstrap ne sert qu'à l'intervalle de confiance
cat("Différence de survie à 3 ans (G-computation Cox) :\n")
cat("  Estimée :", round(S1_3 - S0_3, 3), "\n")
cat("  IC 95%  :", round(quantile(diff_boot, c(0.025, 0.975)), 3), "\n")


# ========================================================================
# Partie 2 : IPTW
# ========================================================================

# --- Étape 1 - Estimer le score de propension

## Base une ligne par individu
df_base <- df |>
  group_by(id) |>
  summarise(A0 = first(A), L0 = first(L), X = first(X)) |>
  ungroup()

## Modèle de propension : on modélise l'exposition A0 en fonction des covariables
## (à la différence de la G-computation qui modélisait l'outcome D)
mod.ps <- glm(A0 ~ X + L0, data = df_base, family = "binomial")

## type = "response" : probabilités prédites P(A0=1 | X, L0), pas le log-odds
df_base$ps <- predict(mod.ps, type = "response")

## Distribution du PS par groupe
summary(df_base$ps[df_base$A0 == 1])
summary(df_base$ps[df_base$A0 == 0])

## Vérifier le chevauchement : si les deux distributions sont bien distinctes,
## la positivité est menacée et les poids seront instables
hist(df_base$ps[df_base$A0 == 1], breaks = 20, col = "#AC182E80",
     main = "Distribution du score de propension",
     xlab = "Score de propension", xlim = c(0, 1))
hist(df_base$ps[df_base$A0 == 0], breaks = 20, col = "#1D276980", add = TRUE)
legend("topright", legend = c("A0 = 1", "A0 = 0"),
       fill = c("#AC182E80", "#1D276980"), bty = "n")

# --- Étape 2 - Calculer les poids IPTW

## Poids non stabilisés : w = 1/ps si exposé, 1/(1-ps) si non exposé
df_base$iptw <- (df_base$A0 == 1) / df_base$ps +
                (df_base$A0 == 0) / (1 - df_base$ps)

## Numérateur des poids stabilisés : probabilité marginale d'exposition
## (sans covariables) — réduit la variance des poids par rapport aux non stabilisés
p.A1 <- mean(df_base$A0)
p.A0 <- 1 - p.A1

## Poids stabilisés : w^s = P(A0) / ps si exposé, P(1-A0) / (1-ps) si non exposé
df_base$iptw.s <- ifelse(df_base$A0 == 1,
                         p.A1 / df_base$ps,
                         p.A0 / (1 - df_base$ps))

## Les poids stabilisés ont une moyenne proche de 1 : vérifier des valeurs
## extrêmes (> 10-20) qui signaleraient un problème de positivité
cat("Poids non stabilisés - moyenne:", round(mean(df_base$iptw), 3),
    " / max:", round(max(df_base$iptw), 2), "\n")
cat("Poids stabilisés     - moyenne:", round(mean(df_base$iptw.s), 3),
    " / max:", round(max(df_base$iptw.s), 2), "\n")

## Fusionner dans df (format long) pour l'analyse de survie pondérée
df <- df |> left_join(df_base |> select(id, ps, iptw, iptw.s), by = "id")

# --- Étape 3 - Vérifier l'équilibre

library(cobalt)

## bal.tab() : différences standardisées (SMD) pour chaque covariable
## binary = "std" : SMD pour les variables binaires, un = TRUE : affiche aussi avant pondération
bal <- bal.tab(A0 ~ X + L0,
               data    = df_base,
               weights = df_base$iptw.s,
               method  = "weighting",
               binary  = "std",
               un      = TRUE)
bal

## Love plot : SMD < 0,1 après pondération indique un équilibre satisfaisant
love.plot(bal,
          thresholds = c(m = 0.1),
          colors     = c("#AC182E", "#1D2769"),
          shapes     = c("circle", "triangle"),
          title      = "Équilibre avant/après IPTW")

# --- Étape 4 - Analyse de survie pondérée (Kaplan-Meier)

## Surv(T.start, T.stop, D) : format comptage (plusieurs lignes par individu)
## weights = iptw.s : chaque observation est pondérée par son poids IPTW stabilisé
km.iptw <- survfit(Surv(T.start, T.stop, D) ~ A0,
                   data    = df,
                   weights = iptw.s)

plot(km.iptw,
     col  = c("#1D2769", "#AC182E"), lwd = 2, conf.int = FALSE,
     xlab = "Temps (années)", ylab = "Probabilité de survie",
     main = "Kaplan-Meier pondéré (IPTW stabilisé)")
legend("bottomleft",
       legend = c("A0 = 0 (non-exposés)", "A0 = 1 (exposés)"),
       col = c("#1D2769", "#AC182E"), lwd = 2, bty = "n")

## Différence de survie à 3 ans (S^(a0=1)(3) - S^(a0=0)(3))
s3 <- summary(km.iptw, times = 3)$surv
cat("S(3 | a0=0)                        =", round(s3[1], 3), "\n")
cat("S(3 | a0=1)                        =", round(s3[2], 3), "\n")
cat("Diff. survie = S(a0=1) - S(a0=0)   =", round(s3[2] - s3[1], 3), "\n")

# --- Comparaison G-computation vs IPTW

## D final : statut décès à la fin du suivi (dernière ligne par individu)
df_base$D <- df |> group_by(id) |> summarise(D = last(D)) |> pull(D)

## Moyenne pondérée de D dans chaque groupe : équivalent IPTW du critère binaire
m1_bin <- weighted.mean(df_base$D[df_base$A0 == 1],
                        df_base$iptw.s[df_base$A0 == 1])
m0_bin <- weighted.mean(df_base$D[df_base$A0 == 0],
                        df_base$iptw.s[df_base$A0 == 0])
ate_iptw_bin <- m1_bin - m0_bin

cat("=== IPTW (critère binaire) ===\n")
cat("E(D^1)                       =", round(m1_bin, 3), "\n")
cat("E(D^0)                       =", round(m0_bin, 3), "\n")
cat("ATE = E(D^1) - E(D^0)        =", round(ate_iptw_bin, 3), "\n\n")

s3 <- summary(km.iptw, times = 3)$surv
cat("=== IPTW (KM pondéré, critère censuré) ===\n")
cat("S(3 | a0=1)                  =", round(s3[2], 3), "\n")
cat("S(3 | a0=0)                  =", round(s3[1], 3), "\n")
cat("Diff. survie = S(1) - S(0)   =", round(s3[2] - s3[1], 3), "\n\n")

cat("=== Vérification du lien ATE ≈ -diff(survie) ===\n")
cat("ATE + diff(survie) =", round(ate_iptw_bin + (s3[2] - s3[1]), 3),
    "  (doit être proche de 0)\n")


# ========================================================================
# Partie 3 : IPCW
# ========================================================================

# --- Étape 1 - Définir les deux groupes de stratégie

## On sépare les individus selon leur stratégie initiale A0 :
## les poids IPCW et la censure artificielle seront calculés séparément
## dans chaque groupe (déviation = arrêt pour df.1, initiation pour df.0)
df.1 <- df[df$A0 == 1, ]
df.0 <- df[df$A0 == 0, ]

cat("Individus avec A0=1 :", length(unique(df.1$id)), "\n")
cat("Individus avec A0=0 :", length(unique(df.0$id)), "\n")

# --- Étape 2 - Censure artificielle dans le groupe A0 = 1

df.1 <- df.1 |>
  group_by(id) |>
  mutate(
    ## cumsumA compte le nombre de visites où A=1 depuis le début
    cumsumA = cumsum(A == 1),
    ## Si l'individu n'a jamais dévié, cumsumA = nombre de visites écoulées = T.start + 1
    ## Si cumsumA < T.start + 1, il y a eu au moins une visite avec A=0 -> déviation
    switchA = if_else(cumsumA == T.start + 1, 0L, 1L),
    ## cumsum(switchA) : une fois la déviation détectée, switchA reste >= 1
    switchA = cumsum(switchA)
  ) |>
  filter(switchA <= 1) |>  # garder jusqu'à la première déviation incluse
  ungroup()

table(df.1$switchA)

# --- Étape 3 - Modèle de déviation et poids IPCW (groupe A0 = 1)

## Régression logistique poolée : modélise P(switch=1) à chaque visite
## as.factor(T.start) : effet du temps, X et L : covariables de confusion
wt.mod.1 <- glm(switchA ~ as.factor(T.start) + X + L,
                family = "binomial", data = df.1)
## 1 - predict(...) : probabilité de NE PAS dévier à cette visite
df.1$wt.denom <- 1 - predict(wt.mod.1, type = "response", newdata = df.1)

## Numérateur : même modèle sans covariables (probabilité marginale de ne pas dévier)
wt.mod.1.num  <- glm(switchA ~ as.factor(T.start),
                     family = "binomial", data = df.1)
df.1$wt.num   <- 1 - predict(wt.mod.1.num, type = "response", newdata = df.1)

## Supprimer la ligne de déviation : on ne garde que les périodes sans déviation
df.1 <- df.1[df.1$switchA == 0, ]

## cumprod : produit cumulé des termes période par période -> poids croissant dans le temps
df.1 <- df.1 |>
  group_by(id) |>
  mutate(wt   = cumprod(1 / wt.denom),          # non stabilisé
         wt.s = cumprod(wt.num / wt.denom)) |>  # stabilisé
  ungroup()

cat("Poids non stabilisés - moy:", round(mean(df.1$wt), 3),
    " / max:", round(max(df.1$wt), 2), "\n")
cat("Poids stabilisés     - moy:", round(mean(df.1$wt.s), 3),
    " / max:", round(max(df.1$wt.s), 2), "\n")

# --- Étape 4 - Répéter pour le groupe A0 = 0

## Même logique que pour df.1, mais la déviation est ici A=1 (passage de 0 à 1)
## cumsumA compte donc le nombre de visites où A=0
df.0 <- df.0 |>
  group_by(id) |>
  mutate(
    cumsumA = cumsum(A == 0),
    switchA = if_else(cumsumA == T.start + 1, 0L, 1L),
    switchA = cumsum(switchA)
  ) |>
  filter(switchA <= 1) |>
  ungroup()

## Modèle poolé de déviation dans df.0 (avec covariables)
wt.mod.0      <- glm(switchA ~ as.factor(T.start) + X + L,
                     family = "binomial", data = df.0)
df.0$wt.denom <- 1 - predict(wt.mod.0, type = "response", newdata = df.0)

## Numérateur (sans covariables)
wt.mod.0.num  <- glm(switchA ~ as.factor(T.start),
                     family = "binomial", data = df.0)
df.0$wt.num   <- 1 - predict(wt.mod.0.num, type = "response", newdata = df.0)

df.0          <- df.0[df.0$switchA == 0, ]
df.0          <- df.0 |>
  group_by(id) |>
  mutate(wt   = cumprod(1 / wt.denom),
         wt.s = cumprod(wt.num / wt.denom)) |>
  ungroup()

## Empilement
dfpp <- rbind(df.1, df.0)
cat("Lignes dans dfpp :", nrow(dfpp), "\n")

# --- Étape 5 - Poids combinés IPTW × IPCW

## Poids combinés : IPTW stabilisé × IPCW stabilisé
dfpp$comb.wt <- dfpp$iptw.s * dfpp$wt.s

cat("Poids IPCW stabilisés - moy:", round(mean(dfpp$wt.s), 3),
    " / max:", round(max(dfpp$wt.s), 2), "\n")
cat("Poids combinés        - moy:", round(mean(dfpp$comb.wt), 3),
    " / max:", round(max(dfpp$comb.wt), 2), "\n")

# --- Étape 6 - Analyse de survie per-protocol

km.pp <- survfit(Surv(T.start, T.stop, D) ~ A0,
                 data    = dfpp,
                 weights = comb.wt)

plot(km.pp,
     col  = c("#1D2769", "#AC182E"), lwd = 2, conf.int = FALSE,
     xlab = "Temps (années)", ylab = "Probabilité de survie",
     main = "Kaplan-Meier per-protocol (IPTW × IPCW)")
legend("bottomleft",
       legend = c("Stratégie ā=0 (jamais exposé)",
                  "Stratégie ā=1 (toujours exposé)"),
       col = c("#1D2769", "#AC182E"), lwd = 2, bty = "n")

## Différence de survie à 3 ans
s3.pp <- summary(km.pp, times = 3)$surv
cat("Survie à 3 ans - ā=0 :", round(s3.pp[1], 3), "\n")
cat("Survie à 3 ans - ā=1 :", round(s3.pp[2], 3), "\n")
cat("Différence     :", round(diff(s3.pp), 3), "\n")


# ========================================================================
# Conclusion : comparaison des methodes
# ========================================================================

# --- Calcul de toutes les analyses (recapitulatif complet)

df <- read.csv("df.csv")
library(survival); library(dplyr)
df <- df |>
  group_by(id) |>
  mutate(A0 = first(A), L0 = first(L)) |>
  ungroup()

## Analyse brute
km.brut <- survfit(Surv(T.start, T.stop, D) ~ A0, data = df)
s3.brut <- summary(km.brut, times = 3)$surv

## IPTW
mod.ps    <- glm(A0 ~ X + L0, data = df[df$T.start == 0, ], family = "binomial")
df$ps     <- predict(mod.ps, newdata = df, type = "response")
p.A1      <- mean(df$A0[df$T.start == 0])
df$iptw.s <- ifelse(df$A0 == 1, p.A1 / df$ps, (1 - p.A1) / (1 - df$ps))
km.iptw   <- survfit(Surv(T.start, T.stop, D) ~ A0, data = df, weights = iptw.s)
s3.iptw   <- summary(km.iptw, times = 3)$surv

## IPTW x IPCW (per-protocol)
df.1 <- df[df$A0 == 1, ] |>
  group_by(id) |>
  mutate(cumsumA = cumsum(A == 1),
         switchA = if_else(cumsumA == T.start + 1, 0L, 1L),
         switchA = cumsum(switchA)) |>
  filter(switchA <= 1) |> ungroup()
wt.mod.1      <- glm(switchA ~ as.factor(T.start) + X + L,
                     family = "binomial", data = df.1)
df.1$wt.denom <- 1 - predict(wt.mod.1, type = "response", newdata = df.1)
wt.mod.1.num  <- glm(switchA ~ as.factor(T.start), family = "binomial", data = df.1)
df.1$wt.num   <- 1 - predict(wt.mod.1.num, type = "response", newdata = df.1)
df.1 <- df.1[df.1$switchA == 0, ] |>
  group_by(id) |>
  mutate(wt.s = cumprod(wt.num / wt.denom)) |> ungroup()

df.0 <- df[df$A0 == 0, ] |>
  group_by(id) |>
  mutate(cumsumA = cumsum(A == 0),
         switchA = if_else(cumsumA == T.start + 1, 0L, 1L),
         switchA = cumsum(switchA)) |>
  filter(switchA <= 1) |> ungroup()
wt.mod.0      <- glm(switchA ~ as.factor(T.start) + X + L,
                     family = "binomial", data = df.0)
df.0$wt.denom <- 1 - predict(wt.mod.0, type = "response", newdata = df.0)
wt.mod.0.num  <- glm(switchA ~ as.factor(T.start), family = "binomial", data = df.0)
df.0$wt.num   <- 1 - predict(wt.mod.0.num, type = "response", newdata = df.0)
df.0 <- df.0[df.0$switchA == 0, ] |>
  group_by(id) |>
  mutate(wt.s = cumprod(wt.num / wt.denom)) |> ungroup()

dfpp         <- rbind(df.1, df.0)
dfpp$comb.wt <- dfpp$iptw.s * dfpp$wt.s
km.pp        <- survfit(Surv(T.start, T.stop, D) ~ A0, data = dfpp, weights = comb.wt)
s3.pp        <- summary(km.pp, times = 3)$surv

## G-computation Cox (méthode du Bonus - Partie 1)
## Modèle poolé avec interaction A0 × (X, L0) pour éviter l'extrapolation
## instable des modèles séparés sur sous-groupes
df_base <- df |>
  group_by(id) |>
  summarise(A0 = first(A), L0 = first(L), X = first(X),
            time = last(T.stop), D = last(D))
mod_cox <- coxph(Surv(time, D) ~ A0 * (X + L0), data = df_base)
bh      <- suppressWarnings(basehaz(mod_cox, centered = FALSE))
H_fun   <- stepfun(bh$time, c(0, bh$hazard))
df1 <- df0 <- df_base; df1$A0 <- 1; df0$A0 <- 0
lp1 <- predict(mod_cox, newdata = df1, type = "lp")
lp0 <- predict(mod_cox, newdata = df0, type = "lp")
t_gcomp <- sort(unique(df_base$time))
S1_marg <- colMeans(exp(outer(-exp(lp1), H_fun(t_gcomp))))
S0_marg <- colMeans(exp(outer(-exp(lp0), H_fun(t_gcomp))))
idx3    <- max(which(t_gcomp <= 3))
s3.gcomp <- c(S0_marg[idx3], S1_marg[idx3])

# --- Comparaison graphique

col0 <- "#1D2769"; col1 <- "#AC182E"

## Palette et types de lignes par méthode
## lty : 1=brut, 2=gcomp, 3=iptw, 4=pp
## col : bleu=A0=0, rouge=A0=1

plot(km.brut,
     col = c(col0, col1), lwd = 2, lty = 1,
     ylim = c(0, 1), xlim = c(0, 3), conf.int = FALSE,
     xlab = "Temps (années)", ylab = "Probabilité de survie",
     main = "Courbes de survie selon l'analyse")

## G-computation Cox (Bonus Partie 1)
lines(t_gcomp, S0_marg, col = col0, lwd = 2, lty = 2, type = "s")
lines(t_gcomp, S1_marg, col = col1, lwd = 2, lty = 2, type = "s")

## IPTW (analogue-ITT)
lines(km.iptw, col = c(col0, col1), lwd = 2, lty = 3, conf.int = FALSE)

## IPTW × IPCW (per-protocol)
lines(km.pp, col = c(col0, col1), lwd = 2, lty = 4, conf.int = FALSE)

## Légende méthodes
legend("bottomleft", bty = "n", lwd = 2, lty = 1:4, col = "gray30",
       legend = c("KM brut (non ajusté)",
                  "G-computation Cox - analogue-ITT [Bonus P.1]",
                  "IPTW - analogue-ITT",
                  "IPTW × IPCW - per-protocol"))

## Légende groupes
legend("topright", bty = "n", lwd = 3, lty = 1,
       col = c(col0, col1),
       legend = c(expression(A[0] == 0), expression(A[0] == 1)))

# --- Récapitulatif des estimations

res <- data.frame(
  Analyse    = c("KM brut (non ajusté)",
                 "Q1 - G-computation Cox [Bonus P.1]",
                 "Q1 - IPTW",
                 "Q2 - IPTW × IPCW (per-protocol)"),
  Estimand   = c("-",
                 "Analogue-ITT : effet d'initier l'exposition",
                 "Analogue-ITT : effet d'initier l'exposition",
                 "Analogue per-protocol : effet de maintenir l'exposition"),
  Ajustement = c("Aucun",
                 "Confusion initiale (X, L₀) - modèle du résultat",
                 "Confusion initiale (X, L₀) - modèle de l'exposition",
                 "Confusion initiale + déviation de stratégie"),
  S3_A0      = round(c(s3.brut[1], s3.gcomp[1], s3.iptw[1], s3.pp[1]), 3),
  S3_A1      = round(c(s3.brut[2], s3.gcomp[2], s3.iptw[2], s3.pp[2]), 3),
  Diff       = round(c(diff(s3.brut), diff(s3.gcomp), diff(s3.iptw), diff(s3.pp)), 3)
)
knitr::kable(res, align = "llllrrr",
             col.names = c("Analyse", "Estimand", "Ajustement",
                           "S(3) - A₀=0", "S(3) - A₀=1",
                           "Δ survie"))


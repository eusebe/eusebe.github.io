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


# ========================================================================
# Partie 1 : G-computation
# ========================================================================


# ========================================================================
# Partie 2 : IPTW
# ========================================================================


# ========================================================================
# Partie 3 : IPCW
# ========================================================================

# ~~~ BONUS 2 - Clonage, censure, pondération : IPCW seul
# (section bonus : a faire chez vous)

## ── Étape 1 : Cloner ────────────────────────────────────────────────────────
## Chaque individu reçoit deux clones : un assigné à la stratégie ā=1, l'autre ā=0
df.c1 <- df; df.c1$strategy <- 1
df.c0 <- df; df.c0$strategy <- 0
df.clone <- rbind(df.c1, df.c0)

## ── Étape 2 : Censure artificielle selon la stratégie assignée ───────────────
df.clone <- df.clone |>
  group_by(id, strategy) |>
  mutate(
    cumsumA = if_else(strategy == 1, cumsum(A == 1), cumsum(A == 0)),
    switchA  = if_else(cumsumA == T.start + 1, 0L, 1L),
    switchA  = cumsum(switchA)
  ) |>
  filter(switchA <= 1) |>
  ungroup()

## ── Étape 3 : IPCW seul, modèles séparés par groupe de clones ───────────────
## À T.start = 0, strategy = 1 : déviation = I(A = 0) → P(pas de déviation | X, L) = P(A=1|X,L)
## À T.start = 0, strategy = 0 : déviation = I(A = 1) → P(pas de déviation | X, L) = P(A=0|X,L)
## Le terme T.start = 0 du modèle par groupe absorbe exactement le poids IPTW.
## Les modèles DOIVENT être séparés car les effets de X et L sont opposés.
dc1 <- df.clone[df.clone$strategy == 1, ]
dc0 <- df.clone[df.clone$strategy == 0, ]

wd1 <- glm(switchA ~ as.factor(T.start) + X + L, family = "binomial", data = dc1)
wn1 <- glm(switchA ~ as.factor(T.start),          family = "binomial", data = dc1)
dc1$wt.denom <- 1 - predict(wd1, type = "response", newdata = dc1)
dc1$wt.num   <- 1 - predict(wn1, type = "response", newdata = dc1)

wd0 <- glm(switchA ~ as.factor(T.start) + X + L, family = "binomial", data = dc0)
wn0 <- glm(switchA ~ as.factor(T.start),          family = "binomial", data = dc0)
dc0$wt.denom <- 1 - predict(wd0, type = "response", newdata = dc0)
dc0$wt.num   <- 1 - predict(wn0, type = "response", newdata = dc0)

df.clone <- rbind(dc1, dc0)
df.clone <- df.clone[df.clone$switchA == 0, ] |>
  group_by(id, strategy) |>
  mutate(ipcw.s = cumprod(wt.num / wt.denom)) |>
  ungroup()

## KM pondéré IPCW seul (approche clonage)
km.clone <- survfit(Surv(T.start, T.stop, D) ~ strategy,
                    data = df.clone, weights = ipcw.s)


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


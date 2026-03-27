# ============================================================
#  NF1 Neurostimulation – GABA/NAA Analysis
#  Mixed-effects model: GABA/NAA ~ Time * Stimulation
#  Updated script (random intercepts, Gaba_Stim_Complete.csv)
# ============================================================

# --- Libraries -----------------------------------------------
library(car)
library(ggplot2)
library(dplyr)
library(ggdist)
library(emmeans)
library(gridExtra)
library(tidyr)
library(conflicted)
library(lme4)
library(lmerTest)

# Conflict management
conflicts_prefer(lmerTest::lmer)
conflicts_prefer(dplyr::filter)


# --- Read in the data ----------------------------------------
# Set your working directory to the folder containing this file first,
# or replace the filename with the full file path.

gaba_data <- read.csv("Gaba_Stim_Complete.csv")

# Inspect
gaba_data


# --- Factor conversion ---------------------------------------
# NOTE: Stimulation levels use "Sham" (capital S) to match the CSV.
# Reference level is Sham so that tDCS and tACS contrasts are vs. control.

gaba_data$ParticipantID <- factor(gaba_data$ParticipantID)

gaba_data$Stimulation <- factor(
  gaba_data$Stimulation,
  levels = c("Sham", "tDCS", "tACS")   # Sham = reference (capitalised)
)

gaba_data$Time <- factor(
  gaba_data$Time,
  levels = c("Pre", "Post")
)


# --- Descriptive statistics ----------------------------------

summary_table <- gaba_data %>%
  group_by(Stimulation, Time) %>%
  summarise(
    N        = n(),
    Mean     = mean(GABA, na.rm = TRUE),
    SD       = sd(GABA, na.rm = TRUE),
    SE       = SD / sqrt(N),
    CI_lower = Mean - qt(0.975, N - 1) * SE,
    CI_upper = Mean + qt(0.975, N - 1) * SE,
    .groups  = "drop"
  )

print(summary_table)


# --- Mixed-effects model -------------------------------------
# Random intercepts for participant (accounts for individual baseline
# differences in GABA/NAA).  A random-slopes term (1 + Time | ParticipantID)
# is not used here because the dataset is unbalanced — many participants
# are missing Pre or Post measurements in one or more conditions —
# which prevents reliable estimation of per-participant slopes and
# typically produces a singular fit or non-convergence.
# The random-intercepts model still uses ALL available observations
# via REML and correctly controls for repeated measures.

model <- lmer(
  GABA ~ Time * Stimulation + (1 | ParticipantID),
  data = gaba_data
)

summary(model)

# Fixed-effects ANOVA table (Type III, Satterthwaite df)
anova(model)


# --- Model diagnostics ---------------------------------------

resid_df <- data.frame(
  Fitted    = fitted(model),
  Residuals = resid(model)
)

# 1. Residuals vs Fitted
resid_fitted <- ggplot(resid_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  theme_classic() +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")

# 2. Q-Q plot of residuals
qq_resid <- ggplot(resid_df, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  theme_classic() +
  labs(title = "Q-Q Plot of Residuals")

# 3. Histogram of residuals
hist_resid <- ggplot(resid_df, aes(x = Residuals)) +
  geom_histogram(bins = 20, fill = "steelblue", colour = "black", alpha = 0.8) +
  theme_classic() +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Count")

# 4. Q-Q plot of random intercepts
re <- ranef(model)$ParticipantID
qq_re_intercept <- ggplot(data.frame(RE = re[, 1]), aes(sample = RE)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  theme_classic() +
  labs(title = "Random Intercepts Q-Q")

# Arrange diagnostics
grid.arrange(resid_fitted, qq_resid, hist_resid, qq_re_intercept, ncol = 2)


# --- Post-hoc: Estimated Marginal Means ----------------------
# Pairwise contrasts within each Time point and within each Stimulation
# condition, with Bonferroni correction.

emm <- emmeans(model, ~ Time * Stimulation)
emm

# Contrasts: Pre vs Post within each stimulation condition
pairs(emmeans(model, ~ Time | Stimulation), adjust = "bonferroni")

# Contrasts: between stimulation conditions at each time point
pairs(emmeans(model, ~ Stimulation | Time), adjust = "bonferroni")


# --- Supplementary: Paired t-tests per condition -------------
# (Descriptive check; not the primary inference.)

for (stim in c("Sham", "tDCS", "tACS")) {
  cat("\n--- Paired t-test:", stim, "---\n")
  stim_wide <- gaba_data %>%
    filter(Stimulation == stim) %>%
    select(ParticipantID, Time, GABA) %>%
    pivot_wider(names_from = Time, values_from = GABA) %>%
    filter(!is.na(Pre) & !is.na(Post))   # keep only complete pairs
  print(t.test(stim_wide$Pre, stim_wide$Post, paired = TRUE))
}


# =============================================================
#  DATA VISUALISATION
#  Recommended graphs for a dissertation on GABA/NAA & stimulation
# =============================================================

# Colour palette (colour-blind friendly — Wong 2011 palette)
# tDCS = Orange, tACS = Sky Blue, Sham = Bluish Green
stim_colours <- c("Sham" = "#009E73", "tDCS" = "#E69F00", "tACS" = "#56B4E9")


# --- Graph 1: Model-estimated means (EMMs) with 95% CI -------
# WHY: This is the primary result figure for a mixed model.
# It shows the model-adjusted means (not raw means), correctly
# accounting for the unbalanced design and individual differences.

tim_colours <- c(
  "Sham" = "#009E73",
  "tDCS" = "#E69F00",
  "tACS" = "#56B4E9"
)

graph1_apa <- ggplot(
  emm_df,
  aes(
    x = Time,
    y = GABA_NAA,
    group = Stimulation,
    colour = Stimulation,
    shape = Stimulation
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.8) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    width = 0.06,
    linewidth = 0.6
  ) +
  scale_x_discrete(labels = c("Pre", "Post")) +
  scale_colour_manual(values = tim_colours) +
  scale_shape_manual(values = c("Sham" = 16, "tDCS" = 17, "tACS" = 15)) +
  scale_y_continuous(limits = c(0.08, 0.15)) +
  labs(
    x = "Time",
    y = "GABA/NAA Ratio (a.u.)",
    colour = "Stimulation",
    shape = "Stimulation"
  ) +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x  = element_text(colour = "black"),
    axis.text.y  = element_text(colour = "black"),
    legend.title = element_text(face = "plain", colour = "black"),
    legend.text  = element_text(colour = "black"),
    legend.position = "right",
    legend.background = element_blank(),
    legend.key = element_blank()
  )

print(graph1_apa)

# --- Graph 2: Individual trajectories (raw data) -------------
# WHY: Essential for transparency with a clinical/sparse dataset.
# Shows individual variability and makes missing data visible.
# Faceted by condition so each panel is readable.

graph2 <- ggplot(
  gaba_data,
  aes(
    x = Time,
    y = GABA,
    group = ParticipantID
  )
) +
  geom_line(
    alpha = 0.35,
    colour = "grey50",
    linewidth = 0.7
  ) +
  geom_point(
    aes(colour = Stimulation),
    size = 2.4,
    alpha = 0.8
  ) +
  scale_colour_manual(values = tim_colours) +
  scale_x_discrete(labels = c("Pre", "Post")) +
  facet_wrap(~ Stimulation) +
  labs(
    x = "Time",
    y = "GABA/NAA Ratio"
  ) +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x  = element_text(colour = "black"),
    axis.text.y  = element_text(colour = "black"),
    strip.text   = element_text(colour = "black", face = "plain"),
    legend.position = "none"
  )

print(graph2)


# --- Graph 3: Pre-Post change (delta) — boxplot + jitter -----
# WHY: Directly visualises the direction and magnitude of change
# per condition. More intuitive for readers than raw Pre/Post values.
# Participants missing either Pre or Post are excluded from this plot
# (NA deltas are dropped automatically by ggplot with a warning).

gaba_change <- gaba_data %>%
  pivot_wider(names_from = Time, values_from = GABA) %>%
  mutate(Delta = Post - Pre)

# How many complete pairs per condition?
gaba_change %>%
  group_by(Stimulation) %>%
  summarise(N_complete = sum(!is.na(Delta)))

graph3 <- ggplot(gaba_change, aes(x = Stimulation, y = Delta,
                                  fill = Stimulation)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_boxplot(alpha = 0.45, outlier.shape = NA, width = 0.5) +
  geom_jitter(aes(colour = Stimulation), width = 0.12, size = 2.5, alpha = 0.8) +
  scale_fill_manual(values  = stim_colours) +
  scale_colour_manual(values = stim_colours) +
  theme_classic(base_size = 13) +
  labs(
    title    = "Pre-to-Post GABA/NAA Change by Stimulation Condition",
    subtitle = "Only participants with both Pre and Post included",
    x        = "Stimulation",
    y        = expression(Delta * " GABA/NAA (Post – Pre, a.u.)")
  ) +
  theme(legend.position = "none")

print(graph3)


# --- Graph 4: Raincloud plot of delta scores -----------------
# WHY: Combines distribution (half-violin), summary box, and raw
# data points — the most information-dense single figure and
# increasingly standard in clinical/neuroscience papers.

graph4 <- ggplot(gaba_change, aes(x = Stimulation, y = Delta,
                                  fill = Stimulation,
                                  colour = Stimulation)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  stat_halfeye(
    adjust = 0.6, width = 0.5, justification = -0.2,
    .width = 0, point_colour = NA, alpha = 0.6
  ) +
  geom_boxplot(
    width = 0.12, outlier.shape = NA,
    colour = "black", fill = "white", alpha = 0.7
  ) +
  geom_jitter(width = 0.05, size = 2, alpha = 0.7) +
  scale_fill_manual(values   = stim_colours) +
  scale_colour_manual(values = stim_colours) +
  theme_classic(base_size = 13) +
  labs(
    title    = "Raincloud Plot: GABA/NAA Change by Stimulation Condition",
    subtitle = "Distribution + boxplot + individual data points",
    x        = "Stimulation",
    y        = expression(Delta * " GABA/NAA (Post – Pre, a.u.)")
  ) +
  theme(legend.position = "none")

print(graph4)


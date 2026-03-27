# ============================================================
#  NF1 Neurostimulation – GABA/NAA Analysis
#  Mixed-effects model: GABA/NAA ~ Time * Stimulation
#  Updated script (random intercepts, Gaba_Stim_Complete.csv)
# ============================================================

# Libraries 
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


# Conflict Management
conflicts_prefer(lmerTest::lmer)
conflicts_prefer(dplyr::filter)


# Read in the Data

gaba_data <- read.csv("Gaba_Stim_Complete.csv")


# Inspect
gaba_data


# Factor Conversion

# Reference level is Sham so that tDCS and tACS contrasts are vs. control

gaba_data$ParticipantID <- factor(gaba_data$ParticipantID)

gaba_data$Stimulation <- factor(
  gaba_data$Stimulation,
  levels = c("Sham", "tDCS", "tACS")   # Sham = reference 
)

gaba_data$Time <- factor(
  gaba_data$Time,
  levels = c("Pre", "Post")
)


# Descriptive Statistics 

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


# Mixed-Effects Model

# Random intercepts for participant (accounts for individual baseline
# differences in GABA/NAA).  A random-slopes term (1 + Time | ParticipantID)
# is not used here because the dataset is unbalanced - many participants
# are missing Pre or Post measurements in one or more conditions.

# The random-intercepts model still uses ALL available observations
# via REML and correctly controls for repeated measures.

model <- lmer(
  GABA ~ Time * Stimulation + (1 | ParticipantID),
  data = gaba_data
)

summary(model)

# Fixed-Effects ANOVA Table (Type III, Satterthwaite df)
anova(model)


# Model Diagnostics

resid_df <- data.frame(
  Fitted    = fitted(model),
  Residuals = resid(model)
)

# PLOTS -> all in the same page

# 1. Residuals vs Fitted
resid_fitted <- ggplot(resid_df, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
  theme_classic() +
  labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")

# 2. Q-Q Plot of Residuals*
qq_resid <- ggplot(resid_df, aes(sample = Residuals)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  theme_classic() +
  labs(title = "Q-Q Plot of Residuals")

# 3. Histogram of Residuals*
hist_resid <- ggplot(resid_df, aes(x = Residuals)) +
  geom_histogram(bins = 20, fill = "steelblue", colour = "black", alpha = 0.8) +
  theme_classic() +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Count")

# 4. Q-Q Plot of Random Intercepts*
re <- ranef(model)$ParticipantID
qq_re_intercept <- ggplot(data.frame(RE = re[, 1]), aes(sample = RE)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  theme_classic() +
  labs(title = "Random Intercepts Q-Q")

# Arrange Diagnostics
grid.arrange(resid_fitted, qq_resid, hist_resid, qq_re_intercept, ncol = 2)


# Post-hoc: Estimated Marginal Means - with Bonferroni correction.

emm <- emmeans(model, ~ Time * Stimulation)
emm

# Contrasts: Pre vs Post within each stimulation condition

pairs(emmeans(model, ~ Time | Stimulation), adjust = "bonferroni")

# Contrasts: between stimulation conditions at each time point

pairs(emmeans(model, ~ Stimulation | Time), adjust = "bonferroni")


# Supplementary: Paired t-tests per condition

# (not primary inference)

for (stim in c("Sham", "tDCS", "tACS")) {
  cat("\n--- Paired t-test:", stim, "---\n")
  stim_wide <- gaba_data %>%
    filter(Stimulation == stim) %>%
    select(ParticipantID, Time, GABA) %>%
    pivot_wider(names_from = Time, values_from = GABA) %>%
    filter(!is.na(Pre) & !is.na(Post))   
  print(t.test(stim_wide$Pre, stim_wide$Post, paired = TRUE))
}


# =============================================================
#  DATA VISUALISATION
# =============================================================

# Colour palette (colour-blind friendly — Wong 2011 palette)
# tDCS = Orange, tACS = Sky Blue, Sham = Bluish Green
stim_colours <- c("Sham" = "#009E73", "tDCS" = "#E69F00", "tACS" = "#56B4E9")


# Graph 1: Model-estimated means (EMMs) with 95% CI 

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

# Graph 2: Individual trajectories (raw data)

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


# Graph 3: Pre-Post change (delta) — boxplot + jitter

gaba_change <- gaba_data %>%
  pivot_wider(names_from = Time, values_from = GABA) %>%
  mutate(Delta = Post - Pre)

# Number of complete pairs per condition?
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


# Graph 4: Raincloud plot of delta scores 

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


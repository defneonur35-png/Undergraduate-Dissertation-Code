# ============================================================
#  NF1 Neurostimulation – Correlation Analysis
#  GABA/NAA Delta × Inverse Efficiency (secondary WM outcome)
#  NOTE: Higher inverse efficiency = WORSE performance
#        So expect opposite sign to d prime if effects are consistent
# ============================================================

library(tidyverse)
library(broom)
library(conflicted)
library(dplyr)
library(ggplot2)
library(ggpubr)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::recode)


# --- Load data -----------------------------------------------
corr_data <- read.csv("inverse_efficiency.csv")

# Check structure and data
str(corr_data)
corr_data


# --- Fix typo and set factors --------------------------------
# Correct "tCDS" typo to "tDCS" if present
corr_data$Stimulation <- recode(corr_data$Stimulation, "tCDS" = "tDCS")

corr_data$Stimulation <- factor(corr_data$Stimulation,
                                levels = c("Sham", "tDCS", "tACS"))

# Colour palette (matching GABA/NAA analysis)
stim_colours <- c("Sham" = "#009E73", "tDCS" = "#E69F00", "tACS" = "#56B4E9")


# --- Descriptive summary -------------------------------------
summary(corr_data)

corr_data %>%
  group_by(Stimulation) %>%
  summarise(
    N                 = n(),
    Mean_GABA_delta   = mean(GABA_Delta, na.rm = TRUE),
    SD_GABA_delta     = sd(GABA_Delta,   na.rm = TRUE),
    Mean_inv_eff      = mean(WM_Score,   na.rm = TRUE),
    SD_inv_eff        = sd(WM_Score,     na.rm = TRUE),
    .groups = "drop"
  )


# --- Correlation analysis ------------------------------------
# Pearson correlations run separately per stimulation condition
# as per supervisor recommendation

cat("\n--- Correlation: Sham ---\n")
cor.test(corr_data$GABA_Delta[corr_data$Stimulation == "Sham"],
         corr_data$WM_Score[corr_data$Stimulation   == "Sham"],
         method = "pearson")

cat("\n--- Correlation: tDCS ---\n")
cor.test(corr_data$GABA_Delta[corr_data$Stimulation == "tDCS"],
         corr_data$WM_Score[corr_data$Stimulation   == "tDCS"],
         method = "pearson")

cat("\n--- Correlation: tACS ---\n")
cor.test(corr_data$GABA_Delta[corr_data$Stimulation == "tACS"],
         corr_data$WM_Score[corr_data$Stimulation   == "tACS"],
         method = "pearson")


# --- Graph 1: Faceted scatterplot (one panel per condition) --
# Primary figure — each condition in its own panel with
# regression line, 95% CI band, and Pearson r + p value

graph1 <- ggplot(corr_data, aes(x = GABA_Delta, y = WM_Score,
                                 colour = Stimulation)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
  stat_cor(method = "pearson",
           aes(label = paste(after_stat(r.label),
                             after_stat(p.label), sep = "~`,`~")),
           label.x.npc = "left", label.y.npc = "top", size = 3.5) +
  scale_colour_manual(values = stim_colours) +
  facet_wrap(~ Stimulation) +
  theme_classic(base_size = 13) +
  labs(
    title    = "GABA/NAA Change vs. Inverse Efficiency by Stimulation Condition",
    subtitle = "Pearson r with 95% CI band  |  Higher IE = worse performance",
    x        = expression(Delta * " GABA/NAA (Post – Pre, a.u.)"),
    y        = "Inverse Efficiency (ms)"
  ) +
  theme(legend.position = "none")

print(graph1)


# --- Graph 2: All conditions side by side --------------------

graph2 <- ggplot(corr_data, aes(x = GABA_Delta, y = WM_Score,
                                 colour = Stimulation)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
  stat_cor(method = "pearson",
           aes(label = paste(after_stat(r.label),
                             after_stat(p.label), sep = "~`,`~")),
           size = 3.5) +
  scale_colour_manual(values = stim_colours) +
  facet_wrap(~ Stimulation, nrow = 1) +
  theme_classic(base_size = 13) +
  labs(
    title = "GABA/NAA Change vs. Inverse Efficiency",
    x     = expression(Delta * " GABA/NAA (Post – Pre, a.u.)"),
    y     = "Inverse Efficiency (ms)"
  ) +
  theme(legend.position = "none")

print(graph2)


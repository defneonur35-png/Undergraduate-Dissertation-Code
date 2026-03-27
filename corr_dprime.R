# ============================================================
#  NF1 Neurostimulation – Correlation Analysis
#  GABA/NAA Delta × d prime (primary WM outcome)
# ============================================================

library(tidyverse)
library(broom)
library(conflicted)
library(dplyr)
library(ggplot2)
library(ggpubr)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::recode)


# Load Data -
corr_data <- read.csv("d_prime_corr.csv")

# Check structure and data
str(corr_data)
corr_data


# Set Factors 

corr_data$Stimulation <- factor(corr_data$Stimulation,
                                levels = c("Sham", "tDCS", "tACS"))

# Colour Palette (matching GABA/NAA analysis)
stim_colours <- c("Sham" = "#009E73", "tDCS" = "#E69F00", "tACS" = "#56B4E9")


# Descriptive Summary 
summary(corr_data)

corr_data %>%
  group_by(Stimulation) %>%
  summarise(
    N                 = n(),
    Mean_GABA_delta   = mean(GABA_Delta, na.rm = TRUE),
    SD_GABA_delta     = sd(GABA_Delta,   na.rm = TRUE),
    Mean_d_prime      = mean(WM_Score,   na.rm = TRUE),
    SD_d_prime        = sd(WM_Score,     na.rm = TRUE),
    .groups = "drop"
  )


# Correlation Analysis

cat("\n Correlation: Sham \n")
cor.test(corr_data$GABA_Delta[corr_data$Stimulation == "Sham"],
         corr_data$WM_Score[corr_data$Stimulation   == "Sham"],
         method = "pearson")

cat("\n Correlation: tDCS \n")
cor.test(corr_data$GABA_Delta[corr_data$Stimulation == "tDCS"],
         corr_data$WM_Score[corr_data$Stimulation   == "tDCS"],
         method = "pearson")

cat("\n Correlation: tACS \n")
cor.test(corr_data$GABA_Delta[corr_data$Stimulation == "tACS"],
         corr_data$WM_Score[corr_data$Stimulation   == "tACS"],
         method = "pearson")


# Graph: Faceted scatterplot (one panel per condition)

corr_data$Stimulation <- factor(
  corr_data$Stimulation,
  levels = c("Sham", "tDCS", "tACS")
)

apa_num <- function(x, digits = 2) {
  out <- sprintf(paste0("%.", digits, "f"), x)
  out <- sub("^0\\.", ".", out)
  out <- sub("^-0\\.", "-.", out)
  out
}

apa_axis <- function(x) {
  out <- sprintf("%.1f", x)
  out <- sub("^0\\.", ".", out)
  out <- sub("^-0\\.", "-.", out)
  out
}

corr_labels <- data.frame(
  Stimulation = factor(c("Sham", "tDCS", "tACS"),
                       levels = c("Sham", "tDCS", "tACS")),
  x = -Inf,
  y = Inf,
  r = c(0.113, -0.076, -0.355),
  p = c(0.688, 0.781, 0.162)
)

corr_labels$label <- paste0(
  "italic(r) == ", apa_num(corr_labels$r, 2),
  " * ',' ~ italic(p) == ", apa_num(corr_labels$p, 2)
)

graph2 <- ggplot(
  corr_data,
  aes(x = GABA_Delta, y = WM_Score, colour = Stimulation)
) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 0.8) +
  geom_text(
    data = corr_labels,
    aes(x = x, y = y, label = label, colour = Stimulation),
    parse = TRUE,
    hjust = -0.1,
    vjust = 1.3,
    size = 4,
    inherit.aes = FALSE
  ) +
  scale_colour_manual(values = stim_colours) +
  scale_x_continuous(labels = apa_axis) +
  facet_wrap(~ Stimulation, nrow = 1) +
  labs(
    x = "\u0394 GABA/NAA Ratio (Post \u2212 Pre)",
    y = "d\u2032"
  ) +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(colour = "black"),
    legend.position = "none"
  )

print(graph2)

# Graph: Forest Plot

# Data for forest plot
corr_forest <- data.frame(
  Outcome = c("d\u2032", "d\u2032", "d\u2032",
              "Inverse Efficiency", "Inverse Efficiency", "Inverse Efficiency"),
  Stimulation = c("Sham", "tDCS", "tACS",
                  "Sham", "tDCS", "tACS"),
  r = c(0.113, -0.076, -0.355,
        0.013, 0.022, 0.483),
  CI_low = c(-0.42, -0.55, -0.71,
             -0.50, -0.48, 0.003),
  CI_high = c(0.59, 0.44, 0.15,
              0.52, 0.51, 0.782)
)

# Factor order
corr_forest$Stimulation <- factor(
  corr_forest$Stimulation,
  levels = c("Sham", "tDCS", "tACS")
)

corr_forest$Outcome <- factor(
  corr_forest$Outcome,
  levels = c("d\u2032", "Inverse Efficiency")
)

# APA-style axis formatter
apa_axis <- function(x) {
  out <- sprintf("%.1f", x)
  out <- sub("^0\\.", ".", out)
  out <- sub("^-0\\.", "-.", out)
  out
}

forest_plot <- ggplot(
  corr_forest,
  aes(x = r, y = Stimulation, colour = Stimulation)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    colour = "grey40",
    linewidth = 0.7
  ) +
  geom_errorbar(
    aes(xmin = CI_low, xmax = CI_high),
    width = 0.15,
    linewidth = 1,
    orientation = "y"
  ) +
  geom_point(size = 4) +
  scale_colour_manual(values = stim_colours) +
  scale_x_continuous(
    limits = c(-0.8, 0.8),
    breaks = seq(-0.8, 0.8, by = 0.2),
    labels = apa_axis
  ) +
  facet_wrap(~ Outcome, ncol = 1) +
  labs(
    x = "Pearson Correlation Coefficient (r)",
    y = "Stimulation Condition"
  ) +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    strip.background = element_rect(fill = "white", colour = "black", linewidth = 0.8),
    strip.text = element_text(colour = "black"),
    legend.position = "none"
  )

print(forest_plot)

# ============================================================
#  NF1 Neurostimulation – Correlation Analysis
#  GABA/NAA Delta × Inverse Efficiency (secondary WM outcome)
# ============================================================

library(tidyverse)
library(broom)
library(conflicted)
library(dplyr)
library(ggplot2)
library(ggpubr)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::recode)


# Load Data
corr_data <- read.csv("inverse_efficiency2.csv")

# Check Structure and Data
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
    Mean_inv_eff      = mean(WM_Score,   na.rm = TRUE),
    SD_inv_eff        = sd(WM_Score,     na.rm = TRUE),
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


# Graph 1: Faceted Scatterplot (one panel per condition) 

# Make sure stimulation order is consistent
corr_data$Stimulation <- factor(
  corr_data$Stimulation,
  levels = c("Sham", "tDCS", "tACS")
)

# APA-style formatter: removes leading zero
apa_num <- function(x, digits = 2) {
  out <- sprintf(paste0("%.", digits, "f"), x)
  out <- sub("^0\\.", ".", out)
  out <- sub("^-0\\.", "-.", out)
  out
}

# APA-style axis labels: -.2, -.1, .0
apa_axis <- function(x) {
  out <- sprintf("%.1f", x)
  out <- sub("^0\\.", ".", out)
  out <- sub("^-0\\.", "-.", out)
  out
}

# Labels fixed to top-left of each facet
corr_stats <- corr_data %>%
  group_by(Stimulation) %>%
  summarise(
    r = cor.test(GABA_Delta, WM_Score, method = "pearson")$estimate,
    p = cor.test(GABA_Delta, WM_Score, method = "pearson")$p.value,
    .groups = "drop"
  )

corr_labels <- data.frame(
  Stimulation = factor(corr_stats$Stimulation, levels = c("Sham", "tDCS", "tACS")),
  x = -Inf,
  y = Inf,
  r = corr_stats$r,
  p = corr_stats$p
)

corr_labels$label <- paste0(
  "italic(r) == ", apa_num(corr_labels$r, 2),
  " * ',' ~ italic(p) == ", apa_num(corr_labels$p, 2)
)

graph1 <- ggplot(
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
    y = "Inverse Efficiency (ms)"
  ) +
  theme_classic(base_size = 11, base_family = "Arial") +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text.x = element_text(colour = "black"),
    axis.text.y = element_text(colour = "black"),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(colour = "black"),
    legend.position = "none",
    plot.title = element_blank(),
    plot.subtitle = element_blank()
  )

print(graph1)



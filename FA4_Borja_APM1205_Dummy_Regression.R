# ==============================================================================
# APM1205 - Applied Regression Analysis
# Formative Assessment 4: Dummy-Variable Regression Using Real-World Data
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Setup and Directory Preparation
# ------------------------------------------------------------------------------
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# Create figures directory if it doesn't already exist
if (!dir.exists("figures")) {
  dir.create("figures")
}

# ------------------------------------------------------------------------------
# Part A: Exploring and Preparing the Data
# ------------------------------------------------------------------------------
data(diamonds)

# Convert cut to an unordered factor and set "Fair" as the reference category
diamonds$cut <- factor(diamonds$cut, ordered = FALSE)
diamonds$cut <- relevel(diamonds$cut, ref = "Fair")

cat("=== PART A: DATASET DIMENSIONS ===\n")
cat("Observations (Rows):", nrow(diamonds), "\n")
cat("Variables (Columns):", ncol(diamonds), "\n\n")

str(diamonds)
summary(diamonds)

# ------------------------------------------------------------------------------
# Part B: Constructing Dummy Variables
# ------------------------------------------------------------------------------
cat("\n=== PART B: DUMMY VARIABLE CONTRASTS (REFERENCE: FAIR) ===\n")
print(contrasts(diamonds$cut))

# ------------------------------------------------------------------------------
# Part C: Additive Dummy-Variable Regression
# ------------------------------------------------------------------------------
model1 <- lm(price ~ carat + cut, data = diamonds)
cat("\n=== PART C: ADDITIVE MODEL SUMMARY ===\n")
summary(model1)

# ------------------------------------------------------------------------------
# Part D: Interaction Model and ANOVA / Incremental F-Test
# ------------------------------------------------------------------------------
model2 <- lm(price ~ carat * cut, data = diamonds)
cat("\n=== PART D: INTERACTION MODEL SUMMARY ===\n")
summary(model2)

cat("\n=== PART D: INCREMENTAL F-TEST (ANOVA) ===\n")
anova_table <- anova(model1, model2)
print(anova_table)

# ------------------------------------------------------------------------------
# Part E: Visualization and Export
# ------------------------------------------------------------------------------
p <- ggplot(diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point(alpha = 0.2, size = 0.75) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Diamond Price versus Carat by Cut Quality",
    subtitle = "Interaction Regression Model with Varying Slopes",
    x = "Carat (Weight)",
    y = "Price (US Dollars)",
    color = "Cut Quality"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

# Save visualization
ggsave(
  filename = "figures/price_vs_carat_by_cut.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)

cat("\nAnalysis complete. Plot saved to figures/price_vs_carat_by_cut.png\n")


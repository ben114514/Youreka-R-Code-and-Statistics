# Load libraries
library(readxl)
library(car)
library(dunn.test)
library(ggplot2)
library(rstatix)
library(dplyr)
library(tidyr)  # Needed for pivot_longer

# Set file path
file_path <- '/Users/benjaminjin/Documents/Youreka Data Processing/Youreka Data.xlsx' # <-put your file path here

# ============================
# Group Comparison by Facility Type (Revised)
# ============================

cat("\n========== Group Comparison by Facility Type (Columns as Groups) ==========\n")

# Read in the sheet where each column is a Facility Type and each row is a CDI incidence
wide_df <- read_excel(file_path, sheet = "Grouped by Facility Type")

# Convert from wide to long format
long_df <- wide_df %>%
  pivot_longer(cols = everything(), names_to = "FacilityType", values_to = "Observed") %>%
  drop_na()

# Ensure correct data types
long_df$Observed <- as.numeric(long_df$Observed)
long_df$FacilityType <- as.factor(long_df$FacilityType)

# Check normality (Shapiro-Wilk) for each group with at least 3 observations
shapiro_results <- long_df %>%
  group_by(FacilityType) %>%
  filter(n() >= 3) %>%
  summarise(p_value = shapiro.test(Observed)$p.value)

cat("Shapiro-Wilk p-values by Facility Type:\n")
print(shapiro_results)

# Levenes Test for homogeneity of variance
levene_result <- leveneTest(Observed ~ FacilityType, data = long_df)
levene_p <- levene_result$`Pr(>F)`[1]
cat("Levenes Test p-value:", round(levene_p, 4), "\n")

# Decide on ANOVA or Kruskal-Wallis
if (all(shapiro_results$p_value > 0.05) && levene_p > 0.05) {
  cat("→ Parametric ANOVA\n")
  anova_model <- aov(Observed ~ FacilityType, data = long_df)
  print(summary(anova_model))
  
  cat("Post-hoc Tukey HSD:\n")
  print(TukeyHSD(anova_model))
  
} else {
  cat("→ Non-parametric Kruskal-Wallis\n")
  kw_result <- kruskal.test(Observed ~ FacilityType, data = long_df)
  print(kw_result)
  
  cat("Post-hoc Dunns Test:\n")
  dunn_result <- dunn.test(x = long_df$Observed,
                           g = long_df$FacilityType,
                           kw = TRUE,
                           list = TRUE,
                           rmc = FALSE)
  print(dunn_result)
}

# Boxplot of groups
ggplot(long_df, aes(x = FacilityType, y = Observed)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "CDI Incidence by Facility Type",
       x = "Facility Type", y = "Observed CDI Incidence") +
  theme_minimal()
print(plot)

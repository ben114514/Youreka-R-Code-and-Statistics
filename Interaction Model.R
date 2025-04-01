# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Set file path
file_path <- '/Users/benjaminjin/Documents/Youreka Data Processing/Youreka Data for Regression Comparison.xlsx'
sheet_names <- excel_sheets(file_path)

# Read each sheet and extract PM2.5 and Observed columns
# Assuming column 2 = PM2.5, column 3 = Observed

data_list <- lapply(sheet_names, function(sheet) {
  df <- read_excel(file_path, sheet = sheet)
  df <- df[, c(1, 2)]
  colnames(df) <- c("PM25", "Observed")
  df <- na.omit(df)
  df$Group <- sheet
  return(df)
})

# Combine all into one data frame
df_all <- bind_rows(data_list)
df_all$Group <- as.factor(df_all$Group)

# Fit the full interaction model (slopes and intercepts vary)
model_full <- lm(Observed ~ PM25 * Group, data = df_all)
summary(model_full)

# Fit the no-interaction model (only intercepts vary)
model_no_interaction <- lm(Observed ~ PM25 + Group, data = df_all)

# Compare models to test if slopes differ significantly
anova(model_no_interaction, model_full)

# Plot the regression lines by group
ggplot(df_all, aes(x = PM25, y = Observed, color = Group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Comparison of Linear Regressions Across Facility Types",
       x = "PM2.5 (µg/m³)", y = "Observed CDI Incidence") +
  theme_minimal()
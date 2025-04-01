# Load libraries
library(readxl)
library(car)
library(dunn.test)
library(ggplot2)
library(rstatix)

# Set file path
file_path <- '/Users/benjaminjin/Documents/Youreka Data Processing/Youreka Data.xlsx' # <-put your file path here
sheet_names <- excel_sheets(file_path)

# ============================
# PART 1: Correlation & Regression
# ============================

# Use sheets that are not the grouped "facility type" one
for (sheet in sheet_names) {
  if (sheet != "Grouped by Facility Type") {
    cat("\n========== Correlation & Regression ==========\n")
    cat("Processing sheet:", sheet, "\n")
    
    # Load data
    df <- read_excel(file_path, sheet = sheet)
    df <- na.omit(df[, c(2, 3)])  # PM2.5 and Observed (by position)
    colnames(df) <- c("PM25", "Observed")
    
    # Shapiro-Wilk for normality
    shapiro_result <- shapiro.test(df$Observed)
    shapiro_p <- shapiro_result$p.value
    cat("Shapiro-Wilk p-value for 'Observed':", round(shapiro_p, 4), "\n")
    
    # Levene's Test for homogeneity of variance (using quartile bins)
    df$PM25 <- as.numeric(df$PM25)
    df$PM25_group <- cut(df$PM25, breaks = quantile(df$PM25, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)
    
    levene_result <- leveneTest(Observed ~ PM25_group, data = df)
    levene_p <- levene_result$`Pr(>F)`[1]
    cat("Levene's Test p-value:", round(levene_p, 4), "\n")
    
    # Correlation
    if (shapiro_p >= 0.05 && levene_p >= 0.05) {
      cor_method <- "pearson"
    } else {
      cor_method <- "spearman"
    }
    
    cor_test <- cor.test(df$PM25, df$Observed, method = cor_method)
    r_val <- unname(cor_test$estimate)
    r_squared <- r_val^2
    
    cat("Correlation method:", cor_method, "\n")
    cat("r =", round(r_val, 3), " | R² =", round(r_squared, 3), "\n")
    print(cor_test)
    
    # Linear Regression
    lm_model <- lm(Observed ~ PM25, data = df)
    print(summary(lm_model))
    
    # Plot
    p <- ggplot(df, aes(x = PM25, y = Observed)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE, color = "blue") +
      labs(title = paste("Linear Regression -", sheet),
           subtitle = paste("R² =", round(summary(lm_model)$r.squared, 3)),
           x = "PM 2.5 (µg/m³)", y = "Observed CDI Incidence") +
      theme_minimal()
    print(p)
  }
}


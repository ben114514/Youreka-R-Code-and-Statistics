# Load required libraries
library(readxl)
library(dplyr)

# File path
file_path <- '/Users/benjaminjin/Documents/Youreka Data Processing/Youreka Data.xlsx' # <-put your file path here
sheet_names <- excel_sheets(file_path)

# Read each sheet and extract PM2.5 and Observed columns

data_list <- lapply(sheet_names, function(sheet) {
  df <- read_excel(file_path, sheet = sheet)
  df <- df[, c(2, 3)]
  colnames(df) <- c("PM25", "Observed")
  df <- na.omit(df)
  df$Group <- sheet
  return(df)
})

# Combine into a single dataframe
combined_df <- bind_rows(data_list)

# Create PM2.5 groups based on quartiles
combined_df$PM25_Group <- cut(combined_df$PM25,
                              breaks = quantile(combined_df$PM25, probs = seq(0, 1, 0.25), na.rm = TRUE),
                              include.lowest = TRUE,
                              labels = c("Low", "Mid-Low", "Mid-High", "High"))

# Create all pairwise combinations of PM2.5 groups
group_levels <- levels(combined_df$PM25_Group)
pairwise_combos <- combn(group_levels, 2, simplify = FALSE)

# Perform Mann-Whitney U test (Wilcoxon rank-sum test) for each pair
cat("==== Pairwise Mann-Whitney U Tests by PM2.5 Group ====")
for (pair in pairwise_combos) {
  group1 <- pair[1]
  group2 <- pair[2]
  
  group1_data <- combined_df %>% filter(PM25_Group == group1) %>% pull(Observed)
  group2_data <- combined_df %>% filter(PM25_Group == group2) %>% pull(Observed)
  
  test_result <- wilcox.test(group1_data, group2_data)
  
  cat(sprintf("\n%s vs %s:\n", group1, group2))
  print(test_result)
}

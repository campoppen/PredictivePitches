# Load necessary libraries
library(dplyr)

# Load the batter data from CSV, edit to where "data.csv" is found
batter_data <- read.csv("/Users/CameronPoppen/Downloads/data.csv")

# Categorize pitches into FB, BB, and OS
batter_data <- batter_data %>%
  mutate(PITCH_CATEGORY = case_when(
    PITCH_TYPE %in% c("FF", "FC", "SI", "FA") ~ "FB",
    PITCH_TYPE %in% c("CU", "SL", "KC", "ST", "SV", "CS", "SC") ~ "BB",
    PITCH_TYPE %in% c("CH", "EP", "KN", "FO", "FS") ~ "OS"
  )) %>%
  filter(!is.na(PITCH_CATEGORY))  # Remove rows with missing PITCH_CATEGORY

# Step 3: Train the multinomial logistic regression model
multinom_model <- multinom(PITCH_CATEGORY ~ BATTER_ID, data = batter_data, trace = FALSE)

# Step 4: Load the prediction dataset from CSV
predictions <- read.csv("/Users/CameronPoppen/Downloads/predictions.csv")

# Step 5: Make predictions using the multinomial logistic regression model
predicted_probs <- predict(multinom_model, newdata = predictions, type = "probs")

# Step 6: Convert the predicted probabilities to proportions for FB, BB, and OS
predictions <- predictions %>%
  mutate(
    PITCH_TYPE_FB = round(predicted_probs[, "FB"], 4),
    PITCH_TYPE_BB = round(predicted_probs[, "BB"], 4),
    PITCH_TYPE_OS = round(predicted_probs[, "OS"], 4),
    GAME_YEAR = 2024
  )

# Step 7: Select only necessary columns for submission
submission <- predictions %>%
  select(BATTER_ID, PLAYER_NAME, GAME_YEAR, PITCH_TYPE_FB, PITCH_TYPE_BB, PITCH_TYPE_OS)

# Step 8: Save the predictions to CSV file
write.csv(submission, "/Users/CameronPoppen/Downloads/predictionsMultiLogReg.csv", row.names = FALSE)
# Saving the model
saveRDS(gbm_model, "./models/saved_gbm_model.rds")

# Load the saved model
loaded_gbm_model <- readRDS("./models/saved_gbm_model.rds")

# Model predicts Wind Speed
new_data <- data.frame(
  # Make sure the factors are correctly assigned with the same levels as in training
  IND = 0,
  RAIN = 0.2,
  IND_1 = 4,
  T_MAX = 9.5,
  IND_2 = 4,
  T_MIN = 3.7
)

# Use the loaded model to make predictions
predictions_loaded_gbm_model <- predict(loaded_gbm_model, newdata = new_data)

# Print predictions
print(predictions_loaded_gbm_model)

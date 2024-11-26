# Load the saved GBM model
loaded_gbm_model <- readRDS("./models/saved_gbm_model.rds")

#* @apiTitle Wind Speed Prediction Model API

#* @apiDescription Predict wind speed based on weather variables.

#* @param IND The value for IND (factor)
#* @param RAIN The amount of rainfall
#* @param IND_1 The value for IND_1 (factor)
#* @param T_MAX The maximum temperature
#* @param IND_2 The value for IND_2 (factor)
#* @param T_MIN The minimum temperature

#* @get /predict_wind_speed
function(IND, RAIN, IND_1, T_MAX, IND_2, T_MIN) {
  
  # Create a data frame using the arguments and ensure factors have the same levels as in training
  to_be_predicted <- data.frame(
    IND = as.numeric(IND),
    RAIN = as.numeric(RAIN),
    IND_1 = as.numeric(IND_1),
    T_MAX = as.numeric(T_MAX),
    IND_2 = as.numeric(IND_2),
    T_MIN = as.numeric(T_MIN)
  )
  
  # Use the loaded model to make predictions
  prediction <- predict(loaded_gbm_model, newdata = to_be_predicted)
  
  # Return the prediction as a JSON response
  return(list(predicted_wind_speed = prediction))
}

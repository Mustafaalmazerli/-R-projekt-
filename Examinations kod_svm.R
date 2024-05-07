# Läs in nödvändiga paket
library(readxl)
library(caret)
library(e1071)

# Läs in data från Excel-filen
file <- "C:/Users/musta/OneDrive/Skrivbord/Examinationsuppgift/data_bil.xlsx"
data_bil_1 <- read_excel(file)
View(data_bil_1)
head(data_bil_1)
spec <- c(train = .6, test = .2, validate = .2)

# Skapa en gruppindelning baserad på fördelningen
g <- sample(cut(
  seq(nrow(data_bil)), 
  nrow(data_bil)*cumsum(c(0,spec)),
  labels = names(spec)
))

# Dela upp datan baserat på grupperna
res <- split(data_bil, g)

# Extrahera tränings-
train_data <- res$train
test_data <- res$test
validate_data <- res$validate

# Skapa en SVM-modell
svm_model <- svm(Price ~ Brand + Year + Fuel + Mileage + Gearbox, data = train_data, kernel = "radial")
summary(svm_model)




# Förutsägelser för hela datamängden med SVM-modellen
predictions_all <- predict(svm_model, newdata = data_bil) 

# Lägg till förutsägelser som en kolumn i datamängden
data_bil$Predicted_Price <- predictions_all

# Skapa scatterplot med ursprungliga data och förutsägelser
ggplot(data = data_bil, aes(x = Year, y = Price, color = Fuel)) +
  geom_point() +  # Ursprungliga datapunkter
  geom_point(aes(y = Predicted_Price), color = "red", size = 2) +  # Förutsägelser
  labs(x = "Year", y = "Price", title = "Scatterplot of Price vs Year with Predictions") +
  theme_minimal()


# Gör förutsägelser på testdata
predictions <- predict(svm_model, newdata = test_data)

# Utvärdera prestanda på testdata (t.ex. medelkvadratiskt fel)
mse <- mean((predictions - test_data$Price)^2)
print(paste("Mean Squared Error on Test Data:", mse))

# Gör förutsägelser på valideringsdata
predictions_validate <- predict(svm_model, newdata = validate_data)

# Utvärdera prestanda på valideringsdata
mse_validate <- mean((predictions_validate - validate_data$Price)^2)
print(paste("Mean Squared Error on Validation Data:", mse_validate))

# Utvärdera R^2 och MAE
R2 <- cor(predictions, test_data$Price)^2
MAE <- mean(abs(predictions - test_data$Price))
print(paste("R-squared:", R2))
print(paste("Mean Absolute Error (MAE):", MAE))

# Sök efter de bästa parametrarna och hyperparametrarna
svm_grid <- expand.grid(sigma = c(0.01, 0.1, 1, 10), C = c(0.1, 1, 10, 100))
svm_ctrl <- trainControl(method = "cv", number = 5)
svm_model_tuned <- train(Price ~ ., data = train_data, method = "svmRadial", trControl = svm_ctrl, tuneGrid = svm_grid)

# Visa de bästa parametrarna och hyperparametrarna
print("Best Parameters and Hyperparameters:")
print(svm_model_tuned$bestTune)
best_sigma <- svm_model_tuned$bestTune$sigma
best_C <- svm_model_tuned$bestTune$C
print(paste("Best Sigma:", best_sigma))
print(paste("Best C:", best_C))

best_svm_model <- svm(Price ~ Brand + Year + Fuel + Mileage + Gearbox, data = train_data, kernel = "radial", cost = 10, gamma = 1 / (2 * 0.01 ^2))
trained_best_svm_model <- best_svm_model
predictions_test_best <- predict(trained_best_svm_model, newdata = test_data)
mse_test_best <- mean((predictions_test_best - test_data$Price)^2)
print(paste("Mean Squared Error on Test Data with Best Model:", mse_test_best))
summary(best_svm_model)


file_500_cars <- "C:/Users/musta/OneDrive/Skrivbord/Volkswagen.xlsx"
data_500_cars <- read_excel(file_500_cars)
View(data_500_cars)



# Förutsäg priserna för de 500 bilarna med din modell igen
predicted_prices_500_cars <- predict(trained_best_svm_model, newdata = data_500_cars)

head(actual_prices)
head(predicted_prices_500_cars)
predicted_prices_500_cars



new_car <- data.frame(Brand = "Toyota", Year = 2014, Fuel = "Hybrid", Mileage = 17121, Gearbox = "Automat")
new_car_prediction <- predict(best_svm_model, newdata = new_car)
print(new_car_prediction)

new_car <- data.frame(Brand = "BMW", Year = 2015, Fuel = "Diesel", Mileage = 17625, Gearbox = "Automat")
new_car_prediction <- predict(best_svm_model, newdata = new_car)
print(new_car_prediction)


# Gör förutsägelser för en ny bil (använda exempeldata)
new_car <- data.frame(Brand = "Citroën", Year = 2014, Mileage = 18800, Fuel = "Diesel", Gearbox = "Automat")
new_prediction <- predict(svm_model, newdata = new_car)
print(paste("Predicted Price for New Car:", new_prediction))
# Gör förutsägelser för en ny bil (använda exempeldata)
new_car <- data.frame(Brand = "Mazda", Year = 2023, Mileage = 0, Fuel = "Bensin", Gearbox = "Automat")
new_prediction <- predict(svm_model, newdata = new_car)
print(paste("Predicted Price for New Car:", new_prediction))
# Gör förutsägelser för en ny bil (använda exempeldata)
new_car <- data.frame(Brand = "Volkswagen", Year = 2011, Mileage = 11599, Fuel = "Bensin", Gearbox = "Manuell")
new_prediction <- predict(svm_model, newdata = new_car)
print(paste("Predicted Price for New Car:", new_prediction))
# Gör förutsägelser för en ny bil (använda exempeldata)
new_car <- data.frame(Brand = "Volkswagen", Year = 2024, Mileage = 0, Fuel = "Bensin", Gearbox = "Automat")
new_prediction <- predict(svm_model, newdata = new_car)
print(paste("Predicted Price for New Car:", new_prediction))

new_car <- data.frame(Brand = "Audi", Year = 2012, Mileage = 11900, Fuel = "Diesel", Gearbox = "Automat")
new_prediction <- predict(svm_model, newdata = new_car)
print(paste("Predicted Price for New Car:", new_prediction))

# Scatterplot för testuppsättningen med förutsägelser
ggplot(data = test_data, aes(x = Year, y = Price, color = Fuel)) +
  geom_point() +  # Ursprungliga datapunkter
  geom_point(aes(y = predictions), color = "red", size = 2) +  # Förutsägelser
  labs(x = "Year", y = "Price", title = "Scatterplot of Price vs Year on Test Data with Predictions") +
  theme_minimal()


# Scatterplot för testuppsättningen: Faktiska priser vs. Förutsagda priser
ggplot(data = test_data, aes(x = Price, y = predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Linje för perfekt förutsägelse
  labs(x = "Actual Price", y = "Predicted Price", title = "Actual vs. Predicted Prices on Test Data")













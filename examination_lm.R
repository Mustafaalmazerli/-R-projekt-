# Läs in nödvändiga paket
library(readxl)
library(dplyr)
library(ggplot2)
library(caret)


# Läs in data från Excel-filen
file <- "C:/Users/musta/OneDrive/Skrivbord/Examinationsuppgift/data_bil.xlsx"
data_bil <- read_excel(file)

# Inspektera data
str(data_bil)
head(data_bil)
summary(data_bil)
View(data_bil)

# Skapa en scatterplot för att visualisera data (t.ex. Price vs. Year)
ggplot(data = data_bil, aes(x = Year, y = Price, color = Fuel)) +
  geom_point(size = 2) +
  labs(x = "Year", y = "Price", title = "Scatterplot of Price vs Year") +
  theme_minimal()

# Dela upp data
set.seed(123)  
spec <- c(train = .6, test = .2, validate = .2)

g <- sample(cut(
  seq(nrow(data_bil)), 
  nrow(data_bil) * cumsum(c(0, spec)),
  labels = names(spec)
))

res <- split(data_bil, g)
train_data <- res$train
test_data <- res$test
validate_data <- res$validate

# Variabelhantering
train_data$Brand <- as.factor(train_data$Brand)
train_data$Fuel <- as.factor(train_data$Fuel)
train_data$Gearbox <- as.factor(train_data$Gearbox)

#data visualisering
ggplot(data = train_data, aes(x = Mileage, y = Price, color = Fuel)) +
  geom_point(size = 2) +
  labs(x = "Mileage", y = "Price", title = "Scatterplot of Price vs Mileage by Brand") +
  theme_minimal()

# Skapa en vektor med alla unika nivåer av Brand från både tränings- och valideringsdatan
all_levels <- union(levels(train_data$Brand), levels(validate_data$Brand))

# Uppdatera faktorn Brand så att den innehåller alla unika nivåer från både tränings- och valideringsdatan
train_data$Brand <- factor(train_data$Brand, levels = all_levels)
validate_data$Brand <- factor(validate_data$Brand, levels = all_levels)

# Bygg en linjär regressionsmodell med träningsdata
lm_model <- lm(Price ~ Brand + Year + Fuel + Mileage + Gearbox, data = train_data)
predictions <- predict(lm_model, newdata = validate_data)

summary(lm_model)

par(mfrow = c(2, 2))
plot(lm_model)
geom_point(lm_model)

vif(lm_model)
# Gör förutsägelser igen efter att NA-värden har tagits bort
predictions <- predict(lm_model, newdata = validate_data)
# Beräkna RMSE
validation_rmse <- sqrt(mean((validate_data$Price - predictions)^2))
validation_rmse

# Bygg linjär regressionsmodell med korsvalidering
ctrl <- trainControl(method = "cv", number = 5)
lm_model_cv <- train(Price ~ Brand + Year + Fuel + Mileage + Gearbox, 
                     data = train_data, 
                     method = "lm",
                     trControl = ctrl)


# Kontrollera vilka variabler som är numeriska
numeric_variables <- sapply(train_data, is.numeric)
print(numeric_variables)

# Visa resultat av korsvalidering
print(lm_model_cv)
# Testa modellen på testdata
predictions_test <- predict(lm_model, newdata = test_data)
# Utvärdera prestanda på testdatan
R_squared_test <- cor(predictions_test, test_data$Price)^2
MAE_test <- mean(abs(predictions_test - test_data$Price))
RMSE_test <- sqrt(mean((predictions_test - test_data$Price)^2))

# Testa modellen på valideringsdata
predictions_validate <- predict(lm_model, newdata = validate_data)
# Utvärdera prestanda på valideringsdatan
R_squared_validate <- cor(predictions_validate, validate_data$Price)^2
MAE_validate <- mean(abs(predictions_validate - validate_data$Price))
RMSE_validate <- sqrt(mean((predictions_validate - validate_data$Price)^2))

# Skriv ut resultaten
print("Resultat för testdata:")
print(paste("R-squared:", R_squared_test))
print(paste("Mean Absolute Error (MAE):", MAE_test))
print(paste("Root Mean Squared Error (RMSE):", RMSE_test))

print("Resultat för valideringsdata:")
print(paste("R-squared:", R_squared_validate))
print(paste("Mean Absolute Error (MAE):", MAE_validate))
print(paste("Root Mean Squared Error (RMSE):", RMSE_validate))

# Create CI & PI for predictions
confidence_intervals <- predict(lm_model, newdata = data_bil, interval = "confidence", level = 0.95)
prediction_intervals <- predict(lm_model, newdata = data_bil, interval = "prediction", level = 0.95)

confidence_intervals
prediction_intervals

# Skapa en tabell med antalet förekomster av varje varumärke
brand_counts <- table(train_data$Brand)
head(sort(brand_counts, decreasing = TRUE))



# Visa koefficienterna för varje attribut i den linjära regressionsmodellen
coefficients(lm_model)
# Skapa en vektor med de nivåer som finns i träningsdatan
valid_levels <- levels(train_data$Brand)

# Uppdatera nivåerna för Brand-faktorn i testdatan
test_data$Brand <- factor(test_data$Brand, levels = valid_levels)

# Gör förutsägelser på den filtrerade testdatan
predictions <- predict(lm_model, newdata = test_data)


# Ta bort NA-värden från både predictions och test_data$Price
valid_indices <- !is.na(predictions) & !is.na(test_data$Price)
predictions_clean <- predictions[valid_indices]
actual_prices <- test_data$Price[valid_indices]




# Skapa en data frame för att hålla de förutsagda priserna och de faktiska priserna för testdata
prediction_data <- data.frame(Actual_Price = test_data$Price, Predicted_Price = predictions)

# Skapa en scatterplot för att jämföra faktiska och förutsagda priser
ggplot(prediction_data, aes(x = Actual_Price, y = Predicted_Price)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +  # Linje för perfekt förutsägelse
  labs(x = "Actual Price", y = "Predicted Price", title = "Actual vs. Predicted Prices") +
  theme_minimal()


# testa modelening 

file_500_cars <- "C:/Users/musta/OneDrive/Skrivbord/Examinationsuppgift/Volkswagen.xlsx"
data_500_cars <- read_excel(file_500_cars)
View(data_500_cars)



# Förutsäg priserna för de 500 bilarna med din modell igen
predicted_prices_500_cars <- predict(lm_model, newdata = data_500_cars)

head(actual_prices)
head(predicted_prices_500_cars)
predicted_prices_500_cars

# Skapa histogram över de förutsagda priserna
hist(predicted_prices_500_cars, breaks = 20, col = "skyblue", main = "Histogram of Predicted Prices",
     xlab = "Predicted Prices", ylab = "Frequency")
predicted_prices_500_cars <- predicted_prices_500_cars[!is.na(predicted_prices_500_cars)]
actual_prices <- actual_prices[!is.na(actual_prices)]
# Jämför de förutsagda priserna med de verkliga priserna
# Beräkna R^2
R_squared <- cor(predicted_prices_500_cars, actual_prices)^2
print(paste("R-squared:", R_squared))

# Beräkna Mean Absolute Error (MAE)
MAE <- mean(abs(predicted_prices_500_cars - actual_prices))
print(paste("Mean Absolute Error (MAE):", MAE))

# Beräkna Root Mean Squared Error (RMSE)
RMSE <- sqrt(mean((predicted_prices_500_cars - actual_prices)^2))
print(paste("Root Mean Squared Error (RMSE):", RMSE))







new_car <- data.frame(Brand = "BMW", Year = 2015, Fuel = "Diesel", Mileage = 17625, Gearbox = "Automat")
new_car_prediction <- predict(lm_model, newdata = new_car)
print(new_car_prediction)



new_car <- data.frame(Brand = "Toyota", Year = 2014, Fuel = "Hybrid", Mileage = 17121, Gearbox = "Automat")
new_car_prediction <- predict(lm_model, newdata = new_car)
print(new_car_prediction)


new_car <- data.frame(Brand = "Volkswagen", Year = 2024, Mileage = 0, Fuel = "Bensin", Gearbox = "Automat")
new_prediction <- predict(lm_model, newdata = new_car)
print(paste("Predicted Price for New Car:", new_prediction))

new_car <- data.frame(Brand = "Volkswagen", Year = 2024, Mileage = 0, Fuel = "Bensin", Gearbox = "Automat")
new_prediction <- predict(lm_model, newdata = new_car)
print(paste("Predicted Price for New Car:", new_prediction))
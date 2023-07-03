library(Quandl)
library(tidyverse)
library(tsibble)
library(feasts)
library(forecast)
library(gridExtra)
library(caret)

data = Quandl('BCB/433', type = "raw")

ggplot(data, aes(x=Date, y=Value))+
  geom_line()

ipca <- 
  data %>%
  filter(Date >= '2007-01-01' & Date <= '2023-07-01') %>%
  mutate(Date = yearmonth(Date)) %>%
  as_tsibble(index = Date)

ipca %>%
  ggplot(aes(x=Date, y=Value))+
  geom_line()

ipca %>%
  gg_subseries(Value)

acf1 = ggAcf(ipca$Value)
pacf1 = ggPacf(ipca$Value)
grid.arrange(acf1, pacf1)

ts_ipca = ts(ipca$Value, start = c(2007,01), frequency=12)

auto.arima(ts_ipca)



library(forecast)

# Convert ts_ipca to numeric if needed
ts_ipca <- as.numeric(ts_ipca)

# Check for missing values and remove them if necessary
ts_ipca <- ts_ipca[!is.na(ts_ipca)]

# Create a time series object
ts_ipca <- ts(ts_ipca)

# Split the data into training and testing sets
intrain <- createDataPartition(ts_ipca, p = 0.9, list = FALSE)
training <- ts_ipca[intrain]
testing <- ts_ipca[-intrain]

# Fit SARIMA model on the training data
fit_sarima <- auto.arima(training, seasonal = TRUE)

# Check residuals using ggtsdisplay and ggAcf plots
ggtsdisplay(residuals(fit_sarima))
ggAcf(residuals(fit_sarima))

# Perform Ljung-Box test on the residuals
Box.test(residuals(fit_sarima), lag = 24,
         fitdf = length(coef(fit_sarima)),
         type = "Ljung")

# Fit SARIMA model on the training data explicitly
sarima_treino <- Arima(training, order = c(1, 0, 0), seasonal = c(0, 0, 1))

# Forecast using the SARIMA model on the testing data
sarima_teste <- forecast(sarima_treino, h = length(testing))

# Calculate forecast accuracy measures
diag <- accuracy(sarima_teste, testing)
print(diag)

# Forecast using the fitted SARIMA model
fsarima <- forecast(fit_sarima, h = 12)
fsarima

autoplot(fsarima)

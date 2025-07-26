
# 0. Clear environment, console, and loaded plots
rm(list = ls())
cat("\014")
if (!is.null(dev.list())) dev.off()

# 1. Install required packages
install.packages("quantmod", "rugarch")
install.packages("RQuantLib")

# 2. Load necessary packages
library(quantmod)
library(rugarch)
library(RQuantLib)

# 3. Load Porsche stock data
getSymbols("P911.DE", src = "yahoo", from = "2022-09-29", auto.assign = TRUE)
porsche = na.omit(Cl(P911.DE))

# 4. Calculate log returns
log_returns = na.omit(diff(log(porsche)))

# Plot Porsche stock price and log return histogram
par(mfrow = c(2, 1))

# Top plot: Porsche stock price
plot(porsche, main = "Porsche Stock Price Since IPO", col = "darkred", lwd = 2,
     ylab = "Closing Price (EUR)", xlab = "Date")
grid()

# Bottom plot: Histogram of daily log returns
hist(log_returns, breaks = 50, col = "grey", border = "white",
     main = "Histogram of Daily Log Returns", xlab = "Log Return")
abline(v = mean(log_returns), col = "red", lwd = 2)

# 5. Calculate historical volatility
hist_vol_annualized = sd(log_returns) * sqrt(365)
cat("Annualized Historical Volatility:", round(hist_vol_annualized * 100, 2), "%\n")

# 6. Specify and estimate GARCH(1,1) model
garch_spec = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "norm"
)
garch_fit = ugarchfit(spec = garch_spec, data = log_returns, solver = "hybrid", fit.control = list(scale = 1))

# If model converges:
if (garch_fit@fit$convergence == 0) {
  garch_forecast = ugarchforecast(garch_fit, n.ahead = 1)
  sigma_tomorrow_garch = sigma(garch_forecast)[1]
  vol_tomorrow_annual = sigma_tomorrow_garch * sqrt(365)
  cat("Forecasted Annualized Volatility for Tomorrow (GARCH):", round(vol_tomorrow_annual * 100, 2), "%\n")
} else {
  warning("GARCH(1,1) model did not converge.")
  sigma_tomorrow_garch = NA
  vol_tomorrow_annual = NA
}

# 7. Specify and estimate ARCH(2) model
arch_spec = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(2, 0)),
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "norm"
)
arch_fit = ugarchfit(spec = arch_spec, data = log_returns, solver = "hybrid", fit.control = list(scale = 1))

# If model converges:
if (arch_fit@fit$convergence == 0) {
  arch_forecast = ugarchforecast(arch_fit, n.ahead = 1)
  sigma_tomorrow_arch = sigma(arch_forecast)[1]
  vol_tomorrow_annual_arch = sigma_tomorrow_arch * sqrt(365)
  cat("Forecasted Annualized Volatility for Tomorrow (ARCH):", round(vol_tomorrow_annual_arch * 100, 2), "%\n")
} else {
  warning("ARCH(1) model did not converge. Trying tseries::garch as fallback...")
    
  sigma_tomorrow_arch = NA
  vol_tomorrow_annual_arch = NA
}

# 7.5 Plot Conditional Volatility from GARCH and ARCH Models
garch_vol_series = sigma(garch_fit)
arch_vol_series = sigma(arch_fit)

# Convert to time series aligned with dates of returns
arch_vol_xts  = xts(sigma(arch_fit),  order.by = index(log_returns))
garch_vol_xts = xts(sigma(garch_fit), order.by = index(log_returns))

# Plot both volatility series
plot(garch_vol_xts, main = "Conditional Volatility (GARCH vs. ARCH)", col = "blue", lwd = 2, ylab = "Volatility", xlab = "Date")
lines(arch_vol_xts, col = "red", lwd = 2)
legend("topright", legend = c("GARCH(1,1)", "ARCH(2)"), col = c("blue", "red"), lwd = 2)

# 8. Prepare for Black-Scholes option pricing
S = as.numeric(last(porsche))
K = S * 1.1
maturity_days = 180
days_in_year = 365
rf_rate = 0.027

cat("Latest Porsche closing price:", round(S, 2), "EUR\n")
cat("S (Spot Price):", round(S, 2), "EUR\n")
cat("K (Strike Price = 10% above):", round(K, 2), "EUR\n")

# 9. Black-Scholes option pricing using GARCH volatility
if (!is.na(vol_tomorrow_annual)) {
  option_result_garch = EuropeanOption(
    type = "call",
    underlying = S,
    strike = K,
    dividendYield = 0,
    riskFreeRate = rf_rate,
    maturity = maturity_days / days_in_year,
    volatility = vol_tomorrow_annual
  )
  cat("Black-Scholes Call Option Price (GARCH volatility):", round(option_result_garch$value, 2), "EUR\n")
} else {
  cat("Could not compute option price with GARCH volatility (NA volatility).\n")  # Error message if volatility is NA
}

# 10. Black-Scholes option pricing using ARCH volatility
if (!is.na(vol_tomorrow_annual_arch)) {
  option_result_arch = EuropeanOption(
    type = "call",
    underlying = S,
    strike = K,
    dividendYield = 0,
    riskFreeRate = rf_rate,
    maturity = maturity_days / days_in_year,
    volatility = vol_tomorrow_annual_arch
  )
  cat("Black-Scholes Call Option Price (ARCH volatility):", round(option_result_arch$value, 2), "EUR\n")
} else {
  cat("Could not compute option price with ARCH volatility (NA volatility).\n")  # Error message if volatility is NA
}

# 11. Specify and estimate ARCH(1) model
arch1_spec = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),  # ARCH(1)
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "norm"
)
arch1_fit = ugarchfit(spec = arch1_spec, data = log_returns, solver = "hybrid", fit.control = list(scale = 1))

# If model converges:
if (arch1_fit@fit$convergence == 0) {
  arch1_forecast = ugarchforecast(arch1_fit, n.ahead = 1)
  sigma_tomorrow_arch1 = sigma(arch1_forecast)[1]
  vol_tomorrow_annual_arch1 = sigma_tomorrow_arch1 * sqrt(365)
  cat("Forecasted Annualized Volatility for Tomorrow (ARCH(1)):", round(vol_tomorrow_annual_arch1 * 100, 2), "%\n")
} else {
  warning("ARCH(1) model did not converge. Skipping ARCH(1) forecast...")
  sigma_tomorrow_arch1 = NA
  vol_tomorrow_annual_arch1 = NA
}

# 12. Black-Scholes option pricing using ARCH(1) volatility
if (!is.na(vol_tomorrow_annual_arch1)) {
  option_result_arch1 = EuropeanOption(
    type = "call",
    underlying = S,
    strike = K,
    dividendYield = 0,
    riskFreeRate = rf_rate,
    maturity = maturity_days / days_in_year,
    volatility = vol_tomorrow_annual_arch1
  )
  cat("Black-Scholes Call Option Price (ARCH(1) volatility):", round(option_result_arch1$value, 2), "EUR\n")
} else {
  cat("Could not compute option price with ARCH(1) volatility (NA volatility).\n")
}
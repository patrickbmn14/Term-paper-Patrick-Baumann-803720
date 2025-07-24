
# 0. Clear environment, console, and loaded plots
rm(list = ls())        # Remove all objects from the environment
cat("\014")            # Clear the console (equivalent to Ctrl+L)
if (!is.null(dev.list())) dev.off()  # Close any open graphics devices (plots)

# 1. Install required packages (only needed once)
install.packages("quantmod", "rugarch")     # quantmod: for downloading and analyzing market data, rugarch: for fitting GARCH models
install.packages("RQuantLib")               # RQuantLib: for option pricing (e.g. Black-Scholes)

# 2. Load necessary packages
library(quantmod)     # For retrieving financial data from Yahoo Finance
library(rugarch)      # For estimating volatility using GARCH models
library(RQuantLib)    # For pricing options using models like Black-Scholes

# 3. Load Porsche stock data
getSymbols("P911.DE", src = "yahoo", from = "2021-01-01", auto.assign = TRUE)  # Download Porsche stock data from Yahoo Finance starting Jan 1, 2021
porsche = na.omit(Cl(P911.DE))  # Extract closing prices and remove missing values (NAs)

# 4. Calculate log returns
log_returns = diff(log(porsche))[-1]        # Compute daily log returns (difference of logarithmic prices), skip first NA
log_returns = na.omit(as.numeric(log_returns))  # Convert to numeric and remove any remaining NAs

# Plot daily log returns
plot(index(porsche)[-1], log_returns, type = "l", col = "darkgreen",
     main = "Porsche Log Returns", xlab = "Date", ylab = "Log Return")
abline(h = 0, col = "gray")

# Plot daily log returns as Histogram
hist(log_returns, breaks = 50, col = "lightblue", border = "white",
     main = "Histogram of Log Returns", xlab = "Log Return")
abline(v = mean(log_returns), col = "red", lwd = 2)


# 5. Calculate historical (annualized) volatility
hist_vol_annualized = sd(log_returns) * sqrt(365)  # Standard deviation of daily returns scaled to annual volatility
cat("Annualized Historical Volatility:", round(hist_vol_annualized * 100, 2), "%\n")  # Print the result in percent

# 6. Specify and estimate GARCH(1,1) model
garch_spec = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),  # Define standard GARCH(1,1) model
  mean.model = list(armaOrder = c(0, 0)),                         # No ARMA component in the mean equation (assumes constant mean)
  distribution.model = "norm"                                     # Assumes normally distributed errors
)
garch_fit = ugarchfit(spec = garch_spec, data = log_returns, solver = "hybrid", fit.control = list(scale = 1))  # Fit the model using hybrid solver

# If model converges:
if (garch_fit@fit$convergence == 0) {
  garch_forecast = ugarchforecast(garch_fit, n.ahead = 1)            # Forecast 1 day ahead volatility
  sigma_tomorrow = sigma(garch_forecast)[1]                          # Extract forecasted standard deviation for tomorrow
  vol_tomorrow_annual = sigma_tomorrow * sqrt(365)                  # Annualize the forecasted volatility
  cat("Forecasted Annualized Volatility for Tomorrow (GARCH):", round(vol_tomorrow_annual * 100, 2), "%\n")  # Print result
} else {
  warning("GARCH(1,1) model did not converge.")                      # If fitting fails, show warning
  sigma_tomorrow = NA
  vol_tomorrow_annual = NA
}

# 7. Specify and estimate ARCH(1) model
arch_spec = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1, 0)),  # Define ARCH(1) model as special case of GARCH(1,0)
  mean.model = list(armaOrder = c(0, 0)),
  distribution.model = "norm"
)
arch_fit = ugarchfit(spec = arch_spec, data = log_returns, solver = "hybrid", fit.control = list(scale = 1))  # Fit the model

# If model converges:
if (arch_fit@fit$convergence == 0) {
  arch_forecast = ugarchforecast(arch_fit, n.ahead = 1)              # Forecast 1 day ahead volatility
  sigma_tomorrow_arch = sigma(arch_forecast)[1]                     # Extract forecasted standard deviation for ARCH(1)
  vol_tomorrow_annual_arch = sigma_tomorrow_arch * sqrt(365)       # Annualize the forecasted volatility
  cat("Forecasted Annualized Volatility for Tomorrow (ARCH):", round(vol_tomorrow_annual_arch * 100, 2), "%\n")
} else {
  warning("ARCH(1) model did not converge. Trying tseries::garch as fallback...")  # Print warning if model did not converge
  # Optional: fallback using the tseries package is commented out
  sigma_tomorrow_arch = NA
  vol_tomorrow_annual_arch = NA
}

# 7.5 (Optional): Plot Conditional Volatility from GARCH and ARCH Models
# Get conditional standard deviations (volatility) from fitted models
garch_vol_series = sigma(garch_fit)
arch_vol_series = sigma(arch_fit)

# Convert to time series aligned with dates of returns
garch_vol_xts = xts(garch_vol_series, order.by = index(porsche)[-(1:1)])  # Skip first value lost in log-return calc
arch_vol_xts = xts(arch_vol_series, order.by = index(porsche)[-(1:1)])

# Plot both volatility series
plot(garch_vol_xts, main = "Conditional Volatility (GARCH vs. ARCH)", col = "blue", lwd = 2, ylab = "Volatility", xlab = "Date")
lines(arch_vol_xts, col = "red", lwd = 2)
legend("topright", legend = c("GARCH(1,1)", "ARCH(1)"), col = c("blue", "red"), lwd = 2)

# 8. Prepare for Black-Scholes option pricing
S = as.numeric(last(porsche))      # Get the latest available Porsche stock price (spot price)
K = S * 1.10                       # Set strike price to 10% above the spot price
maturity_days = 180               # Option expires in 180 days
days_in_year = 365                # Number of days in a year (for scaling)
rf_rate = 0.0249                  # Annualized risk-free interest rate (e.g. 2.49%)

cat("Latest Porsche closing price:", round(S, 2), "EUR\n")             # Print spot price
cat("S (Spot Price):", round(S, 2), "EUR\n")                           # Print again (redundant)
cat("K (Strike Price = 10% above):", round(K, 2), "EUR\n")             # Print calculated strike price

# 9. Black-Scholes option pricing using GARCH volatility
if (!is.na(vol_tomorrow_annual)) {
  option_result_garch = EuropeanOption(
    type = "call",                # Type of option: call
    underlying = S,               # Current spot price
    strike = K,                   # Strike price
    dividendYield = 0,            # Assume no dividend yield
    riskFreeRate = rf_rate,       # Annual risk-free rate
    maturity = maturity_days / days_in_year,  # Convert maturity to years
    volatility = vol_tomorrow_annual          # Use GARCH-based annualized volatility
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

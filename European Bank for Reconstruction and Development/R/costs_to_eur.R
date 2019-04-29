#####
# Exchange rate data is needed, download it from Quandl.
# 500 API calls per day if registered (otherwise 50).
# My API token is included.
# Convert USD and RUB to EUR. If necessary (prior to 2000) convert to DM first
# and then to EUR.
# The function argument should a data frame with at least the columns
# CostEURm, CostUSDm, CostRUBm and date
#####
costs_to_eur <- function(projects_costs = projects_costs){
      library(Quandl)
      token <- "zwawuZSjKpJoGSCQRSVv"
      DMUSD <- Quandl("FRED/EXGEUS", authcode = Quandl.auth(token)) # First year 1971
      currencies <- c("EURRUB", "EURUSD", "EURPLN", "EURRON", "EURTRY")
      for (i in seq_along(currencies)){
            if (!exists(currencies[i])){
                  fx <- Quandl(paste0("QUANDL/", currencies[i]),
                               authcode = Quandl.auth(token))
                  fx <- fx[, c("Date", "Rate")]
                  assign(x = currencies[i], value = fx)
            }
      }
      # All the time series start in 1999
#       EURRUB <- Quandl("QUANDL/EURRUB", authcode = Quandl.auth(token))
#       EURRUB <- EURRUB[, c("Date", "Rate")] # First Date 1999-09-06
#       EURUSD <- Quandl("QUANDL/EURUSD", authcode = Quandl.auth(token))
#       EURUSD <- EURUSD[, c("Date", "Rate")] # First Date 1999-09-06
#       EURPLN <- Quandl("QUANDL/EURPLN", authcode = Quandl.auth(token))
#       EURPLN <- EURPLN[, c("Date", "Rate")] # First Date 1999-09-06
#       EURRON <- Quandl("QUANDL/EURRON", authcode = Quandl.auth(token))
#       EURRON <- EURRON[, c("Date", "Rate")] # First Date 1999-09-06
#       EURTRY <- Quandl("QUANDL/EURTRY", authcode = Quandl.auth(token))
#       EURTRY <- EURTRY[, c("Date", "Rate")] # First Date 1999-09-06
      
      for (i in 1:nrow(projects_costs)){
            date <- projects_costs$Date[i]
            year <- as.numeric(format(date, format = "%Y"))
            # Convert TRY to EUR
            if (!is.na(projects_costs$CostTLm[i])){ # TL, not TRY in EBRD data
                  # Use oldest available value if project date out of range
                  ifelse(test = is.na(EURTRY[EURTRY$Date == date, ]$Rate), 
                         yes = rate <- tail(EURTRY$Rate, 1),
                         no = rate <- EURTRY[EURTRY$Date == date, ]$Rate)
                  cost <- projects_costs$CostTLm[i] / rate
                  projects_costs$CostEURm[i] <- cost
            }
            # Convert RON to EUR
            if (!is.na(projects_costs$CostRONm[i])){
                  # Use oldest available value if project date out of range
                  ifelse(test = is.na(EURRON[EURRON$Date == date, ]$Rate), 
                         yes = rate <- tail(EURRON$Rate, 1),
                         no = rate <- EURRON[EURRON$Date == date, ]$Rate)
                  cost <- projects_costs$CostRONm[i] / rate
                  projects_costs$CostEURm[i] <- cost
            }
            # Convert PLN to EUR
            if (!is.na(projects_costs$CostPLNm[i])){
                  # Use oldest available value if project date out of range
                  ifelse(test = is.na(EURPLN[EURPLN$Date == date, ]$Rate), 
                         yes = rate <- tail(EURPLN$Rate, 1),
                         no = rate <- EURPLN[EURPLN$Date == date, ]$Rate)
                  cost <- projects_costs$CostPLNm[i] / rate
                  projects_costs$CostEURm[i] <- cost
            }
            # Convert RUB to EUR
            if (!is.na(projects_costs$CostRUBm[i])){
                  # Use oldest available value if project date out of range
                  ifelse(test = is.na(EURRUB[EURRUB$Date == date, ]$Rate), 
                         yes = rate <- tail(EURRUB$Rate, 1),
                         no = rate <- EURRUB[EURRUB$Date == date, ]$Rate)
                  cost <- projects_costs$CostRUBm[i] / rate
                  projects_costs$CostEURm[i] <- cost
            }
            # Convert USD to EUR or DM
            if (!is.na(projects_costs$CostUSDm[i])){
                  if (year < 2000){ # Convert USD to DM to EUR
                        # Only monthly data
                        date <- format(date, format = "%Y-%m")
                        date <- paste0(date, "-01")
                        rate <- DMUSD[DMUSD$Date == date, ]$Value
                        cost <- projects_costs$CostUSDm[i] / rate
                        # cost are DM now, convert to EUR
                        cost <- cost / 1.9558
                        projects_costs$CostEURm[i] <- cost
                  } else {
                        rate <- EURUSD[EURUSD$Date == date, ]$Rate
                        cost <- projects_costs$CostUSDm[i] / rate
                        projects_costs$CostEURm[i] <- cost
                  }
            }
      }
      return(projects_costs)
}
#####
# Convert EUR to current EUR, taking inflation into account
# CPI Euro Area will be used for the adjustement.
# Convert all project costs in EUR to current EUR. "Current" refers to June 2014 here.
# "costs" in the following function should be a data frame with date in the
# first column ("YYYY-MM-DD" or "YYYY-MM") and the costs in EUR in the second column:
#####
Sys.setlocale("LC_TIME", "C")
eur_to_current <- function(cost){
      library(Quandl)
      library(zoo)
      dates <- cost[,1]
      eur <- cost[,2]
      if(!exists("CPI")){
            token <- "zwawuZSjKpJoGSCQRSVv"
            # 1990 - 2014. 2005 = Index 100. Monthly data.
            CPI <- Quandl("RATEINF/CPI_EUR", collapse="monthly", 
                          authcode = Quandl.auth(token)) 
      }
      CPI$Date <- as.yearmon(CPI$Date)
      dates <- as.yearmon(dates) # drop day
      cpiCurr <- CPI[CPI$Date == "Jun 2014",]$CPI
      eurCurr <- apply(X = data.frame(dates, eur), 
                       MARGIN = 1, 
                       FUN = function(x){
                             as.numeric(x[2]) * (cpiCurr / CPI[CPI$Date == x[1], ]$CPI)
                       })
      return(eurCurr)
}
#####
# Function to translate German month abbreviations into English.
# Possibly not necessary, but does not do any harm if the dates
# are already in English.
#####
translate_date <- function(date){
      Sys.setlocale("LC_TIME", "C")
      foreign_dates <- c("Mrz", "Mai", "Okt", "Dez")
      eng_dates <- c("Mar", "May", "Oct", "Dec")
      split <- unlist(strsplit(date, split = " "))
      # split[2] is the month name (abbreviated)
      if (split[2] %in% foreign_dates){
            split[2] <- eng_dates[which(foreign_dates == split[2])]
      }
      date <- paste(split, collapse = " ")
      return(date)
}
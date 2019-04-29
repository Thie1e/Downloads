#####
# Function to load the manually saved webpages into R
# The extracted tables with columns date, project ID, country, project title, sector, 
# public/private and status are then merged and returned
#####
get_projects <- function(pathtofiles){
      library(XML)
      files <- list.files(pathtofiles, pattern = "htm")
      files <- paste0(pathtofiles, files) # Full path 
      projects <- data.frame(matrix(NA, ncol = 7, nrow = length(files) * 25))
      for (file in seq_along(files)){
            doc <- htmlTreeParse(files[file], isURL = F, useInternalNodes = T)
            root <- xmlRoot(doc)
            table <- readHTMLTable(root)
            ifelse(file == 1, 
                   projects <- table[2:26, ], 
                   projects <- rbind(projects, table[2:26, ]))
      }
      
      colnames(projects) <- c("Date", "ProjectID", "Country", "Project Title", 
                              "Sector", "Public/Private", "Status")
      dates <- as.character(projects$Date)
      for (i in seq_along(dates)){
            dates[i] <- translate_date(dates[i])
      }
      projects$Date <- as.Date(dates, format = "%d %b %Y")
      return(projects)
}
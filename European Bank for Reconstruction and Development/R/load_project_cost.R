#####
# Extract project cost from seperate project page
# Generate the URL, download the webpage and extract the paragraph about
# project costs
#####
# Function argument should be one row of the projects table
load_project_cost <- function(project){
      library(XML)
      # Random sleep interval to prevent getting booted
      Sys.sleep(sample(seq(1, 3, by=0.01), 1))
      message("Loading project")
      date <- as.Date(project$Date, format = "%d %b %Y")
      year <- format(date, "%Y")
      id <- as.character(project$ProjectID)
      # http://www.ebrd.com/english/pages/project/psd/YEAR/PROJECTID.shtml
      url <- paste0("http://www.ebrd.com/english/pages/project/psd/", 
                    year, "/", id, ".shtml")
      # Check if the page can be loaded, if not return cost = NA
      doc <- try(htmlTreeParse(url, useInternalNodes = T))
      if (class(doc)[1] == "try-error") {
            return(c(ProjectID = project$ProjectID, cost = NA, url = url))
      } else {
            root <- xmlRoot(doc) 
            # This xpath statement looks for an h2 (heading) with the text 
            # "Project Cost" and if found returns the following sibling's text
            # by which is the paragraph under the heading by applying xmlValue()
            # (Not sure if normalize space is necessary)
            xpath <- paste0("//h2[text()[normalize-space(.)='Project Cost']]",
                            "/following-sibling::*[position()=1 and self::p]")
            cost <- xpathSApply(doc, 
                                path = xpath,
                                fun = xmlValue) 
            if (is.null(cost)) cost <- "Project cost not found"
            print(paste(id, cost, url))     
            return(c(ProjectID = id, cost = cost, url = url))
      }
}
joinCountryData2MapNoCat <- function (dF, 
                                      joinCode = "ISO3", 
                                      nameJoinColumn = "ISO3V10", 
                                      nameCountryColumn = "Country", 
                                      suggestForFailedCodes = FALSE, 
                                      mapResolution = "coarse", 
                                      projection = NA, 
                                      verbose = FALSE) 
{
      mapWithData <- getMap(resolution = mapResolution)
      if (!is.na(projection)) 
            warning("the projection argument has been deprecated, returning Lat Lon, use spTransform from package rgdal as shown in help details or the FAQ")
      listJoinCodesNew <- c("ISO_A2", "ISO_A3", "FIPS_10_", "ADMIN", 
                            "ISO_N3")
      listJoinCodesOld <- c("ISO2", "ISO3", "FIPS", "NAME", "UN")
      listJoinCodes <- c(listJoinCodesOld, listJoinCodesNew)
      if (joinCode %in% listJoinCodes == FALSE) {
            stop("your joinCode (", joinCode, ") in joinCountryData2Map() is not one of those supported. Options are :", 
                 paste(listJoinCodes, ""), "\n")
            return(FALSE)
      }
      joinCodeOld <- joinCode
      if (joinCode %in% listJoinCodesOld) {
            joinCode <- listJoinCodesNew[match(joinCode, listJoinCodesOld)]
      }
      if (is.na(match(nameJoinColumn, names(dF)))) {
            stop("your chosen nameJoinColumn :'", nameJoinColumn, 
                 "' seems not to exist in your data, columns = ", 
                 paste(names(dF), ""))
            return(FALSE)
      }
      dF[[joinCode]] <- as.character(dF[[nameJoinColumn]])
      dF[[joinCode]] <- gsub("[[:space:]]*$", "", dF[[joinCode]])
      if (joinCode == "ADMIN") {
            dF$ISO3 <- NA
            for (i in 1:nrow(dF)) dF$ISO3[i] = rwmGetISO3(dF[[joinCode]][i])
            joinCode = "ISO3"
            nameCountryColumn = nameJoinColumn
      }
      matchPosnsInLookup <- match(as.character(dF[[joinCode]]), 
                                  as.character(mapWithData@data[[joinCode]]))
      failedCodes <- dF[[joinCode]][is.na(matchPosnsInLookup)]
      numFailedCodes <- length(failedCodes)
      numMatchedCountries <- nrow(dF) - numFailedCodes
      message(paste(numMatchedCountries, "codes from your data successfully matched countries in the map"))
      failedCountries <- dF[[nameCountryColumn]][is.na(matchPosnsInLookup)]
      failedCountries <- cbind(failedCodes, failedCountries = as.character(failedCountries))
      message(paste(numFailedCodes, "codes from your data failed to match with a country code in the map"))
      if (verbose){
            message("These countries failed to match:")
            message(paste(failedCountries[,2], collapse = " "))
      }
      matchPosnsInUserData <- match(as.character(mapWithData@data[[joinCode]]), 
                                    as.character(dF[[joinCode]]))
      codesMissingFromUserData <- as.character(mapWithData@data[[joinCode]][is.na(matchPosnsInUserData)])
      countriesMissingFromUserData <- as.character(mapWithData@data[["NAME"]][is.na(matchPosnsInUserData)])
      numMissingCodes <- length(codesMissingFromUserData)
      message(paste(numMissingCodes, "codes from the map weren't represented in your data"))
      mapWithData@data <- cbind(mapWithData@data, dF[matchPosnsInUserData, 
                                                     ])
      invisible(mapWithData)
}
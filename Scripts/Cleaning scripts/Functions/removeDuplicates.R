removeDuplicates <- function(inputDF) { # create a function with the name my_function


  # Making unique identifier for each assessment by combining co-id and assessment date.
  if (any(names(inputDF) == "Date.of.Assessment"))  {
  inputDF$coid_assedate <- paste(inputDF$CO.ID, inputDF$Date.of.Assessment, sep = " ")
  } else if (any(names(inputDF) == "Date")) {
    inputDF$coid_assedate <- paste(inputDF$CO.ID, inputDF$Date, sep = " ")
    
  } else {
    stop("variable name not in DF")
  }


  
  # Checking for any duplicate entries (i.e.: any duplicates in the newly made unique identifier).
  # list of duplicates frame gives output of duplicate candidates

  duplicates <- freq(inputDF$coid_assedate)
  duplicatesframe <- as.data.frame(duplicates)
  duplicatesframe <- rownames_to_column(duplicatesframe, var="idvisitdate")

  # Now to remove any non-duplicates in 'duplicatesframe' to show us what the actual duplicates are and how many times they've been duplicated

  duplicatesframe <- duplicatesframe[duplicatesframe$idvisitdate != c("Total", "<NA>"),]
duplicatesframe <- duplicatesframe[-c(3:6)]

duplicateassessments <- duplicatesframe %>%
  filter(Freq>1)


  # Make a new vector to use as list of duplicate entries to remove from inputDF dataframe and then remove duplicated entries from inputDF dataframe with a subset.
  # Because theres no way of knowing which duplicate entry is the correct one, all duplicates are removed.
  idvisitstoremove <- duplicateassessments$idvisitdate
  
  inputDFnoduplicates <- subset(inputDF, !coid_assedate %in% idvisitstoremove)
  
  freqcheck <- as.data.frame(freq(inputDFnoduplicates$coid_assedate))
  freqcheck <- rownames_to_column(freqcheck, var="idvisitdate")
  freqcheck <- freqcheck[freqcheck$idvisitdate != c("Total", "<NA>"),]
  
  # freqcheck

  
  # Making a dataframe of the removed duplicate assessment entries
  toremove <- duplicateassessments$idvisitdate
  removedrecords <- subset(inputDF, coid_assedate %in% toremove)

  
  # Because I've removed all duplicate assessment entries, we might have lost participant/s if their only entry is one of these removed duplicates.
#This will show which inputDF, if any, have been lost.

duplicateassessments$id <- sapply(strsplit(duplicateassessments$idvisitdate," "), `[`, 1)
duplicateassessments$visitdate<- sapply(strsplit(duplicateassessments$idvisitdate," "), `[`, 2)

toremovelostinputDF <- inputDFnoduplicates$CO.ID

lostinputDF <- NA
lostinputDF <- subset(inputDF, !CO.ID %in% toremovelostinputDF)


idvisitstoremove <- duplicateassessments$idvisitdate

outputDF <- subset(inputDF, !coid_assedate %in% idvisitstoremove)

freqcheck <- as.data.frame(freq(outputDF$coid_assedate))
freqcheck <- rownames_to_column(freqcheck, var="idvisitdate")
freqcheck <- freqcheck[freqcheck$idvisitdate != c("Total", "<NA>"),]


return(list(noDupes = outputDF, check = freqcheck, dupes = duplicatesframe))

  
}
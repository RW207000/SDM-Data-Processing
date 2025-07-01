mergeBioMarkerEntries <- function (rawBM) {
  
  fixedBM <- rawBM[1,]
  rowNum = 1
  
  BMcolNums <- 5:(ncol(rawBM))
  
  uniqueAssessmentIDs <- unique(rawBM$Assessment.ID)
  
  for (uniqueID in uniqueAssessmentIDs) {
    

    thisBM <- rawBM[rawBM$Assessment.ID == uniqueID, ]
    thisBMfix <- thisBM[1,]
    
    if (nrow(thisBM) > 1) { # is there more than one entry?
      
      earliestDate = min(thisBM$Date)
      
      for (colNum in BMcolNums) { # loop over columns
        
        vals <- thisBM[, colNum, drop = TRUE]
        
        
        if (!(all(is.na(vals)) | all(is.null(vals)))) { # is the whole section not empty or NULL?
          
          vals = vals[!is.na(vals)] # remove NA values
          
          if (typeof(vals) == "character") { # is the data text
            
            if (n_unique(vals) == 1) {
              correct <- unique(vals)
            }
            
            else if (n_unique(vals[vals != "NULL"]) == 1) {
              correct <- unique(vals[vals != "NULL"])
            }
            
            else {
              #print("More than one non-NULL value, unsure which to pick! Values:")
              print(vals)
              correct = "NULL" # otherwise use null, and flag it up
            }
   
          }
          else if (typeof(vals) == "double") {
            
            if (n_unique(vals) == 1 & !any(is.na(vals))) {
              correct  = unique(vals)
            }
            else if (sum(!is.na(vals)) == 1) { 
              correct = vals[!is.na(vals)] # if there is only one non-NA value, use that one
            }
            else {
              correct = NA
              #print("Multiple non-NA values, unsure which to pick! Values: ")
              print(vals)
            }
            
          }
          
        }
        else {
          correct <- vals[1]
        }
        
        thisBMfix[1, colNum] <- correct
        
      }
      
    }
    
    else {
      thisBMfix = thisBM
    }
    
    fixedBM[rowNum,] = thisBMfix
    rowNum = rowNum + 1
    
  }
  
  return(fixedBM)
  
}



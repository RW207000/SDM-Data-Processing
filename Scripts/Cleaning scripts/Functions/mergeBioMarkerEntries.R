mergeBioMarkerEntries <- function (rawBM) {
  
  fixedBM <- rawBM[1,]
  rowNum = 1
  
  BMcolNums <- 5:(ncol(rawBM))
  
  uniqueAssessmentIDs <- unique(rawBM$Assessment.ID)
  
  for (uniqueID in uniqueAssessmentIDs) {
    
    thisBM <- rawBM[rawBM$Assessment.ID == uniqueID, ]
    thisBMfix <- thisBM[1,]
    
    if (nrow(thisBM) > 1) {
      
      earliestDate = min(thisBM$Date)
      
      for (colNum in BMcolNums) {
        
        vals <- thisBM[, colNum, drop = TRUE]
        #print(vals)
        
        if (!(all(is.na(vals)) | all(is.null(vals)))) { # is the whole section not empty?
          
          if (typeof(vals) == "character") {
            
            a = 1
            
            if (all(vals == "NULL")) {
              correct = "NULL"
            }
            else if (sum(vals == "NULL") != 1) {
              correct = vals[vals != "NULL"]
            }
            else {
              print("More than one non-NULL value, unsure which to pick!")
              correct = "NULL"            
            }
            
            thisBMfix[1, colNum] <- correct
            
          }
          else if (typeof(vals) == "double") {
            
            if (n_unique(vals) == 1) {
              correct  = unique(vals)
            }
            else {
              correct = vals[!is.na(vals)]
            }
            
          }
          
        }
        
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



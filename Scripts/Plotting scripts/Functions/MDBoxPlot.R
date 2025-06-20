MDBoxPlot <- function (MDS, Param, splitType, ttl) {
  
  if (splitType == "None") {
    #select only relevant data
    paramData <- select(MDS, c("Centre.ID.x", "Sex.at.birth", Param))
    
    #change centre ID to factor
    paramData$Centre.ID.x <- as.factor(paramData$Centre.ID.x)
    
    #remove NA from param, 3rd column have parameters to plot
    paramData <- paramData[!is.na(paramData[,3]),]
    
    #rename 3rd column to param so we can reference it
    paramData <- paramData %>% rename(param = 3)
    
    #box plot
    graph <- ggplot(data = paramData, aes(x = Centre.ID.x, y = param)) +
      geom_boxplot(position = position_dodge(preserve='single')) +
      labs(title = ttl, x = "Centre ID", y = Param) +
      theme_classic() +
      scale_fill_manual(values = myColours) +
      geom_point(position = position_dodge(width = 0.75),
                 aes(group = Centre.ID.x))
    
  } else if(splitType == "SBS") {
    
    #select only relevant data
    paramData <- select(MDS, c("Centre.ID.x", "Sex.at.birth", Param))
    
    #change centre ID to factor
    paramData$Centre.ID.x <- as.factor(paramData$Centre.ID.x)
    
    #remove NA from param, 3rd column have parameters to plot
    paramData <- paramData[!is.na(paramData[,3]),]
    
    #rename 3rd column to param so we can reference it
    paramData <- paramData %>% rename(param = 3)
    
    #box plot
    graph <- ggplot(data = paramData, aes(x = Centre.ID.x, y = param, fill = Sex.at.birth, col = Sex.at.birth)) +
      geom_boxplot(position = position_dodge2(preserve='single'),
      ) +
      labs(title = ttl, x = "Centre ID", y = Param) +
      theme_classic() +
      theme() +
      scale_fill_manual(values = myColours) +
      geom_point(position = position_dodge(width = 0.75),
                 aes(group = Sex.at.birth))
    
  } else if(splitType == "NTEO") {
    #select only relevant data
    paramData <- select(MDS, c("Centre.ID.x", "Sex.at.birth", Param))
    
    #change centre ID to factor
    paramData$Centre.ID.x <- as.factor(paramData$Centre.ID.x)
    
    #remove NA from param, 3rd column have parameters to plot
    paramData <- paramData[!is.na(paramData[,3]),]
    
    #rename 3rd column to param so we can reference it
    paramData <- paramData %>% rename(param = 3)
    
    #box plot
    graph <- ggplot(data = paramData, aes(x = Centre.ID.x, y = param, fill = Sex.at.birth, col = Sex.at.birth)) +
      geom_boxplot(position = position_dodge2(preserve='single')) +
      labs(title = ttl, x = "Centre ID", y = Param) + 
      theme_classic() +
      scale_fill_manual(values = myColours) +
      facet_wrap(~ Sex.at.birth) +
      geom_point()
    
  } else if(splitType == "Sex only") {
    
    #select only relevant data
    paramData <- select(MDS, c("Centre.ID.x", "Sex.at.birth", Param))
    
    #change centre ID to factor
    paramData$Centre.ID.x <- as.factor(paramData$Centre.ID.x)
    
    #remove NA from param, 3rd column have parameters to plot
    paramData <- paramData[!is.na(paramData[,3]),]
    
    #rename 3rd column to param so we can reference it
    paramData <- paramData %>% rename(param = 3)
    
    #box plot
    graph <- ggplot(data = paramData, aes(x = Sex.at.birth, y = param, fill = Sex.at.birth, col = Sex.at.birth)) +
      geom_boxplot(position = position_dodge2(preserve='single'),
      ) +
      labs(title = ttl, x = "Centre ID", y = Param) +
      theme_classic() +
      theme() +
      scale_fill_manual(values = myColours) +
      geom_point(position = position_dodge(width = 0.75),
                 aes(group = Sex.at.birth))
    
  } else {
    print("Wrong split type, use None, SBS, or NTEO")
  }
  
  
  
  
  return(graph)
}
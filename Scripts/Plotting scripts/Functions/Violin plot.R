Violin <- function (MDS, Param, ttl, labelHeight) {
  
    #select only relevant data
    paramData <- select(MDS, c("Centre.ID.x", "Sex.at.birth", Param))
    
    #change centre ID to factor
    paramData$Centre.ID.x <- as.factor(paramData$Centre.ID.x)
    
    #remove NA from param, 3rd column have parameters to plot
    paramData <- paramData[!is.na(paramData[,3]),]
    
    #rename 3rd column to param so we can reference it
    paramData <- paramData %>% rename(param = 3)
  
    paramData$i <- 1
    
    labs <- aggregate(i~Sex.at.birth,paramData,sum)
    labs$param<-NA
    
    #violin plot
    graph <- ggplot(data = paramData, aes(x = Sex.at.birth, y = param, fill = Sex.at.birth)) +
      geom_violin() +
      theme_classic() +
      geom_point(position = position_jitter(seed = 1, width = 0.2)) +
      labs(title = ttl, x = "Sex at birth", y = Param) +
      theme(legend.position = "none") +
      geom_text(data=labs,aes(x=Sex.at.birth,y=labelHeight,label=i, hjust = -0.7))
    
  return(graph)
  }

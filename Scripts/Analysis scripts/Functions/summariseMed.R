summariseMed <- function(minimalDataset, medicine, dose_type) {

  medData <- all_data[all_data$Medicine_1 == medicine,]
  
  if (nrow(medData) == 0) {
    print("no medicines with that name")
    return()
  }
  
  if (dose_type == "absoloute") {
  
  StatSummary <- medData %>%
    group_by(Centre.ID.x) %>%
    summarise("mean" = mean(Total_raw_dose, na.rm = T),
              "median" = median(Total_raw_dose, na.rm = T),
              "SD" = sd(Total_raw_dose, na.rm = T),
              "min" = min(Total_raw_dose, na.rm = T),
              "max" = max(Total_raw_dose, na.rm = T),
              "n" = length(Total_raw_dose[!is.na(Total_raw_dose)])) %>%
    mutate_if(is.numeric, ~round(., 2))
  }
  
  else if (dose_type == "equiv") {
    StatSummary <- medData %>%
      group_by(Centre.ID.x) %>%
      summarise("mean" = mean(HC_equivalent_daily_dose_1_1, na.rm = T),
                "median" = median(HC_equivalent_daily_dose_1_1, na.rm = T),
                "SD" = sd(HC_equivalent_daily_dose_1_1, na.rm = T),
                "min" = min(HC_equivalent_daily_dose_1_1, na.rm = T),
                "max" = max(HC_equivalent_daily_dose_1_1, na.rm = T),
                "n" = length(HC_equivalent_daily_dose_1_1[!is.na(HC_equivalent_daily_dose_1_1)])) %>%
      mutate_if(is.numeric, ~round(., 2)) 
    
  }
    
  else if (dose_type == "BSA") {
    StatSummary <- medData %>%
      group_by(Centre.ID.x) %>%
      summarise("mean" = mean(HC_equivalent_daily_dose_per_BSA, na.rm = T),
                "median" = median(HC_equivalent_daily_dose_per_BSA, na.rm = T),
                "SD" = sd(HC_equivalent_daily_dose_per_BSA, na.rm = T),
                "min" = min(HC_equivalent_daily_dose_per_BSA, na.rm = T),
                "max" = max(HC_equivalent_daily_dose_per_BSA, na.rm = T),
                "n" = length(HC_equivalent_daily_dose_per_BSA[!is.na(HC_equivalent_daily_dose_per_BSA)])) %>%
      mutate_if(is.numeric, ~round(., 2)) 
  
  }
  
  else {
    print("incorrect dose type")
  }
  
  return (StatSummary)
  }

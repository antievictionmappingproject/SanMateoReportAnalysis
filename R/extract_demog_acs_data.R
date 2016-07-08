# Read in and clean the ACS summary statistics, extract needed demographic data
# in a friendly format

extract_simplified_race_ACS <- function() {
  ACS_data_full <- read.csv("data/ACS_2014/ACS_14_5YR_DP05.csv",skip=1)
  
  #extract only cols with stats about race (counting each occurence, even
  # in biracial people, as 1)
  ACS_data <- ACS_data_full[,grepl("Race.alone.or.in.combination", names(ACS_data_full))]
  
  # only percentages
  ACS_data <- ACS_data[,grepl("Percent..RACE", names(ACS_data))]
  
  
  names(ACS_data) <- gsub("(.*)\\.\\.\\.", "", names(ACS_data))
  ACS_data <- ACS_data[,names(ACS_data) != "Total.population"]
  
  # add the hispanic stat
  ACS_data$Hispanic <- ACS_data_full[,grepl("Percent\\.\\.(.*)of.any.race.$", names(ACS_data_full))]
  
  
  simplified_race_pct_acs <- as.numeric(ACS_data[1,])
  names(simplified_race_pct_acs) <- names(ACS_data)
  
  names(simplified_race_pct_acs)[names(simplified_race_pct_acs) == "Some.other.race"] <- "Other"
  
  simplified_race_pct_acs
}

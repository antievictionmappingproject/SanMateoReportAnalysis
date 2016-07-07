cleaning_main <- function() {
  # cd to data dir
  setwd("data/")
  
  SM_Evictions <- read.csv("san mateo eviction data.csv", stringsAsFactors = F)
  
  # clean up (chr -> num, replace Unknown with NA)
  numeric_cols <- c("TRACTCE", "poverty", "income", "adults", "children", 
                    "number.of.persons.in.household", "client_age", "zip")
  
  # we suppress the NA coercion warnings because the effect is intended
  for (colname in numeric_cols) {
    SM_Evictions[,colname] <- suppressWarnings(as.numeric(SM_Evictions[,colname]))
  }
  
  
  # clean up (chr->bool)
  bool_cols <- c("Is.Section.8.Client.", "Other.subsidized.housing.", "veteran_household",
                 "disabled", "disabled.person.in.household", "female.headed.household.",
                 "seniors.in.household.", "Originally.a.no.cause.case.")
  for (colname in bool_cols) {
    SM_Evictions[,colname] <- sapply(SM_Evictions[,colname], function(x) {
      if (x == "Yes") { TRUE } else if (x == "No") { FALSE } else { NA }
    })
  }
  
  
  #clean up (chr->Date)
  SM_Evictions$date_Opened <- as.Date(SM_Evictions$date_Opened, format = "%m/%d/%Y")
  
  require(gdata)  # for that handy startsWith function
  # add a cause/nocause bool column, might be interesting
  SM_Evictions$with_cause <- sapply(SM_Evictions$Type.of.Eviction, function(x) {
    if (startsWith(x, "Cause")) { TRUE } else if (startsWith(x, "No-Cause")) { FALSE } else {NA}
  })
  sum(is.na(SM_Evictions$with_cause)) # 809 unknown/other (?)
  
  
  #clean up (add NAs to result col)
  SM_Evictions$Result[SM_Evictions$Result == ""] <- NA
  
  
  
  # Adding more useful columns:
  #simplified_races <- c("Hispanic", "White", "Black", "Native Hawaiian/Pacific Islander",
  #                      "Asian", "American Indian/Alaska Native", "Unknown")
  
  
  # add the any disabled col
  disability_household <- SM_Evictions$disabled.person.in.household
  disability_client <- SM_Evictions$disabled
  
  NA2FALSE <- function(x) {
    x[is.na(x)] <- FALSE
    x
  }
  
  disability_household <- NA2FALSE(disability_household)
  disability_client <- NA2FALSE(disability_client)
  
  
  # Add relevant date columns
  SM_Evictions$any_disabled <- (disability_client | disability_household)
  
  SM_Evictions$date_Year <- as.numeric(format(SM_Evictions$date_Opened, "%Y"))
  SM_Evictions$date_Month <- as.numeric(format(SM_Evictions$date_Opened, "%m"))
  
  SM_Evictions$date_Quarter <- sapply(SM_Evictions$date_Month, function(month){
    if (is.na(month)) { return(NA) }
    if (month <= 3) { 1 } else if (month <= 6) { 2 } else if (month <= 9) { 3 } else { 4 }
  })
  
  SM_Evictions$date_YearQuarter <- paste(SM_Evictions$date_Year, SM_Evictions$date_Quarter, 
                                         sep="Q")
  SM_Evictions$date_YearQuarter[grepl("NA",SM_Evictions$date_YearQuarter)] <- NA
  
  SM_Evictions
}


SM_Evictions <- cleaning_main()
rm("cleaning_main")
setwd("../")

if(!exists("SM_Evictions")){
  source('R/read_clean_expand_data.R')
}
source('R/frequency_functions.R')



###
# PLOTS AND ANALYSIS
###


## CITY / AREA FREQUENCY
city_freq <- sortedfreq(SM_Evictions$city, n = 10)

# make labels perpindicular to axis
par(las = 2)
# increase y axis margin
par(mar=c(5,8,4,2))

# city frequency barplot (note reversed order to get display right)
barplot(rev(city_freq), horiz= T, cex.names = 0.8,
        main = "Eviction frequency by city",
        col = rev(heat.colors(length(city_freq))))


# RACE/ETHNICITY FREQUENCY
race_freq <- sortedfreq(SM_Evictions$race)
# length(race_freq) # 35 categories
top_race <- head(race_freq, n = 20)


# generate a simplified race frequency table. Count mixed race as a count
# for both races. Collapse all Asian and pacific islander into two separate
# categories.
simplified_races <- c("Hispanic", "White", "Black", "Native Hawaiian/Pacific Islander",
                      "Asian", "American Indian/Alaska Native", "Unknown")
race_freq_simplified <- sapply(simplified_races, function (race) {
  sum(grepl(race, SM_Evictions$race))
})
race_freq_simplified["Other"] <- sum(grepl("Other", 
                                           gsub(":(.*)","",SM_Evictions$race)))
race_freq_simplified <- sort(race_freq_simplified, decreasing = T)

# plot the simplified race frequency.
par(mar=c(5,12,4,2))
barplot(rev(race_freq_simplified), horiz = T, cex.names=0.8,
        main = "Eviction frequency by race (simplified)",
        col = cm.colors(length(race_freq_simplified)))



## AGE FREQUENCY
par(mar=c(5,5,4,2))
hist(SM_Evictions$client_age,
     main = "Eviction frequency by known client age")


## CAUSE VS NO CAUSE FREQUENCY
# note that over 800 are not listed as cause vs no cause
cause_freq <- table(SM_Evictions$with_cause)
barplot(cause_freq, names = c("No cause", "Cause"),
        main = "Cause vs. No cause Eviction Frequency")

# percentages
cause_freq["FALSE"] / sum(cause_freq) # 34.5% no cause
cause_freq["TRUE"] / sum(cause_freq)  # 65.5% cause


## EVICTION TYPE FREQUENCY
type_freq <- sort(table(SM_Evictions$Type.of.Eviction))

par(mar=c(5,15,4,2))
barplot(type_freq, horiz=T,
        main = "Eviction Frequency by Type of Eviction",
        col = rev(terrain.colors(length(type_freq))))


## RESULT FREQUENCY
# the MAJORITY of cases do not list a result, but here we display
# frequencies for the ones that do
result_freq <- sort(table(SM_Evictions$Result))
par(mar=c(8,5,4,2))
barplot(result_freq,
        main = "Result Frequencies (for Cases with Recorded Result)")
# perecentages
result_freq["Pay and stay"] / sum(result_freq)  # 24% pay and stay
result_freq["Move"] / sum(result_freq) # 75% move


## POVERTY LEVEL / INCOME FREQUENCY
# the limit excludes outliers
hist(SM_Evictions$poverty[SM_Evictions$poverty < 600],
     main = "Eviction frequency by poverty level of client",
     xlab = "Poverty level")
# the limit excludes outliers
par(mgp = c(4,1,0), mar=c(8,8,4,2))
hist(SM_Evictions$income[SM_Evictions$income < 90000],
     main = "Eviction frequency by income of client",
     xlab = "Annual income")

## EMPLOYMENT
#barplot(table(SM_Evictions$income1), cex.names = 0.8, horiz= T)


## DISABILITY FREQUENCY
any_disabled_freq <- table(SM_Evictions$any_disabled)

barplot(any_disabled_freq, names = c("None or unknown", "Disability"),
        main = "Frequency of eviction w/ Disability in household")


# YEAR, QUARTER FREQ
barplot(height = table(SM_Evictions$date_YearQuarter))
barplot(table(SM_Evictions$date_Year))



# Language
lang_freq <- head(sort(table(SM_Evictions$language), decreasing = T), n = 10)
barplot(lang_freq, horiz = T, cex.names = 0.8,
        main = "Eviction cases by language")


# children
SM_Evictions$children_category <- as.character(SM_Evictions$children)
child_category_freq <- head(sort(table(SM_Evictions$children_category), decreasing = T), n = 7)
barplot(child_category_freq, main = "Eviction cases by number of children")


#ge
rm(list = ls())
require(timeDate)
require(lubridate)

# holidays
# Define a function to get the dates of German holidays
german_holidays <- function(year) {
  # Easter Sunday
  easter_sunday <- as.Date(timeDate::holiday(year, Holiday = "Easter"))
  
  # Calculate other holidays based on Easter Sunday
  holidays <- c(
    as.Date(paste(year, "-01-01", sep = "")),
    as.Date(paste(year, "-01-06", sep = "")),  # Heilige Drei Könige
    easter_sunday - days(2),
    easter_sunday + days(1),
    as.Date(paste(year, "-05-01", sep = "")),
    easter_sunday + days(39),
    easter_sunday + days(50),
    easter_sunday + days(60),                   # Fronleichnam
    as.Date(paste(year, "-10-03", sep = "")),
    as.Date(paste(year, "-10-31", sep = "")),   # Reformationstag
    as.Date(paste(year, "-11-01", sep = "")),   # Allerheiligen
    as.Date(paste(year, "-12-24", sep = "")),   # Heiligabend: People generally work part-time that day
    as.Date(paste(year, "-12-25", sep = "")),
    as.Date(paste(year, "-12-26", sep = ""))
  )
  
  return(holidays)
}

# Get German holidays for the year 2024
holidays <- c(german_holidays(2018), german_holidays(2019), german_holidays(2020), german_holidays(2021),
                     german_holidays(2022), german_holidays(2023), german_holidays(2024))

holidays <- holidays[order(holidays)]

print(holidays)

saveRDS(holidays, "seminar/holidays.RDS")
write.csv(holidays, "seminar/holidays.csv", row.names = FALSE, col.names = FALSE)

#----------------------------------------------------------------------------------------------------------------------------------------------
#   Calgary Transit - Incident Log (2016 - 2020) | When and where is it the safest to drive in Calgary?
#   Data Collection | Data Visualization
#   Source - https://data.calgary.ca/Transportation-Transit/Traffic-Incidents/35ra-9556
#   By: Ethan Mah
#   BTMA 431 - Mini Project 1
#   November 17, 2020
#----------------------------------------------------------------------------------------------------------------------------------------------
#   Packages used in this project:

install.packages("jsonlite")
library(jsonlite)
library(dplyr)

#----------------------------------------------------------------------------------------------------------------------------------------------
#   Data Retrieval / Wrangling:

# Currently, when querying a dataset using an API, R studio will default return the first 1000 rows.
#   This becomes an issue when using big data with more than 1000 rows of data.
#   Therefore, we must slightly modify the API data provided by the City of Calgary by setting a limit parameter (?limit=25000).
url<- paste("https://data.calgary.ca/resource/35ra-9556.json?$limit=25000")

# Using the jsonlite package, we can access our selected web API and parse the response.
Traffic.Incidents <- fromJSON(txt = url)

# The data from Open Calgary is stored as a dataframe
View(Traffic.Incidents)

# Subset the dataframe to include only the columns that will be relevant to me.
Traffic.Incidents <- Traffic.Incidents[,c("incident_info", "description", "start_dt","longitude","latitude","count","quadrant")]

# Results
View(Traffic.Incidents)


# In addition, I want to separate the date and time data from the start_dt column so they're in their own unique column.
#   Personally, I think separating the dates and time values is more useful if the data was transferred to PowerBI as there will be less unique values.
#   Therefore, I need to take the specific portion of each cell in the start_dt column and transfer that to a new vector.
#   Reference - https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/as.POSIX*
Incident.Time <- format(as.POSIXct(strptime(Traffic.Incidents$start_dt, "%Y-%m-%dT%H:%M:%S",tz="")), format = "%H:%M")
Incident.Dates <- format(as.POSIXct(strptime(Traffic.Incidents$start_dt, "%Y-%m-%dT%H:%M:%S",tz="")), format = "%Y/%m/%d")

# In addition, I want to extract the Years to a separate columns in order to filter the data easier
Year <- format(as.POSIXct(strptime(Traffic.Incidents$start_dt, "%Y-%m-%dT%H:%M:%S",tz="")), format = "%Y")

# Using the cbind function, add the new data to the dataframe.
Traffic.Incidents <- cbind(Traffic.Incidents, Incident.Dates, Incident.Time, Year)

# Now that we have the information we want from the start_dt column, we can drop it from the dataframe. 
Traffic.Incidents <- Traffic.Incidents[,-3]

# I want to know what season the incident occurred so I want to use the Dates to automatically produce a specific season.
#   In order to do this, I need to change the class of the Date_column to the date class using the dplyr package
Traffic.Incidents<-mutate(Traffic.Incidents, Incident.Dates = as.Date(Incident.Dates, format = "%Y/%m/%d"))

# I'm also curious of the number of incidents occurring during each season... 
#   Questions like: Are there more reported accidents during winter season vs summer season;
#   I used the resource below to distinguish the season based on the date column.
#   Reference - https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
getSeason <- function(DATES) {
  WS <- as.Date("2012/12/15", format = "%Y/%m/%d") # Winter Solstice
  SE <- as.Date("2012/3/15",  format = "%Y/%m/%d") # Spring Equinox
  SS <- as.Date("2012/6/15",  format = "%Y/%m/%d") # Summer Solstice
  FE <- as.Date("2012/9/15",  format = "%Y/%m/%d") # Fall Equinox
  
  # Convert input information to date class 
  d <- as.Date(strftime(DATES, format="2012/%m/%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

Seasons <- getSeason(Traffic.Incidents$Incident.Dates)

# Bind the new data to the Traffic.Incident dataframe.
Traffic.Incidents <- cbind(Traffic.Incidents,Seasons)

# Now that we got all the data that we want in our dataframe, we can save this dataframe as a csv file if we were to use other visualization tools
#   such as PowerBI
write.csv(Traffic.Incidents, "Traffic.Incidents_EM.csv")

# The newly produced csv file has been included with the following package.
#----------------------------------------------------------------------------------------------------------------------------------------------
#   Data Visualization 
# I will now perform the rest of the data visualization part on the provided Power BI file. 
#----------------------------------------------------------------------------------------------------------------------------------------------

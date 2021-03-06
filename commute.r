# Loads the raw TFL data downloaded from the website and
# performs some basic transformation
library(dplyr)
library(tidyr)
library(ggplot2)

convert_raw_tfl_data <- function(tfl_data){
  # Dropping non-tube journey data.
  # As far as I can tell, this includes auto-topup and bus rides
  tfl_data <- filter(tfl_data, End.Time != "")
  # Dropping silly entries. E.g. when I went into a station just to find out the
  # Tube wasn't working and I had to go out to take a bus
  tfl_data <- filter(tfl_data, !grepl("Entered and exited",Journey.Action))
  tfl_data$Date <- as.Date(tfl_data$Date, format = "%d-%b-%Y")
  tfl_data$Weekday <- factor(weekdays(tfl_data$Date), levels =
                               c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  # Converts strings like 08:30 to the corresponding minutes from midnight
  as.MinutesSinceMidnight <- function(hour){
    tokens <- strsplit(hour, ':')[[1]]
    values <- as.integer(tokens)
    return(as.integer(values[1]*60+values[2]))		
  }
  tfl_data$Start.Time <- sapply(tfl_data$Start.Time, as.MinutesSinceMidnight)
  tfl_data$End.Time <- sapply(tfl_data$End.Time, as.MinutesSinceMidnight)
  tfl_data$Duration <- (tfl_data$End.Time - tfl_data$Start.Time) %% (60*24) # Rides might cross day boundary
  tfl_data <- tfl_data %>% separate(Journey.Action, c("From", "To"), " to ")
  tfl_data$From <- factor(tfl_data$From)
  tfl_data$To <- factor(tfl_data$To)	
  return(subset(tfl_data, select= c(Date, Weekday, Start.Time, End.Time, Duration, From, To)))
}

# Loads the content of a single csv file provided by TFL
load_data <- function(file){
	tfl_data <- read.csv(file, stringsAsFactors=FALSE)
	return(convert_raw_tfl_data(tfl_data))
}

# Loads all the files present in a given folder.
# It returns a single dataframe containing all loaded data.
# Note that I have non-overlapping dataframes so here I am assuming
# there are no duplicates and blindly merge all the data.
load_folder <- function(folder){
  files <- dir(folder, full.names = TRUE, pattern = "*.csv")
  raw_data <- lapply(files, function(file) read.csv(file, stringsAsFactors=FALSE))
  return(convert_raw_tfl_data(bind_rows(raw_data)))
}

# Extracts a commute leg from the data collected from the TFL website.
# from and to are the two stations for the leg
# start time is the hour from which rides are considered to be part of a commute.
# if you want to consider rides that started from 8am to 9am you should specify
# start.time = 8 and interval = 60
extract_commute_leg <- function(tfl_data, from, to, start.time, interval = 60){
	interval.start <- start.time * 60
	interval.end <- start.time * 60 + interval
	tfl_data <- subset(tfl_data,
					(From == from) &					
					(To == to) &
					!(Weekday %in% c('Saturday', 'Sunday'))&
					(Start.Time > interval.start) &
					(Start.Time < interval.end) )
	return(tfl_data)
}

merge_labeled <- function(commute.to, commute.from){
	commute.to$Direction <- factor("To")
	commute.from$Direction <- factor("From")
	return(rbind(commute.to, commute.from))
}

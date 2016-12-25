# Loads the raw TFL data downloaded from the website and
# performs some basic transformation
library(dplyr)
library(tidyr)

load_data <- function(file){
	# TODO: probably better to make this load all the files in a folder
	# it should merge and dedupe them and return a single dataframe
	tfl_data <- read.csv(file, stringsAsFactors=FALSE)
	# Dropping non-tube journey data.
	# As far as I can tell, this includes auto-topup and bus rides
	tfl_data <- subset(tfl_data, End.Time != "")
	tfl_data$Date <- as.Date(tfl_data$Date, format = "%d-%b-%Y")
	tfl_data$Weekday <- factor(weekdays(tfl_data$Date))
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

hist_data <- function(commute.to, commute.from){
	commute.to$Label <- factor("To")
	commute.from$Label <- factor("From")
	return(rbind(commute.to, commute.from))
}

hist_plot <- function(hist_data){
	hist <- ggplot(hist.data, aes(x=Duration, fill=Label)) + geom_histogram(binwidth=1, position="dodge")
	return(hist)
}
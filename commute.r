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
	# Converts strings like 08:30 to the corresponding minutes from midnight
	as.MinutesSinceMidnight <- function(hour){
		tokens <- strsplit(hour, ':')[[1]]
		values <- as.integer(tokens)
		return(as.integer(values[1]*60+values[2]))		
	}
	tfl_data$Start.Time <- sapply(tfl_data$Start.Time, as.MinutesSinceMidnight)
	tfl_data$End.Time <- sapply(tfl_data$End.Time, as.MinutesSinceMidnight)
	tfl_data <- tfl_data %>% separate(Journey.Action, c("From", "To"), " to ")
	tfl_data$From <- factor(tfl_data$From)
	tfl_data$To <- factor(tfl_data$To)	
	return(subset(tfl_data, select= c(Date, Start.Time, End.Time, From, To)))
}
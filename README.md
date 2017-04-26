# Tube commute
Data analysis of the London Underground journey history based on the Oyster card data from the [Transport For London website](https://oyster.tfl.gov.uk/oyster/journeyHistory.do).
You can subscribe and get files containing all the trip details periodically emailed to you.

The goal of this project is to provide an easy way to explore TFL data. Once you loaded the script in R you can play with it interactively. An example session could look like:
```
# Preparation of the data
tfl_data <- load_folder('~/Data/TFL/')
commute.to <- extract_commute_leg(tfl_data, "Tottenham Hale [London Underground]", "Warren Street", 7, 150)
commute.from <- extract_commute_leg(tfl_data, "Warren Street", "Tottenham Hale [London Underground]", 17, 150)
commute.labeled <- merge_labeled(commute.to, commute.from)
#Visualizations
start_time_from_vs_to(commute.labeled)
visualisation_from_vs_to(commute.labeled)
visualisation_from_vs_to_by_day(commute.labeled)
visualise_time_vs_duration(commute.labeled)
```
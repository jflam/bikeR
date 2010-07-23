# Library of R functions for working with Garmin Edge data
# By John Lam


feet_per_meter = 3.2808399

convert_all = function() {
    setwd("c:/fit/fit")
    files = dir(pattern = "*.fit")
    cmd = "c:/fit/biker/FitCSVTool.exe -b %s ../csv/%s.csv"

    exec = function(path) {
        system(sprintf(cmd, path, strsplit(path, "\\.fit")))
    }
 
    lapply(files, exec)
    return(TRUE)
}

load_all = function() {
    setwd("c:/fit/csv")
    files = dir(pattern = "*.csv")
    dates = as.Date(as.character(strsplit(files, "\\.csv")))
    read_file = function(path) {
        data = read.csv(path)
        subset(data, !is.na(data$Speed..m.s.))
    }
    data = lapply(files, read_file)
    list(date = dates, data = data)
}

loadx = function(x, path) {
    load(path)
    return(x)
}

# Compute normalized power for the data set
# Note that it is possible that the data set does not contain
# power data, in which case the function returns NA.

normalized_power = function(d) {
    power = d$Power..watts.
    if (is.null(power))
        return (NA)

    power = power[power > 0]
    return (mean(power ** 4) ** 0.25)
}

# Variability index indicates much variability there was
# in your ride over its duration

variability_index = function(d) {
    power = d$Power..watts.
    if (is.null(power))
        return (NA)

    np = normalized_power(d)
    return (np / mean(power))
}

elevation = function(d) {
    a = d$Altitude..m.
    a = a[!is.na(a)]
    d = diff(a)
    ascending = sum(d[d > 0])
    descending = -sum(d[d < 0])

    list(ascending_meters = ascending,
         descending_meters = descending,
         ascending_feet = ascending * feet_per_meter,
         descending_feet = descending * feet_per_meter)
}

total = function(e) {
    total_ascending = sum(as.double(lapply(e, function(f) { f$ascending_meters } )))
    total_descending = sum(as.double(lapply(e, function(f) { f$descending_meters } )))

    list(total_ascending_meters = total_ascending,
         total_descending_meters = total_descending,
         total_ascending_feet = total_ascending * feet_per_meter,
         total_descending_feet = total_descending * feet_per_meter)
}

daily_total = function(d, date) {
    rides_on_date = d$data[which(d$date == date)]
    e = lapply(rides_on_date, elevation)
    total(e)
}

daily_elevation_report = function(d) {
    dates = unique(d$date)
    daily_total = function(date) {
        rides_on_date = d$data[which(d$date == date)]
        ride_totals = lapply(rides_on_date, elevation)
        c(total(ride_totals), list(rides = length(rides_on_date)))
    }

    daily_totals = lapply(dates, daily_total)
    daily_ascending = as.double(lapply(daily_totals, function(f) { f$total_ascending_meters } ))
    daily_descending = as.double(lapply(daily_totals, function(f) { f$total_descending_meters } ))
    rides = as.double(lapply(daily_totals, function(f) { f$rides } ))

    data.frame(date = dates,
               rides = rides,
               daily_ascending_meters = daily_ascending,
               daily_descending_meters = daily_descending,
               daily_ascending_feet = daily_ascending * feet_per_meter,
               daily_descending_feet = daily_descending * feet_per_meter)
}

elevation_totals = function(d) {
    e = lapply(d$data, elevation)
    as.data.frame(c(rides = length(e), total(e)))
}

get_data = function(date) {
    do.call("rbind", d$data[which(d$date == as.Date(date))])
}


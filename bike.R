# Library of R functions for working with Garmin Edge data
# By John Lam

feet_per_meter = 3.2808399

# Rider constants

ftp = 205  # functional threshold power in watts
quadrant_cadence = 80 # magic 80 rpm cadence value for quadrant analysis
crank_length = 0.170  # 170mm cranks in meters


# Library paths

fit_path = "c:/fit/fit"
csv_path = "c:/fit/csv"
lib_path = "c:/fit/bikeR"

get_filenames = function(paths) {
    filenames = strsplit(paths, "\\.")
    return (sapply(filenames, function(x) { x[1] }))
}

get_path_differences = function() {
    fit_files = get_filenames(dir(fit_path, pattern = "*.fit"))
    csv_files = get_filenames(dir(csv_path, pattern = "*.csv"))
    return (setdiff(fit_files, csv_files))
}

convert_fit_to_csv = function() {
    files = get_path_differences()
    cmd = "c:/fit/biker/FitCSVTool.exe -b %s/%s.fit %s/%s.csv"
    sapply(files, function(p) { system(sprintf(cmd, fit_path, p, csv_path, p)) }) 
    return (TRUE)
}

load_all = function() {
    files = dir(csv_path, pattern = "*.csv")
    dates = as.Date(get_filenames(files))
    read_file = function(path) {
        full_path = sprintf("%s/%s", csv_path, path)
        data = read.csv(full_path)
        data$time_offset = as.integer(data$Timestamp..s.)
        subset(data, !is.na(data$Speed..m.s.))
    }
    data = lapply(files, read_file)
    list(date = dates, data = data)
}

time_slice = function(d, start, duration) {
    # TODO: convert time from hh::mm::ss into offset
    subset(d, d$time_offset > start & d$time_offset < start + duration)   
}

loadx = function(x, path) {
    load(path)
    return(x)
}

# Remove all NA's and zero power values

clean_power = function(d) {
    power = d$Power..watts.
    if (is.null(power))
        return (NA)

    return (power[power > 0 & !is.na(power)])
}

# Compute normalized power for the data set
# Note that it is possible that the data set does not contain
# power data, in which case the function returns NA.

normalized_power = function(power) {
    return (mean(power ** 4) ** 0.25)
}

calc_ftp = function(d) {
    power = clean_power(d)
    p1 = filter(power**4,rep(1/1200,1200))
    p2 = p1[!is.na(p1)]
    p3 = p2 ** 0.25
    ftp = max(p3) * 0.95
    return (ftp)
}

calc_avg20 = function(d) {
    power = clean_power(d)
    p1 = filter(power,rep(1/1200,1200))
    p2 = p1[!is.na(p1)]
    mean(p2)
}

calc_ftp60 = function(d) {
    power = clean_power(d)
    p1 = filter(power**4,rep(1/3600,3600))
    p2 = p1[!is.na(p1)]
    p3 = p2 ** 0.25
    return (max(p3))
}

# Variability index indicates much variability there was
# in your ride over its duration

variability_index = function(power) {
    np = normalized_power(power)
    return (np / mean(power))
}

# Intensity factor is the ratio of the normalized power for
# the ride divided by the rider's functional threshold power

intensity_factor = function(power) {
    np = normalized_power(power)
    npif = np / ftp
    return (npif)
}

# Training stress score indicates a rough duration of 
# post-ride fatigue.
# <150    Low        Recovery complete by next day
# 150-300 Moderate   Some residual fatigue next day
# 300-450 High       Some residual fatigue even after 2 days
# >450    Very high  Residual fatigue lasting several days likely

training_stress_score = function(power, duration) {
    np = normalized_power(power)
    iff = intensity_factor(power)
    return ((duration * np * iff / (ftp * 3600)) * 100)
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

calc_quadrant = function(d) {
    # remove zero power from d and remove zero cadence
    e = subset(d, d$Cadence..rpm. > 0 & d$Power..watts. > 0)
    cadence = e$Cadence..rpm.
    power = e$Power..watts.

    return (list(aepf = (power * 60) / (cadence * 2 * pi * crank_length),
                 cpv = cadence * crank_length * 2 * pi / 60))
}

plot_quadrant = function(d, title=NA) {
    r = calc_quadrant(d)
    t = ifelse(is.na(title), "Quadrant Analysis", sprintf("Quadrant Analysis for %s", title))
    plot(r$cpv, r$aepf, main=t, cex.lab = 0.8, 
         xlab="Circumferential Pedal Velocity (CPV), (m/s)", 
         ylab="Average Effective Pedal Force (AEPF), (N)")
    aepf = (ftp * 60) / (quadrant_cadence * 2 * pi * crank_length)
    cpv = quadrant_cadence * crank_length * 2 * pi / 60

    abline(h = aepf, lty="dotted")
    abline(v = cpv, lty="dotted")

    left = which(r$cpv < cpv)
    right = which(r$cpv >= cpv)
    
    aepf_left = r$aepf[left]
    aepf_right = r$aepf[right]

    q3 = length(aepf_left[aepf_left < aepf])
    q1 = length(aepf_left[aepf_left >= aepf])
    q4 = length(aepf_right[aepf_right < aepf])
    q2 = length(aepf_right[aepf_right >= aepf])

    total = length(r$cpv)

    text(max(r$cpv) - 0.1, aepf + 20, sprintf("FTP: %dW", ftp), font=4, cex=0.8)
    text(cpv + 0.25, max(r$aepf), sprintf("Cadence: %d", quadrant_cadence), font=4, cex=0.8)
    text(min(r$cpv) + 0.12, max(r$aepf), sprintf("Q1: %2.1f%%", q1/total*100), font=4, cex=0.8)
    text(max(r$cpv) - 0.1, max(r$aepf), sprintf("Q2: %2.1f%%", q2/total*100), font=4, cex=0.8)
    text(min(r$cpv) + 0.12, min(r$aepf), sprintf("Q3: %2.1f%%", q3/total*100), font=4, cex=0.8)
    text(max(r$cpv) - 0.1, min(r$aepf), sprintf("Q4: %2.1f%%", q4/total*100), font=4, cex=0.8)
}

plot_cadence = function(d) {
    cadence = d$Cadence..rpm.
    c = cadence[cadence < 150]
    hist(c, max(c) / 5, main="Cadence Histogram",
         xlab="Cadence (rpm)")
}

plot_heartrate = function(d) {
    h = d$Heart.Rate..bpm.
    hist(h, (max(h) - min(h)) / 5, main="Heart Rate Histogram",
         xlab="Heart Rate (bpm)")
}

elevation_totals = function(d) {
    e = lapply(d$data, elevation)
    as.data.frame(c(rides = length(e), total(e)))
}

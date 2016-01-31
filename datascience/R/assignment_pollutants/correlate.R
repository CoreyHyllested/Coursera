
get_stations_use <- function(directory, ids, stations.all) {
	# return all files ###.csv in directory that are in list(ids)
	names <- sprintf('%03d.csv', ids)
	files <- file.path(directory, names)
	match <- (files %in% stations.all)
 
	stations.match <- files[match]
	#print(paste('Return matching Station Files (', length(stations.match), '):'))
	#print(stations.match)
	stations.match
}


get_stations_all <- function(directory, ids) {
	#print(paste('Return All Station Data Files in ', directory))
	files = list.files(directory, pattern="*.csv", full.names = T)
	if (length(files) == 0) {
		msg <- paste('No data files found in ', directory)
		stop(msg)
	}
	#print(paste('Found ', length(files), ' files'))
	files
}


station_process <- function(filenames, pollutant) {
  #print(paste('Processing Stations for', pollutant))
  for (station.file in filenames) {
    station.data      <- read.csv(station.file)
    station.pollutant <- station.data[pollutant]
    station.pollutant <- station.pollutant[!is.na(station.pollutant)]
    rc = mean(station.pollutant)
  }
  rc
}


# create single data frame, takes threshold for complete observations
stations.merge <- function(filenames, threshold=0) {
  #print(paste('Merging', length(filenames), 'Stations'))
  stations.data = lapply(filenames, function(filp) {
      observations.station  = read.csv(file=filp, header=T)
      observations.testing  = observations.station[[1]]
      observations.complete = observations.testing[complete.cases(observations.station)]

#      print(paste('observations:', length(observations.station[[1]]), 'thresh:', threshold, 'comp:', length(observations.complete)))
      if (length(observations.complete) >= threshold) {
        # return all observations from this station.
        observations.station
      } else {
        df <- data.frame(Date=as.Date(character()),
                         sulfate=numeric(), 
                         nitrate=numeric(),
                         ID=numeric()
                         ) 
      }
    }
  )
  rc = do.call(rbind, stations.data)
  rc
}


station.data.process <- function(df, pollutant) {
	df.pollutant <- df[pollutant]
	df.pollutant <- df.pollutant[!is.na(df.pollutant)]
	rc = mean(df.pollutant)
}



pm <- function(dir='.', pollutant, id=1:332) {
	if (pollutant != 'sulfate' && pollutant != 'nitrate') {
		stop('Not searching for Sulfate or Nitrate')
	}

	#print( paste('Find pollutant:', pollutant, sep = " ") )
	fqdp              = normalizePath(dir)
	stations.all      = get_stations_all(fqdp)
	stations.use      = get_stations_use(fqdp, id, stations.all)
	data_frame        = stations.merge(stations.use)
	mean = station.data.process(data_frame, pollutant)
	#stations.all
	print (mean)
}



observations <- function(dir='.', id) {
	fqdp              = normalizePath(dir)
	stations.all      = get_stations_all(fqdp)
	stations.use      = get_stations_use(fqdp, id, stations.all)
	observations.all  = stations.merge(stations.use)
	observations.ids  = observations.all[[4]]
	observations.comp = observations.ids[complete.cases(observations.all)]

	df = as.data.frame(table(observations.comp))
	names(df) <- c('id', 'nobs')
	#print (df)
	df
}




# create single data frame, takes threshold for complete observations
stations.correlate <- function(filenames, threshold=0) {
  stations.correlation = lapply(filenames, function(filp) {
    observations.station  = read.csv(file=filp, header=T)
    observations.testing  = observations.station[[1]]
    observations.complete = observations.testing[complete.cases(observations.station)]
    
    if (length(observations.complete) >= threshold) {
      observations.sulfate = observations.station[['sulfate']]
      observations.nitrate = observations.station[['nitrate']]
      observations.sulfate = observations.sulfate[observations.complete]
      observations.nitrate = observations.nitrate[observations.complete]
      correlation = cor(observations.nitrate, observations.sulfate)
      #print(paste(filp, length(observations.station[['sulfate']]), length(observations.complete), correlation))
      correlation
    } else {
   #   print(paste(filp, length(observations.station[['sulfate']]), length(observations.complete), '-- NEXT '))
     # df = data.frame()
    } 
  })
  
  rc = do.call(rbind, stations.correlation)
}




correlate <- function(dir='.', threshold = 0) {
	fqdp              = normalizePath(dir)
	stations.all      = get_stations_all(fqdp)
	rc <- stations.correlate(stations.all, threshold)
}


#test_1 <- pm (dir='data', pollutant='sulfate', id=1:10)
#test_2 <- pm (dir='data', pollutant='nitrate', id=70:72)
#test_3 <- pm (dir='data', 'nitrate', 23)

#test_4 = observations(dir='data', 1)
#test_5 = observations(dir='data', c(2, 4, 8, 10, 12))
#test_6 = observations(dir='data', 30:25)
#test_7 = observations(dir='data', 3)
#test_8 = correlate(dir='data', 150)
#test_9 = correlate(dir='data', 5000)


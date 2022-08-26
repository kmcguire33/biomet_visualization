# ------------------------------------------------------------------
# AmeriFlux Sandboxing 3,000 site years
# Preparing data from raw AmeriFlux files

# August 26, 2022
# Sophie Ruehr
# sophie_ruehr@berkeley.edu
# ------------------------------------------------------------------

# In preparing data for the app, minimial processing and efficiency were the priorities. The goal was to create smaller output files that would lend themselves to a fast app with quick plotting times.

# 1. SET UP ----- 
library(pacman)
rm(list = ls())
p_load(dplyr, ggplot2, readxl, cowplot, png, tidyr, naniar, GGally)

# Set working directories for data (raw download from AmeriFlux server) and saving outputs
datawd <- '/Volumes/Torpedo/Data.nosync/ameriflux_2022_08_26_full_download'
savewd <- '/Users/sophieruehr/Documents/Academic/Berkeley/Projects/sandbox/August_attempt_6'

# Get names of flux sites
setwd(datawd)
files <- list.files()
sites <- substr(files, 5, 10) 

# 2. INDIVIDUAL SITE DAILY DATA, SUMMARIES, MISSING DATA ----- 
summary_stats_out <- list()
missing_out <- list()
setwd(datawd)
for (i in 1:length(files)) { # Loop through all flux sites 
  
  # Read in data 
  setwd(datawd)
  setwd(paste0('./', files[i]))
  file <- list.files(pattern = '.csv')[1]
  site <- substr(file, 5, 10)
  data <- read.table(file, header = TRUE, skip = 2,sep = ",",
                     fill = T)
  # Set missing data to NA 
  data[data == -9999] <- NA
  
  # Set date time
  data$TIMESTAMP_START <- as.POSIXct(as.character(data$TIMESTAMP_START),
                                     format="%Y%m%d%H%M")
  
  # 1. Get summary statistics
  summary_stats <- c()
  # Loop through each variable in dataset
  for (k in 3:dim(data)[2]) { # Ignore datetime variables 
    var <- as.numeric(data[,k])
    summary_k <- round(summary(var),3)  # Take summary 
    if (length(summary_k) < 7) { # Ensure there are no NA-only values
      summary_k <- c(summary_k, 0)
    }
    
    percent_NA <- round(summary_k[7] / length(data[,k]) * 100,2) # Get percent of NAs in variable
    summary_k <- c(colnames(data)[k], summary_k, percent_NA) # Create new object with variable name and %NA
    names(summary_k)[c(1,8,9)] <- c('Variable','NAs', '% NA') # Rename columns
    summary_stats <- rbind(summary_stats, summary_k) # Add to growing list of variables 
  }
  rownames(summary_stats) <- rep('', dim(summary_stats)[1]) # Get rid of row names 
  
  # Save summary statistics   
  summary_stats_out[[i]] <- summary_stats # Add site to list of summary statistics
  names(summary_stats_out)[[i]] <- site # Name list with site name
  

  # 2. Get averaged daily data 
  
  # Average by day (to diminish data size)
  data$date <- as.Date(data$TIMESTAMP_START) # Create date variable
  dat_daily <- data %>% group_by(date) %>% summarise_all(.funs = mean, na.rm = T) # Take average of each variable for each unique date
  dat_daily <- dat_daily %>% subset(select = -c(TIMESTAMP_START, TIMESTAMP_END)) # Get rid of timestamp variables
  setwd(savewd) 
  site <- sites[i]
  filename <- paste0(site, '.csv')
  write.csv(dat_daily, filename) # Write site level data file
  
  
  # 3. Get missing data by year
  
  # Get missing data 
  data <- data[,-c(1,2)] # Get rid of timestamp variables
  data$year <- as.numeric(format(data$date, '%Y')) # Create year variable
  data <- data %>% filter(!is.na(year)) %>% # Get number of NA by year and variable
    group_by(year) %>% summarise_all(.funs = function(x) {100 * sum(is.na(x))/length(x)})
  data <- as.data.frame(data)
  
  data <- data %>% subset(select = -date) %>% pivot_longer(!year, names_to = 'variable') # Create a longer dataframe
  data$value <- round(data$value,0) # Round missing data %
  
  # Save summary statistics
  missing_out[[i]] <- data # Add missing data information from site to list of all sites
  names(missing_out)[[i]] <- site # Name after site

  print(site)
}

setwd(savewd)
saveRDS(summary_stats_out, 'summary_stats.RDS') # Write summary data
saveRDS(missing_out, 'missing_data_by_year.RDS') # Write missing data

# 3. DOIS and site info -----
doiout <- c()
setwd(datawd)
for (i in 1:length(sites)) {
  # Read in DOI information
  site <- sites[i]
  setwd(datawd)
  wd_data <- list.files(pattern = site)
  setwd(wd_data)
  file <- list.files(pattern = '.xlsx')[1]
  dois <- read_xlsx(file)
  
  # Get DOI, URL, full site name, IGBP, lat, long, MAP, MAT and elevation for each site 
  doi <- c(site,
             ifelse(identical(which(dois$VARIABLE == 'DOI'),integer(0)), 'None found', dois$DATAVALUE[which(dois$VARIABLE == 'DOI')]),
             ifelse(identical(which(dois$VARIABLE == 'SITE_NAME'),integer(0)), 'None found', dois$DATAVALUE[which(dois$VARIABLE == 'SITE_NAME')]),
             ifelse(identical(which(dois$VARIABLE == 'URL_AMERIFLUX'),integer(0)), 'None found', dois$DATAVALUE[which(dois$VARIABLE == 'URL_AMERIFLUX')]),
             ifelse(identical(which(dois$VARIABLE == 'IGBP'),integer(0)), NA, dois$DATAVALUE[which(dois$VARIABLE == 'IGBP')]),
             ifelse(identical(which(dois$VARIABLE == 'LOCATION_LAT'),integer(0)), NA, dois$DATAVALUE[which(dois$VARIABLE == 'LOCATION_LAT')]),
             ifelse(identical(which(dois$VARIABLE == 'LOCATION_LONG'),integer(0)), NA, dois$DATAVALUE[which(dois$VARIABLE == 'LOCATION_LONG')]),
           ifelse(identical(which(dois$VARIABLE == 'MAT'),integer(0)), NA, dois$DATAVALUE[which(dois$VARIABLE == 'MAT')]),
           ifelse(identical(which(dois$VARIABLE == 'MAP'),integer(0)), NA, dois$DATAVALUE[which(dois$VARIABLE == 'MAP')]),
           ifelse(identical(which(dois$VARIABLE == 'LOCATION_ELEV'),integer(0)), NA, dois$DATAVALUE[which(dois$VARIABLE == 'LOCATION_ELEV')]))
           
  doiout <- rbind(doiout, doi) # Save information
  print(site)
  
}

doiout <- as.data.frame(doiout)
# Rename columns
colnames(doiout) <- c('site', 'DOI', 'sitename', 'url', 'IGBP', 'Latitude', 'Longitude',
                      'MAT', 'MAP', 'Elevation')
setwd(savewd)
# Save DOI information
write.csv(doiout, 'dois.csv')

# 4. CREATE SMALLER DATASET TO PLOT ALL SITES AT ONCE -----
setwd(savewd)
# Open DOI file
doiout <- read.csv('dois.csv')
# Get files for all sites
files <- list.files(pattern = '.csv')
files <- files[-c(which(files == 'dois.csv'), which(files == 'all_sites.csv'), 
                  which(files == 'units.csv'))]
set.seed(123) # Set seed for reproducible sampling

# Only a few variables
var_of_interest <- c('GPP', 'NEE', 'RECO','FC', # Ecosystem productivity and C fluxes
                     'VPD', 'RH',  'TA', # Meteorological variables
                     'USTAR', # Atmospheric stability / roughness length approximation (proxy for 'good' EC conditions)
                     'P','SWC', # Water availability 
                     'LE','H', 'NETRAD', 'G', # Surface energy balance
                     'date'
)
datout <- data.frame()

# Loop through all sites and sample smaller portion of data to create small file with all sites represented

for (i in 1:length(files)) {
  # Open daily averaged data at site
  file <- files[i]
  
  cols <- colnames(read.csv(file, nrow = 1)) # Get column names
  
  # Get index of each column of variable of interest
  vars <- sapply(strsplit(cols,"_"),'[',1) 
  indexvar <- c()
  for (j in 1:length(var_of_interest)) {
    indexvar[j] <- which(vars %in% var_of_interest[j])[1] # If multiple sensors/positions, take first appearing in dataset
  }
  indexvar <- na.omit(indexvar)
  
  vec <- c(rep('NULL', length(cols))) 
  vec[c(indexvar)] <- 'numeric'  # Only read in columns of interest
  vec[2] <- 'character' # Date index
  
  data <- read.table(file, sep = ',', header = TRUE, 
                     colClasses = vec, fill = T)
  
  names <- sapply(strsplit(colnames(data),"_"),'[',1)[-1] # Rename columns
  colnames(data) <- c('Date', names)
  
  # Take a 2% random sample of data from the site
  samp <- sample(dim(data)[1], size = round(dim(data)[1] * 0.02, 0), replace = FALSE) 
  data <- data[samp, ] 
  
  # Add 'site' variable to dataframe
  data$site <- substr(file, 1, 6)
  
  # Input site information to dataframe (IGBP, lat, lon, elevation, MAP, MAT and Koppen)
  doi_info <- doiout[which(doiout$site == unique(data$site)),]
  data$IGBP <- doi_info$IGBP
  data$Latitude <- doi_info$Latitude
  data$Longitude <- doi_info$Longitude
  data$Elevation <- doi_info$Elevation
  data$MAT <- doi_info$MAT
  data$MAP <- doi_info$MAP
  data$Koppen <- doi_info$Koppen
  
  # Merge with other sites
  datout <- merge(datout, data, all = T)
  print(unique(data$site))
}

datout <- as.data.frame(datout)
setwd(savewd)
# Save all sites file  
write.csv(datout, 'all_sites.csv')

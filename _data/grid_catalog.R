
## setup
require(dplyr)
require(stringr)
require(lubridate)

## find a record of ARs by grid cell
datelist <- seq(ymd('1980-01-01'), ymd('2019-12-31'), 'days')
hourlist <- rep(datelist, each = 8)
LON <- seq(-110, -125, -0.625)
LAT <- seq(32.5, 42, 0.5)

# require(RCurl)
# temp <- getURL('ftp://sioftp.ucsd.edu/CW3E_DataShare/Rutz_AR_Catalog/Rutz_ARCatalog_MERRA2_1980.nc', dirlistonly = TRUE)
# temp %>% str_split('\r\n')
# 
# start <- Sys.time()
# download.file('ftp://sioftp.ucsd.edu/CW3E_DataShare/Rutz_AR_Catalog/Rutz_ARCatalog_MERRA2_1981.nc',
#               destfile = 'C:/Users/cbowers/Desktop/rutz1981.nc', mode = 'wb')
# Sys.time() - start
# 
# start <- Sys.time()
# download.file('ftp://sioftp.ucsd.edu/CW3E_DataShare/Rutz_AR_Catalog/ReadMe',
#               destfile = 'C:/Users/cbowers/Desktop/readme.txt', mode = 'wb')
# Sys.time() - start


download.file('ftp://cirrus.ucsd.edu/pub/ncview/ncview-2.1.7.tar.gz',
              destfile = 'C:/Users/cbowers/Desktop/ncview-2.1.7.tar.gz', mode = 'wb')
untar('C:/Users/cbowers/Desktop/ncview-2.1.7.tar.gz', exdir = 'C:/Users/cbowers/Desktop')


require(ncdf4)
require(raster)
temp <- ncdump::NetCDF('C:/Users/cbowers/Desktop/rutz1981.nc')
temp <- nc_open('C:/Users/cbowers/Desktop/rutz1981.nc', verbose = TRUE, write = FALSE)
# this crashes R every time

# require(httr)
# url <- 'ftp://sioftp.ucsd.edu/CW3E_DataShare/Rutz_AR_Catalog/Rutz_ARCatalog_MERRA2_1981.nc'
# res <- GET(url, write_disk(basename(url)))
# 
# start <- Sys.time()
# temp <- content(GET(url))
# Sys.time() - start
# 
# require(RCurl)
# temp1 <- getURL(url, ftp.use.epsv = FALSE)
# temp2 <- getURL(url, ftp.use.epsv = TRUE)

ar_grid <- list()
step <- 1
threshold <- 20 #lower bound for number of ARs
tracker <- data.frame(lat = NA, lon = NA, step = NA, AR = NA)
timer <- Sys.time()
pb <- txtProgressBar(min = 0, max = length(LON)*length(LAT), style = 3)
setTxtProgressBar(pb, 0)
for (lon in 1:length(LON)) {
  for (lat in 1:length(LAT)) {
    ## read data
    filename <- paste('http://www.inscc.utah.edu/~rutz/ar_catalogs/merra_0.5/timeseries/ivt_ar_',
                      format(round(LON[lon], 3), nsmall = 3), 'E_',
                      format(round(LAT[lat], 3), nsmall = 3), 'N.txt', sep = '')
    file <- read.table(filename, skip = 1, header = FALSE)
    names(file) <- c('YEAR', 'MONTH', 'DAY', 'HOUR', 'IVT', 'AR', 'AR_new',
                     'AR_duration', 'IVT_total')

    if (sum(file$AR_new) > threshold) {
      ## get a list of all ARs within the given grid cell
      ar_list <- data.frame(start_date = NA, start_hour = NA, end_date = NA,
                            end_hour = NA, IVT_max = NA, IVT_total = NA)
      ar_index <- which(file$AR_new == 1)
      for (id in 1:length(ar_index)) {
        id_start <- ar_index[id]
        id_end <- which(file$IVT_total[id_start:nrow(file)] == 0)[1] + id_start - 2
        ar_list[id, 'start_date'] <- paste(file$YEAR[id_start],
                                           file$MONTH[id_start],
                                           file$DAY[id_start], sep = '-')
        ar_list[id, 'end_date'] <- paste(file$YEAR[id_end], file$MONTH[id_end],
                                         file$DAY[id_end], sep = '-')
        ar_list[id, 'start_hour'] <- file$HOUR[id_start]
        ar_list[id, 'end_hour'] <- file$HOUR[id_end]
        ar_list[id, 'IVT_max'] <- max(file$IVT[id_start:id_end])
        ar_list[id, 'IVT_total'] <- file$IVT_total[id_end]
      }
      ## add duration column (hours)
      ar_list$duration <- (ymd_h(paste(ar_list$end_date, ar_list$end_hour)) -
        ymd_h(paste(ar_list$start_date, ar_list$start_hour))) %>% as.numeric(units = 'hours')

      ## get rid of weak, short-duration ARs
      ar_list <- ar_list[ar_list$IVT_total >= 1.08,]

      ## get rid of grid cells with few ARs
      if(nrow(ar_list) >= threshold) {
        ar_grid[[step]] <- ar_list
      } else {
        ar_grid[[step]] <- NA
      }
    } else {
      ar_grid[[step]] <- NA
    }

    ## update tracker
    tracker[step,] <- c(LAT[lat], LON[lon], step, nrow(ar_list))
    step <- step + 1
    setTxtProgressBar(pb, step)
  }
}
Sys.time() - timer
close(pb)

save(ar_grid, ar_list, tracker, file = './_data/grid_catalog.Rdata')

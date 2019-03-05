library(readr)
library(dplyr)
library(stringr)
library(geojsonio)
library(sp)

path <- "/home/wesley/Dropbox/IC Cepagri/GLM - Global Lightning Mapper/data"
setwd(path)
files <- list.files()
files <- head(files, n=10000)

data <- tibble(lat=as.numeric(), lon=as.numeric(), value=as.numeric(), filename=as.character())

for (i in 1:length(files)){
  newData <- read_delim(files[i], delim=',', col_names=c("lat", "lon", "value"), col_types = c("ccc")) %>% 
    mutate(filename=files[i], state=NA, lat=as.numeric(lat), lon=as.numeric(lon))

  data <- rbind(data, newData)
}

data <- data %>% 
  mutate(date=str_c(substr(filename, 1, 4), substr(filename, 5, 6), substr(filename, 7, 8), sep="-"),
         time=str_c(substr(filename, 9, 10), substr(filename, 11, 12), substr(filename, 13, 14), sep=":"),
         state=NA)

# remove coordinates that don't belong to Brazil
# data <- data %>%
#   select(date, time, state, lat, lon, value) %>%
#   filter(lat < 6 &  lat > -34 & lon > -74.5 & lon < -33)

# start_time <- Sys.time()
# estados <- geojson_read("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/brazil-states.geojson",
#                         what = "sp")
# end_time <- Sys.time()
# time1 <- end_time - start_time
# start_time <- Sys.time()
estados <- geojson_read("../geojsonBrasil/brazil-states.geojson",
                        what = "sp")
# end_time <- Sys.time()
# time2 <- end_time - start_time
# time1
# time2

# compare points
for (i in 1:nrow(data)) {
  coords <- c(data$lon[i], data$lat[i])
  if(any(is.na(coords))) next
  point <- sp::SpatialPoints(matrix(coords, nrow = 1))
  sp::proj4string(point) <- sp::proj4string(estados)
  polygon_check <- sp::over(point, estados)
  data$state[i] <- as.character(polygon_check$sigla)
}

dataBrasil <- data %>% 
  select(-filename) %>% 
  filter(!is.na(data$state))

numberIncidences <- dataBrasil %>%
  select(state) %>%
  group_by(state) %>% 
  summarise(n=n())

# save by day...
# check if there is a .csv file with that day
# if not, create it
# else, add to it
# write_csv(dataBrasil, "../glmBrasil.csv")

library(rgdal)

states <- readOGR("states.json", "OGRGeoJSON")

dat <- structure(list(STATE_CODE = structure(1:8, .Label = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"), 
                                             class = "factor"), stateincome = c(28959299.93, 392185791.54, 8559477.92, 169039212.3, 61092752.9878, 23695740.28, 298860548.9008, 114231960.463),
                      avgstategift = c(48.8204253852119, 64.8584888549168, 45.0472757892964, 53.9720822484215, 50.7162153310643, 53.0923216942408, 62.4033469022953, 50.7348320924839)), 
                 row.names = c(NA, -8L), 
                 class = c("data.table", "data.frame"), 
                 .Names = c("STATE_CODE", "stateincome", "avgstategift"))

states@data <- merge(states@data, dat)
str(states@data)

## 'data.frame': 8 obs. of  6 variables:
##  $ STATE_CODE  : Factor w/ 9 levels "ACT","NSW","NT",..: 1 2 3 5 6 7 8 9
##  $ STE_CODE11  : Factor w/ 9 levels "1","2","3","4",..: 8 1 7 3 4 6 2 5
##  $ STE_NAME11  : Factor w/ 9 levels "Australian Capital Territory",..: 1 2 3 5 6 7 8 9
##  $ ALBERS_SQM  : num  2.36e+09 8.01e+11 1.35e+12 1.73e+12 9.84e+11 ...
##  $ stateincome : num  2.90e+07 3.92e+08 8.56e+06 1.69e+08 6.11e+07 ...
##  $ avgstategift: num  48.8 64.9 45 54 50.7 ...

##################################################################################################
### Script to analyze Ford Go Bike data from launch (6/28/17) to 1/1/2018
### Author: Shimon Israel, February 2018
##################################################################################################

suppressMessages(library(dplyr))
library(sf)

DATA_LOCATION <- "M:/Data/BikeShare"
INPUT <- paste(DATA_LOCATION,"Ford_GoBike_tripdata_20170628_20180101_with_bikeshareforall.csv", sep="/")
data <- read.csv(INPUT)%>% mutate(
  start_lat=as.numeric(sapply(strsplit(as.character(start_location),','),function(x) x[1])),
  start_lon=as.numeric(sapply(strsplit(as.character(start_location),','),function(x) x[2])),
  end_lat=as.numeric(sapply(strsplit(as.character(end_location),','),function(x) x[1])),
  end_lon=as.numeric(sapply(strsplit(as.character(end_location),','),function(x) x[2])),
  trips=1)

data_head <- head(data)


# Create station ID table

stations <- data %>%
  group_by(start_station_id,start_station_name,start_location) %>%
  summarize(total=n()) %>% 
  ungroup()%>% mutate(
    lat=as.numeric(sapply(strsplit(as.character(start_location),','),function(x) x[1])),
    lon=as.numeric(sapply(strsplit(as.character(start_location),','),function(x) x[2])),
    station_id=start_station_id,
    station_name=start_station_name) %>%
  select(station_name,station_id,lat,lon)

# Distances

# Calculate approximate distance in miles between two points
earth.dist <- function (long1, lat1, long2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  d <- d*0.621371
  return(d)
}


distance <- data %>% mutate(
  distance=earth.dist(start_lon,start_lat,end_lon,end_lat))

# Me

me <- distance %>%
  filter(start_station_name=="Emeryville Town Hall" & member_birth_year==1970)

# Trips by gender and distance

gender <- distance %>% group_by(member_gender) %>%
  summarize(distance=mean(distance))

# Mean trip time by user type

mean_time <- data %>% group_by(user_type) %>%
  summarize(num_trips=n(),mean_minutes=mean(duration_sec*trips)/60)


# Number trips by trip origins and destinations

trip_origins <- data %>% group_by(start_station_name, start_lat, start_lon) %>%
  summarize(trip_origins=sum(trips)) %>%
  arrange(desc(trip_origins))
names(trip_origins)[1] <- "station_name"
trip_origins <- ungroup(trip_origins)
  

trip_ends <- data %>% group_by(end_station_name) %>%
  summarize(trip_ends=sum(trips)) %>%
  arrange(desc(trip_ends))
names(trip_ends)[1] <- "station_name"

trip_locations <- left_join(trip_origins,trip_ends, by="station_name") %>%
  rename(lat=start_lat, lon=start_lon)

# Group by origin and destination stations

origin_destination <- data %>%
  group_by(start_station_name,end_station_name) %>%
  summarize(total_trips = sum(trips)) %>%
  arrange(desc(total_trips))

# Output summary

write.csv(trip_locations,paste(DATA_LOCATION,"Trip_Locations.csv",sep="/"))



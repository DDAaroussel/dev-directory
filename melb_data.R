
# Exploring Melbourne based data ------------------------------------------


# Load packages -----------------------------------------------------------

library(readr)
library(dplyr)
library(stringr)
library(ggmap)
library(lubridate)

# Read in data ------------------------------------------------------------

public_bbq_v1 <- read_csv("Public_barbecues.csv")
bike_share <- read_csv(
  "Melbourne_Bike_Share_stations__with_current_number_of_free_and_used_docks__every_15_minutes_.csv"
  )
cafes <- read_csv(
  "Cafes_and_restaurants__with_seating_capacity.csv"
)
drinking_fountains <- read_csv(
  "Drinking_fountains.csv"
)
pedestrian <- read_csv(
  "Pedestrian_volume__updated_monthly_.csv"
)
pedestrian_loc <- read_csv("Pedestrian_sensor_locations.csv")

# Melb BBQ data -----------------------------------------------------------

#Splitting out the bbq description to see if it tells us anything about the types of bbq in Melb
split_string <- str_split_fixed(str_trim(public_bbq_v1$Description), " - ", n = 2)
x <- c("is_bbq", "details")
colnames(split_string) <- x 
public_bbq_v1$bbq_type <- split_string[,2]

#Splitting out the coordinates so they are readable as seperate lat & long fields

y <- c("desc", "coords", "bbq_type")
colnames(public_bbq_v1) <- y

split_coords <- str_split(str_trim(public_bbq_v1$coords), ",", simplify = TRUE)
split_coords

split_coords[,1] <- str_replace_all(split_coords[,1], c("\\("), "")
split_coords[,2] <- str_replace_all(split_coords[,2], c("\\)"), "")
split_coords

public_bbq_v2 <- public_bbq_v1 %>%
  mutate(
    lat = as.numeric(split_coords[,1]),
    long = as.numeric(split_coords[,2])
  )

#BBQ type and lat/long coordinates have successfully been added to the data


#Describe the data

table(public_bbq_v2$bbq_type)

#17 single and 14 double hotplate BBQs in the city. Where are they?

summarised <- public_bbq_v2 %>%
  group_by(bbq_type, lat, long) %>%
  summarise(
    total = n(),
    num_bbqs = sum(is.na(bbq_type))
  )
summarised

#If we want to filter for  the most common BBQs 
summarised_v2 <- filter(summarised, 
                        bbq_type == "Urban Design Double Hotplate" | bbq_type == "Urban Design Single Hotplate")

#Swap out summarised & summarised_v2 to map unfiltered and filtered
qmplot(long, lat, data = summarised, maptype = "toner-lite", color = I("red")) + facet_wrap(~bbq_type) +
  theme(legend.position="none")
qmplot(long, lat, data = summarised, maptype = "toner-lite", color = bbq_type) +
  theme(legend.position="none")

#We can see where the different BBQs are spread out across Melb city



# Melb bike share data ----------------------------------------------------

#Let's start by splitting out the coordinates from this data so it's readable by ggmap
colnames(bike_share) <- tolower(colnames(bike_share))

#So there are definitely some locations that are better than others for bike availability,
#eg. St Kilda seems to have quite a few

bike_coords <- str_split(str_trim(bike_share$coordinates), ",", simplify = TRUE)
bike_coords

bike_coords[,1] <- str_replace_all(bike_coords[,1], c("\\("), "")
bike_coords[,2] <- str_replace_all(bike_coords[,2], c("\\)"), "")
bike_coords

bike_share_v2 <- bike_share %>%
  mutate(
    lat = as.numeric(bike_coords[,1]),
    long = as.numeric(bike_coords[,2]),
    availability_ratio = (nbbikes / nbempty)
  )

ggmap(map) + geom_point(aes(x = lat, y = long), data = bike_share_v2, alpha = 0.5, size = availability_ratio)

qmplot(long, lat, data = bike_share_v2, maptype = "toner-lite", alpha = 0.5, 
       size = availability_ratio) + theme(legend.position="none")

#This map confirms that St Kilda is in fact the best place for finding available bikes




# Cafes and restaurant data -----------------------------------------------

#trim and lowercase the column headers
colnames(cafes) <- tolower(str_replace_all(colnames(cafes), " ", ""))

#rename the x and y coordinates
colnames(cafes)[13] <- "lat" 
colnames(cafes)[12] <- "long"

table(cafes$censusyear) #There appears to be some data errors with years such as 1706 and 3555.

table(cafes$cluesmallarea) #The majority are in CBD however there's a few in the surrounding suburbs.

table(cafes$`industry(anzsic4)description`)

table(cafes$seatingtype)

qmplot(long, lat, data = filter(cafes, censusyear == 2016), maptype = "toner-lite", colour = seatingtype, alpha = 0.5, 
       size = numberofseats) + theme(legend.position="none")




# Drinking fountain data --------------------------------------------------

#The first step can be breaking out the coordinates for mapping, then using a group by and getting a sum
# of all the different drinking fountain types and their location.

table(drinking_fountains$Description)

#Split out the Description by referencing the dash

drinking_fountain_types <- str_split(drinking_fountains$Description, "-")
drinking_fountain_types

#SPlit out the coords the same way I did for the others
drinking_fountain_coords <- str_split(str_trim(drinking_fountains$`Co-ordinates`), ",", simplify = TRUE)
drinking_fountain_coords

drinking_fountain_coords[,1] <- str_replace_all(drinking_fountain_coords[,1], c("\\("), "")
drinking_fountain_coords[,2] <- str_replace_all(drinking_fountain_coords[,2], c("\\)"), "")
drinking_fountain_coords

drinking_fountains_v2 <- drinking_fountains %>%
  mutate(
    lat = as.numeric(drinking_fountain_coords[,1]),
    long = as.numeric(drinking_fountain_coords[,2])
  )

#Do a summary count

df_summary <- drinking_fountains_v2 %>%
  group_by(Description, lat, long) %>%
  summarise(
    total = n(),
    num_df = sum(is.na(Description))
  )
df_summary

qmplot(long, lat, data = df_summary, maptype = "toner-lite", color = Description) +
  theme(legend.position="none")



# Pedestrian data ---------------------------------------------------------

#remove the spaces from the column names
colnames(pedestrian_loc) <- str_replace_all(colnames(pedestrian_loc), " ", "_")
colnames(pedestrian)[9] <- "Sensor_Description"

table(pedestrian_loc$Sensor_Description)
table(pedestrian$Sensor_Name)

#There appears to be the same locations in both datasets so I should be able to do a join
# and get the lat and long of each sensor.

pedestrian_v2 <- left_join(pedestrian, pedestrian_loc, by = "Sensor_Description")

pedestrian_v2$date_test <- dmy_hm(pedestrian_v2$Date_Time)

ped_summarised <- pedestrian_v2 %>%
  group_by(Year, Latitude, Longitude) %>%
  summarise(
    total_volume = sum(Hourly_Counts)
  )

ped_vol_daily <- pedestrian_v2 %>%
  group_by(date_test) %>%
  summarise(
    daily_volume = sum(Hourly_Counts)
  )

qmplot(Longitude, Latitude, data = ped_summarised, maptype = "toner-lite", size = total_volume) + 
  theme(legend.position="none")
#No surprises that Swanston St and Flinders St are the busiest in terms of pedestrian traffic

#Does it change by day? 
#Not from what I can see, the size of the bubbles looks pretty consistent.

#What more can I learn from this data set? Is there a better time, month, day etc for ped traffic?

ggplot(data = ped_summarised, aes(x = Time, y = total_volume)) + 
  geom_bar(stat = "identity", aes(colour = "red", fill = "red")) + 
  theme(legend.position = "none")

ggplot(data = ped_vol_daily, aes(x = date_test, y = daily_volume)) + geom_smooth(se = FALSE)

#October and March have the most pedestrian foot traffic, with June having the lowest. Not sure why though.  

#Friday and Thursday have the most pedestrian foot traffic, with Sunday and Saturday having the lowest. 
#Interesting that the weekend is the lowest.

#4-5pm is also the busiest time of day, with 12-1 also being popular. 
#The lowest time is 4am. 

ggplot(data = ped_summarised, aes(x = Year, y = total_volume)) + 
  geom_bar(stat = "identity", aes(colour = "red", fill = "red")) + 
  theme(legend.position = "none")

#Foot traffic has increased considerably in the years between 2013-2016. 


# Start to combine the data sets to get some insight ----------------------


#What's been the trend of pedestrian traffic over time? In what locations? What calendar days showed the
#most foot traffic?








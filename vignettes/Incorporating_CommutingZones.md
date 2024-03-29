# Using Commuting Zones

What are the Commuting Zones? Commuting zones are geographic areas where
people live and work and are useful for understanding local economies.
Using Commuting Zones instead of cities or zipcodes in GeoLift will take
the commuter effect into account for the geographical randomisation. For
more details, please visit the
[documentation](https://facebookincubator.github.io/CommutingZones/docs/intro)
of Commuting Zones

## Installing Commuting Zones

Installing CommutingZones can be done directly using the remotes package
(which is likely already installed if you GeoLift is present)

``` r
install.packages("remotes")
remotes::install_github("facebookincubator/CommutingZones")
```

## Using Commuting Zones in GeoLift

``` r
library(CommutingZones)
library(dplyr)

# create input for commuting zones. country names needed to be added manually
location_df <- data.frame(
  location = unique(GeoTestData_PreTest$location),
  country = c("United States")
)

# Get google maps API key 
# https://developers.google.com/maps/documentation/maps-static/get-api-key/
matched_df <- commuting_zones(
  data = location_df,
  location_col_name = 'location',
  country_col_name = 'country',
  gmaps_key = 'Enter Your Google Maps API Key'
)
# plot the matched locations
plot(matched_df)

# transform for further GeoLift processing
GeoTestData_PreTest_CZ <- GeoTestData_PreTest %>% 
  merge(matched_df$matched_spdf %>% 
          as.data.frame() %>% 
          select(c("location", "fbcz_id_num")), 
        by = "location", all.x = TRUE) %>% 
  group_by(.data$fbcz_id_num, .data$time) %>% 
  mutate(Y = sum(.data$Y),
         location = paste(location, collapse = ", ")) %>% 
  distinct() %>% 
  rename(location = "fbcz_id_num", 
         location_in_cluster = "location") %>% ungroup()

# treat unmatched locations
unmatched_location <- GeoTestData_PreTest_CZ %>% 
    filter(is.na(location)) %>% 
    pull(location_in_cluster) %>% 
    unique()
df_unmatched <- data.frame(
  location_in_cluster = unmatched_location,
  location = max(GeoTestData_PreTest_CZ$location, na.rm = TRUE) + 
    seq_along(unmatched_location)
)
GeoTestData_PreTest_CZ <- GeoTestData_PreTest_CZ %>% 
  left_join(df_unmatched, by = "location_in_cluster") %>% 
  mutate(location = as.character(coalesce(location.y, location.x))) %>% 
  select(-location.y, -location.x)

# plot pretest
GeoPlot(GeoTestData_PreTest_CZ,
        Y_id = "Y",
        time_id = "time",
        location_id = "location")

# The object GeoTestData_PreTest_CZ can be used in further GeoLift functions 
# like GeoLift::GeoLiftMarketSelection(), GeoLift::GeoLiftPower() etc.
```

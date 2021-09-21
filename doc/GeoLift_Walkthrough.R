## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6, 
  fig.height = 4, 
  fig.align = "center"
)

## ----install, results="hide", message=F, eval=F-------------------------------
#  ## Install devtools if noy already installed
#  install.packages("devtools", repos='http://cran.us.r-project.org')
#  ## Install GeoLift from github
#  devtools::install_github("ArturoEsquerra/GeoLift")

## ----libraries, results="hide", warning=F, message=F--------------------------
library(augsynth)
library(gsynth)
library(GeoLift)
library(dplyr)
library(doParallel)
library(foreach)
library(MarketMatching)

## ----load_data, results="hide", warning=F, message=F--------------------------
data(GeoLift_PreTest)

## ----GeoDataRead--------------------------------------------------------------
GeoTestData_PreTest <- GeoDataRead(data = GeoLift_PreTest,
                                    date_id = "date",
                                    location_id = "location",
                                    Y_id = "Y",
                                    format = "yyyy-mm-dd")
head(GeoTestData_PreTest)

## ----plotting-----------------------------------------------------------------
GeoPlot(GeoTestData_PreTest,
        Y_id = "Y",
        time_id = "time",
        location_id = "location")

## ----numberlocations----------------------------------------------------------
resultsNum <- NumberLocations(data = GeoTestData_PreTest,
                                Y_id = "Y",
                                location_id = "location",
                                time_id = "time",
                                n_sim = 500,
                                treatment_periods = 15,
                                plot = TRUE,
                                power = 0.8,
                                alpha = 0.1,
                                fixed_effects = TRUE,
                                ProgressBar = TRUE)

## ----geoliftpowersearch-------------------------------------------------------
resultsSearch <- GeoLiftPower.search(data = GeoTestData_PreTest,
                                     treatment_periods = c(15),
                                     N = c(2,3,4),
                                     horizon = 50,
                                     Y_id = "Y",
                                     location_id = "location",
                                     time_id = "time",
                                     top_results = 20,
                                     alpha = 0.1,
                                     type = "pValue",
                                     fixed_effects = TRUE,
                                     ProgressBar = TRUE)

## ----moregeoliftpowersearch---------------------------------------------------
head(resultsSearch,50)

## ----marketselection----------------------------------------------------------
MarketSelection(GeoTestData_PreTest, location_id = "location", time_id = "time", Y_id = "Y")[1:5,1:5]

## ----powerfinder, fig.dim = c(7, 7)-------------------------------------------

resultsFind <- GeoLiftPowerFinder(data = GeoTestData_PreTest,
                                  treatment_periods = c(10,15),
                                  N = c(2,3,4),
                                  Y_id = "Y",
                                  location_id = "location",
                                  time_id = "time",
                                  effect_size = seq(0, 0.5, 0.05),
                                  top_results = 5,
                                  alpha = 0.1,
                                  fixed_effects = TRUE,
                                  ProgressBar = TRUE,
                                  plot_best = TRUE)
head(resultsFind,10)

## ----geoliftpower-------------------------------------------------------------
locs <- c("chicago", "portland")

resultsPow <- GeoLiftPower(GeoTestData_PreTest,
                           locations = locs,
                           effect_size = seq(0,0.25,0.01),
                           treatment_periods = c(10,15),
                           horizon = 50,
                           Y_id = "Y",
                           location_id = "location",
                           time_id = "time",
                           cpic = 7.50)

plot(resultsPow, actual_values = TRUE)

## ----test load_data-----------------------------------------------------------
data(GeoLift_Test)

## ----GeoDataRead_Test---------------------------------------------------------
GeoTestData_Test <- GeoDataRead(data = GeoLift_Test,
                                    date_id = "date",
                                    location_id = "location",
                                    Y_id = "Y",
                                    format = "yyyy-mm-dd")
head(GeoTestData_Test)

## ----plottingTest-------------------------------------------------------------
GeoPlot(GeoTestData_Test,
        Y_id = "Y",
        time_id = "time",
        location_id = "location",
        treatment_start = 91)

## ----GeoLift------------------------------------------------------------------
GeoTest <- GeoLift(Y_id = "Y",
                    data = GeoTestData_Test,
                    locations = locs,
                    treatment_start_time = 91,
                    treatment_end_time = 105)

## ----summary------------------------------------------------------------------
summary(GeoTest)

## ----plot_Lift----------------------------------------------------------------
plot(GeoTest, type = "Lift")

## ----plot_ATT-----------------------------------------------------------------
plot(GeoTest, type = "ATT")

## ----GeoLiftBest--------------------------------------------------------------
GeoTestBest <- GeoLift(Y_id = "Y",
                        data = GeoTestData_Test,
                        locations = locs,
                        treatment_start_time = 91,
                        treatment_end_time = 105,
                        model = "best")

summary(GeoTestBest)
plot(GeoTestBest, type = "Lift")
plot(GeoTestBest, type = "ATT")


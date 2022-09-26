#!/usr/bin/env python
# coding: utf-8

#############################################################################################
####################                 GeoLift in Python                 ######################
####################                    Quick guide                   #######################
#############################################################################################

## This is a simplified version of the GeoLift walkthrough in Python and contains only the 
# GeoLift single cell.

################################################################
#### Step 0: Installation

## Please check the README.md file for more details on how installing packages and 
# setting up Python environment.

################################################################
#### Step 1: Please read it before starting

## To run the R `GeoLift` functions in Python, you need to add `GeoLift.` in front of it 
# as in `GeoLift.GeoLiftMarketSelection` for example.
 
## There are 3 functions in Python under `utils.py`. For more details please run `help()` 
# with the name of one the three function below as parameter, e.g. `help(ConvertDf)`: 

## - `GeoLiftData`: Load and return the dataset included in the GeoLift package. 

## - `GeoLiftPlot`: Receive a specific GeoLift Plot function (defined by the `func` parameter),
# its arguments and display the plot.

##  - `ConvertDf`: Convert R dataframe into Pandas if `conv_type = "ToPandas"` or convert 
# Pandas dataframe into R if `conv_type = "ToR"`.
 
## To understand the paraments of a specfic R GeoLift function:
# - you can check the R versions:

## - GeoLift Walkthrough on 
# https://facebookincubator.github.io/GeoLift/docs/GettingStarted/Walkthrough 
# to understand the main parameters used by the GeoLift Single-cell functions. 

## - or you case use the command `print(r.help("name of the function in R"))` without `GeoLift.`. 
# See the following command examples: `print(r.help("GeoDataRead"))`, 
# `print(r.help("GeoLiftMarketSelection"))`.

## For vectors as input, you need to specify if it's a vector of list (defined by `ListVector`), 
# vector of integers (`IntVector`) or vector of strings (`StrVector`). 
# These are the equivalent of `c()` in R.

## For creating a sequence of numbers as input, use `r.seq()`. This is the equivalent of `seq()` 
# in R.

## It's recommended to input only rpy2 or R objects in the GeoLift functions to run it properly.

################################################################
#### Step 2: Loading libraries

## Load libraries

#To import rpy2 objects
from rpy2.robjects import packages, StrVector, IntVector, ListVector, r
from rpy2.robjects.packages import importr

import random
import pandas as pd

#Import GeoLift, GeoLiftPlot and Convert functions
from utils import GeoLiftData, GeoLift, GeoLiftPlot, ConvertDf

################################################################
#### Step 3: Load and formatting data

# pre-test historical data included in the GeoLiftPython folder.
pd_GeoLift_PreTest = pd.read_csv("GeoLift_PreTest.csv")
pd_GeoLift_PreTest.head()

## To input the dataset in the GeoLift functions you need to convert it to a R dataframe using 
# `ConvertDf()` setting up the `conv_type = "ToR"`. Then you can use `print()` to print the
# R dataframe in the screen.
GeoLift_PreTest = ConvertDf(pd_GeoLift_PreTest, conv_type = "ToR")

print(GeoLift_PreTest.head())

## The first step to run a GeoLift test is to read the data into the proper format using the 
# GeoDataRead function.
GeoTestData_PreTest = GeoLift.GeoDataRead(data = GeoLift_PreTest,
                                          date_id = "date",
                                          location_id = "location",
                                          Y_id = "Y",
                                          X = StrVector([]),#empty list as we have no covariates
                                          format = "yyyy-mm-dd",
                                          summary = True)


## Note: Before reading the data into this format, always make sure that there are no missing 
# variables, NAs, or locations with missing time-stamps as those will be dropped by the 
# `GeoLift.GeoDataRead()` function.

# You can visualize in Pandas the dataframe formatted using `ConvertDf()`.
pd_GeoTestData_PreTest = ConvertDf(GeoTestData_PreTest, conv_type = "ToPandas")

pd_GeoTestData_PreTest.head()


## A good next step is to plot the panel data with `GeoPlot()` setting up the partamer 
# `func = GeoLift.GeoPlot` to observe it’s trend, contribution per location, and also to detect 
# any data anomalies before moving on to the data analysis. grdevices.render_to_bytesion, 
# display and Image functions are used in combination to plot and display charts.

GeoLiftPlot(func = GeoLift.GeoPlot,
            topng = True,
            data = GeoTestData_PreTest,
            Y_id = "Y",
            time_id = "time",
            location_id = "location")


## In this case we see a similar pattern that’s shared across all locations. These structural 
# similarities between regions are the key to a successful test!

################################################################
#### Step 4: Power Analysis 

#### Step 4.1: Power Analysis and Market Selection

## Assessing the power and selecting the test markets for a GeoLift test can be accomplished 
# through the `GeoLiftMarketSelection()`. Through a series of simulations, this algorithm will 
# find which are the best combinations of test and control locations for the experiment. 
# Moreover, for each of these test market selections, the function will display the Minimum 
# Detectable Effect, minimum investment needed to run a successful test, and other important 
# model-fit metrics that  will help us select the test that best matches our goals and resources.

## To understand the main parameters in the `GeoLiftMarketSelection()` please check the 
# GeoLift Walkthrough: https://facebookincubator.github.io/GeoLift/docs/GettingStarted/Walkthrough.

## Continuing with the example and in order to explore the function’s capabilities let’s assume we 
# have two restrictions: Chicago must be part of the test markets and we have up to \\$100,000 to 
# run the test. We can include these constraints into `GeoLiftMarketSelection()` with the 
# `include_markets` and `budget` parameters and proceed with the market selection. Moreover, after 
# observing that the historical KPI values in `GeoPlot()` have been stable across time we will 
# proceed with a model with Fixed Effects. Finally, given a CPIC = \\$7.50 obtained from a previous 
# Lift test, a range between two to five test markets, and a duration between 10 and 15 days we obtain:
MarketSelections = GeoLift.GeoLiftMarketSelection(data = GeoTestData_PreTest,
                                                  treatment_periods = IntVector([10,15]),
                                                  N = IntVector([2, 3, 4, 5]),
                                                  Y_id = "Y",
                                                  location_id = "location",
                                                  time_id = "time",
                                                  effect_size = r.seq(0, 0.5, 0.05),
                                                  lookback_window = 1, 
                                                  include_markets = StrVector(['chicago']),
                                                  exclude_markets = StrVector(['honolulu']),
                                                  holdout = IntVector([0.5,1]),
                                                  cpic = 7.50,
                                                  budget = 100000,
                                                  alpha = 0.1,
                                                  Correlations = True,
                                                  fixed_effects = True,
                                                  side_of_test = "two_sided")


## You can visualize the best test markets converting the first position `[0]` of `MarketSelections` 
# using `ConvertDf()`.
MarketSelections_BestMarkets = ConvertDf(MarketSelections[0], conv_type = "ToPandas")

MarketSelections_BestMarkets.head()

## The results in `MarketSelections` show that the test markets with the best ranks are: `(atlanta, 
# chicago, las vegas, saint paul)` and `(chicago, portland)`. We can plot these last two results to 
# inspect them further setting up the parameter `func = GeoLift.plot_GeoLiftMarketSelection` in the 
# `GeoLiftPlot()`. This plot will show how the results of the `GeoLift()` model would look like with 
# the latest possible test period as well as the test’s power curve across a single simulation.

# Plot for atlanta, chicago, las vegas and saint paul for a 15 day test
GeoLiftPlot(func = GeoLift.plot_GeoLiftMarketSelection,
            topng = True,
            x = MarketSelections,
            market_ID = 1, 
            print_summary = False)


# Plot for chicago and porland for a 15 day test
GeoLiftPlot(func = GeoLift.plot_GeoLiftMarketSelection,
            topng = True,
            x = MarketSelections,
            market_ID = 2, 
            print_summary = False)


### Step 4.2 Power output - Deep dive into power curves

## In order to ensure that power is consistent throughout time for these locations, we can run more 
# than 1 simulation for each of the top contenders that came out of `GeoLiftMarketSelection`.

## We will do this by running the `GeoLiftPower` method and expanding our `lookback_window` to 
# 10 days, only for this treatment combination and plot their results.

## NOTE: You could repeat this process for the top 5 treatment combinations that come out of 
# GeoLiftMarketSelection, with increased lookback windows and compare their power curves. 
# We will do it only for Chicago and Portland here.

power_data = GeoLift.GeoLiftPower(data = GeoTestData_PreTest,
                                    locations = StrVector(['chicago', 'portland']),
                                    effect_size = r.seq(-0.25, 0.25, 0.01),
                                    lookback_window = 10,
                                    treatment_periods = 15,
                                    cpic = 7.5,
                                    side_of_test = "two_sided")


GeoLiftPlot(func = GeoLift.plot_GeoLiftPower,
            topng = True,
            x = power_data, 
            show_mde = True, 
            smoothed_values = False, 
            breaks_x_axis = 5)    


## While both market selections perform excellent on all metrics, we will move further with 
# the latter since it allows us to run a successful test with a smaller budget. Also, the 
# power curve is symmetrical with respect to the y axis, it has no power when the true effect 
# is zero, and has a similar behavior with more simulations. Finally, changing the `print_summary` 
# parameter of `GeoLiftPlot` to `True` can provide us with additional information about this 
# market selection.

# Plot for chicago and porland for a 15 day test
GeoLiftPlot(func = GeoLift.plot_GeoLiftMarketSelection,
            topng = True,
            x = MarketSelections,
            market_ID = 2, 
            print_summary = False)


### Step 4.3 Getting weights

## Note: Given that we are not using the complete pre-treatment data to calculate the weights in our
# power analysis simulations, the ones displayed by the plotting function above are not the final 
# values. However, you can easily obtain them with the `GetWeights()`.
weights = GeoLift.GetWeights(data = GeoTestData_PreTest,
                             Y_id = "Y",
                             location_id = "location",
                             time_id = "time",
                             locations = StrVector(['chicago', 'portland']),
                             pretreatment_end_time = 90,
                             fixed_effects = True)


# You can check the top weights by converting `weights` to pandas as shown below.
weights_pd = ConvertDf(df = weights, conv_type= "ToPandas")

# Top weights
weights_pd.sort_values(['weight'], axis=0, ascending=False).head()


################################################################
#### Step 5: Analyzing the Test Results

#### Step 5.1: Loading and formatting data

## Data for the campaign results can be accessed at `GeoLift_Test` that you can find in the 
# GeoLiftPython folder.
pd_GeoLift_Test = pd.read_csv("GeoLift_Test.csv")
pd_GeoLift_Test.head()


## To input the dataset in the GeoLift functions you need to convert it to a R dataframe using 
# `ConvertDf()`. Then you can use `print()` to print the R dataframe in the screen.
GeoLift_Test = ConvertDf(pd_GeoLift_Test, conv_type = "ToR")

print(GeoLift_Test.head())

## Similarly to the process executed at the beginning of the Power Analysis phase, we read the 
# data into GeoLift’s format using the `GeoDataRead` function. You can observe in the summary 
# output that additional 15 periods are contained in the new GeoLift data object.
GeoTestData_Test = GeoLift.GeoDataRead(data = GeoLift_Test,
                                       date_id = 'date',
                                       location_id = 'location',
                                       Y_id = 'Y',
                                       X = StrVector([]), #empty list as we have no covariates
                                       format = 'yyyy-mm-dd',
                                       summary = True)


# You can visualize in Pandas the dataframe formatted using `ConvertDf()`.
pd_GeoTestData_Test = ConvertDf(GeoTestData_Test, conv_type = "ToPandas")

pd_GeoTestData_Test.head()

## The results can also be plotted by setting up the `func = GeoLift.GeoPlot` in the `GeoLiftPlot` 
# function. However, for post-campaign data we can include the time-stamp at which the campaign 
# started through the `treatment_start` parameter to clearly separate the two periods. Plotting 
# the time-series is always useful to detect any anomalies with the data and to start noticing 
# patterns with the test.

GeoLiftPlot(func = GeoLift.GeoPlot,
            topng = True,
            data = GeoTestData_Test,
            Y_id = "Y",
            time_id = "time",
            location_id = "location")


#### Step 5.2: GeoLift Inference

## The next step in the process is to calculate the actual Lift caused by the marketing campaigns 
# on our test locations. To do so we make use of the `GeoLift()` function, which will take as input 
# the GeoLift dataframe as well as information about the test such as which were the cities in the 
# treatment group, when the test started, and when it ended through the `locations`, 
# `treatment_start_time`, and `treatment_end_time` parameters respectively.

GeoTest = GeoLift.GeoLift(Y_id = "Y",
                          data = GeoTestData_Test,
                          locations = StrVector(['chicago', 'portland']),
                          treatment_start_time = 91,
                          treatment_end_time = 105)

# To dig deeper into the results, we can run the `summary_GeoLift()` with `GeoLift` object as input.
p = GeoLift.summary_GeoLift(GeoTest)
print(p)

## The results show that the campaigns led to a 5.4% lift in units sold corresponding to 4667 
# incremental units for this 15-day test. Moreover, the Average Estimated Treatment Effect is of 
# 155.556 units every day of the test. Most importantly, we observe that these results are 
# statistically significant at a 95% level. In fact, there’s only a 1.1% chance of observing an 
# effect of this magnitude or larger if the actual treatment effect was zero. In other words, 
# it is extremely unlikely that these results are just due to chance.

GeoLiftPlot(func = GeoLift.plot_GeoLift,
            topng = True,
            x = GeoTest, 
            type = "Lift")


## Plotting the results is a great way to assess the model’s fit and how effective the campaign was. 
# Taking a close look at the pre-treatment period (period before the dotted vertical line) provides 
# insight into how well our Synthetic Control Model fitted our data. In this specific example, we 
# see that the observed values of the Chicago + Portland test represented in the solid black line were 
# closely replicated by our SCM model shown as the dashed red line. Furthermore, looking at the test 
# period, we can notice the campaign’s incrementality shown as the difference between the sales 
# observed in the test markets and our counterfactual synthetic location. This marked difference 
# between an almost-exact match in pre-treatment periods and gap in test time-stamps provides strong 
# evidence of a successful campaign.

GeoLiftPlot(func = GeoLift.plot_GeoLift,
            topng = True,
            x = GeoTest, 
            type = "ATT")

## Looking at the Average Estimated Treatment Effect’s plot can also be extremely useful. The ATT 
# metric shows us the magnitude of the Average Treatment Effect on a daily basis in contrast with 
# the previous (Lift) plot which focused on aggregated effects. Moreover, this is a great example 
# of a good GeoLift model as it has very small ATT values in the pre-treatment period and large ones 
# when the treatment is administered to our test locations. Moreover, point-wise confidence intervals 
# are included in this chart which help us measure how significant each day’s Lift has been.

#### Step 5.2.1: Improving The Model

GeoTestBest = GeoLift.GeoLift(Y_id = "Y",
                              data = GeoTestData_Test,
                              locations = StrVector(['chicago', 'portland']),
                              treatment_start_time = 91,
                              treatment_end_time = 105,
                              model = "best")

p = GeoLift.summary_GeoLift(GeoTestBest)
print(p)

GeoLiftPlot(func = GeoLift.plot_GeoLift,
            topng = True,
            x = GeoTestBest, 
            type = "Lift")

GeoLiftPlot(func = GeoLift.plot_GeoLift,
            topng = True,
            x = GeoTestBest, 
            type = "ATT")


## The new results augment the GeoLift model with a Ridge prognostic function which improves the model 
# fit as seen in the new L2 Imbalance metrics. This additional robustness is translated in a small 
# increase in the Percent Lift. Furthermore, by augmenting the model with a prognostic function, 
# we have an estimate of the estimated bias that was removed by the Augmented Synthetic Control Model.

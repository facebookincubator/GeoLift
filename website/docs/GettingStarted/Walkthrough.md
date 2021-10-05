---
sidebar_position: 2
---

# GeoLift Walkthrough

---

## 1. Data and R Environment

### Set-up of the R Environment
After installing the `GeoLift` package, it's important to load it into our R session.

``` r
library(GeoLift)
```

### Data

To show an end-to-end implementation of GeoLift we will use simulated
data of 40 US cities across 90 days to first design a test, select test
markets, run power calculations, and finally calculate the Lift caused
by the campaign. As with every GeoLift test, we start analyzing pre-test
historical information. We will use data included in the `GeoLift`
package.

``` r
data(GeoLift_PreTest)
```

The `GeoLift_PreTest` dataset contains three variables: location (city),
date (in “yyyy-mm-dd” format), and Y (number of conversions in each
day/location).

The first step to run a `GeoLift` test is to read the data into the
proper format using the `GeoDataRead` function.

``` r
GeoTestData_PreTest <- GeoDataRead(data = GeoLift_PreTest,
                                    date_id = "date",
                                    location_id = "location",
                                    Y_id = "Y",
                                    format = "yyyy-mm-dd")
head(GeoTestData_PreTest)
#>   location time    Y
#> 1  atlanta    1 3384
#> 2  atlanta    2 3904
#> 3  atlanta    3 5734
#> 4  atlanta    4 4311
#> 5  atlanta    5 3686
#> 6  atlanta    6 3374
```

This function analyzes the dataset, handles locations with missing data,
and returns a data frame with time-stamps instead of dates. In this
case, since we’re inputting daily data each time unit represents a day.

A good next step is to plot the panel data and observe it’s trend,
contribution per location, and also to detect any data anomalies before
moving on to the data analysis.

``` r
GeoPlot(GeoTestData_PreTest,
        Y_id = "Y",
        time_id = "time",
        location_id = "location")
```

![GeoPlot](/img/plotting-1.png)

In this case we see a similar pattern that’s shared across all
locations.

---

## 2. Power Analysis

Running a power analysis is fundamental prior to executing a test. It is
only through a thorough statistical analysis of our data that we can
set-up a test for success. In general, through the power analysis we can
find the optimal number of test locations, best test duration, select
the ideal test and control markets, get a good idea of the budget needed
to run a test, find which is the Minimum Detectable Effect to obtain
significant results, and set proper expectations for the results.

### How many test markets?

In general, there are two main types of users of `GeoLift`’s power
calculations: \* Those who through prior analysis already know which
locations they want to test and simply want to validate their test
design. \* Users who do not have an initial design in mind and want to
do a data-driven test set-up.

For those who fall under the second scenario, the first step is to get
an idea of how many test locations we would generally need to obtain a
well-powered test. This step allows us to better understand the data and
narrow down the number of test configurations. This analysis can be
achieved using the `NumberLocations` function of `GeoLift` which works
by simulating a large amount of tests and assess their results and fit
statistics.

The parameters used in the `NumberLocations` function inform the names
of the time, location, and units variables (time\_id, location\_id, and
time\_id respectively) in the input data. We also specify the length of
the test we have on mind with the treatment\_periods parameter (if not
sure, you can enter a list of test lengths too), the number of
simulations we want to run, control the output through the optional
plotting and progress bar parameters, set the desired significance and
power level, and also decide whether to include unit fixed-effects to
the model. There are several additional parameters that can be used to
further customize the simulations and outputs which can be accessed in
the package’s documentation.

``` r
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
#> Attempting to load the environment 'package:tidyr'
#> [1] "Average Power By Number of Locations"
#> # A tibble: 12 x 3
#>        n mean_pow mean_L2ScaledImbalance
#>    <dbl>    <dbl>                  <dbl>
#>  1     0    0                      1
#>  2     1    0.886                  0.445
#>  3     2    0.872                  0.401
#>  4     4    0.88                   0.366
#>  5     7    0.874                  0.351
#>  6     8    0.886                  0.356
#>  7    10    0.874                  0.359
#>  8    13    0.85                   0.375
#>  9    15    0.844                  0.369
#> 10    16    0.832                  0.375
#> 11    18    0.798                  0.387
#> 12    20    0.834                  0.396
```
![NumberLocations](/img/numberlocations-1.png)

he results show that we can obtain well-powered test even from just a
few locations. Moreover, the output shows the Scaled L2 Imbalance
metric, which is a metric between 0 and 1 that represents how well the
model is predicting the pre-treatment observed values through the
Synthetic Control. Scaled L2 Imbalance values of 0 show a perfect match
of pre-treatment values and values of 1 show a very inaccurate
prediction. In this case, we observe that this metric is relatively
stable once we have a couple or more test locations. These results
provide a good starting point as we now know that we can get a
well-powered test even if we just have a few test locations. In any
case, it is important to mention that `NumberLocations` is just menat to
guide our analysis as power can drastically change between different
market selections even if we keep the same number of test markets.

### Market Selection

Once that we have a better idea of how many markets are needed for a
well.powered test, we can proceed to the Market Selection. In this step
we use simulations to find which are the best combinations of test and
control locations for a predetermined number of test markets. This
process can be accomplished using the `GeoLiftPower.search` function
which will simulate a large amount of tests using rolling windows of
dates to find which combinations of test and control locations can
consistently provide stable and well-powered tests.

Similar to the previous function, we communicate to
`GeoLiftPower.search` the layout of our object through the time\_id,
location\_id, and Y\_id parameters. In addition, we set a test length
(or lengths if unsure) using the treatment\_periods parameter and
significance level through alpha. Moreover, the horizon parameter allows
us to focus more on more recent time-stamps. This is especially useful
if we see that older time-periods have significantly different dynamics
to the more recent (ones where the test would take place). In this case,
setting horizon = 50 is telling `GeoLiftPower.search` that we should
focus on time periods \[50,90\]. In addition, the N parameter specifies
the total number of locations that can be part of the test. Following
the results found by `NumberLocations` we focus on test designs that
have between 2 and 4 cities in the test group. Finally, some of the
outputs of the function can be controlled through parameters such as
top\_results which will determine how many of the top locations will be
printed.

``` r
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
#> [1] "Best 20 test markets:"
#> # A tibble: 20 x 1
#>    location
#>    <chr>
#>  1 chicago, portland
#>  2 chicago, cincinnati
#>  3 chicago, cincinnati, houston, portland
#>  4 chicago, houston, portland
#>  5 columbus, jacksonville, minneapolis
#>  6 columbus, jacksonville, milwaukee, minneapolis
#>  7 jacksonville, minneapolis
#>  8 atlanta, chicago, cincinnati, san diego
#>  9 jacksonville, kansas city, milwaukee, oakland
#> 10 kansas city, milwaukee, oakland
#> 11 columbus, minneapolis, new york
#> 12 detroit, jacksonville, new orleans
#> 13 atlanta, chicago, cincinnati
#> 14 austin, oakland
#> 15 nashville, san diego
#> 16 cleveland, denver, washington
#> 17 dallas, washington
#> 18 cleveland, dallas, denver, washington
#> 19 cleveland, denver
#> 20 atlanta, chicago, reno
```

The results show the best combination of test markets, the average power
that the combination was able to provide, its the average Scaled L2
Imbalance across all simulations, the proportion of total conversions
that the test markets represent, and it’s overall rank determined by
power and imbalance. In this case, we see that a test that uses the
combination of Chicago and Portland as test locations appears as the
top-ranked choice.

Inspecting the object created by `GeoLiftPower.search` shows us a more
detailed list of results.

``` r
head(resultsSearch,10)
#>                                                location mean_pow
#> 1                                     chicago, portland        1
#> 2                                   chicago, cincinnati        1
#> 3                chicago, cincinnati, houston, portland        1
#> 4                            chicago, houston, portland        1
#> 5                   columbus, jacksonville, minneapolis        1
#> 6        columbus, jacksonville, milwaukee, minneapolis        1
#> 7                             jacksonville, minneapolis        1
#> 8               atlanta, chicago, cincinnati, san diego        1
#> 9         jacksonville, kansas city, milwaukee, oakland        1
#> 10                      kansas city, milwaukee, oakland        1

#>    mean_scaled_l2_imbalance ProportionTotal_Y rank
#> 1                 0.1732343        0.03306537    1
#> 2                 0.1909320        0.03418832    2
#> 3                 0.1981689        0.07576405    3
#> 4                 0.2248921        0.05797087    4
#> 5                 0.2315280        0.05485904    5
#> 6                 0.2405941        0.07980000    6
#> 7                 0.2410849        0.03032910    7
#> 8                 0.2442581        0.07972448    8
#> 9                 0.2574026        0.14137854    9
#> 10                0.2685320        0.12663961   10
```

One key part of `GeoLiftPower.search` is that it use the similarities
between the market’s time series to determine hot to best combine them
into the test and control groups. This is important as Synthetic Control
Methods rely on the availability of similar markets to the test in order
to create the counterfactual. For this reason, the `MarketSelection`
function is a core part of `GeoLiftPower.search`. To obtain a better
understanding of the similarity structure between our cities you could
run `MarketSelection`.

``` r
MarketSelection(GeoTestData_PreTest, location_id = "location", time_id = "time", Y_id = "Y")[1:5,1:5]
#>      [,1]          [,2]           [,3]            [,4]          [,5]
#> [1,] "atlanta"     "chicago"      "las vegas"     "cleveland"   "reno"
#> [2,] "austin"      "oakland"      "oklahoma city" "kansas city" "saint paul"
#> [3,] "baltimore"   "philadelphia" "reno"          "chicago"     "houston"
#> [4,] "baton rouge" "portland"     "cincinnati"    "san diego"   "chicago"
#> [5,] "boston"      "new orleans"  "jacksonville"  "milwaukee"   "detroit"
```

The results show a matrix of similarities, where columns 2 onward
represents an ordered list of the most similar location to the one
showed in the first column. For instance, these results show that the
most similar city to Atlanta is Chicago followed by Las Vegas and
Cleveland.

### What about the Minum Detectable Effect?

Even though `GeoLiftPower.search` showed us which combination of test
and control markets were able to consistently obtain well-powered
results in a Synthetic Control test, it doesn’t tell us what is the Lift
we would need to obtain that powerful test. That’s why we recommend to
supplement the results of `GeoLiftPower.search` with the simulations run
by the function `GeoLiftPowerFinder`. More specifically, while the
former analyzes the stability of the time-series and ability of a SCM to
replicate the results, the latter explores the Minimum Detectable Effect
that a combination of test and control locations would provide.

The parameters needed by `resultsFind` are very similar to the ones used
by `GeoLiftPower.search` with the difference that this function doesn’t
use a horizon as it will focus on the most recent available data to run
a test as it is the one that would be closest to the market dynamics
that a test would experience. Also, a sequence of Effect Sizes or Lifts
are needed by this function as part of the effect\_size parameter. Each
element on this sequence will be assessed to find the test’s Minimum
Detectable Effect. In this case, we entered two different test lengths:
10 and 15 days as part of the treatment\_periods parameter. Setting the
plot\_best parameter to TRUE will output the plot of the top 4 test
markets to perform a deeper dive.

``` r

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
```

![PowerFinder_10](/img/powerfinder-1.png)
![PowerFinder_15](/img/powerfinder-2.png)

``` r
head(resultsFind,10)
#>                                    location pvalue duration MinDetectableEffect
#> 1                         chicago, portland      0       10                 0.1
#> 2                         chicago, portland      0       15                 0.1
#> 3                       chicago, cincinnati      0       15                 0.1
#> 4    chicago, cincinnati, houston, portland      0       10                 0.1
#> 5  columbus, minneapolis, new york, orlando      0       10                 0.1
#> 6                     baton rouge, portland      0       15                 0.1
#> 7  columbus, minneapolis, new york, orlando      0       15                 0.1
#> 8                 jacksonville, minneapolis      0       10                 0.1
#> 9                chicago, houston, portland      0       10                 0.1
#> 10               chicago, houston, portland      0       15                 0.1
#>    ScaledL2Imbalance ProportionTotal_Y rank
#> 1          0.1682310        0.03306537    1
#> 2          0.1738778        0.03306537    2
#> 3          0.1842457        0.03418832    3
#> 4          0.1966996        0.07576405    4
#> 5          0.2188363        0.08010168    5
#> 6          0.2220392        0.03289916    6
#> 7          0.2245568        0.08010168    7
#> 8          0.2278603        0.03032910    8
#> 9          0.2305628        0.05797087    9
#> 10         0.2336638        0.05797087   10
```

The results echo the findings of `GeoLiftPower.search` and show that the
combination of Chicago and Portland as test locations is optimal in
terms of stability and a Minimum Detectable Effect that is both small
and which has high certainty. Moreover, the Scaled L2 Imbalance results
show a good fit of the Synthetic Control on pre-treatment periods.

### Finding the Duration and Budget

So far our Market Selection based on Power Analyses provided us with a
promising combination of test and control locations. Specifically, we
would focus our treatment on Chicago and Portland while putting the rest
of the locations in the holdout group. However, we still haven’t found
the ideal test duration and estimated budget to necessary to run this
test. To do so, we need to perform a more thorough Power Analysis
focused on our chosen test configuration. The `GeoLiftPower` function
takes a set of test cities defined in the locations parameter, together
with a horizon, a sequence of lifts or effect sizes to be analyzed, and
a Cost Per Incremental Conversion (CPIC). This CPIC will allow us to
estimate the budget needed to achieve the required lift for a
well-powered test. Moreover, providing a list of different length
durations in the treatment\_periods parameter lets us see what the ideal
test duration should be for this particular set of test locations.

``` r
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
#>    duration lift     power
#> 1        10 0.00 0.0000000
#> 2        10 0.01 0.0000000
#> 3        10 0.02 0.0000000
#> 4        10 0.03 0.0000000
#> 5        10 0.04 0.0312500
#> 6        10 0.05 0.1562500
#> 7        10 0.06 0.4062500
#> 8        10 0.07 0.5625000
#> 9        10 0.08 0.7812500
#> 10       10 0.09 0.9687500
#> 11       10 0.10 0.9687500
#> 12       10 0.11 0.9687500
#> 13       10 0.12 1.0000000
#> 14       10 0.13 1.0000000
#> 15       10 0.14 1.0000000
#> 16       10 0.15 1.0000000
#> 17       10 0.16 1.0000000
#> 18       10 0.17 1.0000000
#> 19       10 0.18 1.0000000
#> 20       10 0.19 1.0000000
#> 21       10 0.20 1.0000000
#> 22       10 0.21 1.0000000
#> 23       10 0.22 1.0000000
#> 24       10 0.23 1.0000000
#> 25       10 0.24 1.0000000
#> 26       10 0.25 1.0000000
#> 27       15 0.00 0.0000000
#> 28       15 0.01 0.0000000
#> 29       15 0.02 0.0000000
#> 30       15 0.03 0.0000000
#> 31       15 0.04 0.0000000
#> 32       15 0.05 0.1851852
#> 33       15 0.06 0.3333333
#> 34       15 0.07 0.5555556
#> 35       15 0.08 0.7037037
#> 36       15 0.09 0.7407407
#> 37       15 0.10 0.8888889
#> 38       15 0.11 1.0000000
#> 39       15 0.12 1.0000000
#> 40       15 0.13 1.0000000
#> 41       15 0.14 1.0000000
#> 42       15 0.15 1.0000000
#> 43       15 0.16 1.0000000
#> 44       15 0.17 1.0000000
#> 45       15 0.18 1.0000000
#> 46       15 0.19 1.0000000
#> 47       15 0.20 1.0000000
#> 48       15 0.21 1.0000000
#> 49       15 0.22 1.0000000
#> 50       15 0.23 1.0000000
#> 51       15 0.24 1.0000000
#> 52       15 0.25 1.0000000
```

![Power_10](/img/geoliftpower-1.png)
![Power_15](/img/geoliftpower-2.png)

The power curves for the 10 and 15 day tests show similar results with a
minimum required lift of roughly 10%. Moreover, budget calculations
indicate that at a $7.50 Cost Per Incremental Conversion, a well-powered
15-day test would require a budget of around $60,000.

---

## 3. Analyzing the Test Results

Based on the results of the Power Calculations, a test is set-up in
which a 15-day marketing campaign will be executed in the cities of
Chicago and Portland while the rest of the locations will be put on
holdout. Following the completion from this marketing campaign, we
receive sales data that reflects these results. This new data-set
contains the same format and information as the pre-test one but
crucially includes results for the duration of the campaign. Depending
on the vertical and product, adding a post-campaign cooldown period
might be useful.

### Test Data

Data for the campaign results can be accessed at `GeoLift_Test`.

``` r
data(GeoLift_Test)
```

Similarly to the process executed at the beginning of the Power Analysis
phase, we read the data into GeoLift’s format using the `GeoDataRead`
function.

``` r
GeoTestData_Test <- GeoDataRead(data = GeoLift_Test,
                                    date_id = "date",
                                    location_id = "location",
                                    Y_id = "Y",
                                    format = "yyyy-mm-dd")
head(GeoTestData_Test)
#>   location time    Y
#> 1  atlanta    1 3384
#> 2  atlanta    2 3904
#> 3  atlanta    3 5734
#> 4  atlanta    4 4311
#> 5  atlanta    5 3686
#> 6  atlanta    6 3374
```

The results can also be plotted using the `GeoPlot` function. However,
for post-campaign data we can include the time-stamp at which the
campaign started through the treatment\_start parameter to clearly
separate the two periods. Plotting the time-series is always useful to
detect any anomalies with the data and to start noticing patterns with
the test.

``` r
GeoPlot(GeoTestData_Test,
        Y_id = "Y",
        time_id = "time",
        location_id = "location",
        treatment_start = 91)
```

![GeoPlotTestData](/img/plottingTest-1.png)

### GeoLift Inference

The next step in the process is to calculate the actual Lift caused by
the marketing campaigns on our test locations. To do so we make use of
the `GeoLift` function, which will take as input the GeoLift dataframe
as well as information about the test such as which were the cities in
the treatment group, when the test started, and when it ended through
the locations, treatment\_start\_time, and treatment\_end\_time
parameters respectively.

``` r
GeoTest <- GeoLift(Y_id = "Y",
                    data = GeoTestData_Test,
                    locations = locs,
                    treatment_start_time = 91,
                    treatment_end_time = 105)
#>
#> GeoLift Output
#>
#> Test results for 15 treatment periods, from time-stamp 91 to 105 for test markets:
#> 1 CHICAGO
#> 2 PORTLAND
#> ##################################
#> #####     Test Statistics    #####
#> ##################################
#>
#> Percent Lift: 5.4%
#>
#> Incremental Y: 4667
#>
#> Average Estimated Treatment Effect (ATT): 155.556
#>
#> The results are significant at a 95% level.
#>
#> There is a 0.5% chance of observing an effect this large or larger assuming treatment effect is zero.
```

The results show that the campaigns led to a 5.4% lift in units sold
corresponding to 4667 incremental units sold for this 15-day test.
Moreover, the Average Estimated Treatment Effect (also known as ATT) is
of 155.556 units every day of the test. Most importantly, we observe
that these results are statistically significant at a 95% level. In
fact, there’s only a 1.1% chance of observing an effect of this
magnitude or larger if the actual treatment effect was zero. In other
words, it is extremely unlikely that these results are just due to
chance. To dig deeper into the results, we can run the summary of our
GeoLift object.

``` r
summary(GeoTest)
#>
#> GeoLift Results Summary
#> ##################################
#> #####     Test Statistics    #####
#> ##################################
#>
#> * Average ATT: 155.556
#> * Percent Lift: 5.4%
#> * Incremental Y: 4667
#> * P-value: 0
#>
#> ##################################
#> #####   Balance Statistics   #####
#> ##################################
#>
#> * L2 Imbalance: 909.489
#> * Scaled L2 Imbalance: 0.1636
#> * Percent improvement from naive model: 83.64%
#> * Average Estimated Bias: NA
#>
#> ##################################
#> #####     Model Weights      #####
#> ##################################
#>
#> * Prognostic Function: NONE
#>
#> * Model Weights:
#>  * austin: 0.0465
#>  * baton rouge: 0.1335
#>  * cincinnati: 0.2272
#>  * dallas: 0.0739
#>  * honolulu: 0.0673
#>  * houston: 0.0046
#>  * miami: 0.2028
#>  * minneapolis: 0.09
#>  * nashville: 0.0685
#>  * new york: 0.0046
#>  * reno: 0.0306
#>  * san antonio: 0.0054
#>  * san diego: 0.0451
```

The summary show additional test statistics such as the p-value which
was equal to 0.01 confirming the highly statistical significance of
these results. Moreover, the summary function provides Balance
Statistics which display data about our model’s fit. The main metric of
model fit used in GeoLift is the L2 Imbalance which represents how far
our synthetic control was from the actual observed values in the
pre-treatment period. That is, how similar the synthetic Chicago +
Portland unit we crated is from the observed values of these cities in
the period before the intervention. A small L2 Imbalance score means
that our model did a great job replicating our test locations while a
large one would indicate a poor fit. However, the L2 Imabalnce metric is
scale-dependent, meaning that it can’t be compared between models with
different KPIs or number of testing periods. For instance, the L2
Imbalance of a model run on units sold will be significantly larger than
a model ran for tons of product sold even if they represent the same
basic underlying metric.

Therefore, given that it’s hard to tell simply by looking at the value
of the L2 Imbalance metric whether the model had a good or poor fit, we
also included the Scaled L2 Imbalance stat which is easier to interpret
as it’s bounded in the range between 0 and 1. A value close to zero
represents a good model fit while values nearing 1 indicate a poor
performance by the Synthetic Control Model. This scaling is accomplished
by comparing the Scaled L2 Imbalance of our Synthetic Control Method
with the Scaled L2 Imbalance obtained by a baseline/naive model (instead
of carefully calculating which is the optimal weighting scheme for the
Synthetic Control, we assign equal weights to each unit in the donor
pool). The latter provides an upper bound of L2 Imbalance, therefore,
the Scaled L2 Imbalance shows us how much better our GeoLift model is
from the baseline.

In fact, another way to look at the Scaled L2 Imbalance is the percent
improvement from a naive model which can be obtained by subtracting our
model’s Scaled L2 Imbalance from 100%. In this case, an improvement
close to 100% (which corresponds to a Scaled L2 Imbalance close to zero)
represents a good model fit. Finally, we also include the weights that
generate our Synthetic Control. In this test we note that the locations
that contribute the most to our GeoLift model are Cincinnati, Miami, and
Baton Rouge.

``` r
plot(GeoTest, type = "Lift")
```

![LiftPlot](/img/plot_Lift-1.png)

Plotting the results is a great way to assess the model’s fit and how
effective the campaign was. Taking a close look at the pre-treatment
period (white background) provides insight into how well our Synthetic
Control Model fitted our data. In this specific example, we see that the
observed values of the Chicago + Portland test represented in the solid
black line were closely replicated by our model shown as the dashed red
line. Furthermore, looking at the test period, we can notice the
campaign’s incrementality shown as the difference between the sales
observed in the test markets and our counterfactual synthetic location.
This marked difference between an almost-exact match in pre-treatment
periods and gap in test time-stamps provides strong evidence of a
successful campaign.

``` r
plot(GeoTest, type = "ATT")
```

![PlotATT](/img/plot_ATT-1.png)

Looking at the Average Estimated Treatment Effect’s plot can also be
extremely useful. The ATT metric shows us the magnitude of the Average
Treatment Effect on a daily basis in contrast with the previous (Lift)
plot which focused on aggregated effects. Moreover, this is a great
example of a good GeoLift model as it has very small ATT values in the
pre-treatment period and large ones when the treatment is administered
to our test locations. Moreover, point-wise confidence intervals are
included in this chart which help us measure how meaningful each day
Lift has been.

### Improving The Model

While the results obtained from the test are robust and highly
significant, a useful feature of GeoLift is its ability to improve the
model fit even further and reduce bias through augmentation by a
prognostic function. There are several options for augmentation of the
standard GeoLift model such as regularization (specifically Ridge) and a
application of Generalized Synthetic Control Model (GSC). While each of
these approaches provide it’s own set of advantages, for instance Ridge
regularization usually performs well when the number of units and
time-periods isn’t large while GSC helps improve fit for situations with
many pre-treatment periods, GeoLift offers the option to let the model
decide which is the best approach by setting the model parameter to
“best”.

``` r
GeoTestBest <- GeoLift(Y_id = "Y",
                        data = GeoTestData_Test,
                        locations = locs,
                        treatment_start_time = 91,
                        treatment_end_time = 105,
                        model = "best")
#>
#> GeoLift Output
#>
#> Test results for 15 treatment periods, from time-stamp 91 to 105 for test markets:
#> 1 CHICAGO
#> 2 PORTLAND
#> ##################################
#> #####     Test Statistics    #####
#> ##################################
#>
#> Percent Lift: 5.5%
#>
#> Incremental Y: 4704
#>
#> Average Estimated Treatment Effect (ATT): 156.805
#>
#> The results are significant at a 95% level.
#>
#> There is a 1.2% chance of observing an effect this large or larger assuming treatment effect is zero.

summary(GeoTestBest)
#>
#> GeoLift Results Summary
#> ##################################
#> #####     Test Statistics    #####
#> ##################################
#>
#> * Average ATT: 156.805
#> * Percent Lift: 5.5%
#> * Incremental Y: 4704
#> * P-value: 0.01
#>
#> ##################################
#> #####   Balance Statistics   #####
#> ##################################
#>
#> * L2 Imbalance: 903.525
#> * Scaled L2 Imbalance: 0.1626
#> * Percent improvement from naive model: 83.74%
#> * Average Estimated Bias: -1.249
#>
#> ##################################
#> #####     Model Weights      #####
#> ##################################
#>
#> * Prognostic Function: RIDGE
#>
#> * Model Weights:
#>  * atlanta: 3e-04
#>  * austin: 0.0467
#>  * baltimore: 1e-04
#>  * baton rouge: 0.1337
#>  * boston: -4e-04
#>  * cincinnati: 0.2273
#>  * columbus: 1e-04
#>  * dallas: 0.0741
#>  * denver: 1e-04
#>  * detroit: 1e-04
#>  * honolulu: 0.0674
#>  * houston: 0.0048
#>  * indianapolis: 1e-04
#>  * jacksonville: -1e-04
#>  * kansas city: -1e-04
#>  * los angeles: 2e-04
#>  * memphis: -2e-04
#>  * miami: 0.2029
#>  * milwaukee: -2e-04
#>  * minneapolis: 0.0901
#>  * nashville: 0.0687
#>  * new orleans: -2e-04
#>  * new york: 0.0048
#>  * oakland: -0.001
#>  * oklahoma city: -7e-04
#>  * orlando: 1e-04
#>  * philadelphia: -4e-04
#>  * reno: 0.0308
#>  * saint paul: 2e-04
#>  * salt lake city: -3e-04
#>  * san antonio: 0.0056
#>  * san diego: 0.0452
#>  * san francisco: 1e-04
#>  * tucson: -1e-04
plot(GeoTestBest, type = "Lift")
```

![PlotLiftBest](/img/GeoLiftBest-1.png)

``` r
plot(GeoTestBest, type = "ATT")
```

![PlotATTBest](/img/GeoLiftBest-2.png)

The new results augment the GeoLift model with a Ridge prognostic
function which improves the model fit as seen in the new L2 Imbalance
metrics. This additional robustness is translated in a small increase in
the Percent Lift. Furthermore, by augmenting the model with a prognostic
function, we have an estimate of the estimated bias.

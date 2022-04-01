---
sidebar_position: 1
---

# GeoLift Best Practices

---

## Data
* Daily granularity in the data is strongly recommended over weekly.
* It is recommended to use the highest level of geographical granularity at which we can target Facebook campaigns in the study's region (Zip Codes, Cities, etc.).
* We recommend having at least 4 - 5x the test duration of pre-campaign historical data of stable data (must not contain structural changes or any other impactful deviation from their data-generating process.).
* At minimum we recommend having 25 pre-treatment periods of 20 or more geo-units.
* Under normal circumstances we advise having historical information to be over the last 52 weeks, this may take into consideration any seasonal variations across brand product sales as well as account for other factors that may not be taken into consideration over a shorter duration.
* The test duration should cover at least one purchase cycle for the product.
* Study should be run for a minimum of 15 days, if daily, and a minimum of 4-6 weeks if weekly to ensure enough data points are available to assess market impact.
* Panel data of covariates is recommended to improve the model but not necessary.
* Date, location, and units (or any other KPI) must be had for each time/location combination (no missing values for any unit or timestamp). Additional covariates can be used but follow the same guideline.

 ---

## Test and Control Markets
* Match test and control markets on variables such as sales plus other relevant variables that are specific to the brand category (e.g. product distribution, seasonal variations across geos).
* Test and control markets should be matched on the exact same outcome of interest, doing this may eliminate any bias in the results across markets.

---

## Local Marketing Efforts
* Account for any local media efforts such as local TV, any regional offline marketing efforts across the selection markets as these may contribute to some unbalancing factors when comparing results across control and test markets.
* Keep all local marketing efforts to be constant across all markets that are taken into consideration across test and control.

---

## National Marketing Efforts
* Take into consideration any significant variations across national media such as TV, Print etc. during pre-test or test period will make it hard to really isolate the impact of Facebook on the outcome of interest.
* For sales to be truly attributed to Facebook variations, all other media should be held constant across the markets and if there are significant variations, make sure to address these before the test.

---
slug: MultiCell
title: Multi-Cell Experiments with GeoLift
authors: [arturo]
tags: [GeoLift, Geo Measurement, MultiCell, CrossChannel, Optimization]
---

## Introducing Multi-Cell GeoLift Tests

We're introducing Multi-Cell capabilities to the GeoLift code that can empower users to easily measure multiple treatments in a single experiment through different cells. These new capabilities unlock the potential to plan and execute tests to measure across strategies and channels! Launched with `GeoLift v2.5`, you now can:

- Easily set-up Multi-Cell tests through a Statistical Power-based Market Selection.

- Calculate and plot the Power Curves for Multi-Cell tests.

- Determine the test design required to find a Winner Cell.

- Inference of Multi-Cell tests.

- Get inspired with some of our external success cases of Multi-Cell tests showing how to [optimize a channel](https://www.facebook.com/business/success/ruffles-mexico?locale=es_LA) or how to measure [incrementality across digitial channels](https://www.facebook.com/business/success/3-liverpool?locale=es_LA).

- Learn how to run Multi-Cell tests with our [new Multi-Cell walkthrough](https://facebookincubator.github.io/GeoLift/docs/GettingStarted/MultiCellWalkthrough) and [GeoLift v2.5 now](https://github.com/facebookincubator/GeoLift/)!


## Multi-Cell Tests

We've introduced a set of new capabilities to GeoLift focused at Multi-Cell tests. Specifically, we added four new functions to set-up and execute these tests in a simple yet powerful way.

### 1. MultiCellMarketSelection
`MultiCellMarketSelection()` will help the user identify and select their Test Markets based on their desired number of cells for either Standard or Inverse/Negative GeoLifts.

![MarketSelection](https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/MultiCellBlog1.jpg?raw=true)

![MarketSelectionLiftPlots](https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/MultiCellBlog2.jpg?raw=true)

### 2. MultiCellPower
After finding the optimal test and control locations for the Multi-Cell test, the user is able to estimate and plot the Power Curves for each of his Test Markets through the `MultiCellPower()` function.

![PowerCurves](https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/MultiCellBlog3.jpg?raw=true)

### 3. MultiCellWinner
When the test's objective is to identify a winner strategy or channel, the user has the option to use `MultiCellWinner()` which will identify how much better the performance of Cell A compared to Cell B should be in order to declare a winner through a statistical test. If more than two cells are provided, the test will perform all pairwise comparisons.

![Winner](https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/MultiCellBlog4.jpg?raw=true)

### 4. GeoLiftMultiCell
Finally, after the test finishes `GeoLiftMultiCell()` will compute the inference and will show whether there was a winning cell based on the test results!

![Results](https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/MultiCellBlog5.jpg?raw=true)
![ResultsPlot](https://github.com/facebookincubator/GeoLift/blob/main/website/static/img/MultiCellBlog6.jpg?raw=true)

## Start Your Multi-Cell Testing Now!
Learn more about how to run Multi-Cell tests through our [Multi-Cell GeoLift Walkthrough](https://facebookincubator.github.io/GeoLift/docs/GettingStarted/MultiCellWalkthrough), get the latest version of GeoLift from the [GitHub repository](https://github.com/facebookincubator/GeoLift/) and start testing!

---
sidebar_position: 2
---

# Comparison to other Methodologies

Below is a quick summary of some of the most popular geo testing algorithms currently used in the market. We have kept it at a very high-level intentionally. A more thorough comparison of GeoLift to other pseudo-experimental approaches will be added soon.

1. **Differences-in-differences:** This is the most popular and easiest approach to run an inference for a geo test. Not recommended usually unless the predicted effect size is really large. DID has fallen out of use mainly due to its reliance on the parallel trends assumption, which rarely happens in actual experiments.
2. **CausalImpact (by Google)**: Uses a Bayesian time-series modeling technique to predict the counterfactual. It’s especially useful when strong prior knowledge is available or when the KPI metric isn’t available and modelling has to be done on a proxy.
3. **GeoX (by Google):** Uses a linear model to predict the counterfactual time-series. It is positioned as a more robust solution for paired market studies and also as having superior small sample properties (low number of test/control markets).
4. **Synthetic Control Method (synth):** Uses a weighted combination of control market conversions to build a counterfactual for test markets. This method has gained a lot of popularity recently but can provide biased results if a perfect match can't be found from the pool of controls. Variations of this method power GeoLift.

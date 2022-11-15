---
sidebar_position: 1
---

# GeoLift Methodology

---

## Incrementality: The Gold-Standard of Measurement
**Incrementality-based** measurement allows us to know the true value of our marketing efforts. In essence, a campaign’s incremental effect is the difference between what we observed as the results of that campaign and what would have happened in a world where it didn’t take place (counterfactual). And while Randomized Controlled Trials (RCTs) such as Facebook's [Conversion Lift](https://www.facebook.com/business/m/one-sheeters/conversion-lift) remain as the gold-standard of incrementality, they are not always technically or practicably feasible. For instance, Lift studies are often gated by the access to online signals (pixels, APIs, SDKs, etc.). This means that advertisers that do not have access to these signals, are in the process of losing them, or do not want to share them weren't able to leverage incrementality to make key business decisions.

---

## Quasi-Experiments and Synthetic Control Methods
Quasi-Experiments offer a great alternative to measure Lift whenever an RCT is not viable and GeoLift leverages some of the latest developments in this area to empower advertisers to embrace incrementality. GeoLift is based on [Synthetic Control Methods (SCMs)](https://economics.mit.edu/sites/default/files/publications/Synthetic%20Control%20Methods.pdf), which work by creating an artificial unit that is as similar as possible to the test unit. Using historical information prior to the treatment SCMs find the combination of untreated units that most closely replicate the treated. This effectively creates a constructed or synthetic control which will provide a robust counterfactual to which we will measure the campaign’s effectiveness. Prior to the intervention the test and synthetic control are virtually identical, therefore, any differences after the treatment started between these two units is the campaign’s incrementality.


![GeoLift Example](/img/Methodology_GeoLift_Lift.png)


The idea behind SCMs is that a combination of units often provides a better comparison for the unit exposed to the intervention than any single unit alone (like in matching analyses). Moreover, by constructing the counterfactual as a weighted average of the units of observation, these approaches provide additional robustness against omitted variable biases as long as the control units have similar characteristics to the test. In practice, unit comparability and similarity are a given since in GeoLift studies we use locations of the same region or country as test and control units. Moreover, SCMs rely on panel data and can reliably account for confounders changing over time unlike traditional Difference-In-Difference quasi-experimental methods.


---
## Making Synthetic Control Methods Even Better
Nevertheless, SCMs are not perfect and are subject to biases due to inexact matching which happens when we can’t reliably re-create the test unit with the controls. Fortunately, GeoLift is based on the combination of two cutting-edge SCM methods that address these limitations and provide a powerful foundation for our tool. Specifically, GeoLift combines the sturdiness of synthetic control estimations of [Augmented Synthetic Control Methods (ASCM)](https://eml.berkeley.edu/~jrothst/workingpapers/BMFR_Synth_Nov_2018.pdf) with the powerful inference capabilities of [Generalized Synthetic Controls (GSC)](https://www.cambridge.org/core/journals/political-analysis/article/generalized-synthetic-control-method-causal-inference-with-interactive-fixed-effects-models/B63A8BD7C239DD4141C67DA10CD0E4F3).

Specifically, GeoLift uses ASCM to estimate and de-bias the synthetic control estimate and then uses GSC’s robustness on small samples and on heterogeneous effects across units to perform inference. Moreover, the two-step implementation of these approaches addresses imbalances caused by the curse of dimensionality which typically causes bias as the amount of historical data and units increases given that the likelihood of finding exact matching decreases rapidly as more dimensions are added to the solutions-space. Finally, GSC provides powerful parametric bootstrapping approaches to provide valid and reliable inference and uncertainty estimates. All-in-all, the combination of these two approaches provide robustness against biases in GeoLift at the cost of additional processing power.

SCMs have been regarded as [“arguably the most important innovation in the policy evaluation literature in the last 15 years”](https://pubs.aeaweb.org/doi/pdfplus/10.1257/jep.31.2.3) (Athey and Imbens 2017). However, their adoption in other areas such as marketing has been slow. This is mainly due to the lack of power calculations for campaigns, which makes it difficult to plan and design future studies. These calculations are particularly important for geo-testing experiments, where the effect sizes are usually small and where there is often a significant chance to fail to find the effect of the experiment. Therefore, running a geo-test without a robust prior power analysis leads to a high chance of failing to find lift, even if it actually happened.

Moreover, through a power analysis we can not only align expectations before the test, but we can even set it up for success by finding which test set-up has the best chance to detect the lift. Through this analysis we can find which are the best test locations, how many we should include, investment level, and even how long we should run the test for in order to be able to detect the lift.

GeoLift addresses these issues by providing three power calculators for three common use-cases:

#### Test length and investment/lift for known test locations

This calculator is useful when an advertiser knows which test locations he wants to use for an experiment, but needs help finding out the investment and test length. The calculator takes as input the dataset, a list of test locations, and a Cost Per Incremental Conversion (if available) to help determine the necessary investment to execute a well-powered test.



#### Optimal Number of Test Markets

When considering running a GeoLift, you might need guidance determining how many test locations they need to use for an experiment. GeoLift provides a power calculator that can provide valuable help to determine how many test markets are needed for a well-powered test.



#### Market Selection

Users that wants to run their first Geo-Test might not know how many markets they should select, which ones to use, or even for how long to run the test. GeoLift provides Power Calculators that help in these situations to find the best possible test based on historical data. Our Market Selection algorithms will set your test up for success!

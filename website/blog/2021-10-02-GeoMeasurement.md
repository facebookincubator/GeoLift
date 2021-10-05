---
slug: geo-measurement-review
title: A Brief Review of Geo Measurement Approaches
authors: [arturo]
tags: [GeoLift, Geo Measurement, SCM, DID]
---

## Intro to Quasi-Experiments
While Randomized Control Trials remain as the gold-standard for causal analysis, good RCTs are hard to come by given how complicated and expensive they are to execute. In particular, their reliance on randomization, which is the foundation for their unbiasedness, is often one of the factors that limit their usage. Some of the most common drawbacks of randomly splitting a population into test and control groups are:

- Implementing and maintaining the randomization throughout the experiment requires a robust infrastructure.
- Limiting the treatment to only the test group can be unethical. For example, restraining the control group from receiving life-saving medicine is wrong. This could also be the case for PSAs.
- It is common to have constraints on which units can be part of the test and control groups. These constraints prevent us from having a good randomization. For example, in a geo-experiment there are often a set of locations that need to receive the treatment and some units can’t get the treatment, which severely reduces the possible randomizations and greatly reduces the experiment’s precision.

Quasi-experimental methods offer a great alternative to measure the impact of a treatment (such as an ad campaign) whenever randomization is not logistically feasible or ethical. These methods differ from traditional RCTs in that they don’t use randomization to select the test and control groups. This gives us a lot of additional flexibility in the experimental design at the cost of a typically larger sample sizes and additional modeling assumptions. Nevertheless, under the right circumstances quasi-experiments provide a great alternative to measure a treatment and can empower advertisers that have been historically unable to use incrementality to start taking decisions based on Lift. Moreover, one of the most commonly used type of quasi-experiments are geo tests, in which the units of experimentation are geographical areas such as zip-codes, cities, regions, or states. In this note we will do a historical review of the most commonly used approaches to geographic quasi-experimentation and compare them to GeoLift.

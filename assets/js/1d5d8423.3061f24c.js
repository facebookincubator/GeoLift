"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[102],{9569:function(e){e.exports=JSON.parse('{"blogPosts":[{"id":"geolift-v23","metadata":{"permalink":"/GeoLift/blog/geolift-v23","editUrl":"https://github.com/facebookincubator/GeoLift/blog/2022-03-03-GeoLiftv23.md","source":"@site/blog/2022-03-03-GeoLiftv23.md","title":"Launching GeoLift v2.3 - Streamlined and Improved Power Calculations","description":"GeoLift v2.3 Release","date":"2022-03-03T00:00:00.000Z","formattedDate":"March 3, 2022","tags":[{"label":"GeoLift","permalink":"/GeoLift/blog/tags/geo-lift"},{"label":"Geo Measurement","permalink":"/GeoLift/blog/tags/geo-measurement"},{"label":"SCM","permalink":"/GeoLift/blog/tags/scm"},{"label":"ASCM","permalink":"/GeoLift/blog/tags/ascm"}],"readingTime":2.715,"truncated":false,"authors":[{"name":"Arturo Esquerra","title":"Marketing Science @ Facebook | GeoLift Team","url":"https://github.com/ArturoEsquerra/","imageURL":"https://github.com/ArturoEsquerra.png","key":"arturo"}],"nextItem":{"title":"A Brief Review of Geo Measurement Approaches","permalink":"/GeoLift/blog/geo-measurement-review"}},"content":"## GeoLift v2.3 Release\\n\\n### TL;DR\\n- The new version of the GeoLift code makes the pre-test analysis (Power Analysis and Market Selection) easier than ever before! The entire process is now handled by a single function: `GeoLiftMarketSelection()`.\\n- The new package contains dozens of new features, functionalities, and bug fixes.\\n- A new and improved walkthrough guide is available to help internal and external users run their GeoLift tests from start to finish!\\n\\n## GeoLift v2.3: Power Calculations Made Easier\\nFollowing the feedback we received, we re-built the Power Calculation and Market Selection functionalities from the ground up! The new code makes this process easier, more streamlined, and more powerful than ever before.\\n\\n### What\'s New?\\n- We\'ve **streamlined the entire Power Analysis and Market Selection process into a single function**: `GeoLiftMarketSelection()`.\\n- Previously, the process to select the test markets was very convoluted and involved going through several functions: `NumberLocations()`, `GeoLiftPower.search()`, `GeoLiftPowerFinder()`, and `GeoLiftPower()`. The new function, `GeoLiftMarketSelection()`, aggregates all of their functionalities and even improves upon them.\\n- We\'re soft-deprecating the `NumberLocations()`, `GeoLiftPower.search()`, `GeoLiftPowerFinder()`, and `GeoLiftPower()`. The development of these functions is now considered complete and they\'re now superseeded by `GeoLiftMarketSelection()`.\\n- We\'ve included **two** new important **metrics** that can help us make better decisions when comparing between different test market candidates: _Average_MDE_ and _abs_lift_in_zero_.\\n- A **new and improved ranking system** takes into consideration all key model performance metrics to identify the best test markets for a given set-up.\\n- We\'ve added the option for **one tailed tests**.  These make the GeoLift model much more powerful than it was before, by simply changing the test hypothesis we want to validate.  Now you can chose from _Positive_, _Negative_ and _Total_ tests in the inference section and one sided or two sided tests in the power section.\\n- The Market Selection process is more flexible and customizable than before! You can now include **additional test constraints** to focus only on the tests that make sense for our client. These are: the available budget (_budget_), acceptable holdout ranges (_holdout_), test markets we want to force into the test regions (_include_markets_), and markets that shouldn\'t be considered as eligible test regions (_exclude_markets_).\\n- We\'ve added a **new plotting method for GeoLiftMarketSelection** objects. Through this method you can easily plot different test market selections and compare their model fit, MDE, and power curves! Plus, they lines have the GeoLift colors!\\n![PlotATTBest](/img/GeoLiftMarketSelection_Plot2-1.png)\\n- The **new function**: `GetWeights()` makes it easy to save the synthetic control weights into a data frame for further analysis.\\n- To aid with **MMM calibrations**, we\'ve included a new parameter in the Market Selection/Power Analysis function: _Correlations_. Setting it to TRUE allows the user to analyze the similarities between test and the control regions.\\n- You can also **plot historical similarities between test and control regions** with `plotCorrels()`.\\n![PlotATTBest](/img/plotcorrels.png)\\n- A revamped **GeoLift walkthrough vignette has been launched** to accompany the new version of the package. This new material provides much more detailed explanation on our model, it\'s parameters, how to run a study, and how to interpret the results.\\n- We\'ve **fixed multiple bug and errors** across the package (thanks for the feedback!).\\n- Thanks to all of these changes, we\'ve **significantly reduced the total time needed in pre-test calculations!**"},{"id":"geo-measurement-review","metadata":{"permalink":"/GeoLift/blog/geo-measurement-review","editUrl":"https://github.com/facebookincubator/GeoLift/blog/2021-10-02-GeoMeasurement.md","source":"@site/blog/2021-10-02-GeoMeasurement.md","title":"A Brief Review of Geo Measurement Approaches","description":"Intro to Quasi-Experiments","date":"2021-10-02T00:00:00.000Z","formattedDate":"October 2, 2021","tags":[{"label":"GeoLift","permalink":"/GeoLift/blog/tags/geo-lift"},{"label":"Geo Measurement","permalink":"/GeoLift/blog/tags/geo-measurement"},{"label":"SCM","permalink":"/GeoLift/blog/tags/scm"},{"label":"DID","permalink":"/GeoLift/blog/tags/did"}],"readingTime":1.68,"truncated":false,"authors":[{"name":"Arturo Esquerra","title":"Marketing Science @ Facebook | GeoLift Team","url":"https://github.com/ArturoEsquerra/","imageURL":"https://github.com/ArturoEsquerra.png","key":"arturo"}],"prevItem":{"title":"Launching GeoLift v2.3 - Streamlined and Improved Power Calculations","permalink":"/GeoLift/blog/geolift-v23"},"nextItem":{"title":"Welcome","permalink":"/GeoLift/blog/welcome"}},"content":"## Intro to Quasi-Experiments\\nWhile Randomized Control Trials remain as the gold-standard for causal analysis, good RCTs are hard to come by given how complicated and expensive they are to execute. In particular, their reliance on randomization, which is the foundation for their unbiasedness, is often one of the factors that limit their usage. Some of the most common drawbacks of randomly splitting a population into test and control groups are:\\n\\n- Implementing and maintaining the randomization throughout the experiment requires a robust infrastructure.\\n- Limiting the treatment to only the test group can be unethical. For example, restraining the control group from receiving life-saving medicine is wrong. This could also be the case for PSAs.\\n- It is common to have constraints on which units can be part of the test and control groups. These constraints prevent us from having a good randomization. For example, in a geo-experiment there are often a set of locations that need to receive the treatment and some units can\u2019t get the treatment, which severely reduces the possible randomizations and greatly reduces the experiment\u2019s precision.\\n\\nQuasi-experimental methods offer a great alternative to measure the impact of a treatment (such as an ad campaign) whenever randomization is not logistically feasible or ethical. These methods differ from traditional RCTs in that they don\u2019t use randomization to select the test and control groups. This gives us a lot of additional flexibility in the experimental design at the cost of a typically larger sample sizes and additional modeling assumptions. Nevertheless, under the right circumstances quasi-experiments provide a great alternative to measure a treatment and can empower advertisers that have been historically unable to use incrementality to start taking decisions based on Lift. Moreover, one of the most commonly used type of quasi-experiments are geo tests, in which the units of experimentation are geographical areas such as zip-codes, cities, regions, or states. In this note we will do a historical review of the most commonly used approaches to geographic quasi-experimentation and compare them to GeoLift."},{"id":"welcome","metadata":{"permalink":"/GeoLift/blog/welcome","editUrl":"https://github.com/facebookincubator/GeoLift/blog/2021-10-01-welcome/index.md","source":"@site/blog/2021-10-01-welcome/index.md","title":"Welcome","description":"Welcome to GeoLift\'s blog where we discuss anything related to:","date":"2021-10-01T00:00:00.000Z","formattedDate":"October 1, 2021","tags":[{"label":"facebook","permalink":"/GeoLift/blog/tags/facebook"},{"label":"hello","permalink":"/GeoLift/blog/tags/hello"},{"label":"GeoLift","permalink":"/GeoLift/blog/tags/geo-lift"}],"readingTime":0.105,"truncated":false,"authors":[{"name":"Arturo Esquerra","title":"Marketing Science @ Facebook | GeoLift Team","url":"https://github.com/ArturoEsquerra/","imageURL":"https://github.com/ArturoEsquerra.png","key":"arturo"},{"name":"Nicolas Cruces","title":"Marketing Science @ Facebook | GeoLift Team","url":"https://github.com/NicolasMatrices-v2","imageURL":"https://github.com/NicolasMatrices-v2.png","key":"nicolas"}],"prevItem":{"title":"A Brief Review of Geo Measurement Approaches","permalink":"/GeoLift/blog/geo-measurement-review"}},"content":"**Welcome to GeoLift\'s blog** where we discuss anything related to:\\n- Geo-testing\\n- Incrementality and Lift\\n- GeoLift\\n- and more!"}]}')}}]);
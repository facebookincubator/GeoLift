    ![GeoLiftLogo](doc/GeoLift_IconText.png)

# GeoLift
[![Project Status: Active  The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview
GeoLift is an end-to-end solution to measure Lift at a Geo-level using the latest developments in Synthetic Control Methods. Through this package it is possible to do a data-driven market selection for a geo-test using a variety of Power Calculators. Moreover, GeoLift features easy-to-use inference and plotting functionalities to analyze the results of a test.

Check out the vignette for a more detailed description of the main functionalities of the package:
- [GeoLift Walkthrough](https://github.com/facebookincubator/GeoLift/blob/master/vignettes/GeoLift_Walkthrough.md)
- [GeoLift Multi-Cell Walkthrough](https://github.com/facebookincubator/GeoLift/blob/main/vignettes/GeoLift_MultiCell_Walkthrough.md)

## Requirements
GeoLift requires or works with:
- R version 4.0.0 or newer.

## Installing GeoLift
To install the package, first make sure that `remotes`, `LowRankQP`, `Synth` and 
`augsynth` are installed.

```
install.packages("remotes", repos='http://cran.us.r-project.org')

install.packages(
  'LowRankQP',
  repos='https://cran.microsoft.com/snapshot/2023-03-01/')

remotes::install_github("cran/synth", dependencies = TRUE)

remotes::install_github("ebenmichael/augsynth")
```

Then, install the package from GitHub:

```
remotes::install_github("facebookincubator/GeoLift")
```

## Contacts
- aesquerra@fb.com, Arturo Esquerra, Facebook Marketing Science Partner
- kpanchal@fb.com, Kanishka Panchal, Facebook Marketing Science Partner
- nicocru@fb.com, Nicolas Cruces, Facebook Marketing Science Partner
- michaelkhalil@fb.com, Michael Khalil, Facebook Marketing Science Partner

## Join the GeoLift community
- [Facebook page](https://www.facebook.com/groups/fbgeolift/)

## License
GeoLift is MIT licensed, as found in the LICENSE file.

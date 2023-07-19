---
sidebar_position: 1
---

# Getting Started with GeoLift

---

## 1. Install R

GeoLift is an R package, therefore, it is necessary to have [R version 4.0.0 (or higher)](https://www.r-project.org/) installed in order to run the code. R is a free software environment for statistical computing and graphics. It compiles and runs on a wide variety of UNIX platforms, Windows and MacOS. To [download R](https://cran.r-project.org/mirrors.html), please choose your preferred [CRAN mirror](https://cran.r-project.org/mirrors.html).

If you have questions about R like how to download and install the software, or what the license terms are, please read their [answers to frequently asked questions](https://cran.r-project.org/faqs.html) before you send an email.

---

## 2. Installing the GeoLift Package

Since GeoLift is currently only available on GitHub, the `remotes` package is a pre-requisite to install it. If you haven't installed this package yet, first run:

```
install.packages("remotes", repos='http://cran.us.r-project.org')
```

To install the package, first make sure that remotes and `augsynth` are installed.

```
remotes::install_github("ebenmichael/augsynth")
```

Finally, we can install the `GeoLift` package with the following command:

```
remotes::install_github("facebookincubator/GeoLift")
```

---

## 3. GeoLift Walkthrough Example

If you're running GeoLift for the first time, we recommend going through the example in the **[GeoLift Walkthrough](./Walkthrough.md)**.


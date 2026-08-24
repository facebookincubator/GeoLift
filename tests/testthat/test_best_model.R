context("model = 'best' consistency across entry points")

## Shared synthetic panel: 8 locations x 60 periods, no treatment effect.
make_panel <- function() {
  set.seed(7)
  units <- paste0("u", 1:8)
  n_t <- 60
  df <- expand.grid(location = units, time = 1:n_t, stringsAsFactors = FALSE)
  df$Y <- 200 + 10 * sin(df$time / 5) +
    as.numeric(factor(df$location)) * 15 + rnorm(nrow(df), 0, 3)
  df
}

test_that("ResolveBestModel picks the lowest imbalance and excludes failures", {
  # Fast unit tests of the selector itself, with ASCMExecution mocked out.
  # l2s maps model name -> scaled_l2_imbalance value, or a function to error.
  fake_ascm <- function(l2s) {
    function(...) {
      out <- l2s[[list(...)$model]]
      if (is.function(out)) out()
      list(augsynth_model = list(scaled_l2_imbalance = out))
    }
  }
  resolve <- function(l2s) {
    with_mocked_bindings(
      suppressMessages(GeoLift:::ResolveBestModel(
        data = data.frame(), treatment_locations = c("u1", "u2"),
        treatment_start_time = 1, treatment_end_time = 2
      )),
      ASCMExecution = fake_ascm(l2s), .package = "GeoLift"
    )
  }
  boom <- function() stop("fit failed")

  # plain minimum
  expect_equal(resolve(list(none = 0.8, ridge = 0.5, GSYN = 0.6)), "ridge")
  expect_equal(resolve(list(none = 0.8, ridge = 0.6, GSYN = 0.5)), "GSYN")
  # a failed candidate is never selected, even when the fitted ones tie
  expect_equal(resolve(list(none = boom, ridge = 0.5, GSYN = 0.5)), "ridge")
  # a strictly-worse 'none' is not selected on a ridge/GSYN tie
  expect_equal(resolve(list(none = 0.8, ridge = 0.5, GSYN = 0.5)), "ridge")
  # all-candidate tie goes to the simplest model
  expect_equal(resolve(list(none = 0.5, ridge = 0.5, GSYN = 0.5)), "none")
  # NaN / NULL imbalances are treated as failures, with a message
  expect_message(
    r <- with_mocked_bindings(
      GeoLift:::ResolveBestModel(
        data = data.frame(), treatment_locations = c("u1", "u2"),
        treatment_start_time = 1, treatment_end_time = 2
      ),
      ASCMExecution = fake_ascm(list(none = NaN, ridge = 0.7, GSYN = NULL)),
      .package = "GeoLift"
    ),
    "candidate 'none' failed"
  )
  expect_equal(r, "ridge")
  # every candidate failing stops with a clear error
  expect_error(resolve(list(none = boom, ridge = NaN, GSYN = boom)),
               "every candidate model failed")
  # single treatment location: GSYN skip must not make GSYN selectable
  expect_equal(
    with_mocked_bindings(
      suppressMessages(GeoLift:::ResolveBestModel(
        data = data.frame(), treatment_locations = "u1",
        treatment_start_time = 1, treatment_end_time = 2
      )),
      ASCMExecution = fake_ascm(list(none = 0.8, ridge = 0.7)), .package = "GeoLift"
    ),
    "ridge"
  )
})

test_that("simulation entry points reject model = 'best' with a clear error", {
  df <- make_panel()
  # Previously: "task 1 failed - \"progfunc must be one of ...\"" buried in a
  # foreach worker; now an immediate, self-explanatory stop().
  expect_error(
    GeoLiftMarketSelection(
      data = df, treatment_periods = 10, N = 1,
      effect_size = c(-0.1), lookback_window = 1, cpic = 1,
      model = "best"
    ),
    "only supported in GeoLift\\(\\) and GetWeights\\(\\)"
  )
  expect_error(
    GeoLiftPower(
      data = df, locations = c("u1"), effect_size = c(-0.1),
      treatment_periods = 10, lookback_window = 1,
      model = "best"
    ),
    "only supported in GeoLift\\(\\) and GetWeights\\(\\)"
  )
})

test_that("GetWeights(model = 'best') works as documented", {
  skip_on_cran()
  skip_if_not_installed("augsynth")
  df <- make_panel()
  # Documented for years, previously died on augsynth's progfunc validation.
  w <- suppressMessages(
    GetWeights(
      data = df, locations = c("u1", "u2"),
      pretreatment_end_time = 50, model = "best"
    )
  )
  expect_s3_class(w, "data.frame")
  expect_true(all(c("location", "weight") %in% names(w)))
  expect_gt(nrow(w), 0)
  expect_true(all(is.finite(w$weight)))
})

test_that("GeoLift(model = 'best') announces the GSYN skip for a single location", {
  skip_on_cran()
  skip_if_not_installed("augsynth")
  df <- make_panel()
  expect_message(
    g <- GeoLift(
      Y_id = "Y", data = df, locations = "u1",
      treatment_start_time = 51, treatment_end_time = 60,
      model = "best"
    ),
    "skipping the GSYN candidate \\(single treatment location\\)"
  )
  expect_s3_class(g, "GeoLift")
  expect_true(is.finite(g$inference$ATT))
})

test_that("GeoLift(model = 'ridge') is unaffected", {
  skip_on_cran()
  skip_if_not_installed("augsynth")
  df <- make_panel()
  g <- suppressMessages(
    GeoLift(
      Y_id = "Y", data = df, locations = "u1",
      treatment_start_time = 51, treatment_end_time = 60,
      model = "ridge"
    )
  )
  expect_s3_class(g, "GeoLift")
  expect_true(is.finite(g$inference$ATT))
})

test_that("bfpwr dependency provides the required API", {
  expect_true(
    utils::packageVersion("bfpwr") >= numeric_version("0.3"),
    info = "jaspPower requires bfpwr 0.3 or newer for sequential Bayes factor design."
  )

  required_exports <- c(
    "bf01",
    "binbf01",
    "dirbf01",
    "nbf01",
    "nbf01seq",
    "nbinbf01",
    "nmbf01",
    "nnmbf01",
    "ntbf01",
    "ntbf01seq",
    "pbf01",
    "pbf01seq",
    "pbinbf01",
    "pnmbf01",
    "powerbf01seq",
    "powertbf01seq",
    "ptbf01",
    "ptbf01seq",
    "tbf01"
  )

  missing_exports <- setdiff(required_exports, getNamespaceExports("bfpwr"))
  expect_equal(missing_exports, character())

  sequential_search_exports <- c(
    "nbf01seq",
    "ntbf01seq",
    "powerbf01seq",
    "powertbf01seq"
  )

  for (export in sequential_search_exports) {
    arg_names <- names(formals(getExportedValue("bfpwr", export)))
    expect_true("search"   %in% arg_names, info = sprintf("bfpwr::%s() must support search.", export))
    expect_true("progress" %in% arg_names, info = sprintf("bfpwr::%s() must support progress.", export))
  }

  z_progress_ticks <- 0L
  z_search <- bfpwr::nbf01seq(
    k1      = 1 / 10,
    k0      = 10,
    power   = 0.8,
    nrange  = c(20, 100),
    minN    = 20,
    by      = 10,
    pm      = 0,
    psd     = 1,
    dpm     = 0.5,
    dpsd    = 1,
    usd     = 1,
    type    = "normal",
    strict  = FALSE,
    integer = TRUE,
    details = TRUE,
    search  = "adaptive",
    progress = function(info) z_progress_ticks <<- z_progress_ticks + 1L
  )
  expect_true(length(z_search[["maximumN"]]) == 1 && is.finite(z_search[["maximumN"]]))
  expect_false(is.null(z_search[["result"]]))
  expect_false(is.null(z_search[["result"]][["n"]]))
  expect_equal(z_progress_ticks, z_search[["evaluations"]])

  t_progress_ticks <- 0L
  t_search <- bfpwr::ntbf01seq(
    k1          = 1 / 2,
    k0          = 2,
    power       = 0.4,
    nrange      = c(20, 80),
    minN        = 20,
    by          = 10,
    plocation   = 0,
    pscale      = 1,
    pdf         = 1,
    dpm         = 0.5,
    dpsd        = 0,
    type        = "two.sample",
    alternative = "two.sided",
    ratio       = 1.5,
    strict      = FALSE,
    integer     = TRUE,
    details     = TRUE,
    search      = "adaptive",
    progress    = function(info) t_progress_ticks <<- t_progress_ticks + 1L
  )
  expect_true(length(t_search[["maximumN"]]) == 1 && is.finite(t_search[["maximumN"]]))
  expect_false(is.null(t_search[["result"]]))
  expect_false(is.null(t_search[["result"]][["n1"]]))
  expect_false(is.null(t_search[["result"]][["n2"]]))
  expect_equal(t_progress_ticks, t_search[["evaluations"]])
})

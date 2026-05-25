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
    "nbinbf01",
    "nmbf01",
    "nnmbf01",
    "ntbf01",
    "pbf01",
    "pbf01seq",
    "pbinbf01",
    "pnmbf01",
    "ptbf01",
    "ptbf01seq",
    "tbf01"
  )

  missing_exports <- setdiff(required_exports, getNamespaceExports("bfpwr"))
  expect_equal(missing_exports, character())
})

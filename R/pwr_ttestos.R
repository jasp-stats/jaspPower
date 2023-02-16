# Originally based on https://github.com/richarddmorey/jpower

ttestOneSClass <- R6::R6Class(
  "ttestOneSClass",
  inherit = ttestPSClass,
  private = list(
    type = "one.sample"
  )
)

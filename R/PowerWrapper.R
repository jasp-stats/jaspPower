#
# Copyright (C) 2013-2025 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# This is a generated file. Don't change it!

#' Power
#'
Power <- function(
          data = NULL,
          version = "1",
          alpha = 0.05,
          alternative = "twoSided",
          baselineProportion = 0.5,
          calculation = "sampleSize",
          comparisonProportion = 0.6,
          effectDirection = "greater",
          effectDirectionSyntheticDataset = "less",
          effectSize = 0.5,
          firstGroupMean = 0,
          firstGroupSd = 1,
          logSampleSize = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          populationSd = 1,
          power = 0.9,
          powerByEffectSize = TRUE,
          powerBySampleSize = FALSE,
          powerContour = TRUE,
          powerDemonstration = FALSE,
          sampleSize = 20,
          sampleSizeRatio = 1,
          saveDataset = FALSE,
          savePath = "",
          secondGroupMean = 0,
          secondGroupSd = 1,
          seed = 1,
          setSeed = FALSE,
          test = "independentSamplesTTest",
          testValue = 0,
          text = TRUE,
          varianceRatio = 2) {

   defaultArgCalls <- formals(jaspPower::Power)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("alternative", "calculation", "effectDirection", "test")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspPower", "Power", "Power.qml", options, version, TRUE))
}
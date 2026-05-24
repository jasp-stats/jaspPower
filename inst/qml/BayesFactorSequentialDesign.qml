//
// Copyright (C) 2013-2024 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import "./qml_components" as PowerComponents

Form
{

	Group
	{
		DropDown
		{
			name: "statisticalTest"
			id:   test
			indexDefaultValue: 0
			label: qsTr("Statistical test:")
			info: qsTr("Select the statistical test for which the Bayes factor sequential design is planned.")
			values: [
				{ label: qsTr("Independent Samples T-Test"), value: "independentSamplesTTest" },
				{ label: qsTr("Paired Samples T-Test"),      value: "pairedSamplesTTest"      },
				{ label: qsTr("One Sample T-Test"),          value: "oneSampleTTest"          },
				{ label: qsTr("Independent Samples Z-Test"), value: "independentSamplesZTest" },
				{ label: qsTr("Paired Samples Z-Test"),      value: "pairedSamplesZTest"      },
				{ label: qsTr("One Sample Z-Test"),          value: "oneSampleZTest"          },
				{ label: qsTr("General (z-approximation)"),  value: "generalZApproximation"   }
			]
		}

		CheckBox
		{
			label: qsTr("Explanatory notes")
			info:  qsTr("Append brief interpretation notes to each displayed Bayesian result.")
			id:    text
			name:  "explanatoryText"
			checked: true
		}

		CheckBox
		{
			label: qsTr("Generate report")
			info:  qsTr("Generate a short report describing the sequential design, priors, thresholds, and achieved probabilities.")
			id:    generateReport
			name:  "generateReport"
			checked: false

			CheckBox
			{
				label: qsTr("LaTeX formatted output")
				info:  qsTr("Format the generated report with LaTeX notation for use in manuscripts and preregistrations.")
				id:    generateReportLatex
				name:  "generateReportLatexFormattedOutput"
				checked: false
			}
		}
	}

	Section
	{
		expanded: true
		title: qsTr("Parameters")
		columns: 1

		PowerComponents.BayesFactorSequentialParameterControls
		{
			id: sequentialParameters
			testValue: test.currentValue
		}
	}

	PowerComponents.BayesFactorAnalysisPriorSection
	{
		testValue: test.currentValue
		supportsDirectionalZ: true
	}

	PowerComponents.BayesFactorDesignPriorSection
	{
		testValue: test.currentValue
	}

	Section
	{
		expanded: true
		title: qsTr("Tables")

		Group
		{
			title: qsTr("Design Results")

			CheckBox
			{
				name: "designSummary"
				label: qsTr("Design summary")
				info:  qsTr("Show the main sequential-design result: the maximum sample size or the achieved probability of conclusive evidence.")
				checked: true
			}

			CheckBox
			{
				name: "sampleSizeSummary"
				label: qsTr("Sample size summary")
				info:  qsTr("Show the expected sample size and its variability under each design prior.")
				checked: true
			}

			CheckBox
			{
				name: "decisionProbabilities"
				label: qsTr("Decision probabilities")
				info:  qsTr("Show final probabilities of evidence for H\u2081, evidence for H\u2080, and inconclusive evidence under each design prior.")
				checked: true
			}

			CheckBox
			{
				name: "designSpecification"
				label: qsTr("Design specification")
				info:  qsTr("Show the analysis and design priors used for computing and evaluating the Bayes factor sequential design.")
				checked: true
			}
		}

		Group
		{
			title: qsTr("Look-by-Look Results")

			CheckBox
			{
				name: "cumulativeDecisionProbabilities"
				label: qsTr("Cumulative decision probabilities")
				info:  qsTr("Show the probability that the study has stopped for each decision by each planned look.")
				checked: false
			}

			CheckBox
			{
				name: "incrementalDecisionProbabilities"
				label: qsTr("Incremental decision probabilities")
				info:  qsTr("Show the probability that each decision is first reached at a given look.")
				checked: false
			}

			CheckBox
			{
				name: "stoppingBoundariesTable"
				label: qsTr("Stopping boundaries")
				info:  qsTr("Show the test-statistic values corresponding to the Bayes factor stopping thresholds at each look.")
				checked: false
			}
		}
	}

	Section
	{
		expanded: true
		title: qsTr("Plots")

		Group
		{
			title: qsTr("Decision Probability")

			CheckBox
			{
				label: qsTr("Cumulative decision probabilities")
				info:  qsTr("Plot how stopping probabilities for conclusive, misleading, and inconclusive evidence accumulate across looks.")
				name:  "cumulativeDecisionProbabilitiesPlot"
				checked: false
			}

			CheckBox
			{
				label: qsTr("Stopping boundaries")
				info:  qsTr("Plot the test-statistic boundaries that trigger stopping for H\u2081 or H\u2080 at each look.")
				name:  "stoppingBoundariesPlot"
				checked: false
			}

			CheckBox
			{
				name: "combineH1H0Figures"
				label: qsTr("Combine H\u2081 and H\u2080")
				info:  qsTr("Display curves for the H\u2081 and H\u2080 design priors in one figure instead of separate figures.")
				checked: false
			}
		}

		PowerComponents.BayesFactorPriorPlot {}

		PowerComponents.BayesFactorPlotSettings {}

	}

	PowerComponents.BayesFactorAnalysisSection
	{
		testValue: test.currentValue
	}

	Section
	{
		expanded: false
		title: qsTr("Advanced")
		columns: 1

		PowerComponents.BayesFactorSequentialSearchBounds
		{
			usesSampleSizeSearch: sequentialParameters.usesSampleSizeSearch
			linkLowerBoundToStartingPoint: sequentialParameters.linksLowerSearchBoundToStartingPoint
			startingPoint: sequentialParameters.searchStartingSampleSize
			isIndependentSamples: test.currentValue.indexOf("independentSamples") !== -1
		}

		Group
		{
			columns: 3

			CheckBox
			{
				Layout.columnSpan: 3
				name: "generateRCode"
				id:   generateRCode
				label: qsTr("Generate R Code")
				info:  qsTr("Generate R code that reproduces the Bayes factor sequential-design calculation.")
				checked: false
			}

			Text { Layout.columnSpan: 2; text: qsTr("Exact integration over all regions:") }
			CheckBox
			{
				name: "exactIntegrationOverAllRegions"
				id:   strictIntegration
				label: ""
				info:  qsTr("Use exact integration over all decision regions when computing sequential design characteristics.")
				checked: true
			}

			Text { text: qsTr("Integration method:") }
			Text { text: qsTr("Method") }
			DropDown
			{
				name: "integrationMethod"
				id:   integrationMethod
				info: qsTr("Numerical method used for multivariate normal probabilities in sequential calculations.")
				indexDefaultValue: 0
				values: [
					{ label: qsTr("Log-scale MVN"), value: "lpmvnorm" },
					{ label: qsTr("MVN"),           value: "pmvnorm"  }
				]
			}

			Text
			{
				text: qsTr("Absolute tolerance:")
				visible: integrationMethod.currentValue === "pmvnorm"
			}
			Text
			{
				text: qsTr("abs")
				visible: integrationMethod.currentValue === "pmvnorm"
			}
			DoubleField
			{
				name: "integrationAbsoluteTolerance"
				id:   integrationAbsEps
				info: qsTr("Absolute error tolerance for numerical integration.")
				min: 0
				defaultValue: 0.000001
				inclusive: JASP.None
				visible: integrationMethod.currentValue === "pmvnorm"
			}

			Text
			{
				text: qsTr("Relative tolerance:")
				visible: integrationMethod.currentValue === "pmvnorm"
			}
			Text
			{
				text: qsTr("rel")
				visible: integrationMethod.currentValue === "pmvnorm"
			}
			DoubleField
			{
				name: "integrationRelativeTolerance"
				id:   integrationRelEps
				info: qsTr("Relative error tolerance for numerical integration.")
				min: 0
				defaultValue: 0
				inclusive: JASP.MinOnly
				visible: integrationMethod.currentValue === "pmvnorm"
			}

			Text
			{
				text: qsTr("Maximum integration points:")
				visible: integrationMethod.currentValue === "pmvnorm"
			}
			Text
			{
				text: qsTr("max")
				visible: integrationMethod.currentValue === "pmvnorm"
			}
			IntegerField
			{
				name: "integrationMaximumPoints"
				id:   integrationMaxPts
				info: qsTr("Maximum number of integration points used by the numerical routine.")
				min: 1000
				defaultValue: 25000
				visible: integrationMethod.currentValue === "pmvnorm"
			}

			Text
			{
				text: qsTr("t search range:")
				visible: test.currentValue.indexOf("TTest") !== -1
			}
			Text
			{
				text: qsTr("Range")
				visible: test.currentValue.indexOf("TTest") !== -1
			}
			DropDown
			{
				name: "tSearchRangeMode"
				id:   drangeMode
				info: qsTr("Choose whether the integration range for t-test calculations is selected automatically or supplied manually.")
				indexDefaultValue: 0
				visible: test.currentValue.indexOf("TTest") !== -1
				values: [
					{ label: qsTr("Adaptive"), value: "adaptive" },
					{ label: qsTr("Custom"),   value: "custom"   }
				]
			}

			Text
			{
				text: qsTr("Lower:")
				visible: test.currentValue.indexOf("TTest") !== -1 && drangeMode.currentValue === "custom"
			}
			Text
			{
				text: qsTr("min")
				visible: test.currentValue.indexOf("TTest") !== -1 && drangeMode.currentValue === "custom"
			}
			DoubleField
			{
				name: "tSearchRangeLower"
				id:   drangeLower
				info: qsTr("Lower bound of the custom integration range for t-test calculations.")
				defaultValue: -5
				negativeValues: true
				visible: test.currentValue.indexOf("TTest") !== -1 && drangeMode.currentValue === "custom"
			}

			Text
			{
				text: qsTr("Upper:")
				visible: test.currentValue.indexOf("TTest") !== -1 && drangeMode.currentValue === "custom"
			}
			Text
			{
				text: qsTr("max")
				visible: test.currentValue.indexOf("TTest") !== -1 && drangeMode.currentValue === "custom"
			}
			DoubleField
			{
				name: "tSearchRangeUpper"
				id:   drangeUpper
				info: qsTr("Upper bound of the custom integration range for t-test calculations.")
				defaultValue: 5
				negativeValues: true
				visible: test.currentValue.indexOf("TTest") !== -1 && drangeMode.currentValue === "custom"
			}

			Text { text: qsTr("Curve points:") }
			Text { text: qsTr("N") }
			IntegerField
			{
				name: "curvePoints"
				id:   plotPoints
				info: qsTr("Number of points used to draw probability and prior curves.")
				min: 10
				defaultValue: 100
			}
		}
	}
}

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
			info: qsTr("Select the statistical test for which the Bayes factor design is planned.")
			values: [
				{ label: qsTr("Independent Samples T-Test"), value: "independentSamplesTTest" },
				{ label: qsTr("Paired Samples T-Test"),      value: "pairedSamplesTTest"      },
				{ label: qsTr("One Sample T-Test"),          value: "oneSampleTTest"          },
				{ label: qsTr("Independent Samples Z-Test"), value: "independentSamplesZTest" },
				{ label: qsTr("Paired Samples Z-Test"),      value: "pairedSamplesZTest"      },
				{ label: qsTr("One Sample Z-Test"),          value: "oneSampleZTest"          },
				{ label: qsTr("One Sample Proportion Test"), value: "oneSampleProportion"     },
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
			info:  qsTr("Generate a short report describing the design, priors, thresholds, and achieved probabilities.")
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

		PowerComponents.BayesFactorCalculationControls
		{
			id: calculationControls
			testValue: test.currentValue
			designType: "fixed"
		}

		Group
		{
			title: qsTr("Sample Size")
			columns: 3

			Text
			{
				text: test.currentValue.indexOf("independentSamples") !== -1 ? qsTr("Sample size in group 1:") : qsTr("Sample size:")
				enabled: calculationControls.calculationValue !== "sampleSize"
			}
			Text
			{
				text: qsTr("N")
				enabled: calculationControls.calculationValue !== "sampleSize"
			}
			IntegerField
			{
				name: "sampleSize"
				id:   sampleSize
				info: qsTr("Fixed sample size at which the probability of conclusive, misleading, and inconclusive evidence is evaluated.")
				min: 2
				defaultValue: 100
				enabled: calculationControls.calculationValue !== "sampleSize"
			}

			Text
			{
				text: qsTr("Allocation ratio (N\u2082/N\u2081):")
				visible: test.currentValue.indexOf("independentSamples") !== -1
			}
			Text
			{
				text: "N\u2082/N\u2081"
				visible: test.currentValue.indexOf("independentSamples") !== -1
			}
			DoubleField
			{
				name: "sampleSizeAllocationRatio"
				id:   sampleSizeRatio
				info: qsTr("Ratio of the sample size in group 2 to the sample size in group 1.")
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue.indexOf("independentSamples") !== -1
			}
		}
	}

	PowerComponents.BayesFactorAnalysisPriorSection
	{
		testValue: test.currentValue
		supportsBinomial: true
	}

	PowerComponents.BayesFactorDesignPriorSection
	{
		testValue: test.currentValue
		supportsBinomial: true
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
				info:  qsTr("Show the main fixed-design result: the required sample size or the achieved probability of conclusive evidence.")
				checked: true
			}

			CheckBox
			{
				name: "decisionProbabilities"
				label: qsTr("Decision probabilities")
				info:  qsTr("Show probabilities of evidence for H\u2081, evidence for H\u2080, and inconclusive evidence under each design prior.")
				checked: true
			}

			CheckBox
			{
				name: "designSpecification"
				label: qsTr("Design specification")
				info:  qsTr("Show the analysis and design priors used for computing and evaluating the Bayes factor design.")
				checked: true
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
				label:  qsTr("Decision probabilities by effect size")
				info:   qsTr("Plot how conclusive, misleading, and inconclusive evidence probabilities change across design-prior effect sizes.")
				id:     decisionProbabilitiesByEffectSize
				name:   "decisionProbabilitiesByEffectSize"
				checked: false
			}

			CheckBox
			{
				label: qsTr("Decision probabilities by sample size")
				info:  qsTr("Plot how conclusive, misleading, and inconclusive evidence probabilities change as the fixed sample size increases.")
				id:    decisionProbabilitiesBySampleSize
				name:  "decisionProbabilitiesBySampleSize"
				checked: false

				CheckBox
				{
					label: qsTr("Log sample-size axis")
					info:  qsTr("Use a logarithmic x-axis to inspect changes across small and large sample sizes.")
					name:  "logSampleSizeAxis"
					checked: true
				}
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

		Group
		{
			columns: 3

			CheckBox
			{
				Layout.columnSpan: 3
				name: "generateRCode"
				id:   generateRCode
				label: qsTr("Generate R code")
				info:  qsTr("Generate R code that reproduces the Bayes factor design calculation.")
				checked: false
			}

			Text
			{
				text: qsTr("Minimum sample size:")
				visible: calculationControls.calculationValue === "sampleSize"
			}
			Text
			{
				text: qsTr("Nmin")
				visible: calculationControls.calculationValue === "sampleSize"
			}
			IntegerField
			{
				name: "minimumSampleSize"
				id:   sampleSizeRangeMin
				info: qsTr("Smallest sample size considered when searching for the target probability of conclusive evidence.")
				min: 1
				defaultValue: 10
				visible: calculationControls.calculationValue === "sampleSize"
			}

			Text
			{
				text: qsTr("Maximum sample size:")
				visible: calculationControls.calculationValue === "sampleSize"
			}
			Text
			{
				text: qsTr("Nmax")
				visible: calculationControls.calculationValue === "sampleSize"
			}
			IntegerField
			{
				name: "maximumSampleSize"
				id:   sampleSizeRangeMax
				info: qsTr("Largest sample size considered when searching for the target probability of conclusive evidence.")
				min: 2
				defaultValue: 10000
				visible: calculationControls.calculationValue === "sampleSize"
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

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
			label: qsTr("Explanatory text")
			id:    text
			name:  "explanatoryText"
			checked: true
		}

		CheckBox
		{
			label: qsTr("Generate report")
			id:    generateReport
			name:  "generateReport"
			checked: false

			CheckBox
			{
				label: qsTr("LaTeX formatted output")
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
				checked: true
			}

			CheckBox
			{
				name: "decisionProbabilities"
				label: qsTr("Decision probabilities")
				checked: true
			}

			CheckBox
			{
				name: "designSpecification"
				label: qsTr("Design specification")
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
				id:     decisionProbabilitiesByEffectSize
				name:   "decisionProbabilitiesByEffectSize"
				checked: false
			}

			CheckBox
			{
				label: qsTr("Decision probabilities by sample size")
				id:    decisionProbabilitiesBySampleSize
				name:  "decisionProbabilitiesBySampleSize"
				checked: false

				CheckBox
				{
					label: qsTr("Log sample-size axis")
					name:  "logSampleSizeAxis"
					checked: true
				}
			}

			CheckBox
			{
				name: "combineH1H0Figures"
				label: qsTr("Combine H\u2081 and H\u2080")
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
				label: qsTr("Generate R Code")
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
				min: 10
				defaultValue: 100
			}
		}
	}
}

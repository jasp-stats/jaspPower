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
		title: qsTr("Summary")

		Group
		{
			CheckBox
			{
				name: "summaryDesign"
				label: qsTr("Design")
				checked: true
			}

			CheckBox
			{
				name: "summarySampleSize"
				label: qsTr("Sample size")
				checked: true
			}

			CheckBox
			{
				name: "summaryEvidence"
				label: qsTr("Evidence")
				checked: true
			}

			CheckBox
			{
				name: "summarySpecification"
				label: qsTr("Specification")
				checked: true
			}
		}

		Group
		{
			title: qsTr("Stagewise")

			CheckBox
			{
				name: "stagewiseEvidence"
				label: qsTr("Evidence")
				checked: false
			}

			CheckBox
			{
				name: "stagewiseIncrementalEvidence"
				label: qsTr("Incremental evidence")
				checked: false
			}

			CheckBox
			{
				name: "stagewiseStoppingBoundaries"
				label: qsTr("Stopping boundaries")
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
			title: qsTr("Evidence")

			CheckBox
			{
				label: qsTr("Stopping probabilities")
				id:    stoppingProbabilitiesPlot
				name:  "stoppingProbabilities"
				checked: false
			}

			CheckBox
			{
				label: qsTr("Stopping boundaries")
				id:    stoppingBoundariesPlot
				name:  "stoppingBoundaries"
				checked: false
			}

			CheckBox
			{
				name: "mergeH1AndH0Figures"
				label: qsTr("Merge H\u2081 and H\u2080 figures")
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
				checked: false
			}

			Text { Layout.columnSpan: 2; text: qsTr("Exact integration over all regions:") }
			CheckBox
			{
				name: "exactIntegrationOverAllRegions"
				id:   strictIntegration
				label: ""
				checked: true
			}

			Text { text: qsTr("Integration method:") }
			Text { text: qsTr("Method") }
			DropDown
			{
				name: "integrationMethod"
				id:   integrationMethod
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

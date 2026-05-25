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

Section
{
	expanded: false
	title: qsTr("Observed Data Analysis")
	columns: 2

	property string testValue: ""

	RadioButtonGroup
	{
		Layout.columnSpan:      2
		name:                   "observedDataAnalysisInput"
		id:                     observedAnalysisInput
		radioButtonsOnSameRow:	true
		title:                  qsTr("Input")
		info:                   qsTr("Choose whether to enter observed data as summary statistics or select variables from the dataset.")

		RadioButton
		{
			id:      observedSummaryStatisticsInput
			value:   "summaryStatistics"
			label:   qsTr("Summary statistics")
			checked: true
		}

		RadioButton
		{
			id:    observedColumnInput
			value: "columns"
			label: qsTr("Dataset")
		}
	}

	Group
	{
		visible: observedAnalysisInput.value === "summaryStatistics"

		IntegerField
		{
			name: "observedSuccesses"
			label: qsTr("Successes")
			info:  qsTr("Number of observed successes used to compute the observed Bayes factor.")
			min: 0
			defaultValue: 0
			visible: testValue === "oneSampleProportion"
		}

		IntegerField
		{
			name: "observedFailures"
			label: qsTr("Failures")
			info:  qsTr("Number of observed failures used to compute the observed Bayes factor.")
			min: 0
			defaultValue: 0
			visible: testValue === "oneSampleProportion"
		}

		DoubleField
		{
			name: "observedEffectSize"
			label: qsTr("Effect size")
			info:  qsTr("Observed effect estimate on the scale selected for the general z-test approximation.")
			defaultValue: 0
			negativeValues: true
			visible: testValue === "generalZApproximation"
		}

		DoubleField
		{
			name: "observedStandardError"
			label: qsTr("SE")
			info:  qsTr("Standard error of the observed effect estimate.")
			min: 0
			defaultValue: 0
			inclusive: JASP.MinOnly
			visible: testValue === "generalZApproximation"
		}

		DoubleField
		{
			name: "observedT"
			label: qsTr("t")
			info:  qsTr("Observed t statistic used to compute the observed Bayes factor.")
			defaultValue: 0
			negativeValues: true
			visible: testValue.indexOf("TTest") !== -1 && observedInputType.value === "tAndN"
		}

		DoubleField
		{
			name: "observedCohensD"
			label: qsTr("Cohen's d")
			info:  qsTr("Observed standardized mean difference used to compute the observed Bayes factor.")
			defaultValue: 0
			negativeValues: true
			visible: testValue.indexOf("TTest") !== -1 && observedInputType.value === "cohensD"
		}

		Group
		{
			columns: 2
			visible: testValue === "independentSamplesZTest" || (testValue === "independentSamplesTTest" && observedInputType.value === "meansAndSDs")

			DoubleField
			{
				name: "observedMean1"
				label: qsTr("Mean 1")
				info:  qsTr("Observed mean in group 1.")
				defaultValue: 0
				negativeValues: true
			}

			DoubleField
			{
				name: "observedSd1"
				label: qsTr("SD 1")
				info:  qsTr("Observed standard deviation in group 1.")
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: testValue === "independentSamplesTTest"
			}
		}

		Group
		{
			columns: 2
			visible: testValue === "independentSamplesZTest" || (testValue === "independentSamplesTTest" && observedInputType.value === "meansAndSDs")

			DoubleField
			{
				name: "observedMean2"
				label: qsTr("Mean 2")
				info:  qsTr("Observed mean in group 2.")
				defaultValue: 0
				negativeValues: true
			}

			DoubleField
			{
				name: "observedSd2"
				label: qsTr("SD 2")
				info:  qsTr("Observed standard deviation in group 2.")
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: testValue === "independentSamplesTTest"
			}
		}

		Group
		{
			columns: 1
			visible: testValue === "oneSampleZTest" || testValue === "oneSampleTTest" || testValue === "pairedSamplesZTest" || testValue === "pairedSamplesTTest"

			DoubleField
			{
				name: "observedMean"
				label: qsTr("Mean")
				info:  qsTr("Observed mean used to compute the observed Bayes factor.")
				defaultValue: 0
				negativeValues: true
				visible: testValue === "oneSampleZTest" || (testValue === "oneSampleTTest" && observedInputType.value === "meanAndSD")
			}

			DoubleField
			{
				name: "observedMeanDifference"
				label: qsTr("Mean difference")
				info:  qsTr("Observed paired mean difference used to compute the observed Bayes factor.")
				defaultValue: 0
				negativeValues: true
				visible: testValue === "pairedSamplesZTest" || (testValue === "pairedSamplesTTest" && observedInputType.value === "meanDiffAndSD")
			}

			DoubleField
			{
				name: "observedSd"
				label: qsTr("SD")
				info:  qsTr("Observed standard deviation used to compute the observed Bayes factor.")
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: testValue === "oneSampleTTest" && observedInputType.value === "meanAndSD"
			}

			DoubleField
			{
				name: "observedSdDifference"
				label: qsTr("SD")
				info:  qsTr("Observed standard deviation of paired differences.")
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: testValue === "pairedSamplesTTest" && observedInputType.value === "meanDiffAndSD"
			}

			IntegerField
			{
				name: "observedSampleSize"
				label: qsTr("Sample size")
				info:  qsTr("Observed sample size used to compute the observed Bayes factor.")
				min: 0
				defaultValue: 0
			}
		}

		IntegerField
		{
			name: "observedSampleSizeGroup1"
			label: testValue === "independentSamplesTTest" ? qsTr("Sample size group 1") : qsTr("N\u2081")
			info:  qsTr("Observed sample size in group 1.")
			min: 0
			defaultValue: 0
			visible: testValue.indexOf("independentSamples") !== -1
		}

		IntegerField
		{
			name: "observedSampleSizeGroup2"
			label: testValue === "independentSamplesTTest" ? qsTr("Sample size group 2") : qsTr("N\u2082")
			info:  qsTr("Observed sample size in group 2.")
			min: 0
			defaultValue: 0
			visible: testValue.indexOf("independentSamples") !== -1
		}
	}

	RadioButtonGroup
	{
		name:    "observedInputType"
		id:      observedInputType
		title:   qsTr("Input Type")
		info:    qsTr("Choose which summary statistics are available for the observed t-test result.")
		visible: observedAnalysisInput.value === "summaryStatistics" && testValue.indexOf("TTest") !== -1

		RadioButton
		{
			value:   "tAndN"
			label:   testValue === "independentSamplesTTest" ? qsTr("t and Sample Sizes") : qsTr("t and Sample Size")
			checked: true
		}

		RadioButton
		{
			value: "cohensD"
			label: testValue === "independentSamplesTTest" ? qsTr("Cohen's d and Sample Sizes") : qsTr("Cohen's d and Sample Size")
		}

		RadioButton
		{
			value:   "meansAndSDs"
			label:   qsTr("Means, SDs, and Sample Sizes")
			visible: testValue === "independentSamplesTTest"
		}

		RadioButton
		{
			value:   "meanDiffAndSD"
			label:   qsTr("Mean Diff., SD, and Sample Size")
			visible: testValue === "pairedSamplesTTest"
		}

		RadioButton
		{
			value:   "meanAndSD"
			label:   qsTr("Mean, SD, and Sample Size")
			visible: testValue === "oneSampleTTest"
		}
	}

	VariablesForm
	{
		Layout.columnSpan: 2
		visible: observedAnalysisInput.value === "columns"

		AvailableVariablesList
		{
			name: "observedAvailableVariables"
			info: qsTr("Dataset columns available for the observed-data Bayes factor analysis.")
		}

		AssignedVariablesList
		{
			name: "observedVariable"
			label: qsTr("Variable")
			info:  qsTr("Observed scale variable for the one-sample or general z-test analysis.")
			allowedColumns: ["scale"]
			singleVariable: true
			visible: (testValue.indexOf("oneSample") !== -1 && testValue !== "oneSampleProportion") || testValue === "generalZApproximation"
		}

		AssignedPairsVariablesList
		{
			name: "observedVariablePairs"
			title: qsTr("Variable Pairs")
			info:  qsTr("Observed paired variables used to compute the paired-samples Bayes factor.")
			allowedColumns: ["scale"]
			minNumericLevels: 2
			maxRows: 1
			visible: testValue.indexOf("pairedSamples") !== -1
		}

		AssignedVariablesList
		{
			name: "observedDependentVariable"
			label: qsTr("Dependent Variable")
			info:  qsTr("Observed outcome variable for the independent-samples Bayes factor.")
			allowedColumns: ["scale"]
			singleVariable: true
			visible: testValue.indexOf("independentSamples") !== -1
		}

		AssignedVariablesList
		{
			name: "observedGroupingVariable"
			label: qsTr("Grouping Variable")
			info:  qsTr("Grouping variable that defines the two independent samples.")
			allowedColumns: ["nominal", "ordinal", "scale"]
			singleVariable: true
			visible: testValue.indexOf("independentSamples") !== -1
		}

		AssignedVariablesList
		{
			name: "observedProportionVariable"
			label: qsTr("Variable")
			info:  qsTr("Observed variable containing success and failure values for the binomial test.")
			allowedColumns: ["nominal", "ordinal", "scale"]
			singleVariable: true
			visible: testValue === "oneSampleProportion"
		}
	}

	TextField
	{
		Layout.columnSpan: 2
		name: "observedSuccessValue"
		label: qsTr("Success value")
		info:  qsTr("Value in the selected variable that is counted as a success.")
		defaultValue: "1"
		visible: observedAnalysisInput.value === "columns" && testValue === "oneSampleProportion"
	}
}

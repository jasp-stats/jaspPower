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

Form
{
	DropDown
	{
		name: "test"
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

	Section
	{
		expanded: true
		title: qsTr("Parameters")
		columns: 1

		Group
		{
			columns: 3

			Text { Layout.columnSpan: 2; text: qsTr("Calculation:") }
			DropDown
			{
				name: "calculation"
				id:   calc
				indexDefaultValue: 0
				label: ""
				values: [
					{ label: qsTr("Evidence Probability"), value: "evidenceProbability" },
					{ label: qsTr("Maximum Sample Size"),  value: "sampleSize"          }
				]
			}

			Text { text: qsTr("Evidence for H\u2081:") }
			Text { text: "BF\u2081\u2080 \u2265" }
			DoubleField
			{
				name: "bf10Threshold"
				id:   bf10Threshold
				min: 1
				defaultValue: 10
				inclusive: JASP.None
			}

			Text { text: qsTr("Evidence for H\u2080:") }
			Text { text: "BF\u2080\u2081 \u2265" }
			DoubleField
			{
				name: "bf01Threshold"
				id:   bf01Threshold
				min: 1
				defaultValue: 10
				inclusive: JASP.None
			}

			Text
			{
				Layout.columnSpan: 2
				text: qsTr("Find maximum sample size for:")
				visible: calc.currentValue === "sampleSize"
			}
			DropDown
			{
				name: "evidenceTarget"
				id:   evidenceTarget
				indexDefaultValue: 0
				label: ""
				visible: calc.currentValue === "sampleSize"
				values: [
					{ label: qsTr("H\u2081 (BF\u2081\u2080)"), value: "h1" },
					{ label: qsTr("H\u2080 (BF\u2080\u2081)"), value: "h0" }
				]
			}

			Text
			{
				text: qsTr("Minimal desired evidence probability:")
				visible: calc.currentValue === "sampleSize"
			}
			Text
			{
				text: evidenceTarget.currentValue === "h1" ? "Pr(BF\u2081\u2080 \u2265 k)" : "Pr(BF\u2080\u2081 \u2265 k)"
				visible: calc.currentValue === "sampleSize"
			}
			DoubleField
			{
				name: "evidenceProbability"
				id:   evidenceProbability
				min: 0
				max: 1
				defaultValue: 0.9
				inclusive: JASP.None
				visible: calc.currentValue === "sampleSize"
			}

			Text
			{
				text: qsTr("Alternative Hypothesis:")
			}
			Text
			{
				text: qsTr("H\u2081")
			}
			DropDown
			{
				name: "alternative"
				id:   alternative
				indexDefaultValue: 0
				values: [
					{ label: qsTr("Two-sided"),           value: "twoSided" },
					{ label: qsTr("Less (One-sided)"),    value: "less"     },
					{ label: qsTr("Greater (One-sided)"), value: "greater"  }
				]
			}

			Text
			{
				text: qsTr("Look schedule:")
				visible: !(calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			Text
			{
				text: qsTr("Type")
				visible: !(calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			DropDown
			{
				name: "lookScheduleMode"
				id:   lookScheduleMode
				indexDefaultValue: 0
				visible: !(calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
				values: [
					{ label: qsTr("Equally spaced"), value: "even"   },
					{ label: qsTr("Custom"),         value: "custom" }
				]
			}

			Text
			{
				text: qsTr("Number of looks:")
				visible: lookScheduleMode.currentValue === "even" && !(calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			Text
			{
				text: qsTr("K")
				visible: lookScheduleMode.currentValue === "even" && !(calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			IntegerField
			{
				name: "numberOfLooks"
				id:   numberOfLooks
				min: 1
				defaultValue: 5
				visible: lookScheduleMode.currentValue === "even" && !(calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}

			Text
			{
				text: qsTr("First information fraction:")
				visible: calc.currentValue === "sampleSize" && lookScheduleMode.currentValue === "even"
			}
			Text
			{
				text: qsTr("I1/Imax")
				visible: calc.currentValue === "sampleSize" && lookScheduleMode.currentValue === "even"
			}
			DoubleField
			{
				name: "informationFractionFirstLook"
				id:   informationFractionFirstLook
				min: 0
				max: 1
				defaultValue: 0.2
				inclusive: JASP.None
				visible: calc.currentValue === "sampleSize" && lookScheduleMode.currentValue === "even"
			}

			Text
			{
				text: test.currentValue.indexOf("independentSamples") !== -1 ? qsTr("Sample size at first look per group:") : qsTr("Sample size at first look:")
				visible: calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "even" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			Text
			{
				text: "N\u2081"
				visible: calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "even" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			IntegerField
			{
				name: "sampleSizeFirstLook"
				id:   sampleSizeFirstLook
				min: 2
				defaultValue: 20
				visible: calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "even" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}

			Text
			{
				text: test.currentValue.indexOf("independentSamples") !== -1 ? qsTr("Maximum sample size per group:") : qsTr("Maximum sample size:")
				visible: calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "even" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			Text
			{
				text: "N"
				visible: calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "even" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			IntegerField
			{
				name: "sampleSize"
				id:   sampleSize
				min: 2
				defaultValue: 100
				visible: calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "even" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}

			Text
			{
				text: test.currentValue.indexOf("independentSamples") !== -1 ? qsTr("Group 1 sample size schedule:") : qsTr("Sample size schedule:")
				visible: calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "custom" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			Text
			{
				text: test.currentValue.indexOf("independentSamples") !== -1 ? "N\u2081" : "N"
				visible: calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "custom" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			TextField
			{
				name: "sampleSizeSchedule"
				id:   sampleSizeSchedule
				defaultValue: "20, 40, 60, 80, 100"
				fieldWidth: 140
				visible: calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "custom" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}

			Text
			{
				text: qsTr("Group 2 sample size schedule:")
				visible: test.currentValue.indexOf("independentSamples") !== -1 && calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "custom" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			Text
			{
				text: "N\u2082"
				visible: test.currentValue.indexOf("independentSamples") !== -1 && calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "custom" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			TextField
			{
				name: "sampleSizeSecondGroupSchedule"
				id:   sampleSizeSecondGroupSchedule
				defaultValue: "20, 40, 60, 80, 100"
				fieldWidth: 140
				visible: test.currentValue.indexOf("independentSamples") !== -1 && calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "custom" && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}

			Text
			{
				text: qsTr("Information fractions:")
				visible: calc.currentValue === "sampleSize" && lookScheduleMode.currentValue === "custom"
			}
			Text
			{
				text: qsTr("I/Imax")
				visible: calc.currentValue === "sampleSize" && lookScheduleMode.currentValue === "custom"
			}
			TextField
			{
				name: "informationFractionSchedule"
				id:   informationFractionSchedule
				defaultValue: "0.2, 0.4, 0.6, 0.8, 1"
				fieldWidth: 140
				visible: calc.currentValue === "sampleSize" && lookScheduleMode.currentValue === "custom"
			}

			Text
			{
				text: qsTr("Sample size ratio:")
				visible: test.currentValue.indexOf("independentSamples") !== -1 && (calc.currentValue === "sampleSize" || lookScheduleMode.currentValue === "even")
			}
			Text
			{
				text: "N\u2082/N\u2081"
				visible: test.currentValue.indexOf("independentSamples") !== -1 && (calc.currentValue === "sampleSize" || lookScheduleMode.currentValue === "even")
			}
			DoubleField
			{
				name: "sampleSizeRatio"
				id:   sampleSizeRatio
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue.indexOf("independentSamples") !== -1 && (calc.currentValue === "sampleSize" || lookScheduleMode.currentValue === "even")
			}

			Text
			{
				text: qsTr("Search lower bound:")
				visible: calc.currentValue === "sampleSize"
			}
			Text
			{
				text: "Nmax,min"
				visible: calc.currentValue === "sampleSize"
			}
			IntegerField
			{
				name: "sampleSizeRangeMin"
				id:   sampleSizeRangeMin
				min: 2
				defaultValue: 20
				visible: calc.currentValue === "sampleSize"
			}

			Text
			{
				text: qsTr("Search upper bound:")
				visible: calc.currentValue === "sampleSize"
			}
			Text
			{
				text: "Nmax,max"
				visible: calc.currentValue === "sampleSize"
			}
			IntegerField
			{
				name: "sampleSizeRangeMax"
				id:   sampleSizeRangeMax
				min: 2
				defaultValue: 500
				visible: calc.currentValue === "sampleSize"
			}

			Text
			{
				text: qsTr("Known standard deviation:")
				visible: test.currentValue.indexOf("ZTest") !== -1
			}
			Text
			{
				text: "\u03C3"
				visible: test.currentValue.indexOf("ZTest") !== -1
			}
			DoubleField
			{
				name: "standardDeviation"
				id:   standardDeviation
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue.indexOf("ZTest") !== -1
			}

			Text
			{
				text: qsTr("Parameterization:")
				visible: test.currentValue === "generalZApproximation"
			}
			Text
			{
				text: qsTr("Scale")
				visible: test.currentValue === "generalZApproximation"
			}
			DropDown
			{
				name: "generalZParameterization"
				id:   generalZParameterization
				indexDefaultValue: 0
				visible: test.currentValue === "generalZApproximation"
				values: calc.currentValue === "sampleSize" ?
				[
					{ label: qsTr("Effect size"), value: "effectSize" },
					{ label: qsTr("Unit information SD"), value: "unitInformationSd" }
				] :
				[
					{ label: qsTr("Effect size"), value: "effectSize" },
					{ label: qsTr("Unit information SD"), value: "unitInformationSd" },
					{ label: qsTr("Standard error schedule"), value: "standardErrorSchedule" }
				]
			}

			Text
			{
				text: qsTr("Unit information SD:")
				visible: test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "unitInformationSd"
			}
			Text
			{
				text: qsTr("UISD")
				visible: test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "unitInformationSd"
			}
			DoubleField
			{
				name: "unitInformationSd"
				id:   unitInformationSd
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "unitInformationSd"
			}

			Text
			{
				text: qsTr("Standard error schedule:")
				visible: calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule"
			}
			Text
			{
				text: qsTr("SE")
				visible: calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule"
			}
			TextField
			{
				name: "standardErrorSchedule"
				id:   standardErrorSchedule
				defaultValue: "0.224, 0.158, 0.129, 0.112, 0.100"
				fieldWidth: 140
				visible: calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule"
			}
		}
	}

	Section
	{
		expanded: true
		title: qsTr("Analysis Prior")
		columns: 1

		Group
		{
			title: qsTr("Prior Under H\u2080")
			columns: 3

			Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
			DropDown
			{
				name: "nullPriorDistribution"
				id:   nullPriorDistribution
				indexDefaultValue: 0
				label: ""
				enabled: false
				values: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && alternative.currentValue !== "twoSided" ?
				[
					{ label: alternative.currentValue === "less" ? qsTr("Directional (\u03B8 \u2265 \u03B8\u2080)") : qsTr("Directional (\u03B8 \u2264 \u03B8\u2080)"), value: "directional" }
				] :
				[
					{ label: qsTr("Point null (\u03B8 = \u03B8\u2080)"), value: "point" }
				]
			}

			Text { text: qsTr("Null value:") }
			Text { text: "\u03B8\u2080" }
			DoubleField
			{
				name: "nullValue"
				id:   nullValue
				defaultValue: 0
			}
		}

		Group
		{
			title: qsTr("Prior Under H\u2081")
			columns: 3

			Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
			DropDown
			{
				name: "analysisPriorDistribution"
				id:   analysisPriorDistribution
				indexDefaultValue: 0
				label: ""
				values: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && alternative.currentValue !== "twoSided" ?
				[
					{ label: qsTr("Normal"), value: "normal" }
				] :
				(test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation" ?
				[
					{ label: qsTr("Normal"),                 value: "normal"             },
					{ label: qsTr("Point"),                  value: "point"              },
					{ label: qsTr("Normal-moment (mode)"),   value: "normalMomentMode"   },
					{ label: qsTr("Normal-moment (spread)"), value: "normalMomentSpread" }
				] :
				[
					{ label: qsTr("Student-t"), value: "t" }
				])
			}

			Text
			{
				text: qsTr("Prior point:")
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "point"
			}
			Text
			{
				text: "\u03B8\u2081"
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "point"
			}
			DoubleField
			{
				name: "analysisPriorPoint"
				id:   analysisPriorPoint
				defaultValue: 0.5
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "point"
			}

			Text
			{
				text: qsTr("Prior mean:")
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normal"
			}
			Text
			{
				text: "\u03BC"
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normal"
			}
			DoubleField
			{
				name: "analysisPriorMean"
				id:   analysisPriorMean
				defaultValue: 0
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normal"
			}

			Text
			{
				text: qsTr("Prior scale:")
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normal"
			}
			Text
			{
				text: "\u03C3"
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normal"
			}
			DoubleField
			{
				name: "analysisPriorSd"
				id:   analysisPriorSd
				min: 0
				defaultValue: 0.707
				inclusive: JASP.None
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normal"
			}

			Text
			{
				text: qsTr("Prior spread:")
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normalMomentSpread"
			}
			Text
			{
				text: "\u03C4"
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normalMomentSpread"
			}
			DoubleField
			{
				name: "momentPriorSpread"
				id:   momentPriorSpread
				min: 0
				defaultValue: 0.707
				inclusive: JASP.None
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normalMomentSpread"
			}

			Text
			{
				text: qsTr("Prior mode:")
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normalMomentMode"
			}
			Text
			{
				text: "|m|"
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normalMomentMode"
			}
			DoubleField
			{
				name: "momentPriorMode"
				id:   momentPriorMode
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normalMomentMode"
			}

			Text
			{
				Layout.columnSpan: 2
				text: qsTr("Prior modes:")
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normalMomentSpread"
			}
			Text
			{
				text: "\u00B1\u221A2\u03C4"
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normalMomentSpread"
			}

			Text
			{
				Layout.columnSpan: 2
				text: qsTr("Back-computed spread:")
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normalMomentMode"
			}
			Text
			{
				text: "\u03C4 = |m|/\u221A2"
				visible: (test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation") && analysisPriorDistribution.currentValue === "normalMomentMode"
			}

			Text
			{
				text: qsTr("Prior location:")
				visible: test.currentValue.indexOf("TTest") !== -1
			}
			Text
			{
				text: "\u03BC"
				visible: test.currentValue.indexOf("TTest") !== -1
			}
			DoubleField
			{
				name: "tPriorLocation"
				id:   tPriorLocation
				defaultValue: 0
				visible: test.currentValue.indexOf("TTest") !== -1
			}

			Text
			{
				text: qsTr("Prior scale:")
				visible: test.currentValue.indexOf("TTest") !== -1
			}
			Text
			{
				text: "\u03C3"
				visible: test.currentValue.indexOf("TTest") !== -1
			}
			DoubleField
			{
				name: "tPriorScale"
				id:   tPriorScale
				min: 0
				defaultValue: 0.707
				inclusive: JASP.None
				visible: test.currentValue.indexOf("TTest") !== -1
			}

			Text
			{
				text: qsTr("Prior degrees of freedom:")
				visible: test.currentValue.indexOf("TTest") !== -1
			}
			Text
			{
				text: "df"
				visible: test.currentValue.indexOf("TTest") !== -1
			}
			DoubleField
			{
				name: "tPriorDf"
				id:   tPriorDf
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue.indexOf("TTest") !== -1
			}
		}
	}

	Section
	{
		expanded: true
		title: qsTr("Design Prior")
		columns: 1

		Group
		{
			columns: 3

			Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
			DropDown
			{
				name: "designPrior"
				id:   designPrior
				indexDefaultValue: 0
				label: ""
				values: [
					{ label: qsTr("Point"),  value: "point"  },
					{ label: qsTr("Normal"), value: "normal" }
				]
			}

			Text { text: qsTr("Mean:") }
			Text { text: "\u03BC" }
			DoubleField
			{
				name: "designPriorMean"
				id:   designPriorMean
				defaultValue: 0.5
			}

			Text
			{
				text: qsTr("Standard deviation:")
				enabled: designPrior.currentValue === "normal"
			}
			Text
			{
				text: "\u03C3"
				enabled: designPrior.currentValue === "normal"
			}
			DoubleField
			{
				name: "designPriorSd"
				id:   designPriorSd
				min: 0
				defaultValue: 0.1
				inclusive: JASP.None
				enabled: designPrior.currentValue === "normal"
			}
		}
	}

	Section
	{
		expanded: true
		title: qsTr("Plots")

		CheckBox
		{
			label: qsTr("Stopping probabilities")
			id:    stoppingProbabilitiesPlot
			name:  "stoppingProbabilitiesPlot"
			checked: true
		}

		CheckBox
		{
			label: qsTr("Stopping boundaries")
			id:    stoppingBoundariesPlot
			name:  "stoppingBoundariesPlot"
			checked: true
		}

		CheckBox
		{
			label: qsTr("Show H\u2080 reference")
			id:    showNullReference
			name:  "showNullReference"
			checked: true
		}

		CheckBox
		{
			label: qsTr("Prior distribution")
			id:    priorDistribution
			name:  "priorDistribution"
			checked: true
		}

		CheckBox
		{
			label: qsTr("Explanatory text")
			id:    text
			name:  "text"
			checked: true
		}
	}

	Section
	{
		expanded: false
		title: qsTr("Advanced Options")
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

			Text { Layout.columnSpan: 2; text: qsTr("Exact integration over all regions:") }
			CheckBox
			{
				name: "strictIntegration"
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
				name: "integrationAbsEps"
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
				name: "integrationRelEps"
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
				name: "integrationMaxPts"
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
				name: "drangeMode"
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
				name: "drangeLower"
				id:   drangeLower
				defaultValue: -5
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
				name: "drangeUpper"
				id:   drangeUpper
				defaultValue: 5
				visible: test.currentValue.indexOf("TTest") !== -1 && drangeMode.currentValue === "custom"
			}

			Text { text: qsTr("Curve points:") }
			Text { text: qsTr("N") }
			IntegerField
			{
				name: "plotPoints"
				id:   plotPoints
				min: 10
				defaultValue: 100
			}
		}
	}
}

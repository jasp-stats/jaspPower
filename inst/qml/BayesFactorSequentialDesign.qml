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

	Group
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

		CheckBox
		{
			label: qsTr("Explanatory text")
			id:    text
			name:  "text"
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
				name:  "generateReportLatex"
				checked: false
			}
		}
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
					{ label: qsTr("Conclusive evidence"),  value: "evidenceProbability" },
					{ label: qsTr("Maximum Sample Size"),  value: "sampleSize"          }
				]
			}

			Text { text: qsTr("Bayes factor for H\u2081:") }
			Text { text: "BF\u2081\u2080 \u2265" }
			DoubleField
			{
				name: "bf10Threshold"
				id:   bf10Threshold
				min: 1
				defaultValue: 10
				inclusive: JASP.None
			}

			Text { text: qsTr("Bayes factor for H\u2080:") }
			Text { text: "BF\u2080\u2081 \u2265" }
			DoubleField
			{
				name: "bf01Threshold"
				id:   bf01Threshold
				min: 1
				defaultValue: 10
				inclusive: JASP.None
			}

			Text { text: qsTr("Conclusive evidence for H\u2081:"); visible: calc.currentValue === "sampleSize" }
			Text { text: "Pr(BF\u2081\u2080 \u2265 k)"; visible: calc.currentValue === "sampleSize" }
			DoubleField
			{
				name: "targetPowerH1"
				id:   targetPowerH1
				min: 0
				max: 1
				defaultValue: 0.9
				inclusive: JASP.None
				visible: calc.currentValue === "sampleSize"
			}

			Text { text: qsTr("Conclusive evidence for H\u2080:"); visible: calc.currentValue === "sampleSize" }
			Text { text: "Pr(BF\u2080\u2081 \u2265 k)"; visible: calc.currentValue === "sampleSize" }
			DoubleField
			{
				name: "targetPowerH0"
				id:   targetPowerH0
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
					{ label: qsTr("Equally spaced"),        value: "even"     },
					{ label: qsTr("Sample size increase"),  value: "increase" },
					{ label: qsTr("Custom"),                value: "custom"   }
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
				visible: (lookScheduleMode.currentValue === "increase" || (calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "even")) && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			Text
			{
				text: "N\u2081"
				visible: (lookScheduleMode.currentValue === "increase" || (calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "even")) && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			IntegerField
			{
				name: "sampleSizeFirstLook"
				id:   sampleSizeFirstLook
				min: 2
				defaultValue: 20
				visible: (lookScheduleMode.currentValue === "increase" || (calc.currentValue === "evidenceProbability" && lookScheduleMode.currentValue === "even")) && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}

			Text
			{
				text: test.currentValue.indexOf("independentSamples") !== -1 ? qsTr("Sample size increase per look per group:") : qsTr("Sample size increase per look:")
				visible: lookScheduleMode.currentValue === "increase" && !(calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			Text
			{
				text: "\u0394N"
				visible: lookScheduleMode.currentValue === "increase" && !(calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			IntegerField
			{
				name: "sampleSizeIncrease"
				id:   sampleSizeIncrease
				min: 1
				defaultValue: 20
				visible: lookScheduleMode.currentValue === "increase" && !(calc.currentValue === "evidenceProbability" && test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}

			Text
			{
				text: test.currentValue.indexOf("independentSamples") !== -1 ? qsTr("Maximum sample size per group:") : qsTr("Maximum sample size:")
				visible: calc.currentValue === "evidenceProbability" && (lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase") && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			Text
			{
				text: "N"
				visible: calc.currentValue === "evidenceProbability" && (lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase") && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
			}
			IntegerField
			{
				name: "sampleSize"
				id:   sampleSize
				min: 2
				defaultValue: 100
				visible: calc.currentValue === "evidenceProbability" && (lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase") && !(test.currentValue === "generalZApproximation" && generalZParameterization.currentValue === "standardErrorSchedule")
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
				visible: test.currentValue.indexOf("independentSamples") !== -1 && (calc.currentValue === "sampleSize" || lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase")
			}
			Text
			{
				text: "N\u2082/N\u2081"
				visible: test.currentValue.indexOf("independentSamples") !== -1 && (calc.currentValue === "sampleSize" || lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase")
			}
			DoubleField
			{
				name: "sampleSizeRatio"
				id:   sampleSizeRatio
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue.indexOf("independentSamples") !== -1 && (calc.currentValue === "sampleSize" || lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase")
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
			DropDown
			{
				name: "generalZParameterization"
				id:   generalZParameterization
				indexDefaultValue: 0
				Layout.columnSpan: 2
				visible: test.currentValue === "generalZApproximation"
				values: calc.currentValue === "sampleSize" ?
				[
					{ label: qsTr("SMD"),        value: "standardizedMeanDifference" },
					{ label: qsTr("Fisher's z"), value: "fisherZCorrelation"        },
					{ label: qsTr("logOR"),      value: "logOddsRatio"              },
					{ label: qsTr("logRR"),      value: "logRiskRatio"              },
					{ label: qsTr("logHR"),      value: "logHazardRatio"            },
					{ label: qsTr("logIRR"),     value: "logIncidenceRateRatio"     },
					{ label: qsTr("Specify"),    value: "unitInformationSd"         }
				] :
				[
					{ label: qsTr("SMD"),                     value: "standardizedMeanDifference" },
					{ label: qsTr("Fisher's z"),              value: "fisherZCorrelation"        },
					{ label: qsTr("logOR"),                   value: "logOddsRatio"              },
					{ label: qsTr("logRR"),                   value: "logRiskRatio"              },
					{ label: qsTr("logHR"),                   value: "logHazardRatio"            },
					{ label: qsTr("logIRR"),                  value: "logIncidenceRateRatio"     },
					{ label: qsTr("Specify"),                 value: "unitInformationSd"         },
					{ label: qsTr("Standard error schedule"), value: "standardErrorSchedule"     }
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
				negativeValues: true
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
					{ label: qsTr("Cauchy"),    value: "cauchy" },
					{ label: qsTr("Student-t"), value: "t"      }
				])
			}

			Text
			{
				text: qsTr("Prior location:")
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
				negativeValues: true
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
				negativeValues: true
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
				negativeValues: true
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
				visible: test.currentValue.indexOf("TTest") !== -1 && analysisPriorDistribution.currentValue === "t"
			}
			Text
			{
				text: "df"
				visible: test.currentValue.indexOf("TTest") !== -1 && analysisPriorDistribution.currentValue === "t"
			}
			DoubleField
			{
				name: "tPriorDf"
				id:   tPriorDf
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue.indexOf("TTest") !== -1 && analysisPriorDistribution.currentValue === "t"
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
			title: qsTr("Prior Under H\u2080")
			columns: 3

			Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
			DropDown
			{
				name: "designNullPrior"
				id:   designNullPrior
				indexDefaultValue: 0
				label: ""
				values: [
					{ label: qsTr("Point"),  value: "point"  },
					{ label: qsTr("Normal"), value: "normal" }
				]
			}

			Text { text: designNullPrior.currentValue === "point" ? qsTr("Location:") : qsTr("Mean:") }
			Text { text: "\u03BC" }
			DoubleField
			{
				name: "designNullPriorMean"
				id:   designNullPriorMean
				defaultValue: 0
				negativeValues: true
			}

			Text
			{
				text: qsTr("Standard deviation:")
				visible: designNullPrior.currentValue === "normal"
			}
			Text
			{
				text: "\u03C3"
				visible: designNullPrior.currentValue === "normal"
			}
			DoubleField
			{
				name: "designNullPriorSd"
				id:   designNullPriorSd
				min: 0
				defaultValue: 0.1
				inclusive: JASP.None
				visible: designNullPrior.currentValue === "normal"
			}
		}

		Group
		{
			title: qsTr("Prior Under H\u2081")
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

			Text { text: designPrior.currentValue === "point" ? qsTr("Location:") : qsTr("Mean:") }
			Text { text: "\u03BC" }
			DoubleField
			{
				name: "designPriorMean"
				id:   designPriorMean
				defaultValue: 0.5
				negativeValues: true
			}

			Text
			{
				text: qsTr("Standard deviation:")
				visible: designPrior.currentValue === "normal"
			}
			Text
			{
				text: "\u03C3"
				visible: designPrior.currentValue === "normal"
			}
			DoubleField
			{
				name: "designPriorSd"
				id:   designPriorSd
				min: 0
				defaultValue: 0.1
				inclusive: JASP.None
				visible: designPrior.currentValue === "normal"
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

			CheckBox
			{
				label: qsTr("Design")
				name:  "priorDistributionDesign"
				checked: true
			}

			CheckBox
			{
				label: qsTr("Analysis")
				name:  "priorDistributionAnalysis"
				checked: true
			}

			CheckBox
			{
				label: qsTr("Merge figures")
				name:  "priorDistributionMerge"
				checked: false
			}
		}

	}

	Section
	{
		expanded: false
		title: qsTr("Analysis")
		columns: 1

		RadioButtonGroup
		{
			name:  "observedAnalysisInput"
			id:    observedAnalysisInput
			title: qsTr("Input")

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
				label: qsTr("Columns")
			}
		}

		Group
		{
			title: qsTr("Summary Statistics")
			columns: 2
			visible: observedAnalysisInput.value === "summaryStatistics"

			RadioButtonGroup
			{
				name:  "observedInputType"
				id:    observedInputType
				title: qsTr("Input Type")
				Layout.columnSpan: 2
				visible: test.currentValue.indexOf("TTest") !== -1

				RadioButton
				{
					value:   "tAndN"
					label:   test.currentValue === "independentSamplesTTest" ? qsTr("t and Sample Sizes") : qsTr("t and Sample Size")
					checked: true
				}

				RadioButton
				{
					value: "cohensD"
					label: test.currentValue === "independentSamplesTTest" ? qsTr("Cohen's d and Sample Sizes") : qsTr("Cohen's d and Sample Size")
				}

				RadioButton
				{
					value:   "meansAndSDs"
					label:   qsTr("Means, SDs, and Sample Sizes")
					visible: test.currentValue === "independentSamplesTTest"
				}

				RadioButton
				{
					value:   "meanDiffAndSD"
					label:   qsTr("Mean Diff., SD, and Sample Size")
					visible: test.currentValue === "pairedSamplesTTest"
				}

				RadioButton
				{
					value:   "meanAndSD"
					label:   qsTr("Mean, SD, and Sample Size")
					visible: test.currentValue === "oneSampleTTest"
				}
			}

			DoubleField
			{
				name: "observedEstimate"
				label: qsTr("Estimate")
				defaultValue: 0
				visible: test.currentValue === "generalZApproximation"
			}

			DoubleField
			{
				name: "observedStandardError"
				label: qsTr("Standard error")
				min: 0
				defaultValue: 0
				inclusive: JASP.MinOnly
				visible: test.currentValue === "generalZApproximation"
			}

			DoubleField
			{
				name: "observedTStatistic"
				label: qsTr("t")
				defaultValue: 0
				negativeValues: true
				visible: test.currentValue.indexOf("TTest") !== -1 && observedInputType.value === "tAndN"
			}

			DoubleField
			{
				name: "observedCohensD"
				label: qsTr("Cohen's d")
				defaultValue: 0
				negativeValues: true
				visible: test.currentValue.indexOf("TTest") !== -1 && observedInputType.value === "cohensD"
			}

			IntegerField
			{
				name: "observedN1"
				label: test.currentValue === "independentSamplesTTest" ? qsTr("Sample size group 1") : qsTr("N\u2081")
				min: 0
				defaultValue: 0
				visible: test.currentValue.indexOf("independentSamples") !== -1
			}

			IntegerField
			{
				name: "observedN2"
				label: test.currentValue === "independentSamplesTTest" ? qsTr("Sample size group 2") : qsTr("N\u2082")
				min: 0
				defaultValue: 0
				visible: test.currentValue.indexOf("independentSamples") !== -1
			}

			DoubleField
			{
				name: "observedMean1"
				label: qsTr("Mean 1")
				defaultValue: 0
				negativeValues: true
				visible: test.currentValue === "independentSamplesZTest" || (test.currentValue === "independentSamplesTTest" && observedInputType.value === "meansAndSDs")
			}

			DoubleField
			{
				name: "observedMean2"
				label: qsTr("Mean 2")
				defaultValue: 0
				negativeValues: true
				visible: test.currentValue === "independentSamplesZTest" || (test.currentValue === "independentSamplesTTest" && observedInputType.value === "meansAndSDs")
			}

			DoubleField
			{
				name: "observedSd1"
				label: qsTr("SD 1")
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue === "independentSamplesTTest" && observedInputType.value === "meansAndSDs"
			}

			DoubleField
			{
				name: "observedSd2"
				label: qsTr("SD 2")
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue === "independentSamplesTTest" && observedInputType.value === "meansAndSDs"
			}

			IntegerField
			{
				name: "observedN"
				label: test.currentValue.indexOf("TTest") !== -1 ? qsTr("Sample size") : qsTr("N")
				min: 0
				defaultValue: 0
				visible: test.currentValue.indexOf("independentSamples") === -1 && test.currentValue !== "generalZApproximation"
			}

			DoubleField
			{
				name: "observedMean"
				label: qsTr("Mean")
				defaultValue: 0
				negativeValues: true
				visible: test.currentValue === "oneSampleZTest" || (test.currentValue === "oneSampleTTest" && observedInputType.value === "meanAndSD")
			}

			DoubleField
			{
				name: "observedSd"
				label: qsTr("SD")
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue === "oneSampleTTest" && observedInputType.value === "meanAndSD"
			}

			DoubleField
			{
				name: "observedMeanDifference"
				label: qsTr("Mean difference")
				defaultValue: 0
				negativeValues: true
				visible: test.currentValue === "pairedSamplesZTest" || (test.currentValue === "pairedSamplesTTest" && observedInputType.value === "meanDiffAndSD")
			}

			DoubleField
			{
				name: "observedSdDifference"
				label: qsTr("SD difference")
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue === "pairedSamplesTTest" && observedInputType.value === "meanDiffAndSD"
			}
		}

		VariablesForm
		{
			visible: observedAnalysisInput.value === "columns"

			AvailableVariablesList
			{
				name: "observedAvailableVariables"
			}

			AssignedVariablesList
			{
				name: "observedVariable"
				label: qsTr("Variable")
				allowedColumns: ["scale"]
				singleVariable: true
				visible: test.currentValue.indexOf("oneSample") !== -1 || test.currentValue === "generalZApproximation"
			}

			AssignedVariablesList
			{
				name: "observedFirstVariable"
				label: qsTr("First Variable")
				allowedColumns: ["scale"]
				singleVariable: true
				visible: test.currentValue.indexOf("pairedSamples") !== -1
			}

			AssignedVariablesList
			{
				name: "observedSecondVariable"
				label: qsTr("Second Variable")
				allowedColumns: ["scale"]
				singleVariable: true
				visible: test.currentValue.indexOf("pairedSamples") !== -1
			}

			AssignedVariablesList
			{
				name: "observedDependentVariable"
				label: qsTr("Dependent Variable")
				allowedColumns: ["scale"]
				singleVariable: true
				visible: test.currentValue.indexOf("independentSamples") !== -1
			}

			AssignedVariablesList
			{
				name: "observedGroupingVariable"
				label: qsTr("Grouping Variable")
				allowedColumns: ["nominal", "ordinal", "scale"]
				singleVariable: true
				visible: test.currentValue.indexOf("independentSamples") !== -1
			}
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

			CheckBox
			{
				Layout.columnSpan: 3
				name: "mergeH1H0Figures"
				id:   mergeH1H0Figures
				label: qsTr("Merge H\u2081 and H\u2080 figures")
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
				name: "drangeUpper"
				id:   drangeUpper
				defaultValue: 5
				negativeValues: true
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

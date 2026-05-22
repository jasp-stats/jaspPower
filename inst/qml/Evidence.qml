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
			{ label: qsTr("One Sample Proportion Test"), value: "oneSampleProportion"     },
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

	Section
	{
		expanded: true
		title: qsTr("Parameters")
		columns: 1

		Group
		{
			columns: 3

			Text { Layout.columnSpan: 2; text: qsTr("I want to calculate the ...") }
			DropDown
			{
				name: "calculation"
				id:   calc
				indexDefaultValue: 0
				label: ""
				values: [
					{ label: qsTr("Sample Size N"),        value: "sampleSize"          },
					{ label: qsTr("Conclusive evidence"),  value: "evidenceProbability" }
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

			Text { text: qsTr("Conclusive evidence for H\u2081:"); enabled: calc.currentValue !== "evidenceProbability" }
			Text { text: "Pr(BF\u2081\u2080 \u2265 k)"; enabled: calc.currentValue !== "evidenceProbability" }
			DoubleField
			{
				name: "targetPowerH1"
				id:   targetPowerH1
				min: 0
				max: 1
				defaultValue: 0.9
				inclusive: JASP.None
				enabled: calc.currentValue !== "evidenceProbability"
			}

			Text { text: qsTr("Conclusive evidence for H\u2080:"); enabled: calc.currentValue !== "evidenceProbability" }
			Text { text: "Pr(BF\u2080\u2081 \u2265 k)"; enabled: calc.currentValue !== "evidenceProbability" }
			DoubleField
			{
				name: "targetPowerH0"
				id:   targetPowerH0
				min: 0
				max: 1
				defaultValue: 0.9
				inclusive: JASP.None
				enabled: calc.currentValue !== "evidenceProbability"
			}

			Text
			{
				text: test.currentValue.indexOf("independentSamples") !== -1 ? qsTr("Sample size in group 1:") : qsTr("Sample size:")
				enabled: calc.currentValue !== "sampleSize"
			}
			Text
			{
				text: qsTr("N")
				enabled: calc.currentValue !== "sampleSize"
			}
			IntegerField
			{
				name: "sampleSize"
				id:   sampleSize
				min: 2
				defaultValue: 20
				enabled: calc.currentValue !== "sampleSize"
			}

			Text
			{
				text: qsTr("Sample size ratio:")
				visible: test.currentValue.indexOf("independentSamples") !== -1
			}
			Text
			{
				text: "N\u2082/N\u2081"
				visible: test.currentValue.indexOf("independentSamples") !== -1
			}
			DoubleField
			{
				name: "sampleSizeRatio"
				id:   sampleSizeRatio
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue.indexOf("independentSamples") !== -1
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
				values: [
					{ label: qsTr("SMD"),        value: "standardizedMeanDifference" },
					{ label: qsTr("Fisher's z"), value: "fisherZCorrelation"        },
					{ label: qsTr("logOR"),      value: "logOddsRatio"              },
					{ label: qsTr("logRR"),      value: "logRiskRatio"              },
					{ label: qsTr("logHR"),      value: "logHazardRatio"            },
					{ label: qsTr("logIRR"),     value: "logIncidenceRateRatio"     },
					{ label: qsTr("Specify"),    value: "unitInformationSd"         }
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
				text: qsTr("Alternative Hypothesis:")
				visible: test.currentValue.indexOf("TTest") !== -1
			}
			Text
			{
				text: qsTr("H\u2081")
				visible: test.currentValue.indexOf("TTest") !== -1
			}
			DropDown
			{
				name: "alternative"
				id:   alternative
				indexDefaultValue: 0
				visible: test.currentValue.indexOf("TTest") !== -1
				values: [
					{ label: qsTr("Two-sided"),          value: "twoSided" },
					{ label: qsTr("Less (One-sided)"),   value: "less"     },
					{ label: qsTr("Greater (One-sided)"), value: "greater"  }
				]
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
				enabled: test.currentValue === "oneSampleProportion"
				values: test.currentValue === "oneSampleProportion" ?
				[
					{ label: qsTr("Point null (p = p\u2080)"),      value: "point"     },
					{ label: qsTr("Directional (p \u2264 p\u2080)"), value: "direction" }
				] :
				[
					{ label: qsTr("Point null (\u03B8 = \u03B8\u2080)"), value: "point" }
				]
			}

			Text
			{
				text: qsTr("Null value:")
				visible: test.currentValue !== "oneSampleProportion"
			}
			Text
			{
				text: "\u03B8\u2080"
				visible: test.currentValue !== "oneSampleProportion"
			}
			DoubleField
			{
				name: "nullValue"
				id:   nullValue
				defaultValue: 0
				negativeValues: true
				visible: test.currentValue !== "oneSampleProportion"
			}

			Text
			{
				text: qsTr("Hypothesized proportion")
				visible: test.currentValue === "oneSampleProportion"
			}
			Text
			{
				text: "p\u2080"
				visible: test.currentValue === "oneSampleProportion"
			}
			DoubleField
			{
				name: "nullProportion"
				id:   nullProportion
				min: 0
				max: 1
				defaultValue: 0.5
				inclusive: JASP.None
				visible: test.currentValue === "oneSampleProportion"
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
				values: test.currentValue === "oneSampleProportion" ?
				[
					{ label: qsTr("Beta prior"), value: "beta" }
				] :
				(test.currentValue.indexOf("ZTest") !== -1 || test.currentValue === "generalZApproximation" ?
				[
					{ label: qsTr("Normal"),                  value: "normal"             },
					{ label: qsTr("Point"),                   value: "point"              },
					{ label: qsTr("Normal-moment (mode)"),    value: "normalMomentMode"   },
					{ label: qsTr("Normal-moment (spread)"),  value: "normalMomentSpread" }
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

			Text
			{
				text: qsTr("Beta prior successes:")
				visible: test.currentValue === "oneSampleProportion"
			}
			Text
			{
				text: "a"
				visible: test.currentValue === "oneSampleProportion"
			}
			DoubleField
			{
				name: "analysisPriorSuccesses"
				id:   analysisPriorSuccesses
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue === "oneSampleProportion"
			}

			Text
			{
				text: qsTr("Beta prior failures:")
				visible: test.currentValue === "oneSampleProportion"
			}
			Text
			{
				text: "b"
				visible: test.currentValue === "oneSampleProportion"
			}
			DoubleField
			{
				name: "analysisPriorFailures"
				id:   analysisPriorFailures
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue === "oneSampleProportion"
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
			visible: test.currentValue !== "oneSampleProportion"

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
			title: qsTr("Prior Under H\u2080")
			columns: 3
			visible: test.currentValue === "oneSampleProportion"

			Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
			DropDown
			{
				name: "binomialDesignNullPrior"
				id:   binomialDesignNullPrior
				indexDefaultValue: 0
				label: ""
				values: [
					{ label: qsTr("Point proportion"), value: "point" },
					{ label: qsTr("Beta prior"),       value: "beta"  }
				]
			}

			Text
			{
				text: qsTr("Design proportion:")
				visible: binomialDesignNullPrior.currentValue === "point"
			}
			Text
			{
				text: "p"
				visible: binomialDesignNullPrior.currentValue === "point"
			}
			DoubleField
			{
				name: "designNullProportion"
				id:   designNullProportion
				min: 0
				max: 1
				defaultValue: 0.5
				inclusive: JASP.None
				visible: binomialDesignNullPrior.currentValue === "point"
			}

			Text
			{
				text: qsTr("Beta prior successes:")
				visible: binomialDesignNullPrior.currentValue === "beta"
			}
			Text
			{
				text: "a"
				visible: binomialDesignNullPrior.currentValue === "beta"
			}
			DoubleField
			{
				name: "designNullPriorSuccesses"
				id:   designNullPriorSuccesses
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: binomialDesignNullPrior.currentValue === "beta"
			}

			Text
			{
				text: qsTr("Beta prior failures:")
				visible: binomialDesignNullPrior.currentValue === "beta"
			}
			Text
			{
				text: "b"
				visible: binomialDesignNullPrior.currentValue === "beta"
			}
			DoubleField
			{
				name: "designNullPriorFailures"
				id:   designNullPriorFailures
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: binomialDesignNullPrior.currentValue === "beta"
			}

			Text
			{
				text: qsTr("Lower truncation:")
				visible: binomialDesignNullPrior.currentValue === "beta"
			}
			Text
			{
				text: "l"
				visible: binomialDesignNullPrior.currentValue === "beta"
			}
			DoubleField
			{
				name: "designNullPriorLower"
				id:   designNullPriorLower
				min: 0
				max: 1
				defaultValue: 0
				inclusive: JASP.MinOnly
				visible: binomialDesignNullPrior.currentValue === "beta"
			}

			Text
			{
				text: qsTr("Upper truncation:")
				visible: binomialDesignNullPrior.currentValue === "beta"
			}
			Text
			{
				text: "u"
				visible: binomialDesignNullPrior.currentValue === "beta"
			}
			DoubleField
			{
				name: "designNullPriorUpper"
				id:   designNullPriorUpper
				min: 0
				max: 1
				defaultValue: 0.5
				inclusive: JASP.MaxOnly
				visible: binomialDesignNullPrior.currentValue === "beta"
			}
		}

		Group
		{
			title: qsTr("Prior Under H\u2081")
			columns: 3
			visible: test.currentValue !== "oneSampleProportion"

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

		Group
		{
			title: qsTr("Prior Under H\u2081")
			columns: 3
			visible: test.currentValue === "oneSampleProportion"

			Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
			DropDown
			{
				name: "binomialDesignPrior"
				id:   binomialDesignPrior
				indexDefaultValue: 0
				label: ""
				values: [
					{ label: qsTr("Point proportion"), value: "point" },
					{ label: qsTr("Beta prior"),       value: "beta"  }
				]
			}

			Text
			{
				text: qsTr("Design proportion:")
				visible: binomialDesignPrior.currentValue === "point"
			}
			Text
			{
				text: "p"
				visible: binomialDesignPrior.currentValue === "point"
			}
			DoubleField
			{
				name: "designProportion"
				id:   designProportion
				min: 0
				max: 1
				defaultValue: 0.6
				inclusive: JASP.None
				visible: binomialDesignPrior.currentValue === "point"
			}

			Text
			{
				text: qsTr("Beta prior successes:")
				visible: binomialDesignPrior.currentValue === "beta"
			}
			Text
			{
				text: "a"
				visible: binomialDesignPrior.currentValue === "beta"
			}
			DoubleField
			{
				name: "designPriorSuccesses"
				id:   designPriorSuccesses
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: binomialDesignPrior.currentValue === "beta"
			}

			Text
			{
				text: qsTr("Beta prior failures:")
				visible: binomialDesignPrior.currentValue === "beta"
			}
			Text
			{
				text: "b"
				visible: binomialDesignPrior.currentValue === "beta"
			}
			DoubleField
			{
				name: "designPriorFailures"
				id:   designPriorFailures
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: binomialDesignPrior.currentValue === "beta"
			}

			Text
			{
				text: qsTr("Lower truncation:")
				visible: binomialDesignPrior.currentValue === "beta"
			}
			Text
			{
				text: "l"
				visible: binomialDesignPrior.currentValue === "beta"
			}
			DoubleField
			{
				name: "designPriorLower"
				id:   designPriorLower
				min: 0
				max: 1
				defaultValue: 0
				inclusive: JASP.MinOnly
				visible: binomialDesignPrior.currentValue === "beta"
			}

			Text
			{
				text: qsTr("Upper truncation:")
				visible: binomialDesignPrior.currentValue === "beta"
			}
			Text
			{
				text: "u"
				visible: binomialDesignPrior.currentValue === "beta"
			}
			DoubleField
			{
				name: "designPriorUpper"
				id:   designPriorUpper
				min: 0
				max: 1
				defaultValue: 1
				inclusive: JASP.MaxOnly
				visible: binomialDesignPrior.currentValue === "beta"
			}
		}
	}

	Section
	{
		expanded: true
		title: qsTr("Plots")

		CheckBox
		{
			label: test.currentValue === "oneSampleProportion" ? qsTr("Conclusive evidence and misleading evidence by proportion") : qsTr("Conclusive evidence and misleading evidence by effect size")
			id:    evidenceByEffectSize
			name:  "evidenceByEffectSize"
			checked: true
		}

		CheckBox
		{
			label: qsTr("Conclusive evidence and misleading evidence by N")
			id:    evidenceBySampleSize
			name:  "evidenceBySampleSize"
			checked: false

			CheckBox
			{
				label: qsTr("Log sample size")
				name:  "logSampleSize"
				checked: true
			}
		}

		CheckBox
		{
			label: qsTr("Show BF\u2081\u2080 and BF\u2080\u2081 targets")
			id:    showBothEvidenceTargets
			name:  "showBothEvidenceTargets"
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

			IntegerField
			{
				name: "observedTrials"
				label: qsTr("Trials")
				min: 0
				defaultValue: 0
				visible: test.currentValue === "oneSampleProportion"
			}

			IntegerField
			{
				name: "observedSuccesses"
				label: qsTr("Successes")
				min: 0
				defaultValue: 0
				visible: test.currentValue === "oneSampleProportion"
			}

			DoubleField
			{
				name: "observedEstimate"
				label: qsTr("Estimate")
				defaultValue: 0
				negativeValues: true
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
				visible: test.currentValue.indexOf("independentSamples") === -1 && test.currentValue !== "oneSampleProportion" && test.currentValue !== "generalZApproximation"
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
				visible: (test.currentValue.indexOf("oneSample") !== -1 && test.currentValue !== "oneSampleProportion") || test.currentValue === "generalZApproximation"
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

			AssignedVariablesList
			{
				name: "observedProportionVariable"
				label: qsTr("Variable")
				allowedColumns: ["nominal", "ordinal", "scale"]
				singleVariable: true
				visible: test.currentValue === "oneSampleProportion"
			}
		}

		TextField
		{
			name: "observedSuccessValue"
			label: qsTr("Success value")
			defaultValue: "1"
			visible: observedAnalysisInput.value === "columns" && test.currentValue === "oneSampleProportion"
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

			Text
			{
				text: qsTr("Minimum sample size:")
				visible: calc.currentValue === "sampleSize"
			}
			Text
			{
				text: qsTr("min")
				visible: calc.currentValue === "sampleSize"
			}
			IntegerField
			{
				name: "sampleSizeRangeMin"
				id:   sampleSizeRangeMin
				min: 1
				defaultValue: 2
				visible: calc.currentValue === "sampleSize"
			}

			Text
			{
				text: qsTr("Maximum sample size:")
				visible: calc.currentValue === "sampleSize"
			}
			Text
			{
				text: qsTr("max")
				visible: calc.currentValue === "sampleSize"
			}
			IntegerField
			{
				name: "sampleSizeRangeMax"
				id:   sampleSizeRangeMax
				min: 2
				defaultValue: 10000
				visible: calc.currentValue === "sampleSize"
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

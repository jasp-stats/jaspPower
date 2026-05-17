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
			{ label: qsTr("One Sample Proportion Test"), value: "oneSampleProportion"     }
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

			Text { Layout.columnSpan: 2; text: qsTr("I want to calculate the ...") }
			DropDown
			{
				name: "calculation"
				id:   calc
				indexDefaultValue: 0
				label: ""
				values: [
					{ label: qsTr("Sample Size N"),        value: "sampleSize"          },
					{ label: qsTr("Evidence Probability"), value: "evidenceProbability" }
				]
			}

			Text { Layout.columnSpan: 2; text: qsTr("Evidence for:") }
			DropDown
			{
				name: "evidenceTarget"
				id:   evidenceTarget
				indexDefaultValue: 0
				label: ""
				values: [
					{ label: qsTr("H\u2081 (BF\u2081\u2080)"), value: "h1" },
					{ label: qsTr("H\u2080 (BF\u2080\u2081)"), value: "h0" }
				]
			}

			Text { text: qsTr("Evidence threshold:") }
			Text { text: evidenceTarget.currentValue === "h1" ? "BF\u2081\u2080 \u2265" : "BF\u2080\u2081 \u2265" }
			DoubleField
			{
				name: "bfThreshold"
				id:   bfThreshold
				min: 1
				defaultValue: 10
				inclusive: JASP.None
			}

			Text
			{
				text: qsTr("Minimal desired evidence probability:")
				enabled: calc.currentValue !== "evidenceProbability"
			}
			Text
			{
				text: evidenceTarget.currentValue === "h1" ? "Pr(BF\u2081\u2080 \u2265 k)" : "Pr(BF\u2080\u2081 \u2265 k)"
				enabled: calc.currentValue !== "evidenceProbability"
			}
			DoubleField
			{
				name: "evidenceProbability"
				id:   evidenceProbability
				min: 0
				max: 1
				defaultValue: 0.9
				inclusive: JASP.None
				enabled: calc.currentValue !== "evidenceProbability"
			}

			Text
			{
				text: test.currentValue.indexOf("independentSamples") !== -1 || test.currentValue.indexOf("pairedSamples") !== -1 ? qsTr("Sample size per group:") : qsTr("Sample size:")
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
				text: "N\u2081/N\u2082"
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
				(test.currentValue.indexOf("ZTest") !== -1 ?
				[
					{ label: qsTr("Normal"),                  value: "normal"             },
					{ label: qsTr("Normal-moment (mode)"),    value: "normalMomentMode"   },
					{ label: qsTr("Normal-moment (spread)"),  value: "normalMomentSpread" }
				] :
				[
					{ label: qsTr("t prior"), value: "t" }
				])
			}

			Text
			{
				text: qsTr("Prior mean:")
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normal"
			}
			Text
			{
				text: "\u03BC"
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normal"
			}
			DoubleField
			{
				name: "analysisPriorMean"
				id:   analysisPriorMean
				defaultValue: 0
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normal"
			}

			Text
			{
				text: qsTr("Prior scale:")
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normal"
			}
			Text
			{
				text: "\u03C3"
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normal"
			}
			DoubleField
			{
				name: "analysisPriorSd"
				id:   analysisPriorSd
				min: 0
				defaultValue: 0.707
				inclusive: JASP.None
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normal"
			}

			Text
			{
				text: qsTr("Prior spread:")
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normalMomentSpread"
			}
			Text
			{
				text: "\u03C4"
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normalMomentSpread"
			}
			DoubleField
			{
				name: "momentPriorSpread"
				id:   momentPriorSpread
				min: 0
				defaultValue: 0.707
				inclusive: JASP.None
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normalMomentSpread"
			}

			Text
			{
				text: qsTr("Prior mode:")
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normalMomentMode"
			}
			Text
			{
				text: "|m|"
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normalMomentMode"
			}
			DoubleField
			{
				name: "momentPriorMode"
				id:   momentPriorMode
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normalMomentMode"
			}

			Text
			{
				Layout.columnSpan: 2
				text: qsTr("Prior modes:")
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normalMomentSpread"
			}
			Text
			{
				text: "\u00B1\u221A2\u03C4"
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normalMomentSpread"
			}

			Text
			{
				Layout.columnSpan: 2
				text: qsTr("Back-computed spread:")
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normalMomentMode"
			}
			Text
			{
				text: "\u03C4 = |m|/\u221A2"
				visible: test.currentValue.indexOf("ZTest") !== -1 && analysisPriorDistribution.currentValue === "normalMomentMode"
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

		Group
		{
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
			label: qsTr("Evidence curve by effect size")
			id:    evidenceByEffectSize
			name:  "evidenceByEffectSize"
			checked: true
			visible: test.currentValue !== "oneSampleProportion"
		}

		CheckBox
		{
			label: qsTr("Evidence curve by N")
			id:    evidenceBySampleSize
			name:  "evidenceBySampleSize"
			checked: false
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

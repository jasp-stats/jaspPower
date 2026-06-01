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
	expanded: true
	title: qsTr("Analysis Prior")
	columns: 2

	property string testValue: ""
	property bool supportsBinomial: false
	property bool supportsDirectionalZ: false
	property bool isBinomial: supportsBinomial && testValue === "oneSampleProportion"
	property bool isContinuousZ: testValue.indexOf("ZTest") !== -1 || testValue === "generalZApproximation"
	property bool isTTest: testValue.indexOf("TTest") !== -1
	property bool usesDirectionalZ: supportsDirectionalZ && isContinuousZ && alternative.currentValue !== "twoSided"
	property bool usesPriorDirection: !isBinomial && (isTTest || supportsDirectionalZ)

	Group
	{
		title: qsTr("Prior Under H\u2080")
		columns: 3

		Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
		DropDown
		{
			name: "nullPriorDistribution"
			id:   nullPriorDistribution
			info: qsTr("Prior specification for H\u2080 used to compute the Bayes factor from the observed data.")
			indexDefaultValue: 0
			label: ""
			enabled: isBinomial
			values: isBinomial ?
			[
				{ label: qsTr("Point null (p = p₀)"),      value: "point"     },
				{ label: qsTr("Directional (p ≤ p₀)"), value: "direction" }
			] :
			(usesDirectionalZ ?
			[
				{ label: alternative.currentValue === "less" ? qsTr("Directional (θ ≥ θ₀)") : qsTr("Directional (θ ≤ θ₀)"), value: "directional" }
			] :
			[
				{ label: qsTr("Point null (θ = θ₀)"), value: "point" }
			])
		}

		Text
		{
			text: qsTr("Null value:")
			visible: !isBinomial
		}
		Text
		{
			text: "\u03B8\u2080"
			visible: !isBinomial
		}
		DoubleField
		{
			name: "nullValue"
			id:   nullValue
			info: qsTr("Parameter value specified by the null hypothesis.")
			defaultValue: 0
			negativeValues: true
			visible: !isBinomial
		}

		Text
		{
			text: qsTr("Hypothesized proportion")
			visible: isBinomial
		}
		Text
		{
			text: "p\u2080"
			visible: isBinomial
		}
		DoubleField
		{
			name: "nullProportion"
			id:   nullProportion
			info: qsTr("Proportion specified by the null hypothesis for the binomial test.")
			min: 0
			max: 1
			defaultValue: 0.5
			inclusive: JASP.None
			visible: isBinomial
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
			info: qsTr("Prior distribution under H\u2081 used to compute the Bayes factor from the data.")
			indexDefaultValue: 0
			label: ""
			values: isBinomial ?
			[
				{ label: qsTr("Beta prior"), value: "beta" }
			] :
			(usesDirectionalZ ?
			[
				{ label: qsTr("Normal"), value: "normal" }
			] :
			(isContinuousZ ?
			[
				{ label: qsTr("Normal"),                  value: "normal"             },
				{ label: qsTr("Point"),                   value: "point"              },
				{ label: qsTr("Normal-moment (mode)"),    value: "normalMomentMode"   },
				{ label: qsTr("Normal-moment (spread)"),  value: "normalMomentSpread" }
			] :
			[
				{ label: qsTr("Cauchy"),    value: "cauchy" },
				{ label: qsTr("Student-t"), value: "t"      }
			]))
		}

		Text
		{
			text: qsTr("Prior direction:")
			visible: usesPriorDirection
		}
		Text
		{
			text: ""
			visible: usesPriorDirection
		}
		DropDown
		{
			name: "analysisPriorDirection"
			id:   alternative
			info: qsTr("Direction of the analysis prior under H\u2081.")
			indexDefaultValue: 2
			visible: usesPriorDirection
			values: [
				{ label: qsTr("Two-sided"),     value: "twoSided" },
				{ label: qsTr("Less"), 			value: "less"     },
				{ label: qsTr("Greater"),		value: "greater"  }
			]
		}

		Text
		{
			text: qsTr("Prior location:")
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "point"
		}
		Text
		{
			text: "\u03B8\u2081"
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "point"
		}
		DoubleField
		{
			name: "analysisPriorLocation"
			id:   analysisPriorPoint
			info: qsTr("Point location of the H\u2081 analysis prior.")
			defaultValue: 0.5
			negativeValues: true
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "point"
		}

		Text
		{
			text: qsTr("Prior mean:")
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal"
		}
		Text
		{
			text: "\u03BC"
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal"
		}
		DoubleField
		{
			name: "analysisPriorMean"
			id:   analysisPriorMean
			info: qsTr("Mean of the normal H\u2081 analysis prior.")
			defaultValue: 0
			negativeValues: true
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal"
		}

		Text
		{
			text: qsTr("Prior scale:")
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal"
		}
		Text
		{
			text: "\u03C3"
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal"
		}
		DoubleField
		{
			name: "analysisPriorScale"
			id:   analysisPriorSd
			info: qsTr("Scale of the normal H\u2081 analysis prior.")
			min: 0
			defaultValue: 0.707
			inclusive: JASP.None
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal"
		}

		Text
		{
			text: qsTr("Prior spread:")
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentSpread"
		}
		Text
		{
			text: "\u03C4"
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentSpread"
		}
		DoubleField
		{
			name: "analysisPriorSpread"
			id:   momentPriorSpread
			info: qsTr("Spread parameter of the non-local moment prior under H\u2081.")
			min: 0
			defaultValue: 0.707
			inclusive: JASP.None
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentSpread"
		}

		Text
		{
			text: qsTr("Prior mode:")
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentMode"
		}
		Text
		{
			text: "|m|"
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentMode"
		}
		DoubleField
		{
			name: "analysisPriorMode"
			id:   momentPriorMode
			info: qsTr("Mode of the non-local moment prior under H\u2081.")
			min: 0
			defaultValue: 1
			inclusive: JASP.None
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentMode"
		}

		Text
		{
			Layout.columnSpan: 2
			text: qsTr("Prior modes:")
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentSpread"
		}
		Text
		{
			text: "\u00B1\u221A2\u03C4"
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentSpread"
		}

		Text
		{
			Layout.columnSpan: 2
			text: qsTr("Back-computed spread:")
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentMode"
		}
		Text
		{
			text: "\u03C4 = |m|/\u221A2"
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentMode"
		}

		Text
		{
			text: qsTr("Prior location:")
			visible: isTTest
		}
		Text
		{
			text: "\u03BC"
			visible: isTTest
		}
		DoubleField
		{
			name: "tPriorLocation"
			id:   tPriorLocation
			info: qsTr("Location of the Cauchy or Student-t analysis prior under H\u2081.")
			defaultValue: 0
			negativeValues: true
			visible: isTTest
		}

		Text
		{
			text: qsTr("Prior scale:")
			visible: isTTest
		}
		Text
		{
			text: "\u03C3"
			visible: isTTest
		}
		DoubleField
		{
			name: "tPriorScale"
			id:   tPriorScale
			info: qsTr("Scale of the Cauchy or Student-t analysis prior under H\u2081.")
			min: 0
			defaultValue: 0.707
			inclusive: JASP.None
			visible: isTTest
		}

		Text
		{
			text: qsTr("Prior degrees of freedom:")
			visible: isTTest && analysisPriorDistribution.currentValue === "t"
		}
		Text
		{
			text: "df"
			visible: isTTest && analysisPriorDistribution.currentValue === "t"
		}
		DoubleField
		{
			name: "tPriorDegreesOfFreedom"
			id:   tPriorDf
			info: qsTr("Degrees of freedom of the Student-t analysis prior under H\u2081.")
			min: 0
			defaultValue: 1
			inclusive: JASP.None
			visible: isTTest && analysisPriorDistribution.currentValue === "t"
		}

		Text
		{
			text: qsTr("Beta prior successes:")
			visible: isBinomial
		}
		Text
		{
			text: "a"
			visible: isBinomial
		}
		DoubleField
		{
			name: "analysisPriorSuccesses"
			id:   analysisPriorSuccesses
			info: qsTr("Success parameter of the beta analysis prior under H\u2081.")
			min: 0
			defaultValue: 1
			inclusive: JASP.None
			visible: isBinomial
		}

		Text
		{
			text: qsTr("Beta prior failures:")
			visible: isBinomial
		}
		Text
		{
			text: "b"
			visible: isBinomial
		}
		DoubleField
		{
			name: "analysisPriorFailures"
			id:   analysisPriorFailures
			info: qsTr("Failure parameter of the beta analysis prior under H\u2081.")
			min: 0
			defaultValue: 1
			inclusive: JASP.None
			visible: isBinomial
		}
	}
}

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
	// The composite directional H₀ (θ ≤ θ₀ vs. θ > θ₀) is only available for sequential z-test designs with a normal prior
	property bool supportsDirectionalNull: false
	property bool isBinomial: supportsBinomial && testValue === "oneSampleProportion"
	property bool isContinuousZ: testValue.indexOf("ZTest") !== -1 || testValue === "generalZApproximation"
	property bool isTTest: testValue.indexOf("TTest") !== -1
	property bool usesPriorDirection: !isBinomial
	property bool isOneSided: usesPriorDirection && alternative.currentValue !== "twoSided"
	property bool offersDirectionalNull: supportsDirectionalNull && isContinuousZ && isOneSided && analysisPriorDistribution.currentValue === "normal"
	// two critical values per look (two-sided prior with spread): only then does exact integration over all regions matter
	property bool hasTwoCriticalValues: !isBinomial && !isOneSided && !(isContinuousZ && analysisPriorDistribution.currentValue === "point")

	Group
	{
		title: qsTr("Prior Under H\u2080")
		columns: 3

		Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
		DropDown
		{
			name: "nullPriorDistribution"
			id:   nullPriorDistribution
			info: qsTr("Prior specification for H\u2080 used to compute the Bayes factor from the observed data. A point null fixes the parameter at the null value. A directional null places the opposite half of the analysis prior on H\u2080 (available for one-sided normal priors in sequential z-test designs and for the proportion test).")
			indexDefaultValue: 0
			label: ""
			enabled: isBinomial || offersDirectionalNull
			values: isBinomial ?
			[
				{ label: qsTr("Point null (p = p₀)"),      value: "point"     },
				{ label: qsTr("Directional (p ≤ p₀)"), value: "direction" }
			] :
			(offersDirectionalNull ?
			[
				{ label: qsTr("Point null (θ = θ₀)"), value: "point" },
				{ label: alternative.currentValue === "less" ? qsTr("Directional (θ ≥ θ₀)") : qsTr("Directional (θ ≤ θ₀)"), value: "directional" }
			] :
			[
				{ label: qsTr("Point null (θ = θ₀)"), value: "point" }
			])
		}

		Text { text: qsTr("Null value:"); visible: !isBinomial }
		Text { text: "\u03B8\u2080"; visible: !isBinomial }
		DoubleField
		{
			name: "nullValue"
			info: qsTr("Parameter value specified by the null hypothesis.")
			defaultValue: 0
			negativeValues: true
			visible: !isBinomial
		}

		Text { text: qsTr("Hypothesized proportion"); visible: isBinomial }
		Text { text: "p\u2080"; visible: isBinomial }
		DoubleField
		{
			name: "nullProportion"
			info: qsTr("Proportion specified by the null hypothesis for the binomial test.")
			defaultValue: 0.5
			min: 0
			max: 1
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
			])
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
			info: qsTr("Direction of the analysis prior under H\u2081. One-sided directions truncate the analysis prior to values below (less) or above (greater) the null value and renormalize it; the prior parameters describe the distribution before truncation.")
			indexDefaultValue: 2
			visible: usesPriorDirection
			values: [
				{ label: qsTr("Two-sided"),     value: "twoSided" },
				{ label: qsTr("Less"), 			value: "less"     },
				{ label: qsTr("Greater"),		value: "greater"  }
			]
		}

		Text { text: qsTr("Prior location:"); visible: isContinuousZ && analysisPriorDistribution.currentValue === "point" }
		Text { text: "\u03B8\u2081"; visible: isContinuousZ && analysisPriorDistribution.currentValue === "point" }
		DoubleField
		{
			name: "analysisPriorLocation"
			info: qsTr("Point location of the H\u2081 analysis prior. For a one-sided direction, it must lie strictly on the H\u2081 side of the null value.")
			defaultValue: 0.5
			negativeValues: true
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "point"
		}

		Text { text: qsTr("Prior mean:"); visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal" }
		Text { text: "\u03BC"; visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal" }
		DoubleField
		{
			name: "analysisPriorMean"
			info: qsTr("Mean of the normal H\u2081 analysis prior.")
			defaultValue: 0
			negativeValues: true
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal"
		}

		Text { text: qsTr("Prior scale:"); visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal" }
		Text { text: "\u03C3"; visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal" }
		DoubleField
		{
			name: "analysisPriorScale"
			info: qsTr("Scale of the normal H\u2081 analysis prior.")
			defaultValue: 0.707
			min: 0
			inclusive: JASP.None
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal"
		}

		Text { text: qsTr("Prior spread:"); visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentSpread" }
		Text { text: "\u03C4"; visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentSpread" }
		DoubleField
		{
			name: "analysisPriorSpread"
			info: qsTr("Spread parameter of the non-local moment prior under H\u2081.")
			defaultValue: 0.354 // modes at 0.5 (= sqrt(2) * 0.354), like the mode parameterization
			min: 0
			inclusive: JASP.None
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentSpread"
		}

		Text { text: qsTr("Prior mode:"); visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentMode" }
		Text { text: "|m|"; visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentMode" }
		DoubleField
		{
			name: "analysisPriorMode"
			info: qsTr("Mode of the non-local moment prior under H\u2081.")
			defaultValue: 0.5
			min: 0
			inclusive: JASP.None
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentMode"
		}

		Text
		{
			Layout.columnSpan: 2
			text: isOneSided ? qsTr("Prior mode:") : qsTr("Prior modes:")
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentSpread"
		}
		Text
		{
			text: (isOneSided ? (alternative.currentValue === "less" ? "\u2212" : "+") : "\u00B1") + "\u221A2\u03C4"
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

		Text { text: qsTr("Prior location:"); visible: isTTest }
		Text { text: "\u03BC"; visible: isTTest }
		DoubleField
		{
			name: "tPriorLocation"
			info: qsTr("Location of the Cauchy or Student-t analysis prior under H\u2081.")
			defaultValue: 0
			negativeValues: true
			visible: isTTest
		}

		Text { text: qsTr("Prior scale:"); visible: isTTest }
		Text { text: "\u03C3"; visible: isTTest }
		DoubleField
		{
			name: "tPriorScale"
			info: qsTr("Scale of the Cauchy or Student-t analysis prior under H\u2081.")
			defaultValue: 0.707
			min: 0
			inclusive: JASP.None
			visible: isTTest
		}

		Text { text: qsTr("Prior degrees of freedom:"); visible: isTTest && analysisPriorDistribution.currentValue === "t" }
		Text { text: "df"; visible: isTTest && analysisPriorDistribution.currentValue === "t" }
		DoubleField
		{
			name: "tPriorDegreesOfFreedom"
			info: qsTr("Degrees of freedom of the Student-t analysis prior under H\u2081.")
			defaultValue: 1
			min: 0
			inclusive: JASP.None
			visible: isTTest && analysisPriorDistribution.currentValue === "t"
		}

		Text { text: qsTr("Beta prior successes:"); visible: isBinomial }
		Text { text: "a"; visible: isBinomial }
		DoubleField
		{
			name: "analysisPriorSuccesses"
			info: qsTr("Success parameter of the beta analysis prior under H\u2081.")
			defaultValue: 1
			min: 0
			inclusive: JASP.None
			visible: isBinomial
		}

		Text { text: qsTr("Beta prior failures:"); visible: isBinomial }
		Text { text: "b"; visible: isBinomial }
		DoubleField
		{
			name: "analysisPriorFailures"
			info: qsTr("Failure parameter of the beta analysis prior under H\u2081.")
			defaultValue: 1
			min: 0
			inclusive: JASP.None
			visible: isBinomial
		}
	}
}

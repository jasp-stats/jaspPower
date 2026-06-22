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

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Null value:")
			symbolText: "\u03B8\u2080"
			fieldName: "nullValue"
			fieldInfo: qsTr("Parameter value specified by the null hypothesis.")
			defaultValue: 0
			negativeValues: true
			visible: !isBinomial
		}

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Hypothesized proportion")
			symbolText: "p\u2080"
			fieldName: "nullProportion"
			fieldInfo: qsTr("Proportion specified by the null hypothesis for the binomial test.")
			hasMinimum: true
			hasMaximum: true
			minimum: 0
			maximum: 1
			defaultValue: 0.5
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

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Prior location:")
			symbolText: "\u03B8\u2081"
			fieldName: "analysisPriorLocation"
			fieldInfo: qsTr("Point location of the H\u2081 analysis prior.")
			defaultValue: 0.5
			negativeValues: true
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "point"
		}

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Prior mean:")
			symbolText: "\u03BC"
			fieldName: "analysisPriorMean"
			fieldInfo: qsTr("Mean of the normal H\u2081 analysis prior.")
			defaultValue: 0
			negativeValues: true
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal"
		}

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Prior scale:")
			symbolText: "\u03C3"
			fieldName: "analysisPriorScale"
			fieldInfo: qsTr("Scale of the normal H\u2081 analysis prior.")
			hasMinimum: true
			minimum: 0
			defaultValue: 0.707
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normal"
		}

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Prior spread:")
			symbolText: "\u03C4"
			fieldName: "analysisPriorSpread"
			fieldInfo: qsTr("Spread parameter of the non-local moment prior under H\u2081.")
			hasMinimum: true
			minimum: 0
			defaultValue: 0.707
			visible: isContinuousZ && analysisPriorDistribution.currentValue === "normalMomentSpread"
		}

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Prior mode:")
			symbolText: "|m|"
			fieldName: "analysisPriorMode"
			fieldInfo: qsTr("Mode of the non-local moment prior under H\u2081.")
			hasMinimum: true
			minimum: 0
			defaultValue: 1
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

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Prior location:")
			symbolText: "\u03BC"
			fieldName: "tPriorLocation"
			fieldInfo: qsTr("Location of the Cauchy or Student-t analysis prior under H\u2081.")
			defaultValue: 0
			negativeValues: true
			visible: isTTest
		}

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Prior scale:")
			symbolText: "\u03C3"
			fieldName: "tPriorScale"
			fieldInfo: qsTr("Scale of the Cauchy or Student-t analysis prior under H\u2081.")
			hasMinimum: true
			minimum: 0
			defaultValue: 0.707
			visible: isTTest
		}

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Prior degrees of freedom:")
			symbolText: "df"
			fieldName: "tPriorDegreesOfFreedom"
			fieldInfo: qsTr("Degrees of freedom of the Student-t analysis prior under H\u2081.")
			hasMinimum: true
			minimum: 0
			defaultValue: 1
			visible: isTTest && analysisPriorDistribution.currentValue === "t"
		}

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Beta prior successes:")
			symbolText: "a"
			fieldName: "analysisPriorSuccesses"
			fieldInfo: qsTr("Success parameter of the beta analysis prior under H\u2081.")
			hasMinimum: true
			minimum: 0
			defaultValue: 1
			visible: isBinomial
		}

		BayesFactorPriorDoubleFieldRow
		{
			labelText: qsTr("Beta prior failures:")
			symbolText: "b"
			fieldName: "analysisPriorFailures"
			fieldInfo: qsTr("Failure parameter of the beta analysis prior under H\u2081.")
			hasMinimum: true
			minimum: 0
			defaultValue: 1
			visible: isBinomial
		}
	}
}

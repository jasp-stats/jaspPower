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
	title: qsTr("Design Prior")
	columns: 2

	property string testValue: ""
	property bool supportsBinomial: false
	property bool isBinomial: supportsBinomial && testValue === "oneSampleProportion"

	Group
	{
		title: qsTr("Prior Under H\u2080")
		columns: 3
		visible: !isBinomial

		Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
		DropDown
		{
			name: "designNullPriorDistribution"
			id:   designNullPrior
			info: qsTr("Design prior under H\u2080 used to evaluate study outcomes when the null hypothesis is true.")
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
			info: qsTr("Location or mean of the H\u2080 design prior.")
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
			name: "designNullPriorStandardDeviation"
			id:   designNullPriorSd
			info: qsTr("Standard deviation of the normal H\u2080 design prior.")
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
		visible: isBinomial

		Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
		DropDown
		{
			name: "binomialDesignNullPriorDistribution"
			id:   binomialDesignNullPrior
			info: qsTr("Design prior under H\u2080 used to evaluate binomial study outcomes when the null hypothesis is true.")
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
			info: qsTr("Point proportion assumed under the H\u2080 design prior.")
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
			info: qsTr("Success parameter of the beta H\u2080 design prior.")
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
			info: qsTr("Failure parameter of the beta H\u2080 design prior.")
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
			name: "designNullPriorLowerTruncation"
			id:   designNullPriorLower
			info: qsTr("Lower truncation point for the beta H\u2080 design prior.")
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
			name: "designNullPriorUpperTruncation"
			id:   designNullPriorUpper
			info: qsTr("Upper truncation point for the beta H\u2080 design prior.")
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
		visible: !isBinomial

		Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
		DropDown
		{
			name: "designPriorDistribution"
			id:   designPrior
			info: qsTr("Design prior under H\u2081 used to evaluate study outcomes when the alternative hypothesis is true.")
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
			info: qsTr("Location or mean of the H\u2081 design prior.")
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
			name: "designPriorStandardDeviation"
			id:   designPriorSd
			info: qsTr("Standard deviation of the normal H\u2081 design prior.")
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
		visible: isBinomial

		Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
		DropDown
		{
			name: "binomialDesignPriorDistribution"
			id:   binomialDesignPrior
			info: qsTr("Design prior under H\u2081 used to evaluate binomial study outcomes when the alternative hypothesis is true.")
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
			info: qsTr("Point proportion assumed under the H\u2081 design prior.")
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
			info: qsTr("Success parameter of the beta H\u2081 design prior.")
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
			info: qsTr("Failure parameter of the beta H\u2081 design prior.")
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
			name: "designPriorLowerTruncation"
			id:   designPriorLower
			info: qsTr("Lower truncation point for the beta H\u2081 design prior.")
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
			name: "designPriorUpperTruncation"
			id:   designPriorUpper
			info: qsTr("Upper truncation point for the beta H\u2081 design prior.")
			min: 0
			max: 1
			defaultValue: 1
			inclusive: JASP.MaxOnly
			visible: binomialDesignPrior.currentValue === "beta"
		}
	}
}

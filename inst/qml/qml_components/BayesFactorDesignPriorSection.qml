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
	columns: 1

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
			name: "designNullPriorStandardDeviation"
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
		visible: isBinomial

		Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
		DropDown
		{
			name: "binomialDesignNullPriorDistribution"
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
			name: "designNullPriorLowerTruncation"
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
			name: "designNullPriorUpperTruncation"
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
		visible: !isBinomial

		Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
		DropDown
		{
			name: "designPriorDistribution"
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
			name: "designPriorStandardDeviation"
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
		visible: isBinomial

		Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
		DropDown
		{
			name: "binomialDesignPriorDistribution"
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
			name: "designPriorLowerTruncation"
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
			name: "designPriorUpperTruncation"
			id:   designPriorUpper
			min: 0
			max: 1
			defaultValue: 1
			inclusive: JASP.MaxOnly
			visible: binomialDesignPrior.currentValue === "beta"
		}
	}
}

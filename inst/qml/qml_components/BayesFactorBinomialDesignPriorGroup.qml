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

Group
{
	id: root
	columns: 3
	title: groupTitle

	property string groupTitle: ""
	property string distributionName: ""
	property string pointName: ""
	property string successesName: ""
	property string failuresName: ""
	property string lowerName: ""
	property string upperName: ""
	property string distributionInfo: ""
	property string pointInfo: ""
	property string successesInfo: ""
	property string failuresInfo: ""
	property string lowerInfo: ""
	property string upperInfo: ""
	property double pointDefault: 0.5
	property double lowerDefault: 0
	property double upperDefault: 1

	function syncTruncationRange()
	{
		if (lowerTruncation.value >= 1)
			lowerTruncation.value = 0.99

		if (upperTruncation.value <= lowerTruncation.value)
			upperTruncation.value = Math.min(1, lowerTruncation.value + 0.01)
	}

	Component.onCompleted: syncTruncationRange()

	Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
	DropDown
	{
		name: root.distributionName
		id:   designPriorDistribution
		info: root.distributionInfo
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
		visible: designPriorDistribution.currentValue === "point"
	}
	Text
	{
		text: "p"
		visible: designPriorDistribution.currentValue === "point"
	}
	DoubleField
	{
		name: root.pointName
		info: root.pointInfo
		min: 0
		max: 1
		defaultValue: root.pointDefault
		inclusive: JASP.None
		visible: designPriorDistribution.currentValue === "point"
	}

	Text
	{
		text: qsTr("Beta prior successes:")
		visible: designPriorDistribution.currentValue === "beta"
	}
	Text
	{
		text: "a"
		visible: designPriorDistribution.currentValue === "beta"
	}
	DoubleField
	{
		name: root.successesName
		info: root.successesInfo
		min: 0
		defaultValue: 1
		inclusive: JASP.None
		visible: designPriorDistribution.currentValue === "beta"
	}

	Text
	{
		text: qsTr("Beta prior failures:")
		visible: designPriorDistribution.currentValue === "beta"
	}
	Text
	{
		text: "b"
		visible: designPriorDistribution.currentValue === "beta"
	}
	DoubleField
	{
		name: root.failuresName
		info: root.failuresInfo
		min: 0
		defaultValue: 1
		inclusive: JASP.None
		visible: designPriorDistribution.currentValue === "beta"
	}

	Text
	{
		text: qsTr("Lower truncation:")
		visible: designPriorDistribution.currentValue === "beta"
	}
	Text
	{
		text: "l"
		visible: designPriorDistribution.currentValue === "beta"
	}
	DoubleField
	{
		name: root.lowerName
		id:   lowerTruncation
		info: root.lowerInfo
		min: 0
		max: 1
		defaultValue: root.lowerDefault
		inclusive: JASP.MinOnly
		visible: designPriorDistribution.currentValue === "beta"
		onValueChanged: syncTruncationRange()
	}

	Text
	{
		text: qsTr("Upper truncation:")
		visible: designPriorDistribution.currentValue === "beta"
	}
	Text
	{
		text: "u"
		visible: designPriorDistribution.currentValue === "beta"
	}
	DoubleField
	{
		name: root.upperName
		id:   upperTruncation
		info: root.upperInfo
		min: lowerTruncation.value
		max: 1
		defaultValue: root.upperDefault
		inclusive: JASP.MaxOnly
		visible: designPriorDistribution.currentValue === "beta"
	}
}

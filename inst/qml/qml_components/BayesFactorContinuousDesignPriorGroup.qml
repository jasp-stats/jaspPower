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
	property string meanName: ""
	property string sdName: ""
	property string distributionInfo: ""
	property string meanInfo: ""
	property string sdInfo: ""
	property double meanDefault: 0
	property double sdDefault: 0.1

	Text { Layout.columnSpan: 2; text: qsTr("Distribution:") }
	DropDown
	{
		name: root.distributionName
		id:   designPriorDistribution
		info: root.distributionInfo
		indexDefaultValue: 0
		label: ""
		values: [
			{ label: qsTr("Point"),  value: "point"  },
			{ label: qsTr("Normal"), value: "normal" }
		]
	}

	Text { text: designPriorDistribution.currentValue === "point" ? qsTr("Location:") : qsTr("Mean:") }
	Text { text: "\u03BC" }
	DoubleField
	{
		name: root.meanName
		info: root.meanInfo
		defaultValue: root.meanDefault
		negativeValues: true
	}

	Text
	{
		text: qsTr("Standard deviation:")
		visible: designPriorDistribution.currentValue === "normal"
	}
	Text
	{
		text: "\u03C3"
		visible: designPriorDistribution.currentValue === "normal"
	}
	DoubleField
	{
		name: root.sdName
		info: root.sdInfo
		min: 0
		defaultValue: root.sdDefault
		inclusive: JASP.None
		visible: designPriorDistribution.currentValue === "normal"
	}
}

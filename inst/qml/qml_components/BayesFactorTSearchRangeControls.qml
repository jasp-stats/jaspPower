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
	Layout.columnSpan: 3

	property string testValue: ""

	visible: testValue.indexOf("TTest") !== -1

	function syncRange()
	{
		if (tRangeUpper.value <= tRangeLower.value)
			tRangeUpper.value = tRangeLower.value + 1
	}

	Component.onCompleted: syncRange()

	Text { text: qsTr("t search range:") }
	Text { text: qsTr("Range") }
	DropDown
	{
		name: "tSearchRangeMode"
		id:   tRangeMode
		info: qsTr("Choose whether the integration range for t-test calculations is selected automatically or supplied manually.")
		indexDefaultValue: 0
		values: [
			{ label: qsTr("Adaptive"), value: "adaptive" },
			{ label: qsTr("Custom"),   value: "custom"   }
		]
	}

	Text
	{
		text: qsTr("Lower:")
		visible: tRangeMode.currentValue === "custom"
	}
	Text
	{
		text: qsTr("min")
		visible: tRangeMode.currentValue === "custom"
	}
	DoubleField
	{
		name: "tSearchRangeLower"
		id:   tRangeLower
		info: qsTr("Lower bound of the custom integration range for t-test calculations.")
		defaultValue: -5
		negativeValues: true
		visible: tRangeMode.currentValue === "custom"
		onValueChanged: syncRange()
	}

	Text
	{
		text: qsTr("Upper:")
		visible: tRangeMode.currentValue === "custom"
	}
	Text
	{
		text: qsTr("max")
		visible: tRangeMode.currentValue === "custom"
	}
	DoubleField
	{
		name: "tSearchRangeUpper"
		id:   tRangeUpper
		info: qsTr("Upper bound of the custom integration range for t-test calculations.")
		min: tRangeLower.value
		defaultValue: 5
		negativeValues: true
		inclusive: JASP.None
		visible: tRangeMode.currentValue === "custom"
	}
}

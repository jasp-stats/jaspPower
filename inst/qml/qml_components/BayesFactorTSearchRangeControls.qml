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
	// fixed designs search critical standardized mean differences (bfpwr `drange`); sequential designs search
	// critical t statistics (bfpwr `trange`)
	property bool sequential: false
	property string rangeScale: sequential ? qsTr("t-statistic") : qsTr("standardized mean difference")

	visible: testValue.indexOf("TTest") !== -1

	function syncRange()
	{
		if (tRangeUpper.value <= tRangeLower.value)
			tRangeUpper.value = tRangeLower.value + 1
	}

	Component.onCompleted: syncRange()

	Text { text: qsTr("Critical value search:") }
	Text { text: qsTr("Range") }
	DropDown
	{
		name: "tSearchRangeMode"
		id:   tRangeMode
		info: qsTr("Choose whether the %1 range in which the t-test critical values are searched is selected automatically or supplied manually.").arg(rangeScale)
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
		info: qsTr("Lower bound of the custom %1 range in which the t-test critical values are searched.").arg(rangeScale)
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
		info: qsTr("Upper bound of the custom %1 range in which the t-test critical values are searched.").arg(rangeScale)
		min: tRangeLower.value
		defaultValue: 5
		negativeValues: true
		inclusive: JASP.None
		visible: tRangeMode.currentValue === "custom"
	}
}

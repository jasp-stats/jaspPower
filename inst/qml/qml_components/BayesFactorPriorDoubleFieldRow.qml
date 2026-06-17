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
import QtQml
import JASP
import JASP.Controls

GridLayout
{
	id: root
	columns: 3
	Layout.columnSpan: 3
	Layout.fillWidth: true

	property string labelText: ""
	property string symbolText: ""
	property string fieldName: ""
	property string fieldInfo: ""
	property double defaultValue: 0
	property bool negativeValues: false
	property bool hasMinimum: false
	property bool hasMaximum: false
	property double minimum: 0
	property double maximum: 1
	property int inclusive: JASP.None

	Text { text: root.labelText }
	Text { text: root.symbolText }
	DoubleField
	{
		id: priorField
		name: root.fieldName
		info: root.fieldInfo
		defaultValue: root.defaultValue
		negativeValues: root.negativeValues
		inclusive: root.inclusive
	}

	Binding
	{
		target: priorField
		property: "min"
		value: root.minimum
		when: root.hasMinimum
	}

	Binding
	{
		target: priorField
		property: "max"
		value: root.maximum
		when: root.hasMaximum
	}
}

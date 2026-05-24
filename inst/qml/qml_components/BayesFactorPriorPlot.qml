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
	title:   qsTr("Prior Distribution")
	columns: 1

	CheckBox
	{
		label: qsTr("Design prior distribution")
		name:  "designPriorDistributionFigure"
		checked: true
	}

	CheckBox
	{
		label: qsTr("Analysis prior distribution")
		name:  "analysisPriorDistributionFigure"
		checked: true
	}

	CheckBox
	{
		label: qsTr("Combine design and analysis")
		name:  "combineDesignAnalysisPriorFigures"
		checked: false
	}
}

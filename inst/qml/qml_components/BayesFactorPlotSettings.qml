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
	title: qsTr("Settings")

	DropDown
	{
		name:       "legendPosition"
		label:      qsTr("Legend position")
		info:       qsTr("Choose where the plot legend is shown.")
		startValue: "right"
		values:
		[
			{ label: qsTr("None"),           value: "none"        },
			{ label: qsTr("Bottom"),         value: "bottom"      },
			{ label: qsTr("Right"),          value: "right"       },
			{ label: qsTr("Right (inside)"), value: "rightInside" },
			{ label: qsTr("Top"),            value: "top"         },
			{ label: qsTr("Left"),           value: "left"        }
		]
	}

	ColorPalette
	{
		name:      "colorPalette"
		label:     qsTr("Color palette")
		infoLabel: qsTr("Color palette")
		info:      qsTr("Choose the color palette used for evidence outcomes and design priors in plots.")
	}
}

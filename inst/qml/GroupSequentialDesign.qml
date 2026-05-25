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
import "./qml_components" as PowerComponents

Form
{
	id: root
	property int defaultGridPoints: 18

	Group
	{
		DropDown
		{
			name: "designType"
			id:   designType
			indexDefaultValue: 0
			label: qsTr("Design type:")
			values: [
				{ label: qsTr("One-sided"),                                               value: "oneSided"                       },
				{ label: qsTr("Two-sided symmetric"),                                     value: "twoSidedSymmetric"              },
				{ label: qsTr("Two-sided asymmetric \u03B2-spending (binding)"),          value: "twoSidedAsymmetricBinding"      },
				{ label: qsTr("Two-sided asymmetric \u03B2-spending (non-binding)"),      value: "twoSidedAsymmetricNonBinding"   }
			]
			info: qsTr("Selects one-sided, symmetric two-sided, or asymmetric two-sided \u03B2-spending designs. Binding or non-binding controls Type I error computation after a lower-bound crossing.")
		}

		CheckBox
		{
			name:    "text"
			id:      explanatoryText
			label:   qsTr("Explanatory text")
			checked: true
			info:    qsTr("Display a short explanation of the design.")
		}
	}

	Section
	{
		expanded: true
		title:    qsTr("Parameters")
		columns:  1

		PowerComponents.GroupSequentialParameterControls {}
	}

	Section
	{
		expanded: true
		title:    qsTr("Boundaries")
		columns:  1

		PowerComponents.GroupSequentialBoundaryControls
		{
			designTypeValue: designType.currentValue
		}
	}

	Section
	{
		expanded: true
		title:    qsTr("Tables")

		Group
		{
			title: qsTr("Design Results")

			CheckBox
			{
				name:    "designSummary"
				label:   qsTr("Design summary")
				checked: true
				info:    qsTr("Display the main design summary and effect scale conversions when applicable.")
			}

			CheckBox
			{
				name:    "sampleSizeSummary"
				label:   qsTr("Sample size / event details")
				checked: true
				info:    qsTr("Display endpoint-specific allocation, subject, and analysis-time details when available.")
			}
		}

		Group
		{
			title: qsTr("Look-by-Look Results")

			CheckBox
			{
				name:    "stoppingBoundariesTable"
				label:   qsTr("Look schedule and stopping boundaries")
				checked: true
				info:    qsTr("Display the information schedule, Z-boundaries, and nominal p-values by look.")
			}

			CheckBox
			{
				name:    "crossingProbabilitiesTable"
				label:   qsTr("Boundary crossing probabilities")
				checked: false
				info:    qsTr("Display final, cumulative, and stagewise boundary crossing probabilities under H\u2080 and H\u2081.")
			}
		}
	}

	Section
	{
		expanded: true
		title:    qsTr("Plots")

		Group
		{
			title: qsTr("Design Figures")

			CheckBox
			{
				name:    "boundariesPlot"
				id:      boundariesPlot
				label:   qsTr("Stopping boundaries")
				checked: true
				info:    qsTr("Display stopping boundaries across analyses.")
			}

			CheckBox
			{
				name:    "crossingProbabilitiesPlot"
				id:      crossingProbabilitiesPlot
				label:   qsTr("Boundary crossing probabilities")
				checked: false
				info:    qsTr("Display cumulative boundary crossing probabilities under H\u2080 and H\u2081.")
			}
		}

		PowerComponents.BayesFactorPlotSettings
		{
			colorPaletteInfo: qsTr("Choose the color palette used for boundaries in plots.")
		}
	}

	Section
	{
		expanded: false
		title:    qsTr("Advanced Options")
		columns:  1

		Group
		{
			columns: 3

			CheckBox
			{
				Layout.columnSpan: 3
				name:    "generateRCode"
				id:      generateRCode
				label:   qsTr("Generate R code")
				checked: false
				info:    qsTr("Display copyable R code for the current settings.")
			}

			Text { Layout.columnSpan: 2; text: qsTr("Integration grid points:") }
			IntegerField
			{
				name:         "gridPoints"
				id:           gridPoints
				min:          1
				max:          80
				defaultValue: root.defaultGridPoints
				info:         qsTr("Integer from 1 to 80 controlling the numerical integration grid. Default is %1; larger values can improve accuracy but slow computation.").arg(root.defaultGridPoints)
			}
		}
	}
}

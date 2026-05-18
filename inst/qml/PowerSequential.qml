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

Form
{
	Section
	{
		expanded: true
		title:    qsTr("Design")
		columns:  1

		Group
		{
			columns: 3

			Text { Layout.columnSpan: 2; text: qsTr("Design type:") }
			DropDown
			{
				name: "designType"
				id:   designType
				indexDefaultValue: 0
				values: [
					{ label: qsTr("One-sided"),                                  value: "oneSided" },
					{ label: qsTr("Two-sided symmetric"),                        value: "twoSidedSymmetric" },
					{ label: qsTr("Two-sided asymmetric, binding futility"),     value: "twoSidedAsymmetricBinding" },
					{ label: qsTr("Two-sided asymmetric, non-binding futility"), value: "twoSidedAsymmetricNonBinding" }
				]
				info: qsTr("Selects the classical group sequential design type.")
			}

			Text { text: qsTr("Number of looks:") }
			Text { text: "K" }
			IntegerField
			{
				name:         "numberOfLooks"
				id:           numberOfLooks
				min:          2
				max:          30
				defaultValue: 3
				info:         qsTr("Number of planned analyses, including the final analysis.")
			}

			Text { text: qsTr("Type I error rate:") }
			Text { text: qsTr("One-sided alpha") }
			DoubleField
			{
				name:         "alpha"
				id:           alpha
				min:          0
				max:          1
				defaultValue: 0.025
				decimals:     4
				inclusive:    JASP.None
				info:         qsTr("One-sided Type I error rate.")
			}

			Text { text: qsTr("Power:") }
			Text { text: qsTr("1 - beta") }
			DoubleField
			{
				name:         "power"
				id:           power
				min:          0
				max:          1
				defaultValue: 0.9
				decimals:     4
				inclusive:    JASP.None
				info:         qsTr("Target power under the alternative hypothesis.")
			}

			Text { Layout.columnSpan: 2; text: qsTr("Sample size input:") }
			DropDown
			{
				name: "sampleSizeMode"
				id:   sampleSizeMode
				indexDefaultValue: 0
				values: [
					{ label: qsTr("Generic information ratio"), value: "generic" },
					{ label: qsTr("Fixed design sample size"), value: "fixedDesign" },
					{ label: qsTr("Standardized effect size"), value: "effectSize" }
				]
				info: qsTr("Choose whether the design reports information ratios, is scaled from a fixed design sample size, or is derived from an effect size.")
			}

			Text
			{
				text:    qsTr("Fixed design sample size:")
				visible: sampleSizeMode.currentValue === "fixedDesign"
			}
			Text
			{
				text:    "N"
				visible: sampleSizeMode.currentValue === "fixedDesign"
			}
			IntegerField
			{
				name:         "fixedSampleSize"
				id:           fixedSampleSize
				min:          2
				defaultValue: 100
				visible:      sampleSizeMode.currentValue === "fixedDesign"
				info:         qsTr("Sample size for the corresponding fixed design.")
			}

			Text
			{
				text:    qsTr("Standardized effect size:")
				visible: sampleSizeMode.currentValue === "effectSize"
			}
			Text
			{
				text:    qsTr("Delta")
				visible: sampleSizeMode.currentValue === "effectSize"
			}
			DoubleField
			{
				name:         "effectSize"
				id:           effectSize
				min:          0
				defaultValue: 0.5
				decimals:     4
				inclusive:    JASP.None
				visible:      sampleSizeMode.currentValue === "effectSize"
				info:         qsTr("Positive standardized effect size under the alternative hypothesis.")
			}

			Text { Layout.columnSpan: 2; text: qsTr("Look schedule:") }
			DropDown
			{
				name: "timingMode"
				id:   timingMode
				indexDefaultValue: 0
				values: [
					{ label: qsTr("Equally spaced information"), value: "even" },
					{ label: qsTr("Custom information fractions"), value: "custom" }
				]
				info: qsTr("Controls the information fraction at each look.")
			}

			Text
			{
				Layout.columnSpan: 2
				text:    qsTr("Information fractions:")
				visible: timingMode.currentValue === "custom"
			}
			TextField
			{
				name:         "timing"
				id:           timing
				defaultValue: "0.33, 0.67, 1"
				fieldWidth:   140
				visible:      timingMode.currentValue === "custom"
				info:         qsTr("Increasing information fractions. Supply K values ending in 1, or K - 1 interim values.")
			}
		}
	}

	Section
	{
		expanded: true
		title:    qsTr("Boundaries")
		columns:  1

		Group
		{
			columns: 3

			Text { Layout.columnSpan: 2; text: qsTr("Upper boundary:") }
			DropDown
			{
				name: "upperBoundary"
				id:   upperBoundary
				indexDefaultValue: 0
				values: [
					{ label: qsTr("O'Brien-Fleming"),       value: "obrienFleming" },
					{ label: qsTr("Pocock"),                value: "pocock" },
					{ label: qsTr("Wang-Tsiatis"),          value: "wangTsiatis" },
					{ label: qsTr("Hwang-Shih-DeCani"),     value: "hwangShihDeCani" },
					{ label: qsTr("Kim-DeMets power"),      value: "kimDeMetsPower" }
				]
				info: qsTr("Efficacy boundary family. For asymmetric designs, O'Brien-Fleming and Pocock are implemented as spending functions.")
			}

			Text
			{
				text:    qsTr("Upper boundary parameter:")
				enabled: upperBoundary.currentValue !== "obrienFleming" && upperBoundary.currentValue !== "pocock"
			}
			Text
			{
				text:    upperBoundary.currentValue === "wangTsiatis" ? qsTr("Delta") : qsTr("Parameter")
				enabled: upperBoundary.currentValue !== "obrienFleming" && upperBoundary.currentValue !== "pocock"
			}
			DoubleField
			{
				name:         "upperBoundaryParameter"
				id:           upperBoundaryParameter
				defaultValue: 0.25
				decimals:     4
				enabled:      upperBoundary.currentValue !== "obrienFleming" && upperBoundary.currentValue !== "pocock"
				info:         qsTr("Parameter for Wang-Tsiatis or spending-function boundaries.")
			}

			Text
			{
				Layout.columnSpan: 2
				text:    qsTr("Lower boundary:")
				visible: designType.currentValue === "twoSidedAsymmetricBinding" || designType.currentValue === "twoSidedAsymmetricNonBinding"
			}
			DropDown
			{
				name: "lowerBoundary"
				id:   lowerBoundary
				indexDefaultValue: 2
				visible: designType.currentValue === "twoSidedAsymmetricBinding" || designType.currentValue === "twoSidedAsymmetricNonBinding"
				values: [
					{ label: qsTr("O'Brien-Fleming spending"), value: "obrienFleming" },
					{ label: qsTr("Pocock spending"),          value: "pocock" },
					{ label: qsTr("Hwang-Shih-DeCani"),     value: "hwangShihDeCani" },
					{ label: qsTr("Kim-DeMets power"),      value: "kimDeMetsPower" }
				]
				info: qsTr("Futility boundary family for asymmetric two-sided designs.")
			}

			Text
			{
				text:    qsTr("Lower boundary parameter:")
				visible: designType.currentValue === "twoSidedAsymmetricBinding" || designType.currentValue === "twoSidedAsymmetricNonBinding"
				enabled: lowerBoundary.currentValue !== "obrienFleming" && lowerBoundary.currentValue !== "pocock"
			}
			Text
			{
				text:    qsTr("Parameter")
				visible: designType.currentValue === "twoSidedAsymmetricBinding" || designType.currentValue === "twoSidedAsymmetricNonBinding"
				enabled: lowerBoundary.currentValue !== "obrienFleming" && lowerBoundary.currentValue !== "pocock"
			}
			DoubleField
			{
				name:         "lowerBoundaryParameter"
				id:           lowerBoundaryParameter
				defaultValue: 0.25
				decimals:     4
				visible:      designType.currentValue === "twoSidedAsymmetricBinding" || designType.currentValue === "twoSidedAsymmetricNonBinding"
				enabled:      lowerBoundary.currentValue !== "obrienFleming" && lowerBoundary.currentValue !== "pocock"
				info:         qsTr("Parameter for the lower spending function.")
			}
		}
	}

	Section
	{
		expanded: true
		title:    qsTr("Plots")

		CheckBox
		{
			label:   qsTr("Stopping boundaries")
			id:      boundariesPlot
			name:    "boundariesPlot"
			checked: true
			info:    qsTr("Display stopping boundaries across analyses.")
		}

		CheckBox
		{
			label:   qsTr("Boundary crossing probabilities")
			id:      crossingProbabilitiesPlot
			name:    "crossingProbabilitiesPlot"
			checked: true
			info:    qsTr("Display cumulative boundary crossing probabilities under H0 and H1.")
		}

		CheckBox
		{
			label:   qsTr("Explanatory text")
			id:      text
			name:    "text"
			checked: true
			info:    qsTr("Display a short explanation of the design.")
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

			Text { Layout.columnSpan: 2; text: qsTr("Integration grid points:") }
			IntegerField
			{
				name:         "gridPoints"
				id:           gridPoints
				min:          1
				max:          80
				defaultValue: 18
				info:         qsTr("Controls the numerical integration grid used by gsDesign.")
			}

			Text { Layout.columnSpan: 2; text: qsTr("Generate R Code:") }
			CheckBox
			{
				name:    "generateRCode"
				id:      generateRCode
				label:   ""
				checked: false
				info:    qsTr("Display the gsDesign R call for the current settings.")
			}
		}
	}
}

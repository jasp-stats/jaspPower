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
	columns: 1

	property string designTypeValue: ""
	property bool usesFutilityBoundary: designTypeValue === "twoSidedAsymmetricBinding" || designTypeValue === "twoSidedAsymmetricNonBinding"
	property bool controlsReady: false
	property string previousUpperBoundary: ""
	property string previousLowerBoundary: ""
	property bool previousUsesFutilityBoundary: false
	property double defaultWangTsiatisParameter: 0.25
	property double defaultUpperHsdParameter: -4
	property double defaultLowerHsdParameter: -2
	property double defaultKimDeMetsParameter: 1
	property double minimumHsdParameter: -40
	property double maximumHsdParameter: 40
	property double minimumKimDeMetsParameter: 0
	property double maximumKimDeMetsParameter: 50

	Component.onCompleted:
	{
		controlsReady = true
		previousUpperBoundary = upperBoundary.currentValue
		previousLowerBoundary = lowerBoundary.currentValue
		previousUsesFutilityBoundary = root.usesFutilityBoundary
		root.syncUpperBoundaryParameter(false)
		root.syncLowerBoundaryParameter(root.usesFutilityBoundary)
	}

	onDesignTypeValueChanged:
	{
		if (controlsReady)
		{
			if (root.usesFutilityBoundary && !previousUsesFutilityBoundary)
				root.syncLowerBoundaryParameter(true)
			previousUsesFutilityBoundary = root.usesFutilityBoundary
		}
	}

	function boundaryUsesParameter(boundary)
	{
		return boundary === "wangTsiatis" || boundary === "hwangShihDeCani" || boundary === "kimDeMetsPower"
	}

	function boundaryParameterDefault(boundary, isUpper)
	{
		if (boundary === "hwangShihDeCani")
			return isUpper ? root.defaultUpperHsdParameter : root.defaultLowerHsdParameter
		if (boundary === "kimDeMetsPower")
			return root.defaultKimDeMetsParameter
		return root.defaultWangTsiatisParameter
	}

	function boundaryParameterMinimum(boundary)
	{
		if (boundary === "hwangShihDeCani")
			return root.minimumHsdParameter
		if (boundary === "kimDeMetsPower")
			return root.minimumKimDeMetsParameter
		return -Infinity
	}

	function boundaryParameterMaximum(boundary)
	{
		if (boundary === "hwangShihDeCani")
			return root.maximumHsdParameter
		if (boundary === "kimDeMetsPower")
			return root.maximumKimDeMetsParameter
		return Infinity
	}

	function boundaryParameterInclusive(boundary)
	{
		if (boundary === "kimDeMetsPower")
			return JASP.MaxOnly
		return JASP.MinMax
	}

	function boundaryParameterValueIsValid(boundary, value)
	{
		var x = Number(value)
		if (!isFinite(x))
			return false
		if (boundary === "hwangShihDeCani")
			return x >= root.minimumHsdParameter && x <= root.maximumHsdParameter
		if (boundary === "kimDeMetsPower")
			return x > root.minimumKimDeMetsParameter && x <= root.maximumKimDeMetsParameter
		return true
	}

	function syncBoundaryParameter(field, boundary, previousBoundary, isUpper, forceDefault)
	{
		if (!root.boundaryUsesParameter(boundary))
			return

		var nextDefault = root.boundaryParameterDefault(boundary, isUpper)
		if (forceDefault || !root.boundaryUsesParameter(previousBoundary) || !root.boundaryParameterValueIsValid(boundary, field.value))
		{
			field.value = nextDefault
			return
		}

		var previousDefault = root.boundaryParameterDefault(previousBoundary, isUpper)
		if (Number(field.value) === previousDefault)
			field.value = nextDefault
	}

	function syncUpperBoundaryParameter(forceDefault)
	{
		root.syncBoundaryParameter(upperBoundaryParameter, upperBoundary.currentValue, previousUpperBoundary, true, forceDefault)
		previousUpperBoundary = upperBoundary.currentValue
	}

	function syncLowerBoundaryParameter(forceDefault)
	{
		root.syncBoundaryParameter(lowerBoundaryParameter, lowerBoundary.currentValue, previousLowerBoundary, false, forceDefault)
		previousLowerBoundary = lowerBoundary.currentValue
	}

	function boundaryParameterSymbol(boundary)
	{
		if (boundary === "wangTsiatis")
			return qsTr("\u0394 (WT)")
		if (boundary === "hwangShihDeCani")
			return qsTr("\u03B3")
		if (boundary === "kimDeMetsPower")
			return qsTr("\u03C1")
		return qsTr("Parameter")
	}

	function boundaryParameterInfo(boundary, isUpper)
	{
		if (boundary === "hwangShihDeCani")
			return qsTr("\u03B3 parameter for Hwang-Shih-DeCani spending; allowable range is [%1, %2]. Default is %3 for the upper boundary and %4 for the lower boundary.")
				.arg(root.minimumHsdParameter)
				.arg(root.maximumHsdParameter)
				.arg(root.defaultUpperHsdParameter)
				.arg(root.defaultLowerHsdParameter)
		if (boundary === "kimDeMetsPower")
			return qsTr("\u03C1 parameter for Kim-DeMets power spending; allowable range is (%1, %2]. Default is %3.")
				.arg(root.minimumKimDeMetsParameter)
				.arg(root.maximumKimDeMetsParameter)
				.arg(root.defaultKimDeMetsParameter)
		if (boundary === "wangTsiatis")
			return qsTr("\u0394 parameter for the Wang-Tsiatis boundary family. Default is %1; \u0394 = 0 gives an O'Brien-Fleming-like boundary and \u0394 = 0.5 gives a Pocock-like boundary.")
				.arg(root.defaultWangTsiatisParameter)
		return isUpper ? qsTr("Parameter for the selected upper boundary.") : qsTr("Parameter for the selected lower spending function.")
	}

	Group
	{
		title: qsTr("Efficacy Boundary")
		columns: 3

		Text { Layout.columnSpan: 2; text: qsTr("Upper boundary:") }
		DropDown
		{
			name: "upperBoundary"
			id:   upperBoundary
			indexDefaultValue: 0
			values: root.usesFutilityBoundary ?
			[
				{ label: qsTr("O'Brien-Fleming spending"), value: "obrienFleming"     },
				{ label: qsTr("Pocock spending"),          value: "pocock"            },
				{ label: qsTr("Hwang-Shih-DeCani"),        value: "hwangShihDeCani"   },
				{ label: qsTr("Kim-DeMets power"),         value: "kimDeMetsPower"    }
			] :
			[
				{ label: qsTr("O'Brien-Fleming"),      value: "obrienFleming"     },
				{ label: qsTr("Pocock"),               value: "pocock"            },
				{ label: qsTr("Wang-Tsiatis"),         value: "wangTsiatis"       },
				{ label: qsTr("Hwang-Shih-DeCani"),    value: "hwangShihDeCani"   },
				{ label: qsTr("Kim-DeMets power"),     value: "kimDeMetsPower"    }
			]
			info: qsTr("Efficacy boundary family. For one-sided and symmetric designs, O'Brien-Fleming and Pocock are boundary types; for asymmetric designs they are Lan-DeMets spending functions.")
			onCurrentValueChanged:
			{
				if (root.controlsReady)
					root.syncUpperBoundaryParameter(false)
			}
		}

		Text
		{
			text:    qsTr("Upper boundary parameter:")
			visible: root.boundaryUsesParameter(upperBoundary.currentValue)
			enabled: root.boundaryUsesParameter(upperBoundary.currentValue)
		}
		Text
		{
			text:    root.boundaryParameterSymbol(upperBoundary.currentValue)
			visible: root.boundaryUsesParameter(upperBoundary.currentValue)
			enabled: root.boundaryUsesParameter(upperBoundary.currentValue)
		}
		DoubleField
		{
			name:         "upperBoundaryParameter"
			id:           upperBoundaryParameter
			defaultValue: root.boundaryParameterDefault(upperBoundary.currentValue, true)
			min:          root.boundaryParameterMinimum(upperBoundary.currentValue)
			max:          root.boundaryParameterMaximum(upperBoundary.currentValue)
			inclusive:    root.boundaryParameterInclusive(upperBoundary.currentValue)
			decimals:     4
			visible:      root.boundaryUsesParameter(upperBoundary.currentValue)
			enabled:      root.boundaryUsesParameter(upperBoundary.currentValue)
			negativeValues: root.boundaryParameterMinimum(upperBoundary.currentValue) < 0
			info:         root.boundaryParameterInfo(upperBoundary.currentValue, true)
		}
	}

	Group
	{
		title: qsTr("Futility Boundary")
		columns: 3
		visible: root.usesFutilityBoundary

		Text { Layout.columnSpan: 2; text: qsTr("Lower boundary:") }
		DropDown
		{
			name: "lowerBoundary"
			id:   lowerBoundary
			indexDefaultValue: 2
			values: [
				{ label: qsTr("O'Brien-Fleming spending"), value: "obrienFleming"     },
				{ label: qsTr("Pocock spending"),          value: "pocock"            },
				{ label: qsTr("Hwang-Shih-DeCani"),        value: "hwangShihDeCani"   },
				{ label: qsTr("Kim-DeMets power"),         value: "kimDeMetsPower"    }
			]
			info: qsTr("Futility boundary family for asymmetric \u03B2-spending designs. Lower-bound spending is computed under the alternative hypothesis.")
			onCurrentValueChanged:
			{
				if (root.controlsReady)
					root.syncLowerBoundaryParameter(false)
			}
		}

		Text
		{
			text:    qsTr("Lower boundary parameter:")
			visible: root.boundaryUsesParameter(lowerBoundary.currentValue)
			enabled: root.boundaryUsesParameter(lowerBoundary.currentValue)
		}
		Text
		{
			text:    root.boundaryParameterSymbol(lowerBoundary.currentValue)
			visible: root.boundaryUsesParameter(lowerBoundary.currentValue)
			enabled: root.boundaryUsesParameter(lowerBoundary.currentValue)
		}
		DoubleField
		{
			name:         "lowerBoundaryParameter"
			id:           lowerBoundaryParameter
			defaultValue: root.boundaryParameterDefault(lowerBoundary.currentValue, false)
			min:          root.boundaryParameterMinimum(lowerBoundary.currentValue)
			max:          root.boundaryParameterMaximum(lowerBoundary.currentValue)
			inclusive:    root.boundaryParameterInclusive(lowerBoundary.currentValue)
			decimals:     4
			visible:      root.boundaryUsesParameter(lowerBoundary.currentValue)
			enabled:      root.boundaryUsesParameter(lowerBoundary.currentValue)
			negativeValues: root.boundaryParameterMinimum(lowerBoundary.currentValue) < 0
			info:         root.boundaryParameterInfo(lowerBoundary.currentValue, false)
		}
	}
}

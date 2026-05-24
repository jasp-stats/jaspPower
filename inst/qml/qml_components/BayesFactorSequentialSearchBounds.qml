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
	title: qsTr("Sample Size Search")
	columns: 3

	property bool usesSampleSizeSearch: false
	property bool linkLowerBoundToStartingPoint: false
	property bool isIndependentSamples: false
	property int startingPoint: 0
	property string maximumSampleSizeSymbol: isIndependentSamples ? "Nmax,1" : "Nmax"

	visible: usesSampleSizeSearch

	function syncSearchBounds()
	{
		if (linkLowerBoundToStartingPoint)
			sampleSizeRangeMin.value = Math.max(2, Math.ceil(startingPoint))

		if (sampleSizeRangeMax.value <= sampleSizeRangeMin.value)
			sampleSizeRangeMax.value = sampleSizeRangeMin.value + 1
	}

	onStartingPointChanged: syncSearchBounds()
	onLinkLowerBoundToStartingPointChanged: syncSearchBounds()
	Component.onCompleted: syncSearchBounds()

	Text
	{
		text: linkLowerBoundToStartingPoint ? qsTr("Lower search bound from first look:") : qsTr("Lower search bound for maximum sample size:")
	}
	Text
	{
		text: maximumSampleSizeSymbol + qsTr(", min")
	}
	IntegerField
	{
		name: "lowerSearchBoundForMaximumSampleSize"
		id:   sampleSizeRangeMin
		info: qsTr("Smallest maximum sample size considered when searching for the target probability of conclusive evidence.")
		min: 2
		defaultValue: 20
		enabled: !linkLowerBoundToStartingPoint
		onValueChanged: syncSearchBounds()
	}

	Text
	{
		text: qsTr("Upper search bound for maximum sample size:")
	}
	Text
	{
		text: maximumSampleSizeSymbol + qsTr(", max")
	}
	IntegerField
	{
		name: "upperSearchBoundForMaximumSampleSize"
		id:   sampleSizeRangeMax
		info: qsTr("Largest maximum sample size considered when searching for the target probability of conclusive evidence.")
		min: sampleSizeRangeMin.value + 1
		defaultValue: 10000
	}
}

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

	property string testValue: ""
	property alias calculationValue: calculationControls.calculationValue
	property bool isIndependentSamples: testValue.indexOf("independentSamples") !== -1
	property bool usesSampleSizeSearch: calculationControls.usesSampleSizeSearch
	property bool usesEvidenceProbability: calculationControls.calculationValue === "evidenceProbability"
	property bool usesStandardErrorSchedule: calculationControls.usesStandardErrorSchedule
	property bool usesGeneralZStandardErrorParameterization: calculationControls.usesGeneralZStandardErrorParameterization
	property bool linksLowerSearchBoundToStartingPoint: usesSampleSizeSearch && lookScheduleMode.currentValue === "increase"
	property int searchStartingSampleSize: sampleSizeFirstLook.value
	property string maximumSampleSizeSymbol: isIndependentSamples ? "Nmax,1" : "Nmax"

	BayesFactorCalculationControls
	{
		id: calculationControls
		testValue: root.testValue
		designType: "sequential"
	}

	Group
	{
		title: qsTr("Information Schedule")
		columns: 3
		visible: root.usesStandardErrorSchedule

		Text { text: qsTr("Standard error schedule:") }
		Text { text: qsTr("SE") }
		TextField
		{
			name: "standardErrorSchedule"
			id:   standardErrorSchedule
			defaultValue: "0.224, 0.158, 0.129, 0.112, 0.100"
			fieldWidth: 140
		}
	}

	Group
	{
		title: qsTr("Look Schedule")
		columns: 3
		visible: !root.usesStandardErrorSchedule

		Text
		{
			text: qsTr("Look schedule:")
		}
		Text
		{
			text: qsTr("Type")
		}
		DropDown
		{
			name: "lookScheduleType"
			id:   lookScheduleMode
			indexDefaultValue: 0
			values: root.usesSampleSizeSearch ?
			[
				{ label: qsTr("Equally spaced information"),  value: "even"     },
				{ label: qsTr("Sample size increase"),        value: "increase" },
				{ label: qsTr("Custom information fractions"), value: "custom"   }
			] :
			[
				{ label: qsTr("Equally spaced sample sizes"), value: "even"     },
				{ label: qsTr("Sample size increase"),        value: "increase" },
				{ label: qsTr("Custom sample-size schedule"), value: "custom"   }
			]
		}

		Text
		{
			text: qsTr("Number of looks:")
			visible: lookScheduleMode.currentValue === "even"
		}
		Text
		{
			text: qsTr("K")
			visible: lookScheduleMode.currentValue === "even"
		}
		IntegerField
		{
			name: "numberOfLooks"
			id:   numberOfLooks
			min: 1
			defaultValue: 5
			visible: lookScheduleMode.currentValue === "even"
		}

		Text
		{
			text: qsTr("First information fraction:")
			visible: root.usesSampleSizeSearch && lookScheduleMode.currentValue === "even"
		}
		Text
		{
			text: qsTr("I1/Imax")
			visible: root.usesSampleSizeSearch && lookScheduleMode.currentValue === "even"
		}
		DoubleField
		{
			name: "firstInformationFraction"
			id:   informationFractionFirstLook
			min: 0
			max: 1
			defaultValue: 0.2
			inclusive: JASP.None
			visible: root.usesSampleSizeSearch && lookScheduleMode.currentValue === "even"
		}

		Text
		{
			text: root.isIndependentSamples ? qsTr("Initial sample size in group 1:") : qsTr("Initial sample size:")
			visible: lookScheduleMode.currentValue === "increase" || (root.usesEvidenceProbability && lookScheduleMode.currentValue === "even")
		}
		Text
		{
			text: root.isIndependentSamples ? "Nmin,1" : "Nmin"
			visible: lookScheduleMode.currentValue === "increase" || (root.usesEvidenceProbability && lookScheduleMode.currentValue === "even")
		}
		IntegerField
		{
			name: "initialSampleSize"
			id:   sampleSizeFirstLook
			min: 2
			defaultValue: 20
			visible: lookScheduleMode.currentValue === "increase" || (root.usesEvidenceProbability && lookScheduleMode.currentValue === "even")
		}

		Text
		{
			text: root.isIndependentSamples ? qsTr("Sample size increase per look in group 1:") : qsTr("Sample size increase per look:")
			visible: lookScheduleMode.currentValue === "increase"
		}
		Text
		{
			text: root.isIndependentSamples ? "\u0394N\u2081" : "\u0394N"
			visible: lookScheduleMode.currentValue === "increase"
		}
		IntegerField
		{
			name: "sampleSizeIncreasePerLook"
			id:   sampleSizeIncrease
			min: 1
			defaultValue: 20
			visible: lookScheduleMode.currentValue === "increase"
		}

		Text
		{
			text: root.isIndependentSamples ? qsTr("Maximum sample size in group 1:") : qsTr("Maximum sample size:")
			visible: root.usesEvidenceProbability && (lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase")
		}
		Text
		{
			text: root.maximumSampleSizeSymbol
			visible: root.usesEvidenceProbability && (lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase")
		}
		IntegerField
		{
			name: "maximumSampleSize"
			id:   sampleSize
			min: 2
			defaultValue: 100
			visible: root.usesEvidenceProbability && (lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase")
		}

		Text
		{
			text: root.isIndependentSamples ? qsTr("Sample size schedule in group 1:") : qsTr("Sample size schedule:")
			visible: root.usesEvidenceProbability && lookScheduleMode.currentValue === "custom"
		}
		Text
		{
			text: root.isIndependentSamples ? "N\u2081" : "N"
			visible: root.usesEvidenceProbability && lookScheduleMode.currentValue === "custom"
		}
		TextField
		{
			name: "sampleSizeSchedule"
			id:   sampleSizeSchedule
			defaultValue: "20, 40, 60, 80, 100"
			fieldWidth: 140
			visible: root.usesEvidenceProbability && lookScheduleMode.currentValue === "custom"
		}

		Text
		{
			text: qsTr("Sample size schedule in group 2:")
			visible: root.isIndependentSamples && root.usesEvidenceProbability && lookScheduleMode.currentValue === "custom"
		}
		Text
		{
			text: "N\u2082"
			visible: root.isIndependentSamples && root.usesEvidenceProbability && lookScheduleMode.currentValue === "custom"
		}
		TextField
		{
			name: "sampleSizeScheduleGroup2"
			id:   sampleSizeSecondGroupSchedule
			defaultValue: "20, 40, 60, 80, 100"
			fieldWidth: 140
			visible: root.isIndependentSamples && root.usesEvidenceProbability && lookScheduleMode.currentValue === "custom"
		}

		Text
		{
			text: qsTr("Information fractions:")
			visible: root.usesSampleSizeSearch && lookScheduleMode.currentValue === "custom"
		}
		Text
		{
			text: qsTr("I/Imax")
			visible: root.usesSampleSizeSearch && lookScheduleMode.currentValue === "custom"
		}
		TextField
		{
			name: "informationFractionSchedule"
			id:   informationFractionSchedule
			defaultValue: "0.2, 0.4, 0.6, 0.8, 1"
			fieldWidth: 140
			visible: root.usesSampleSizeSearch && lookScheduleMode.currentValue === "custom"
		}

		Text
		{
			text: qsTr("Allocation ratio (N\u2082/N\u2081):")
			visible: root.isIndependentSamples && (root.usesSampleSizeSearch || lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase")
		}
		Text
		{
			text: "N\u2082/N\u2081"
			visible: root.isIndependentSamples && (root.usesSampleSizeSearch || lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase")
		}
		DoubleField
		{
			name: "sampleSizeAllocationRatio"
			id:   sampleSizeRatio
			min: 0
			defaultValue: 1
			inclusive: JASP.None
			visible: root.isIndependentSamples && (root.usesSampleSizeSearch || lookScheduleMode.currentValue === "even" || lookScheduleMode.currentValue === "increase")
		}
	}
}

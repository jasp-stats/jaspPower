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
	property string designType: "fixed"
	property bool isSequentialDesign: designType === "sequential"
	property bool isIndependentSamples: testValue.indexOf("independentSamples") !== -1
	property bool isGeneralZ: testValue === "generalZApproximation"
	property bool isZTest: testValue.indexOf("ZTest") !== -1
	property bool isTTest: testValue.indexOf("TTest") !== -1
	property alias calculationValue: calc.currentValue
	property alias generalZParameterizationValue: generalZParameterization.currentValue
	property bool usesSampleSizeSearch: calc.currentValue === "sampleSize"
	property bool usesGeneralZStandardErrorParameterization: isSequentialDesign && isGeneralZ && generalZParameterization.currentValue === "standardErrorSchedule"
	property bool usesStandardErrorSchedule: calc.currentValue === "evidenceProbability" && usesGeneralZStandardErrorParameterization

	Group
	{
		title: qsTr("Calculation Target")
		columns: 3

		Text { Layout.columnSpan: 2; text: qsTr("I want to calculate the ...") }
		DropDown
		{
			name: "calculationTarget"
			id:   calc
			indexDefaultValue: 0
			label: ""
			values: [
				{ label: root.isSequentialDesign ? qsTr("Maximum sample size") : qsTr("Sample Size"), value: "sampleSize"          },
				{ label: qsTr("Pr(Conclusive evidence)"),                                        value: "evidenceProbability" }
			]
		}
	}

	Group
	{
		title: qsTr("Evidence Thresholds")
		columns: 3

		Text { text: qsTr("Conclusive evidence threshold for H\u2081:") }
		Text { text: "BF\u2081\u2080 \u2265 k\u2081" }
		DoubleField
		{
			name: "conclusiveEvidenceThresholdH1"
			id:   bf10Threshold
			min: 1
			defaultValue: 10
			inclusive: JASP.None
		}

		Text { text: qsTr("Conclusive evidence threshold for H\u2080:") }
		Text { text: "BF\u2080\u2081 \u2265 k\u2080" }
		DoubleField
		{
			name: "conclusiveEvidenceThresholdH0"
			id:   bf01Threshold
			min: 1
			defaultValue: 10
			inclusive: JASP.None
		}

		Text { text: qsTr("Probability of conclusive evidence under H\u2081:"); enabled: calc.currentValue !== "evidenceProbability" }
		Text { text: "Pr(BF\u2081\u2080 \u2265 k\u2081)"; enabled: calc.currentValue !== "evidenceProbability" }
		DoubleField
		{
			name: "probabilityOfConclusiveEvidenceUnderH1"
			id:   targetPowerH1
			min: 0
			max: 1
			defaultValue: 0.8
			inclusive: JASP.None
			enabled: calc.currentValue !== "evidenceProbability"
		}

		Text { text: qsTr("Probability of conclusive evidence under H\u2080:"); enabled: calc.currentValue !== "evidenceProbability" }
		Text { text: "Pr(BF\u2080\u2081 \u2265 k\u2080)"; enabled: calc.currentValue !== "evidenceProbability" }
		DoubleField
		{
			name: "probabilityOfConclusiveEvidenceUnderH0"
			id:   targetPowerH0
			min: 0
			max: 1
			defaultValue: 0.8
			inclusive: JASP.None
			enabled: calc.currentValue !== "evidenceProbability"
		}
	}

	Group
	{
		title: qsTr("Test Parameters")
		columns: 3
		visible: root.isZTest || root.isGeneralZ

		Text
		{
			text: qsTr("Known standard deviation:")
			visible: root.isZTest
		}
		Text
		{
			text: "\u03C3"
			visible: root.isZTest
		}
		DoubleField
		{
			name: "knownStandardDeviation"
			id:   standardDeviation
			min: 0
			defaultValue: 1
			inclusive: JASP.None
			visible: root.isZTest
		}

		Text
		{
			text: qsTr("Parameterization:")
			visible: root.isGeneralZ
		}
		DropDown
		{
			name: "generalZParameterization"
			id:   generalZParameterization
			indexDefaultValue: 0
			Layout.columnSpan: 2
			visible: root.isGeneralZ
			values: root.isSequentialDesign && calc.currentValue === "evidenceProbability" ?
			[
				{ label: qsTr("SMD"),                     value: "standardizedMeanDifference" },
				{ label: qsTr("Fisher's z"),              value: "fisherZCorrelation"        },
				{ label: qsTr("logOR"),                   value: "logOddsRatio"              },
				{ label: qsTr("logRR"),                   value: "logRiskRatio"              },
				{ label: qsTr("logHR"),                   value: "logHazardRatio"            },
				{ label: qsTr("logIRR"),                  value: "logIncidenceRateRatio"     },
				{ label: qsTr("Specify"),                 value: "unitInformationSd"         },
				{ label: qsTr("Standard error schedule"), value: "standardErrorSchedule"     }
			] :
			[
				{ label: qsTr("SMD"),        value: "standardizedMeanDifference" },
				{ label: qsTr("Fisher's z"), value: "fisherZCorrelation"        },
				{ label: qsTr("logOR"),      value: "logOddsRatio"              },
				{ label: qsTr("logRR"),      value: "logRiskRatio"              },
				{ label: qsTr("logHR"),      value: "logHazardRatio"            },
				{ label: qsTr("logIRR"),     value: "logIncidenceRateRatio"     },
				{ label: qsTr("Specify"),    value: "unitInformationSd"         }
			]
		}

		Text
		{
			text: qsTr("Unit information SD:")
			visible: root.isGeneralZ && generalZParameterization.currentValue === "unitInformationSd"
		}
		Text
		{
			text: qsTr("UISD")
			visible: root.isGeneralZ && generalZParameterization.currentValue === "unitInformationSd"
		}
		DoubleField
		{
			name: "unitInformationSd"
			id:   unitInformationSd
			min: 0
			defaultValue: 1
			inclusive: JASP.None
			visible: root.isGeneralZ && generalZParameterization.currentValue === "unitInformationSd"
		}
	}

}

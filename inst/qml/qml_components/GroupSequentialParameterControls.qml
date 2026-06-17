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

	property bool usesEffectScale: sampleSizeMode.currentValue === "effectSize"
	property bool usesEffectValue: usesEffectScale && [
		"canonicalDelta", "oneSamplePairedSmd", "independentSamplesSmd"
	].indexOf(effectScale.currentValue) !== -1
	property bool usesMeanDifference: usesEffectScale && [
		"meanDifferenceOneSamplePaired", "meanDifferenceIndependentSamples"
	].indexOf(effectScale.currentValue) !== -1
	property bool usesNormalSd: usesEffectScale && [
		"meanDifferenceOneSamplePaired", "meanDifferenceIndependentSamples"
	].indexOf(effectScale.currentValue) !== -1
	property bool usesAllocationRatio: usesEffectScale && [
		"independentSamplesSmd", "meanDifferenceIndependentSamples",
		"twoSampleBinary", "survivalHazardRatio"
	].indexOf(effectScale.currentValue) !== -1
	property bool usesBinaryEndpoint: usesEffectScale && effectScale.currentValue === "twoSampleBinary"
	property bool usesSurvivalEndpoint: usesEffectScale && effectScale.currentValue === "survivalHazardRatio"
	property bool usesNSurvivalAccrual: usesSurvivalEndpoint && survivalInformationMethod.currentValue === "nSurvival"
	property bool usesGsSurvAccrual: usesSurvivalEndpoint && survivalInformationMethod.currentValue === "gsSurv"
	property bool usesSurvivalAccrual: usesNSurvivalAccrual || usesGsSurvAccrual
	property bool usesBinaryEventRateInput: usesBinaryEndpoint && binaryInputMode.currentValue === "eventRates"
	property bool usesBinaryEffectInput: usesBinaryEndpoint && binaryInputMode.currentValue === "baselineEffect"
	property bool controlsReady: false
	property int previousNumberOfLooks: 3
	property string previousSurvivalInformationMethod: "events"
	property string previousBinaryEffectScale: "riskDifference"
	property double defaultAlpha: 0.025
	property double defaultPower: 0.9
	property int defaultFixedSampleSize: 100
	property double defaultEffectSize: 0.5
	property double defaultNaturalEffectSize: 0.5
	property double defaultEffectStandardDeviation: 1
	property double defaultAllocationRatio: 1
	property double defaultBinaryEventRateGroup1: 0.5
	property double defaultBinaryEventRateGroup2: 0.4
	property double defaultBinaryBaselineEventRate: 0.5
	property double defaultBinaryAlternativeRiskDifference: 0.1
	property double defaultBinaryNullRiskDifference: 0
	property double defaultNullRatio: 1
	property double defaultRatioEffect: 1.3
	property double defaultNSurvivalControlHazard: 0.0833
	property double defaultGsSurvControlHazard: 0.1155
	property double defaultSurvivalDropoutHazard: 0
	property double defaultSurvivalAccrualDuration: 12
	property double defaultNSurvivalStudyDuration: 24
	property double defaultGsSurvStudyDuration: 18
	property double defaultSurvivalEntryGamma: 0.1
	property double defaultSurvivalAccrualRate: 1
	property double defaultSurvivalMinimumFollowup: 6
	property int defaultNumberOfLooks: 3

	Component.onCompleted:
	{
		controlsReady = true
		previousNumberOfLooks = root.lookCount(numberOfLooks.value)
		previousSurvivalInformationMethod = survivalInformationMethod.currentValue
		previousBinaryEffectScale = binaryEffectScale.currentValue
		root.syncTimingDefault(false)
		root.syncSurvivalDefaults(false)
		root.syncBinaryAlternativeDefault(false)
	}

	function lookCount(value)
	{
		var looks = Number(value)
		if (!isFinite(looks) || looks < 2)
			return root.defaultNumberOfLooks
		return Math.round(looks)
	}

	function defaultTimingText(value)
	{
		var looks = root.lookCount(value)
		var values = []
		for (var i = 1; i <= looks; i++)
			values.push(i === looks ? "1" : (i / looks).toFixed(2))
		return values.join(", ")
	}

	function syncTimingDefault(forceDefault)
	{
		var looks = root.lookCount(numberOfLooks.value)
		if (timingMode.currentValue !== "custom")
		{
			previousNumberOfLooks = looks
			return
		}

		var previousDefault = root.defaultTimingText(previousNumberOfLooks)
		var originalDefault = root.defaultTimingText(root.defaultNumberOfLooks)
		if (forceDefault || timing.value === "" || timing.value === previousDefault || timing.value === originalDefault)
			timing.value = root.defaultTimingText(looks)
		previousNumberOfLooks = looks
	}

	function survivalHazardRatioDefault(method)
	{
		return root.defaultRatioEffect
	}

	function survivalControlHazardDefault(method)
	{
		if (method === "nSurvival")
			return root.defaultNSurvivalControlHazard
		return root.defaultGsSurvControlHazard
	}

	function survivalStudyDurationDefault(method)
	{
		if (method === "gsSurv")
			return root.defaultGsSurvStudyDuration
		return root.defaultNSurvivalStudyDuration
	}

	function syncPositiveDefault(field, nextDefault, previousDefault, forceDefault)
	{
		var value = Number(field.value)
		if (forceDefault || !isFinite(value) || value <= 0 ||
			Math.abs(value - previousDefault) < 1e-4)
			field.value = nextDefault
	}

	function syncSurvivalDefaults(forceDefault)
	{
		var method = survivalInformationMethod.currentValue
		var previousMethod = previousSurvivalInformationMethod
		if (method === "nSurvival")
			nullHazardRatio.value = root.defaultNullRatio

		root.syncPositiveDefault(
			hazardRatio,
			root.survivalHazardRatioDefault(method),
			root.survivalHazardRatioDefault(previousMethod),
			forceDefault
		)
		root.syncPositiveDefault(
			survivalControlHazard,
			root.survivalControlHazardDefault(method),
			root.survivalControlHazardDefault(previousMethod),
			forceDefault
		)
		root.syncPositiveDefault(
			survivalStudyDuration,
			root.survivalStudyDurationDefault(method),
			root.survivalStudyDurationDefault(previousMethod),
			forceDefault
		)

		previousSurvivalInformationMethod = method
	}

	function binaryAlternativeEffectDefault(scale)
	{
		if (scale === "riskDifference")
			return root.defaultBinaryAlternativeRiskDifference
		return root.defaultRatioEffect
	}

	function binaryAlternativeEffectLabel(scale)
	{
		if (scale === "riskDifference")
			return qsTr("Alternative risk difference:")
		if (scale === "riskRatio")
			return qsTr("Alternative risk ratio:")
		if (scale === "oddsRatio")
			return qsTr("Alternative odds ratio:")
		return qsTr("Alternative effect:")
	}

	function binaryAlternativeEffectSymbol(scale)
	{
		if (scale === "riskDifference")
			return qsTr("p\u2081 - p\u2082")
		if (scale === "riskRatio")
			return qsTr("RR")
		if (scale === "oddsRatio")
			return qsTr("OR")
		return qsTr("effect")
	}

	function binaryAlternativeEffectInfo(scale)
	{
		if (scale === "riskDifference")
			return qsTr("Alternative risk difference p1 - p2. With the default baseline event probability 0.5, the default difference 0.1 implies p2 = 0.4.")
		if (scale === "riskRatio")
			return qsTr("Alternative risk ratio p1 / p2. The default example effect is 1.3.")
		if (scale === "oddsRatio")
			return qsTr("Alternative odds ratio comparing p1 to p2. The default example effect is 1.3.")
		return qsTr("Alternative effect used to derive the second event probability.")
	}

	function binaryEffectScaleInfo(inputMode)
	{
		if (inputMode === "eventRates")
			return qsTr("Scale used for the binary fixed-design sample-size calculation and null margin. The event probabilities are entered directly.")
		return qsTr("Scale used to derive the second event probability and to set the binary fixed-design sample-size calculation.")
	}

	function binaryAlternativeEffectMinimum(scale)
	{
		if (scale === "riskDifference")
			return -1
		return 0
	}

	function binaryAlternativeEffectMaximum(scale)
	{
		if (scale === "riskDifference")
			return 1
		return Infinity
	}

	function binaryAlternativeEffectValueIsValid(scale, value)
	{
		var x = Number(value)
		if (!isFinite(x))
			return false
		if (scale === "riskDifference")
			return x > -1 && x < 1
		return x > 0
	}

	function syncBinaryAlternativeDefault(forceDefault)
	{
		var scale = binaryEffectScale.currentValue
		var previousScale = previousBinaryEffectScale
		var previousDefault = root.binaryAlternativeEffectDefault(previousScale)
		var value = Number(binaryAlternativeEffect.value)

		if (forceDefault || !root.binaryAlternativeEffectValueIsValid(scale, value) ||
			Math.abs(value - previousDefault) < 1e-4)
			binaryAlternativeEffect.value = root.binaryAlternativeEffectDefault(scale)

		previousBinaryEffectScale = scale
	}

	function effectValueLabel(scale)
	{
		if (scale === "canonicalDelta")
			return qsTr("Canonical effect:")
		if (scale === "oneSamplePairedSmd" || scale === "independentSamplesSmd")
			return qsTr("Standardized mean difference:")
		return qsTr("Effect magnitude:")
	}

	function effectValueSymbol(scale)
	{
		if (scale === "canonicalDelta")
			return qsTr("\u03B4")
		if (scale === "oneSamplePairedSmd" || scale === "independentSamplesSmd")
			return qsTr("SMD")
		return qsTr("effect")
	}

	function effectValueInfo(scale)
	{
		if (scale === "canonicalDelta")
			return qsTr("Canonical information-scale effect. This is not necessarily Cohen's d.")
		if (scale === "oneSamplePairedSmd")
			return qsTr("Standardized mean difference for a one-sample design, or mean paired difference divided by the SD of paired differences for a paired design.")
		if (scale === "independentSamplesSmd")
			return qsTr("Standardized mean difference for two independent groups. The design uses total sample size as information and adjusts for the allocation ratio.")
		return qsTr("Effect magnitude used to compute the information-scale \u03B4.")
	}

	Group
	{
		title: qsTr("Error Rates")
		columns: 3

		Text { text: qsTr("Type I error rate:") }
		Text { text: qsTr("\u03B1") }
		DoubleField
		{
			name:         "alpha"
			id:           alpha
			min:          0
			max:          1
			defaultValue: root.defaultAlpha
			decimals:     4
			inclusive:    JASP.None
			info:         qsTr("One-sided upper-bound Type I error rate. For symmetric two-sided designs, the total Type I error rate is 2\u03B1.")
		}

		Text { text: qsTr("Target power:") }
		Text { text: qsTr("1 - \u03B2") }
		DoubleField
		{
			name:         "power"
			id:           power
			min:          0
			max:          1
			defaultValue: root.defaultPower
			decimals:     4
			inclusive:    JASP.None
			info:         qsTr("Target power under the alternative hypothesis. gsDesign uses \u03B2 = 1 - power and adjusts the maximum information to achieve this power; for asymmetric \u03B2-spending designs, \u03B2 is also the total lower-bound spending under the alternative.")
		}
	}

	Group
	{
		title: qsTr("Information and Sample Size Scale")
		columns: 3

		Text { Layout.columnSpan: 2; text: qsTr("Design scaling method:") }
		DropDown
		{
			name: "sampleSizeMode"
			id:   sampleSizeMode
			indexDefaultValue: 0
			values: [
				{ label: qsTr("Information ratio"),                 value: "generic"     },
				{ label: qsTr("Fixed design value"),                value: "fixedDesign" },
				{ label: qsTr("Information-scale effect (\u03B4)"), value: "effectSize"  }
			]
			info: qsTr("Controls whether the design is scaled by relative information, a fixed-design value, or an effect-specific sample-size calculation.")
		}

		Text
		{
			text:    qsTr("Fixed design value:")
			visible: sampleSizeMode.currentValue === "fixedDesign"
		}
		Text
		{
			text:    qsTr("N, info, or events")
			visible: sampleSizeMode.currentValue === "fixedDesign"
		}
		IntegerField
		{
			name:         "fixedSampleSize"
			id:           fixedSampleSize
			min:          2
			defaultValue: root.defaultFixedSampleSize
			visible:      sampleSizeMode.currentValue === "fixedDesign"
			info:         qsTr("Fixed-design value with no interim analyses. For simple sample-size designs this is N; for information-based or event-driven designs, enter information units or events.")
		}

		Text { Layout.columnSpan: 2; text: qsTr("Effect scale:"); visible: root.usesEffectScale }
		DropDown
		{
			name: "effectScale"
			id:   effectScale
			indexDefaultValue: 0
			visible: root.usesEffectScale
			info: qsTr("Scale used to convert the planned effect to the information scale. Endpoint-specific binary and survival choices first compute fixed-design sample size or events.")
			values: [
				{ label: qsTr("Canonical effect (\u03B4)"),              value: "canonicalDelta"                  },
				{ label: qsTr("Standardized mean difference (one sample / paired)"), value: "oneSamplePairedSmd"  },
				{ label: qsTr("Standardized mean difference (independent samples)"), value: "independentSamplesSmd" },
				{ label: qsTr("Mean difference (one sample / paired)"),  value: "meanDifferenceOneSamplePaired" },
				{ label: qsTr("Mean difference (independent samples)"),  value: "meanDifferenceIndependentSamples" },
				{ label: qsTr("Two-sample binary endpoint"),             value: "twoSampleBinary"                },
				{ label: qsTr("Survival hazard ratio"),                  value: "survivalHazardRatio"            }
			]
		}

		Text { text: root.effectValueLabel(effectScale.currentValue); visible: root.usesEffectValue }
		Text { text: root.effectValueSymbol(effectScale.currentValue); visible: root.usesEffectValue }
		DoubleField
		{
			name:         "effectSize"
			id:           effectSize
			min:          0
			defaultValue: root.defaultEffectSize
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesEffectValue
			info:         root.effectValueInfo(effectScale.currentValue)
		}

		Text
		{
			text:    qsTr("Mean difference:")
			visible: root.usesMeanDifference
		}
		Text { text: qsTr("\u0394"); visible: root.usesMeanDifference }
		DoubleField
		{
			name:         "naturalEffectSize"
			id:           naturalEffectSize
			min:          0
			defaultValue: root.defaultNaturalEffectSize
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesMeanDifference
			info:         qsTr("Absolute effect on the natural scale. Direction is ignored for information planning.")
		}

		Text
		{
			text:    effectScale.currentValue === "meanDifferenceIndependentSamples" ? qsTr("Common standard deviation:") : qsTr("Outcome / difference SD:")
			visible: root.usesNormalSd
		}
		Text { text: qsTr("SD"); visible: root.usesNormalSd }
		DoubleField
		{
			name:         "effectStandardDeviation"
			id:           effectStandardDeviation
			min:          0
			defaultValue: root.defaultEffectStandardDeviation
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesNormalSd
			info:         qsTr("For one-sample designs, use the outcome SD; for paired designs, use the SD of paired differences; for independent samples, use the common outcome SD.")
		}

		Text { text: qsTr("Allocation ratio:"); visible: root.usesAllocationRatio }
		Text { text: qsTr("N\u2082/N\u2081"); visible: root.usesAllocationRatio }
		DoubleField
		{
			name:         "sampleSizeAllocationRatio"
			id:           sampleSizeRatio
			min:          0
			defaultValue: root.defaultAllocationRatio
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesAllocationRatio
			info:         qsTr("Ratio of group 2 to group 1 sample size. For survival endpoints, treat group 2 as experimental and group 1 as control.")
		}

		Text { Layout.columnSpan: 2; text: qsTr("Binary endpoint input:"); visible: root.usesBinaryEndpoint }
		DropDown
		{
			name: "binaryInputMode"
			id:   binaryInputMode
			indexDefaultValue: 0
			visible: root.usesBinaryEndpoint
			info: qsTr("Choose whether to enter both event probabilities directly or enter a baseline event probability and an alternative effect.")
			values: [
				{ label: qsTr("Baseline event probability + effect"), value: "baselineEffect" },
				{ label: qsTr("Event probabilities (p\u2081, p\u2082)"), value: "eventRates"     }
			]
		}

		Text { text: qsTr("Event probability group 1:"); visible: root.usesBinaryEventRateInput }
		Text { text: qsTr("p\u2081"); visible: root.usesBinaryEventRateInput }
		DoubleField
		{
			name:         "binaryEventRateGroup1"
			id:           binaryEventRateGroup1
			min:          0
			max:          1
			defaultValue: root.defaultBinaryEventRateGroup1
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesBinaryEventRateInput
			info:         qsTr("Event probability in group 1 under the alternative hypothesis.")
		}

		Text { text: qsTr("Event probability group 2:"); visible: root.usesBinaryEventRateInput }
		Text { text: qsTr("p\u2082"); visible: root.usesBinaryEventRateInput }
		DoubleField
		{
			name:         "binaryEventRateGroup2"
			id:           binaryEventRateGroup2
			min:          0
			max:          1
			defaultValue: root.defaultBinaryEventRateGroup2
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesBinaryEventRateInput
			info:         qsTr("Event probability in group 2 under the alternative hypothesis.")
		}

		Text { text: qsTr("Baseline event probability:"); visible: root.usesBinaryEffectInput }
		Text { text: qsTr("p\u2081"); visible: root.usesBinaryEffectInput }
		DoubleField
		{
			name:         "binaryBaselineEventRate"
			id:           binaryBaselineEventRate
			min:          0
			max:          1
			defaultValue: root.defaultBinaryBaselineEventRate
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesBinaryEffectInput
			info:         qsTr("Event probability for group 1 under the alternative hypothesis. A value near 0.5 is conservative for risk-difference planning because it gives the largest Bernoulli variance.")
		}

		Text { Layout.columnSpan: 2; text: qsTr("Binary effect scale:"); visible: root.usesBinaryEndpoint }
		DropDown
		{
			name: "binaryEffectScale"
			id:   binaryEffectScale
			indexDefaultValue: 0
			visible: root.usesBinaryEndpoint
			info: root.binaryEffectScaleInfo(binaryInputMode.currentValue)
			values: [
				{ label: qsTr("Risk difference"), value: "riskDifference" },
				{ label: qsTr("Risk ratio"),      value: "riskRatio"      },
				{ label: qsTr("Odds ratio"),      value: "oddsRatio"      }
			]
			onCurrentValueChanged:
			{
				if (root.controlsReady)
					root.syncBinaryAlternativeDefault(false)
			}
		}

		Text { text: root.binaryAlternativeEffectLabel(binaryEffectScale.currentValue); visible: root.usesBinaryEffectInput }
		Text { text: root.binaryAlternativeEffectSymbol(binaryEffectScale.currentValue); visible: root.usesBinaryEffectInput }
		DoubleField
		{
			name:         "binaryAlternativeEffect"
			id:           binaryAlternativeEffect
			min:          root.binaryAlternativeEffectMinimum(binaryEffectScale.currentValue)
			max:          root.binaryAlternativeEffectMaximum(binaryEffectScale.currentValue)
			defaultValue: root.binaryAlternativeEffectDefault(binaryEffectScale.currentValue)
			decimals:     4
			inclusive:    JASP.None
			negativeValues: binaryEffectScale.currentValue === "riskDifference"
			visible:      root.usesBinaryEffectInput
			info:         root.binaryAlternativeEffectInfo(binaryEffectScale.currentValue)
		}

		Text { text: qsTr("Null risk difference:"); visible: root.usesBinaryEndpoint && binaryEffectScale.currentValue === "riskDifference" }
		Text { text: qsTr("p\u2081 - p\u2082"); visible: root.usesBinaryEndpoint && binaryEffectScale.currentValue === "riskDifference" }
		DoubleField
		{
			name:           "binaryNullRiskDifference"
			id:             binaryNullRiskDifference
			defaultValue:   root.defaultBinaryNullRiskDifference
			decimals:       4
			negativeValues: true
			visible:        root.usesBinaryEndpoint && binaryEffectScale.currentValue === "riskDifference"
			info:           qsTr("Null risk difference p1 - p2. Use 0 for equal event probabilities.")
		}

		Text { text: qsTr("Null risk ratio:"); visible: root.usesBinaryEndpoint && binaryEffectScale.currentValue === "riskRatio" }
		Text { text: qsTr("RR\u2080"); visible: root.usesBinaryEndpoint && binaryEffectScale.currentValue === "riskRatio" }
		DoubleField
		{
			name:         "binaryNullRiskRatio"
			id:           binaryNullRiskRatio
			min:          0
			defaultValue: root.defaultNullRatio
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesBinaryEndpoint && binaryEffectScale.currentValue === "riskRatio"
			info:         qsTr("Null risk ratio p1 / p2. Use 1 for equal event probabilities.")
		}

		Text { text: qsTr("Null odds ratio:"); visible: root.usesBinaryEndpoint && binaryEffectScale.currentValue === "oddsRatio" }
		Text { text: qsTr("OR\u2080"); visible: root.usesBinaryEndpoint && binaryEffectScale.currentValue === "oddsRatio" }
		DoubleField
		{
			name:         "binaryNullOddsRatio"
			id:           binaryNullOddsRatio
			min:          0
			defaultValue: root.defaultNullRatio
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesBinaryEndpoint && binaryEffectScale.currentValue === "oddsRatio"
			info:         qsTr("Null odds ratio. Use 1 for equal event probabilities.")
		}

		Text { text: qsTr("Alternative hazard ratio:"); visible: root.usesSurvivalEndpoint }
		Text { text: qsTr("HR"); visible: root.usesSurvivalEndpoint }
		DoubleField
		{
			name:         "hazardRatio"
			id:           hazardRatio
			min:          0
			defaultValue: root.survivalHazardRatioDefault(survivalInformationMethod.currentValue)
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesSurvivalEndpoint
			info:         qsTr("Experimental/control hazard ratio under the alternative hypothesis. The default example effect is 1.3.")
		}

		Text { text: qsTr("Null hazard ratio:"); visible: root.usesSurvivalEndpoint; enabled: !root.usesNSurvivalAccrual }
		Text { text: qsTr("HR\u2080"); visible: root.usesSurvivalEndpoint; enabled: !root.usesNSurvivalAccrual }
		DoubleField
		{
			name:         "nullHazardRatio"
			id:           nullHazardRatio
			min:          0
			defaultValue: root.defaultNullRatio
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesSurvivalEndpoint
			enabled:      !root.usesNSurvivalAccrual
			info:         root.usesNSurvivalAccrual ? qsTr("The subjects + events survival calculation supports only HR0 = 1. Use events only or group sequential accrual for a non-null hazard-ratio margin.") : qsTr("Hazard ratio specified by the null hypothesis.")
		}

		Text { Layout.columnSpan: 2; text: qsTr("Survival information method:"); visible: root.usesSurvivalEndpoint }
		DropDown
		{
			name: "survivalInformationMethod"
			id:   survivalInformationMethod
			indexDefaultValue: 0
			visible: root.usesSurvivalEndpoint
			info: qsTr("Selects whether survival information is planned as events only, as fixed-design subjects plus events, or directly as a group sequential survival design with study-time accrual assumptions.")
			values: [
				{ label: qsTr("Events only"),                        value: "events"    },
				{ label: qsTr("Subjects + events"),                  value: "nSurvival" },
				{ label: qsTr("Group sequential accrual"),           value: "gsSurv"    }
			]
			onCurrentValueChanged:
			{
				if (root.controlsReady)
					root.syncSurvivalDefaults(false)
			}
		}

		Text { text: qsTr("Control hazard:"); visible: root.usesSurvivalAccrual }
		Text { text: qsTr("\u03BBC"); visible: root.usesSurvivalAccrual }
		DoubleField
		{
			name:         "survivalControlHazard"
			id:           survivalControlHazard
			min:          0
			defaultValue: root.survivalControlHazardDefault(survivalInformationMethod.currentValue)
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesSurvivalAccrual
			info:         qsTr("Control-group event hazard per time unit. Use the same time unit as accrual, study duration, follow-up, and dropout hazards. Defaults to 0.0833 for subjects + events and 0.1155 for group sequential accrual.")
		}

		Text { text: qsTr("Dropout hazard:"); visible: root.usesSurvivalAccrual }
		Text { text: qsTr("\u03B7"); visible: root.usesSurvivalAccrual }
		DoubleField
		{
			name:         "survivalDropoutHazard"
			id:           survivalDropoutHazard
			min:          0
			defaultValue: root.defaultSurvivalDropoutHazard
			decimals:     4
			inclusive:    JASP.MinMax
			visible:      root.usesSurvivalAccrual
			info:         qsTr("Equal dropout hazard for both groups, per time unit. Use 0 for no dropout.")
		}

		Text { text: qsTr("Accrual duration:"); visible: root.usesSurvivalAccrual }
		Text { text: root.usesNSurvivalAccrual ? qsTr("Tr") : qsTr("R"); visible: root.usesSurvivalAccrual }
		DoubleField
		{
			name:         "survivalAccrualDuration"
			id:           survivalAccrualDuration
			min:          0
			defaultValue: root.defaultSurvivalAccrualDuration
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesSurvivalAccrual
			info:         qsTr("Enrollment duration in the same time unit as the survival hazards and study duration.")
		}

		Text { text: qsTr("Study duration:"); visible: root.usesSurvivalAccrual }
		Text { text: root.usesNSurvivalAccrual ? qsTr("Ts") : qsTr("T"); visible: root.usesSurvivalAccrual }
		DoubleField
		{
			name:         "survivalStudyDuration"
			id:           survivalStudyDuration
			min:          0
			defaultValue: root.survivalStudyDurationDefault(survivalInformationMethod.currentValue)
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesSurvivalAccrual
			info:         qsTr("Maximum study duration. Use the same time unit as the control hazard and accrual duration. Defaults to 24 for subjects + events and 18 for group sequential accrual.")
		}

		Text { text: qsTr("Entry distribution:"); visible: root.usesNSurvivalAccrual }
		Text { text: qsTr("entry"); visible: root.usesNSurvivalAccrual }
		DropDown
		{
			name: "survivalEntry"
			id:   survivalEntry
			indexDefaultValue: 0
			visible: root.usesNSurvivalAccrual
			info: qsTr("Patient entry distribution during the enrollment period.")
			values: [
				{ label: qsTr("Uniform"),     value: "unif" },
				{ label: qsTr("Exponential"), value: "expo" }
			]
		}

		Text { text: qsTr("Entry rate parameter:"); visible: root.usesNSurvivalAccrual && survivalEntry.currentValue === "expo" }
		Text { text: qsTr("\u03B3"); visible: root.usesNSurvivalAccrual && survivalEntry.currentValue === "expo" }
		DoubleField
		{
			name:           "survivalEntryGamma"
			id:             survivalEntryGamma
			defaultValue:   root.defaultSurvivalEntryGamma
			decimals:       4
			negativeValues: true
			visible:        root.usesNSurvivalAccrual && survivalEntry.currentValue === "expo"
			info:           qsTr("Non-zero \u03B3 for exponential entry; positive values are convex and negative values are concave. The default is a non-zero starting value.")
		}

		Text { text: qsTr("Accrual-rate pattern:"); visible: root.usesGsSurvAccrual }
		Text { text: qsTr("\u03B3"); visible: root.usesGsSurvAccrual }
		DoubleField
		{
			name:         "survivalAccrualRate"
			id:           survivalAccrualRate
			min:          0
			defaultValue: root.defaultSurvivalAccrualRate
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesGsSurvAccrual
			info:         qsTr("Enrollment-rate pattern. With fixed study duration and minimum follow-up, the calculation rescales this value to the required accrual rate; the default scalar 1 gives constant accrual.")
		}

		Text { text: qsTr("Minimum follow-up:"); visible: root.usesGsSurvAccrual }
		Text { text: qsTr("minfup"); visible: root.usesGsSurvAccrual }
		DoubleField
		{
			name:         "survivalMinimumFollowup"
			id:           survivalMinimumFollowup
			min:          0
			defaultValue: root.defaultSurvivalMinimumFollowup
			decimals:     4
			inclusive:    JASP.None
			visible:      root.usesGsSurvAccrual
			info:         qsTr("Minimum follow-up duration in the same time unit as the study duration.")
		}

		Text { Layout.columnSpan: 2; text: qsTr("Accrual calculation method:"); visible: root.usesGsSurvAccrual }
		DropDown
		{
			name: "survivalGsSurvMethod"
			id:   survivalGsSurvMethod
			indexDefaultValue: 0
			visible: root.usesGsSurvAccrual
			info: qsTr("Method used for the fixed-design survival calculation inside the group sequential accrual design.")
			values: [
				{ label: qsTr("Lachin-Foulkes"),   value: "LachinFoulkes"   },
				{ label: qsTr("Schoenfeld"),       value: "Schoenfeld"       },
				{ label: qsTr("Freedman"),         value: "Freedman"         },
				{ label: qsTr("Bernstein-Lagakos"), value: "BernsteinLagakos" }
			]
		}
	}

	Group
	{
		title: qsTr("Look Schedule")
		columns: 3

		Text { text: qsTr("Number of looks:") }
		Text { text: "K" }
		IntegerField
		{
			name:         "numberOfLooks"
			id:           numberOfLooks
			min:          2
			max:          30
			defaultValue: root.defaultNumberOfLooks
			info:         qsTr("Number of planned analyses, including the final analysis.")
			onValueChanged:
			{
				if (root.controlsReady)
					root.syncTimingDefault(false)
			}
		}

		Text { Layout.columnSpan: 2; text: qsTr("Look schedule:") }
		DropDown
		{
			name: "timingMode"
			id:   timingMode
			indexDefaultValue: 0
			values: [
				{ label: qsTr("Equally spaced information"),    value: "even"   },
				{ label: qsTr("Custom information fractions"),  value: "custom" }
			]
			info: qsTr("Controls the relative timing of interim analyses on the planned information scale; this is information time, not calendar time.")
			onCurrentValueChanged:
			{
				if (root.controlsReady)
					root.syncTimingDefault(false)
			}
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
			defaultValue: root.defaultTimingText(numberOfLooks.value)
			fieldWidth:   140
			visible:      timingMode.currentValue === "custom"
			info:         qsTr("Increasing information fractions. Supply K values ending in 1, or K - 1 interim values strictly below 1. The default schedule is generated from K.")
		}
	}
}

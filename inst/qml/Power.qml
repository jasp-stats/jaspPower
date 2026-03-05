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
	readonly property bool isIndependentSamplesTTest: test.currentValue === "independentSamplesTTest"
	readonly property bool isPairedSamplesTTest: test.currentValue === "pairedSamplesTTest"
	readonly property bool isOneSampleTTest: test.currentValue === "oneSampleTTest"
	readonly property bool isOneSampleZTest: test.currentValue === "oneSampleZTest"
	readonly property bool isOneSampleProportionTest: test.currentValue === "oneSampleProportion"
	readonly property bool isTwoSamplesProportionTest: test.currentValue === "twoSamplesProportion"
	readonly property bool isOneSampleVarianceRatioTest: test.currentValue === "oneSampleVarianceRatio"
	readonly property bool isTwoSamplesVarianceRatioTest: test.currentValue === "twoSamplesVarianceRatio"
	readonly property bool isBayesianOneSampleTTest: test.currentValue === "bayesianOneSampleTTest"
	readonly property bool isBayesianIndependentSamplesTTest: test.currentValue === "bayesianIndependentSamplesTTest"
	readonly property bool isBayesianOneSampleProportion: test.currentValue === "bayesianOneSampleProportion"
	readonly property bool isBayesianTest: isBayesianOneSampleTTest || isBayesianIndependentSamplesTTest || isBayesianOneSampleProportion
	readonly property bool isFrequentistTOrZTest: isIndependentSamplesTTest || isPairedSamplesTTest || isOneSampleTTest || isOneSampleZTest
	readonly property bool isSingleSampleSizeTest: isOneSampleTTest || isOneSampleZTest || isOneSampleProportionTest || isOneSampleVarianceRatioTest || isBayesianOneSampleTTest || isBayesianOneSampleProportion
	readonly property bool isPerGroupSampleSizeTest: isIndependentSamplesTTest || isPairedSamplesTTest || isTwoSamplesProportionTest || isTwoSamplesVarianceRatioTest || isBayesianIndependentSamplesTTest
	readonly property bool hasSampleSizeRatio: isIndependentSamplesTTest || isTwoSamplesProportionTest || isTwoSamplesVarianceRatioTest || isBayesianIndependentSamplesTTest

	DropDown
	{
		name: "test"
		id:   test
		indexDefaultValue: 0
		label: qsTr("Statistical test:")
			values: [
				{ label: "Independent Samples T-Test", value: "independentSamplesTTest" },
				{ label: "Paired Samples T-Test",  value: "pairedSamplesTTest"          },
				{ label: "One Sample T-Test",  value: "oneSampleTTest"           },
				{ label: "One Sample Z-Test",  value: "oneSampleZTest"           },
				{ label: "One Sample Proportion Test",  value: "oneSampleProportion"     },
				{ label: "Two Samples Proportion Test",  value: "twoSamplesProportion"    },
				{ label: "One Sample Variance Ratio Test",  value: "oneSampleVarianceRatio" },
				{ label: "Two Samples Variance Ratio Test",  value: "twoSamplesVarianceRatio"},
				{ label: "Bayesian One Sample T-Test", value: "bayesianOneSampleTTest" },
				{ label: "Bayesian Independent Samples T-Test", value: "bayesianIndependentSamplesTTest" },
				{ label: "Bayesian One Sample Proportion Test", value: "bayesianOneSampleProportion" }
				//{ label: "One Sample Poisson Rate Test",  value: "oneSamplePoisson"   },
				//{ label: "Two Samples Poisson Rate Test",  value: "twoSamplesPoisson"  }
				//{ label: "ANOVA",  value: "anova" }
		]
	}

	Section
	{
		expanded: true
		visible: test.currentValue !== 'anova'
		title: qsTr("Parameters")
		columns: 1

		Group
		{
			columns: 3

			Text { Layout.columnSpan: 2; text: qsTr("I want to calculate the ..."); visible: !isBayesianTest }
			DropDown
			{
				name: "calculation"
				id:   calc
				indexDefaultValue: 0
				label: ""
				visible: !isBayesianTest
				values: [
					{ label: "Sample Size N", value: "sampleSize"},
					{ label: "Power",  value: "power" },
					{ label: "Effect size",  value: "effectSize"}
				]
			}
			Text { Layout.columnSpan: 2; text: qsTr("I want to calculate the ..."); visible: isBayesianTest }
			DropDown
			{
				name: "bayesianCalculation"
				id: bayesianCalc
				indexDefaultValue: 0
				label: ""
				visible: isBayesianTest
				values: [
					{ label: "Required sample size", value: "sampleSize" },
					{ label: "Evidence rates", value: "evidenceRates" }
				]
			}

			Text
			{
				Layout.columnSpan: 2;
				text: qsTr("Direction of the effect:")
				visible: (isOneSampleProportionTest || isTwoSamplesProportionTest || isOneSampleVarianceRatioTest || isTwoSamplesVarianceRatioTest) && calc.currentValue == "effectSize" && alt.value == "twoSided"
				enabled: calc.currentValue == "effectSize"
			}
			DropDown
			{
				id: direction
				name: "effectDirection"
				label: ""
				visible: (isOneSampleProportionTest || isTwoSamplesProportionTest || isOneSampleVarianceRatioTest || isTwoSamplesVarianceRatioTest) && calc.currentValue == "effectSize" && alt.value == "twoSided"
				enabled: calc.currentValue == "effectSize"
				values: [
					{ label: (isOneSampleProportionTest || isTwoSamplesProportionTest) ? (isOneSampleProportionTest ? qsTr("p\u2081 > p\u2080") : qsTr("p\u2081 > p\u2082")) : qsTr("\u03C1 > 1"), value: "greater"},
					{ label: (isOneSampleProportionTest || isTwoSamplesProportionTest) ? (isOneSampleProportionTest ? qsTr("p\u2081 < p\u2080") : qsTr("p\u2081 < p\u2082")) : qsTr("\u03C1 < 1"),  value: "less" }
				]
			}

			Text
			{
				text: isOneSampleProportionTest ? qsTr("Hypothesized proportion") : (isBayesianOneSampleProportion ? qsTr("Null proportion") : qsTr("Baseline proportion"))
				visible: isOneSampleProportionTest || isTwoSamplesProportionTest || isBayesianOneSampleProportion
			}
			Text
			{
				visible: isOneSampleProportionTest || isTwoSamplesProportionTest || isBayesianOneSampleProportion
				text: isOneSampleProportionTest || isBayesianOneSampleProportion ? qsTr("p₀") : qsTr("p₂")
			}
			DoubleField
			{
				id: p0
				name: "baselineProportion"
				min: 0
				max: 1
				defaultValue: 0.5
				inclusive: JASP.None
				visible: isOneSampleProportionTest || isTwoSamplesProportionTest || isBayesianOneSampleProportion
			}

			Text
			{
				text: qsTr("Comparison proportion")
				visible: isOneSampleProportionTest || isTwoSamplesProportionTest
				enabled: calc.currentValue != "effectSize"
			}
			Text
			{
				text: qsTr("p₁")
				visible: isOneSampleProportionTest || isTwoSamplesProportionTest
				enabled: calc.currentValue != "effectSize"
			}
			DoubleField
			{
				id: p1
				name: "comparisonProportion"
				min: 0
				max: 1
				defaultValue: 0.6
				inclusive: JASP.None
				visible: isOneSampleProportionTest || isTwoSamplesProportionTest
				enabled: calc.currentValue != "effectSize"
			}

			Text
			{
				text: qsTr("Minimal effect size of interest:")
				visible: isFrequentistTOrZTest
				enabled: calc.currentValue != "effectSize"
			}
			Text
			{
				text: qsTr("|δ|")
				visible: isFrequentistTOrZTest
				enabled: calc.currentValue != "effectSize"
			}
			DoubleField
			{
				id: es
				name: "effectSize"
				defaultValue: 0.5
				visible: isFrequentistTOrZTest
				enabled: calc.currentValue != "effectSize"
			}

			Text
			{
				text: qsTr("Minimal effect size of interest:")
				visible: isOneSampleVarianceRatioTest || isTwoSamplesVarianceRatioTest
				enabled: calc.currentValue != "effectSize"
			}
			Text
			{
				text: isTwoSamplesVarianceRatioTest ? qsTr("\u03C1 (\u03C3\u2081\u00B2/\u03C3\u2082\u00B2)") : qsTr("\u03C1 (\u03C3\u00B2/\u03C3\u2080\u00B2)")
				visible: isOneSampleVarianceRatioTest || isTwoSamplesVarianceRatioTest
				enabled: calc.currentValue != "effectSize"
			}
			DoubleField
			{
				id: rho
				name: "varianceRatio"
				defaultValue: 2
				visible: isOneSampleVarianceRatioTest || isTwoSamplesVarianceRatioTest
				enabled: calc.currentValue != "effectSize"
			}

			Text
			{
				text: qsTr("Minimal desired power:")
				visible: !isBayesianTest
				enabled: calc.currentValue != "power"
			}
			Text
			{
				text: qsTr("(1-β)")
				visible: !isBayesianTest
				enabled: calc.currentValue != "power"
			}
			DoubleField
			{
				id: power
				name: "power"
				min: 0.1
				max: 1
				defaultValue: 0.9
				visible: !isBayesianTest
				enabled: calc.currentValue != "power"
				inclusive: JASP.MinOnly
			}

			Text { text: qsTr("Type I error rate:"); visible: !isBayesianTest }
			Text { text: qsTr("α"); visible: !isBayesianTest }
			DoubleField
			{
				id: alpha
				name: "alpha"
				min: 0
				max: 1
				defaultValue: 0.05
				inclusive: JASP.None
				visible: !isBayesianTest
			}

			// No groups in single sample t-test
			Text
			{
				text: qsTr("Sample size:")
				visible: isSingleSampleSizeTest
				enabled: isBayesianTest ? bayesianCalc.currentValue == "evidenceRates" : calc.currentValue != "sampleSize"
			}
			Text
			{
				text: qsTr("Sample size per group:")
				visible: isPerGroupSampleSizeTest
				enabled: isBayesianTest ? bayesianCalc.currentValue == "evidenceRates" : calc.currentValue != "sampleSize"
			}
			Text
			{
				text: qsTr("N")
				visible: isSingleSampleSizeTest || isPerGroupSampleSizeTest
				enabled: isBayesianTest ? bayesianCalc.currentValue == "evidenceRates" : calc.currentValue != "sampleSize"
			}
			IntegerField
			{
				id: n
				name: "sampleSize"
				min: 2
				defaultValue: 20
				visible: isSingleSampleSizeTest || isPerGroupSampleSizeTest
				enabled: isBayesianTest ? bayesianCalc.currentValue == "evidenceRates" : calc.currentValue != "sampleSize"
			}

			// No sample size ratio in single sample t-test
			Text
			{
				text: qsTr("Sample size ratio:")
				visible: hasSampleSizeRatio
			}
			Text
			{
				text: qsTr("N₁/N₂")
				visible: hasSampleSizeRatio
			}
			DoubleField
			{
				id: n_ratio
				name: "sampleSizeRatio"
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: hasSampleSizeRatio
			}

			Text { text: qsTr("Alternative Hypothesis:") }
			Text { text: qsTr("H\u2081") }
			DropDown
			{
				name: "alternative"
				id:   alt
				indexDefaultValue: 0
				values: isFrequentistTOrZTest ?
				[
					{ label: "Two-sided", value: "twoSided"},
					{ label: "One-sided",  value: "greater" }
				] :
				[
					{ label: "Two-sided", value: "twoSided"},
					{ label: "Less (One-sided)",  value: "less" },
					{ label: "Greater (One-sided)",  value: "greater"}
				]
			}

			Text
			{
				text: qsTr("BF evidence threshold:")
				visible: isBayesianTest
			}
			Text
			{
				text: qsTr("B")
				visible: isBayesianTest
			}
			DoubleField
			{
				name: "bayesianThreshold"
				min: 1
				defaultValue: 3
				inclusive: JASP.MinOnly
				visible: isBayesianTest
			}

			Text
			{
				text: qsTr("Rate type:")
				visible: isBayesianTest && bayesianCalc.currentValue == "sampleSize"
			}
			Text
			{
				text: qsTr("Target")
				visible: isBayesianTest && bayesianCalc.currentValue == "sampleSize"
			}
			DropDown
			{
				id: bayesianRateType
				name: "bayesianRateType"
				visible: isBayesianTest && bayesianCalc.currentValue == "sampleSize"
				values: [
					{ label: "True/False Positive", value: "positive" },
					{ label: "True/False Negative", value: "negative" }
				]
			}

			Text
			{
				text: qsTr("Target true rate:")
				visible: isBayesianTest && bayesianCalc.currentValue == "sampleSize"
			}
			Text
			{
				text: qsTr("T")
				visible: isBayesianTest && bayesianCalc.currentValue == "sampleSize"
			}
			DoubleField
			{
				name: "bayesianTrueRate"
				min: 0.6
				max: 0.999
				defaultValue: 0.8
				inclusive: JASP.None
				visible: isBayesianTest && bayesianCalc.currentValue == "sampleSize"
			}

			Text
			{
				text: qsTr("Target false rate:")
				visible: isBayesianTest && bayesianCalc.currentValue == "sampleSize"
			}
			Text
			{
				text: qsTr("F")
				visible: isBayesianTest && bayesianCalc.currentValue == "sampleSize"
			}
			DoubleField
			{
				name: "bayesianFalseRate"
				min: 0.001
				max: 0.1
				defaultValue: 0.05
				inclusive: JASP.None
				visible: isBayesianTest && bayesianCalc.currentValue == "sampleSize"
			}

			Text
			{
				text: qsTr("Analysis prior:")
				visible: isBayesianTest
			}
			Text
			{
				text: qsTr("Type")
				visible: isBayesianTest
			}
			DropDown
			{
				id: bayesianPrior
				name: "bayesianPrior"
				visible: isBayesianTest
				values: isBayesianOneSampleProportion ?
				[
					{ label: "Beta", value: "beta" },
					{ label: "Moment", value: "Moment" }
				] :
				[
					{ label: "Normal", value: "Normal" },
					{ label: "Moment", value: "Moment" },
					{ label: "t-distribution", value: "t-distribution" }
				]
			}

			Text
			{
				text: qsTr("Prior location:")
				visible: isBayesianTest && !isBayesianOneSampleProportion
			}
			Text
			{
				text: qsTr("μ")
				visible: isBayesianTest && !isBayesianOneSampleProportion
			}
			DoubleField
			{
				name: "bayesianPriorLocation"
				defaultValue: 0
				visible: isBayesianTest && !isBayesianOneSampleProportion
			}

			Text
			{
				text: qsTr("Prior scale:")
				visible: isBayesianTest && (!isBayesianOneSampleProportion || bayesianPrior.currentValue == "Moment")
			}
			Text
			{
				text: qsTr("σ")
				visible: isBayesianTest && (!isBayesianOneSampleProportion || bayesianPrior.currentValue == "Moment")
			}
			DoubleField
			{
				name: "bayesianPriorScale"
				min: 0
				defaultValue: isBayesianOneSampleProportion ? 0.1 : 0.707
				inclusive: JASP.None
				visible: isBayesianTest && (!isBayesianOneSampleProportion || bayesianPrior.currentValue == "Moment")
			}

			Text
			{
				text: qsTr("Prior degrees of freedom:")
				visible: isBayesianTest && !isBayesianOneSampleProportion && bayesianPrior.currentValue == "t-distribution"
			}
			Text
			{
				text: qsTr("ν")
				visible: isBayesianTest && !isBayesianOneSampleProportion && bayesianPrior.currentValue == "t-distribution"
			}
			DoubleField
			{
				name: "bayesianPriorDf"
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: isBayesianTest && !isBayesianOneSampleProportion && bayesianPrior.currentValue == "t-distribution"
			}

			Text
			{
				text: qsTr("Beta prior α:")
				visible: isBayesianOneSampleProportion && bayesianPrior.currentValue == "beta"
			}
			Text
			{
				text: qsTr("α")
				visible: isBayesianOneSampleProportion && bayesianPrior.currentValue == "beta"
			}
			DoubleField
			{
				name: "bayesianPriorAlpha"
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: isBayesianOneSampleProportion && bayesianPrior.currentValue == "beta"
			}

			Text
			{
				text: qsTr("Beta prior β:")
				visible: isBayesianOneSampleProportion && bayesianPrior.currentValue == "beta"
			}
			Text
			{
				text: qsTr("β")
				visible: isBayesianOneSampleProportion && bayesianPrior.currentValue == "beta"
			}
			DoubleField
			{
				name: "bayesianPriorBeta"
				min: 0
				defaultValue: 1
				inclusive: JASP.None
				visible: isBayesianOneSampleProportion && bayesianPrior.currentValue == "beta"
			}
		}
	}

  // TODO: ANOVA Section

	Section
	{
		expanded: true
		title: qsTr("Plots")

		CheckBox {
			label: qsTr("Power contour plot")
			id: powerContour
			name: "powerContour"
			checked: true
			visible: !isBayesianTest
		}

		CheckBox {
			label: qsTr("Power demonstration")
			id: powerDist
			name: "powerDemonstration"
			checked: false
			visible: !isBayesianTest
		}

		CheckBox {
			label: qsTr("Power curve by effect size")
			id: powerCurveES
			name: "powerByEffectSize"
			checked: true
			visible: !isBayesianTest
		}

		CheckBox {
			label: qsTr("Power curve by N")
			id: powerCurveN
			name: "powerBySampleSize"
			checked: false
			visible: !isBayesianTest
		}

		CheckBox {
			label: qsTr("Explanatory text")
			id: text
			name: "text"
			checked: true
		}
	}
	Section
	{
		expanded: true
		title: qsTr("Data Generation")
		visible: !isBayesianTest

		Group
		{
			id:	parameters
			visible: !isOneSampleProportionTest && !isTwoSamplesProportionTest
			columns: 2

			Group
			{
				title: qsTr("Parameters")
				DoubleField
				{
					name: "firstGroupMean"
					label: isOneSampleVarianceRatioTest ? qsTr("\u0078\u0305") : qsTr("\u0078\u0305\u2081")
					defaultValue: 0
					visible: isOneSampleVarianceRatioTest || isTwoSamplesVarianceRatioTest
				}

				DoubleField
				{
					name: "secondGroupMean"
					label: qsTr("\u0078\u0305\u2082")
					defaultValue: 0
					visible: isIndependentSamplesTTest || isPairedSamplesTTest || isTwoSamplesVarianceRatioTest
				}

				DoubleField
				{
					name: "testValue"
					label: qsTr("\u03BC\u2080")
					defaultValue: 0
					visible: isOneSampleTTest || isOneSampleZTest
				}

				DoubleField
				{
					name: "firstGroupSd"
					label: isOneSampleTTest ? qsTr("s") : qsTr("s\u2081")
					defaultValue: 1
					visible: isIndependentSamplesTTest || isPairedSamplesTTest || isOneSampleTTest
				}

				DoubleField
				{
					name: "populationSd"
					label: isOneSampleZTest ? qsTr("\u03C3") : qsTr("\u03C3\u2080")
					defaultValue: 1
					visible: isOneSampleZTest || isOneSampleVarianceRatioTest
				}

				DoubleField
				{
					name: "secondGroupSd"
					label: qsTr("s\u2082")
					defaultValue: 1
					visible: isIndependentSamplesTTest || isPairedSamplesTTest || isTwoSamplesVarianceRatioTest
				}
			}

			RadioButtonGroup
			{
				name: "effectDirectionSyntheticDataset"
				title: qsTr("Effect direction")
				visible: !isOneSampleVarianceRatioTest && !isTwoSamplesVarianceRatioTest
				RadioButton { value: "less"; label: (isOneSampleTTest || isOneSampleZTest) ? qsTr("\u0078\u0305 < \u03BC\u2080") : qsTr("\u0078\u0305\u2081 < \u0078\u0305\u2082"); checked: true }
				RadioButton { value: "greater"; label: (isOneSampleTTest || isOneSampleZTest) ? qsTr("\u0078\u0305 > \u03BC\u2080") : qsTr("\u0078\u0305\u2081 > \u0078\u0305\u2082") }
			}
		}

		Group
		{
			title: qsTr("Export synthetic dataset")
			FileSelector
			{
				id:						    savePath
				name:					    "savePath"
				label:				    qsTr("Save as")
				placeholderText:	qsTr("e.g., location/power.csv")
				filter:					  "*.csv"
				save:					    true
				fieldWidth:				180 * preferencesModel.uiScale
			}

			CheckBox
			{
				id:						saveDataset
				name:					"saveDataset"
				text:					qsTr("Save generated dataset")
				enabled:			savePath.value != ""
				Layout.leftMargin:  10 * preferencesModel.uiScale
			}
		}
		SetSeed{}
	}
}

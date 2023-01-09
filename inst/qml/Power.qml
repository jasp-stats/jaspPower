//
// Copyright (C) 2013-2021 University of Amsterdam
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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0

Form
{
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
      { label: "Two Samples Variance Ratio Test",  value: "twoSamplesVarianceRatio"}
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

    Group
		{
			Layout.columnSpan: 2

			Group
			{
				columns: 2

        Text { text: qsTr("I want to calculate the ...") }
        DropDown
        {
          name: "calculation"
          id:   calc
          indexDefaultValue: 0
          label: qsTr("")
          values: [
            { label: "Sample Size N", value: "sampleSize"},
            { label: "Power",  value: "power" },
            { label: "Effect size",  value: "effectSize"}
          ]
        }

        Text {
          text: qsTr("Direction of the effect:")
          visible: (test.currentIndex == 4 || test.currentIndex == 5 || test.currentIndex == 6 || test.currentIndex == 7) && calc.currentIndex == 2 && alt.value == "twoSided"
          enabled: calc.currentIndex == 2
        }
				DropDown 
        {
          id: direction
          name: "effectDirection"
          label: qsTr("")
          visible: (test.currentIndex == 4 || test.currentIndex == 5 || test.currentIndex == 6 || test.currentIndex == 7) && calc.currentIndex == 2 && alt.value == "twoSided"
          enabled: calc.currentIndex == 2
          values: [
            { label: (test.currentIndex == 4 || test.currentIndex == 5) ? ((test.currentIndex == 4) ? qsTr("p\u2081 > p\u2080") : qsTr("p\u2081 > p\u2082")) : qsTr("\u03C1 > 1"), value: "greater"},
            { label: (test.currentIndex == 4 || test.currentIndex == 5) ? ((test.currentIndex == 4) ? qsTr("p\u2081 < p\u2080") : qsTr("p\u2081 < p\u2082")) : qsTr("\u03C1 < 1"),  value: "less" }
          ]
        }

        Text {
          text: (test.currentIndex == 4) ? qsTr("Hypothesized proportion") : qsTr("Baseline proportion")
          visible: test.currentIndex == 4 || test.currentIndex == 5
        }
				DoubleField {
          id: p0
          name: "baselineProportion"
          label: (test.currentIndex == 4) ? qsTr("p₀") : qsTr("p₂")
          min: 0.01
          max: 0.99
          defaultValue: 0.5
          visible: test.currentIndex == 4 || test.currentIndex == 5
        }

        Text {
          text: qsTr("Comparison proportion")
          visible: test.currentIndex == 4 || test.currentIndex == 5
          enabled: calc.currentIndex != 2
        }
				DoubleField {
          id: p1
          name: "comparisonProportion"
          label: qsTr("p₁")
          min: 0.01
          max: 0.99
          defaultValue: 0.6
          visible: test.currentIndex == 4 || test.currentIndex == 5
          enabled: calc.currentIndex != 2
        }

        Text {
          text: qsTr("Minimal effect size of interest:")
          visible: test.currentIndex == 0 || test.currentIndex == 1 || test.currentIndex == 2 || test.currentIndex == 3
          enabled: calc.currentIndex != 2
        }
				DoubleField {
          id: es
          name: "effectSize"
          label: qsTr("|δ|")
          defaultValue: 0.5
          visible: test.currentIndex == 0 || test.currentIndex == 1 || test.currentIndex == 2 || test.currentIndex == 3
          enabled: calc.currentIndex != 2
        }

				Text {
          text: qsTr("Minimal effect size of interest:")
          visible: test.currentIndex == 6 || test.currentIndex == 7
          enabled: calc.currentIndex != 2
        }
				DoubleField {
          id: rho
          name: "varianceRatio"
          label: (test.currentIndex == 7) ? qsTr("\u03C1 (\u03C3\u2081\u00B2/\u03C3\u2082\u00B2)") : qsTr("\u03C1 (\u03C3\u00B2/\u03C3\u2080\u00B2)")
          defaultValue: 2
          visible: test.currentIndex == 6 || test.currentIndex == 7
          enabled: calc.currentIndex != 2
        }

				Text {
          text: qsTr("Minimal desired power:")
          enabled: calc.currentIndex != 1
        }
				DoubleField {
          id: power
          name: "power"
          label: qsTr("(1-β)")
          min: 0.1
          max: 0.999
          defaultValue: 0.9
          enabled: calc.currentIndex != 1
        }

        Text { text: qsTr("Type I error rate:") }
				DoubleField {
          id: alpha
          name: "alpha"
          label: qsTr("α")
          min: 0
          defaultValue: 0.05
        }

        // No groups in single sample t-test
				Text {
          text: qsTr("Sample size:")
          visible: test.currentIndex == 2 || test.currentIndex == 3 || test.currentIndex == 4 || test.currentIndex == 6 || test.currentIndex == 8
          enabled: calc.currentIndex != 0
        }
        Text {
          text: qsTr("Sample size per group:")
          visible: test.currentIndex == 0 || test.currentIndex == 1 || test.currentIndex == 5 || test.currentIndex == 7 || test.currentIndex == 9
          enabled: calc.currentIndex != 0
        }
				IntegerField {
          id: n
          name: "sampleSize"
          label: qsTr("N")
          min: 2
          defaultValue: 20
          enabled: calc.currentIndex != 0
        }

        // No sample size ratio in single sample t-test
        Text {
          text: qsTr("Sample size ratio:")
          visible: test.currentIndex == 0 || test.currentIndex == 5 || test.currentIndex == 7 || test.currentIndex == 9
        }
				DoubleField {
          id: n_ratio
          name: "sampleSizeRatio"
          label: qsTr("N₁/N₂")
          min: 0
          defaultValue: 1
          visible: test.currentIndex == 0 || test.currentIndex == 5 || test.currentIndex == 7 || test.currentIndex == 9
        }

        Text { text: qsTr("Alternative Hypothesis:") }
        DropDown
        {
          name: "alternative"
          id:   alt
          indexDefaultValue: 0
          label: qsTr("H\u2081")
          values: (test.currentIndex == 0 || test.currentIndex == 1 || test.currentIndex == 2 || test.currentIndex == 3) ?
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
    }

    CheckBox {
      label: qsTr("Power demonstration")
      id: powerDist
      name: "powerDemonstration"
      checked: false
    }

    CheckBox {
      label: qsTr("Power curve by effect size")
      id: powerCurveES
      name: "powerByEffectSize"
      checked: true
    }

    CheckBox {
      label: qsTr("Power curve by N")
      id: powerCurveN
      name: "powerBySampleSize"
      checked: false
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

	  Group
	  {
		  id:	parameters
      visible: test.currentIndex != 4 && test.currentIndex != 5
      columns: 2

      Group
      {
        title: qsTr("Parameters")
        DoubleField
        {
          name: "firstGroupMean"
          label: test.currentIndex == 6 ? qsTr("\u0078\u0305") : qsTr("\u0078\u0305\u2081")
          defaultValue: 0
          visible: test.currentIndex == 6 || test.currentIndex == 7
        }

        DoubleField
        {
          name: "secondGroupMean"
          label: qsTr("\u0078\u0305\u2082")
          defaultValue: 0
          visible: test.currentIndex == 0 || test.currentIndex == 1 || test.currentIndex == 7
        }

        DoubleField
        {
          name: "testValue"
          label: qsTr("\u03BC\u2080")
          defaultValue: 0
          visible: test.currentIndex == 2 || test.currentIndex == 3
        }

        DoubleField
        {
          name: "firstGroupSd"
          label: (test.currentIndex == 2) ? qsTr("s") : qsTr("s\u2081")
          defaultValue: 1
          visible: test.currentIndex == 0 || test.currentIndex == 1 || test.currentIndex == 2
        }

        DoubleField
        {
          name: "populationSd"
          label: test.currentIndex == 3 ? qsTr("\u03C3") : qsTr("\u03C3\u2080")
          defaultValue: 1
          visible: test.currentIndex == 3 || test.currentIndex == 6
        }

        DoubleField
        {
          name: "secondGroupSd"
          label: qsTr("s\u2082")
          defaultValue: 1
          visible: test.currentIndex == 0 || test.currentIndex == 1 || test.currentIndex == 7
        }
      }

      RadioButtonGroup
      {
        name: "effectDirectionSyntheticDataset"
        title: qsTr("Effect direction")
        visible: test.currentIndex != 6 && test.currentIndex != 7
        RadioButton { value: "less"; label: (test.currentIndex == 2 || test.currentIndex == 3) ? qsTr("\u0078\u0305 < \u03BC\u2080") : qsTr("\u0078\u0305\u2081 < \u0078\u0305\u2082"); checked: true }
        RadioButton { value: "greater"; label: (test.currentIndex == 2 || test.currentIndex == 3) ? qsTr("\u0078\u0305 > \u03BC\u2080") : qsTr("\u0078\u0305\u2081 > \u0078\u0305\u2082") }
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

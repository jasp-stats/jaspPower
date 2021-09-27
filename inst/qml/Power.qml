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
      { label: "Independent Samples T-Test", value: "ttest_independent"},
      { label: "Paired Samples T-Test",  value: "ttest_paired" },
      { label: "One Sample T-Test",  value: "ttest_onesample"   },
      { label: "ANOVA",  value: "anova" }
    ]
  }

	Section
	{
		expanded: true
    visible: test.currentValue !== 'anova'
		title: qsTr("Parameters (t-tests)")

    Group
		{
			Layout.columnSpan: 2

			Group
			{
				columns: 2

        Text { text: qsTr("I want to calculate the ...") }
        DropDown
        {
          name: "calc"
          id:   calc
          indexDefaultValue: 0
          label: qsTr("")
          values: [
            { label: "N per group", value: "n"},
            { label: "Power",  value: "power" },
            { label: "Effect size",  value: "es"}
          ]
        }

				Text {
          text: qsTr("Minimally-interesting effect size:")
          enabled: calc.currentIndex != 2
        }
				DoubleField {
          id: es
          name: "es"
          label: qsTr("δ")
          min: 0.01
          defaultValue: 0.5
          enabled: calc.currentIndex != 2
        }

				Text {
          text: qsTr("Minimum desired power:")
          enabled: calc.currentIndex != 1
        }
				DoubleField {
          id: power
          name: "power"
          label: qsTr("(1-β)")
          min: 0
          max: 1
          defaultValue: 0.9
          enabled: calc.currentIndex != 1
        }

        // No groups in single sample t-test
				Text {
          text: qsTr("Sample size:")
          visible: test.currentValue == 'ttest_onesample'
          enabled: calc.currentIndex != 0
        }
        Text {
          text: qsTr("Sample size per group:")
          visible: test.currentValue == 'ttest_independent' || test.currentValue == 'ttest_paired'
          enabled: calc.currentIndex != 0
        }
				IntegerField {
          id: n
          name: "n"
          label: qsTr("N")
          min: 2
          defaultValue: 20
          enabled: calc.currentIndex != 0
        }

        Text { text: qsTr("Type I error rate:") }
				DoubleField {
          id: alpha
          name: "alpha"
          label: qsTr("α")
          min: 0
          defaultValue: 0.05
        }

        // No sample size ratio in single sample t-test
        Text {
          text: qsTr("Sample size ratio:")
          visible: test.currentValue == 'ttest_independent' || test.currentValue == 'ttest_paired'
        }
				DoubleField {
          id: n_ratio
          name: "n_ratio"
          label: qsTr("N₁/N₂")
          min: 0
          defaultValue: 1
          visible: test.currentValue == 'ttest_independent' || test.currentValue == 'ttest_paired'
        }

        Text { text: qsTr("Alternative Hypothesis:") }
        DropDown
        {
          name: "alt"
          id:   alt
          indexDefaultValue: 0
          label: qsTr("")
          values: [
            { label: "Two-sided", value: "twosided"},
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
		title: qsTr("Display")

    CheckBox {
      label: qsTr("Power contour plot")
      id: powerContour
      name: "powerContour"
      checked: true
    }

    CheckBox {
      label: qsTr("Power demonstration")
      id: powerDist
      name: "powerDist"
      checked: false
    }

    CheckBox {
      label: qsTr("Power curve by effect size")
      id: powerCurveES
      name: "powerCurveES"
      checked: true
    }

    CheckBox {
      label: qsTr("Power curve by N")
      id: powerCurveN
      name: "powerCurveN"
      checked: false
    }

    CheckBox {
      label: qsTr("Explanatory text")
      id: text
      name: "text"
      checked: true
    }
  }
}

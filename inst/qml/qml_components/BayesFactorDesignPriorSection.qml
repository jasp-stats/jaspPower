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
import JASP
import JASP.Controls

Section
{
	expanded: true
	title: qsTr("Design Prior")
	columns: 2

	property string testValue: ""
	property bool supportsBinomial: false
	property bool isBinomial: supportsBinomial && testValue === "oneSampleProportion"

	BayesFactorContinuousDesignPriorGroup
	{
		groupTitle: qsTr("Prior Under H\u2080")
		visible: !isBinomial
		distributionName: "designNullPriorDistribution"
		meanName: "designNullPriorMean"
		sdName: "designNullPriorStandardDeviation"
		distributionInfo: qsTr("Design prior under H\u2080 used to evaluate study outcomes when the null hypothesis is true.")
		meanInfo: qsTr("Location or mean of the H\u2080 design prior.")
		sdInfo: qsTr("Standard deviation of the normal H\u2080 design prior.")
		meanDefault: 0
	}

	BayesFactorBinomialDesignPriorGroup
	{
		groupTitle: qsTr("Prior Under H\u2080")
		visible: isBinomial
		distributionName: "binomialDesignNullPriorDistribution"
		pointName: "designNullProportion"
		successesName: "designNullPriorSuccesses"
		failuresName: "designNullPriorFailures"
		lowerName: "designNullPriorLowerTruncation"
		upperName: "designNullPriorUpperTruncation"
		distributionInfo: qsTr("Design prior under H\u2080 used to evaluate binomial study outcomes when the null hypothesis is true.")
		pointInfo: qsTr("Point proportion assumed under the H\u2080 design prior.")
		successesInfo: qsTr("Success parameter of the beta H\u2080 design prior.")
		failuresInfo: qsTr("Failure parameter of the beta H\u2080 design prior.")
		lowerInfo: qsTr("Lower truncation point for the beta H\u2080 design prior.")
		upperInfo: qsTr("Upper truncation point for the beta H\u2080 design prior.")
		pointDefault: 0.5
		upperDefault: 0.5
	}

	BayesFactorContinuousDesignPriorGroup
	{
		groupTitle: qsTr("Prior Under H\u2081")
		visible: !isBinomial
		distributionName: "designPriorDistribution"
		meanName: "designPriorMean"
		sdName: "designPriorStandardDeviation"
		distributionInfo: qsTr("Design prior under H\u2081 used to evaluate study outcomes when the alternative hypothesis is true.")
		meanInfo: qsTr("Location or mean of the H\u2081 design prior.")
		sdInfo: qsTr("Standard deviation of the normal H\u2081 design prior.")
		meanDefault: 0.5
	}

	BayesFactorBinomialDesignPriorGroup
	{
		groupTitle: qsTr("Prior Under H\u2081")
		visible: isBinomial
		distributionName: "binomialDesignPriorDistribution"
		pointName: "designProportion"
		successesName: "designPriorSuccesses"
		failuresName: "designPriorFailures"
		lowerName: "designPriorLowerTruncation"
		upperName: "designPriorUpperTruncation"
		distributionInfo: qsTr("Design prior under H\u2081 used to evaluate binomial study outcomes when the alternative hypothesis is true.")
		pointInfo: qsTr("Point proportion assumed under the H\u2081 design prior.")
		successesInfo: qsTr("Success parameter of the beta H\u2081 design prior.")
		failuresInfo: qsTr("Failure parameter of the beta H\u2081 design prior.")
		lowerInfo: qsTr("Lower truncation point for the beta H\u2081 design prior.")
		upperInfo: qsTr("Upper truncation point for the beta H\u2081 design prior.")
		pointDefault: 0.6
		upperDefault: 1
	}
}

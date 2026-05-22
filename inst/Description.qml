import QtQuick
import JASP.Module

Description
{
	title		: 	qsTr("Power")
	icon		: 	"power.svg"
	description	: 	qsTr("This module allows you to conduct power analyses.")
	requiresData:	false
	hasWrappers: 	false
	preloadData:  	true
	
	GroupTitle
	{
		title:		qsTr("Classical")
		icon:		"power.svg"
	}

	Analysis
	{
		title:	qsTr("Power")
		func: 	"Power"
	}

	Analysis
	{
		title:	qsTr("Group Sequential Design")
		func: 	"GroupSequentialDesign"
	}

	GroupTitle
	{
		title:		qsTr("Bayesian")
		icon:		"powerBayes.svg"
	}

	Analysis
	{
		title:	qsTr("Bayes Factor Design")
		func: 	"BayesFactorDesign"
	}

	Analysis
	{
		title:	qsTr("Bayes Factor Sequential Design")
		func: 	"BayesFactorSequentialDesign"
	}
}

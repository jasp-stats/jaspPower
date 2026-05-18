import QtQuick
import JASP.Module

Description
{
	title		: 	qsTr("Power")
	icon		: 	"power.svg"
	description	: 	qsTr("This module allows you to conduct power analyses.")
	requiresData:	false
	hasWrappers: 	false
	preloadData:  	false
	
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
		func: 	"PowerSequential"
	}

	GroupTitle
	{
		title:		qsTr("Bayesian")
		icon:		"powerBayes.svg"
	}

	Analysis
	{
		title:	qsTr("Evidence")
		func: 	"Evidence"
	}

	Analysis
	{
		title:	qsTr("Evidence (Sequential)")
		func: 	"EvidenceSequential"
	}
}

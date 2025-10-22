import QtQuick
import JASP.Module

Description
{
	title		: 	qsTr("Power")
	icon		: 	"power.svg"
	description	: 	qsTr("This module allows you to conduct power analyses.")
	requiresData:	false
	hasWrappers: 	false
	
	Analysis
	{
		title:	qsTr("Power")
		func: 	"Power"
	}
}

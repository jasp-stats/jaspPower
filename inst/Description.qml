import QtQuick
import JASP.Module

Description
{
	name		: "jaspPower"
	title		: qsTr("Power")
	icon:			"power.svg"
	description	: qsTr("This module allows you to conduct power analyses.")
	version			: "0.20.0"
	author		: "JASP Team"
	maintainer	: "JASP Team <info@jasp-stats.org>"
	website		: "jasp-stats.org"
	license		: "GPL (>= 2)"
	requiresData:	false

	Analysis
	{
		title:	qsTr("Power")
		func: 	"Power"
	}
}

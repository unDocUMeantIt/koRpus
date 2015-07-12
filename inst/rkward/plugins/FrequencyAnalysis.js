// this code was generated using the rkwarddev package.
//perhaps don't make changes here, but in the rkwarddev script instead!



function preprocess(){
	// add requirements etc. here

}

function calculate(){
	// read in variables from dialog

	var varHyphenTagged = getValue("varHyphenTagged");
	var varkRpFreqObj = getValue("varkRpFreqObj");
	var chcTrmfrqnc = getValue("chc_Trmfrqnc");
	var chcShwfrqnc = getValue("chc_Shwfrqnc");
	var saveFrq = getValue("saveFrq");
	var drpUscrpsdt = getValue("drp_Uscrpsdt");
	var brwDatabsfl = getValue("brw_Databsfl");
	var spnNmbrfrnn = getValue("spn_Nmbrfrnn");
	var saveCorpFrq = getValue("saveCorpFrq");

	// the R code to be evaluated
	if(!varkRpFreqObj) {
		if(drpUscrpsdt == "LCC" && brwDatabsfl) {
			echo("corp.freq.obj <- read.corp.LCC(\n\t\"" + brwDatabsfl + "\"\n)\n\n");
		} else if(drpUscrpsdt == "celex" && brwDatabsfl && spnNmbrfrnn) {
			echo("corp.freq.obj <- read.corp.celex(\n\t\"" + brwDatabsfl + "\",\n\trunning.words=" + spnNmbrfrnn + "\n)\n\n");
		}
	}
	echo("freq.analysis.obj <- freq.analysis(\n\t" + varHyphenTagged);
	if(!varkRpFreqObj) {
		if(drpUscrpsdt != "none") {
			echo(",\n\tcorp.freq=corp.freq.obj");
		}
	} else {
		echo(",\n\tcorp.freq=" + varkRpFreqObj);
	}
	if(!chcTrmfrqnc) {
		echo(",\n\ttfidf=FALSE");
	}
	echo("\n)\n\n");
}

function printout(){
	// printout the results


	var chcShwfrqnc = getValue("chc_Shwfrqnc");
	new Header(i18n("Frequency analysis results")).print();

	echo("rk.print(summary(freq.analysis.obj))\n\n");
	if(chcShwfrqnc) {
		new Header(i18n("Frequencies of types & token (by word class)"), 3).print();

	echo("freqTypeToken <- data.frame(\n\t" + "types=slot(freq.analysis.obj, \"desc\")$freq.types,\n\t" + "token=slot(freq.analysis.obj, \"desc\")$freq.token\n)\n" + "rk.print(freqTypeToken)\n\n");
	}
	//// save result object
	// read in saveobject variables
	var saveFrq = getValue("saveFrq");
	var saveFrqActive = getValue("saveFrq.active");
	var saveFrqParent = getValue("saveFrq.parent");	var saveCorpFrq = getValue("saveCorpFrq");
	var saveCorpFrqActive = getValue("saveCorpFrq.active");
	var saveCorpFrqParent = getValue("saveCorpFrq.parent");
	// assign object to chosen environment
	if(saveFrqActive) {
		echo(".GlobalEnv$" + saveFrq + " <- freq.analysis.obj\n");
	}
	if(saveCorpFrqActive) {
		echo(".GlobalEnv$" + saveCorpFrq + " <- corp.freq.obj\n");
	}

}


// this code was generated using the rkwarddev package.
//perhaps don't make changes here, but in the rkwarddev script instead!



function preprocess(){
	// add requirements etc. here
	echo("require(koRpus)\n");
}

function calculate(){
	// read in variables from dialog

	var radSlctprtn = getString("rad_Slctprtn");
	var brwTrTggrrt = getString("brw_TrTggrrt");
	var drpTextlngg = getString("drp_Textlngg");
	var brwTxttnlyz = getString("brw_Txttnlyz");
	var saveTaggedText = getString("saveTaggedText");
	var chcDtcthdln = getBoolean("chc_Dtcthdln.state");
	var chcDtctprgr = getBoolean("chc_Dtctprgr.state");
	var showTagged = getBoolean("showTagged.state");

	// the R code to be evaluated
	var valueChcDtcthdln = getValue("chc_Dtcthdln");
	var valueChcDtctprgr = getValue("chc_Dtctprgr");
	// define the array arrDetect for values of R option "detect"
	var arrDetect = new Array();
	arrDetect.push(valueChcDtcthdln, valueChcDtctprgr);
	// clean array arrDetect from empty strings
	arrDetect = arrDetect.filter(String);
	// set the actual variable optDetect for R option "detect=c()"
	if(arrDetect.length > 0) {
		var optDetect = ",\n\tdetect=c(" + arrDetect.join(", ") + ")";
	} else {
		var optDetect = "";
	}

	if(drpTextlngg == "de-utf8") {
		var TTLang = "de";
	} else if(drpTextlngg == "fr-utf8") {
		var TTLang = "fr";
	} else if(drpTextlngg == "es-utf8") {
		var TTLang = "es";
	} else if(drpTextlngg == "it-utf8") {
		var TTLang = "it";
	} else {
		var TTLang = drpTextlngg;
	}
	if(radSlctprtn == "file") {
		echo("tagged.text.obj <- tokenize(\n\t\"" + brwTxttnlyz + "\",\n\tlang=\"" + TTLang + "\"" + optDetect + "\n)\n\n");
	} else {
		echo("tagged.text.obj <- treetag(\n\t\"" + brwTxttnlyz + "\",\n\ttreetagger=\"manual\",\n\tlang=\"" + TTLang + "\",\n\tTT.options=list(path=\"" + brwTrTggrrt + "\",\n\tpreset=\"" + drpTextlngg + "\")\n)\n\n");
	}
}

function printout(){
	// printout the results


	var brwTxttnlyz = getValue("brw_Txttnlyz");
	var drpTextlngg = getValue("drp_Textlngg");
	var showTagged = getValue("showTagged");
	new Header(i18n("Tokenizing & POS tagging results")).add(i18n("Text"), getValue("brw_Txttnlyz")).add(i18n("Language"), getValue("drp_Textlngg")).print();

	new Header(i18n("Word class distribution"), 3).print();

	echo("rk.print(summary(tagged.text.obj))\n");
	if(showTagged) {
		new Header(i18n("Tagged text"), 3).print();

	echo("rk.print(taggedText(tagged.text.obj))\n");
	}
	echo("\n");
	//// save result object
	// read in saveobject variables
	var saveTaggedText = getValue("saveTaggedText");
	var saveTaggedTextActive = getValue("saveTaggedText.active");
	var saveTaggedTextParent = getValue("saveTaggedText.parent");
	// assign object to chosen environment
	if(saveTaggedTextActive) {
		echo(".GlobalEnv$" + saveTaggedText + " <- tagged.text.obj\n");
	}

}


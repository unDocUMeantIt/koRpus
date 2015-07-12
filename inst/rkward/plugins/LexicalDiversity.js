// this code was generated using the rkwarddev package.
//perhaps don't make changes here, but in the rkwarddev script instead!



function preprocess(){
	// add requirements etc. here

}

function calculate(){
	// read in variables from dialog

	var varHyphenTagged = getValue("varHyphenTagged");
	var TTR = getValue("TTR");
	var MSTTR = getValue("MSTTR");
	var MATTR = getValue("MATTR");
	var Cld = getValue("Cld");
	var Rld = getValue("Rld");
	var CTTR = getValue("CTTR");
	var Uld = getValue("Uld");
	var Sld = getValue("Sld");
	var Kld = getValue("Kld");
	var Maas = getValue("Maas");
	var HDD = getValue("HDD");
	var MTLD = getValue("MTLD");
	var MTLDMA = getValue("MTLDMA");
	var types = getValue("types");
	var saveLD = getValue("saveLD");
	var TTRChar = getValue("TTRChar");
	var MATTRChar = getValue("MATTRChar");
	var CldChar = getValue("CldChar");
	var RldChar = getValue("RldChar");
	var CTTRChar = getValue("CTTRChar");
	var UldChar = getValue("UldChar");
	var SldChar = getValue("SldChar");
	var KldChar = getValue("KldChar");
	var MaasChar = getValue("MaasChar");
	var HDDChar = getValue("HDDChar");
	var MTLDChar = getValue("MTLDChar");
	var MTLDMAChar = getValue("MTLDMAChar");
	var spnStpszbtw = getValue("spn_Stpszbtw");
	var chcCassnstv = getValue("chc_Cassnstv");
	var chcLemmatiz = getValue("chc_Lemmatiz");
	var chcKptypstk = getValue("chc_Kptypstk");
	var inpBsfrlgrt = getValue("inp_Bsfrlgrt");
	var spnSgmntszt = getValue("spn_Sgmntszt");
	var spnWndwsztk = getValue("spn_Wndwsztk");
	var spnRndmsmpl = getValue("spn_Rndmsmpl");
	var spnFactorsz = getValue("spn_Factorsz");
	var spnMnmmnmbr = getValue("spn_Mnmmnmbr");
	var chcKplldtls = getValue("chc_Kplldtls");

	// the R code to be evaluated
	var valueTTR = getValue("TTR");
	var valueMSTTR = getValue("MSTTR");
	var valueMATTR = getValue("MATTR");
	var valueCld = getValue("Cld");
	var valueRld = getValue("Rld");
	var valueCTTR = getValue("CTTR");
	var valueUld = getValue("Uld");
	var valueSld = getValue("Sld");
	var valueKld = getValue("Kld");
	var valueMaas = getValue("Maas");
	var valueHDD = getValue("HDD");
	var valueMTLD = getValue("MTLD");
	var valueMTLDMA = getValue("MTLDMA");
	// define the array arrMeasure for values of R option "measure"
	var arrMeasure = new Array();
	arrMeasure.push(valueTTR, valueMSTTR, valueMATTR, valueCld, valueRld, valueCTTR, valueUld, valueSld, valueKld, valueMaas, valueHDD, valueMTLD, valueMTLDMA);
	// clean array arrMeasure from empty strings
	arrMeasure = arrMeasure.filter(String);
	// set the actual variable optMeasure for R option "measure=c()"
	if(arrMeasure.length > 0) {
		var optMeasure = ",\n\tmeasure=c(\"" + arrMeasure.join("\", \"") + "\")";
	} else {
		var optMeasure = "";
	}

	var valueTTRChar = getValue("TTRChar");
	var valueMATTRChar = getValue("MATTRChar");
	var valueCldChar = getValue("CldChar");
	var valueRldChar = getValue("RldChar");
	var valueCTTRChar = getValue("CTTRChar");
	var valueUldChar = getValue("UldChar");
	var valueSldChar = getValue("SldChar");
	var valueKldChar = getValue("KldChar");
	var valueMaasChar = getValue("MaasChar");
	var valueHDDChar = getValue("HDDChar");
	var valueMTLDChar = getValue("MTLDChar");
	var valueMTLDMAChar = getValue("MTLDMAChar");
	// define the array arrChar for values of R option "char"
	var arrChar = new Array();
	arrChar.push(valueTTRChar, valueMATTRChar, valueCldChar, valueRldChar, valueCTTRChar, valueUldChar, valueSldChar, valueKldChar, valueMaasChar, valueHDDChar, valueMTLDChar, valueMTLDMAChar);
	// clean array arrChar from empty strings
	arrChar = arrChar.filter(String);
	// set the actual variable optChar for R option "char=c()"
	if(arrChar.length > 0) {
		var optChar = ",\n\tchar=c(\"" + arrChar.join("\", \"") + "\")";
	} else {
		var optChar = "";
	}

	var frmMATTREnabled = getValue("frm_MATTR.enabled");
	var frmMSTTREnabled = getValue("frm_MSTTR.enabled");
	var frmHDDEnabled = getValue("frm_HDD.enabled");
	var frmMTLDMTLDEnabled = getValue("frm_MTLDMTLD.enabled");
	echo("lexical.diversity.obj <- lex.div(\n\t" + varHyphenTagged);
	if(spnSgmntszt != "100" && frmMSTTREnabled) {
		echo(",\n\tsegment=" + spnSgmntszt);
	}
	if(frmMTLDMTLDEnabled) {
		if(spnFactorsz != "0.72") {
		echo(",\n\tfactor.size=" + spnFactorsz);
	}
	if(spnMnmmnmbr != "9") {
		echo(",\n\tmin.tokens=" + spnMnmmnmbr);
	}
	}
	if(spnRndmsmpl != "42" && frmHDDEnabled) {
		echo(",\n\trand.sample=" + spnRndmsmpl);
	}
	if(spnWndwsztk != "100" && frmMATTREnabled) {
		echo(",\n\twindow=" + spnWndwsztk);
	}
	if(chcCassnstv) {
		echo(",\n\tcase.sens=TRUE");
	}
	if(chcLemmatiz) {
		echo(",\n\tlemmatize=TRUE");
	}
	if(chcKplldtls && frmMTLDMTLDEnabled) {
		echo(",\n\tdetailed=TRUE");
	}
	echo(optMeasure);
	if(optChar != "") {
		echo(optChar);
	if(spnStpszbtw != "5") {
		echo(",\n\tchar.steps=" + spnStpszbtw);
	}
	} else {
		echo(",\n\tchar=NULL");
	}
	if(inpBsfrlgrt != "10") {
		echo(",\n\tlog.base=" + inpBsfrlgrt);
	}
	if(chcKptypstk) {
		echo(",\n\tkeep.tokens=TRUE");
	}
	echo(",\n\tquiet=TRUE\n)\n\n");
}

function printout(){
	// printout the results


	var spnSgmntszt = getValue("spn_Sgmntszt");
	var spnFactorsz = getValue("spn_Factorsz");
	var spnMnmmnmbr = getValue("spn_Mnmmnmbr");
	var spnRndmsmpl = getValue("spn_Rndmsmpl");
	var spnWndwsztk = getValue("spn_Wndwsztk");
	var chcCassnstv = getValue("chc_Cassnstv");
	var chcLemmatiz = getValue("chc_Lemmatiz");
	var chcKplldtls = getValue("chc_Kplldtls");
	var types = getValue("types");
	new Header(i18n("Lexical diversity results")).add(i18n("MSTTR segment size"), getValue("spn_Sgmntszt")).add(i18n("MTLD factor size"), getValue("spn_Factorsz")).add(i18n("MTLD-MA min. tokens/factor"), getValue("spn_Mnmmnmbr")).add(i18n("HD-D random sample size"), getValue("spn_Rndmsmpl")).add(i18n("MATTR window size"), getValue("spn_Wndwsztk")).add(i18n("Case sensitive"), getValue("chc_Cassnstv")).add(i18n("Lemmatize"), getValue("chc_Lemmatiz")).add(i18n("Keep MTLD/MTLD-MA details"), getValue("chc_Kplldtls")).print();

	echo("rk.results(summary(lexical.diversity.obj))\n");
	if(types) {
		new Header(i18n("Identified types in text"), 3).print();

	echo("rk.print(slot(lexical.diversity.obj, \"tt\")[[\"types\"]])\n");
	}
	echo("\n");
	//// save result object
	// read in saveobject variables
	var saveLD = getValue("saveLD");
	var saveLDActive = getValue("saveLD.active");
	var saveLDParent = getValue("saveLD.parent");
	// assign object to chosen environment
	if(saveLDActive) {
		echo(".GlobalEnv$" + saveLD + " <- lexical.diversity.obj\n");
	}

}


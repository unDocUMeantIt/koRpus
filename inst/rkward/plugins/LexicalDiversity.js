// this code was generated using the rkwarddev package.
// perhaps don't make changes here, but in the rkwarddev script instead!
// 
// look for a file called: $SRC/inst/rkward/rkwarddev_koRpus_plugin_script.R



function preprocess(is_preview){
  // add requirements etc. here

}

function calculate(is_preview){
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
  var showTypes = getValue("showTypes");
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
  var stepSize = getValue("stepSize");
  var LDcaseSens = getValue("LDcaseSens");
  var LDlemmatize = getValue("LDlemmatize");
  var LDkeepTokens = getValue("LDkeepTokens");
  var LDlog = getValue("LDlog");
  var LDsegment = getValue("LDsegment");
  var LDwindow = getValue("LDwindow");
  var LDsampleSize = getValue("LDsampleSize");
  var LDfactorSize = getValue("LDfactorSize");
  var LDminTokens = getValue("LDminTokens");
  var LDdetails = getValue("LDdetails");

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

  var optMATTREnabled = getValue("optMATTR.enabled");
  var optMSTTREnabled = getValue("optMSTTR.enabled");
  var optHDDEnabled = getValue("optHDD.enabled");
  var optMTLDEnabled = getValue("optMTLD.enabled");
  echo("lexical.diversity.obj <- lex.div(\n\t" + varHyphenTagged);
  if(LDsegment != 100 && optMSTTREnabled) {
    echo(",\n\tsegment=" + LDsegment);  
  } else {}
  if(optMTLDEnabled) {
    if(LDfactorSize != 0.72) {
      echo(",\n\tfactor.size=" + LDfactorSize);  
    } else {}  
    if(LDminTokens != 9) {
      echo(",\n\tmin.tokens=" + LDminTokens);  
    } else {}  
  } else {}
  if(LDsampleSize != 42 && optHDDEnabled) {
    echo(",\n\trand.sample=" + LDsampleSize);  
  } else {}
  if(LDwindow != 100 && optMATTREnabled) {
    echo(",\n\twindow=" + LDwindow);  
  } else {}
  if(LDcaseSens) {
    echo(",\n  case.sens=TRUE");
  } else {}
  if(LDlemmatize) {
    echo(",\n  lemmatize=TRUE");
  } else {}
  if(LDdetails && optMTLDEnabled) {
    echo(",\n\tdetailed=TRUE");  
  } else {}
  echo(optMeasure);
  if(optChar != "") {
    echo(optChar);  
    if(stepSize != 5) {
      echo(",\n\tchar.steps=" + stepSize);  
    } else {}  
  } else {
    echo(",\n\tchar=NULL");  
  }
  if(LDlog != 10) {
    echo(",\n\tlog.base=" + LDlog);  
  } else {}
  if(LDkeepTokens) {
    echo(",\n  keep.tokens=TRUE");
  } else {}
  echo(",\n\tquiet=TRUE\n)\n\n");
}

function printout(is_preview){
  // printout the results
  var LDsegment = getValue("LDsegment");
  var LDfactorSize = getValue("LDfactorSize");
  var LDminTokens = getValue("LDminTokens");
  var LDsampleSize = getValue("LDsampleSize");
  var LDwindow = getValue("LDwindow");
  var LDcaseSens = getValue("LDcaseSens");
  var LDlemmatize = getValue("LDlemmatize");
  var LDdetails = getValue("LDdetails");
  var showTypes = getValue("showTypes");
  new Header(i18n("Lexical diversity results")).add(i18n("MSTTR segment size"), getValue("LDsegment")).add(i18n("MTLD factor size"), getValue("LDfactorSize")).add(i18n("MTLD-MA min. tokens/factor"), getValue("LDminTokens")).add(i18n("HD-D random sample size"), getValue("LDsampleSize")).add(i18n("MATTR window size"), getValue("LDwindow")).add(i18n("Case sensitive"), getValue("LDcaseSens")).add(i18n("Lemmatize"), getValue("LDlemmatize")).add(i18n("Keep MTLD/MTLD-MA details"), getValue("LDdetails")).print();
  echo("rk.results(summary(lexical.diversity.obj))\n");
  if(showTypes) {
    new Header(i18n("Identified types in text"), 3).print();  
    echo("rk.print(slot(lexical.diversity.obj, \"tt\")[[\"types\"]])\n");  
  } else {}
  echo("\n");
  //// save result object
  // read in saveobject variables
  var saveLD = getValue("saveLD");
  var saveLDActive = getValue("saveLD.active");
  var saveLDParent = getValue("saveLD.parent");
  // assign object to chosen environment
  if(saveLDActive) {
    echo(".GlobalEnv$" + saveLD + " <- lexical.diversity.obj\n");
  } else {}

}


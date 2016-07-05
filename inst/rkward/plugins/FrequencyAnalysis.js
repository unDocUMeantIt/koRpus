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
  var varkRpFreqObj = getValue("varkRpFreqObj");
  var tfidf = getValue("tfidf");
  var freqShowTypes = getValue("freqShowTypes");
  var corpusDB = getValue("corpusDB");
  var corpDBdir = getValue("corpDBdir");
  var CelexRunWd = getValue("CelexRunWd");

  // the R code to be evaluated
  if(!varkRpFreqObj) {
    if(corpusDB == "LCC" && corpDBdir) {
      echo("corp.freq.obj <- read.corp.LCC(\n\t\"" + corpDBdir + "\"\n)\n\n");  
    } else if(corpusDB == "celex" && corpDBdir) {
      echo("corp.freq.obj <- read.corp.celex(\n\t\"" + corpDBdir + "\",\n\trunning.words=" + CelexRunWd + "\n)\n\n");  
    } else {}  
  } else {}
  echo("freq.analysis.obj <- freq.analysis(\n\t" + varHyphenTagged);
  if(!varkRpFreqObj) {
    if(corpusDB != "none") {
      echo(",\n\tcorp.freq=corp.freq.obj");  
    } else {}  
  } else {
    echo(",\n\tcorp.freq=" + varkRpFreqObj);  
  }
  if(!tfidf) {
    echo(",\n\ttfidf=FALSE");  
  } else {}
  echo("\n)\n\n");
}

function printout(is_preview){
  // printout the results
  var freqShowTypes = getValue("freqShowTypes");
  new Header(i18n("Frequency analysis results")).print();
  echo("rk.print(summary(freq.analysis.obj))\n\n");
  if(freqShowTypes) {
    new Header(i18n("Frequencies of types & token (by word class)"), 3).print();  
    echo("freqTypeToken <- data.frame(\n\t" + "types=slot(freq.analysis.obj, \"desc\")$freq.types,\n\t" + "token=slot(freq.analysis.obj, \"desc\")$freq.token\n)\n" + "rk.print(freqTypeToken)\n\n");  
  } else {}
  //// save result object
  // read in saveobject variables
  var saveFrq = getValue("saveFrq");
  var saveFrqActive = getValue("saveFrq.active");
  var saveFrqParent = getValue("saveFrq.parent");  var saveCorpFrq = getValue("saveCorpFrq");
  var saveCorpFrqActive = getValue("saveCorpFrq.active");
  var saveCorpFrqParent = getValue("saveCorpFrq.parent");
  // assign object to chosen environment
  if(saveFrqActive) {
    echo(".GlobalEnv$" + saveFrq + " <- freq.analysis.obj\n");
  } else {}
  if(saveCorpFrqActive) {
    echo(".GlobalEnv$" + saveCorpFrq + " <- corp.freq.obj\n");
  } else {}

}


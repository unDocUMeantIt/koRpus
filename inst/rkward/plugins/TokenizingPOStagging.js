// this code was generated using the rkwarddev package.
// perhaps don't make changes here, but in the rkwarddev script instead!
// 
// look for a file called: $SRC/inst/rkward/rkwarddev_koRpus_plugin_script.R



function preprocess(is_preview){
  // add requirements etc. here
  echo("require(koRpus)\n");

  var language = getValue("language");
  if(language == "nl") {
    echo("require(koRpus.lang.nl)\n");  
  } else if(language == "pt") {
    echo("require(koRpus.lang.pt)\n");  
  } else {}
}

function calculate(is_preview){
  // read in variables from dialog
  var operationMode = getString("operationMode");
  var TTRootDir = getString("TTRootDir");
  var language = getString("language");
  var textFile = getString("textFile");
  var detectHeadlines = getBoolean("detectHeadlines.state");
  var detectParagraphs = getBoolean("detectParagraphs.state");
  var showTagged = getBoolean("showTagged.state");

  // the R code to be evaluated
  var valueDetectHeadlines = getValue("detectHeadlines");
  var valueDetectParagraphs = getValue("detectParagraphs");
  // define the array arrDetect for values of R option "detect"
  var arrDetect = new Array();
  arrDetect.push(valueDetectHeadlines, valueDetectParagraphs);
  // clean array arrDetect from empty strings
  arrDetect = arrDetect.filter(String);
  // set the actual variable optDetect for R option "detect=c()"
  if(arrDetect.length > 0) {
    var optDetect = ",\n\tdetect=c(" + arrDetect.join(", ") + ")";
  } else {
    var optDetect = "";
  }

  var TTLang = language;
  if(operationMode == "file") {
    echo("tagged.text.obj <- tokenize(\n\t\"" + textFile + "\",\n\tlang=\"" + TTLang + "\"" + optDetect + "\n)\n\n");  
  } else {
    echo("tagged.text.obj <- treetag(\n\t\"" + textFile + "\",\n\ttreetagger=\"manual\",\n\tlang=\"" + TTLang + "\",\n\tTT.options=list(path=\"" + TTRootDir + "\",\n\tpreset=\"" + language + "\")\n)\n\n");  
  }
}

function printout(is_preview){
  // printout the results
  var textFile = getValue("textFile");
  var language = getValue("language");
  var showTagged = getValue("showTagged");
  new Header(i18n("Tokenizing & POS tagging results")).add(i18n("Text"), getValue("textFile")).add(i18n("Language"), getValue("language")).print();
  new Header(i18n("Word class distribution"), 3).print();
  echo("rk.print(summary(tagged.text.obj))\n");
  if(showTagged) {
    new Header(i18n("Tagged text"), 3).print();  
    echo("rk.print(taggedText(tagged.text.obj))\n");  
  } else {}
  echo("\n");
  //// save result object
  // read in saveobject variables
  var saveTaggedText = getValue("saveTaggedText");
  var saveTaggedTextActive = getValue("saveTaggedText.active");
  var saveTaggedTextParent = getValue("saveTaggedText.parent");
  // assign object to chosen environment
  if(saveTaggedTextActive) {
    echo(".GlobalEnv$" + saveTaggedText + " <- tagged.text.obj\n");
  } else {}

}


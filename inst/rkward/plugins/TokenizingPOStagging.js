



function preprocess(){
  // add requirements etc. here
  echo("require(koRpus)\n");
}

function calculate(){
  // read in variables from dialog
  var operationMode = getString("operationMode");
  var TTRootDir = getString("TTRootDir");
  var language = getString("language");
  var textFile = getString("textFile");
  var saveTaggedText = getString("saveTaggedText");
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

  if(language == "de-utf8") {
    var TTLang = "de";  
  } else if(language == "fr-utf8") {
    var TTLang = "fr";  
  } else if(language == "es-utf8") {
    var TTLang = "es";  
  } else if(language == "it-utf8") {
    var TTLang = "it";  
  } else {
    var TTLang = language;  
  }
  if(operationMode == "file") {
    echo("tagged.text.obj <- tokenize(\n\t\"" + textFile + "\",\n\tlang=\"" + TTLang + "\"" + optDetect + "\n)\n\n");  
  } else {
    echo("tagged.text.obj <- treetag(\n\t\"" + textFile + "\",\n\ttreetagger=\"manual\",\n\tlang=\"" + TTLang + "\",\n\tTT.options=list(path=\"" + TTRootDir + "\",\n\tpreset=\"" + language + "\")\n)\n\n");  
  }
}

function printout(){
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






function preprocess(){
  // add requirements etc. here

}

function calculate(){
  // read in variables from dialog
  var varHyphenTagged = getString("varHyphenTagged");
  var saveHyphen = getString("saveHyphen");
  var showHyphenation = getBoolean("showHyphenation.state");

  // the R code to be evaluated
  echo("hyphenated.text.obj <- hyphen(\n\t" + varHyphenTagged + ",\n\tquiet=TRUE\n)\n\n");
}

function printout(){
  // printout the results
  new Header(i18n("Hyphenation results")).print();

  var showHyphenation = getValue("showHyphenation");
  if(showHyphenation) {
    echo("rk.print(hyphenated.text.obj@hyphen)\n\n");  
  } else {}
  //// save result object
  // read in saveobject variables
  var saveHyphen = getValue("saveHyphen");
  var saveHyphenActive = getValue("saveHyphen.active");
  var saveHyphenParent = getValue("saveHyphen.parent");
  // assign object to chosen environment
  if(saveHyphenActive) {
    echo(".GlobalEnv$" + saveHyphen + " <- hyphenated.text.obj\n");
  } else {}

}


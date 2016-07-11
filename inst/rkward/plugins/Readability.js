// this code was generated using the rkwarddev package.
// perhaps don't make changes here, but in the rkwarddev script instead!
// 
// look for a file called: $SRC/inst/rkward/rkwarddev_koRpus_plugin_script.R



function preprocess(is_preview){
  // add requirements etc. here

}

function calculate(is_preview){
  // read in variables from dialog
  var varHyphenTagged = getString("varHyphenTagged");
  var varTaggedHyphenated = getString("varTaggedHyphenated");
  var readbWLldc = getString("readbWLldc");
  var readbWLsdc = getString("readbWLsdc");
  var readbWLHaJa = getString("readbWLHaJa");
  var ARI = getBoolean("ARI.state");
  var ARINRI = getBoolean("ARINRI.state");
  var ColemanLiau = getBoolean("ColemanLiau.state");
  var DanielsonBryan = getBoolean("DanielsonBryan.state");
  var DickesSteiwer = getBoolean("DickesSteiwer.state");
  var Fucks = getBoolean("Fucks.state");
  var LIX = getBoolean("LIX.state");
  var RIX = getBoolean("RIX.state");
  var Coleman = getBoolean("Coleman.state");
  var ELF = getBoolean("ELF.state");
  var FarrJenkinsPaterson = getBoolean("FarrJenkinsPaterson.state");
  var FarrJenkinsPatersonPSK = getBoolean("FarrJenkinsPatersonPSK.state");
  var Flesch = getBoolean("Flesch.state");
  var FleschDE = getBoolean("FleschDE.state");
  var FleschES = getBoolean("FleschES.state");
  var FleschSzigriszt = getBoolean("FleschSzigriszt.state");
  var FleschFR = getBoolean("FleschFR.state");
  var FleschNL = getBoolean("FleschNL.state");
  var FleschNLB = getBoolean("FleschNLB.state");
  var FleschPSK = getBoolean("FleschPSK.state");
  var FleschKincaid = getBoolean("FleschKincaid.state");
  var FOG = getBoolean("FOG.state");
  var FOGPSK = getBoolean("FOGPSK.state");
  var FOGNRI = getBoolean("FOGNRI.state");
  var FORCAST = getBoolean("FORCAST.state");
  var FORCASTRGL = getBoolean("FORCASTRGL.state");
  var TRI = getBoolean("TRI.state");
  var LinsearWrite = getBoolean("LinsearWrite.state");
  var SMOG = getBoolean("SMOG.state");
  var SMOGC = getBoolean("SMOGC.state");
  var SMOGsimple = getBoolean("SMOGsimple.state");
  var Qu = getBoolean("Qu.state");
  var Strain = getBoolean("Strain.state");
  var Tuldava = getBoolean("Tuldava.state");
  var WheelerSmith = getBoolean("WheelerSmith.state");
  var WheelerSmithDE = getBoolean("WheelerSmithDE.state");
  var nWS = getBoolean("nWS.state");
  var Bormuth = getBoolean("Bormuth.state");
  var DaleChall = getBoolean("DaleChall.state");
  var DaleChallPSK = getBoolean("DaleChallPSK.state");
  var DaleChallOld = getBoolean("DaleChallOld.state");
  var DRP = getBoolean("DRP.state");
  var HarrisJacobson = getBoolean("HarrisJacobson.state");
  var Spache = getBoolean("Spache.state");
  var SpacheOld = getBoolean("SpacheOld.state");

  // the R code to be evaluated
  var valueARI = getValue("ARI");
  var valueARINRI = getValue("ARINRI");
  var valueBormuth = getValue("Bormuth");
  var valueColeman = getValue("Coleman");
  var valueColemanLiau = getValue("ColemanLiau");
  var valueDaleChall = getValue("DaleChall");
  var valueDaleChallPSK = getValue("DaleChallPSK");
  var valueDaleChallOld = getValue("DaleChallOld");
  var valueDanielsonBryan = getValue("DanielsonBryan");
  var valueDickesSteiwer = getValue("DickesSteiwer");
  var valueDRP = getValue("DRP");
  var valueELF = getValue("ELF");
  var valueFarrJenkinsPaterson = getValue("FarrJenkinsPaterson");
  var valueFarrJenkinsPatersonPSK = getValue("FarrJenkinsPatersonPSK");
  var valueFlesch = getValue("Flesch");
  var valueFleschES = getValue("FleschES");
  var valueFleschSzigriszt = getValue("FleschSzigriszt");
  var valueFleschNL = getValue("FleschNL");
  var valueFleschNLB = getValue("FleschNLB");
  var valueFleschDE = getValue("FleschDE");
  var valueFleschFR = getValue("FleschFR");
  var valueFleschPSK = getValue("FleschPSK");
  var valueFleschKincaid = getValue("FleschKincaid");
  var valueFOG = getValue("FOG");
  var valueFOGPSK = getValue("FOGPSK");
  var valueFOGNRI = getValue("FOGNRI");
  var valueFORCAST = getValue("FORCAST");
  var valueFORCASTRGL = getValue("FORCASTRGL");
  var valueFucks = getValue("Fucks");
  var valueHarrisJacobson = getValue("HarrisJacobson");
  var valueLinsearWrite = getValue("LinsearWrite");
  var valueLIX = getValue("LIX");
  var valueNWS = getValue("nWS");
  var valueQu = getValue("Qu");
  var valueRIX = getValue("RIX");
  var valueSMOG = getValue("SMOG");
  var valueSMOGC = getValue("SMOGC");
  var valueSMOGsimple = getValue("SMOGsimple");
  var valueStrain = getValue("Strain");
  var valueSpache = getValue("Spache");
  var valueSpacheOld = getValue("SpacheOld");
  var valueTRI = getValue("TRI");
  var valueTuldava = getValue("Tuldava");
  var valueWheelerSmith = getValue("WheelerSmith");
  var valueWheelerSmithDE = getValue("WheelerSmithDE");
  // define the array arrIndex for values of R option "index"
  var arrIndex = new Array();
  arrIndex.push(valueARI, valueARINRI, valueBormuth, valueColeman, valueColemanLiau, valueDaleChall, valueDaleChallPSK, valueDaleChallOld, valueDanielsonBryan, valueDickesSteiwer, valueDRP, valueELF, valueFarrJenkinsPaterson, valueFarrJenkinsPatersonPSK, valueFlesch, valueFleschDE, valueFleschES, valueFleschSzigriszt, valueFleschFR, valueFleschNL, valueFleschNLB, valueFleschPSK, valueFleschKincaid, valueFOG, valueFOGPSK, valueFOGNRI, valueFORCAST, valueFORCASTRGL, valueFucks, valueHarrisJacobson, valueLinsearWrite, valueLIX, valueNWS, valueQu, valueRIX, valueSMOG, valueSMOGC, valueSMOGsimple, valueSpache, valueSpacheOld, valueStrain, valueTRI, valueTuldava, valueWheelerSmith, valueWheelerSmithDE);
  // clean array arrIndex from empty strings
  arrIndex = arrIndex.filter(String);
  // set the actual variable optIndex for R option "index=c()"
  if(arrIndex.length > 0) {
    var optIndex = ",\n\tindex=c(\"" + arrIndex.join("\", \"") + "\")";
  } else {
    var optIndex = "";
  }

  // define the array arrRdbWordLists for values of R option "word.lists"
  var arrRdbWordLists = new Array();
  if((Bormuth || DRP) && readbWLldc) {
    arrRdbWordLists.push("\n\t\tBormuth=\"" + readbWLldc + "\"");
  } else {
    arrRdbWordLists.push();
  }
  if((DaleChall || DaleChallPSK || DaleChallOld) && readbWLldc) {
    arrRdbWordLists.push("\n\t\tDale.Chall=\"" + readbWLldc + "\"");
  } else {
    arrRdbWordLists.push();
  }
  if(HarrisJacobson && readbWLHaJa) {
    arrRdbWordLists.push("\n\t\tHarris.Jacobson=\"" + readbWLHaJa + "\"");
  } else {
    arrRdbWordLists.push();
  }
  if((Spache || SpacheOld) && readbWLsdc) {
    arrRdbWordLists.push("\n\t\tSpache=\"" + readbWLsdc + "\"");
  } else {
    arrRdbWordLists.push();
  }
  // clean array arrRdbWordLists from empty strings
  arrRdbWordLists = arrRdbWordLists.filter(String);
  // set the actual variable rdbWordLists with all values for R option "word.lists"
  if(arrRdbWordLists.length > 0) {
    var rdbWordLists = ",\n\tword.lists=list(" + arrRdbWordLists.join(", ") + ")";
  } else {
    var rdbWordLists = "";
  }

  echo("readability.obj <- readability(\n\t" + varHyphenTagged + optIndex);
  if(varTaggedHyphenated != "") {
    echo(",\n\thyphen=" + varTaggedHyphenated);  
  } else {}
  echo(rdbWordLists);
  echo(",\n\tquiet=TRUE\n)\n\n");
}

function printout(is_preview){
  // printout the results
  new Header(i18n("Readability results")).print();
  echo("rk.results(summary(readability.obj))\n");
  //// save result object
  // read in saveobject variables
  var saveReadb = getValue("saveReadb");
  var saveReadbActive = getValue("saveReadb.active");
  var saveReadbParent = getValue("saveReadb.parent");
  // assign object to chosen environment
  if(saveReadbActive) {
    echo(".GlobalEnv$" + saveReadb + " <- readability.obj\n");
  } else {}

}


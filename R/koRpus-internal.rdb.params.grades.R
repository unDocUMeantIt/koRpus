# Copyright 2010-2014 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package koRpus.
#
# koRpus is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# koRpus is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with koRpus.  If not, see <http://www.gnu.org/licenses/>.


# these functions are primarily being used by readability()
# and have been moved here to make the code more readable itself ;-)

## set default parameters
default.params <- function(){
  default.parameters <- list(
    ARI=c(asl=0.5, awl=4.71, const=21.43),
    Bormuth=list(
      clz=35,
      meanc=c(const=0.886593, awl=0.08364, afw=0.161911, asl1=0.021401, asl2=0.000577, asl3=0.000005),
      grade=c(const=4.275, m1=12.881, m2=34.934, m3=20.388, c1=26.194, c2=2.046, c3=11.767, mc1=44.285, mc2=97.62, mc3=59.538)),
    Coleman=list(syll=1,
      clz1=c(word=1.29, const=38.45),
      clz2=c(word=1.16, sntc=1.48, const=37.95),
      clz3=c(word=1.07, sntc=1.18, pron=0.76, const=34.02),
      clz4=c(word=1.04, sntc=1.06, pron=0.56, prep=0.36, const=26.01)),
    Coleman.Liau=list(
      ecp=c(const=141.8401, char=0.21459, sntc=1.079812),
      grade=c(ecp=-27.4004, const=23.06395),
      short=c(awl=5.88, spw=29.6, const=15.8)),
    Dale.Chall=c(const=64, dword=0.95, asl=0.69),
    Danielson.Bryan=list(
      db1=c(cpb=1.0364, cps=0.0194, const=0.6059),
      db2=c(const=131.059, cpb=10.364, cps=0.194)),
    Dickes.Steiwer=list(const=235.95993, awl=73.021, asl=12.56438, ttr=50.03293, case.sens=FALSE),
    ELF=c(syll=2),
    Farr.Jenkins.Paterson=c(const=-31.517, asl=1.015, monsy=1.599),
    Flesch=c(const=206.835, asl=1.015, asw=84.6),
    Flesch.Kincaid=c(asl=0.39, asw=11.8, const=15.59),
    FOG=list(syll=3, const=0.4, suffix=c("es", "ed", "ing")),
    FORCAST=c(syll=1, mult=.10, const=20),
    Harris.Jacobson=list(char=6,
      hj1=c(dword=0.094, asl=0.168, const=0.502),
      hj2=c(dword=0.140, asl=0.153, const=0.560),
      hj3=c(asl=0.158, lword=0.055, const=0.355),
      hj4=c(dword=0.070, asl=0.125, lword=0.037, const=0.497),
      hj5=c(dword=0.118, asl=0.134, lword=0.032, const=0.424)),
    Linsear.Write=c(short.syll=2, long.syll=3, thrs=20),
    LIX=c(char=6, const=100),
    nWS=list(ms.syll=3, iw.char=6, es.syll=1,
      nws1=c(ms=19.35, sl=0.1672, iw=12.97, es=3.27, const=0.875),
      nws2=c(ms=20.07, sl=0.1682, iw=13.73, const=2.779),
      nws3=c(ms=29.63, sl=0.1905, const=1.1144),
      nws4=c(ms=27.44, sl=0.2656, const=1.693)),
    RIX=c(char=6),
    SMOG=c(syll=3, sqrt=1.043, fact=30, const=3.1291, sqrt.const=0),
    Spache=c(asl=0.121, dword=0.082, const=0.659),
    Strain=c(sent=3, const=10),
    Traenkle.Bailer=list(
      TB1=c(const=224.6814, awl=79.8304, asl=12.24032, prep=1.292857),
      TB2=c(const=234.1063, awl=96.11069, prep=2.05444, conj=1.02805)),
    TRI=c(syll=1, word=0.449, pnct=2.467, frgn=0.937, const=14.417),
    Tuldava=c(syll=1, word1=1, word2=1, sent=1),
    Wheeler.Smith=c(syll=2)
  )
}

## grade levels
## function get.grade.level()
get.grade.level <- function(raw, measure, lang="en"){
  grade.level <- NA
  grade.level.num <- NA
  reading.age <- NA
  reading.age.num <- NA
  # Dale-Chall
  if(identical(measure, "Dale.Chall")){
    if(raw >= 10){
      grade.level <- ">= 16 (college graduate)"
      grade.level.num <- 16
      reading.age <- ">= 22"
      reading.age.num <- 22
    } else {}
    if(raw < 10){
      grade.level <- "13-15 (college)"
      grade.level.num <- 13
      reading.age <- "18-22"
      reading.age.num <- 18
    } else {}
    if(raw < 9){
      grade.level <- "11-12"
      grade.level.num <- 11
      reading.age <- "16-18"
      reading.age.num <- 16
    } else {}
    if(raw < 8){
      grade.level <- "9-10"
      grade.level.num <- 9
      reading.age <- "14-16"
      reading.age.num <- 14
    } else {}
    if(raw < 7){
      grade.level <- "7-8"
      grade.level.num <- 7
      reading.age <- "12-14"
      reading.age.num <- 12
    } else {}
    if(raw < 6){
      grade.level <- "5-6"
      grade.level.num <- 5
      reading.age <- "10-12"
      reading.age.num <- 10
    } else {}
    if(raw < 5){
      grade.level <- "< 4"
      grade.level.num <- 1
      reading.age <- "5-10"
      reading.age.num <- 5
    } else {}
  } else {}
  if(identical(measure, "Dale.Chall.PSK")){
    grade.level.num <- raw
    if(raw >= 15.5){
      grade.level <- ">= 16 (college graduate)"
      reading.age <- ">= 22"
      reading.age.num <- 22
    } else {}
    if(raw < 15.5){
      grade.level <- "13-15 (college)"
      reading.age <- "18-22"
      reading.age.num <- 18
    } else {}
    if(raw < 12.5){
      grade.level <- "11-12"
      reading.age <- "16-18"
      reading.age.num <- 16
    } else {}
    if(raw < 10.5){
      grade.level <- "9-10"
      reading.age <- "14-16"
      reading.age.num <- 14
    } else {}
    if(raw < 8.5){
      grade.level <- "7-8"
      reading.age <- "12-14"
      reading.age.num <- 12
    } else {}
    if(raw < 6.5){
      grade.level <- "5-6"
      reading.age <- "10-12"
      reading.age.num <- 10
    } else {}
    if(raw < 4.5){
      grade.level <- "<= 4"
      reading.age <- "5-10"
      reading.age.num <- 5
    } else {}
  } else {} # ende dale-chall psk
  if(identical(measure, "Dale.Chall.new")){
    if(raw <= 15){
      grade.level <- ">= 16 (college graduate)"
      grade.level.num <- 16
      reading.age <- ">= 22"
      reading.age.num <- 22
    } else {}
    if(raw > 15){
      grade.level <- "13-15 (college)"
      grade.level.num <- 13
      reading.age <- "18-22"
      reading.age.num <- 18
    } else {}
    if(raw > 21){
      grade.level <- "11-12"
      grade.level.num <- 11
      reading.age <- "16-18"
      reading.age.num <- 16
    } else {}
    if(raw > 27){
      grade.level <- "9-10"
      grade.level.num <- 9
      reading.age <- "14-16"
      reading.age.num <- 14
    } else {}
    if(raw > 33){
      grade.level <- "7-8"
      grade.level.num <- 7
      reading.age <- "12-14"
      reading.age.num <- 12
    } else {}
    if(raw > 39){
      grade.level <- "5-6"
      grade.level.num <- 5
      reading.age <- "10-12"
      reading.age.num <- 10
    } else {}
    if(raw > 44){
      grade.level <- "4"
      grade.level.num <- 4
      reading.age <- "9-10"
      reading.age.num <- 9
    } else {}
    if(raw > 49){
      grade.level <- "3"
      grade.level.num <- 3
      reading.age <- "8-9"
      reading.age.num <- 8
    } else {}
    if(raw > 53){
      grade.level <- "2"
      grade.level.num <- 2
      reading.age <- "7-8"
      reading.age.num <- 7
    } else {}
    if(raw > 57){
      grade.level <- "1"
      grade.level.num <- 1
      reading.age <- "6-7"
      reading.age.num <- 6
    } else {}
  } else {}
  # end dale.chall

  # Danielson.Bryan
  if(identical(measure, "Danielson.Bryan")){
    if(raw >= 90){
      grade.level <- "<= 3"
      grade.level.num <- 3
    } else {}
    if(raw < 90){
      grade.level <- "4"
      grade.level.num <- 4
    } else {}
    if(raw < 80){
      grade.level <- "5"
      grade.level.num <- 5
    } else {}
    if(raw < 70){
      grade.level <- "6"
      grade.level.num <- 6
    } else {}
    if(raw < 60){
      grade.level <- "7-8"
      grade.level.num <- 7
    } else {}
    if(raw < 50){
      grade.level <- "9-12"
      grade.level.num <- 9
    } else {}
    if(raw < 30){
      grade.level <- ">= 13 (college)"
      grade.level.num <- 13
    } else {}
  } else {}
  # end Danielson.Bryan

  # Flesch RE
  if(identical(measure, "Flesch")){
    if(raw < 30){
  #  0 to 30. . . . . college graduate.
      grade.level <- ">= 16 (college graduate)"
      grade.level.num <- 16
    } else {}
    if(raw >= 30){
  #  30 to 50. . . . . 13th to 16th grade (college level) 
      grade.level <- ">= 13 (college)"
      grade.level.num <- 13
    } else {}
    if(raw >= 50){
  #  50 to 60. . . . . 10 to 12th grade (high school) 
      grade.level <- ">= 10 (high school)"
      grade.level.num <- 10
    } else {}
    if(raw >= 60){
  #  60 to 70. . . . . 8th to 9th grade 
      grade.level <- "8-9"
      grade.level.num <- 8
    } else {}
    if(raw >= 70){
  #  70 to 80. . . . . 7th grade 
      grade.level <- "7"
      grade.level.num <- 7
    } else {}
    if(raw >= 80){
  #  80 to 90. . . . . 6th grade 
      grade.level <- "6"
      grade.level.num <- 6
    } else {}
    if(raw >= 90){
  # 90 to 100. . . . .5th grade 
      grade.level <- "5"
      grade.level.num <- 5
    } else {}
  } else {}
  # end Flesch RE


  # LIX
  #   0-24  Very easy
  #   25-34  Easy
  #   35-44  Standard
  #   45-54  Difficult
  #   55+  Very difficult
  if(identical(measure, "LIX")){
    if(raw <= 24){
      grade.level <- "very easy"
    } else {}
    if(raw > 24){
      grade.level <- "easy"
    } else {}
    if(raw > 34){
      grade.level <- "standard"
    } else {}
    if(raw > 44){
      grade.level <- "difficult"
    } else {}
    if(raw > 54){
      grade.level <- "very difficult"
    } else {}
  } else {}
  if(identical(measure, "LIX.grade")){
    if(raw < 34){
      grade.level <- "< 5"
      grade.level.num <- 4
    } else {}
    if(raw >= 34){
      grade.level <- "5"
      grade.level.num <- 5
    } else {}
    if(raw >= 38){
      grade.level <- "6"
      grade.level.num <- 6
    } else {}
    if(raw >= 41){
      grade.level <- "7"
      grade.level.num <- 7
    } else {}
    if(raw >= 44){
      grade.level <- "8"
      grade.level.num <- 8
    } else {}
    if(raw >= 48){
      grade.level <- "9"
      grade.level.num <- 9
    } else {}
    if(raw >= 51){
      grade.level <- "10"
      grade.level.num <- 10
    } else {}
    if(raw >= 54){
      grade.level <- "11"
      grade.level.num <- 11
    } else {}
    if(raw > 57){
      grade.level <- "> 11"
      grade.level.num <- 12
    } else {}
  } else {}
  # end LIX

  # RIX
  if(identical(measure, "RIX")){
    if(raw < 0.2){
      grade.level <- "1"
      grade.level.num <- 1
    } else {}
    if(raw >= 0.2){
      grade.level <- "2"
      grade.level.num <- 2
    } else {}
    if(raw >= 0.5){
      grade.level <- "3"
      grade.level.num <- 3
    } else {}
    if(raw >= 0.8){
      grade.level <- "4"
      grade.level.num <- 4
    } else {}
    if(raw >= 1.3){
      grade.level <- "5"
      grade.level.num <- 5
    } else {}
    if(raw >= 1.8){
      grade.level <- "6"
      grade.level.num <- 6
    } else {}
    if(raw >= 2.4){
      grade.level <- "7"
      grade.level.num <- 7
    } else {}
    if(raw >= 3.0){
      grade.level <- "8"
      grade.level.num <- 8
    } else {}
    if(raw >= 3.7){
      grade.level <- "9"
      grade.level.num <- 9
    } else {}
    if(raw >= 4.5){
      grade.level <- "10"
      grade.level.num <- 10
    } else {}
    if(raw >= 5.3){
      grade.level <- "11"
      grade.level.num <- 11
    } else {}
    if(raw >= 6.2){
      grade.level <- "12"
      grade.level.num <- 12
    } else {}
    if(raw >= 7.2){
      grade.level <- "> 12 (college)"
      grade.level.num <- 13
    } else {}
  } else {}
  # end RIX

  # Wheeler-Smith
  if(identical(measure, "Wheeler.Smith")){
    if(raw < 8){
      grade.level <- "< 1"
      grade.level.num <- 0
    } else {}
    if(raw >= 8){
      grade.level <- "1"
      grade.level.num <- 1
    } else {}
    if(raw > 11.5){
      grade.level <- "2"
      grade.level.num <- 2
    } else {}
    if(raw > 19){
      grade.level <- "3"
      grade.level.num <- 3
    } else {}
    if(raw > 26.5){
      grade.level <- "4"
      grade.level.num <- 4
    } else {}
    if(raw >= 34.5){
      grade.level <- "> 4"
      grade.level.num <- 5
    } else {}
  } else {}
  if(identical(measure, "Wheeler.Smith.de")){
    if(raw < 2.5){
      grade.level <- "< 1"
      grade.level.num <- 0
    } else {}
    if(raw >= 2.5){
      grade.level <- "1"
      grade.level.num <- 1
    } else {}
    if(raw > 6){
      grade.level <- "2"
      grade.level.num <- 2
    } else {}
    if(raw > 9){
      grade.level <- "3"
      grade.level.num <- 3
    } else {}
    if(raw > 12){
      grade.level <- "4"
      grade.level.num <- 4
    } else {}
    if(raw > 16){
      grade.level <- "5"
      grade.level.num <- 5
    } else {}
    if(raw > 20){
      grade.level <- "6"
      grade.level.num <- 6
    } else {}
    if(raw > 24){
      grade.level <- "7"
      grade.level.num <- 7
    } else {}
    if(raw > 29){
      grade.level <- "8"
      grade.level.num <- 8
    } else {}
    if(raw >= 34){
      grade.level <- "> 8"
      grade.level.num <- 9
    } else {}
  } else {}
  # end Wheeler-Smith

  # Coleman.Liau
   # probably not needed, grade is directly computed
  if(identical(measure, "Coleman.Liau")){
    if(raw < 0.2){
      grade.level <- "1"
      grade.level.num <- 1
    } else {}
    if(raw >= 0.2){
      grade.level <- "2"
      grade.level.num <- 2
    } else {}
    if(raw >= 0.5){
      grade.level <- "3"
      grade.level.num <- 3
    } else {}
    if(raw >= 0.8){
      grade.level <- "4"
      grade.level.num <- 4
    } else {}
    if(raw >= 1.3){
      grade.level <- "5"
      grade.level.num <- 5
    } else {}
    if(raw >= 1.8){
      grade.level <- "6"
      grade.level.num <- 6
    } else {}
    if(raw >= 2.4){
      grade.level <- "7"
      grade.level.num <- 7
    } else {}
    if(raw >= 3.0){
      grade.level <- "8"
      grade.level.num <- 8
    } else {}
    if(raw >= 3.7){
      grade.level <- "9"
      grade.level.num <- 9
    } else {}
    if(raw >= 4.5){
      grade.level <- "10"
      grade.level.num <- 10
    } else {}
    if(raw >= 5.3){
      grade.level <- "11"
      grade.level.num <- 11
    } else {}
    if(raw >= 6.2){
      grade.level <- "12"
      grade.level.num <- 12
    } else {}
    if(raw >= 7.2){
      grade.level <- "college"
      grade.level.num <- 13
    } else {}
  } else {}
  # end Coleman.Liau

  results <- list(grade=grade.level, grade.min=grade.level.num, age=reading.age, age.min=reading.age.num)
  return(results)
} ## end function get.grade.level

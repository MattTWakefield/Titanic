library(foreign)
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

PDR2015<-fread("C:/PDR_COPY/NFIRS Data/2015/basicincident.txt", sep="^")
PDR2015F<-fread("C:/PDR_COPY/NFIRS Data/2015/fireincident.txt", sep="^")
PDR2015<-fread("C:/PDR_COPY/NFIRS Data/2015/basicincident.txt", sep="^")
PDR2015F<-fread("C:/PDR_COPY/NFIRS Data/2015/fireincident.txt", sep="^")
PDR2015<-fread("C:/PDR_COPY/NFIRS Data/2015/basicincident.txt", sep="^")
PDR2015F<-fread("C:/PDR_COPY/NFIRS Data/2015/fireincident.txt", sep="^")


PDR2015<-PDR2015%>%filter(as.numeric(as.character(INC_TYPE))>=100
       , as.numeric(as.character(INC_TYPE))<=123
       , !is.na(STATE)
       , AID == '1' 
       | AID == '2'
       | AID == 'N')

PDRID<-paste0(
str_pad(PDR2015$STATE, max(str_length(PDR2015$STATE),na.rm = TRUE),"left","0"),
str_pad(PDR2015$FDID, max(str_length(PDR2015$FDID),na.rm = TRUE),"left","0"),
str_pad(PDR2015$INC_DATE, max(str_length(PDR2015$INC_DATE),na.rm = TRUE),"left","0"),
str_pad(PDR2015$INC_NO, max(str_length(PDR2015$INC_NO),na.rm = TRUE),"left","0"),
str_pad(PDR2015$EXP_NO, max(str_length(PDR2015$EXP_NO),na.rm = TRUE),"left","0"))

PDR2015<-cbind(PDRID,PDR2015)



PDRIDF<-paste0(
  str_pad(PDR2015F$STATE, max(str_length(PDR2015F$STATE),na.rm = TRUE),"left","0"),
  str_pad(PDR2015F$FDID, max(str_length(PDR2015F$FDID),na.rm = TRUE),"left","0"),
  str_pad(PDR2015F$INC_DATE, max(str_length(PDR2015F$INC_DATE),na.rm = TRUE),"left","0"),
  str_pad(PDR2015F$INC_NO, max(str_length(PDR2015F$INC_NO),na.rm = TRUE),"left","0"),
  str_pad(PDR2015F$EXP_NO, max(str_length(PDR2015F$EXP_NO),na.rm = TRUE),"left","0"))

PDR2015F<-cbind(PDRIDF,PDR2015F)

left_join(PDR2015,)
library(foreign)
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

PDR2015<-fread("C:/PDR_COPY/NFIRS Data/2015/basicincident.txt", sep="^", colClasses = c("ALARM"="numeric", "ARRIVAL"="numeric","INC_CONT" = "numeric","LU_CLEAR" = "numeric"),stringsAsFactors = TRUE)
PDR2015F<-fread("C:/PDR_COPY/NFIRS Data/2015/fireincident.txt", sep="^", stringsAsFactors = TRUE)
PDR2014<-fread("C:/PDR_COPY/NFIRS Data/2014/basicincident.txt", sep="^", colClasses = c("ALARM"="numeric", "ARRIVAL"="numeric","INC_CONT" = "numeric","LU_CLEAR" = "numeric"),stringsAsFactors = TRUE)
PDR2014F<-fread("C:/PDR_COPY/NFIRS Data/2014/fireincident.txt", sep="^", stringsAsFactors = TRUE)
PDR2013<-fread("C:/PDR_COPY/NFIRS Data/2013/basicincident.txt", sep="^", colClasses = c("ALARM"="numeric", "ARRIVAL"="numeric","INC_CONT" = "numeric","LU_CLEAR" = "numeric"),stringsAsFactors = TRUE)
PDR2013F<-fread("C:/PDR_COPY/NFIRS Data/2013/fireincident.txt", sep="^", stringsAsFactors = TRUE)
PDR2012<-fread("C:/PDR_COPY/NFIRS Data/2012/basicincident.txt", sep="^", colClasses = c("ALARM"="numeric", "ARRIVAL"="numeric","INC_CONT" = "numeric","LU_CLEAR" = "numeric"),stringsAsFactors = TRUE)
PDR2012F<-fread("C:/PDR_COPY/NFIRS Data/2012/fireincident.txt", sep="^", stringsAsFactors = TRUE)
PDR2011<-read.dbf("C:/PDR_COPY/NFIRS DATA/2011/basicincident.dbf")
PDR2011F<-read.dbf("C:/PDR_COPY/NFIRS DATA/2011/fireincident.dbf")

PDR<-rbind(PDR2011, PDR2012, PDR2013, PDR2014, PDR2015)
rm(PDR2011, PDR2012, PDR2013, PDR2014, PDR2015)
gc()

PDRF<-rbind(PDR2011F, PDR2012F, PDR2013F, PDR2014F, PDR2015F)
rm(PDR2011F, PDR2012F, PDR2013F, PDR2014F, PDR2015F)
gc()


#Excludes confied fires.
PDR<-PDR%>%filter(as.numeric(as.character(INC_TYPE))>=110
       , as.numeric(as.character(INC_TYPE))<=123
       , !as.numeric(as.character(INC_TYPE)) %in% c(113,114,115,116,117,118)
       , !is.na(STATE)
       , AID == '1' 
       | AID == '2'
       | AID == 'N')

PDRID<-paste0(
str_pad(PDR$STATE, max(str_length(PDR$STATE),na.rm = TRUE),"left","0"),
str_pad(PDR$FDID, max(str_length(PDR$FDID),na.rm = TRUE),"left","0"),
str_pad(PDR$INC_DATE, max(str_length(PDR$INC_DATE),na.rm = TRUE),"left","0"),
str_pad(PDR$INC_NO, max(str_length(PDR$INC_NO),na.rm = TRUE),"left","0"),
str_pad(PDR$EXP_NO, max(str_length(PDR$EXP_NO),na.rm = TRUE),"left","0"))

PDR<-cbind(PDRID,PDR)
PDR$PDRID<-as.character(PDR$PDRID)


PDRF<-PDRF[str_length(PDRF$EXP_NO)!=6,]
PDRIDF<-paste0(
  str_pad(PDRF$STATE, max(str_length(PDRF$STATE),na.rm = TRUE),"left","0"),
  str_pad(PDRF$FDID, max(str_length(PDRF$FDID),na.rm = TRUE),"left","0"),
  str_pad(PDRF$INC_DATE, max(str_length(PDRF$INC_DATE),na.rm = TRUE),"left","0"),
  str_pad(PDRF$INC_NO, max(str_length(PDRF$INC_NO),na.rm = TRUE),"left","0"),
  str_pad(PDRF$EXP_NO, max(str_length(PDRF$EXP_NO),na.rm = TRUE),"left","0"))

PDRF<-cbind(PDRIDF,PDRF)  


 
Big15<-inner_join(PDR,PDRF, by=c("PDRID"="PDRIDF"))
Big15<-Big15%>%filter(as.numeric(as.character(INC_TYPE))>=110


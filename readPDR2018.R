readPDR<-function(){

library(foreign)
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

PDR2015<-fread("C:/PDR_COPY/NFIRS Data/2015/basicincident.txt", sep="^", colClasses = c("ALARM"="numeric", "ARRIVAL"="numeric","INC_CONT" = "numeric","LU_CLEAR" = "numeric"),stringsAsFactors = TRUE)
PDR2015F<-fread("C:/PDR_COPY/NFIRS Data/2015/fireincident.txt", sep="^", stringsAsFactors = TRUE)
print("2015 Data Read")
PDR2014<-fread("C:/PDR_COPY/NFIRS Data/2014/basicincident.txt", sep="^", colClasses = c("ALARM"="numeric", "ARRIVAL"="numeric","INC_CONT" = "numeric","LU_CLEAR" = "numeric"),stringsAsFactors = TRUE)
PDR2014F<-fread("C:/PDR_COPY/NFIRS Data/2014/fireincident.txt", sep="^", stringsAsFactors = TRUE)
print("2014 Data Read")
PDR2013<-fread("C:/PDR_COPY/NFIRS Data/2013/basicincident.txt", sep="^", colClasses = c("ALARM"="numeric", "ARRIVAL"="numeric","INC_CONT" = "numeric","LU_CLEAR" = "numeric"),stringsAsFactors = TRUE)
PDR2013F<-fread("C:/PDR_COPY/NFIRS Data/2013/fireincident.txt", sep="^", stringsAsFactors = TRUE)
print("2013 Data Read")
PDR2012<-fread("C:/PDR_COPY/NFIRS Data/2012/basicincident.txt", sep="^", colClasses = c("ALARM"="numeric", "ARRIVAL"="numeric","INC_CONT" = "numeric","LU_CLEAR" = "numeric"),stringsAsFactors = TRUE)
PDR2012F<-fread("C:/PDR_COPY/NFIRS Data/2012/fireincident.txt", sep="^", stringsAsFactors = TRUE)
print("2012 Data Read")
PDR2011<-read.dbf("C:/PDR_COPY/NFIRS DATA/2011/basicincident.dbf")
PDR2011F<-read.dbf("C:/PDR_COPY/NFIRS DATA/2011/fireincident.dbf")
print("2011 Data Read")

PDR2015C<-fread("C:/PDR_COPY/NFIRS Data/2015/causes.txt", sep="^", stringsAsFactors = TRUE)
PDR2014C<-fread("C:/PDR_COPY/NFIRS Data/2014/causes.txt", sep=",", stringsAsFactors = TRUE)
PDR2013C<-fread("C:/PDR_COPY/NFIRS Data/2013/causes.txt", sep="^", stringsAsFactors = TRUE)
PDR2012C<-fread("C:/PDR_COPY/NFIRS Data/2012/causes.txt", sep="^", stringsAsFactors = TRUE)
PDR2011C<-read.dbf("C:/PDR_COPY/NFIRS Data/2011/causes11.dbf")
print("Caues Read")
print("Files Read")


PDR<-rbind(PDR2011, PDR2012, PDR2013, PDR2014, PDR2015)
rm(PDR2011, PDR2012, PDR2013, PDR2014, PDR2015)
gc()

PDRF<-rbind(PDR2011F, PDR2012F, PDR2013F, PDR2014F, PDR2015F)
rm(PDR2011F, PDR2012F, PDR2013F, PDR2014F, PDR2015F)
gc()

PDRC<-rbind(PDR2011C, PDR2012C, PDR2013C, PDR2014C, PDR2015C)
rm(PDR2011C, PDR2012C, PDR2013C, PDR2014C, PDR2015C)
gc()


print("Combined Years")
#Excludes confied fires.
PDR<-PDR%>%filter(as.numeric(as.character(INC_TYPE))>=110
       , as.numeric(as.character(INC_TYPE))<=123
       , !as.numeric(as.character(INC_TYPE)) %in% c(113,114,115,116,117,118)
       , !is.na(STATE)
       , AID == '1' 
       | AID == '2'
       | AID == 'N')

print("Filtered out non confined structure fires and aid given calls")

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

PDRC<-PDRC[str_length(PDRC$EXP_NO)!=6,]
PDRIDC<-paste0(
  str_pad(PDRC$STATE, max(str_length(PDRC$STATE),na.rm = TRUE),"left","0"),
  str_pad(PDRC$FDID, max(str_length(PDRC$FDID),na.rm = TRUE),"left","0"),
  str_pad(PDRC$INC_DATE, max(str_length(PDRC$INC_DATE),na.rm = TRUE),"left","0"),
  str_pad(PDRC$INC_NO, max(str_length(PDRC$INC_NO),na.rm = TRUE),"left","0"),
  str_pad(PDRC$EXP_NO, max(str_length(PDRC$EXP_NO),na.rm = TRUE),"left","0"))

PDRC<-cbind(PDRIDC,PDRC)

print("IDs Created")  

PDR<-inner_join(PDR,PDRF, by=c("PDRID"="PDRIDF"))

PDR<-left_join(PDR, PDRC, by=c("PDRID"="PDRIDC"))

print("joined basic, fire and cause file")

PDR$ALARM<-mdy_hm(PDR$ALARM)
PDR$ARRIVAL<-mdy_hm(PDR$ARRIVAL)
PDR$LU_CLEAR<-mdy_hm(PDR$LU_CLEAR)
PDR$INC_DATE<-mdy(PDR$INC_DATE)

PDR$year<-as.factor(year(PDR$ALARM))
PDR$month<-as.factor(month(PDR$ALARM))
PDR$hour<-as.factor(hour(PDR$ALARM))
PDR$weekday<-as.factor(weekdays(PDR$ALARM))
PDR$week<-as.factor(week(PDR$ALARM))




PDR<-PDR%>%mutate(GSM_FLAG = 
                    case_when(ALARM>='2016-11-27'& 
                                ALARM<'2016-11-29' & 
                                STATE == "TN" & FDID %in% c('78101',
                                                            '78113',
                                                            '78121',
                                                            '78133',
                                                            '78143',
                                                            '78151',
                                                            '78161',
                                                            '78171',
                                                            '78181',
                                                            '78721',
                                                            '78811')  ~ 1, 
                                                            TRUE ~ 0))


PDR$RT<-PDR$ARRIVAL-PDR$ALARM

PDR$PROP_USE<-as.character(PDR$PROP_USE)%>%as.integer()
PDR$INC_TYPE<-as.character(PDR$INC_TYPE)%>%as.integer()
PDR$CAUSE_CODE<-as.character(PDR$CAUSE_CODE)%>%as.integer()
PDR$ALARMS<-as.character(PDR$ALARMS)%>%as.integer()



PDR[is.na(PDR$OTH_DEATH),"OTH_DEATH"]<-0
print("converted variables, added GSM flag, and turned rows with null fatalities into 0")
rm(PDRC, PDRF, PDRID, PDRIDC, PDRIDF)
PDR<<-PDR
gc()
print("extranious objects removed & garbage collection")
return(PDR)
}





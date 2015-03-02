# Functions for basic data analysis.
# Function to tell you whether participants looked to Instrument on each trial
# The last SummaryBy statement can be modified to look at only the first X gazes, currently set at 4 to capture the first 3 eye movements (1 is starting point), currently 
LooksToInst = function(data){
require(plyr)
data = ddply(data,~Name.+Trial,transform,NextCode = c(Code[2:(length(Code))],Code[length(Code)]))
data$SwitInst = 0
data[data$NextCode %in% c("TI"),]$SwitInst = 1
data = ddply(data,~Name.+Trial,transform,LookEnd = c(TimeFrame[2:(length(TimeFrame))],TimeFrame[length(TimeFrame)]))
data$LookTime = data$LookEnd - data$TimeFrame
if ("RC" %in% colnames(data)){
data.Look <- summaryBy(Inst+SwitInst~Age+QCond+Cond+Pop+Name.+ItemNo+ExOrd, data = data[ data$LookTime > 0,], FUN = sum) # This and the above 3 lines ensure that we don't count any 1-frame-long looks, eg the last line of each trial in the datafile. 
}
else data.Look <- summaryBy(Inst+SwitInst~Age+QCond+Cond+Name.+ItemNo, data = data[ data$LookTime > 0,], FUN = sum) # This and the above 3 lines ensure that we don't count any 1-frame-long looks, eg the last line of each trial in the datafile. 
data.Look$Inst = 0
data.Look[data.Look$Inst.sum > 0,]$Inst = 1
return(data.Look)
}



#######



proc_subj.NoExpand = function(filename,Timing,PlaceCodes){
require(gdata)
subj = read.xls(filename, sheet=2)
subj <- subj[!grepl("set",subj$Marker.Name, ignore.case = TRUE),]
#subj <- subj[subj$Marker.Name %in% c("BL","CN","LL","LR","OT","UL","UR"),]

subj = merge(subj,PlaceCodes, by = "Trial", sort = FALSE)
subj = merge(subj,Timing, by = "Trial",sort = FALSE)



subj$Marker.Name = as.character(subj$Marker.Name)
subj$Marker.Name = gsub(" ","",subj$Marker.Name)
subj$Code = NA
subj$Code = subj$Marker.Name

for (i in unique(PlaceCodes$Trial)){
	if (length(subj[subj$Trial == i & subj$Marker.Name == "UL",]$Code) > 0){subj[subj$Trial == i & subj$Marker.Name == "UL",]$Code = as.character(PlaceCodes[PlaceCodes$Trial == i ,]$UL)}
	if (length(subj[subj$Trial == i & subj$Marker.Name == "UR",]$Code) > 0){subj[subj$Trial == i & subj$Marker.Name == "UR",]$Code = as.character(PlaceCodes[PlaceCodes$Trial == i ,]$UR)}
	if (length(subj[subj$Trial == i & subj$Marker.Name == "LL",]$Code) > 0){subj[subj$Trial == i & subj$Marker.Name == "LL",]$Code = as.character(PlaceCodes[PlaceCodes$Trial == i ,]$LL)}
	if (length(subj[subj$Trial == i & subj$Marker.Name == "LR",]$Code) > 0){subj[subj$Trial == i & subj$Marker.Name == "LR",]$Code = as.character(PlaceCodes[PlaceCodes$Trial == i ,]$LR)}
	}

subj$NounFrame = round((subj$N.ONSET - subj$V.ONSET)*(1000/30))
subj$OffsetFrame = round((subj$OFFSET - subj$V.ONSET)*(1000/30))

subj$Hour = read.table(textConnection(as.character(subj$Start)), sep = ":")[,1]
subj$Min = read.table(textConnection(as.character(subj$Start)), sep = ":")[,2]
subj$Sec = read.table(textConnection(as.character(subj$Start)), sep = ":")[,3]
subj$Frame = read.table(textConnection(as.character(subj$Start)), sep = ":")[,4]
subj$FullTimeFrame = subj$Frame+(subj$Sec*(1000/30))+(subj$Min*1798)+(subj$Hour*107892)
subj$TimeFrame = NA
for (i in unique(subj$Trial)){
	
	subj[subj$Trial == i,]$TimeFrame = subj[subj$Trial == i,]$FullTimeFrame - min(subj[subj$Trial == i,]$FullTimeFrame)
	}	



subj -> FullSubj

FullSubj$PT = 0
if (length(FullSubj[FullSubj$Code == "PT",]$PT>0)){FullSubj[FullSubj$Code == "PT",]$PT = 1}

FullSubj$ST = 0
if (length(FullSubj[FullSubj$Code == "ST",]$ST>0)){FullSubj[FullSubj$Code == "ST",]$ST =  1}

FullSubj$D1 = 0
if (length(FullSubj[FullSubj$Code == "D1",]$D1>0)){FullSubj[FullSubj$Code == "D1",]$D1 = 1}

FullSubj$D2 = 0
if (length(FullSubj[FullSubj$Code == "D2",]$D2 > 0)){FullSubj[FullSubj$Code == "D2",]$D2 = 1}


FullSubj$Cond <- "Phon"
FullSubj[grep("SEM",FullSubj$Trial),]$Cond <- "Sem"
FullSubj$Cond <- as.factor(FullSubj$Cond)

FullSubj$Type <- ifelse(length(grep("B",FullSubj$Trial)) > 0, "B","A")
FullSubj$Type <- as.factor(FullSubj$Type)

FullSubj$Marker.Name = as.factor(FullSubj$Marker.Name)
FullSubj$Code = as.factor(FullSubj$Code)
# 
# FullSubj$TimeWindow <- "Predict"
# FullSubj[FullSubj$TimeFrame > FullSubj$NounFrame,]$TimeWindow <- "Recog"
# FullSubj$TimeWindow <- as.factor(FullSubj$TimeWindow)

return(FullSubj)
}




#######



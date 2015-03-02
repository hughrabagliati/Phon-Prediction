library(reshape2)
library(plyr)
library(car)
require(gdata)
require(ggplot2)
require(longitudinalData)
library(doBy)
# Phon Prediction Processing scripts
Timing <- read.csv("./EyeGaze/Timing.csv",header = T)
PlaceCodes <- read.csv("./EyeGaze/PlaceCodes.csv",header =  T)
Demog <- read.csv("./EyeGaze/PhPrDemographics.csv", header = T)
PhPr <- ET_Import.NoExpand("./EyeGaze/SubjData/",Timing,PlaceCodes)
Demog$Age <- (Demog$Age - mean(Demog$Age, na.rm = T))/2*sd(Demog$Age, na.rm = T)
Demog$BPVS.raw <- (Demog$BPVS.raw - mean(Demog$BPVS.raw, na.rm = T))/2*sd(Demog$BPVS.raw, na.rm = T)
Demog$BPVS.stan <- (Demog$BPVS.stan - mean(Demog$BPVS.stan, na.rm = T))/2*sd(Demog$BPVS.stan, na.rm = T)


ddply(PhPr, .(Name.,Trial,Cond,Type,V.ONSET,N.ONSET,OFFSET,NounFrame,OffsetFrame), summarize, TimeFrame = c(0:max(TimeFrame))) -> PhPr.Expand
PhPr.Expand <- merge(PhPr,PhPr.Expand, by = c("Name.","Trial","Cond","Type","V.ONSET","N.ONSET","OFFSET","NounFrame","OffsetFrame","TimeFrame"), all= TRUE)
PhPr.Expand$PT <- t(imputation(matrix(PhPr.Expand$PT, nrow = 1),method = "locf"))
PhPr.Expand$ST <- t(imputation(matrix(PhPr.Expand$ST, nrow = 1),method = "locf"))
PhPr.Expand$D1 <- t(imputation(matrix(PhPr.Expand$D1, nrow = 1),method = "locf"))
PhPr.Expand$D2 <- t(imputation(matrix(PhPr.Expand$D2, nrow = 1),method = "locf"))

PhPr.Expand <- ddply(PhPr.Expand, .(Name.,Trial), transform, TimeFrame = TimeFrame - NounFrame) 

PhPr.Expand <- ddply(PhPr.Expand, .(Name.,Trial), transform, TimeWindow = ifelse(TimeFrame >= 0,"Recog","Predict")) 
PhPr.Expand <- ddply(PhPr.Expand, .(Name.,Trial,TimeWindow), transform, Time = TimeFrame*(1000/30) )

PhPr.Expand <- merge(PhPr.Expand, Demog) 

# Test we have 16 trials for each subject
PhPr.Expand$Test <- 1
summaryBy(Test~Name. + Trial, data = PhPr.Expand) -> a
summaryBy(Test.mean~Name. , data = a, FUN = c(length)) -> a
a[a$Test.mean.length <16,]

save(list = "PhPr.Expand", file = "PhPr_Expand_Child.RDATA")


SumFun <- function(x){sum(x)*(1000/30)}

print("By Subject means, 200ms before and after noun, looking within a trial")
PhPr.Sum <- summaryBy(PT+ST+D1+D2 ~ Name. + Trial + Cond + Age + BPVS.raw + BPVS.stan, data = PhPr.Expand[PhPr.Expand$Time >= -200 & PhPr.Expand$Time <= 100 ,],keep.names = T, FUN = c(SumFun))
PhPr.Sum$Count <- 1
summaryBy(Count ~ Name., data = PhPr.Sum, FUN = c(length)) -> a
# How many missing trials
a[a$Count.length <16,]

PhPr.BySubj <- summaryBy(PT+ST+D1+D2 ~ Name. + Cond + Age + BPVS.raw + BPVS.stan, data = PhPr.Sum, keep.names = T) 
PhPr.BySubj$SRatio <- PhPr.BySubj$ST/(PhPr.BySubj$ST + PhPr.BySubj$D1)
PhPr.BySubj$PRatio <- PhPr.BySubj$PT/(PhPr.BySubj$PT + PhPr.BySubj$D2)
summaryBy(PT+ST+D1+D2 ~ Cond, data = PhPr.Sum, keep.names = T)

PhPr.Sum1 <- PhPr.Sum

PhPr.Sum1$PT <- ifelse(PhPr.Sum1$PT > 0 ,1, 0)
PhPr.Sum1$ST <- ifelse(PhPr.Sum1$ST > 0 ,1, 0)
PhPr.Sum1$D1 <- ifelse(PhPr.Sum1$D1 > 0 ,1, 0)
PhPr.Sum1$D2 <- ifelse(PhPr.Sum1$D2 > 0 ,1, 0)
PhPr.BySubj1 <- summaryBy(PT+ST+D1+D2 ~ Name. + Cond + Age + BPVS.raw + BPVS.stan, data = PhPr.Sum1, keep.names = T) 

PhPr.BySubj1$SRatio <- PhPr.BySubj1$ST/(PhPr.BySubj1$ST + PhPr.BySubj1$D1)
PhPr.BySubj1$PRatio <- PhPr.BySubj1$PT/(PhPr.BySubj1$PT + PhPr.BySubj1$D2)

summaryBy(PT+ST+D1+D2 +SRatio  + PRatio~ Cond, data = PhPr.BySubj1, keep.names = T, na.rm = T)

summary(lm(SRatio~1+Cond, data = PhPr.BySubj))
summary(lm(SRatio~1+Cond, data = PhPr.BySubj1))
summary(lm(PRatio~1+Cond, data = PhPr.BySubj))
summary(lm(PRatio~1+Cond, data = PhPr.BySubj1))

summary(lm(SRatio~1+BPVS.raw, data = subset(PhPr.BySubj, Cond == "Sem")))
summary(lm(SRatio~1+ BPVS.raw, data = subset(PhPr.BySubj1, Cond == "Sem")))
summary(lm(PRatio~1+ BPVS.raw, data = subset(PhPr.BySubj, Cond == "Phon")))
summary(lm(PRatio~1+ BPVS.raw, data = subset(PhPr.BySubj1, Cond == "Phon")))

se <- function(x){
	x <- sd(x)/sqrt(36)
	return(x)
	}

PhPr.Graph <- summaryBy(PT+ST+D1+D2~Time+Cond+Name.+Trial, data = PhPr.Expand[PhPr.Expand$Time <= 1000 & PhPr.Expand$Time >= -2500  ,], FUN = c(mean),keep.names = T)
PhPr.Graph <- summaryBy(PT+ST+D1+D2~Time+Cond+Name., data = PhPr.Graph, FUN = c(mean),keep.names = T)
PhPr.Graph <- summaryBy(PT+ST+D1+D2~Time+Cond, data = PhPr.Graph, FUN = c(mean,se))


PhPr.Graph2 <- melt(PhPr.Graph,
        # ID variables - all the variables to keep but not split apart on
    id.vars=c("Time","Cond"),
        # The source columns
    measure.vars=c("PT.mean","ST.mean", "D1.mean", "D2.mean" ),
        # Name of the destination column that will identify the original
        # column that the measurement came from
    value.name="Prop",
    variable.name ="Quadrant"
    )

#QUD.Graph2$Quadrant <- revalue(QUD.Graph2$Quadrant, c("Inst.mean"="Target Instrument", "TA.mean"="Target Animal", "DA.mean" = "Distractor Animal","DI.mean" = "Distractor Instrument"))    
#QUD.Graph2$Quadrant <- ordered(QUD.Graph2$Quadrant, levels = c("Target Animal", "Distractor Animal", "Target Instrument", "Distractor Instrument"))
ggplot(PhPr.Graph2,aes(Time,Prop,linetype = Quadrant)) + facet_wrap(~Cond, nrow = 2) + stat_summary(fun.y = mean, geom = "line", size = 1) + theme(legend.title=element_blank(),legend.position="bottom")+ theme(legend.title=element_blank(),legend.position="bottom")+scale_linetype_manual(values=c(1,2,3,4))+labs(x = "Time (ms)",y = "Proportion of Looks")

# 
PhPr.Expand$Target = ifelse(rowMeans(PhPr.Expand[, c("PT","ST","D1","D2")])>0,1,0)
summaryBy(Target~Name. + Trial, data = PhPr.Expand) -> k
k[k$Target.mean < 0.25,] -> k
for (i in unique(k$Name.)){
  print(i)
  for (j in unique(k[k$Name. == i,]$Trial)){
    print(j)
    PhPr.Expand <- PhPr.Expand[!(PhPr.Expand$Name. == i & PhPr.Expand$Trial == j),] 
    summary(PhPr.Expand)
  }
}
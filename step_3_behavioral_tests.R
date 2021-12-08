# Load some packages that we will need

library(rstatix)
library(ggplot2)
library(ggpubr)
library(wesanderson)
library(RColorBrewer)
library(R.matlab)
library(gridExtra)
install.packages("xlsx", dep = T)
library("xlsx")
library(openxlsx)
library(ggplot2)
library(ggpubr)
library(rstatix)
install.packages("vcd")
library(vcd)

#Chi square Test
all_data=read.xlsx("C:/statistics/Behavioral_data_questionnaires.xlsx", sheet=4, startRow = 1)
all_data$ISC_EF=factor(all_data$ISC_EF)
all_data$ISC_SR=factor(all_data$ISC_SR)
all_data$ISC_THGP=factor(all_data$ISC_THGP)
all_data$ISC_BYD=factor(all_data$ISC_BYD)
all_data$Group=factor(all_data$Group,levels=c(1,2),labels=c('trained group','control group'))
all_data$General=factor(all_data$General,levels=c(-2,-1,0,1,2),labels=c('media lit.','rather media lit.','hesitant','rather not media lit.','not media lit.'))

all_data$General=factor(all_data$General)
print(str(all_data))

factor.data <- data.frame(all_data$Group, all_data$General)
factor.data = table(all_data$Group, all_data$General) 
print(factor.data)
print(chisq.test(factor.data))
print(chisq.test(all_data$Group, all_data$General))

prop.table(table(all_data$General))
table(all_data$General, all_data$Group)
prop.table(table(all_data$General, all_data$Group))*100
prop.table(table(all_data$General, all_data$Group), margin=2)*100

fisher.test(all_data$General, all_data$Group)

library(DescTools)
GTest(all_data$General, all_data$Group)

CramerV(all_data$General, all_data$Group)



all_data$General=factor(all_data$General,levels=c(-2,-1,0,1,2),labels=c('media literate','rather media literate','hesitant','rather not media literate','not media literate'))
mosaic(Group ~ General, data = all_data)
ang_labels <-  c(0, 0, 0, 0, 0) #angles
pos_labels = rep("right", 5) #position of names
args = list(set_varnames = c(Group = "", General = "")) #deleating the names of the axes
mosaic(Group ~ General, data = all_data, rot_labels = ang_labels, just_labels = pos_labels,
       labeling_args = args)

#Chi square of the group & interest in media

all_data=read.xlsx("C:/statistics/Behavioral_data_questionnaires.xlsx", sheet=4, startRow = 1)

all_data$Group=factor(all_data$Group,levels=c(1,2),labels=c('trained group','control group'))
all_data$General=factor(all_data$General,levels=c(-2,-1,0,1,2),labels=c('m.l.','rather m.l.','hesitant','rather not m.l.','not m.l.'))
all_data$TV.time=factor(all_data$TV.time)
all_data$Youtube.time=factor(all_data$Youtube.time)
all_data$Social.media.time=factor(all_data$Social.media.time)
all_data$Movie.time=factor(all_data$Movie.time)
all_data$seen1=factor(all_data$seen1)
all_data$seen2=factor(all_data$seen2)
all_data$seen3=factor(all_data$seen3)
all_data$seen4=factor(all_data$seen4)
all_data$Major=factor(all_data$Major)
print(str(all_data))

factor_major <- data.frame(all_data$Group, all_data$Major)
factor_major = table(all_data$Group, all_data$Major) 
print(factor_major)
print(chisq.test(factor_major))

fisher.test(all_data$Major, all_data$Group)
GTest(all_data$Major, all_data$Group)
table(all_data$Major, all_data$Group) 
chisq.test(all_data$Major, all_data$Group)

factor_major2 <- data.frame(all_data$Major, all_data$General)
factor_major2 = table(all_data$Major, all_data$General) 
print(factor_major2)
print(chisq.test(factor_major2))

factor_TV <- data.frame(all_data$Group, all_data$TV.time)
factor_TV = table(all_data$Group, all_data$TV.time) 
print(factor_TV)
print(chisq.test(factor_TV))

fisher.test(all_data$TV.time, all_data$Group)
GTest(all_data$TV.time, all_data$Group)

factor_Youtube <- data.frame(all_data$Group, all_data$Youtube.time)
factor_Youtube = table(all_data$Group, all_data$Youtube.time) 
print(factor_Youtube)
print(chisq.test(factor_Youtube))

fisher.test(all_data$Youtube.time, all_data$Group)
GTest(all_data$Youtube.time, all_data$Group)


factor_SM <- data.frame(all_data$Group, all_data$Social.media.time)
factor_SM = table(all_data$Group, all_data$Social.media.time) 
print(factor_SM)
print(chisq.test(factor_SM))

fisher.test(all_data$Social.media.time, all_data$Group)
GTest(all_data$Social.media.time, all_data$Group)


factor_movie <- data.frame(all_data$Group, all_data$Movie.time)
factor_movie = table(all_data$Group, all_data$Movie.time) 
print(factor_movie)
print(chisq.test(factor_movie))

fisher.test(all_data$Movie.time, all_data$Group)
GTest(all_data$Movie.time, all_data$Group)

factor_seen1 <- data.frame(all_data$Group, all_data$seen1)
factor_seen1 = table(all_data$Group, all_data$seen1) 
print(factor_seen1)
print(chisq.test(factor_seen1))

fisher.test(all_data$seen1, all_data$Group)
GTest(all_data$seen1, all_data$Group)

factor_seen2 <- data.frame(all_data$Group, all_data$seen2)
factor_seen2 = table(all_data$Group, all_data$seen2) 
print(factor_seen2)
print(chisq.test(factor_seen2))

fisher.test(all_data$seen2, all_data$Group)
GTest(all_data$seen2, all_data$Group)


factor_seen3 <- data.frame(all_data$Group, all_data$seen3)
factor_seen3 = table(all_data$Group, all_data$seen3) 
print(factor_seen3)
print(chisq.test(factor_seen3))

fisher.test(all_data$seen3, all_data$Group)
GTest(all_data$seen3, all_data$Group)


factor_seen4 <- data.frame(all_data$Group, all_data$seen4)
factor_seen4 = table(all_data$Group, all_data$seen4) 
print(factor_seen4)
print(chisq.test(factor_seen4))

fisher.test(all_data$seen4, all_data$Group)
GTest(all_data$seen4, all_data$Group)


#ISC data ~ division into groups

factor.data <- data.frame(all_data$ISC_EF, all_data$General)
factor.data = table(all_data$ISC_EF, all_data$General) 
print(factor.data)
print(chisq.test(factor.data))

factor.data <- data.frame(all_data$ISC_SR, all_data$General)
factor.data = table(all_data$ISC_SR, all_data$General) 
print(factor.data)
print(chisq.test(factor.data))

factor.data <- data.frame(all_data$ISC_THGP, all_data$General)
factor.data = table(all_data$ISC_THGP, all_data$General) 
print(factor.data)
print(chisq.test(factor.data))

factor.data <- data.frame(all_data$ISC_BYD, all_data$General)
factor.data = table(all_data$ISC_BYD, all_data$General) 
print(factor.data)
print(chisq.test(factor.data))



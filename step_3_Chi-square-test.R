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


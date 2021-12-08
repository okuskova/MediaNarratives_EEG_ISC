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

#Interest ~ Motive ~ Time 

all_data=read.xlsx("C:/statistics/Behavioral_data_questionnaires.xlsx", sheet=4, startRow = 1)
all_data$ISC_EF=as.numeric(c(all_data$ISC_EF))
all_data$ISC_SR=as.numeric(c(all_data$ISC_SR))
all_data$ISC_THGP=as.numeric(c(all_data$ISC_THGP))
all_data$ISC_BYD=as.numeric(c(all_data$ISC_BYD))
all_data$Group=factor(all_data$Group,levels=c(1,2),labels=c('trained group','control group'))

# Shapiro-Wilk normality test for test score
shapiro.test(all_data$Interest_1) 
shapiro.test(all_data$Interest_2) 
shapiro.test(all_data$Interest_3) 
shapiro.test(all_data$Interest_4) 
shapiro.test(all_data$Motiv_1) 
shapiro.test(all_data$Motiv_2)
shapiro.test(all_data$Motiv_3)
shapiro.test(all_data$Motiv_4)
# Shapiro-Wilk normality test for ISC
shapiro.test(all_data$ISC_THGP) 
# Shapiro-Wilk normality test for NfC
shapiro.test(all_data$Need.for.cognition) 
# Shapiro-Wilk normality test for Crit.
shapiro.test(all_data$Critical.analysis.of.media) 
# Shapiro-Wilk normality test for Conf
shapiro.test(all_data$Conformity) 



# ISC_Interest
ggtest1=ggscatter(all_data, x = "Interest_1", y = "ISC_EF",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_EF ~ Group*Interest_1)

ggtest1

ggtest2=ggscatter(all_data, x = "Interest_2", y = "ISC_SR",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_SR ~ Group*Interest_2)

ggtest2


ggtest3=ggscatter(all_data, x = "Interest_3", y = "ISC_THGP",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_THGP ~ Group*Interest_3)

ggtest3


ggtest4=ggscatter(all_data, x = "Interest_4", y = "ISC_BYD",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_BYD ~ Group*Interest_4)

ggtest4

grid.arrange(ggtest1,ggtest2,ggtest3,ggtest4,ncol=2)



# ISC_Motive
ggtest1=ggscatter(all_data, x = "Motiv_1", y = "ISC_EF",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_EF ~ Group*Motiv_1)

ggtest1



ggtest2=ggscatter(all_data, x = "Motiv_2", y = "ISC_SR",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_SR ~ Group*Motiv_2)

ggtest2


ggtest3=ggscatter(all_data, x = "Motiv_3", y = "ISC_THGP",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_THGP ~ Group*Motiv_3)

ggtest3



ggtest4=ggscatter(all_data, x = "Motiv_4", y = "ISC_BYD",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_BYD ~ Group*Motiv_4)

ggtest4

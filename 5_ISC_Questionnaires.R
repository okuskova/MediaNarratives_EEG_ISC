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


all_data=read.xlsx("C:/statistics/Behavioral_data_questionnaires.xlsx", sheet=4, startRow = 1)
all_data$ISC_EF=as.numeric(c(all_data$ISC_EF))
all_data$ISC_SR=as.numeric(c(all_data$ISC_SR))
all_data$ISC_THGP=as.numeric(c(all_data$ISC_THGP))
all_data$ISC_BYD=as.numeric(c(all_data$ISC_BYD))
all_data$Group=factor(all_data$Group,levels=c(1,2),labels=c('trained group','control'))

# Shapiro-Wilk normality test for test score
# Shapiro-Wilk normality test for ISC
shapiro.test(all_data$ISC_THGP) 
# Shapiro-Wilk normality test for NfC
shapiro.test(all_data$Need.for.cognition) 
# Shapiro-Wilk normality test for Crit.
shapiro.test(all_data$Critical.analysis.of.media) 
# Shapiro-Wilk normality test for Conf
shapiro.test(all_data$Conformity) 



# ISC_EF
ggtest1=ggscatter(all_data, x = "Media.literacy.test", y = "ISC_EF",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_EF ~ Group*Media.literacy.test)

ggtest1

ggNfC1=ggscatter(all_data, x = "Need.for.cognition", y = "ISC_EF",
                 color = "Group", shape='Group',
                 add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 90, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_EF ~ Group*Need.for.cognition)

ggNfC1

ggCr1=ggscatter(all_data, x = "Critical.analysis.of.media", y = "ISC_EF",
                color = "Group", shape='Group',
                add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 90, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_EF ~ Group*Critical.analysis.of.media)

ggCr1

ggConf1=ggscatter(all_data, x = "Conformity", y = "ISC_EF",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_EF ~ Group*Conformity)

ggConf1

grid.arrange(ggtest1,ggCr1,ggNfC1,ggConf1,ncol=2)


# ISC_SR
ggtest2=ggscatter(all_data, x = "Media.literacy.test", y = "ISC_SR",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_SR ~ Group*Media.literacy.test)

ggtest2

ggNfC2=ggscatter(all_data, x = "Need.for.cognition", y = "ISC_SR",
                 color = "Group", shape='Group',
                 add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 90, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_SR ~ Group*Need.for.cognition)

ggNfC2

ggCr2=ggscatter(all_data, x = "Critical.analysis.of.media", y = "ISC_SR",
                color = "Group", shape='Group',
                add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 86, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_SR ~ Group*Critical.analysis.of.media)

ggCr2

ggConf2=ggscatter(all_data, x = "Conformity", y = "ISC_SR",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_SR ~ Group*Conformity)

ggConf2

grid.arrange(ggtest2,ggCr2,ggNfC2,ggConf2,ncol=2)

# ISC_THGP
ggtest3=ggscatter(all_data, x = "Media.literacy.test", y = "ISC_THGP",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 1, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_THGP ~ Group*Media.literacy.test)

ggtest3

ggNfC3=ggscatter(all_data, x = "Need.for.cognition", y = "ISC_THGP",
                 color = "Group", shape='Group',
                 add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 86, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_THGP ~ Group*Need.for.cognition)

ggNfC3

ggCr3=ggscatter(all_data, x = "Critical.analysis.of.media", y = "ISC_THGP",
                color = "Group", shape='Group',
                add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 86, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_THGP ~ Group*Critical.analysis.of.media)

ggCr3

ggConf3=ggscatter(all_data, x = "Conformity", y = "ISC_THGP",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = -5, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_THGP ~ Group*Conformity)

ggConf3

grid.arrange(ggtest3,ggCr3,ggNfC3,ggConf3,ncol=2)



# ISC_BYD
ggtest4=ggscatter(all_data, x = "Media.literacy.test", y = "ISC_BYD",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 0.5, method='spearman', cor.coef.name='rho')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_BYD ~ Group*Media.literacy.test)

ggtest4

ggNfC4=ggscatter(all_data, x = "Need.for.cognition", y = "ISC_BYD",
                 color = "Group", shape='Group',
                 add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 86, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_BYD ~ Group*Need.for.cognition)

ggNfC4

ggCr4=ggscatter(all_data, x = "Critical.analysis.of.media", y = "ISC_BYD",
                color = "Group", shape='Group',
                add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = 86, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_BYD ~ Group*Critical.analysis.of.media)

ggCr4

ggConf4=ggscatter(all_data, x = "Conformity", y = "ISC_BYD",
                  color = "Group", shape='Group',
                  add = "reg.line", conf.int = TRUE)+
  stat_cor(aes(color = Group), label.x = -5, method='pearson', cor.coef.name='R')+
  scale_color_manual(values=c('indianred1','royalblue1'))
all_data %>% anova_test(ISC_BYD ~ Group*Conformity)

ggConf4

grid.arrange(ggtest4,ggCr4,ggNfC4,ggConf4,ncol=2)






grid.arrange(ggtest1,ggtest2,ggtest3,ggtest4,ncol=2)
grid.arrange(ggCr1,ggCr2,ggCr3,ggCr4,ncol=2)
grid.arrange(ggNfC1,ggNfC2,ggNfC3,ggNfC4,ncol=2)
grid.arrange(ggConf1,ggConf2,ggConf3,ggConf4,ncol=2)


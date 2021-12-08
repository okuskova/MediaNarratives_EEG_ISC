# Load some packages that we will need

library(R.matlab)
library(rstatix)
library(ggplot2)
library(ggpubr)
library(wesanderson)
library(RColorBrewer)
library(R.matlab)
library(gridExtra)

## P-adjustment 

ml=readMat('ml_f3_pvals.mat')$pvals   # on the example of one video - f3

# adjust the p-values separately for each component

p1=ml[,1]
length(p1[p1<0.05])
p1_adj=p.adjust(p1,method='BH')
length(p1_adj[p1_adj<0.05])
# repeat for 2nd and 3rd component

# adjust the p-values for all components that we use together (recommended)
p=ml[,1:3]
length(p[which(p[,1]<0.05),1])
length(p[which(p[,2]<0.05),2])
length(p[which(p[,3]<0.05),3])

p_adj=matrix(p.adjust(p,method='BH'),nrow(p),3)
length(p_adj[which(p_adj[,1]<0.05),1])
length(p_adj[which(p_adj[,2]<0.05),2])
length(p_adj[which(p_adj[,3]<0.05),3])


## Proportion test. We need:
# x: the number of significant time windows
# n: the total number of time windows

# on the example of one video

df=data.frame('percentage'=100*c(0.7902,0.4276,0.6295,0.7915,0.3825,0.6135),
              'group'=c(rep(' trained',3),rep('control',3)),
              'component'=c(rep(c('C1','C2','C3'),2)))

Nwindows=753

# Test for 1st component
test1=prop.test(n=c(Nwindows,Nwindows),x=c(df[1,1]/100*Nwindows, df[4,1]/100*Nwindows))
p1=test1$p.value
# Test for 2nd component
test2=prop.test(n=c(Nwindows,Nwindows),x=c(df[2,1]/100*Nwindows, df[5,1]/100*Nwindows))
p2=test2$p.value
# Test for 3rd component
test3=prop.test(n=c(Nwindows,Nwindows),x=c(df[3,1]/100*Nwindows, df[6,1]/100*Nwindows))
p3=test3$p.value

res=data.frame('group1'=c('m','m','m'),
               'group2'=c('n','n','n'),
               'y.position'=c(100,100,100),
               'component'=c('C1','C2','C3'),
               'p'=c(round(p1,3),round(p2,3),round(p3,3)),
               'xmin'=c(0.8,1.8,2.8),
               'xmax'=c(1.2,2.2,3.2),
               'p.signif'=c(ifelse(p1>0.05,'ns',round(p1,3)),
                            ifelse(p2>0.05,'ns',round(p2,3)),
                            ifelse(p3>0.05,'ns',round(p3,3))))

g3=ggbarplot(df, x = "component", y = "percentage", fill='group',
             position=position_dodge(width=0.8))+
  scale_fill_manual(values=c('indianred1','royalblue1'))+
  ylim(c(0,110))+
  theme(text = element_text(size=10),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=5))+ 
  stat_pvalue_manual(res, tip.length = 0, bracket.size = 1.2, size=4,
                     hide.ns=F, label= 'p = {p}', vjust=-0.5)

p1
p2
p3

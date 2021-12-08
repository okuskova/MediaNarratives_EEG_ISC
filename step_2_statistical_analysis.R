# Load some packages that we will need
library(rstatix)
library(ggplot2)
library(ggpubr)
library(wesanderson)
library(RColorBrewer)
library(R.matlab)
library(gridExtra)


# Media literate
ml_fake1=readMat('ml_fake1_ISC_persubject_sum.mat')$ISC.persubject
ml_fake1=c(ml_fake1)
ml_fake2=readMat('ml_fake2_ISC_persubject_sum.mat')$ISC.persubject
ml_fake2=c(ml_fake2)
ml_fake3=readMat('ml_fake3_ISC_persubject_sum.mat')$ISC.persubject
ml_fake3=c(ml_fake3)
ml_control=readMat('ml_control_ISC_persubject_sum.mat')$ISC.persubject
ml_control=c(ml_control)

# Naive
n_fake1=readMat('n_fake1_ISC_persubject_sum.mat')$ISC.persubject
n_fake1=c(n_fake1)
n_fake2=readMat('n_fake2_ISC_persubject_sum.mat')$ISC.persubject
n_fake2=c(n_fake2)
n_fake3=readMat('n_fake3_ISC_persubject_sum.mat')$ISC.persubject
n_fake3=c(n_fake3)
n_control=readMat('n_control_ISC_persubject_sum.mat')$ISC.persubject
n_control=c(n_control)

# Store Isc as sum of the 3 stringest components
df_fake1=data.frame('ISC'=c(ml_fake1,n_fake1),
                    'group'=c(rep(' trained group', length(ml_fake1)),
                              rep('control group', length(n_fake1))))

df_fake2=data.frame('ISC'=c(ml_fake2,n_fake2),
                    'group'=c(rep(' trained group', length(ml_fake2)),
                              rep('control group', length(n_fake2))))

df_fake3=data.frame('ISC'=c(ml_fake3,n_fake3),
                    'group'=c(rep(' trained group', length(ml_fake3)),
                              rep('control group', length(n_fake3))))

df_control=data.frame('ISC'=c(ml_control,n_control),
                      'group'=c(rep(' trained group', length(ml_control)),
                                rep('control group', length(n_control))))



# Compare the groups in Fake 1

# Statistics
df_fake1 %>%
  group_by(group) %>%
  identify_outliers(ISC)

df_fake1 %>%
  group_by(group) %>%
  shapiro_test(ISC)

stat.test <- df_fake1 %>% 
  t_test(ISC ~ group) %>%
  add_significance()
stat.test

df_fake1 %>%  cohens_d(ISC ~ group)

# Report
stat.test = stat.test %>% add_xy_position(x = "group")
stat.test$y.position=0.35
g1=ggbarplot(df_fake1, x = "group", y = "ISC", error.plot = 'upper_errorbar',
             add = c("mean_se"), add.params = list(size=1), fill='group')+
  geom_jitter(size=1, width=0.1)+
  scale_fill_manual(values=c('indianred1','royalblue1'))+
  ylim(c(0,0.4))+
  theme(text = element_text(size=10),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=5),
        legend.position = 100)+ 
  stat_pvalue_manual(stat.test, tip.length = 0, bracket.size = 1.2, size=7, hide.ns=T) +
  labs(subtitle = get_test_label(stat.test, detailed = T))
g1


# Compare the groups in Fake 2

# Statistics
df_fake2 %>%
  group_by(group) %>%
  identify_outliers(ISC)

df_fake2 %>%
  group_by(group) %>%
  shapiro_test(ISC)

stat.test <- df_fake2 %>% 
  t_test(ISC ~ group) %>%
  add_significance()
stat.test

stat.test <- df_fake2 %>% 
  wilcox_test(ISC ~ group) %>%
  add_significance()
stat.test

df_fake2 %>%  cohens_d(ISC ~ group)

# Report
stat.test = stat.test %>% add_xy_position(x = "group")
stat.test$y.position=0.35
g2=ggbarplot(df_fake2, x = "group", y = "ISC", error.plot = 'upper_errorbar',
             add = c("mean_se"), add.params = list(size=1), fill='group')+
  geom_jitter(size=1, width=0.1)+
  scale_fill_manual(values=c('indianred1','royalblue1'))+
  ylim(c(0,0.4))+
  theme(text = element_text(size=10),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=5),
        legend.position  = 100)+ 
  stat_pvalue_manual(stat.test, tip.length = 0, bracket.size = 1.2, size=7, hide.ns=T) +
  labs(subtitle = get_test_label(stat.test, detailed = T), fill='')
g2


# Compare the groups in Fake 3

# Statistics
df_fake3 %>%
  group_by(group) %>%
  identify_outliers(ISC)

df_fake3 %>%
  group_by(group) %>%
  shapiro_test(ISC)

stat.test <- df_fake3 %>% 
  t_test(ISC ~ group) %>%
  add_significance()
stat.test

stat.test <- df_fake3 %>% 
  wilcox_test(ISC ~ group) %>%
  add_significance()
stat.test

stat.test <- df_fake3 %>% 
  t_test(ISC ~ group) %>%
  add_significance()
stat.test

df_fake3 %>%  cohens_d(ISC ~ group)

# Report
stat.test = stat.test %>% add_xy_position(x = "group")
stat.test$y.position=0.35
g3=ggbarplot(df_fake3, x = "group", y = "ISC", error.plot = 'upper_errorbar',
             add = c("mean_se"), add.params = list(size=1), fill='group')+
  geom_jitter(size=1, width=0.1)+
  scale_fill_manual(values=c('indianred1','royalblue1'))+
  ylim(c(0,0.4))+
  theme(text = element_text(size=10),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=5),
        legend.position  = 100)+ 
  stat_pvalue_manual(stat.test, tip.length = 0, bracket.size = 1.2, size=7, hide.ns=T) +
  labs(subtitle = get_test_label(stat.test, detailed = T), fill='')
g3


# Compare the groups in control

# Statistics
df_control %>%
  group_by(group) %>%
  identify_outliers(ISC)

df_control %>%
  group_by(group) %>%
  shapiro_test(ISC)

stat.test <- df_control %>% 
  t_test(ISC ~ group) %>%
  add_significance()
stat.test

df_control %>%  cohens_d(ISC ~ group)

# Report
stat.test = stat.test %>% add_xy_position(x = "group")
stat.test$y.position=0.35
g4=ggbarplot(df_control, x = "group", y = "ISC", error.plot = 'upper_errorbar',
             add = c("mean_se"), add.params = list(size=1), fill='group')+
  geom_jitter(size=1, width=0.1)+
  scale_fill_manual(values=c('indianred1','royalblue1'))+
  ylim(c(0,0.4))+
  theme(text = element_text(size=10),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=5),
        legend.position  = 100)+ 
  stat_pvalue_manual(stat.test, tip.length = 0, bracket.size = 1.2, size=7, hide.ns=T) +
  labs(subtitle = get_test_label(stat.test, detailed = T), fill='')
g4

grid.arrange(g1,g2,g3,g4,ncol=2)







########## For a Mixed analysis with both within-subjects factors and between-subjects factors (for both) ##########

df=data.frame('ISC'=c(ml_fake1,ml_fake2, ml_fake3, ml_control,
                      n_fake1,n_fake2, n_fake3, n_control),
              'group'=c(rep(' trained group', 4*length(ml_fake1)),
                        rep('control group', 4*length(n_fake1))),
              'condition'=c(rep(c(rep('EF',length(ml_fake1)),
                                  rep('SR',length(ml_fake2)),
                                  rep('THGP',length(ml_fake3)),
                                  rep('BYD',length(ml_control))),2)),
              'id'=c(rep(1:length(ml_fake1),4),
                     rep((length(ml_fake1)+1):(length(ml_fake1)+length(n_fake1)),4)))
df$condition=factor(df$condition,levels=c('EF','SR','THGP','BYD'),labels=c('EF-video','SR-video','THGP-video','BYD-video'))

# Visualization
ggbarplot(df, x = "condition", y = "ISC", fill='group',  position = position_dodge(0.8),
          error.plot = 'upper_errorbar', add = c("mean_se"), add.params = list(size=1),
          palette = "Set1")+
  scale_fill_manual(values=c('indianred1','royalblue1'))+
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=10))

# Statistics
df %>%
  group_by(group, condition) %>%
  shapiro_test(ISC)

ggqqplot(df, x = "ISC", facet.by = c("group",'condition'))

res.aov <- anova_test(
  data = df, dv = ISC, wid = id,
  between = group, within = condition
)
get_anova_table(res.aov)

pwc <- df %>%
  group_by(condition) %>%
  pairwise_t_test(
    ISC ~ group, paired = F, 
    p.adjust.method = "bonferroni"
  )
pwc

# Report
pwc <- pwc %>% add_xy_position(x = "condition")
ggbarplot(df, x = "condition", y = "ISC", fill='group',  position = position_dodge(0.8),
          error.plot = 'upper_errorbar', add = c("mean_se"), add.params = list(size=1),
          palette = "Set1")+
  scale_fill_manual(values=c('indianred1','royalblue1'))+
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=10))+
  stat_pvalue_manual(pwc, tip.length = 0, bracket.size = 1.2, size=7, hide.ns=T) +
  labs(
    #subtitle = get_test_label(res.aov, detailed = F),
    #caption = get_pwc_label(pwc)
  )


# Another layout

pwc2 <- df %>%
  group_by(group) %>%
  pairwise_t_test(
    ISC ~ condition, paired = T, 
    p.adjust.method = "bonferroni"
  )
pwc2

# Report
pwc2 <- pwc2 %>% add_xy_position(x = "group")
ggbarplot(df, x = "group", y = "ISC", fill='condition',  position = position_dodge(0.8),
          error.plot = 'upper_errorbar', add = c("mean_se"), add.params = list(size=1),
          palette = "jco")+
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=10))+
  stat_pvalue_manual(pwc2, tip.length = 0, bracket.size = 1.2, size=7, hide.ns=T) +
  labs(
    subtitle = get_test_label(res.aov, detailed = F),
    caption = get_pwc_label(pwc)
  )



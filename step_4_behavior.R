## Changes in attitude

# Load some packages that we will need

install.packages("nparLD")
library("nparLD")

### Load all the data

ml_all=read.xlsx("C:/before_after_data.xlsx", sheet=1, startRow = 1)

# Media literate - fake1 video
ml_f1=ml_all[3:17,1:7]
# Media literate - fake1 video - indirect/direct/control questions
ml_f1_in=ml_f1[,1:3]
ml_f1_d=ml_f1[,c(1,4,5)]
ml_f1_c=ml_f1[,c(1,6,7)]

# Media literate - fake2 video
ml_f2=ml_all[3:17,c(1,8:13)]
# Media literate - fake2 video - indirect/direct/control questions
ml_f2_in=ml_f2[,1:3]
ml_f2_d=ml_f2[,c(1,4,5)]
ml_f2_c=ml_f2[,c(1,6,7)]

# Media literate - fake3 video
ml_f3=ml_all[3:17,c(1,14:19)]
# Media literate - fake3 video - indirect/direct/control questions
ml_f3_in=ml_f3[,1:3]
ml_f3_d=ml_f3[,c(1,4,5)]
ml_f3_c=ml_f3[,c(1,6,7)]

# Media literate - control video
ml_c=ml_all[3:17,c(1,20:25)]
# Media literate - control video - indirect/direct/control questions
ml_c_in=ml_c[,1:3]
ml_c_d=ml_c[,c(1,4,5)]
ml_c_c=ml_c[,c(1,6,7)]

n_all=read.xlsx("C:/before_after_data.xlsx",sheet=2)

# Non-media literate - fake1 video
n_f1=n_all[3:17,1:7]
# Non-media literate - fake1 video - indirect/direct/control questions
n_f1_in=n_f1[,1:3]
n_f1_d=n_f1[,c(1,4,5)]
n_f1_c=n_f1[,c(1,6,7)]

# Non-media literate - fake2 video
n_f2=n_all[3:17,c(1,8:13)]
# Non-media literate - fake2 video - indirect/direct/control questions
n_f2_in=n_f2[,1:3]
n_f2_d=n_f2[,c(1,4,5)]
n_f2_c=n_f2[,c(1,6,7)]

# Non-media literate - fake3 video
n_f3=n_all[3:17,c(1,14:19)]
# Non-media literate - fake3 video - indirect/direct/control questions
n_f3_in=n_f3[,1:3]
n_f3_d=n_f3[,c(1,4,5)]
n_f3_c=n_f3[,c(1,6,7)]

# Non-media literate - control video
n_c=n_all[3:17,c(1,20:25)]
# Non-media literate - control video - indirect/direct/control questions
n_c_in=n_c[,1:3]
n_c_d=n_c[,c(1,4,5)]
n_c_c=n_c[,c(1,6,7)]


#### Summarize all data in a table

Nml=length(ml_f1_in[,2])
Nn=length(n_f1_in[,2])

df=data.frame('id'=c(ml_f1_in[,1],ml_f1_in[,1],ml_f1_d[,1],ml_f1_d[,1],ml_f1_c[,1],ml_f1_c[,1],
                     ml_f2_in[,1],ml_f2_in[,1],ml_f2_d[,1],ml_f2_d[,1],ml_f2_c[,1],ml_f2_c[,1],
                     ml_f3_in[,1],ml_f3_in[,1],ml_f3_d[,1],ml_f3_d[,1],ml_f3_c[,1],ml_f3_c[,1],
                     ml_c_in[,1],ml_c_in[,1],ml_c_d[,1],ml_c_d[,1],ml_c_c[,1],ml_c_c[,1],
                     n_f1_in[,1],n_f1_in[,1],n_f1_d[,1],n_f1_d[,1],n_f1_c[,1],n_f1_c[,1],
                     n_f2_in[,1],n_f2_in[,1],n_f2_d[,1],n_f2_d[,1],n_f2_c[,1],n_f2_c[,1],
                     n_f3_in[,1],n_f3_in[,1],n_f3_d[,1],n_f3_d[,1],n_f3_c[,1],n_f3_c[,1],
                     n_c_in[,1],n_c_in[,1],n_c_d[,1],n_c_d[,1],n_c_c[,1],n_c_c[,1]),
              
              'score'=c(ml_f1_in[,2],ml_f1_in[,3],ml_f1_d[,2],ml_f1_d[,3],ml_f1_c[,2],ml_f1_c[,3],
                        ml_f2_in[,2],ml_f2_in[,3],ml_f2_d[,2],ml_f2_d[,3],ml_f2_c[,2],ml_f2_c[,3],
                        ml_f3_in[,2],ml_f3_in[,3],ml_f3_d[,2],ml_f3_d[,3],ml_f3_c[,2],ml_f3_c[,3],
                        ml_c_in[,2],ml_c_in[,3],ml_c_d[,2],ml_c_d[,3],ml_c_c[,2],ml_c_c[,3],
                        n_f1_in[,2],n_f1_in[,3],n_f1_d[,2],n_f1_d[,3],n_f1_c[,2],n_f1_c[,3],
                        n_f2_in[,2],n_f2_in[,3],n_f2_d[,2],n_f2_d[,3],n_f2_c[,2],n_f2_c[,3],
                        n_f3_in[,2],n_f3_in[,3],n_f3_d[,2],n_f3_d[,3],n_f3_c[,2],n_f3_c[,3],
                        n_c_in[,2],n_c_in[,3],n_c_d[,2],n_c_d[,3],n_c_c[,2],n_c_c[,3]),
              
              'group'=c(rep(' Trained',Nml*24),rep('Control',Nn*24)),
              
              'question'=c(rep(c(rep('indirect question',Nml*2),rep('direct question',Nml*2),rep('control question',Nml*2)),4),
                           rep(c(rep('indirect question',Nn*2),rep('direct question',Nn*2),rep('control question',Nn*2)),4)),
              
              'video'=c(rep('EF',Nml*6),rep('SR',Nml*6),rep('THGP',Nml*6),rep('BYD',Nml*6),
                        rep('EF',Nn*6),rep('SR',Nn*6),rep('THGP',Nn*6),rep('BYD',Nn*6)),
              
              'session'=c(rep(c(rep('before',Nml),rep('after',Nml)),3*4),
                          rep(c(rep('before',Nn),rep('after',Nn)),3*4)))


# If you want to see all observations of a single subject, e.g. M101, type:
df[which(df$id=='M101'),]

# Evaluate the type of the variables
str(df)
df$score=as.numeric(as.character(df$score))
df$question=factor(df$question,levels=c('direct question','indirect question','control question'),labels=c('direct statement','indirect statement','control statement'))
df$video=factor(df$video,levels=c('EF','SR','THGP','BYD'),labels=c('EF-video','SR-video','THGP-video','BYD-video'))
df$session=factor(df$session,levels=c('before','after'))
str(df)
summary(df)

# Let's visualize the data without statistics
# group x video x question x session x score (x id)

ggbarplot(df, x = "group", y = "score", fill='session',  position = position_dodge(0.8),
          error.plot = 'upper_errorbar', add = c("mean_se"), add.params = list(size=1),
          palette = "Set1", facet.by=c('video','question'))+
  scale_fill_manual(values=c('indianred1','royalblue1'))+
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=10))

# Apply Mixed Anova with Group as between subject factor and Session as within subject factor
# When doing this, the data should be grouped in terms of Video and Question

df %>%
  group_by(video, question, group) %>%
  identify_outliers(score)

df %>%
  group_by(video, question, group) %>%
  shapiro_test(score)

# Check whether there is a non-parametric test equivalent to Mixed Anova (keyword: wilcoxon)
res.aov <- df %>%
  group_by(question, video) %>%
  anova_test(dv = score, wid = id,
             between = group, within = session
  )
get_anova_table(res.aov)

# get new pvalues from non parametric test
#res.aov$p[seq(3,nrow(res.aov),3)]=new_p_values

# Pairwise comparisons (before vs. after for each group)
pwc <- df %>%
  group_by(question, video, group) %>%
  pairwise_wilcox_test(score ~ session, paired=T,
                       p.adjust.method = "none")
pwc

pwc=pwc %>% add_xy_position(x='group')
pwc$p.adj[1]=1
pwc$p[1]=1

ggbarplot(df, x = "group", y = "score", fill='session',  position = position_dodge(0.8),
          error.plot = 'upper_errorbar', add = c("mean_se"), add.params = list(size=1),
          palette = "Set1", facet.by=c('video','question'))+
  ylim(c(0,1.2))+
  stat_pvalue_manual(pwc, hide.ns = T, tip.length = 0)+
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=10))

#new attempt
res.aov <- df %>%
  group_by(question, video) %>%
  anova_test(dv = score, wid = id,
             between = group, within = session
  )
get_anova_table(res.aov)


pwc <- df %>%
  group_by(question, video, session) %>%
  pairwise_wilcox_test(score ~ group, paired=F,
                       p.adjust.method = "none")
pwc

pwc=pwc %>% add_xy_position(x='session')
pwc$p.adj[1]=1
pwc$p[1]=1

ggbarplot(df, x = "session", y = "score", fill='group',  position = position_dodge(0.8),
          error.plot = 'upper_errorbar', add = c("mean_se"), add.params = list(size=1),
          palette = "Set1", facet.by=c('video','question'))+
  scale_fill_manual(values=c('indianred1','royalblue1'))+
  ylim(c(0,1.2))+
  stat_pvalue_manual(pwc, hide.ns = T, tip.length = 0)+
  theme(text = element_text(size=20),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=10))


#Non-parametric ANOVA
#1.1
data_df11=df[which(df$question=='direct question' & df$video=='EF'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df11,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)


#1.2
data_df12=df[which(df$question=='indirect question' & df$video=='EF'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df12,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)


#1.3
data_df13=df[which(df$question=='control question' & df$video=='EF'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df13,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)

#2.1
data_df21=df[which(df$question=='direct question' & df$video=='SR'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df21,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)

#2.2
data_df22=df[which(df$question=='indirect question' & df$video=='SR'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df22,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)

#2.3
data_df23=df[which(df$question=='control' & df$video=='SR'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df23,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)

#3.1
data_df31=df[which(df$question=='direct question' & df$video=='THGP'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df31,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)

#3.2
data_df32=df[which(df$question=='indirect' & df$video=='THGP'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df32,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)

#3.3
data_df33=df[which(df$question=='control question' & df$video=='THGP'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df33,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)

#4.1
data_df41=df[which(df$question=='direct question' & df$video=='BYD'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df41,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)

#4.2
data_df42=df[which(df$question=='indirect question' & df$video=='BYD'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df42,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)


#4.3
data_df43=df[which(df$question=='control question' & df$video=='BYD'),]

ex.f1f1np = nparLD(score ~ session * group, data = data_df43,
                   subject = "id", description = FALSE)
summary(ex.f1f1np)


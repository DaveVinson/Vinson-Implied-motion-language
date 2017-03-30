############################################################################
# Analysis: Implied motion in language can influence visual spatial memory #
########################### David W. Vinson ################################
###### Mansuscript published in the Journal of Memory and Cognition ########
############################################################################

############################################################################
#load functions and libraries
############################################################################
library(phia)
library(lsr)
library(LCA)
source('functions.R')
############################################################################
# Data loading experiment 1
# NOTE: Experiments were originally run as 3 sep experiments Reviewers encourages consolidation into 2 experiments
############################################################################
setwd('data/exp1/')
exp1 <- raw2clean(list.files(pattern="[.]txt$"))

setwd('data/exp3a/')
exp3a <- raw2clean(list.files(pattern="[.]txt$"))

results <- rbind(exp1,exp3a)
rm(exp1,exp3a)

############################################################################
#Data loading experiment 2
# NOTE: Experiments were originally run as 3 sep experiments Reviewers encourages consolidation into 2 experiments
############################################################################
rm(results)

setwd('data/exp2/')
exp2 <- raw2clean(list.files(pattern="[.]txt$"))

setwd('data/exp3b/')
exp3b <- raw2clean(list.files(pattern="[.]txt$"))

results <- rbind(exp2,exp3b)
rm(exp2,exp3b)

############################################################################
#convert data to on gravity dimension  
############################################################################
#For exp 1 only
cv = cos(-20*pi/180)
results$ny = -results$y +76.3 #76.3 centers the intercept of the GD vector dimension at zero. 
#new coordinate for actual location 
120/cv

#For exp 2 only
cv = cos(20*pi/180)
results$ny =  results$y +169.22 #169.22 centers the intercept of the GD vector dimension at zero.
#new coordinate for actual location 
410/cv

#run for all experiments
for (i in 1:nrow(results)){
  results$newx  = (results$x)/cv #we call newx our gravity dimension
}

############################################################################
############ Removing data 3 standard deviations away from mean ############ 
############################################################################
############################################################################
results = results[results$rt<90000,] #remove all data that takes more than 25s to complete
upper=mean(results$rt)+(3*(sd(results$rt)))
lower=mean(results$rt)-(3*(sd(results$rt)))
test = results[results$rt<upper,]
results=test[test$rt>lower,]

upper=mean(results$conf)+(3*(sd(results$conf))) 
lower=mean(results$conf)-(3*(sd(results$conf)))
test = results[results$conf<upper,]
results=test[test$conf>lower,] ##### convienent of confidence doesn't result in any dropped data. 

upper=mean(results$newx)+(3*(sd(results$newx)))
lower=mean(results$newx)-(3*(sd(results$newx)))
test = results[results$newx<upper,]
results=test[test$newx>lower,]

results$newx = results$newx-(120/cv)#exp1
results$newx = results$newx-(410/cv)#exp2

#for exp 2 only
results$newx = -results$newx #exp2 only

############################################################################
############ RT analyses (and confidence) ##################################
############################################################################
mean(results$rt/10)
stderr(results$rt/10)
mean(results$conf)
stderr(results$conf)

##### IV : lang/dir DV: rt,conf,x,y #####
aov.out = aov(rt ~ lang*dir, data=results) #trim rt
aov.out = aov(conf ~ lang*dir,data=results) #trim conf
aov.out = aov(newx ~ lang*dir,data=results) #trim x

summary(aov.out)
etaSquared(aov.out, type = 2, anova = FALSE )

TukeyHSD(aov.out) #simple effects tests


mean(results[results$lang=="forward",]$rt/10) #down/back |mu = 410 for exp2
stderr(results[results$lang=="forward",]$rt/10)
mean(results[results$lang=="backward",]$rt/10) #down/back |mu = 410 for exp2
stderr(results[results$lang=="backward",]$rt/10)
mean(results[results$lang=="Null",]$rt/10) #down/back |mu = 410 for exp2
stderr(results[results$lang=="Null",]$rt/10)

mean(results[results$dir=="up",]$conf)
stderr(results[results$dir=="up",]$conf)
stderr(results[results$dir=="down",]$conf)

stderr(results$conf)

############################################################################
############   T-tests #####################################################
############################################################################
# trim appropriately before running analyses/breaking into sep conditions. x and y seperately. 
results$cc = 0
results[results$lang=="forward" & results$dir=="up",]$cc="Forward-Up"
results[results$lang=="forward" & results$dir=="down",]$cc="Forward-Down"
results[results$lang=="backward" & results$dir=="up",]$cc="Backward-Up"
results[results$lang=="backward" & results$dir=="down",]$cc="Backward-Down"
results[results$lang=="Null" & results$dir=="up",]$cc="No Lang-Up"
results[results$lang=="Null" & results$dir=="down",]$cc="No Lang-Down"

t.test(results[results$cc=="Forward-Up",]$newx,mu=0) #down/back |mu = 410 for exp2
stderr(results[results$cc=="Forward-Up",]$newx)
t.test(results[results$cc=="Forward-Down",]$newx,mu=0) #up/back |mu = 410 for exp2
stderr(results[results$cc=="Forward-Down",]$newx) 
t.test(results[results$cc=="Backward-Up",]$newx,mu=0) #up/forward |mu = 410 for exp2
stderr(results[results$cc=="Backward-Up",]$newx)
t.test(results[results$cc=="Backward-Down",]$newx,mu=0) #down/forward |mu = 410 for exp2
stderr(results[results$cc=="Backward-Down",]$newx)
t.test(results[results$cc=="No Lang-Up",]$newx,mu=0) #up/forward |mu = 410 for exp2
stderr(results[results$cc=="No Lang-Up",]$newx)
t.test(results[results$cc=="No Lang-Down",]$newx,mu=0) #down/forward |mu = 410 for exp2
stderr(results[results$cc=="No Lang-Down",]$newx)

############################################################################
############ summary analysis 1 and 3a, 2 and 3b ###########################
############################################################################
library(doBy)
library(ggplot2)
library(scales)

means = summaryBy(newx ~ cc,  data = results, 
          FUN = function(x) { c(m = mean(x), s = stderr(x)) } ) #-mean for exp 2?
means$x = c(2,5,6,1,3,4)
means$cc <- factor(means$cc, levels = means$cc[order(-means$x)])

ggplot(data=means,aes(newx.m,cc))+
  geom_point()+
  geom_errorbarh(aes(xmin=newx.m-newx.s,xmax=newx.m+newx.s,height=.5))+
  geom_vline(xintercept=c(0), linetype="dotted")+
  theme_bw()+
  ggtitle("Experiment Two")+
  #ggtitle("Experiment One")+
  xlab("Gravity Dimension") +
  ylab("Condition") +
  annotate("text", x = 0,y=1, label = "Actual")
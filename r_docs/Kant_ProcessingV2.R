#Kant_Processing is a script for preparing data and providing 
#summary measures and plots for intial analysis
install.packages(c("dplyr", "tidyselect", "magrittr", "ggplot2", "ggpubr"))
rm(list=ls())

library(readr)
library(dplyr)
library(tidyselect)
library(magrittr)
library(ggplot2)
library(ggpubr)


files <- list.files(path = "./data/mturk_data_first_50_subjs", pattern = "*.csv", full.names = T)
pilot <- sapply(files, readr::read_csv, simplify=FALSE) 
Mpilot = bind_rows(pilot)

N = 75 #Set the min number of trials
cleanPilot = select_(Mpilot,"user_id","condition","total_trials","risk","gain","no_effect", "experimental_treatment_selected")
cleanPilot = na.omit(cleanPilot)
cleanPilot = filter(cleanPilot, total_trials >= N)
cleanPilot = mutate(cleanPilot, ratio = gain / risk)
cleanPilot$ratio = round(cleanPilot$ratio,4)
cleanPilot$condition = factor(cleanPilot$condition)


survey = read.csv("./data/Qualtrics Data/Risk_Data.csv", header=FALSE, comment.char="#", stringsAsFactors=TRUE)
survey = survey[,-c(1:13)]
questions_strings = survey[1,] #Question ID's 
survey = survey[-1,]
colnames(survey)[1]= 'user_id' 
colnames(survey)[19]= 'Attention_Check' 
survey = filter(survey, Attention_Check == 0)

Cog_Domains = factor(c("Attention (Concentration)", "Motor Function", "Language (spoken)","Long Term Memory","Mood","Self-control","Short Term (Working) Memory"))


cleanPilot = merge(cleanPilot, survey, "user_id")

Neuroethics_Judgement = cleanPilot

rm(Mpilot, pilot, files, cleanPilot) #Clean up the environment

#Neuroethics_Judgement[,7] <- lapply(Neuroethics_Judgement[,7], as.numeric)

#How many observatoins does each subject have in each class (yes and no)?
datalist = list()
for (i in Neuroethics_Judgement$user_id){
  x = filter(Neuroethics_Judgement, user_id == i)
  dat = table(x$experimental_treatment_selected) #Check Class bias
  datalist[[i]] = dat # add it to your list
}
Observation_Counts = do.call(rbind, datalist)
colnames(Observation_Counts) = c('No', 'Yes')
summary(Observation_Counts)
View(Observation_Counts)
rm(i, x,datalist, dat)


##To test linearity for each of he continuous variables
#Divide into cats 
sub= filter(Neuroethics_Judgement, user_id == "nB8WmX4K")
TEST = mutate(sub, quantile = ntile(risk, 4))
xx = xtabs(~experimental_treatment_selected + quantile, data = TEST)

iyes = filter(TEST, experimental_treatment_selected==TRUE)

#Caulate portions 
#Calculate ln (p/1-p) and plot 
#Plot aganst the median




#Logsistic Reg
BaseMod = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ risk + gain, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))
Ratio_mod = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ ratio, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))

user_id = names(BaseMod) #creates vector of user_id's 

#pchisq(84.895-21.849, 73) #Chi squared to see relative differeneces from the null
anova(mod3$zrwNx8or, mod4$zrwNx8or, test="Chisq")
#drop1(BaseMod$FX9Kjnnw, test="Chisq")
#mod1 = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ gain, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))
#mod2 = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ no_effect, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))
#mod4 = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ risk*gain, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))

mod3 = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ risk + gain, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))
mod4 = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ risk + gain + risk*gain, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))


#predicted = predict(i, type="response")
#optCutOff = optimalCutoff(ii$experimental_treatment_selected, predicted)[1] 
#lapply(BaseMod, summary)
lapply(mod4, summary)
summary(mod3$nB8WmX4K)
summary(mod4$nB8WmX4K)

#Omnibus =  list(BaseMod, Ratio_mod)
# display results


#lapply(BaseMod,confint) # 95% CI for the coefficients
# exp(coef(m))exponentiated coefficients # 95% CI for exponentiated coefficients exp(confint(m))
#lapply(BaseMod, predict(type="response")) # predicted values
#lapply(BaseMod, residuals( type="deviance")) # residuals



#Extraction of the Coefficients for risk and gain
Coefficients = lapply(BaseMod, coef)
Coefficients = do.call(rbind, Coefficients)
Coefficients = Coefficients[,-1] #Removes intercept
Coefficients = data.frame(Coefficients)
colnames(Coefficients)= c('Risk_Beta', 'Gain_Beta')
#Rename
XX = exp(Coefficients)#Odds Ratios
colnames(XX) = c('OddR_Risk_Beta', 'OddR_Gain_Beta')
#Round 
XX = round(XX,3)
Coefficients[ , c('Risk_Beta', 'Gain_Beta')] = round(Coefficients[ , c('Risk_Beta', 'Gain_Beta')],3)
#Add Merge parameter
XX$user_id = names(BaseMod)
Coefficients$user_id = names(BaseMod)
Coefficients= merge(Coefficients, XX, "user_id")
#Add to main dataset
Neuroethics_Judgement= merge(Neuroethics_Judgement, Coefficients, "user_id")
rm(Coefficients, XX)


#Extraction of the Coefficients for Ratio
Coefficients = lapply(Ratio_mod, coef)
Coefficients = do.call(rbind, Coefficients)
Coefficients = Coefficients[,-1]
Coefficients = data.frame(Coefficients)
colnames(Coefficients)= c('Ratio_Beta')

#Rename
XX = exp(Coefficients)#Odds Ratios
colnames(XX) = 'OddR_Ratio_Beta'
#Round 
XX$OddR_Ratio_Beta = round(XX$OddR_Ratio_Beta,3)
Coefficients[ , 'OddR_Ratio_Beta'] = round(Coefficients[ , Coefficients[ , 'OddR_Ratio_Beta']],3)
#Add Merge parameter
XX$user_id = names(Ratio_mod)
Coefficients$user_id = names(Ratio_mod)
Coefficients= merge(Coefficients, XX, "user_id")
#Add to main dataset
Neuroethics_Judgement= merge(Neuroethics_Judgement, Coefficients, "user_id")
rm(Coefficients, XX)

#Coefficients$Odds_RISK = round(exp(Coefficients[,1]),3)
#Coefficients$Odds_GAIN = round(exp(Coefficients[,2]),3)
#Coefficients = slice(Coefficients, rep(1:n(), each = 75)) #Method only works if all observations are the same length
#we need the exp betas to for interptation purposes
#Add Coefficients to the main CSV
#Neuroethics_Judgement = bind_cols(Neuroethics_Judgement, Coefficients)

P = exp(log-odds) / (1 + exp(log-odds))


rm(TEST)

#Condition density odd ratio plots
i= "Attention (Concentration)"
for (i in Cog_Domains)
#TEST = select_(Neuroethics_Judgement,"user_id","condition","Risk_Beta","Ratio_Beta" ,"Gain_Beta", "OddR_Risk_Beta", "OddR_Gain_Beta")
#TEST = distinct(Neuroethics_Judgement,user_id, condition, Risk_Beta, Gain_Beta, Ratio_Beta, Ratio_Beta, OddR_Risk_Beta, OddR_Gain_Beta)

TEST = select_(Neuroethics_Judgement,"user_id","condition","Risk_Beta","Ratio_Beta" ,"Gain_Beta")
TEST = distinct(Neuroethics_Judgement,user_id, condition, Risk_Beta, Gain_Beta, Ratio_Beta, Ratio_Beta)
#TEST = filter(TEST, condition == i)

XX = split(TEST, TEST$condition)

#LONG FORMAT OPTION
#library(plyr); library(dplyr)
library(reshape)
data_long = melt(TEST, id.vars=c("user_id", "condition"))
View(data_long)

##PLOTS##
source("./r_docs/Multiple_plot_function.R")

#Density Plot for Risk, Gain, and Ratio


p1 = ggplot(TEST, aes(Risk_Beta)) +
  geom_density(aes(fill=factor(condition)), alpha=0.8) +
  scale_x_continuous("Beta", breaks=seq(-.5,.2,.05), limits=c(-.5, .2))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

p2 = ggplot(TEST, aes(Gain_Beta)) +
  geom_density(aes(fill=factor(condition)), alpha=0.8) +
  scale_x_continuous("Beta", breaks=seq(-.5,.2,.05), limits=c(-.5, .2))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

p3 = ggplot(TEST, aes(Ratio_Beta)) +
  geom_density(aes(fill=factor(condition)), alpha=0.8) +
  scale_x_continuous("Beta", breaks=seq(-.5,.2,.05), limits=c(-.5, .2))+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

multiplot(p1, p2, p3, cols=1)


#Draft of Main Box Plot Across conditions
ggplot(data_long, aes(condition, y=value, fill=variable)) +
  geom_boxplot()+
  scale_y_continuous("Beta", breaks=seq(-.3, 1,.05), limits=c(-.3, 1)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Prediction weights across Cognition", 
       subtitle="an investigation of domain differences",
       caption="Source: CogNew",
       x="Cognitive Domian",
       y="Log Odds")

#Separate Box Plots across conditions
ggplot(data_long, aes(condition, y=value, fill=variable)) +
  geom_boxplot()+
  facet_wrap(~condition, scale="free")


#ID individual
user_id = filter(Neuroethics_Judgement, user_id == "zrwNx8or")
sub= filter(Neuroethics_Judgement, user_id == "nB8WmX4K")
i = "zrwNx8or"


#PLOT 1
for (i in user_id){
  sub = filter(Neuroethics_Judgement, user_id == i)
  plot(sub$risk, sub$experimental_treatment_selected, 
       xaxt="n",
       yaxt="n",
       main = paste("75 Trials Pilot Subject: ",toString(i)),
       xlab = "Harm Probabilities", 
       ylab = "Choosing an Experimental Treatment", col = "blue")
       axis(side=1,at=seq(0,100,10),lwd=3)
       axis(side=2,at=c(0, 1),
       labels=c("No","Yes"))
}

    
#PLOT 2
library(ggplot2)
for (i in user_id){
  sub = filter(Neuroethics_Judgement, user_id == i)
  final.plot = 
  ggplot(sub, aes(x = risk, fill = experimental_treatment_selected)) + 
  geom_histogram(binwidth = 10, position = "stack", bins = 100, color = "black") +
  ggtitle(paste("75 Trials Pilot Subject: ",toString(i))) +
  scale_x_continuous("Harm Probabilities", breaks=seq(0,100,5), limits=c(0, 100))+
  scale_y_continuous("Frequency", breaks=seq(0,15,1), limits=c(0,15))+
  theme_linedraw() + 
  scale_fill_discrete(name = "", labels = c("No to experimental treatment","Yes to experimental treatment"))
 
  pdf(paste0(i , ".pdf"))
  print(final.plot)
  dev.off()
}




#THE SUPER PLOT
library(ggplot2)
for (i in user_id){
  sub = filter(Neuroethics_Judgement, user_id == i)
  p1 = 
    ggplot(sub, aes(x = risk, fill = experimental_treatment_selected)) + 
    geom_histogram(binwidth = 4, position = "stack", bins = 100, color = "black") +
    ggtitle(paste("75 Trials Pilot Subject: ",toString(i))) +
    scale_x_continuous("Harm Probabilities", breaks=seq(0,100,5), limits=c(0, 100))+
    scale_y_continuous("Number of Occurrences", breaks=seq(0,15,1), limits=c(0,15))+
    theme_linedraw() + 
    theme(legend.position=c(.78, .78)) +
    scale_fill_discrete(name = "", labels = c("No to experimental treatment","Yes to experimental treatment"))
  
  p2 = 
    ggplot(sub, aes(x = gain, fill = experimental_treatment_selected)) + 
    geom_histogram(binwidth = 4, position = "stack", bins = 100, color = "black") +
    scale_x_continuous("Gain Probabilities", breaks=seq(0,100,5), limits=c(0, 100))+
    scale_y_continuous("Number of Occurrences", breaks=seq(0,15,1), limits=c(0,15))+
    theme_linedraw() +
    theme(legend.position="none")
  
  p3 =
    ggplot(sub, aes(x = ratio, fill = experimental_treatment_selected)) +
    geom_histogram(binwidth= .04, position="stack",color="black") +
    scale_x_continuous("Ratio (Benefit over Harm) Probabilities", breaks=seq(0,20,.2), limits=c(0, 20)) +
    scale_y_continuous("Number of Occurrences", breaks=seq(0,15,1), limits=c(0,15)) +
    theme_linedraw()+
    theme(legend.position="none", axis.text.x  = element_text(angle=50, vjust=0.5, size=6)) 
  
  png(filename = paste0(i , ".png"),
      width = 1300, height = 575)  
  multiplot(p1, p2, p3, cols=2)
  dev.off()
}

#Trials on some plots
ggplot(sub,aes(x = risk , y = gain)) + 
  geom_point(aes(color = experimental_treatment_selected), size = qsec, alpha = .7) +
  ggtitle(paste("75 Trials Pilot Subject: ",toString(i))) +
  scale_x_continuous("Risk Probabilities", breaks=seq(0,100,5), limits=c(0, 100)) +
  scale_y_continuous("Gain Probabilities", breaks=seq(0,100,5), limits=c(0, 100)) + 
  theme(legend.position="none")


library(ggExtra)
g = ggplot(sub, aes(risk, gain)) + 
  geom_count(aes(color = experimental_treatment_selected), size = 5, alpha = .7) + 
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill= "blue")



#Check the density plots of the resp

for (i in user_id){
  sub = filter(Neuroethics_Judgement, user_id == i)
  iyes = filter(sub, experimental_treatment_selected==TRUE)
  ino = filter(sub, experimental_treatment_selected==FALSE)
  
  p1 = ggdensity(ino$risk, 
            main = paste("No occurances subject: ",toString(i)),
            xlab = "risk")
  p2 = ggdensity(iyes$risk, 
            main = paste("Yes occurances subject: ",toString(i)),
            xlab = "risk")
  
  p3 = ggdensity(ino$gain, 
            main = paste("No occurances subject: ",toString(i)),
            xlab = "gain")
  p4 = ggdensity(iyes$gain, 
            main = paste("Yes occurances subject: ",toString(i)),
            xlab = "gain")
  
  p5 = ggdensity(ino$no_effect, 
            main = paste("No occurances subject: ",toString(i)),
            xlab = "no effect")
  p6 = ggdensity(iyes$no_effect, 
            main = paste("Yes occurances subject: ",toString(i)),
            xlab = "no effect")
  
  png(filename = paste0(i , ".png"),
      width = 1234, height = 468)
  multiplot(p1, p2, p3, p4, p5, p6, cols=3)
  dev.off()
}
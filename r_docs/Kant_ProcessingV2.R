#Kant_Processing is a script for preparing data and providing 
#summary measures and plots for intial analysis
install.packages(c("dplyr", "tidyselect", "magrittr", "ggplot2", "ggpubr"))
rm(list=ls())

source("./r_docs/LoadLibraries.R")
LoadLibraries()

#Import and Transform
files <- list.files(path = "./data/mturk_data_first_50_subjs", pattern = "*.csv", full.names = T)
pilot <- sapply(files, readr::read_csv, simplify=FALSE) 
Mpilot = bind_rows(pilot)

N = 75 #Set the min number of trials
cleanPilot = select_(Mpilot,"user_id","condition","total_trials","risk","gain","no_effect", "experimental_treatment_selected")
cleanPilot = na.omit(cleanPilot)
cleanPilot = filter(cleanPilot, total_trials >= N)
cleanPilot = mutate(cleanPilot, ratio = gain / risk)#Gain over Risk
cleanPilot$ratio = round(cleanPilot$ratio,4)
cleanPilot$condition = factor(cleanPilot$condition)


survey = read.csv("./data/Qualtrics Data/Risk_Data.csv", header=FALSE, comment.char="#", stringsAsFactors=TRUE)
survey = survey[,-c(1:13)]
questions_strings = survey[1,] #Question ID's 
survey = survey[-1,]
colnames(survey)[1]= 'user_id' 
colnames(survey)[19]= 'Attention_Check' 
survey = filter(survey, Attention_Check == 0)

Demographics = survey[, 97:114]
colnames(Demographics) = c('Sex','Age', 'Race/Ethnicity', 'Education')

Cog_Domains = factor(c("Attention (Concentration)", "Motor Function", "Language (spoken)","Long Term Memory","Mood","Self-control","Short Term (Working) Memory"))


cleanPilot = merge(cleanPilot, survey, "user_id")

Neuroethics_Judgement = cleanPilot
Neuroethics_Judgement$experimental_treatment_selected = factor(Neuroethics_Judgement$experimental_treatment_selected, labels = c("No to treatment","Yes to treatment"))


rm(Mpilot, pilot, files, cleanPilot) #Clean up the environment

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




####Logsistic Reg
BaseMod = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ risk + gain, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))
Ratio_mod = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ ratio, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))
InteractionMod = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ risk + gain + risk*gain, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))


user_id = names(BaseMod) #creates vector of user_id's 

#pchisq(84.895-21.849, 73) #Chi squared to see relative differeneces from the null
#anova(mod3$nB8WmX4K, mod4$nB8WmX4K, test="Chisq")

#loop an anove test of the interaction on base and ratio model
datalist = list()
for (i in 1:46){
dat = anova(Ratio_mod[[i]], InteractionMod[[i]], test="Chisq")
  datalist[[i]] = dat
}
lapply(datalist, print) 

drop1(InteractionMod$nB8WmX4K, test="Chisq")
#mod1 = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ gain, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))
#mod2 = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ no_effect, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))
#mod4 = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ risk*gain, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))

#mod3 = by(Neuroethics_Judgement, Neuroethics_Judgement$user_id, function(x) glm(experimental_treatment_selected ~ risk + gain, data = x, family = binomial(link = "logit"),control=glm.control(maxit=50)))



#individual curve fitting for a single predictor
sub = filter(Neuroethics_Judgement, user_id == "nB8WmX4K")
logr <- glm(experimental_treatment_selected ~ risk, data = sub, family = binomial(link = "logit"),control=glm.control(maxit=50))
summary(logr)
ggplot(sub, aes(x=risk, y=experimental_treatment_selected)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
  par(mar = c(4, 4, 1, 1)) # Reduce some of the margins so that the plot fits better
  plot(sub$risk, sub$experimental_treatment_selected)
  curve(predict(logr, data.frame(risk=x), type="response"), add=TRUE) 
##########################################
  
#predicted = predict(i, type="response")
#optCutOff = optimalCutoff(ii$experimental_treatment_selected, predicted)[1] 
#lapply(BaseMod, summary)
#lapply(mod4[1:10], summary)
#summary(mod3$nB8WmX4K)
#summary(mod4$nB8WmX4K)

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

Coefficients$user_id = names(BaseMod)

Neuroethics_Judgement= merge(Neuroethics_Judgement, Coefficients, "user_id")
rm(Coefficients, XX)



Coefficients = exp(Coefficients)#exponetiated 
#Rename
XX = exp(Coefficients)#Odds Ratios
colnames(XX) = c('OddR_Risk_Beta', 'OddR_Gain_Beta')
#Round 
XX = round(XX,4)
Coefficients[ , c('Risk_Beta', 'Gain_Beta')] = round(Coefficients[ , c('Risk_Beta', 'Gain_Beta')],4)
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

Coefficients$user_id = names(Ratio_mod)
Neuroethics_Judgement= merge(Neuroethics_Judgement, Coefficients, "user_id")
rm(Coefficients, XX)


Coefficients = exp(Coefficients)#exponetiated 
#Rename
XX = exp(Coefficients)#Odds Ratios
colnames(XX) = 'OddR_Ratio_Beta'
#Round 
XX$OddR_Ratio_Beta = round(XX$OddR_Ratio_Beta,3)
Coefficients[ , "Ratio_Beta"] = round(Coefficients[ , "Ratio_Beta"], 4)
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


#Condition density plots

TEST = select_(Neuroethics_Judgement,"user_id","condition","Risk_Beta","Ratio_Beta" ,"Gain_Beta")
TEST = distinct(Neuroethics_Judgement,user_id, condition, Risk_Beta, Gain_Beta, Ratio_Beta, Ratio_Beta)


XX = split(TEST, TEST$condition)

#LONG FORMAT OPTION
library(reshape)
data_long = melt(TEST, id.vars=c("user_id", "condition"))
View(data_long)

##PLOTS##
source("./r_docs/Multiple_plot_function.R")



#one way between anova:
#IV: variable
#IV: condition
#DV: value
aov2 <- aov(value ~ variable*condition, data = data_long)
summary(aov2)
model.tables(aov2, "means")


#Draft of Main Box Plot Across conditions
ggplot(data_long, aes(condition, y=value, fill=variable)) +
  geom_boxplot()+
  scale_y_continuous("Beta", breaks=seq(-.3, 2,.05), limits=c(-.3, 2)) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="", 
       subtitle="an investigation of domain differences",
       caption="Source: CogNew",
       x="Cognitive Domian",
       y="Log Odds")

#Separate Box Plots across conditions
ggplot(data_long, aes(condition, y=value, fill=variable)) +
  geom_boxplot()+
  facet_wrap(~condition, scale="free")


#ID individual
user_id = filter(Neuroethics_Judgement, user_id == "nB8WmX4K")
sub= filter(Neuroethics_Judgement, user_id == "nB8WmX4K")
i = "nB8WmX4K"

##########Omnibus plot 

for (i in user_id){
  sub = filter(Neuroethics_Judgement, user_id == i)
  
  p1 = ggdensity(sub, 
          main = paste("subject: ",toString(i)),
          x = "risk",
          legend.title = "",
          xticks.by = 10,
          add = "mean", rug = TRUE,
          color = "experimental_treatment_selected", fill ="experimental_treatment_selected", palette = c("#f8766d", "#00bfc4"))
  p3 = ggdensity(sub, 
          x = "gain",
          legend = "none",
          xticks.by = 10,
          add = "mean", rug = TRUE,
          color = "experimental_treatment_selected", fill ="experimental_treatment_selected", palette = c("#f8766d", "#00bfc4"))
  p5 = ggdensity(sub, 
          x = "no_effect",
          legend = "none",
          xticks.by = 10,
          add = "mean", rug = TRUE,
          color = "experimental_treatment_selected", fill ="experimental_treatment_selected", palette = c("#f8766d", "#00bfc4"))

  p2=gghistogram(sub, 
            x = "risk",
            ylab = "Number of Occurrences",
            add = "mean", rug = TRUE,
            fill = "experimental_treatment_selected",  palette = c("#f8766d", "#00bfc4"),
            add_density = TRUE, bins = 30,
            legend = "none",
            xticks.by = 10)
  p4=gghistogram(sub, 
            x = "gain",
            ylab = "Number of Occurrences",
            add = "mean", rug = TRUE,
            fill = "experimental_treatment_selected",  palette = c("#f8766d", "#00bfc4"),
            add_density = TRUE, bins = 30,
            legend = "none",
            xticks.by = 10)
  p6=gghistogram(sub, 
            x = "no_effect",
            alpha = .5,
            ylab = "Number of Occurrences",
            add = "mean", rug = TRUE,
            fill = "experimental_treatment_selected",  palette = c("#f8766d", "#00bfc4"),
            add_density = TRUE, bins = 30, 
            legend = "none",
            xticks.by = 10)

  png(filename = paste0(i , ".png"),
    width = 1234, height = 468)
  multiplot(p1, p2, p3, p4, p5, p6, cols=3)
  dev.off()
}

##RATIO PLOTS
for (i in user_id){
  sub = filter(Neuroethics_Judgement, user_id == i)
  
  p1 = ggdensity(sub, 
               x = "ratio",
               legend.title = paste("75 Trials Pilot Subject: ",toString(i)),
               add = "mean", rug = TRUE,
               color = "experimental_treatment_selected", fill ="experimental_treatment_selected", palette = c("#f8766d", "#00bfc4"),
               xticks.by = 1)
  p2 = gghistogram(sub, 
            x = "ratio",
            alpha = .5,
            ylab = "Number of Occurrences",
            add = "mean", rug = TRUE,
            fill = "experimental_treatment_selected",  palette = c("#f8766d", "#00bfc4"),
            add_density = TRUE, bins = 200,
            legend = "none", 
            xticks.by = 1)
  
  png(filename = paste0(i , ".png"),
    width = 1234, height = 468)
  multiplot(p1, p2, cols=1)
  dev.off()
}

#Individual Scatter: subject graph 

for (i in user_id){
  sub = filter(Neuroethics_Judgement, user_id == i)
  
g = ggplot(sub,aes(x = risk , y = gain)) + 
  geom_point(aes(color = experimental_treatment_selected), size = 2.5, alpha = .7) +
  ggtitle(paste("75 Trials Pilot Subject: ",toString(i))) +
  scale_x_continuous("Risk Probabilities", breaks=seq(0,100,5), limits=c(0, 100)) +
  scale_y_continuous("Gain Probabilities", breaks=seq(0,100,5), limits=c(0, 100)) + 
  theme(legend.position="none")
  
  png(filename = paste0(i , ".png"),
      width = 500, height = 372)
  print(g)
  dev.off()
}
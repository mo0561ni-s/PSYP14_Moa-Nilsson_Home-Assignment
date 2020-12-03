#Assignment 1----
library(psych) #Just Loading Some Packages
library(tidyverse)
library(car)
library(lm.beta)
library(sandwich)
library(lmboot)
library(lmtest)
library(gsheet)
library(ggplot2)
library(lme4)
library(lmerTest)
library(cAIC4)
library(r2glmm)
library(MuMIn)


DataSample1 <- read.csv("https://tinyurl.com/ha-dataset1")

#Data Exploration And Visualisation ----
view(DataSample1)
summary(DataSample1)

DataSample1 %>%  #Visualization Age
  ggplot() + 
  aes(age) + 
  geom_bar()

DataSample1 %>%  #Visualization Sex
  ggplot() + 
  aes(sex) + 
  geom_bar()

DataSample1 %>%  #Visualization STAI_Trait
  ggplot() + 
  aes(STAI_trait) + 
  geom_bar()

DataSample1 %>% #Visualization HouseholdI Ncome
  ggplot() +
  aes(household_income) + 
  geom_histogram()

DataSample1 %>% #Visualization Pain_Cat
  ggplot() +
  aes(pain_cat) + 
  geom_bar()
  
DataSample1 %>% #VisualizationP ain
  ggplot() + 
  aes(pain) + 
  geom_bar()
  
DataSample1 %>% #Visualization Cortisol Serum
  ggplot() + 
  aes(cortisol_serum) + 
  geom_histogram()
  
DataSample1 %>% #Visualization Cortisol Saliva
  ggplot() + 
  aes(cortisol_saliva) + 
  geom_histogram()
  
DataSample1 %>% #Visualization Mindfulness
  ggplot() + 
  aes(mindfulness) + 
  geom_histogram()
  
DataSample1 %>% #Visualization Weight
  ggplot() + 
  aes(weight) + 
  geom_histogram()

DataSample1 %>% #Visualization IQ
  ggplot() + 
  aes(IQ) + 
  geom_histogram()

describe(DataSample1)

view(CorrectedDataSample1)
#Data Correction----

CorrectedDataSample1 <- DataSample1 %>% 
  mutate(age = replace(age, age =="444", NA)) %>% 
  mutate(STAI_trait = replace(STAI_trait, STAI_trait == "3.9", NA)) %>% 
  mutate(household_income = replace(household_income, household_income == "-3732", NA))

summary(CorrectedDataSample1)

CorrectedDataSample1 <- drop_na(CorrectedDataSample1)

#Visualizing again in order to double check that everything has been corrected

CorrectedDataSample1 %>% #VisualizationHouseholdINcome
  ggplot() +
  aes(household_income) + 
  geom_histogram()

CorrectedDataSample1 %>% #VisualizationHouseholdINcome
  ggplot() +
  aes(age) + 
  geom_bar()

CorrectedDataSample1 %>% #VisualizationHouseholdINcome
  ggplot() +
  aes(STAI_trait) + 
  geom_bar()

#Creating Models ----
Mod1 <- lm(CorrectedDataSample1$pain ~ sex + age, data = CorrectedDataSample1)

print(Mod1)
summary(Mod1)

Mod2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = CorrectedDataSample1)

print(Mod2)  
summary(Mod2)

#Visualizing The Model ----
Plot1 <- Mod1 %>% 
  ggplot() + 
  aes(x = CorrectedDataSample1$pain, y = age, color = sex) + 
  geom_point() + 
  geom_smooth(method = lm)

Plot1

#ModelDiagnostics ----

#CooksDistance----

Mod1 %>% 
  plot(which = 4) #99, 126, 139 marked

CorrectedDataSample1 %>% 
  slice(c(99, 126, 139)) #Values Seems Reasonable So I Keep Them

Mod2 %>% 
  plot(which = 4) # 68, 99, 112 marked

CorrectedDataSample1 %>% 
  slice(68, 113, 147) #ValuesSeemsReasonableSoIKeepThem

#Normality----

#QQPlot
QQ1 <- Mod1 %>% 
  plot(which = 2)

QQ2 <- Mod2 %>% 
  plot(which = 2)

#Linearity----


Mod1 %>% 
residualPlots()

Mod2 %>% 
  residualPlots()

#HomogeneityOfVariance----
HoV1 <- Mod1 %>% 
  plot(which = 3)

HoV2 <- Mod2 %>% 
  plot(which = 3)

Mod1 %>% bptest()

Mod2 %>% bptest()
#Multicollinearity----

vif(Mod1)

vif(Mod2)

#Removing cortisol saliva

RevisedMod2 <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = CorrectedDataSample1)


summary(RevisedMod2)

vif(RevisedMod2)
#ComparingModels ----

AIC(Mod1)
AIC(RevisedMod2)

summary(Mod1)
summary(RevisedMod2)

anova(Mod1, RevisedMod2)

lm.beta(Mod1)

lm.beta(RevisedMod2)

confint(Mod1)

confint(RevisedMod2)







#Assignment2 ----

TheoryBasedModel <- RevisedMod2

#BackwardsRegression ----

InitialBackMod <- lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + household_income + IQ, data = CorrectedDataSample1)


BackwardModel <- step(InitialBackMod, direction = "backward")

print(BackwardModel)

BackwardModel <- lm(pain ~ sex + age + pain_cat + mindfulness + cortisol_serum + household_income, data = CorrectedDataSample1)

print(BackwardModel)
summary(BackwardModel)
#Initial model ----

summary(InitialBackMod)
lm.beta(InitialBackMod)
confint(InitialBackMod) %>% 
  format(scientific = F)

AIC(InitialBackMod)

#Model Diagnostics 2----

#CooksDistance2 ----
BackwardModel %>% #102, 112 and 146 marked
plot(which = 4)

CorrectedDataSample1 %>% 
  slice(c(102, 112, 146))

#Normality2----
QQBack <- BackwardModel %>% 
  plot(which = 2)

#Linearity2----

BackwardModel %>% 
  residualPlots()

#HomogeneityOfVariance2----
HoVback <- BackwardModel %>% 
  plot(which=3)

BackwardModel %>% bptest()


#Multicollinearity2----
vif(BackwardModel)

#ModelComparison2----
print(BackwardModel)
summary(BackwardModel)


AIC(BackwardModel)
AIC(TheoryBasedModel)

summary(BackwardModel)
summary(TheoryBasedModel)

anova(BackwardModel, TheoryBasedModel)

lm.beta(BackwardModel)

lm.beta(TheoryBasedModel)

confint(BackwardModel)

confint(TheoryBasedModel)

#DataSet2----

DataSample2 <- ha_dataset2

view(DataSample2)
summary(DataSample2)

DataSample2$mindfulness %>% 
  max()

CorrectedDataSample2 <- DataSample2 %>%  #Correcting the mindfulness value
  mutate(mindfulness = replace(mindfulness, mindfulness=="7.17", NA))

CorrectedDataSample2$mindfulness %>% 
  summary()

CorrectedDataSample2 <- drop_na(CorrectedDataSample2)

CorrectedDataSample2 <- as.tibble(CorrectedDataSample2)
#prediction ----

predtest <- predict(TheoryBasedModel, CorrectedDataSample2)

predtestback <- predict(BackwardModel,CorrectedDataSample2)

#RSS----

RSS_test  <- sum((CorrectedDataSample2[, "pain"] - predtest)^2)
RSS_test

RSS_test_back = sum((CorrectedDataSample2[,"pain"] - predtestback)^2)
RSS_test_back

#Assignment 3 ----
DataSample3 <- home_sample_3
summary(DataSample3)

#CheckingTheDataSet ----

summary(DataSample3)

DataSample3 %>%  #Visualization STAI_Trait
  ggplot() + 
  aes(sex) + 
  geom_bar()

CorrectedDataSample3 <- DataSample3 %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_1", 1)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_2", 2)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_3", 3)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_4", 4)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_5", 5)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_6", 6)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_7", 7)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_8", 8)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_9", 9)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_10", 10)) %>% 
  mutate(household_income = replace(household_income, household_income =="-6994", NA)) %>% 
  mutate(sex = replace(sex, sex == "femlae", "female"))

CorrectedDataSample3 %>% drop_na()

#LinearModelwithclustering----
RepeatedVariables <- CorrectedDataSample3$hospital

RandomInterceptModel =lmer(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|hospital), data = CorrectedDataSample3)
RandomInterceptModel

confint(RandomInterceptModel)
r.squaredGLMM(RandomInterceptModel)
summary(RandomInterceptModel)

#Applying this to data set 4----
DataSample4 <- home_sample_4
summary(DataSample4)
#CorrectingDataSet4 ----

DataSample4 %>%  #Visualization STAI_Trait
  ggplot() + 
  aes(sex) + 
  geom_bar()

summary(DataSample4)
CorrectedDataSample4 <- DataSample4 %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_1", 1)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_2", 2)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_3", 3)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_4", 4)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_5", 5)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_6", 6)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_7", 7)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_8", 8)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_9", 9)) %>% 
  mutate(hospital = replace(hospital, hospital =="hospital_10", 10)) %>% 
  mutate(household_income = replace(household_income, household_income =="-23482", NA)) %>% 
  mutate(mindfulness = replace(mindfulness, mindfulness == "6.050", NA))

CorrectedDataSample4 %>% drop_na()



#Applying model to sample 4----

predtest4 <- predict(RandomInterceptModel, CorrectedDataSample4, allow.new.levels = T)


#RSS----

RSS4  <- sum((CorrectedDataSample4[, "pain"] - predtest4)^2)
RSS4

predtest4 %>% 
  summary()

r.squaredGLMM(RandomInterceptModel, envir = CorrectedDataSample3)

r.squaredGLMM(RandomInterceptModel, envir = CorrectedDataSample4)

#Last model ----

RandomSlopeModel =lmer(pain ~ cortisol_serum + (cortisol_serum|hospital), data = CorrectedDataSample3)
summary(RandomSlopeModel)

#Visualization ----

Slopes <- CorrectedDataSample3 %>% 
  mutate(predint =predict(RandomInterceptModel),predslop =predict(RandomSlopeModel))
Slopes

RandomSlopeSlope <- Slopes%>%
  ggplot()+
  aes(y = pain, x = cortisol_serum, group = hospital)+
  geom_point(aes(color = hospital), size = 4)+
  geom_line(color='red',aes(y=predslop, x=cortisol_serum))+
  facet_wrap(~hospital, ncol = 2)
RandomSlopeSlope

r.squaredGLMM(RandomSlopeModel)
r.squaredGLMM((RandomInterceptModel))

#Set up
install.packages("dplyr")
update.packages("dplyr")
library(dplyr)
install.packages("NHANES")
library(NHANES)
data(NHANES)
?NHANES
dim(NHANES) #n = 10,000 obs. of 76 variables
library(tidyverse)

#A. Data cleaning + Defining population


#Selecting adults >18
nhanes_adult <- subset(NHANES, NHANES$Age >= 18)
dim(nhanes_adult) #n = 7481, 76
str(nhanes_adult)
colSums(is.na(nhanes_adult))

#Dropping cases with missing outcome variables
nhanes_adult1 <- subset(nhanes_adult, !is.na(nhanes_adult$BPSysAve))
dim(nhanes_adult1) #n = 7205
nhanes_adult1 <- subset(nhanes_adult1, !is.na(nhanes_adult1$BPDiaAve))
dim(nhanes_adult1) #n = 7205
summary(nhanes_adult1$BPSysAve)
summary(nhanes_adult1$BPDiaAve)
nhanes_adult1 <- subset(nhanes_adult1, nhanes_adult1$BPDiaAve >0)
nhanes_adult1 <- subset(nhanes_adult1, nhanes_adult1$BPDia1 >0)
nhanes_adult1 <- subset(nhanes_adult1, nhanes_adult1$BPDia2 >0)
nhanes_adult1 <- subset(nhanes_adult1, nhanes_adult1$BPDia3 >0)
dim(nhanes_adult1) #n = 6668
summary(nhanes_adult1$BPDiaAve)

#Excluding malingnant hypertension
boxplot(nhanes_adult1$BPSys1, nhanes_adult1$BPSys2, nhanes_adult1$BPSys3, nhanes_adult1$BPSysAve)
boxplot(nhanes_adult1$BPDia1, nhanes_adult1$BPDia2, nhanes_adult1$BPDia3, nhanes_adult1$BPDiaAve)
nhanes_adult1 <- subset(nhanes_adult1, nhanes_adult1$BPSysAve < 180)
nhanes_adult1 <- subset(nhanes_adult1, nhanes_adult1$BPDiaAve < 110)

#Dropping cases with missing exposure variables
nhanes_adult1 <- subset(nhanes_adult1, !is.na(nhanes_adult1$BMI))
nhanes_adult1 <- subset(nhanes_adult1, !is.na(nhanes_adult1$SleepHrsNight))

#Selecting only non-pregnant adults
typeof(nhanes_adult$PregnantNow)
table(nhanes_adult$PregnantNow)
levels(nhanes_adult$PregnantNow)
nhanes_adult1 <- nhanes_adult1[!nhanes_adult1$PregnantNow %in% "Yes",]
dim(nhanes_adult1) #n = 6504
table(nhanes_adult1$PregnantNow)

#Exploring potential other exclusion criteria
sort(colSums(is.na(nhanes_adult1)))
typeof(nhanes_adult1$Diabetes)
levels(nhanes_adult1$Diabetes)
table(nhanes_adult1$Diabetes) #642/6520 are diabetic i.e. approx 10%
nhanes_adult1<- nhanes_adult1[!nhanes_adult1$Diabetes %in% "Yes", ]
dim(nhanes_adult1) #5862
table(nhanes_adult1$Diabetes) #5860

#B. Data analysis
#Table 1
nhanes_adult1$hypertensive <- nhanes_adult1$BPSysAve > 140 | nhanes_adult1$BPDiaAve > 90
install.packages("tableone")
library(tableone)
str(nhanes_adult1)
variables = c("Age", "BMI", "BPSysAve", "BPDiaAve", "Diabetes", "SleepHrsNight", "PhysActiveDays", 
              "Gender", "Race1", "Education", "MaritalStatus", "HHIncome", 
              "Depressed", "SleepTrouble", "PhysActive", "Alcohol12PlusYr", "SmokeNow", "Smoke100")
facvariables = c("Gender", "Race1", "Education", "MaritalStatus", "HHIncome", 
                 "Depressed", "SleepTrouble", "PhysActive", "Alcohol12PlusYr", "SmokeNow", "Smoke100")
tab <- CreateTableOne(data = nhanes_adult1, vars = variables, factorVars = facvariables, 
                      strata = "hypertensive")
table <- print(tab)
write.csv(table, file = "Table1.csv")

#Data visualisation and manipulation
hist(nhanes_adult1$BPSysAve) #skewed not a problem
boxplot(nhanes_adult1$BPSysAve)
hist(nhanes_adult1$BPDiaAve)
boxplot(nhanes_adult1$BPDiaAve)

cor(nhanes_adult1$BPSysAve, nhanes_adult1$BPDiaAve)
qplot(nhanes_adult1$BPSysAve, nhanes_adult1$BPDiaAve)
#Not highly correlated therefore consider as two separate outcomes

#BinarySleep 
hist(nhanes_adult1$SleepHrsNight)
nhanes_adult1_SleepHrsNight_under7 <- subset(nhanes_adult1, nhanes_adult1$SleepHrsNight<7) #2068
nhanes_adult1_SleepHrsNight_over7 <- subset(nhanes_adult1, nhanes_adult1$SleepHrsNight>=7) #3794
qplot(nhanes_adult1$SleepHrsNight, nhanes_adult1$BPSysAve)
cor(!is.na(nhanes_adult1$SleepHrsNight), nhanes_adult1$BPSysAve)
dim(nhanes_adult1_SleepHrsNight_under7) #2068
dim(nhanes_adult1_SleepHrsNight_over7) #3794

qplot(nhanes_adult1_SleepHrsNight_under7$SleepHrsNight, nhanes_adult1_SleepHrsNight_under7$BPSysAve)
qplot(nhanes_adult1_SleepHrsNight_over7$SleepHrsNight, nhanes_adult1_SleepHrsNight_over7$BPSysAve)
qplot(nhanes_adult1_SleepHrsNight_under7$SleepHrsNight, nhanes_adult1_SleepHrsNight_under7$BPDiaAve)
qplot(nhanes_adult1_SleepHrsNight_over7$SleepHrsNight, nhanes_adult1_SleepHrsNight_over7$BPDiaAve)
cor(!is.na(nhanes_adult1$SleepHrsNight), nhanes_adult1$BPDiaAve)

ggplot(nhanes_adult1_SleepHrsNight_under7, aes(x=SleepHrsNight, y=BPSysAve, color=Age,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_under7, aes(x=SleepHrsNight, y=BPDiaAve, color=Age,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_over7, aes(x=SleepHrsNight, y=BPSysAve, color=Age,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_over7, aes(x=SleepHrsNight, y=BPDiaAve, color=Age,)) +geom_point()

ggplot(nhanes_adult1_SleepHrsNight_under7, aes(x=SleepHrsNight, y=BPSysAve, color=Gender,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_under7, aes(x=SleepHrsNight, y=BPDiaAve, color=Gender,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_over7, aes(x=SleepHrsNight, y=BPSysAve, color=Gender,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_over7, aes(x=SleepHrsNight, y=BPDiaAve, color=Gender,)) +geom_point()

#Univariate
model_under7<- lm(nhanes_adult1_SleepHrsNight_under7$BPSysAve ~ nhanes_adult1_SleepHrsNight_under7$SleepHrsNight)
summary(model)
plot(model)
model

model_over7<- lm(nhanes_adult1_SleepHrsNight_over7$BPSysAve ~ nhanes_adult1_SleepHrsNight_over7$SleepHrsNight)
summary(model)
plot(model)
model

#Multivariate
#With age and gender
boxplot(nhanes_adult1_SleepHrsNight_under7$Age)
boxplot(nhanes_adult1_SleepHrsNight_over7$Age)
table(nhanes_adult1_SleepHrsNight_under7$Gender) # Fem=977, Male=1091 
table(nhanes_adult1_SleepHrsNight_over7$Gender) #Fem=1934, Male=1860

model1_under7 <- lm(nhanes_adult1_SleepHrsNight_under7$BPSysAve ~ nhanes_adult1_SleepHrsNight_under7$SleepHrsNight +nhanes_adult1_SleepHrsNight_under7$Age + nhanes_adult1_SleepHrsNight_under7$Gender)
summary(model1_under7)
plot(model1_under7)
model1_over7 <- lm(nhanes_adult1_SleepHrsNight_over7$BPSysAve ~ nhanes_adult1_SleepHrsNight_over7$SleepHrsNight +nhanes_adult1_SleepHrsNight_over7$Age + nhanes_adult1_SleepHrsNight_over7$Gender)
summary(model1_over7)
plot(model1_over7)

#With age, gender, race, physical activity, education and alcohol 
#7 hours and under 
sum(is.na(nhanes_adult1_SleepHrsNight_under7$Race1)) #0
table(nhanes_adult1_SleepHrsNight_under7$Race1)
table(nhanes_adult1_SleepHrsNight_under7$Education)
sum(is.na(nhanes_adult1_SleepHrsNight_under7$Education)) #51
hist(nhanes_adult1_SleepHrsNight_under7$AlcoholYear)
sum(is.na(nhanes_adult1_SleepHrsNight_under7$AlcoholYear)) #384
table(nhanes_adult1_SleepHrsNight_under7$PhysActive)
sum(is.na(nhanes_adult1_SleepHrsNight_under7$PhysActive)) #0

#Over 7 hours
sum(is.na(nhanes_adult1_SleepHrsNight_over7$Race1))#0
table(nhanes_adult1_SleepHrsNight_over7$Race1)
table(nhanes_adult1_SleepHrsNight_over7$Education)
sum(is.na(nhanes_adult1_SleepHrsNight_over7$Education))#166
hist(nhanes_adult1_SleepHrsNight_over7$AlcoholYear)
sum(is.na(nhanes_adult1_SleepHrsNight_over7$AlcoholYear))#737
table(nhanes_adult1_SleepHrsNight_over7$PhysActive)
sum(is.na(nhanes_adult1_SleepHrsNight_over7$PhysActive))#0

#7 hours and under
model2_under7<- lm(nhanes_adult1_SleepHrsNight_under7$BPSysAve ~ nhanes_adult1_SleepHrsNight_under7$SleepHrsNight + nhanes_adult1_SleepHrsNight_under7$Gender + nhanes_adult1_SleepHrsNight_under7$Race1 + nhanes_adult1_SleepHrsNight_under7$Education + nhanes_adult1_SleepHrsNight_under7$PhysActive)
summary(model2_under7)
plot(model2_under7)

#Over 7 hours
model2_over7<- lm(nhanes_adult1_SleepHrsNight_over7$BPSysAve ~ nhanes_adult1_SleepHrsNight_over7$SleepHrsNight + nhanes_adult1_SleepHrsNight_over7$Gender + nhanes_adult1_SleepHrsNight_over7$Race1 + nhanes_adult1_SleepHrsNight_over7$Education + nhanes_adult1_SleepHrsNight_over7$PhysActive)
summary(model2_over7)
plot(model2_over7)

#Descriptive Statistics 
#7 hours and under
mean(nhanes_adult1$BPDiaAve[nhanes_adult1$SleepHrsNight <7], na.rm=TRUE) #71.32689
mean(nhanes_adult1$BPSysAve[nhanes_adult1$SleepHrsNight <7], na.rm = TRUE) #120.5358

#Over 7 hours
mean(nhanes_adult1$BPDiaAve[nhanes_adult1$SleepHrsNight>=7], na.rm=TRUE)#69.88877
mean(nhanes_adult1$BPSysAve[nhanes_adult1$SleepHrsNight>=7], na.rm = TRUE)#118.8722

t.test(nhanes_adult1$BPDiaAve[nhanes_adult1$SleepHrsNight<7], nhanes_adult1$BPSysAve[nhanes_adult1$SleepHrsNight>=7])


install.packages("gplots")
library("gplots")
chisq <- chisq.test(nhanes_adult1)




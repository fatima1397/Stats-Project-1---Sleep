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
nhanes_adult1_SleepHrsNight_under5 <- subset(nhanes_adult1, nhanes_adult1$SleepHrsNight<=5) #706
nhanes_adult1_SleepHrsNight_over5 <- subset(nhanes_adult1, nhanes_adult1$SleepHrsNight>5) #5156
qplot(nhanes_adult1$SleepHrsNight, nhanes_adult1$BPSysAve)
cor(!is.na(nhanes_adult1$SleepHrsNight), nhanes_adult1$BPSysAve)
dim(nhanes_adult1_SleepHrsNight_under5) #706
dim(nhanes_adult1_SleepHrsNight_over5) #5156

qplot(nhanes_adult1_SleepHrsNight_under5$SleepHrsNight, nhanes_adult1_SleepHrsNight_under5$BPSysAve)
qplot(nhanes_adult1_SleepHrsNight_over5$SleepHrsNight, nhanes_adult1_SleepHrsNight_over5$BPSysAve)
qplot(nhanes_adult1_SleepHrsNight_under5$SleepHrsNight, nhanes_adult1_SleepHrsNight_under5$BPDiaAve)
qplot(nhanes_adult1_SleepHrsNight_over5$SleepHrsNight, nhanes_adult1_SleepHrsNight_over5$BPDiaAve)
cor(!is.na(nhanes_adult1$SleepHrsNight), nhanes_adult1$BPDiaAve)

ggplot(nhanes_adult1_SleepHrsNight_under5, aes(x=SleepHrsNight, y=BPSysAve, color=Age,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_under5, aes(x=SleepHrsNight, y=BPDiaAve, color=Age,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_over5, aes(x=SleepHrsNight, y=BPSysAve, color=Age,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_over5, aes(x=SleepHrsNight, y=BPDiaAve, color=Age,)) +geom_point()

ggplot(nhanes_adult1_SleepHrsNight_under5, aes(x=SleepHrsNight, y=BPSysAve, color=Gender,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_under5, aes(x=SleepHrsNight, y=BPDiaAve, color=Gender,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_over5, aes(x=SleepHrsNight, y=BPSysAve, color=Gender,)) +geom_point()
ggplot(nhanes_adult1_SleepHrsNight_over5, aes(x=SleepHrsNight, y=BPDiaAve, color=Gender,)) +geom_point()

#Univariate
model_under5<- lm(nhanes_adult1_SleepHrsNight_under5$BPSysAve ~ nhanes_adult1_SleepHrsNight_under5$SleepHrsNight)
summary(model)
plot(model)
model

model_over5<- lm(nhanes_adult1_SleepHrsNight_over5$BPSysAve ~ nhanes_adult1_SleepHrsNight_over5$SleepHrsNight)
summary(model)
plot(model)
model

#Multivariate
#With age and gender
boxplot(nhanes_adult1_SleepHrsNight_under5$Age)
boxplot(nhanes_adult1_SleepHrsNight_over5$Age)
table(nhanes_adult1_SleepHrsNight_under5$Gender) # Fem=366, Male=340 
table(nhanes_adult1_SleepHrsNight_over5$Gender) #Fem=2545, Male=2611

model1_under5 <- lm(nhanes_adult1_SleepHrsNight_under5$BPSysAve ~ nhanes_adult1_SleepHrsNight_under5$SleepHrsNight +nhanes_adult1_SleepHrsNight_under5$Age + nhanes_adult1_SleepHrsNight_under5$Gender)
summary(model1_under5)
plot(model1_under5)
model1_over5 <- lm(nhanes_adult1_SleepHrsNight_over5$BPSysAve ~ nhanes_adult1_SleepHrsNight_over5$SleepHrsNight +nhanes_adult1_SleepHrsNight_over5$Age + nhanes_adult1_SleepHrsNight_over5$Gender)
summary(model1_over5)
plot(model1_over5)

#With age, gender, race, physical activity, education and alcohol 
#5 hours and under 
sum(is.na(nhanes_adult1_SleepHrsNight_under5$Race1)) #0
table(nhanes_adult1_SleepHrsNight_under5$Race1)
table(nhanes_adult1_SleepHrsNight_under5$Education)
sum(is.na(nhanes_adult1_SleepHrsNight_under5$Education)) #16
hist(nhanes_adult1_SleepHrsNight_under5$AlcoholYear)
sum(is.na(nhanes_adult1_SleepHrsNight_under5$AlcoholYear)) #159
table(nhanes_adult1_SleepHrsNight_under5$PhysActive)
sum(is.na(nhanes_adult1_SleepHrsNight_under5$PhysActive)) #0

#Over 5 hours
sum(is.na(nhanes_adult1_SleepHrsNight_over5$Race1))#0
table(nhanes_adult1_SleepHrsNight_over5$Race1)
table(nhanes_adult1_SleepHrsNight_over5$Education)
sum(is.na(nhanes_adult1_SleepHrsNight_over5$Education))#201
hist(nhanes_adult1_SleepHrsNight_over5$AlcoholYear)
sum(is.na(nhanes_adult1_SleepHrsNight_over5$AlcoholYear))#962
table(nhanes_adult1_SleepHrsNight_over5$PhysActive)
sum(is.na(nhanes_adult1_SleepHrsNight_over5$PhysActive))#0

#5 hours and under
model2_under5<- lm(nhanes_adult1_SleepHrsNight_under5$BPSysAve ~ nhanes_adult1_SleepHrsNight_under5$SleepHrsNight + nhanes_adult1_SleepHrsNight_under5$Gender + nhanes_adult1_SleepHrsNight_under5$Race1 + nhanes_adult1_SleepHrsNight_under5$Education + nhanes_adult1_SleepHrsNight_under5$PhysActive)
summary(model2_under5)
plot(model2_under5)

#Over 5 hours
model2_over5<- lm(nhanes_adult1_SleepHrsNight_over5$BPSysAve ~ nhanes_adult1_SleepHrsNight_over5$SleepHrsNight + nhanes_adult1_SleepHrsNight_over5$Gender + nhanes_adult1_SleepHrsNight_over5$Race1 + nhanes_adult1_SleepHrsNight_over5$Education + nhanes_adult1_SleepHrsNight_over5$PhysActive)
summary(model2_over5)
plot(model2_over5)

#Descriptive Statistics 
#5 hours and under
mean(nhanes_adult1$BPDiaAve[nhanes_adult1$SleepHrsNight<=5], na.rm=TRUE) #70.56799
mean(nhanes_adult1$BPSysAve[nhanes_adult1$SleepHrsNight<=5], na.rm = TRUE) #120.2011

#Over 5 hours
mean(nhanes_adult1$BPDiaAve[nhanes_adult1$SleepHrsNight>5], na.rm=TRUE)#70.37258
mean(nhanes_adult1$BPSysAve[nhanes_adult1$SleepHrsNight>5], na.rm = TRUE)#119.3574

t.test(nhanes_adult1$BPDiaAve[nhanes_adult1$SleepHrsNight<=5], nhanes_adult1$BPSysAve[nhanes_adult1$SleepHrsNight>5])


install.packages("gplots")
library("gplots")
chisq <- chisq.test(nhanes_adult1)




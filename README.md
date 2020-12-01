# Hospital cost Analysis
hosp <- read.csv("hospital.csv")
head(hosp)
summary(hosp)
attach(hosp)

  AGE FEMALE LOS RACE TOTCHG APRDRG
1  17      1   2    1   2660    560
2  17      0   2    1   1689    753
3  17      1   7    1  20060    930
4  17      1   1    1    736    758
5  17      1   1    1   1194    754
6  17      0   0    1   3305    347

#1 insight
hist(AGE)
#To see the value of category of infants.
high<-as.factor(AGE)
summary(high)
#there are 307 cases in the category 0. which means infants have a highest frequency to visit the hospital.
#age category of 0 seems to be  frequently using the hospital.
tapply(TOTCHG,AGE,sum)
which.max(tapply(TOTCHG,AGE,sum))
#max expenditure also by infant of 0 age =678118, 15=111747 17=174777

#2 
Expnd<-as.factor(APRDRG)
summary(Expnd)
which.max(summary(Expnd))
tapply(TOTCHG,Expnd,sum)
which.max(tapply(TOTCHG,Expnd,sum))
max(tapply(TOTCHG,Expnd,sum))

#OR

max(tapply(TOTCHG,APRDRG,sum))
#From the results we can see that the category 640 has the maximum entries of hospitalization
#and also has the highest total hospitalization cost (437978). 

#3
#h0:The race of the patient is related to the hospitalization costs. 
#h1:no relation

linear<-as.factor(RACE)
summary(linear)

hospna<-na.omit(hosp)
modelannova<-aov(TOTCHG~RACE)
summary(modelannova)

#pvalue comes out to be very high 68% this means we can take risk and reject the null hypothesis
#this means  there is no relation between the race of patient and the hospital cost. 

#4
modelm1<-lm(TOTCHG~AGE+FEMALE)
summary(modelm1)
#pvalue for age is very less this means it is a  important factor in the hospital costs as seen by the significance levels and p-values
#gender has also less p value means it is also having the impact on cost and same with intercept


#5
modelm2<-lm(LOS~AGE+FEMALE+RACE)
summary(modelm2)
#except for the intercept.
#The very high p-value signifies that there is no linear relationship between the given variables.
#That is, with just the age, gender, and race, it is not possible to predict the los of a patient

#6
modelm3<-lm(TOTCHG~ .,data=hospna) 
summary(modelm3)
#the Pvalues isWe can see that age and length of stay affect the total hospital cost.

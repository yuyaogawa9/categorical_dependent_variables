
library("stats")
library("dplyr")
library("lmtest")

#set the working directory
#download the data for cigarrette and excel sheet 2 which contains the variables
data_cig = read_excel("Cigarette_Data.xlsx", sheet =2)
#define smoke
smoke = data_cig$dadattack
#run the linear probability model
lmp <- lm(smoke ~ black + hisp + male + age + famsize + married + divorced, 
          data = data_cig)
#get summary
summary(lmp)

#run the probit model
probit <- glm(smoke ~ black + hisp + male + age + famsize + married + divorced, 
              data = data_cig, family = binomial(link = "probit"))
#find the summary
summary(probit)
#see if there is a value greater than 1
probit$fitted.values

library(mfx)
#find the marginal effect of the probit model
#the following takes the average of all independent variables to calculate the marginal effect
probitmfx(probit, data = data_cig, atmean = T)
#the following calculate the average of all marginal effect
probitmfx(probit, data = data_cig, atmean = F)

#use the logit model
logit <- glm(smoke ~ black + hisp + male + age + famsize + married + divorced, 
             data = data_cig, family = binomial(link = "logit"))
summary(logit)
logitmfx(logit, data = data_cig, atmean = F)


#use the multinomial logit model
install.packages("nnet")
install.packages("margins")
install.packages("lmtest")
install.packages("stargazer")
library(stats)
library(dplyr)
library(lmtest)
library(nnet)
library(margins)
library(stargazer)
library(MASS)
library(erer)

#running the multinomial logit
multiNomial <- multinom(publish ~ ranked + pubbyphd + male + intlstu , data = Publication_1)
summary(multiNomial)

#running the probit function
Publication_1$publish <- as.factor(Publication_1$publish)
ordprob <- polr(publish ~ ranked + pubbyphd + male + intlstu, data = Publication_1, method = "probit")
summary(ordprob)
ME <- ocME(w = ordprob)
ME$out

#poisson regression
hunt.data = Hunting_Data_ch15
olsModel <- lm(huntDays ~ male + married + divorced + hsonly + somecol + ba + somepost +
                 postgrad + black + other + Income + southcentral, data = hunt.data)
summary(olsModel)

poissonreg = glm(huntDays ~ male + married + divorced + hsonly + somecol + ba + somepost +
                   postgrad + black + other + Income + southcentral, data = hunt.data, family = poisson())
summary(poissonreg)


#run OSL

data10 = X2018NFLSeasonWinsOnFieldStats
lmp2 <- lm(WinDummy ~ firstDdiff + PassYdif + RushYdif + AwayDummy + TODiff, 
           data = data10)
summary(lmp2)

probit2 <- glm(WinDummy ~ firstDdiff + PassYdif + RushYdif + AwayDummy + TODiff, 
               data = data10,family = binomial(link = "probit"))
summary(probit2)

library(mfx)
probitmfx(probit2, data = data10, atmean = F)

logit2 <- glm(WinDummy ~ firstDdiff + PassYdif + RushYdif + AwayDummy + TODiff, 
              data = data10,family = binomial(link = "logit"))
summary(logit2)

logitmfx(logit2, data10, atmean = F)


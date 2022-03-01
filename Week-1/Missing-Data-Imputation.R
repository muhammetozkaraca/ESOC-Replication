library(tidyverse)

wave3 <- read.table ("siswave3v4impute3.csv", header=T, sep=",")
attach(wave3) # to access the variables present in the data framework without calling the data frame
n <- nrow (wave3)


na.fix <- function (a) {
  ifelse (a<0 | a==999999, NA, a)
}

earnings <- na.fix(rearn) + na.fix(tearn)
earnings <- earnings/1000

cbind (sex, race, educ_r, r_age, earnings, police)[91:95,]


random.imp <- function (a) {
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed [missing] <- sample(a.obs, n.missing, replace = TRUE)
  return(imputed)
}

earnings.imp <- random.imp (earnings)

hist(earnings)
hist(earnings.imp)

head(earnings, 10)
head(earnings.imp, 10)

topcode <- function (a, top) {
  return(ifelse (a>top, top, a))
}

earnings.top <- topcode(earnings, 100)

hist(earnings.top[earnings>0])

colnames(wave3)

white <- ifelse (race==1, 1, 0)
white[is.na(race)] <- 0
male <- ifelse (sex==1, 1, 0)
over65 <- ifelse (r_age>65, 1, 0)
immig[is.na(immig)] <- 0
educ_r[is.na(educ_r)] <- 2.5
earners[is.na(earners)] <- 1
no_earners <- ifelse (earners==0, 1, 0)
workhrs.top <- topcode (workhrs, 40)

na.fix <- function (a) {
  ifelse (a<0 | a==999999, NA, a)
}

is.any <- function (a) {
  any.a <- ifelse (a>0, 1, 0)
  any.a[is.na(a)] <- 0
  return(any.a)
}

workmos <- workmos
earnings <- na.fix(rearn) + na.fix(tearn)
earnings.orig <- earnings
earnings[workmos==0] <- 0
retirement <- na.fix(socsec) + na.fix(pension)
interest <- na.fix(interest)
assistance <- na.fix(unemp) + na.fix(ssi) + na.fix(welfare) + na.fix(charity)
other <- na.fix(alimony) + na.fix(giftmon)

any.unemp <- is.any (unemp)
any.ssi <- is.any (ssi)
any.welfare <- is.any (welfare)
any.charity <- is.any (charity)


sis <- data.frame(cbind(
  earnings, earnings.top, male, over65, white, immig, educ_r, workmos, workhrs.top, 
  any.ssi, any.welfare, any.charity
))



lm.imp.1 <- lm(
  earnings ~ male + over65 + white + immig + educ_r + workmos + workhrs.top +
    any.ssi + any.welfare + any.charity, data = sis, subset = earnings>0)


pred.1 <- predict(lm.imp.1, sis)

impute <- function (a, a.impute) {
  ifelse (is.na(a), a.impute, a)
}

earnings.imp.1 <- impute(earnings, pred.1)

lm.imp.2.sqrt <- lm(I(sqrt(earnings.top)) ~ male + over65 + white + immig +
                        educ_r + workmos + workhrs.top + any.ssi + any.welfare +
                        any.charity, data = sis, subset = earnings > 0) 


display(lm.imp.2.sqrt)

pred.2.sqrt <- predict(lm.imp.2.sqrt, sis)
pred.2 <- topcode (pred.2.sqrt^2, 100)
earnings.imp.2 <- impute (earnings.top, pred.2)

earnings.imp.2
head(earnings.imp.2)

  
hist(earnings.imp.2[is.na(earnings)])
  

pred.4.sqrt <- rnorm (
  n, predict(lm.imp.2.sqrt, sis),
  sigma.hat (lm.imp.2.sqrt))
  
pred.4 <- topcode(pred.4.sqrt^2, 100)
earnings.imp.4 <- impute(earnings.top, pred.4)






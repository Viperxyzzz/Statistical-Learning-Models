
install.packages("readxl")
library(readxl)
data<-read_xlsx("D2SMSA.xlsx", sheet ="D2SMSA")
attach(data)
colnames(data)

### fitting the multiple regression to the data
lm.fitted<-lm(formula= crime ~ nophy + nobed + pinc + laborf)

summary(lm.fitted)

 
# Test of normality of residuals
shapiro.test(lm.fitted$residuals) 

# creating residuals vector
res <-resid(lm.fitted)
      


# Autocorrelation test
Box.test(lm.fitted$residuals,lag=1)

###################################### stop here ######################################################

#installation of the package olsrr##############
install.packages("olsrr")
library(olsrr)

#all possible regression
mod_all_poss <- lm(crime ~ populat + pcity + psen + nophy + nobed)
ols_step_all_possible(mod_all_poss)





###################

x <- dffits(model=lm.fitted)
plot(x,type='h')
dfbeta(model=lm.fitted)
y <- dfbeta(model=lm.fitted)
plot(y[,3], type = 'h')



library(car) # Here is the vif
vif(mod=lm.fitted)

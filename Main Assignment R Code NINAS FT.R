setwd("C:\\Users\\ninas\\R\\RPackages")

.libPaths('C:\\Users\\ninas\\R\\RPackages')
library('psych')
library('ggplot2')
library(sjmisc)
library('plotrix')
library(nortest)
library(sjPlot)
library('corrplot')
library('glmnet')
library(car)
library('randtests')
library('lmtest')
library('MASS')
library('rtf')
library(caret)

#Load data and data cleaning
bike_sharing <- read.csv2("C:\\Users\\ninas\\OneDrive\\Desktop\\MSc Business Analytics\\1st Quarter\\Statistics for BA 1\\main assignment\\bike_08.csv")
str(bike_sharing)
#check for NAs
sum(is.na(bike_sharing))
#data cleaning
bike_sharing <- bike_sharing[,c(-1,-2)] #remove 'x' and 'instant' variables
bike_sharing$dteday <- as.Date(bike_sharing$dteday)
bike_sharing$dteday <- as.numeric(format(bike_sharing$dteday, format = '%d'))
names(bike_sharing)[1] <- 'day'
months <- c("Jan","Feb","Mar",
            "Apr","May","Jun",
            "Jul","Aug","Sep",
            "Oct","Nov","Dec")
bike_sharing$mnth <- factor(bike_sharing$mnth, levels = c(1:12), labels = months); 
bike_sharing$season <- factor(bike_sharing$season, levels = c(1:4), labels = c('Winter','Spring','Summer','Fall'))
bike_sharing$yr <- factor(bike_sharing$yr, levels = c(0,1), labels = c('2011', '2012')); 
bike_sharing$holiday <- factor(bike_sharing$holiday, levels = c(0,1), labels = c('No', 'Yes')); 
bike_sharing$weekday <- factor(bike_sharing$weekday, levels = c(0:6), labels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
bike_sharing$workingday <- factor(bike_sharing$workingday, levels = c(0,1), labels = c('No','Yes')); 
bike_sharing$weathersit <- factor(bike_sharing$weathersit, levels = c(1:4), labels = c('Clear Weather', 'Misty-Cloudy', 'Light Conditions', 'Heavy Conditions'))
bike_sharing$hr <- factor(bike_sharing$hr, levels = c(0:23))
bike_sharing$casual <- as.numeric(bike_sharing$casual)
bike_sharing$registered <- as.numeric(bike_sharing$registered)
bike_sharing$cnt <- as.numeric(bike_sharing$cnt)
#datatypes change of variables
bike_sharing$temp <- bike_sharing$temp * 41
bike_sharing$atemp <- bike_sharing$atemp * 50
bike_sharing$hum <- bike_sharing$hum * 100
bike_sharing$windspeed <- bike_sharing$windspeed * 67
#return temperature, felt temperature, humidity and wind speed to their original unit of measure

str(bike_sharing)
summary(bike_sharing)
bike_classes <- sapply(bike_sharing,class)
bike_numerics <- bike_sharing[ , bike_classes ==('numeric')]
describe(bike_numerics)
#functions to summarize data

#visualization of numeric variables with histograms
n <- nrow(bike_sharing)
bike_names <- c('Day','Temperature', 'Feeling Temperature', 'Humidity', 'Wind speed', 'Casual Users', 'Registered Users', 'Total Users')
describe(bike_numerics)
par(mfrow = c(4,2))
for (i in 1:8){
  hist(bike_numerics[,i], main = bike_names[i], probability = TRUE, col = 'lightblue', xlab = bike_names[i])
}

factor_names <- c('Season', 'Year', 'Month', 'Hour','Holiday', 'Weekday', 'Working Day', 'Weather')
bike_factors <- bike_sharing[, bike_classes ==('factor')]
bivariate_var <- bike_factors[,c(5,7)]
#assign bivariate factors into bivariate_var and the rest of the factors into into bike_factors
par(mfrow = c(4,2))
#plot all variables inside bike_factors variable with boxplots
for (i in 1:8){
  boxplot(bike_sharing$cnt ~ bike_factors[,i], xlab = factor_names[i], main = factor_names[i]
          , col = 'lightblue', ylab = 'Total users')
  abline(lm(bike_sharing$cnt ~ as.numeric(bike_factors[,i])), col = 2, lty = 2)
}

par(mai=c(0.5,1.5,0.5,0.5))
par(mfrow = c(1,1))
#barplot of bivariate variables
barplot(sapply(bivariate_var,table)/n, horiz=T, las=1, col=2:3, ylim=c(0,8),cex.names=1.5)
legend('top', fil=2:3, legend=c('No','Yes'), ncol=2, bty='n',cex=1.5)

#create and visualize a correlation table with the linear correlation of all numeric variables
cor_table <- cor(bike_sharing[,(bike_classes != 'factor')])
cor_table <- round(cor_table,2)
index <- c(1, 2, 3,6,4,8,7,5)
cor_table <- cor_table[index, index]
par(mfrow = c(1,2))
corrplot(cor_table, method = 'number')
corrplot(cor_table, method = 'ellipse')



#Question 2 - identify a model using lasso and stepwise methods
#create a full model
full_model <- lm(cnt~.-registered-casual, data = bike_sharing)
summary(full_model)
#create a constant model
constant_model <- lm(cnt~1, data = bike_sharing)
summary(constant_model)

#conduct lasso
full_matrix <- model.matrix(full_model)[,-1]; 
lasso <- glmnet(full_matrix, bike_sharing$cnt)
par(mfrow = c(1,1))
plot(lasso, xvar = "lambda", label = T)
#Use cross validation to find a reasonable value for lambda 
lasso1 <- cv.glmnet(full_matrix, bike_sharing$cnt, alpha = 1)
coef(lasso1, s = "lambda.1se")
lasso1$lambda
lasso1$lambda.min
lasso1$lambda.1se
plot(lasso1)
coef(lasso1, s = "lambda.min")
coef(lasso1, s = "lambda.1se")
plot(lasso1$glmnet.fit, xvar = "lambda")
abline(v=log(c(lasso1$lambda.min, lasso1$lambda.1se)), lty =2)

#production of the model that was screened with a lambda 1se
lasso_model <- lm(cnt ~ season + yr + mnth + hr + holiday + weekday 
                  + weathersit + temp + hum + windspeed, data = bike_sharing)
summary(lasso_model)


#AIC Methods because we want a prediction model
model1 <- step(lasso_model, direction = 'both')
summary(model1)
model2 <- step(lasso_model, direction = 'backward')
summary(model2)
model4 <- step(constant_model, scope=list(lower=constant_model,upper=lasso_model), direction = 'both')
summary(model4)
model5 <- step(constant_model, scope=list(lower=constant_model,upper=lasso_model), direction = 'forward')
summary(model5)# we will proceed with model 5, most models produced the same model
stepwise_model <- model5


#Question 3 - check the assumptions of the model
#multi-collinearity of the x variables
round(vif(stepwise_model),1) #multi-collinearity
#fixing multi-collinearity - removal of month variable
model6 <- lm(cnt ~ season + yr + hr + holiday
             + weathersit + temp + hum + windspeed, data = bike_sharing)
summary(model6)
round(vif(model6),2)#multi-collinearity is ok!


#normality of the residuals - rejected
par(mfrow = c(1,1))
plot(model6, which = 2) 
shapiro.test(model6$residuals)
lillie.test(model6$residuals)


#Costant variance - rejected
Stud.residuals <- rstudent(model6)
yhat <- fitted(model6)
par(mfrow=c(1,3))
plot(yhat, Stud.residuals, main = 'Fitted Values vs residuals')
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2, main = 'Fitted values vs Squared Residuals')
abline(h=4, col=2, lty=2)
plot(yhat, Stud.residuals^(1/2), main = 'Fitted values vs Squared Root Residuals')
abline(h=sqrt(2), col=2, lty=2)

ncvTest(model6)

yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
leveneTest(rstudent(model6)~yhat.quantiles)
par(mfrow = c(1,1))
boxplot(rstudent(model6)~yhat.quantiles, col = 'lightblue', ylab = 'Studentized residuals'
        ,xlab = 'Fitted Values Quantiles')

#Non-linearity - rejected
residualPlot(model6, type='rstudent')
residualPlots(model6, plot=F, type = "rstudent")

#Independence - not rejected
plot(rstudent(model6), type='l', ylab = 'Studentized Residuals')
runs.test(model6$res)
durbinWatsonTest(model6)


#attempts to fix normality, linearity and homoscedasticity
#use of polynomial effects to fix non-linearity
log_model3 <- lm(log(cnt) ~  season + yr + hr + holiday
                 + weathersit + temp + hum + windspeed
                 +poly(temp,15) + poly(hum,15) + poly(windspeed,15)
                 , data = bike_sharing)

#this model fixes linearity
log_model3 <- lm(log(cnt)  ~  season + yr + hr 
                 + weathersit + log(temp) + hum 
                 + I(temp^3) + I(hum^2) 
                 , data = bike_sharing)

summary(log_model3)
residualPlot(log_model3, type='rstudent')
residualPlots(log_model3, plot=F, type = "rstudent")
#use polyonomial effect of 3rd degree to temp and 2nd degree to humidity and log to temp and cnt - linearity fixed

#use of weighted least squares transformation to fix homoscedasticity
#define weights to use
wt <- 1 / lm(abs(log_model3$residuals) ~ log_model3$fitted.values)$fitted.values^2

#perform weighted least squares regression
final_model <-  lm(log(cnt)  ~  season + yr + hr 
                 + weathersit + log(temp) + hum 
                 + I(temp^3) + I(hum^2) 
                 , data=bike_sharing, weights=wt)

#view summary of model
summary(final_model)

#Costant variance - fixed
Stud.residuals <- rstudent(final_model)
yhat <- fitted(final_model)
par(mfrow=c(1,3))
plot(yhat, Stud.residuals, main = 'Fitted Values vs residuals')
abline(h=c(-2,2), col=2, lty=2)
plot(yhat, Stud.residuals^2, main = 'Fitted values vs Squared Residuals')
abline(h=4, col=2, lty=2)
plot(yhat, Stud.residuals^(1/2), main = 'Fitted values vs Squared Root Residuals')
abline(h=sqrt(2), col=2, lty=2)

ncvTest(final_model)

yhat.quantiles<-cut(yhat, breaks=quantile(yhat, probs=seq(0,1,0.25)), dig.lab=6)
leveneTest(rstudent(final_model)~yhat.quantiles)
par(mfrow = c(1,1))
boxplot(rstudent(final_model)~yhat.quantiles, col = 'lightblue', ylab = 'Studentized residuals'
        ,xlab = 'Fitted Values Quantiles', main = 'Homoscedasticity boxplot')

residualPlot(final_model, type='rstudent', main = 'Linearity Plot')
residualPlots(final_model, plot=F, type = "rstudent")

#Independence - remains intact
plot(rstudent(final_model), type='l', ylab = 'Studentized Residuals', main = 'Independence Plot')
runs.test(final_model$res)
durbinWatsonTest(final_model)

#normality still not fixed
par(mfrow = c(1,1))
plot(final_model, which = 2) 
shapiro.test(final_model$residuals)
lillie.test(final_model$residuals)


##5 - ASSES OUT-OF-SAMPLE PREDICTION & COMPARE MODELS FROM Q2
#load test dataset
test_dataset <- read.csv2("C:\\Users\\ninas\\OneDrive\\Desktop\\MSc Business Analytics\\1st Quarter\\Statistics for BA 1\\main assignment\\bike_test.csv")

#transform data just like the initial dataset (data types, removal of variables, denormalize variables)
test_dataset <- test_dataset[,c(-1,-2)] #remove 'x' and 'instant' variables
test_dataset$dteday <- as.Date(test_dataset$dteday)
test_dataset$dteday <- as.numeric(format(test_dataset$dteday, format = '%d'))
names(test_dataset)[1] <- 'day'
months <- c("Jan","Feb","Mar",
            "Apr","May","Jun",
            "Jul","Aug","Sep",
            "Oct","Nov","Dec")
test_dataset$mnth <- factor(test_dataset$mnth, levels = c(1:12), labels = months); 
test_dataset$season <- factor(test_dataset$season, levels = c(1:4), labels = c('Winter','Spring','Summer','Fall'))
test_dataset$yr <- factor(test_dataset$yr, levels = c(0,1), labels = c('2011', '2012')); 
test_dataset$holiday <- factor(test_dataset$holiday, levels = c(0,1), labels = c('No', 'Yes')); 
test_dataset$weekday <- factor(test_dataset$weekday, levels = c(0:6), labels = c('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'))
test_dataset$workingday <- factor(test_dataset$workingday, levels = c(0,1), labels = c('No','Yes')); 
test_dataset$weathersit <- factor(test_dataset$weathersit, levels = c(1:4), labels = c('Clear Weather', 'Misty-Cloudy', 'Light Conditions', 'Heavy Conditions'))
test_dataset$hr <- factor(test_dataset$hr, levels = c(0:23))
test_dataset$casual <- as.numeric(test_dataset$casual)
test_dataset$registered <- as.numeric(test_dataset$registered)
test_dataset$cnt <- as.numeric(test_dataset$cnt)

test_dataset$temp <- test_dataset$temp * 41
test_dataset$atemp <- test_dataset$atemp * 50
test_dataset$hum <- test_dataset$hum * 100
test_dataset$windspeed <- test_dataset$windspeed * 67

#conduct predictions for each model from Q2 
lasso_model_pred <- predict(lasso_model, test_dataset)
stepwise_model_pred <- predict(model5, test_dataset)
full_model_pred <- predict(full_model, test_dataset)
constant_model_pred <- predict(constant_model, test_dataset)

#calculating the predictive performance of each model from RMSE
RMSE_lasso = RMSE(lasso_model_pred, test_dataset$cnt)
RMSE_step =  RMSE(stepwise_model_pred, test_dataset$cnt)
RMSE_full = RMSE(full_model_pred, test_dataset$cnt)
RMSE_Constant = RMSE(constant_model_pred, test_dataset$cnt)
data.frame(Full_model = RMSE_full
           ,Lasso_model =  RMSE_lasso,Stepwise_model =  RMSE_step
           ,Constant_model = RMSE_Constant)  



#6 - DESCRIPTION OF A TYPICAL DAY IN EACH SEASON
#create 4 data frames, 1 for each season from the initial dataset
Spring <- bike_sharing[which(bike_sharing$season == 'Spring'),c(-2,-3,-4,-7)]
Summer <- bike_sharing[which(bike_sharing$season == 'Summer'),c(-2,-3,-4,-7)]
Winter <- bike_sharing[which(bike_sharing$season == 'Winter'),c(-2,-3,-4,-7)]
Fall <- bike_sharing[which(bike_sharing$season == 'Fall'),c(-2,-3,-4,-7)]

#for spring
#descriptive data of spring
summary(Spring)
describe(Spring[,sapply(Spring,class) == 'numeric'])
names <- c('Hourly total users', 'Total Users vs. Holidays', 'Total Users vs. Working days', 'Users vs. Weather')
#visualizations of numeric and categorical variables for spring
par(mfrow = c(2,2))
for (i in 2:5){
  boxplot(Spring$cnt~Spring[,i], col = 'lightblue', main = paste( names[i-1],'during Spring'))
  abline(lm(Spring$cnt~as.numeric(Spring[,i])), col = 2, lty = 2)
}

Spring_num <- Spring[,sapply(Spring,class) == 'numeric']
par(mfrow = c(4,2))
for (j in 1:8){
  hist(Spring_num[,j], col = 'lightblue', main =  paste(bike_names[j], 'during Spring'))
}



#for Summer
#descriptive data of summer
summary(Summer)
describe(Summer[,sapply(Summer,class) == 'numeric'])
#visualizations of numeric and categorical variables for summer
par(mfrow = c(2,2))
for (i in 2:5){
  boxplot(Summer$cnt~Summer[,i], col = 'lightblue', main = paste( names[i-1],'during Summer'))
  abline(lm(Summer$cnt~as.numeric(Summer[,i])), col = 2, lty = 2)
}

Summer_num <- Summer[,sapply(Summer,class) == 'numeric']
par(mfrow = c(4,2))
for (j in 1:8){
  hist(Summer_num[,j], col = 'lightblue', main =  paste(bike_names[j], 'during Summer'))
}


#for Winter
#descriptive data of winter
summary(Winter)
describe(Winter[,sapply(Winter,class) == 'numeric'])
#visualizations of numeric and categorical variables for winter
par(mfrow = c(2,2))
for (i in 2:5){
  boxplot(Winter$cnt~Winter[,i], col = 'lightblue', main = paste( names[i-1],'during Winter'))
  abline(lm(Winter$cnt~as.numeric(Winter[,i])), col = 2, lty = 2)
}

Winter_num <- Winter[,sapply(Winter,class) == 'numeric']
par(mfrow = c(4,2))
for (j in 1:8){
  hist(Winter_num[,j], col = 'lightblue', main =  paste(bike_names[j], 'during Winter'))
}


#for Fall
#descriptive data of fall
summary(Fall)
describe(Fall[,sapply(Fall,class) == 'numeric'])
#visualizations of numeric and categorical variables for fall
par(mfrow = c(2,2))
for (i in 2:5){
  boxplot(Fall$cnt~Fall[,i], col = 'lightblue', main = paste( names[i-1],'during Fall'))
  abline(lm(Fall$cnt~as.numeric(Fall[,i])), col = 2, lty = 2)
}

Fall_num <- Fall[,sapply(Fall,class) == 'numeric']
par(mfrow = c(4,2))
for (j in 1:8){
  hist(Fall_num[,j], col = 'lightblue', main =  paste(bike_names[j], 'during Fall'))
}




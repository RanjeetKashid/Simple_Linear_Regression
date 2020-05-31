######### Question 1 ##########

library(readr)
calories_consumed <- read_csv(file.choose())
View(calories_consumed)
attach(calories_consumed)

install.packages("lattice")
library(lattice)

# EDA
summary(calories_consumed)
sum(is.na(calories_consumed))

install.packages("moments") 
library(moments)

skewness(`Calories Consumed`)
skewness(`Weight gained (grams)`)

hist(`Calories Consumed`)
hist(`Weight gained (grams)`)

boxplot(`Calories Consumed`,horizontal = TRUE)
boxplot(`Weight gained (grams)`)

qqnorm(`Calories Consumed`, main = "Calories Consumed")
qqline(`Calories Consumed`)
qqnorm(`Weight gained (grams)`, main = "Weight Gained")
qqline(`Weight gained (grams)`)

# Scatter plot
plot(`Calories Consumed`,`Weight gained (grams)`,col = "dodgerblue4", main = "Scatter Plot", col.main = "dodgerblue4",
     col.lab = "dodgerblue4", xlab = "Calories Consumed", ylab = "Weight Gained", pch=20)

# Correlation Coefficient(r)
cor(`Calories Consumed`,`Weight gained (grams)`)

#### Simple Linear Regression Model ####

reg_1 <- lm(`Weight gained (grams)`~`Calories Consumed`, data = calories_consumed)

summary(reg)

reg_1$coefficients
reg_1$fitted.values
reg_1$residuals

sum(reg_1$residuals)
mean(reg_1$residuals)
sqrt(mean(reg_1$fitted.values^2))

install.packages("ggplot2")
library(ggplot2)

ggplot(data = calories_consumed, aes(x = `Calories Consumed`, y = `Weight gained (grams)`)) + geom_point(color = "blue") + geom_line(color = "red", data = calories_consumed, aes(x = `Calories Consumed`, y = reg_1$fitted.values))

#### Sqrt Transformation ####

plot(`Calories Consumed`,`Weight gained (grams)`)
plot(sqrt(`Calories Consumed`),`Weight gained (grams)`)
cor(sqrt(`Calories Consumed`), `Weight gained (grams)`)
reg_1_sqrt <- lm(`Weight gained (grams)` ~ sqrt(`Calories Consumed`))
summary(reg_1_sqrt)

sum(reg_1_sqrt$residuals)
mean(reg_1_sqrt$residuals)
sqrt(mean(reg_1_sqrt$residuals^2))

#### Log Transformation ####

plot(`Calories Consumed`,`Weight gained (grams)`)
plot(log(`Calories Consumed`),`Weight gained (grams)`)
cor(log(`Calories Consumed`),`Weight gained (grams)`)
reg_1_log <- lm(`Weight gained (grams)` ~ log(`Calories Consumed`))
summary(reg_1_log)

sum(reg_1_log$residuals)
mean(reg_1_log$residuals)
sqrt(mean(reg_1_log$residuals^2))

#### Sqrt Transformation of Y variable ####

plot(`Calories Consumed`,`Weight gained (grams)`)
plot(`Calories Consumed`,sqrt(`Weight gained (grams)`))
cor(`Calories Consumed`,sqrt(`Weight gained (grams)`))
reg_1_sqrtY <- lm(sqrt(`Weight gained (grams)`) ~ `Calories Consumed`)
summary(reg_1_sqrtY)              

reg_1_sqrtY$fitted.values                  
pred_reg1_sqrtY <- reg_1_sqrtY$fitted.values^2
err_reg1_sqrtY <- `Weight gained (grams)`- pred_reg1_sqrtY
sqrt(mean(err_reg1_sqrtY^2))

ggplot(data = calories_consumed, aes(x = `Calories Consumed`, y = `Weight gained (grams)`)) + geom_point(color = "blue") + geom_line(color = "red", data = calories_consumed, aes(x = `Calories Consumed`, y = pred_reg1_sqrtY))

#### Log Transformation of Y variable ####

plot(`Calories Consumed`,`Weight gained (grams)`)
plot(`Calories Consumed`,log(`Weight gained (grams)`))
cor(`Calories Consumed`,log(`Weight gained (grams)`))
reg_1_logY <- lm(log(`Weight gained (grams)`) ~ `Calories Consumed`)
summary(reg_1_logY)

pred_reg1_logY <- exp(reg_1_logY$fitted.values)
err_reg1_logY <- `Weight gained (grams)` - pred_reg1_logY
sqrt(mean(err_reg1_logY^2))

#### Solution 1 ####
# Sqrt transformation of Y variable gives the best results, because it has the highest r value, R^2 value and lowest RMSE.

#Visualisation
ggplot(data = calories_consumed, aes(x = `Calories Consumed`, y = `Weight gained (grams)`)) + geom_point(color = "blue") + geom_line(color = "red", data = calories_consumed, aes(x = `Calories Consumed`, y = pred_reg1_sqrtY))


######### Question 2 ##########

delivery_time <- read_csv(file.choose())
View(delivery_time)
attach(delivery_time)

# EDA
summary(delivery_time)
sum(is.na(delivery_time))

skewness(`Delivery Time`)
skewness(`Sorting Time`)

hist(`Delivery Time`)
hist(`Sorting Time`)
boxplot(`Delivery Time`)
boxplot(`Sorting Time`)

qqnorm(`Delivery Time`)
qqline(`Delivery Time`)
qqnorm(`Sorting Time`)
qqline(`Sorting Time`)

# Scatter plot
plot(`Sorting Time`, `Delivery Time`, main = "Scatter Plot")

# Correlation Coefficent
cor(`Sorting Time`, `Delivery Time`)

#### Simple Linear Regression Model ####

reg_2 <- lm(`Delivery Time` ~ `Sorting Time`, data = delivery_time)
summary(reg_2)

sum(reg_2$residuals)
sqrt(mean(reg_2$residuals^2))

#### Sqrt Transformation ####

plot(`Sorting Time`, `Delivery Time`)
plot(sqrt(`Sorting Time`), `Delivery Time`)
cor(sqrt(`Sorting Time`), `Delivery Time`)
reg_2_sqrt <- lm(`Delivery Time` ~ sqrt(`Sorting Time`))
summary(reg_2_sqrt)

sqrt(mean(reg_2_sqrt$residuals^2))

#### Log Transformation ####

plot(`Sorting Time`, `Delivery Time`)
plot(log(`Sorting Time`), `Delivery Time`)
cor(log(`Sorting Time`), `Delivery Time`)
reg_2_log <- lm(`Delivery Time` ~ log(`Sorting Time`))
summary(reg_2_log)

sqrt(mean(reg_2_log$residuals^2))

#### Sqrt Transformation of Y Variable ####

plot(`Sorting Time`, `Delivery Time`)
plot(`Sorting Time`, sqrt(`Delivery Time`))
cor(`Sorting Time`, sqrt(`Delivery Time`))
reg_2_sqrtY <- lm(sqrt(`Delivery Time`) ~ `Sorting Time`)
summary(reg_2_sqrtY)

pred_reg2_sqrtY <- reg_2_sqrtY$fitted.values^2
err_reg2_sqrtY <- `Delivery Time` - pred_reg2_sqrtY
sqrt(mean(err_reg2_sqrtY^2))

#### Log Transformation of Y Variable ####

plot(`Sorting Time`, `Delivery Time`)
plot(`Sorting Time`, log(`Delivery Time`))
cor(`Sorting Time`, log(`Delivery Time`))
reg_2_logY <- lm(log(`Delivery Time`) ~ `Sorting Time`)
summary(reg_2_logY)

pred_reg2_logY <- exp(reg_2_logY$fitted.values)
err_reg2_logY <- `Delivery Time` - pred_reg2_logY
sqrt(mean(err_reg2_logY^2))

#### Polynomial Transformation (2 Degree) ####

plot(`Sorting Time`, log(`Delivery Time`))
plot(`Sorting Time`*`Sorting Time`, log(`Delivery Time`))

cor(`Sorting Time`,log(`Delivery Time`))
cor(`Sorting Time`*`Sorting Time`,log(`Delivery Time`))

reg_2_2degree <- lm(log(`Delivery Time`) ~ `Sorting Time` + I(`Sorting Time`*`Sorting Time`))
summary(reg_2_2degree)

pred_reg2_2degree <- exp(reg_2_2degree$fitted.values)
err_reg2_2degree <- `Delivery Time` - pred_reg2_2degree

sqrt(mean(err_reg2_2degree^2))

#### Polynomial Transformation (3 Degree) ####

plot(`Sorting Time`, log(`Delivery Time`))
plot(`Sorting Time`*`Sorting Time`, log(`Delivery Time`))
plot(`Sorting Time`*`Sorting Time`* `Sorting Time`, log(`Delivery Time`))

cor(`Sorting Time`,log(`Delivery Time`))
cor(`Sorting Time`*`Sorting Time`,log(`Delivery Time`))
cor(`Sorting Time`*`Sorting Time`*`Sorting Time`,log(`Delivery Time`))

reg_2_3degree <- lm(log(`Delivery Time`) ~ `Sorting Time` + I(`Sorting Time`*`Sorting Time`) + I(`Sorting Time`*`Sorting Time`*`Sorting Time`))
summary(reg_2_3degree)

pred_reg2_3degree <- exp(reg_2_3degree$fitted.values)
err_reg2_3degree <- `Delivery Time` - pred_reg2_3degree

sqrt(mean(err_reg2_3degree^2))

#### Log of Y and sqrt of X transformation ####

plot(`Sorting Time`, `Delivery Time`)
plot(sqrt(`Sorting Time`),log(`Delivery Time`))

cor(sqrt(`Sorting Time`),log(`Delivery Time`))

reg_2_logy_sqrtx <- lm(log(`Delivery Time`) ~ sqrt(`Sorting Time`))
summary(reg_2_logy_sqrtx)

pred_reg2_logy_sqrtx <- exp(reg_2_logy_sqrtx$fitted.values)
err_reg2_logy_sqrtx <- `Delivery Time` - pred_reg2_logy_sqrtx

sqrt(mean(err_reg2_logy_sqrtx^2))

#### Lof of Y and Log of X Transformation ####

plot(`Sorting Time`, `Delivery Time`)
plot(log(`Sorting Time`), log(`Delivery Time`))

cor(log(`Sorting Time`), log(`Delivery Time`))

reg_2_logy_logx <- lm(log(`Delivery Time`) ~ log(`Sorting Time`))
summary(reg_2_logy_logx)

pred_reg2_logy_logx <- exp(reg_2_logy_logx$fitted.values)
err_reg2_logy_logx <- `Delivery Time` - pred_reg2_logy_logx

sqrt(mean(err_reg2_logy_logx^2))

#### Solution 2 ####
#Sqrt Transformation of X variable

#Visualization
ggplot(data = delivery_time, aes(x = `Sorting Time`, y = `Delivery Time`)) + geom_point(color = "blue") + geom_line(color = "red", data = delivery_time, aes(x = `Sorting Time`, y = reg_2_sqrt$fitted.values))


######### Question 3 ##########

emp_data <- read_csv(file.choose())
View(emp_data)
attach(emp_data)

# EDA
summary(emp_data)
sum(is.na(emp_data))

skewness(Salary_hike)
skewness(Churn_out_rate)

hist(Salary_hike)
hist(Churn_out_rate)
boxplot(Salary_hike, horizontal = TRUE)
boxplot(Churn_out_rate, horizontal = TRUE)

qqnorm(Salary_hike, main = "Salary Hike")
qqline(Salary_hike)
qqnorm(Churn_out_rate, main = "Churn Out Rate")
qqline(Churn_out_rate)

# Scatter Plot
plot(Salary_hike, Churn_out_rate, main = "Scatter Plot")

# Correlation Coefficient
cor(Salary_hike, Churn_out_rate)

#### Simple Linear Regression Model ####

reg_3 <- lm(Churn_out_rate ~ Salary_hike, data = emp_data)
summary(reg_3)

sum(reg_3$residuals)
mean(reg_3$residuals)
sqrt(mean(reg_3$residuals^2))

#### Sqrt Transformation ####

plot(Salary_hike, Churn_out_rate)
plot(sqrt(Salary_hike), Churn_out_rate)
cor(sqrt(Salary_hike), Churn_out_rate)
reg_3_sqrt <- lm(Churn_out_rate ~ sqrt(Salary_hike))
summary(reg_3_sqrt)

sqrt(mean(reg_3_sqrt$residuals^2))

#### Log Transformation ####

plot(Salary_hike, Churn_out_rate)
plot(log(Salary_hike), Churn_out_rate)
cor(log(Salary_hike), Churn_out_rate)
reg_3_log <- lm(Churn_out_rate ~ log(Salary_hike))
summary(reg_3_log)

sqrt(mean(reg_3_log$residuals^2))

#### Sqrt Transformation of Y Variable ####

plot(Salary_hike, Churn_out_rate)
plot(Salary_hike, sqrt(Churn_out_rate))
cor(Salary_hike, sqrt(Churn_out_rate))
reg_3_sqrtY <- lm(sqrt(Churn_out_rate) ~ Salary_hike)
summary(reg_3_sqrtY)

pred_reg3_sqrtY <- reg_3_sqrtY$fitted.values^2
err_reg3_sqrtY <- Churn_out_rate - pred_reg3_sqrtY

sqrt(mean(err_reg3_sqrtY^2))

#### Log Transformation of Y variable ####

plot(Salary_hike, Churn_out_rate)
plot(Salary_hike, log(Churn_out_rate))
cor(Salary_hike, log(Churn_out_rate))
reg_3_logY <- lm(log(Churn_out_rate) ~ Salary_hike)
summary(reg_3_logY)

pred_reg3_logY <- exp(reg_3_logY$fitted.values)
err_reg3_logY <- Churn_out_rate - pred_reg3_logY

sqrt(mean(err_reg3_logY^2))

#### Solution 3 ####
# Log Transformation of Y variable

# Visualisation 
ggplot(data = emp_data, aes(x = Salary_hike, y = Churn_out_rate)) + geom_point(color = "blue") + geom_line(color = "red", data = emp_data, aes(x = Salary_hike, y = pred_reg3_logY))


######### Question 4 ##########

Salary_Data <- read_csv(file.choose())
View(Salary_Data)
attach(Salary_Data)

# EDA
summary(Salary_Data)
sum(is.na(Salary_Data))

skewness(YearsExperience)
skewness(Salary)

hist(YearsExperience)
hist(Salary)
boxplot(YearsExperience, horizontal = TRUE)
boxplot(Salary, horizontal = TRUE)

qqnorm(YearsExperience)
qqline(YearsExperience)
qqnorm(Salary)
qqline(Salary)

# Scatter Plot
plot(YearsExperience,Salary, main = "Scatter Plot")

# Correlation Coefficient
cor(YearsExperience,Salary)

#### Simple Linear Regression Model ####

reg_4 <- lm(Salary ~ YearsExperience)
summary(reg_4)

sum(reg_4$residuals)
mean(reg_4$residuals)
sqrt(mean(reg_4$residuals^2))

#### Sqrt Transformation ####

plot(YearsExperience,Salary)
plot(sqrt(YearsExperience), Salary)
cor(sqrt(YearsExperience), Salary)
reg_4_sqrt <- lm(Salary ~ sqrt(YearsExperience))
summary(reg_4_sqrt)

sqrt(mean(reg_4_sqrt$residuals^2))

#### Log Transformation ####

plot(YearsExperience,Salary)
plot(log(YearsExperience),Salary)
cor(log(YearsExperience),Salary)
reg_4_log <- lm(Salary ~ log(YearsExperience))
summary(reg_4_log)

sqrt(mean(reg_4_log$residuals^2))

#### Sqrt Transformation of Y Variable ####

plot(YearsExperience,Salary)
plot(YearsExperience,sqrt(Salary))
cor(YearsExperience,sqrt(Salary))
reg_4_sqrtY <- lm(sqrt(Salary) ~ YearsExperience)
summary(reg_4_sqrtY)

pred_reg4_sqrtY <- reg_4_sqrtY$fitted.values^2
err_reg4_sqrtY <- Salary - pred_reg4_sqrtY

sqrt(mean(err_reg4_sqrtY^2))

#### Log Transformation of Y Variable ####

plot(YearsExperience,Salary)
plot(YearsExperience,log(Salary))
cor(YearsExperience,log(Salary))
reg_4_logY <- lm(log(Salary) ~ YearsExperience)
summary(reg_4_logY)

pred_reg4_logY <- exp(reg_4_logY$fitted.values)
err_reg4_logY <- Salary - pred_reg4_logY

sqrt(mean(err_reg4_logY^2))

#### Solution 4 ####
# Simple Linear Regression Model

#Visualisation
ggplot(data = Salary_Data, aes(x = YearsExperience, y = Salary)) + geom_point(color = "blue") + geom_line(color = "red", data = Salary_Data, aes(x = YearsExperience, y = reg_4$fitted.values))

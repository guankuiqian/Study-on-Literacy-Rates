#########################
#         HW 3          # 
#########################
graphics.off()
rm(list=ls())

# SDGB 7840: Explanatory Models
# Sumi Choudhury

# read individual csv files with variables data
data_L <- read.csv("Adult_Literacy_Rate.csv", header = F, sep=",", stringsAsFactors = F)
data_GG <- read.csv("GDP_Growth_Annual%.csv", header = F, sep=",", stringsAsFactors = F)
data_GC <- read.csv("GDP_Per_Capita.csv", header = F, sep=",", stringsAsFactors = F)
data_IM <- read.csv("Immunization_DPT_%Children_12-23months.csv", header = F, sep=",", stringsAsFactors = F)
data_IS <- read.csv("Improved_Sanitation_Facilities.csv", header = F, sep=",", stringsAsFactors = F)
data_IW <- read.csv("Improved_Water_Source.csv", header = F, sep=",", stringsAsFactors = F)
data_IR <- read.csv("Infant_Mortality_Rate.csv", header = F, sep=",", stringsAsFactors = F)
data_IU <- read.csv("Internet_Users.csv", header = F, sep=",", stringsAsFactors = F)
data_PG <- read.csv("Population_Growth_Annual%.csv", header = F, sep=",", stringsAsFactors = F)
data_PH <- read.csv("Prevalence_of_HIV_%Total_ages15-49.csv", header = F, sep=",", stringsAsFactors = F)
data_UP <- read.csv("Urban_Population_%Total.csv", header = F, sep=",", stringsAsFactors = F)

View(data_L)

# combine all variables into one dataframe
data_all <- cbind((data_L), (data_GG[, 60]), (data_GC[, 60]), (data_IM[, 60]),
                  (data_IS[, 60]), (data_IW[, 60]), (data_IR[, 60]), (data_IU[, 60]),
                  (data_PG[, 60]), (data_PH[, 60]), (data_UP[, 60]))

View(data_all)

# select rows and columns for data set
data_rc <- data_all[(6:269),c(1,60,62:71)]
View(data_rc)

# rename column names
colnames(data_rc) <- c("Countries", "Adult_Literacy", "GDP_Growth_Annual",
                       "GDP_Per_Capita", "Immunization_DPT_Children_12_23_mo",
                       "Improved_Sanitation", "Improved_Water", "Infant_Mortality_Rate",
                       "Internet_Users", "Population_Growth_Annual", "Prevalence_of_HIV_Total_15_49",
                       "Urban_Population_Total")
View(data_rc)

# remove incomplete records
data_2015 <- na.omit(data_rc)
View(data_2015)

# view countries
countries <- data_2015[,1]
countries

# create a correlation matrix and export the file to excel
my_table <- cor(data_2015[sapply(data_2015, is.numeric)])
my_table
write.csv(my_table, "Correlation_Matrix.csv")

# create a scatterplot matrix
pairs(data_2015[sapply(data_2015, is.numeric)], las=TRUE, pch=20)

install.packages("psych")
library(psych)

# create a pairs panel with scatterplot and histograms combined
pairs.panels(data_2015[,(2:12)], density = FALSE, smooth = FALSE, hist.col = "royalblue1")

# use for loop to generate histograms for all the variables
par(mfrow=c(3,4))
colnames <- dimnames(data_2015)[[2]]
for (i in 2:12) {
  hist(data_2015[,i], main=colnames[i], probability=TRUE, col="royalblue1", border="white")
}

# create a dataframe to include just the variables to be used in the linear model;
# leave out the 'country' column
data_model <- data_2015[, (2:12)]

# create summary statistics for data set and export to excel
summary_table <- summary(data_model)
summary_table
write.csv(summary_table, "Summary_Statistics.csv")

# based on the histograms, we need to transform the variables
# contains negative values that must be transformed using a constant of 1 - min(x)
data_model['t_Adult_Literacy'] <- log1p(data_model['Adult_Literacy'])
data_model['t_GDP_Growth_Annual'] <- log1p(data_model['GDP_Growth_Annual'] - min(data_model['GDP_Growth_Annual']))
data_model['t_GDP_Per_Capita'] <- log1p(data_model['GDP_Per_Capita'])
data_model['t_Immunization_DPT_Children_12_23_mo'] <- log1p(data_model['Immunization_DPT_Children_12_23_mo'])
data_model['t_Improved_Sanitation'] <- log1p(data_model['Improved_Sanitation'])
data_model['t_Improved_Water'] <- log1p(data_model['Improved_Water'])
data_model['t_Infant_Mortality_Rate'] <- log1p(data_model['Infant_Mortality_Rate'])
data_model['t_Internet_Users'] <- log1p(data_model['Internet_Users'])
data_model['t_Population_Growth_Annual'] <- log1p(data_model['Population_Growth_Annual'] - min(data_model['Population_Growth_Annual']))
data_model['t_Prevalence_of_HIV_Total_15_49'] <- log1p(data_model['Prevalence_of_HIV_Total_15_49'])
data_model['t_Urban_Population_Total'] <- log1p(data_model['Urban_Population_Total'])

View(data_model)

# use for loop to generate histograms for all transformed variables
par(mfrow=c(3,4))
colnames <- dimnames(data_model)[[2]]
for (i in 12:22) {
  hist(data_model[,i], main=colnames[i], probability=TRUE, col="royalblue1", border="white")
}

# select the new columns for the model
data.log <- data.frame(data_model[, (12:22)])
View(data.log)

# create a correlation matrix for logged variables and export the file to excel
my_table2 <- cor(data.log[sapply(data.log, is.numeric)])
my_table2
write.csv(my_table2, "Correlation_Matrix_Transformed.csv")

# summary statistics for log transformed variables
summary(data.log)

# create linear model for full data set; notice that sign of the coefficent
# for improved water has changed compaired to pairwise correlation matrix
full <- lm(t_Adult_Literacy ~., data = data.log)
summary(full)

install.packages("usdm")

library(usdm)

# compute variance inflation factors; exclude response variable
vif(data.log[,c("t_GDP_Growth_Annual","t_GDP_Per_Capita", "t_Immunization_DPT_Children_12_23_mo",
                "t_Improved_Sanitation", "t_Improved_Water", "t_Infant_Mortality_Rate",
                "t_Internet_Users", "t_Population_Growth_Annual", "t_Prevalence_of_HIV_Total_15_49",
                "t_Urban_Population_Total")])

# since VIF's are all within acceptable range, create model with only variables
# of significance; notice improved water still has a negative sign
# prevalence of HIV also has a negative sign
model.1 <- lm(t_Adult_Literacy ~ t_Immunization_DPT_Children_12_23_mo + 
                t_Improved_Sanitation + t_Improved_Water + t_Infant_Mortality_Rate + 
                t_Prevalence_of_HIV_Total_15_49, data = data.log)
summary(model.1)

# create model to remove improved water and prevalence of HIV
model.2 <- lm(t_Adult_Literacy ~ t_Immunization_DPT_Children_12_23_mo + 
                t_Improved_Sanitation + t_Infant_Mortality_Rate, data = data.log)
summary(model.2)

# since Immunization DPT is now insignificant, remove it from the model
model.3 <- lm(t_Adult_Literacy ~ t_Improved_Sanitation + t_Infant_Mortality_Rate, data = data.log)
summary(model.3)


# check the assumptions
# residuals vs. fitted Adult Literacy Rate (y-hat), residuals vs. predictor variables
par(mfrow=c(2,2))

plot(model.3$fitted, model.3$residuals, las=TRUE, 
     main="(B) Residuals vs. Fitted Values", 
     xlab="Fitted Values (%)", ylab="Residuals (%)")
abline(h=0, col = "gray50", lty = 2)

plot(data.log$t_Improved_Sanitation, model.3$residuals, las=TRUE, 
     main="(B) Residuals vs. Improved Sanitation (%)", 
     xlab="Improved Sanitation (%)", ylab="Residuals (%)")
abline(h=0, col = "gray50", lty = 2)

plot(data.log$t_Infant_Mortality_Rate, model.3$residuals, las=TRUE, 
     main="(B) Residuals vs. Infant Mortality Rate (%)", 
     xlab="Infant Mortality Rate (%)", ylab="Residuals (%)")
abline(h=0, col = "gray50", lty = 2)

plot(data.log$t_Prevalence_of_HIV_Total_15_49, model.3$residuals, las=TRUE, 
     main="(B) Residuals vs. Prevalence of HIV (%)", 
     xlab="Prevalence of HIV (%)", ylab="Residuals (%)")
abline(h=0, col = "gray50", lty = 2)

# plot the residuals, normal qq and cook's distance graphs
plot(model.3)

install.packages("lmtest")
library(lmtest)

# durbin watson d-statistic; note since p-value is not significant, we will
# fail to reject the null hypothesis that rho = 0; there is zero autocrorrelation
# of the residuals

dwtest(model.3)

# train and test our model (50/50)

indexes <- sample(1:nrow(data.log), size=0.5*nrow(data.log))
test <- data.log[indexes,]
train <- data.log[-indexes,]

View(train)
View(test)

fit <- lm(t_Adult_Literacy ~ t_Improved_Sanitation + t_Infant_Mortality_Rate, 
          data = train)

summary(fit)

test.pred <- predict(fit,newdata=test)
test.y    <- test$t_Adult_Literacy

SS.test.total      <- sum((test.y - mean(train$t_Adult_Literacy))^2)
SS.test.residual   <- sum((test.y - test.pred)^2)
SS.test.regression <- sum((test.pred - mean(train$t_Adult_Literacy))^2)

test.rsq <- 1 - SS.test.residual/SS.test.total  
test.rsq


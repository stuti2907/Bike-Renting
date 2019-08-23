rm (list = ls()) # Cleaing the evironment
setwd('C:/Users/HP/Desktop/edwisor/Project 2') #setting directory
getwd()

#Reading the data
data = read.csv('day.csv') 

#knowing structure of our data
str(data) #We have 731 obs. of 16 variables

head(data)

#Check summary of dataset
summary(data)

#Get the column names
names(data)
length(names(data)) 

#Get number of unique values column wise

cnames = names(data)
for (i in cnames) {
  print(i)
  print(length(unique(data[,i])))
} 


############################1. DATA PRE-POCESSING########################
#########################################################################


#Changing the data types of categorical variables
data$season= factor(data$season)
data$yr = factor(data$yr)
data$mnth = factor(data$mnth)
data$holiday = factor(data$holiday)
data$weekday = factor(data$weekday)
data$workingday = factor(data$workingday)
data$weathersit = factor(data$weathersit)

############################2. VISUALIZATION #########################
######################################################################
install.packages("ggplot2")
install.packages("scales")
library(ggplot2)
library(scales)

#A)Univariate
#variable quantity

d = density(data$cnt)
plot(d, main="distribution of cnt") 

#B)bi-variant 
#B.1)categorical variables vs target variable
plot(cnt ~ season , data = data, main = 'season')# we see least rentals are in season 1 and most in season 3
plot(cnt ~ yr, data= data, main = 'yr')# #we see rental are high in 2012, this tells the rental is increasing with time
plot(cnt ~ mnth, data= data, main = 'mnth')#we see rental high from march to oct
plot(cnt ~ holiday, data= data, main = 'holiday')#we see rental high in weekdays
plot(cnt ~ weekday , data = data, main = 'weekday')#not much difference 
ggplot(data , aes_string(x=data$workingday)) + geom_bar(stat = "count", fill = "DarkslateBlue") + 
  xlab("Working day") + ylab("Count") + ggtitle("Working day distribution") + theme(text = element_text(size = 15))
#bikes are rented more on working days
plot(cnt ~ weathersit , data = data, main = 'weathersit')#we see rental are high with clear weather and low with rainy

#B.2)continuous variables vs target variable 
plot1 = lm(cnt ~ temp, data = data)
with(data ,plot(temp, cnt, main = 'temp'))
abline(plot1) #rental counts increase with increase in temperature

plot2 = lm(cnt ~ hum, data = data)
with(data ,plot(hum, cnt, main = 'hum'))
abline(plot2) #rental count decrease with increase in humidity

plot3 = lm(cnt ~ windspeed , data = data)
with (data, plot(windspeed, cnt, main = 'windspeed'))
abline(plot3) #rental count decrease with increase in windspeed

plot4 = lm(cnt ~ casual, data = data)
with(data ,plot(casual,cnt,main = 'casual'))
abline(plot4) 

plot5 = lm(cnt ~ registered , data = data)
with (data, plot(registered, cnt, main = 'registered'))
abline(plot5)

############################3. MISSING VALUE ANALYSIS########################
#############################################################################
#Creating dataframe with missing values present in each variable
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
#Calculating percentage missing value
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(data)) * 100 
missing_val

############################4. OUTLIER ANALYSIS #########################
#########################################################################
#let us first check outliers 

numeric_index = sapply(data, is.numeric) # creating numerical value index
numeric_data = data[,numeric_index] # storing numeric data
cnames = colnames(numeric_data) #storing numeric data column names

#Creating box-plot to analyze outliers
for (i in 1:length(cnames)){
  assign(paste0("gn", i), ggplot(aes_string(y = cnames[i], x = "cnt"), data = subset(data)) +
           stat_boxplot(geom = "errorbar", width = 0.5) + 
           geom_boxplot(outlier.colour = "red", fill = "blue", outlier.shape = 18, outlier.size = 1, notch = FALSE) + 
           theme(legend.position = "bottom") + labs(y = cnames[i], x="count") + ggtitle(paste("Boxplot of count for", cnames[i])))
}
gridExtra::grid.arrange(gn2, gn3, gn4,gn5, gn6, gn7, ncol = 3, nrow = 3) # excludif gn1 as that is unique for each observation

#replace outliers with NA 
for(i in cnames) {
  print(i)
  val = data[,i][data[,i] %in% boxplot.stats(data[,i]) $out]
  
  print(length(val))
  data[,i][data[,i] %in% val] = NA
}

#imputing NA values
data$hum[is.na(data$hum)] = mean(data$hum,na.rm = T) #closest value was from mean
data$windspeed[is.na(data$windspeed)] = median(data$windspeed, na.rm = T)#closest value was from median
data$casual[is.na(data$casual)] = data$cnt - data$registered # as cnt = casual + registered

boxplot(data[,c( 'casual')])


#since outliers were not going even after computation also,I preferred to go for deleting the outlier records for casual variable
#Select and Remove the outliers

###Remove all the rows which contains outliers
val = data[,'casual'][data[,'casual'] %in% boxplot.stats(data[,'casual']) $out]
print(length(val))

data = data[which(!data[,'casual'] %in% val),]

##44 rows got deleted

dim(data) # 716, 16

#Confirm again if any missing value exists
sum(is.na(data))

#Confirm again if any outlier exists

for (i in cnames) {
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
}

length(val)

#Till here we have nor any missing value neither any outliers.
# creating copy of imputed data 
copy1 = data

############################5. FEATURE SELECTION #########################
##########################################################################

#now we have to consider on FEATURE SELECTION
#Check for multicollinearity using corelation graph
library(corrgram)

#A. Correlation check on continuous variable
corrgram(data[,cnames], order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "correlation plot")

#B. Anova test on categorical variable 
#creating subset randomly for anova 
data = subset(data, select=-c(instant, atemp, casual, registered, dteday))

anova_test=aov(cnt~season + yr + mnth + holiday + weekday + workingday + weathersit, data = data)
summary(anova_test)

data = subset(data, select=-c(holiday))
head(data) 
write.csv(data,"Clean_bike_data_R.csv", row.names = F)

############################6. FEATURE SCALING #########################
##########################################################################


hist(data$temp)
hist(data$hum)
hist(data$windspeed)
hist(data$cnt)

############################MODELING########################
############################################################

#Sampling
set.seed(101)
train_index = sample(1:nrow(data), 0.8*nrow(data))
data_train = data[train_index,] 
data_test = data[-train_index,]

#Function to calculate MAPE
mape = function(actual, predict){
  mean(abs((actual-predict)/actual))*100
}

##1. *******************Decision tree*************************
#*************************************************************
install.packages("rpart.plot")
library(rpart.plot)
library(rpart)

#Model
set.seed(101)
model_DT = rpart(cnt~. , data = data_train, method = "anova")
summary(model_DT)
plt = rpart.plot(model_DT, type = 5, digits = 2, fallen.leaves = TRUE)
head(model_DT)
head(data_test)


#Predictions
DT_Predict = predict(model_DT, data_test[,-10])

#Plot a graph for actual vs predicted values
plot(data_test$cnt,type="l",lty=2,col="green")
lines(DT_Predict,col="blue")

#Evaluation statistics
install.packages("caret")
library(caret)
postResample(DT_Predict, data_test$cnt)#R-sq = 0.7506466
mape(data_test$cnt, DT_Predict) #193.0598

plot(data_test$cnt, DT_Predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'DT model')

##2. *******************Random forest*************************
#*************************************************************

install.packages("randomForest")
library(randomForest)
library(inTrees)

#Model
set.seed(101)
model_RF = randomForest(cnt ~. , data_train, importance = TRUE, ntree = 500)
model_RF

#Error plotting
plot(model_RF) #my error i decreasing with higher number of trees

#Predict test data using RF model
RF_Predict = predict(model_RF, data_test[,-10])
#Plot a graph for actual vs predicted values
plot(data_test$cnt,type="l",lty=2,col="green")
lines(RF_Predict,col="blue")

#Evaluation statistics
postResample(RF_Predict, data_test$cnt)#R-sq = 0.8623323
mape(data_test$cnt, RF_Predict) #142.8853

varImpPlot(model_RF) #Check the importance of variables in our RT model 

plot(data_test$cnt, RF_Predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'RF model')

##3. *******************Linear regression*************************
#*****************************************************************

#Model
set.seed(101)
model_LR = lm(cnt ~. , data = data_train)
summary(model_LR)

#Predictions
LR_Predict = predict(model_LR, data_test[,-10])
#Plot a graph for actual vs predicted values
plot(data_test$cnt,type="l",lty=2,col="green")
lines(LR_Predict,col="blue") 


#Evaluation statistics
postResample(LR_Predict, data_test$cnt)#R-sq = 0.819
mape(data_test$cnt, LR_Predict) #143.731
plot(data_test$cnt, LR_Predict, xlab = 'Actual values', ylab = 'Predicted values', main = 'LR model') 


#As per the above calculation Random forest is best suite for our Prediction.



                ###################################################
                #     Time Series Analysis of Cocacola sales      #
                ###################################################

plastics_sales <- read.csv("D:\\Data Science\\assignment\\PlasticSales.csv",header = T)
View(plastics_sales)

#Library use for Forecasting
library(fpp2)
library(forecast)
library(ggplot2)

plastics_ts <- ts(plastics_sales$Sales,frequency = 12,start = c(1949,1))
print(plastics_ts)

                          ################################
                          #     Preliminary Analysis     #
                          ################################

#Time plot
autoplot(plastics_ts,color="blue")+
  ggtitle("Time-Plot:plastic Sales Trend on different years")+
  ylab(" Sales Figure ")

#Data Has a positive trend it shows that the sales figures increasing continuously during
#different years,to analyze further deep into the time series we need to remove trend
Diff_ts <- diff(plastics_ts)

#Time plot of differentiate data
autoplot(Diff_ts,color="blue")+
  ggtitle("Time-Plot:change in the sales figures of plastics Across The years")+
  ylab("Sales Figures")

#now as we make our trend stationary we will look into our seasonality

ggseasonplot(Diff_ts)

#From the Seasonal plot we can say that there is a sudden peak in the sales of cocacola on
#2nd quarter and drop in the sales data on quarter 1 and quarter 4
#so there is a seasonal drop and peak in the sales figure of cocacola.

#Lets look into the seasonal subseries plot for more clear understanding about the pattern.

ggsubseriesplot(Diff_ts)

#From the above plot we can say that there is both trend and seasonality in our data
#set.As we had done analysis on our time series now we will build a model on it and 
#according to the accuracy we select our model.we use two Analysis techniques :-
#1.Model Based Forecasting Analysis
#2.Data driven Forecasting Analysis.

                            #########################
                            #   Data Preperation    #
                            #########################

#As we are dealing with the months of the year so we need to create dummy variable for 12 months.

Month <- data.frame(outer(rep(month.abb,length=60),month.abb,"==")+0)
View(Month)

#Assign Colum names for better understanding.
colnames(Month) <- month.abb
View(Month)

#Combine the airlines data with the month data 
plastics_sales2 <- data.frame(plastics_sales,Month)
View(plastics_sales2)

#We have to build few columns into the plastics_sales2 for calculating different models.
plastics_sales2["t"] <-1:60 
View(plastics_sales2)
head(plastics_sales2)

plastics_sales2["t_squared"] <- plastics_sales2["t"]*plastics_sales2["t"]
plastics_sales2["log_Sales"] <- log(plastics_sales2["Sales"])
View(plastics_sales2)
head(plastics_sales2)


               ###################################################
               #   Data Partition into training and testing set  #
               ###################################################

#Training Set 
train_1 <- plastics_sales2[1:48,]
View(train_1)
head(train_1)

train_2 <- plastics_sales[1:48,] #Select data from 1949 to 1952 into training set
View(train_2)
head(train_2)

train_ts <- ts(train_2$Sales,frequency = 12,start = c(1949,1)) #transform into time series
print(train_ts)

#Testing Set
test_1 <- plastics_sales2[49:60,]
View(test_1)
head(test_1)

test_2 <- plastics_sales[49:60,] #Select data from 1953 into testing set
head(test_2)

test_ts <- ts(test_2$Sales,frequency = 12,start = c(1953,1))#Transform into time series
print(test_ts)

#Create a function to calculate Mean Absolute Percentage Error
mape <- function(actual,pred){
  mape <- mean(abs((actual-pred)/actual))*100
  return(mape)
}

#######################################################################################
# From the Visualization we can conclude that there is both additive and multiplicative 
# seasonality in the data set because the mean is constant till 1992 and there is a 
#increase in trend in 1993 so will try to build both multiplicative seasonality model
#and Additive seasonality model and check for accuracy.
#######################################################################################


                  ##############################################
                  #      Model Based Forecasting Analysis      #
                  ##############################################
attach(train_1)

########### Multiplicative Seasonality #############

Multi_Season <- lm(log_Sales ~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_1)
summary(Multi_Season)


#Comput the model on test data set and predict the value 
Multi_Season_pred <- data.frame(predict(Multi_Season,newdata = test_1,interval = "predict"))
Multi_Season_pred

#As our prediction contains logrithmic function within it we need to transform it
Multi_Season_pred1 <- exp(Multi_Season_pred$fit)
Multi_Season_pred1

#Calculate Mean Absolute Percentage Error
mape(test_1$Sales,Multi_Season_pred1) #MAPE=15.91


############# Multiplicative Seasonality with linear data ###########
Multi_Season_linear <- lm(log_Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_1)
summary(Multi_Season_linear)

#Comput the model on test data set and predict the value 
Multi_Season_linear_pred <- data.frame(predict(Multi_Season_linear,newdata = test_1,interval = "predict"))
Multi_Season_linear_pred

#As our prediction contains logrithmic function within it we need to transform it
Multi_Season_pred2 <- exp(Multi_Season_linear_pred$fit)
Multi_Season_pred2

#Calculate Mean Absolute Percentage Error
mape(test_1$Sales,Multi_Season_pred2) #MAPE=6.83

############## Additive Seasonality ################

Add_season <- lm(train_1$Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_1)
summary(Add_season)


#Comput the model on test data set and predict the value 
Add_season_pred <- data.frame(predict(Add_season,newdata = test_1,interval = "predict"))
Add_season_pred

#Calculate Mean Absolute Percentage Error
mape(test_1$Sales,Add_season_pred$fit) #MAPE=40.98

############## Additive Seasonality with linear model ################

Add_season_linear <- lm(train_1$Sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_1)
summary(Add_season_linear)


#Comput the model on test data set and predict the value 
Add_season_linear_pred <- data.frame(predict(Add_season_linear,newdata = test_1,interval = "predict"))
Add_season_linear_pred

#Calculate Mean Absolute Percentage Error
mape(test_1$Sales,Add_season_linear_pred$fit) #MAPE=8.23


############## Additive Seasonality with quadratic model ################

Add_season_quad <- lm(train_1$Sales~t_squared+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data = train_1)
summary(Add_season_quad)


#Comput the model on test data set and predict the value 
Add_season_quad_pred <- data.frame(predict(Add_season_quad,newdata = test_1,interval = "predict"))
Add_season_quad_pred

#Calculate Mean Absolute Percentage Error
mape(test_1$Sales,Add_season_quad_pred$fit) #MAPE=14.51


          ##########################################
          ##    Data Driven forecasting Analysis  ##
          ##########################################

############## Naive Forecasting Method #################

naive_model <- snaive(train_ts,h=12)
naive_model
summary(naive_model)

plot(naive_model)

#For calculating the mape we need the point forecast of naive model and compare it with test data.
naive_df <- data.frame(naive_model)
naive_pred <- naive_df$Point.Forecast

mape(test_1$Sales,naive_pred)
#Mean Absolute Percentage Error=12.86


###############  Holt's Trend Method  ###################

#Build a model for alpha=0.2 and beta=0.2
holt_model <- holt(train_ts,h=12,alpha = 0.2,beta = 0.2)
summary(holt_model)

plot(holt_model)

#Transform the above model into the data frame.   
holt_df <- data.frame(holt_model)
holt_pred<- holt_df$Point.Forecast
#Checking for accuracy of the model.
mape(test_1$Sales,holt_pred) #MAPE=27.97


#Build a model for alpha=0.3 and beta=0.3
holt_model_1 <- holt(train_ts,h=12,alpha = 0.3,beta = 0.3)
summary(holt_model_1)

plot(holt_model_1)

#Transform the above model into the data frame.   
holt_df1 <- data.frame(holt_model_1)
holt_pred1<- holt_df1$Point.Forecast
#Checking for accuracy of the model.
mape(test_1$Sales,holt_pred1) #MAPE=108.04
#The mape goes up so we have to try for leeser value alpha and beta

#Build a model for alpha=0.1 and beta=0.1
holt_model_2 <- holt(train_ts,h=12,alpha = 0.1,beta = 0.1)
summary(holt_model_2)

plot(holt_model_2)

#Transform the above model into the data frame.   
holt_df2 <- data.frame(holt_model_2)
holt_pred2 <- holt_df2$Point.Forecast
#Checking for accuracy of the model.
mape(test_1$Sales,holt_pred2) #MAPE=61.09


#Build a model without a optimum value for level and trend
holt_model_3 <- holt(train_ts,h=12)
summary(holt_model_3)

plot(holt_model_3)

#Transform the above model into the data frame.   
holt_df3 <- data.frame(holt_model_3)
holt_pred3 <- holt_df3$Point.Forecast
#Checking for accuracy of the model.
mape(test_1$Sales,holt_pred3) #MAPE=102.72

###############  Holt winter exponential method  ###################

#Build a model for alpha=0.1,beta=0.1 and gamma=0.1

holt_winter_model <- HoltWinters(train_ts,alpha = 0.1,beta = 0.1,gamma = 0.1)
holt_winter_model
summary(holt_winter_model)


plot(holt_winter_model)

#Lets predict the passenger count for next 12 month and transform it into data frame.
holt_winter_pred <- predict(holt_winter_model,n.ahead = 12)
holt_winter_pred<-data.frame(holt_winter_pred)
holt_winter_pred
#Mean Absolute Percentage Error
mape(test_1$Sales,holt_winter_pred$fit)#MAPE=9.32


#Build a model for alpha=0.1,beta=0.1 and gamma=0.05

holt_winter_model2 <- HoltWinters(train_ts,alpha = 0.1,beta = 0.1,gamma = 0.05)
summary(holt_winter_model2)


plot(holt_winter_model2)

#Lets predict the passenger count for next 12 month and transform it into data frame.
holt_winter_pred2 <- predict(holt_winter_model2,n.ahead = 12)
holt_winter_pred2 <- data.frame(holt_winter_pred2)
holt_winter_pred2
#Mean Absolute Percentage Error
mape(test_1$Sales,holt_winter_pred2$fit)#MAPE=9.32

#Lets Build our model without assigning any optimum value for alpha,beta and gamma.

holt_winter_model3 <- HoltWinters(train_ts)
holt_winter_model3
summary(holt_winter_model3)


plot(holt_winter_model3)

#Lets predict the passenger count for next 12 month and transform it into data frame.
holt_winter_pred3 <- predict(holt_winter_model3,n.ahead = 12)
holt_winter_pred3 <- data.frame(holt_winter_pred3)
holt_winter_pred3
#Mean Absolute Percentage Error
mape(test_1$Sales,holt_winter_pred3$fit)#MAPE=11.66

################################################################################
#We got our least value for mape from the multiplicative seasonality with linear data
#model,so that model will provide us with the least error forecasted values
################################################################################






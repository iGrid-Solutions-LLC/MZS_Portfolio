library(tidyverse)
library(openxlsx)
library(ggcorrplot)
library(RSQLite)
library(DT)

data <-read_csv("dionaeaClean2.csv")
data2<-read_csv("C:/Users/12567/Desktop/R/Interviews/diamonds.csv")
data2

sapply(data2,unclass)

colnames(data2)

data.matrix(data2)

#rename x,y,z columns in diamond dataset
diamond<-data2%>%
  rename("length.mm"=x,
         "width.mm"=y,
         "depth.mm"=z)

#Giving categorical variables numeric distinctions
# diamond%>%
#   distinct(cut)
# 
# diamond$cutindex=1
# diamond[diamond$cut=="Premium",]$cutindex=2
# diamond[diamond$cut=="Good",]$cutindex=3
# diamond[diamond$cut=="Very Good",]$cutindex=4
# diamond[diamond$cut=="Fair",]$cutindex=5
# 
# diamond%>%
#   distinct(color)
# 


diamond_features<-diamond%>%
  select(-1)

#turning data into matrix to do some analysis
matrix_d<-data.matrix(diamond_features)

corr<-round(cor(matrix_d),
      digits = 2 # rounded to 2 decimals
)

ggcorrplot(corr,
           lab = TRUE,
           type = "upper")

colnames(diamond)

d_model<-lm(price~length.mm+width.mm+depth.mm,data=diamond)
summary(d_model)

layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(d_model)

#Create a database from diamonds data----

#Load Data
diamonds<-read.csv("Diamonds Prices2022.csv")

#Create separate tables for clarity, cut and color
clarity<-diamonds%>%
  distinct(clarity)%>%
  arrange(clarity)%>%
  mutate(id=paste0("c",row_number()))

color<-diamonds%>%
  distinct(color)%>%
  arrange(color)%>%
  mutate(id=paste0("cl",row_number()))

cut<-diamonds%>%
  distinct(cut)%>%
  arrange(cut)%>%
  mutate(id=paste0("ct",row_number()))

#Create new dataframe with just id's for cut, clarity and color
diamonds2<-diamonds%>%
  left_join(color,by="color")%>%
  rename(colorid="id")%>%
  left_join(cut,by="cut")%>%
  rename(cutid="id")%>%
  left_join(clarity,by="clarity")%>%
  rename(clarityid="id")%>%
  select(-cut,-clarity,-color)

diamonds<-diamonds%>%
  rename("length.mm"=x,
         "width.mm"=y,
         "depth.mm"=z)

diamonds2<-diamonds2%>%
  rename("length.mm"=x,
         "width.mm"=y,
         "depth.mm"=z,
         "id"=X)

#create database for diamonds
conn<-dbConnect(RSQLite::SQLite(),"diamonds.db")

#create diamon_sales table
dbWriteTable(conn,"diamond_sales",diamonds2)

#Create clarity, color and cut tables
dbWriteTable(conn,"diamond_cut",cut)
dbWriteTable(conn,"diamond_color",color)
dbWriteTable(conn,"diamond_clarity",clarity)

#List tables
dbListTables(conn)

#Check with query
dbGetQuery(conn,"Select * from diamond_color")
dbGetQuery(conn,"Select * from diamond_cut")
dbGetQuery(conn,"Select * from diamond_clarity")
dbGetQuery(conn, "select * from diamond_sales limit 10")

#Create query to get full table again
df<-dbGetQuery(conn,
           "Select diamond_sales.id,
           diamond_sales.carat,
           diamond_cut.cut,
           diamond_color.color,
           diamond_clarity.clarity,
           diamond_sales.depth,
           diamond_sales.'length.mm',
           diamond_sales.'width.mm',
           diamond_sales.'depth.mm',
           diamond_sales.price
           from diamond_sales
           inner join diamond_color on diamond_sales.colorid=diamond_color.id
           inner join diamond_cut on diamond_cut.id=diamond_sales.cutid
           inner join diamond_clarity on diamond_sales.clarityid=diamond_clarity.id")

#close connection
dbDisconnect(conn)

#Open connection
conn<-dbConnect(RSQLite::SQLite(),"diamond.db")



tables<-dbListTables(conn)

tibble(tables)


#Linear Regression? ----
library(caret)
library(ggplot2)


head(diamonds)

#I just want price and carat for now
diamonds_formodel<-diamond%>%
  select(price,carat,depth.mm,width.mm,length.mm)

#Now we'll split the data into a training set and a test set
# We'll use 75% of the data for training and 25% for testing
set.seed(123)
indx<-createDataPartition(diamonds_formodel$price,p=0.75,list=FALSE)
train_data<-diamonds_formodel[indx,]
test_data<-diamonds_formodel[-indx,]

# Now we'll train a linear regression model using the train_data
# We'll use the "lm" method from the stats package
linear_model<-train(price~.,data=train_data,method="lm")

# Now we'll use the model to make predictions on the test data
predictions <- predict(linear_model, test_data)

# Now let's evaluate the model performance 
# We'll use the mean squared error (MSE) as our evaluation metric
MSE <- mean((predictions - test_data$price)^2)
print(MSE)


#Mean ABsolute Value
abs_residuals <- abs(predictions - test_data$price)

# Finally, we'll take the mean of the absolute residuals to get the MAE
MAE <- mean(abs_residuals)
print(MAE)

#R-squared value
residuals <- predictions - test_data$price

# Next, we'll calculate the total sum of squares (TSS)
# TSS is the sum of the squared differences between the actual values and the mean of the actual values
TSS <- sum((test_data$price - mean(test_data$price))^2)

# Now we'll calculate the residual sum of squares (RSS)
# RSS is the sum of the squared residuals
RSS <- sum(residuals^2)

# Finally, we'll calculate R-squared
R_squared <- 1 - (RSS/TSS)

print(paste("R-squared:",R_squared))
print(paste("MSE:",MSE))
print(paste("MAE:",MAE))


ggplot(data = test_data, aes(x = price, y = predictions)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",color="blue",size=1.5) +
  ggtitle("Predicted price vs actual price")

hist(diamonds$price)
max(diamonds$price)


#Linear Regression next round of tests---
library(caret)
library(glmnet)
library(tidyverse)
library(ggplot2)
library(gt)


data(diamonds)

#data
diamonds<-read_csv("C:/Users/12567/Desktop/R/Interviews/diamonds.csv")
diamonds<-diamonds%>%
  rename("length.mm"=x,
         "width.mm"=y,
         "depth.mm"=z)

# Next, we'll split the data into a training set and a test set
# We'll use 75% of the data for training and 25% for testing
set.seed(123)
indx <- createDataPartition(diamonds$price, p = 0.75, list = FALSE)
train_data <- diamonds[indx,]
test_data <- diamonds[-indx,]

#Model 1: Price ~ carat
model1<-train(price~carat,data=train_data,method="lm")

#Model 2: price ~ depth.mm + length.mm + width.mm
model2<-train(price~depth.mm + length.mm + width.mm,data=train_data,method="lm")

#Model 3: price ~ depth.mm + length.mm + width.mm
model3<-train(price~carat + depth.mm + length.mm + width.mm,data=train_data,method="lm")


#Compare modles by MSE using resamples
resample<-resamples(list(model1,model2,model3),metric="MSE")
summary(resample)

modelresults<-data.frame(resample[2]) #Put results in dataframe
colnames(modelresults)<-gsub("values.","",colnames(modelresults)) #remove values. from column names


#A bit of processing of the results data to format it for a final summary table
Model1Results<-modelresults%>%
  select(Model1.MAE,Model1.RMSE,Model1.Rsquared)

colnames(Model1Results)<-gsub("Model1.","",colnames(Model1Results))


Model2Results<-modelresults%>%
  select(Model2.MAE,Model2.RMSE,Model2.Rsquared)%>%
  mutate(Model="Model2")

colnames(Model2Results)<-gsub("Model2.","",colnames(Model2Results))

Model3Results<-modelresults%>%
  select(Model3.MAE,Model3.RMSE,Model3.Rsquared)%>%
  mutate(Model="Model3")

colnames(Model3Results)<-gsub("Model3.","",colnames(Model3Results))

ModelResults<-rbind(Model1Results,Model2Results,Model3Results)

rm(modelresults,Model1Results,Model2Results,Model3Results,resample)

ModelResults%>%
  group_by(Model)%>%
  summarise(MAE=mean(MAE),RMSE=mean(RMSE),Rsquared=mean(Rsquared))%>%
  gt()


#Compare the two models graphically
pred1<-predict(model1,test_data)

pred2<-predict(model2,test_data)

pred3<-predict(model3,test_data)


#Create dataframes for each of the model's predicted values
model1_df <- data.frame(pred = pred1, actual = test_data$price, model = "model1")
model2_df <- data.frame(pred = pred2, actual = test_data$price, model = "model2")
model3_df <- data.frame(pred = pred3, actual = test_data$price, model = "model3")

#Combine into 1 dataframe
models_df <- rbind(model1_df, model2_df, model3_df)


# We'll use the mean squared error (MSE) as our evaluation metric
MSE1 <- mean((pred1 - test_data$price)^2)
print(MSE1)
MSE2 <- mean((pred2 - test_data$price)^2)
print(MSE2)
MSE3 <- mean((pred3 - test_data$price)^2)
print(MSE3)

prettyNum(MSE1,big.mark=",")

#MAE
#Mean ABsolute Value
abs_residuals1 <- abs(pred1 - test_data$price)
abs_residuals2 <- abs(pred2 - test_data$price)
abs_residuals3 <- abs(pred3 - test_data$price)

# Finally, we'll take the mean of the absolute residuals to get the MAE
MAE1 <- mean(abs_residuals1)
print(MAE1)
MAE2 <- mean(abs_residuals2)
print(MAE2)
MAE3 <- mean(abs_residuals3)
print(MAE3)

#Rsquared
residuals1 <- pred1 - test_data$price
residuals2 <- pred2 - test_data$price
residuals3 <- pred3 - test_data$price

# Next, we'll calculate the total sum of squares (TSS)
# TSS is the sum of the squared differences between the actual values and the mean of the actual values
TSS <- sum((test_data$price - mean(test_data$price))^2)

# Now we'll calculate the residual sum of squares (RSS)
# RSS is the sum of the squared residuals
RSS1 <- sum(residuals1^2)
RSS2 <- sum(residuals2^2)
RSS3 <- sum(residuals3^2)

# Finally, we'll calculate R-squared
R_squared1 <- 1 - (RSS1/TSS)
R_squared2 <- 1 - (RSS2/TSS)
R_squared3 <- 1 - (RSS3/TSS)

print(R_squared1)
print(R_squared2)
print(R_squared3)

#Put stats into dataframe
modelstats<-data.frame(MSE=c(MSE1,MSE2,MSE3),MAE=c(MAE1,MAE2,MAE3),Rsquared=c(R_squared1,R_squared2,R_squared3))
row.names(modelstats)<-c("Model1","Model2","Model3")

datatable(modelstats)

#now plot
ggplot(models_df, aes(x = actual, y = pred, color = model)) +
  geom_point() +
  # geom_abline(intercept = 0, slope = 1, linetype = "dashed",size=1.2) +
  geom_smooth(method = "lm",size=1.2,color="black") +
  ggtitle("Comparison of Model Predictions") +
  xlab("Actual Values") +
  ylab("Predicted Values")+
  facet_grid(~model)


ggplot(models_df, aes(x = actual, y = pred, color = model)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(color = model)) +
  scale_color_manual(values = c("model1" = "red", "model2" = "blue")) +
  ggtitle("Comparison of Model Predictions") +
  xlab("Actual Values") +
  ylab("Predicted Values")

ggplot(models_df, aes(x = actual, y = pred)) +
  geom_point(aes(color = model)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = model)) +
  scale_color_manual(values = c("model1" = "red", "model2" = "blue")) +
  ggtitle("Comparison of Model Predictions") +
  xlab("Actual Values") +
  ylab("Predicted Values")

ggplot(models_df)+
  geom_point(aes(x=actual,y=pred,color=model))+
  geom_smooth(method="lm",aes(x=actial.u=pred,color=c("model1"="red","model2","blue")))


#Feature Selection using Lasso (Least Absolute Shrinkage and Selection Operator)

#Create a matrix of indepedent variables
x <-data.matrix(train_data[,-7]) #remove "price" and turn into a matrix

#create vector of dependent variable
y<-train_data$price 

#Fit a lasso model to the data
fit<-glmnet(x,y,alpha=1)

#plot lasso coefficients
plot(fit,xvar="lambda",label=TRUE)

#Now select variables with non-zero coefficients
coefs<-coef(fit, s="lambda.min")
selected_vars<-rownames(coefs)[coefs !=0]




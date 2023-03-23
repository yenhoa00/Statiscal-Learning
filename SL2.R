library(tidyverse)
library(readxl)
library(ggplot2)
library(car)
library(corrplot)
library(leaflet)
library(readr)
library(dplyr)       
library(rpart)
library(rpart.plot)
library(tree)
library(party)
library(rmarkdown)
library(randomForest)
library(caret)
library("factoextra")
library("gridExtra")
library(GGally)
library(ggfortify)
library(cluster)
library(glmnet)




#Importdata
bnb <- read_excel("~/Downloads/hawaii.xlsx")
#bnb <- merge(x= airbnb,y= neighbourhoods,by="neighbourhood_cleansed")


#Cleandata
#bnb <- bnb %>%
  #mutate(av365 = round(availability_365/365,2)*100)
bnb<-subset(bnb, select=-c(host_id, id, host_name, name, host_acceptance_rate, host_response_rate), bnb$price<500)
view(bnb)
str(bnb)


#check for Na and Null
bnb %>% summarise_all(~ sum(is.null(.))) %>% sum()
bnb %>% summarise_all(~ sum(is.na(.))) %>% sum()
bnb<-na.omit(bnb)
summary(bnb)

#Visualization
#leafletmap
map <- colorFactor(palette = c("pink", "red", "white", "green", "yellow"), domain = bnb$neighbourhood)
leaflet(data = bnb) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  addCircleMarkers(~longitude, ~latitude, color = ~map(zone), weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1,
                                                                                                     label = paste("Name:", bnb$name)) %>% 
  addLegend("bottomright", pal = map, values = ~neighbourhood,
            title = "Neighbourhood groups",
            opacity = 1
  )

#price
ggplot(bnb, aes(price)) + geom_density(aes(fill = price), alpha=0.3) + ggtitle("Listing Density by Price")


#number of properties in each neighbourhood
ggplot(bnb, aes(x=neighbourhood))+
  geom_bar(stat="count", width=0.7)+
  geom_text(stat='count', aes(label=..count..), vjust=0.025)+
  labs(title = "# of Properties in each neighbourhood",x="neighbourhood", y="#ofProperties")


#room_type 
price_roomtype <- bnb %>% 
  group_by(neighbourhood, room_type) %>% summarise(Mean_Price = mean(price))


ggplot(price_roomtype, aes(x = reorder(neighbourhood, -Mean_Price), y = Mean_Price, fill = room_type)) + 
  geom_bar(stat="identity",colour="black",position=position_dodge()) + 
  labs(title="Mean Price of each Zone according to Type of The Room") + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), legend.position = c(0.9, 0.8)) + xlab("") + ylab("Mean Price")

#pricebyneighbourhood
neighbourhood_price <- bnb%>%
  group_by(neighbourhood)%>%
  summarize(count = n(),
            avg_price = mean(price),
            med_price = median(price))


ggplot(neighbourhood_price, aes(neighbourhood, count, fill = neighbourhood))+
  geom_col(col = "black")+
  geom_line(aes(neighbourhood, avg_price*10), group = 1, lty = 2)+
  geom_label(aes(neighbourhood, avg_price*10, label = paste("$", round(avg_price), sep = "")),
             fill = "white", col = "red")+
  labs(y = "Total apartments and Average daily price by Zone",
       x = "Zone",
       title = "Prices According to Zone")+
  theme(legend.position = "none")    



#correlation - 
bnb_cor <- bnb[, sapply(bnb, is.numeric)]
bnb_cor <- bnb_cor[complete.cases(bnb_cor), ]
correlation_matrix <- cor(bnb_cor)
corrplot(correlation_matrix, method = "circle")



#predictiveanalysis

#Transform categorical to numeric
bnb$neighbourhood <- as.numeric(as.factor(bnb$neighbourhood))
bnb$room_type <- as.numeric(as.factor(bnb$room_type))
bnb$price<-log(bnb$price)


#Creating training and test sets
bnb1<-subset(bnb, select = -c(amenities))
sample <- sample(nrow(bnb1), nrow(bnb1) * 0.7)
train <- bnb1[sample,]
test <- bnb1[-sample,]

#Regression
lm_train <- lm(price~., data = train)
summary(lm_train)
plot(lm_train)

lm_pred <- predict(lm_train, test)
RMSE_reg <-sqrt(mean( (test$price - lm_pred)**2 ))
SSE <- sum((test$price - lm_pred)**2)
SSR <- sum((lm_pred - mean(test$price)) ** 2)
SST <- SSR +SSE
R2 <- (SST - SSE) / SST
cat("SST: ", SST, "   SSE: ", SSE, "   SSR: ", SSR, "\nR2: ", R2, "   RMSE: ", RMSE_reg)

lm_prediction = (data.frame(exp((lm_pred)), exp((test$price))))
colnames(lm_prediction) <- c("Predicted_price","Real_price")
head(lm_prediction,10)


#LASSO
x_train <-model.matrix(price~., train)[,-1]
y_train <- train$price

#find the best cv
cv <- cv.glmnet(x_train, y_train, alpha = 1)
cv$lambda.min

#fitmodel
ls_train <- glmnet(x_train, y_train, alpha = 1, lambda = cv$lambda.min)
coef(ls_train)

#Pred
x_test <- model.matrix(price ~., test)[,-1]
predictions <- ls_train %>% predict(x_test) %>% as.vector()

data.frame(
  RMSE = RMSE(predictions, test$price),
  Rsquare = R2(predictions, test$price)
)


##Ridge
#find the best cv
cvR <- cv.glmnet(x_train, y_train, alpha = 0)
cvR$lambda.min

#fitmodel
r_train <- glmnet(x_train, y_train, alpha = 0, lambda = cvR$lambda.min)
coef(r_train)
r_pred <- r_train %>% predict(x_test) %>% as.vector()

data.frame(
  RMSE = RMSE(r_pred, test$price),
  Rsquare = R2(r_pred, test$price)
)



#DecisionTree
dt_train <- rpart(price ~., data = train)
rpart.plot(dt_train)
printcp(dt_train)
dt_pred<-predict(dt_train, test)


RMSE_dt <-sqrt(mean( (test$price - dt_pred)**2 ))
SSE2 <- sum((test$price - dt_pred)**2)
SSR2 <- sum((dt_pred - mean(test$price)) ** 2)
SST2 <- SSR2 +SSE2
R22 <- (SST2 - SSE2) / SST2
cat("SST: ", SST2, "   SSE: ", SSE2, "   SSR: ", SSR2, "\nR2: ", R22, "   RMSE: ", RMSE_dt)

dt_prediction = (data.frame((dt_pred), (test$price)))
colnames(dt_prediction) <- c("Predicted_price","Real_price")
head(dt_prediction,10)



#RandomForest
rf_train<-randomForest(price~., data=train, importance= TRUE, ntry = 19)
rf_pred <- predict(rf_train, test)
RMSE_rf <-sqrt(mean( (test$price - rf_pred)**2 ))
SSE1 <- sum((test$price - rf_pred)**2)
SSR1 <- sum((rf_pred - mean(test$price)) ** 2)
SST1 <- SSR1 +SSE1
R21 <- (SST1 - SSE1) / SST1
cat("SST: ", SST1, "   SSE: ", SSE1, "   SSR: ", SSR1, "\nR2: ", R21, "   RMSE: ", RMSE_rf)


rf_prediction = (data.frame((rf_pred), (test$price)))
colnames(rf_prediction) <- c("Predicted_price","Real_price")
head(rf_prediction,10)

importance(rf_train)
varImpPlot(rf_train)


#create dataset
test2 <- test %>%
  mutate(predicted_total_price = rf_pred,
         actual_total_price = test$price,
         Residuals = predicted_total_price - actual_total_price)%>%
  select(predicted_total_price, actual_total_price, Residuals)

#visualise
ggplot(test2, aes(actual_total_price, predicted_total_price))+
  geom_point(alpha = 0.5, color = "blue")+
  geom_smooth(method = "loess", col = "red", se = FALSE)+
  labs(x = "Actual Price",
       y = "Predicted Price")


#Subset of Hawaii
bnb.hawaii<- subset(bnb, neighbourhood == 1 )
view(bnb.hawaii)

#Split
bnb.hawaii<-subset(bnb.hawaii, select = -c(amenities))
sample.hawaii <- sample(nrow(bnb.hawaii), nrow(bnb.hawaii) * 0.7)
train.hawaii <- bnb.hawaii[sample.hawaii,]
test.hawaii <- bnb.hawaii[-sample.hawaii,]


#Regression
lm_train.hawaii <- lm(price~., data = train.hawaii)
summary(lm_train.hawaii)
plot(lm_train.hawaii)

lm_pred.hawaii <- predict(lm_train.hawaii, test.hawaii)
RMSE_reg.hawaii <-sqrt(mean( (test.hawaii$price - lm_pred.hawaii)**2 ))
SSE.hawaii <- sum((test.hawaii$price - lm_pred.hawaii)**2)
SSR.hawaii <- sum((lm_pred.hawaii - mean(test.hawaii$price)) ** 2)
SST.hawaii <- SSR.hawaii +SSE.hawaii
R2.hawaii <- (SST.hawaii - SSE.hawaii) / SST.hawaii
cat("SST: ", SST.hawaii, "   SSE: ", SSE.hawaii, "   SSR: ", SSR.hawaii, "\nR2: ", R2.hawaii, "   RMSE: ", RMSE_reg.hawaii)

lm_prediction.hawaii = (data.frame((lm_pred.hawaii), (test.hawaii$price)))
colnames(lm_prediction.hawaii) <- c("Predicted_price","Real_price")
head(lm_prediction.hawaii,10)


#DecisionTree
dt_train.hawaii <- rpart(price ~., data = train.hawaii)
rpart.plot(dt_train.hawaii)
printcp(dt_train.hawaii)
dt_pred.hawaii<-predict(dt_train.hawaii, test.hawaii)


RMSE_dt.hawaii <-sqrt(mean( (test.hawaii$price - dt_pred.hawaii)**2 ))
SSE2.hawaii <- sum((test.hawaii$price - dt_pred.hawaii)**2)
SSR2.hawaii <- sum((dt_pred.hawaii - mean(test.hawaii$price)) ** 2)
SST2.hawaii <- SSR2.hawaii +SSE2.hawaii
R22.hawaii <- (SST2.hawaii - SSE2.hawaii) / SST2.hawaii
cat("SST: ", SST2.hawaii, "   SSE: ", SSE2.hawaii, "   SSR: ", SSR2.hawaii, "\nR2: ", R22.hawaii, "   RMSE: ", RMSE_dt.hawaii)

dt_prediction.hawaii = (data.frame((dt_pred.hawaii), (test.hawaii$price)))
colnames(dt_prediction.hawaii) <- c("Predicted_price","Real_price")
head(dt_prediction.hawaii,10)



#RandomForest
rf_train.hawaii<-randomForest(price~., data=train.hawaii)
rf_pred.hawaii <- predict(rf_train.hawaii, test.hawaii)
RMSE_rf.hawaii <-sqrt(mean( (test.hawaii$price - rf_pred.hawaii)**2 ))
SSE1.hawaii <- sum((test.hawaii$price - rf_pred.hawaii)**2)
SSR1.hawaii <- sum((rf_pred.hawaii - mean(test.hawaii$price)) ** 2)
SST1.hawaii <- SSR1.hawaii +SSE1.hawaii
R21.hawaii <- (SST1.hawaii - SSE1.hawaii) / SST1.hawaii
cat("SST: ", SST1.hawaii, "   SSE: ", SSE1.hawaii, "   SSR: ", SSR1.hawaii, "\nR2: ", R21.hawaii, "   RMSE: ", RMSE_rf.hawaii)


rf_prediction.hawaii = (data.frame((rf_pred.hawaii), (test.hawaii$price)))
colnames(rf_prediction.hawaii) <- c("Predicted_price","Real_price")
head(rf_prediction.hawaii,10)




#Subset of Honolulu 
bnb.hono<- subset(bnb, neighbourhood == 2 )
view(bnb.hono)

#Split
bnb.hono<-subset(bnb.hono, select = -c(amenities))
sample.hono <- sample(nrow(bnb.hono), nrow(bnb.hono) * 0.7)
train.hono <- bnb.hono[sample.hono,]
test.hono <- bnb.hono[-sample.hono,]

#Regression
lm_train.hono <- lm(price~., data = train.hono)
summary(lm_train.hono)
plot(lm_train.hono)

lm_pred.hono <- predict(lm_train.hono, test.hono)
RMSE_reg.hono <-sqrt(mean( (test.hono$price - lm_pred.hono)**2 ))
SSE.hono <- sum((test.hono$price - lm_pred.hono)**2)
SSR.hono <- sum((lm_pred.hono - mean(test.hono$price)) ** 2)
SST.hono <- SSR.hono +SSE.hono
R2.hono <- (SST.hono - SSE.hono) / SST.hono
cat("SST: ", SST.hono, "   SSE: ", SSE.hono, "   SSR: ", SSR.hono, "\nR2: ", R2.hono, "   RMSE: ", RMSE_reg.hono)

lm_prediction.hono = (data.frame((lm_pred.hono), (test.hono$price)))
colnames(lm_prediction.hono) <- c("Predicted_price","Real_price")
head(lm_prediction.hono,10)


#DecisionTree
dt_train.hono <- rpart(price ~., data = train.hono)
rpart.plot(dt_train.hono)
printcp(dt_train.hono)
dt_pred.hono<-predict(dt_train.hono, test.hono)


RMSE_dt.hono <-sqrt(mean( (test.hono$price - dt_pred.hono)**2 ))
SSE2.hono <- sum((test.hono$price - dt_pred.hono)**2)
SSR2.hono <- sum((dt_pred.hono - mean(test.hono$price)) ** 2)
SST2.hono <- SSR2.hono +SSE2.hono
R22.hono <- (SST2.hono - SSE2.hono) / SST2.hono
cat("SST: ", SST2.hono, "   SSE: ", SSE2.hono, "   SSR: ", SSR2.hono, "\nR2: ", R22.hono, "   RMSE: ", RMSE_dt.hono)

dt_prediction.hono = (data.frame((dt_pred.hono), (test.hono$price)))
colnames(dt_prediction.hono) <- c("Predicted_price","Real_price")
head(dt_prediction.hono,10)



#RandomForest
rf_train.hono<-randomForest(price~., data=train.hono)
rf_pred.hono <- predict(rf_train.hono, test.hono)
RMSE_rf.hono <-sqrt(mean( (test.hono$price - rf_pred.hono)**2 ))
SSE1.hono <- sum((test.hono$price - rf_pred.hono)**2)
SSR1.hono <- sum((rf_pred.hono - mean(test.hono$price)) ** 2)
SST1.hono <- SSR1.hono +SSE1.hono
R21.hono <- (SST1.hono - SSE1.hono) / SST1.hono
cat("SST: ", SST1.hono, "   SSE: ", SSE1.hono, "   SSR: ", SSR1.hono, "\nR2: ", R21.hono, "   RMSE: ", RMSE_rf.hono)


rf_prediction.hono = (data.frame((rf_pred.hono), (test.hono$price)))
colnames(rf_prediction.hono) <- c("Predicted_price","Real_price")
head(rf_prediction.hono,10)



# Table the Results
results_table <-tibble(Model_Type = c( "Linear",
                                       "Random Forest",
                                      "Regression Tree"
                                      ), 
                       RMSE = c(RMSE_reg, RMSE_rf, RMSE_dt
                                ),
                       R2 = c(R2, R21, R22)) %>% 
  mutate(RMSE = sprintf("%0.2f", RMSE))
knitr::kable(results_table)

# Table the Results of neighbourhood
results_table.neighbour <-tibble(Model_Type = c( "Linear",
                                       "Random Forest",
                                       "Regression Tree"
), 
RMSE.hawaii = c(RMSE_reg.hawaii, RMSE_rf.hawaii, RMSE_dt.hawaii
),
RMSE.hono = c( RMSE_reg.hono, RMSE_rf.hono, RMSE_dt.hono
               ),
R2.hawaii = c(R2.hawaii, R21.hawaii, R22.hawaii) ,
R2.hono = c(R2.hono, R21.hono, R22.hono))

knitr::kable(results_table.neighbour)



##UnsupervisedLearning

#PCA
bnb2 <- bnb[, sapply(bnb, is.numeric)]
bnb2 <- as.data.frame(scale(bnb2))
bnb2 <- bnb2[complete.cases(bnb2), ]
corr <- cor(bnb2)
corrplot(corr, method = "circle")

#vis
bnb.pca <- prcomp(bnb2, scale = TRUE)
print(bnb.pca)

summary(bnb.pca)

eig.bnb<-get_eigenvalue(bnb.pca)
eig.bnb

fviz_eig(bnb.pca, col.var="blue")

#var
var.bnb <- get_pca_var(bnb.pca)
var.bnb

head(var.bnb$cos2)

#corr
corrplot(var.bnb$cos2, is.corr=FALSE)
fviz_cos2(bnb.pca, choice = "var", axes = 1:2)

fviz_pca_var(bnb.pca,
             col.var = "cos2", # Color by the quality of representation
             gradient.cols = c("darkorchid4", "gold", "darkorange"),
             repel = TRUE
)

a<-fviz_contrib(bnb.pca, choice = "var", axes = 1)
b<-fviz_contrib(bnb.pca, choice = "var", axes = 2)
grid.arrange(a,b, ncol=2, top='Contribution of the variables to the first two PCs')

#KMeans
set.seed(1)
km_bnb <- kmeans(bnb2, 2)
print(km_bnb)
km_bnb$cluster
km_bnb$withinss
km_bnb$size

aggregate(bnb2, by=list(km_bnb$cluster), mean)
kmeans<-eclust(bnb2, k=2)
autoplot(bnb.pca, data=kmeans, colour="cluster")


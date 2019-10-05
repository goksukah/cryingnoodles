#UCLA Introduction to Data Science
#CLASS PROJECT
#Goksu KAHRIMAN


#1) Accessing the Data Set
getwd()
setwd("/Users/Goksu/Desktop/Data Science/data/Class Project")  # Relative path, do not forget to change according to your file directory
housing = read.csv("housing.csv")  # read csv file
head(housing)
summary(housing)

#longitude         latitude     housing_median_age  total_rooms    total_bedrooms     population      households    
#Min.   :-124.3   Min.   :32.54   Min.   : 1.00      Min.   :    2   Min.   :   1.0   Min.   :    3   Min.   :   1.0  
#1st Qu.:-121.8   1st Qu.:33.93   1st Qu.:18.00      1st Qu.: 1448   1st Qu.: 296.0   1st Qu.:  787   1st Qu.: 280.0  
#Median :-118.5   Median :34.26   Median :29.00      Median : 2127   Median : 435.0   Median : 1166   Median : 409.0  
#Mean   :-119.6   Mean   :35.63   Mean   :28.64      Mean   : 2636   Mean   : 537.9   Mean   : 1425   Mean   : 499.5  
#3rd Qu.:-118.0   3rd Qu.:37.71   3rd Qu.:37.00      3rd Qu.: 3148   3rd Qu.: 647.0   3rd Qu.: 1725   3rd Qu.: 605.0  
#Max.   :-114.3   Max.   :41.95   Max.   :52.00      Max.   :39320   Max.   :6445.0   Max.   :35682   Max.   :6082.0  
#NA's   :207                                      
# median_income     median_house_value   ocean_proximity
# Min.   : 0.4999   Min.   : 14999     <1H OCEAN :9136  
# 1st Qu.: 2.5634   1st Qu.:119600     INLAND    :6551  
# Median : 3.5348   Median :179700     ISLAND    :   5  
# Mean   : 3.8707   Mean   :206856     NEAR BAY  :2290  
# 3rd Qu.: 4.7432   3rd Qu.:264725     NEAR OCEAN:2658  
# Max.   :15.0001   Max.   :500001

levels(housing$ocean_proximity)
#[1] "<1H OCEAN"  "INLAND"     "ISLAND"     "NEAR BAY"   "NEAR OCEAN"

#2) Data Visualization
par(mfrow=c(3,3), mar=c(5,4,2,2)) #layout to view all histograms in one view (3x3), changing margin size
hist(housing$longitude, col="blue", xlab="Longitude", main="Histogram of housing$longtitude")
hist(housing$latitude, col="gold", xlab="Latitude", main="Histogram of housing$latitude")
hist(housing$housing_median_age, col="pink", xlab="housing_median_age", main="Histogram of housing$housing_median_age")
hist(housing$total_rooms, col="red", xlab="total_rooms", main="Histogram of housing$total_rooms")
hist(housing$total_bedrooms, col="darkgreen", xlab="total_bedrooms", main="Histogram of housing$total_bedrooms")
hist(housing$population, col="green", xlab="population", main="Histogram of housing$population")
hist(housing$households, col="yellow", xlab="households", main="Histogram of housing$households")
hist(housing$median_income, col="orange", xlab="median_income", main="Histogram of housing$median_income")
hist(housing$median_house_value, col="purple", xlab="median_house_value", main="Histogram of housing$median_house_value")

#We can clearly say that most of the features are skewed when we look at the histograms above.

#3
fivenum(housing$total_bedrooms, na.rm=TRUE)
#[1]    1  296  435  647 6445               #435 is the median value, it is also given in the summary function output

#following transformation is needed for the imputation for the missin data
housing = transform(housing, total_bedrooms = ifelse(is.na(total_bedrooms), median(total_bedrooms, na.rm=TRUE), total_bedrooms))
head(housing)


# Create new binary categorical variable
newcol1 <- data.frame(less_1H_OCEAN=(housing$ocean_proximity=="<1H OCEAN"))
newcol2 <- data.frame(INLAND=(housing$ocean_proximity=="INLAND"))
newcol3 <- data.frame(ISLAND=(housing$ocean_proximity=="ISLAND"))
newcol4 <- data.frame(NEAR_BAY=(housing$ocean_proximity=="NEAR BAY"))
newcol5 <- data.frame(NEAR_OCEAN=(housing$ocean_proximity=="NEAR OCEAN"))

#dropping ocean_proximity column
housing <- subset(housing, select = -c(ocean_proximity))
#housing$mean_number_bedrooms<- housing$total_bedrooms
#housing$mean_number_rooms<- housing$total_rooms
class(housing)

#housing$mean_number_bedrooms <- housing$total_bedrooms/housing$households
#housing$mean_number_rooms <- housing$total_rooms/housing$households
housing<- within(housing, mean_number_rooms <- (housing$total_rooms/housing$households))
housing<- within(housing, mean_number_bedrooms <- (housing$total_bedrooms/housing$households))

head(housing)


housing <- subset(housing, select = -c(total_bedrooms,total_rooms))
#head(housing)

housing$less_1H_OCEAN <- newcol1$less_1H_OCEAN
housing$ISLAND <-newcol3$ISLAND
housing$INLAND <-newcol2$INLAND
housing$NEAR_BAY <-newcol4$NEAR_BAY
housing$NEAR_OCEAN <-newcol5$NEAR_OCEAN
head(housing)

housing$median_house_value1<- housing$median_house_value
housing <- subset(housing, select = -c(median_house_value))
housing$median_house_value <- housing$median_house_value1
housing <- subset(housing, select = -c(median_house_value1))
head(housing)

cleaned_housing <-housing
cleaned_housing[, -c(9,10,11,12,13,14)] <- scale(cleaned_housing[, -c(9,10,11,12,13,14)])
head(cleaned_housing)
summary(cleaned_housing)

install.packages("randomForest")
library(randomForest)
#rfNews()

n <- nrow(cleaned_housing)
ntrain <- round(n*0.8)  # 80% for training set
set.seed(314)    # Set seed for reproducible results

tindex <- sample(n, ntrain)   # Create an index

train <- cleaned_housing[tindex,]   # Create training set
test<- cleaned_housing[-tindex,]   # Create test set

train_x <- subset(train, select = -c(median_house_value))
train_y <- subset(train, select = c(median_house_value))

class(train_x)

#rf = randomForest(train_x, y=train_y, ntree=500, importance =T)
rf <- randomForest(train_x, y=train_y[,1], ntree=500, importance =TRUE)


length(train_x)
length(train_y)

names(rf)
#[1] "call"            "type"            "predicted"       "err.rate"        "confusion"       "votes"          
#[7] "oob.times"       "classes"         "importance"      "importanceSD"    "localImportance" "proximity"      
#[13] "ntree"           "mtry"            "forest"          "y"               "test"            "inbag"          
#[19] "terms"         "coefs"           "y"               "test"            "inbag"  

rf$importance
#                         %IncMSE IncNodePurity
#longitude            6676116532  2.458200e+13
#latitude             5307165528  2.152635e+13
#housing_median_age   1041977226  9.587209e+12
#population           1047928214  7.399628e+12
#households           1177723653  7.912216e+12
#median_income        8688636656  7.540466e+13
#mean_number_rooms    1847239090  2.101139e+13
#mean_number_bedrooms  424282596  7.497485e+12
#less_1H_OCEAN        1583373592  4.254420e+12
#ISLAND                  1461811  7.287072e+10
#INLAND               4148110088  3.183930e+13
#NEAR_BAY              385095242  1.138036e+12
#NEAR_OCEAN            458006545  1.927328e+12

#We can say that most important predictor is "median_income" since it has the highest MSE value.

oob_prediction = predict(rf)
train_mse = mean(as.numeric(unlist(oob_prediction - train_y)^2))
#Unlist thing above might be wrong , but it was giving me errors if I didn`t use it`

oob_rmse = sqrt(train_mse)
oob_rmse
#[1] 49066.23

test_x <- subset(test, select = -c(median_house_value))
test_y <- subset(test, select = c(median_house_value))
y_pred = predict(rf, test_x)
test_mse = mean(as.numeric(unlist(y_pred - test_y)^2))
test_rmse =sqrt(test_mse)
test_rmse
#[1] 48731.11

#Train and test error values are similar; we can say that this model provides a good predictor. 

## Load in libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggmap)
library(stringr)
library(lubridate)
library(boot)
library(glmnet)
library(caret)

## Read in data
threeoneonedata <- read_csv('data/2015_311.csv')
df_co <- read_csv('data/ad_viz_plotval_data_co.csv')
df_ozone <- read_csv('data/ad_viz_plotval_data_ozone.csv')
df_pm25 <- read_csv('data/ad_viz_plotval_data_pm2_5.csv')

df_311 <- threeoneonedata %>% filter(Borough == 'MANHATTAN') 

map <- get_map(location = 'manhattan', zoom= 12, maptype = 'watercolor',
               source = 'google', color = 'color')
map <- ggmap(map) + geom_point(data = df_pm25, na.rm = T,
                               aes(x=df_pm25$SITE_LONGITUDE, y=df_pm25$SITE_LATITUDE), color= 'darkred', size = 3)
map + geom_point(data = df_ozone, na.rm = T, 
                 aes(x=df_ozone$SITE_LONGITUDE, y=df_ozone$SITE_LATITUDE), color= 'purple', size = 4)
map + geom_point(data = df_co, na.rm = T, 
                 aes(x=df_co$SITE_LONGITUDE, y=df_co$SITE_LATITUDE), color= 'green', show.legend = T, size = 1, alpha = 1)

df_co_AQI <- df_co %>% select(Date, co.AQI = contains('AQI'))
df_ozone_AQI <- df_ozone %>% select(Date, ozone.AQI = contains('AQI'))
df_pm_AQI <- df_pm25 %>% select(Date, pm.AQI = contains('AQI'))

getAQIHC <- function(AQI){
  HC <- ifelse(AQI<=50,'Good',
               ifelse(AQI<=100, 'Moderate',
                      ifelse(AQI<=150, 'Unhealthy for sensitive groups',
                             ifelse(AQI<=200,'Unhealthy',
                                    ifelse(AQI<=300,'Very unhealthy','Hazardous')))))
}
print(getAQIHC(52))



df_co_AQI_HC <- df_co_AQI %>% mutate(co.Health.concern = getAQIHC(co.AQI))
df_pm_AQI_HC <- df_pm_AQI %>% mutate(pm.Health.concern = getAQIHC(pm.AQI))
df_ozone_AQI_HC <- df_ozone_AQI %>% mutate(ozone.Health.concern = getAQIHC(ozone.AQI))

# Check for duplicates in dates
sum(duplicated(df_pm_AQI$Date))
sum(duplicated(df_co_AQI$Date))
sum(duplicated(df_ozone_AQI$Date))

#pm has duplicates in the dates
df_pm_AQI_sum <- df_pm_AQI %>% group_by(Date) %>% dplyr::summarise(pm.AQI = mean(pm.AQI))
head(df_pm_AQI_sum)


df_pm_AQI_HC <- df_pm_AQI_sum %>% mutate(pm.Health.concern = getAQIHC(pm.AQI))


df_aq_HC <- df_co_AQI_HC %>% inner_join(df_pm_AQI_HC, by = 'Date') %>% inner_join(df_ozone_AQI_HC, by = 'Date') 
head(df_aq_HC)

df_aq <- df_co_AQI %>% inner_join(df_pm_AQI_sum, by = 'Date') %>% inner_join(df_ozone_AQI, by = 'Date') 
head(df_aq)

names(df_311) <- str_replace_all(names(df_311), pattern = '[^[:alnum:]]', replacement = '_')
head(names(df_311))

df_311$Date <- str_extract(df_311$Created_Date, '[:digit:]+/[:digit:]+/[:digit:]+')

df_311_aq <- df_311 %>% inner_join(df_aq_HC, by = 'Date')
str(df_311_aq)


unique(df_311_aq$Complaint_Type)
table(df_311_aq$Complaint_Type)

df_311_aq$Date <- mdy(df_311_aq$Date)

df_wd <- df_311_aq %>% select(Date, Complaint_Type, ozone.AQI, ozone.Health.concern,
                              co.AQI, co.Health.concern, pm.AQI, pm.Health.concern) %>%
  filter(str_detect(Complaint_Type, 'Noise')) %>%
  group_by(Date) %>% mutate(N = n(), Oz = mean(ozone.AQI), wd = wday(Date)) %>%
  mutate(day_ = ifelse(wd %in% c(1,7), 'weekend','workday')) %>% ungroup() %>% select(-wd)

head(df_wd)

set.seed(3456)
trainIndex <- createDataPartition(df_wd$N, p = .8, list = F)
head(trainIndex)
df_wd_train <- df_wd[ trainIndex,]
df_wd_test  <- df_wd[-trainIndex,]

ggplot(data = df_wd_train, aes(y = N, x = day_, fill = day_)) +
  geom_boxplot()# here you can do T-test to see if there is a real difference etc.

df1_t <- df_wd_train %>%
  filter(day_ == "weekend") %>%
  select(N) %>%
  na.omit()

df2_t <- df_wd_train %>%
  filter(day_ == "workday")  %>%
  select(N) %>%
  na.omit()


t.test(x = df1_t$N, y = df2_t$N)

# how about additng Ozone?
ggplot(data = df_wd_train, aes(y = N, x = Oz, color = day_)) +
  geom_point() + stat_smooth(method = "lm")

#how about Carbon Oxide? 
ggplot(data = df_wd_train, aes(y = N, x = co.AQI, color = day_)) +
  geom_point() + stat_smooth(method = "lm")

# how about PM?
ggplot(data = df_wd_train, aes(y = N, x = pm.AQI, color = day_)) +
  geom_point() + stat_smooth(method = "lm")

FitLm <- lm(data = df_wd_train, N ~ Oz)
FitLm
# how big is st.d coefficiencts? How big is a p-value
summary(FitLm)


rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
} # bootstrapping with 1000 replications

results_r2 <- boot(data=df_wd_train, statistic=rsq,
                   R=100, formula= N ~ Oz + co.AQI)
results_r2 
cf <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample
  fit <- lm(formula, data=d)
  return(coef(fit))
} 
results_cf <- boot(data=df_wd_train, statistic=cf,
                   R=100, formula= N ~ Oz + co.AQI)
results_cf

FitLm2_1 <- lm(data = df_wd_train, N ~ Oz + day_)
summary(FitLm2_1)
# How does it looks on the graph?
testF_Oz <- lm(data = df_wd_train, N ~ Oz)

plot(N ~ Oz, data = df_wd_train)
abline(testF_Oz)

testF_Oz_day <- lm(data = df_wd_train, N ~ Oz + day_) 
# change in intersection coefplot(N ~ Oz, data = df_wd_train)
abline(testF_Oz)
abline(testF_Oz_day, col = "red") # # will print line for day_ = weekday
abline(a = coef(testF_Oz_day)[1] + coef(testF_Oz_day)[3],
       b = coef(testF_Oz_day)[2],
       col = "green") # # will print line for day_ = workday# Interaction terms
testF_Oz_Ozday <- lm(data = df_wd_train, N ~ Oz + day_ + Oz:day_) # changes intersection and slope given a value of categorical predictor
testF_Oz_Ozday <- lm(data = df_wd_train, N ~ Oz*day_) # an equivalentsummary(testF_Oz_Ozday)plot(N ~ Oz, data = df_wd_train)

abline(testF_Oz)
abline(testF_Oz_day, col = "red") # # will print line for day_ = weekday
abline(a = coef(testF_Oz_Ozday)[1] + coef(testF_Oz_Ozday)[3],
       b = coef(testF_Oz_Ozday)[2] + coef(testF_Oz_Ozday)[4],
       col = "green") # # will print line for day_ = workday
# More terms. We can add other air parameters
FitLm2_1_2 <- lm(data = df_wd_train, N ~ Oz + co.AQI + pm.AQI + day_)
summary(FitLm2_1_2)
summary( lm(data = df_wd_train, N ~ ((Oz + co.AQI + pm.AQI)*day_)) )

names(df_wd_train)
#if you don't have variables you ignore you can use FitLm2_2 <- lm(data = df_wd_train, N ~ .)
#summary(FitLm2_2)

FitLm2_22 <- lm(data = df_wd_train, 
                N ~ Oz*co.AQI*pm.AQI*day_*Complaint_Type)
summary(FitLm2_22)
length(coef(FitLm2_22))  # 128 coefficients!!!!

FitLm3 <- lm(data = df_wd_train, N ~ Oz + co.AQI + I(Oz^2) + 
               I(co.AQI^2) + day_)
summary(FitLm3)

FitLm5 <- lm(data = df_wd_train, N ~ poly(Oz, 3) + poly(co.AQI, 3) +
               day_)
summary(FitLm5)

head(df_wd_train)
data_1_train <- df_wd_train %>% ungroup() %>% select(-Date)
head(data_1_train)

data_1_test <- df_wd_test %>% ungroup() %>% select(-Date)

# create interaction term inside df. 
#Create dummy variables at the same time
dv <- dummyVars(~ day_ + Oz, data = data_1_train)
dv
predict(dv, data_1_train) %>% head()

dv <- dummyVars(~ (Oz+co.AQI+day_)^2, data = data_1_train)
predict(dv, data_1_train) %>% head()

# all predictors
names(data_1_train)
# before creating dummies, check if some variables should be removed as zero var predictors


########## Near zero var  
# this should be done only for not categorical. 
# Do this before creating dummies
nzv <- nearZeroVar(data_1_train)
nzv # nothing to remove
#data_1_train <- data_1_train[, -nzv]
#data_1_test <- data_1_test[, -nzv]

dv <- dummyVars(~ (Oz+co.AQI+pm.AQI+day_+Complaint_Type)^2, 
                data = data_1_train)
predict(dv, data_1_train) %>% dim  # 62 predictors
data_62_train <- data.frame( predict(dv, data_1_train) )

names(data_62_train)

data_62_test <- data.frame( predict(dv, data_1_test) )
########## Correlated
highlyCorDescr  <- findCorrelation(cor(data_62_train), 
                                   cutoff = .7, verbose = TRUE)
highlyCorDescr 
data_62_train <- data_62_train[,-highlyCorDescr]
data_62_test <- data_62_test[,-highlyCorDescr]

######### Linearly dependent
comboInfo <- findLinearCombos(data_62_train)
comboInfo
data_62_train <- data_62_train[, -comboInfo$remove]
dim(data_62_train)

data_62_test <- data_62_test[, -comboInfo$remove]

######### We do not cover he  re Centering and Scaling, Imputation, Transforming Predictors. Read about this on caret page
subsets <- c(1:5, 10, 15, 20)
#The simulation will fit models with subset sizes of 25, 20, 15, 10, 5, 4, 3, 2, 1.

set.seed(10)
ctrl <- rfeControl(functions = lmFuncs,
                   method = "cv",
                   number = 10,
                   repeats = 5,
                   verbose = TRUE)

lmProfile <- rfe(x = data_62_train, y = data_1_train$N,
                 sizes = subsets,
                 rfeControl = ctrl)

lmProfile
predictors(lmProfile)
#plot
trellis.par.set(caretTheme())
plot(lmProfile, type = c("g", "o"))

# Now we can use only chosen predictors to do lm

train_data <- cbind(data_62_train, data_1_train$N)
names(train_data)[ncol(train_data)] <- "N" 
Fit_test <- lm(N ~ ., data = train_data)
summary(Fit_test)

dv <- dummyVars(~ (Oz+co.AQI+pm.AQI+day_+Complaint_Type)^2, 
                data = data_1_train)
predict(dv, data_1_train) %>% dim  # 62 predictors
data_cor_62_train <- data.frame( predict(dv, data_1_train) )

data_cor_62_test <- data.frame( predict(dv, data_1_test) )


names(data_cor_62_train)


enetGrid <- expand.grid(.alpha = seq(0, 1, 0.1), #Aplha between 0 (ridge) to 1 (lasso).
                        .lambda = seq(0, 10, by = 2))

########################################## training model
ctrl <- trainControl(method = "cv", number = 10,
                     verboseIter = T)
set.seed(1)
enetTune <- train(data_1_train$N ~ ., data = data_cor_62_train,   
                  method = "glmnet", 
                  tuneGrid = enetGrid,
                  trControl = ctrl)

enetTune
enetTune$bestTune
# in enetTune table find R2 corresponding to these values
# it is 0.2798806
# correlation 0.5290374
summary(enetTune$finalModel)

plot(enetTune)

varImp(enetTune)

test_data <- cbind(data_cor_62_test, data_1_test$N)
names(test_data)[ncol(test_data)] <- "N" 

prediction <- predict(enetTune, test_data)
RMSE(pred = prediction, obs = test_data$N) 
# which is little better than rfe obtained model 

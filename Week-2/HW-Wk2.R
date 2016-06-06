## Load in libraries
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmap)
library(stringr)
library(lubridate)
library(boot)
library(glmnet)
library(caret)
library(zoo)

## Read in data
threeoneonedata <- read_csv('data/2015_311.csv')

sel.Boroughs <- c('STATEN ISLAND', 'BROOKLYN', 'MANHATTAN')
df_311 <- threeoneonedata %>% filter(Borough %in% sel.Boroughs) 
df_311$Date <- as.Date(df_311$`Created Date`, format = '%m/%d/%Y ')

complaints <- c('Lead','Radioactive Material','Water Quality',
                'Air Quality','Industrial Waste','Drinking Water',
                'Water System','Drinking','PAINT/PLASTER',
                'PLUMBING', 'General COnstruction/Plumbing')
filtered_df <- df_311 %>% filter(`Complaint Type` %in% complaints) %>%
  select(date_ = Date, `Complaint Type`, Longitude, Latitude)

tidy_df <- filtered_df %>% group_by(date_) %>% summarise(total.counts = n())
dim(filtered_df)
# quick plot, we can see it's only for 2015 so thats the data we will grab
count.plot <- ggplot(data= tidy_df, aes(x = date_, y =total.counts)) + geom_line()
count.plot

# Get lead data
df_pb <- read_csv('data/ad_viz_plotval_data_pb_2015.csv')
df_pb$Date <- mdy(df_pb$Date)
unique(df_pb$DAILY_AQI_VALUE)
unique(df_pb$DAILY_OBS_COUNT)


# grab units
pb.units  <- df_pb$UNITS[1]

df_pb2 <- df_pb %>% select(date_ = Date, Pb.conc = `Daily Mean Pb Concentration`, SITE_LATITUDE, SITE_LONGITUDE)
# Check for duplicates in dates
sum(duplicated(df_pb2$date_))
df_pb3 <- df_pb2 %>% group_by(date_) %>% dplyr::summarise(Pb.conc = mean(Pb.conc))
head(df_pb3)
## every 6 days

# Add in all dates
#df_pb3 <- as.data.frame(filtered_df$date_) %>% full_join(df_pb3, by = 'date_')
pbconc.plot <- ggplot(data= df_pb3, aes(x = date_, y = Pb.conc)) + geom_line()
pbconc.plot

par(mfrow = c(2,1))
pbconc.plot
count.plot
par(mfrow = c(1,1))

map <- get_map(location = 'Staten Island', zoom= 10, maptype = 'watercolor',
               source = 'google', color = 'color')

map <- ggmap(map) + geom_point(data = df_pb2, na.rm = T,
                               aes(x=df_pb2$SITE_LONGITUDE, y=df_pb2$SITE_LATITUDE), color= 'darkred', size = 3)
map


# Join tables

df_tot <- filtered_df %>% inner_join(df_pb3, by = 'date_')

# Interpolate NAs in pb conc
df_tot$Pb.conc <- na.spline(df_tot$Pb.conc, along = index(df_tot$date_), na.rm = T)
df_tot$Pb.conc[df_tot$Pb.conc<0] <- 0
head(df_tot)
# Interpolate NAs in pb conc
pbconc.plot <- ggplot(data= df_tot, aes(x = date_, y = Pb.conc)) + geom_line()
pbconc.plot

df_tot2 <- df_tot %>% dplyr::group_by(date_, `Complaint Type`) %>%
  dplyr::mutate(total.counts = n()) %>% ungroup()

head(df_tot2)

set.seed(3456)
trainIndex <- createDataPartition(df_tot2$total.counts, p = .8, list = F)
head(trainIndex)
df_tot_train <- df_tot2[ trainIndex,]
df_tot_test  <- df_tot2[-trainIndex,]

df1_t <- df_tot_train %>%
  filter(`Complaint Type` == 'Lead') %>%
  select(total.counts, Pb.conc) %>%
  na.omit()

df2_t <- df_tot_test %>%
  filter(`Complaint Type` != 'Lead')  %>%
  select(total.counts, Pb.conc) %>%
  na.omit()



# Plot pb conc vs  
ggplot(data = df1_t, aes(y = total.counts, x = Pb.conc)) +
  geom_point() + stat_smooth(method = "lm")

t.test(x = df1_t$total.counts, y = df2_t$total.counts)

## Linear regression
FitLm <- lm(data = df_tot_train, total.counts ~ Pb.conc)
FitLm
# how big is st.d coefficiencts? How big is a p-value
summary(FitLm)

# multivariate regression 
FitLm2_1 <- lm(data = df_tot_train, total.counts ~ poly(Pb.conc,2)*`Complaint Type`)
summary(FitLm2_1)
length(FitLm2_1)

par(mfrow = c(2,2))
plot(FitLm2_1)
par(mfrow = c(1,1))

#Create dummy variables at the same time
mydv <- dummyVars(~ Pb.conc + `Complaint Type`, data = df_tot_train)
mydv
predict(mydv, df_tot_train) %>% head()

data_new_train <- data.frame(predict(mydv, df_tot_train))
data_new_test <- data.frame(predict(mydv, df_tot_test))

new_highlyCorDescr  <- findCorrelation(cor(data_new_train), 
                                   cutoff = .9, verbose = TRUE)
new_highlyCorDescr 
# none are particularly correlated

new_comboInfo <- findLinearCombos(data_new_train)
new_comboInfo
# none are linearly dependent


set.seed(12213)
index <- createFolds(df_tot_train$total.counts, k = 8, returnTrain = T)

for(i in 1:length(index)){
  crrltn = cor(data_new_train[index[[i]],])     
  caret::findCorrelation(crrltn, cutoff = .90, names = T, verbose = T)
}

subsets <- c(1:5)
set.seed(10)
ctrl <- rfeControl(functions = lmFuncs,
                   method = "repeatedcv",
                   number = 8,
                   index = index,
                   repeats = 5,
                   verbose = TRUE)

lmProfile <- rfe(x = data_new_train, y = df_tot_train$total.counts,
                 sizes = subsets,
                 rfeControl = ctrl)


data_cor_new_train <- data.frame(predict(mydv, df_tot_train))
data_cor_new_test <- data.frame(predict(mydv, df_tot_test))


enetGrid <- expand.grid(.alpha = seq(0, 1, 0.1), #Aplha between 0 (ridge) to 1 (lasso).
                        .lambda = seq(0, 10, by = 2))

ctrl <- trainControl(method = "cv", number = 10,
                     verboseIter = T)
set.seed(1)
enetTune <- train(df_tot_train$total.counts ~ ., data = data_cor_new_train,   
                  method = "glmnet", 
                  tuneGrid = enetGrid,
                  trControl = ctrl)

enetTune
enetTune$bestTune
summary(enetTune$finalModel)
plot(enetTune)
plot(varImp(enetTune))

test_data <- cbind(data_cor_new_test, df_tot_test$total.counts)
names(test_data)[ncol(test_data)] <- "total.counts" 

prediction <- predict(enetTune, test_data)
RMSE(pred = prediction, obs = test_data$total.counts) 

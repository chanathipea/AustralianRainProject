#######
# Importing Data and required package
#######
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(MLeval)) install.packages('MLeval', repos = "http://cran.us.r-project.org")
if(!require(pROC)) install.packages('pROC', repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages('glmnet', repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages('Rborist', repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages('xgboost', repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages('e1071', repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(lubridate)
library(pROC)
library(MLeval)
library(doParallel)
library(glmnet)
library(Rborist)
library(xgboost)
library(e1071)

url <- "https://raw.githubusercontent.com/chanathipea/AustralianRainProject/main/weatherAUS.csv"
tmp_filename <- tempfile()
download.file(url, tmp_filename)
dat <- read_csv(tmp_filename)
file.remove(tmp_filename)

ausrain <- as.data.frame(dat)

#########
# Data cleansing and Data manipulation
#########
# Total row is 145460 with 23 columns
dim(ausrain)
colnames(ausrain)

summary(ausrain)
str(ausrain)

# We are trying to predict rainning on tomorrow day, so NA of RainTomorrow will be filtered out
ausrain <- ausrain %>% filter(!is.na(RainTomorrow))

# Confirm no incorrect record by seeing the unique value before convert to factor
ausrain %>% summarize(Today = unique(RainToday))
ausrain %>% summarize(Tomorrow = unique(RainTomorrow))
ausrain %>% summarize(WindDir = unique(WindGustDir))
ausrain %>% summarize(Wind9am = unique(WindDir9am))
ausrain %>% summarize(Wind3pm = unique(WindDir3pm))
ausrain %>% summarize(Wind3pm = unique(Location))

ausrain <- ausrain %>% mutate(RainToday = factor(RainToday, levels = c("Yes", "No")),
                              RainTomorrow = factor(RainTomorrow, levels = c("Yes", "No")),
                              WindGustDir = as.factor(WindGustDir),
                              WindDir9am = as.factor(WindDir9am),
                              WindDir3pm = as.factor(WindDir3pm),
                              Location = as.factor(Location))

# Evaporation NA 143636 rows, Sunshine NA 142771 rows, Cloud9am NA 55888 rows, Cloud3pm NA 59358 rows.
# Tryin to remove those columns from analysis

ausrain <- ausrain %>% select(-c(Evaporation , Sunshine, Cloud9am, Cloud3pm))

# The rest of missing values are explore separately
nadata <- ausrain %>% mutate(WGD = is.na(WindGustDir),
                             WGS = is.na(WindGustSpeed),
                             WD9 = is.na(WindDir9am),
                             WD3 = is.na(WindDir3pm),
                             WS9 = is.na(WindSpeed9am),
                             WS3 = is.na(WindSpeed3pm),
                             HD9 = is.na(Humidity9am),
                             HD3 = is.na(Humidity3pm),
                             P9na = is.na(Pressure9am),
                             P3na = is.na(Pressure3pm),
                             T9 = is.na(Temp9am),
                             T3 = is.na(Temp3pm))

# Whole Newcastle and Albany do not have WindGustDir and WindGustSpeed
# If we filter out the NA observations in WindGustDir and WindGustSpeed, the whole 2 locations observation will be missing
# In order to retain 2 locations, those 2 columns will be removed.
# whole Newcastle and Albany
nadata %>% ggplot(aes(x = Date, y = WGD )) + 
  geom_point(alpha = 0.5, size = 0.3) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Location) + 
  labs(title = 'WindGustDir NA value by location')
nadata %>% ggplot(aes(x = Location, fill = WGD)) +
  geom_bar(position = 'fill') + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = 'WindGustDir NA value ratio by location') +
  theme(legend.position = 'bottom')

# whole Newcastle and Albany
nadata %>% ggplot(aes(x = Date, y = WGS)) + 
  geom_point(alpha = 0.5, size = 0.3) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Location) + 
  labs(title = 'WindGustSpeed NA value by location')
nadata %>% ggplot(aes(x = Location, fill = WGS)) +
  geom_bar(position = 'fill') + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = 'WindGustSpeed NA value ratio by location') +
  theme(legend.position = 'bottom')

# Pressure9am and Pressure3pm still have about 10% of NA value
# MountGinini, Newcastle, Penrith and SalmonGums do not have Pressure9am and Pressure3pm
# If we filter out the NA observations, the whole 4 locations observation will be missing
# In order to retain 4 locations, those 2 columns will be removed.
# Remove
nadata %>% ggplot(aes(x = Date, y = P9na)) + 
  geom_point(alpha = 0.5, size = 0.3) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Location) + 
  labs(title = 'Pressure9am NA value by location')
nadata %>% ggplot(aes(x = Location, fill = P9na)) +
  geom_bar(position = 'fill') + 
  scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = 'Pressure9am NA value ratio by location') +
  theme(legend.position = 'bottom')

# Remove
nadata %>% ggplot(aes(x = Date, y = P3na)) + 
  geom_point(alpha = 0.5, size = 0.3) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~Location) + 
  labs(title = 'Pressure3pm NA value by location')
nadata %>% ggplot(aes(x = Location, fill = P3na)) +
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 90)) + 
  labs(title = 'Pressure3pm NA value ratio by location') +
  theme(legend.position = 'bottom')

nadata %>% ggplot(aes(x = Date, y = WD9)) + 
  geom_point(alpha = 0.5, size = 0.3) + 
  facet_wrap(~Location)
nadata %>% ggplot(aes(x = Location, fill = WD9)) +
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 30))

nadata %>% ggplot(aes(x = Date, y = WD3)) + 
  geom_point(alpha = 0.5, size = 0.3) + 
  facet_wrap(~Location)
nadata %>% ggplot(aes(x = Location, fill = WD3)) +
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 30))

nadata %>% ggplot(aes(x = Date, y = WS9)) + 
  geom_point(alpha = 0.5, size = 0.3) + 
  facet_wrap(~Location)
nadata %>% ggplot(aes(x = Location, fill = WS9)) +
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 30))

nadata %>% ggplot(aes(x = Date, y = WS3)) + 
  geom_point(alpha = 0.5, size = 0.3) + 
  facet_wrap(~Location)
nadata %>% ggplot(aes(x = Location, fill = WS3)) +
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 30))

nadata %>% ggplot(aes(x = Date, y = HD9)) + 
  geom_point(alpha = 0.5, size = 0.3) + 
  facet_wrap(~Location)
nadata %>% ggplot(aes(x = Location, fill = HD9)) +
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 30))

nadata %>% ggplot(aes(x = Date, y = HD3)) + 
  geom_point(alpha = 0.5, size = 0.3) + 
  facet_wrap(~Location)
nadata %>% ggplot(aes(x = Location, fill = HD3)) +
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 30))

nadata %>% ggplot(aes(x = Date, y = T9)) + 
  geom_point(alpha = 0.5, size = 0.3) + 
  facet_wrap(~Location)
nadata %>% ggplot(aes(x = Location, fill = T9)) +
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 30))

nadata %>% ggplot(aes(x = Date, y = T3)) + 
  geom_point(alpha = 0.5, size = 0.3) + 
  facet_wrap(~Location)
nadata %>% ggplot(aes(x = Location, fill = T3)) +
  geom_bar() + scale_x_discrete(guide = guide_axis(angle = 30))

# 4 variables are removed
ausrain <- ausrain %>% select(-c(Pressure9am, Pressure3pm, WindGustDir, WindGustSpeed))

ausrain <- ausrain %>% drop_na()

# Get month variable
ausrain <- ausrain %>% mutate(Month = month(Date)) %>% mutate(Month = as.factor(Month))

summary(ausrain)

#####
# Exploratory data analysis
#####
summary(ausrain)
str(ausrain)

# Target Variable Analysis
ausrain %>% ggplot(aes(x = RainTomorrow, fill = RainTomorrow)) +
  geom_bar() + 
  labs(title = 'Number of Yes vs No Raintomorrow') +
  theme(legend.position = 'bottom')

ausrain %>% group_by(RainTomorrow) %>% summarize(count = n())

## Explore by location
# There is significant difference of rain probability on each Location  
ausrain %>% ggplot(aes(x = Location, fill = RainTomorrow)) +
  geom_bar(position = 'fill') + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = 'Normalized Ratio of RainTomorrow by Location') +
  theme(legend.position = 'bottom')

ausrain %>% group_by(Location) %>% 
  summarize(Ytomorrow = sum(RainTomorrow == 'Yes'),
            Total = n(), Rainratio = Ytomorrow/Total) %>%
  arrange(Rainratio) %>%
  ggplot(aes(x = reorder(Location,Rainratio), y = Rainratio)) + 
  geom_col() + scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = 'Probability of RainTomorrow by Location') + 
  xlab('Location')

## Explore by WindDir9am
ausrain %>% ggplot(aes(x = WindDir9am, fill = RainTomorrow)) + 
  geom_bar(position = 'fill') + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = 'Normalized Ratio of RainTomorrow by WindDir9am') +
  theme(legend.position = 'bottom')

ausrain %>% group_by(WindDir9am) %>% 
  summarize(Ytomorrow = sum(RainTomorrow == 'Yes'),
            Total = n(), Rainratio = Ytomorrow/Total) %>%
  arrange(Rainratio) %>%
  ggplot(aes(x = reorder(WindDir9am,Rainratio), y = Rainratio)) + 
  geom_col() + scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = 'Probability of RainTomorrow by WindDir9am') + 
  xlab('WindDirection')

## Explore by WindDir3pm
# small difference between each wind direction
ausrain %>% ggplot(aes(x = WindDir3pm, fill = RainTomorrow)) + 
  geom_bar(position = 'fill') + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = 'Normalized Ratio of RainTomorrow by WindDir3pm')  +
  theme(legend.position = 'bottom')

ausrain %>% group_by(WindDir3pm) %>% 
  summarize(Ytomorrow = sum(RainTomorrow == 'Yes'),
            Total = n(), Rainratio = Ytomorrow/Total) %>%
  arrange(Rainratio) %>%
  ggplot(aes(x = reorder(WindDir3pm,Rainratio), y = Rainratio)) + 
  geom_col() + scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = 'Probability of RainTomorrow by WindDir3pm') + 
  xlab('WindDirection')

## Explore by RainToday
ausrain %>% ggplot(aes(x = RainToday, fill = RainTomorrow)) + 
  geom_bar(position = 'fill') +
  labs(title = 'Normalized Ratio of RainTomorrow by RainToday')  +
  theme(legend.position = 'bottom')

ausrain %>% group_by(RainToday) %>% 
  summarize(Ytomorrow = sum(RainTomorrow == 'Yes'),
            Total = n(), Rainratio = Ytomorrow/Total) %>%
  arrange(Rainratio) %>%
  ggplot(aes(x = RainToday, y = Rainratio)) + 
  geom_col() +
  labs(title = 'Probability of RainTomorrow if it RainToday') + 
  xlab('RainToday')

## Explore by Month
ausrain %>% ggplot(aes(x = Month, fill = RainTomorrow)) + 
  geom_bar(position = 'fill') +
  labs(title = 'Normalized Ratio of RainTomorrow by Month')  +
  theme(legend.position = 'bottom')

ausrain %>% group_by(Month) %>% 
  summarize(Ytomorrow = sum(RainTomorrow == 'Yes'),
            Total = n(), Rainratio = Ytomorrow/Total) %>%
  arrange(Rainratio) %>%
  ggplot(aes(x = Month, y = Rainratio)) + 
  geom_col() +
  labs(title = 'Probability of Rain by Month') + 
  xlab('Month')

## pairplot
ausrain %>% select(MinTemp, MaxTemp, Rainfall, WindSpeed9am, 
                   WindSpeed3pm, Humidity9am, Humidity3pm, 
                   Temp9am, Temp3pm) %>% 
  cor() %>% heatmap(Colv = NA, Rowv = NA)

## Mintemp (not significant)
# not clear difference in Mean
ausrain %>% ggplot(aes(y = MinTemp , x = RainTomorrow)) +
  geom_boxplot() +
  labs(title = 'MinTemp comparison between class of target variable')

# separate into each location, mintemp has no clear relationship with tomorrow raining.
# Such as in AliceSprings, mean mintemp is higher if tomorrow is raining. 
# In contrast, in Albany, mintemp is lower if tomorrow is raining.
ausrain %>% ggplot(aes(x = RainTomorrow, y = MinTemp)) + 
  geom_boxplot()+ facet_wrap(~Location, ncol = 10) +
  labs(title = 'MinTemp comparison between class of target variable by location')

# not clear difference in Mean
ausrain %>% ggplot(aes(x = RainTomorrow, y = MinTemp)) + 
  geom_boxplot()+ facet_wrap(~WindDir9am)

ausrain %>% ggplot(aes(x = RainTomorrow, y = MinTemp)) + 
  geom_boxplot()+ facet_wrap(~WindDir3pm)

ausrain %>% ggplot(aes(x = RainTomorrow, y = MinTemp)) + 
  geom_boxplot()+ facet_wrap(~RainToday)

ausrain %>% ggplot(aes(x = RainTomorrow, y = MinTemp)) + 
  geom_boxplot()+ facet_wrap(~Month)

## Maxtemp (there is some difference in mean even not very significant)
ausrain %>% ggplot(aes(y = MaxTemp , x = RainTomorrow)) +
  geom_boxplot() +
  labs(title = 'MaxTemp comparison between class of target variable')

# At least all location have a lower Maxtemp if it is raining tomorrow.
ausrain %>% ggplot(aes(x = RainTomorrow, y = MaxTemp)) + 
  geom_boxplot()+ facet_wrap(~Location)

ausrain %>% ggplot(aes(x = RainTomorrow, y = MaxTemp)) + 
  geom_boxplot()+ facet_wrap(~WindDir9am)

ausrain %>% ggplot(aes(x = RainTomorrow, y = MaxTemp)) + 
  geom_boxplot()+ facet_wrap(~WindDir3pm)

ausrain %>% ggplot(aes(x = RainTomorrow, y = MaxTemp)) + 
  geom_boxplot()+ facet_wrap(~RainToday)

ausrain %>% ggplot(aes(x = RainTomorrow, y = MaxTemp)) + 
  geom_boxplot()+ facet_wrap(~Month)

## Rainfall (should not be used)
ausrain %>% ggplot(aes(y = Rainfall , x = RainTomorrow)) +
  geom_boxplot() +
  labs(title = 'Rainfall comparison between class of target variable')

ausrain %>% ggplot(aes(x = RainTomorrow, y = Rainfall)) + 
  geom_boxplot()+ facet_wrap(~Location)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Rainfall)) + 
  geom_boxplot()+ facet_wrap(~WindDir9am)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Rainfall)) + 
  geom_boxplot()+ facet_wrap(~WindDir3pm)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Rainfall)) + 
  geom_boxplot()+ facet_wrap(~RainToday)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Rainfall)) + 
  geom_boxplot()+ facet_wrap(~Month)

## Explore by WindSpeed9am (Not significant)
ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed9am)) + 
  geom_boxplot() +
  labs(title = 'Windspeed9am comparison between class of target variable')

ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed9am)) + 
  geom_boxplot()+ facet_wrap(~Location)

ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed9am)) + 
  geom_boxplot()+ facet_wrap(~WindDir9am)

ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed9am)) + 
  geom_boxplot()+ facet_wrap(~WindDir3pm)

ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed9am)) + 
  geom_boxplot()+ facet_wrap(~RainToday)

ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed9am)) + 
  geom_boxplot()+ facet_wrap(~Month)

## Explore by WindSpeed3pm (Not significant)
ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed3pm)) + 
  geom_boxplot() +
  labs(title = 'Windspeed3pm comparison between class of target variable')

ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed3pm)) + 
  geom_boxplot()+ facet_wrap(~Location)

ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed3pm)) + 
  geom_boxplot()+ facet_wrap(~WindDir9am)

ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed3pm)) + 
  geom_boxplot()+ facet_wrap(~WindDir3pm)

ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed3pm)) + 
  geom_boxplot()+ facet_wrap(~RainToday)

ausrain %>% ggplot(aes(x = RainTomorrow, y = WindSpeed3pm)) + 
  geom_boxplot()+ facet_wrap(~Month)

## Explore by Humidity9am (Selected)
ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity9am)) + 
  geom_boxplot() +
  labs(title = 'Humidity9am comparison between class of target variable')

ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity9am)) + 
  geom_boxplot()+ facet_wrap(~Location)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity9am)) + 
  geom_boxplot()+ facet_wrap(~WindDir9am)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity9am)) + 
  geom_boxplot()+ facet_wrap(~WindDir3pm)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity9am)) + 
  geom_boxplot()+ facet_wrap(~RainToday)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity9am)) + 
  geom_boxplot()+ facet_wrap(~Month)

## Explore by Humidity3pm (Selected)
ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity3pm)) + 
  geom_boxplot() +
  labs(title = 'Humidity3pm comparison between class of target variable')

ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity3pm)) + 
  geom_boxplot()+ facet_wrap(~Location)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity3pm)) + 
  geom_boxplot()+ facet_wrap(~WindDir9am)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity3pm)) + 
  geom_boxplot()+ facet_wrap(~WindDir3pm)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity3pm)) + 
  geom_boxplot()+ facet_wrap(~RainToday)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Humidity3pm)) + 
  geom_boxplot()+ facet_wrap(~Month)

## Explore by Temp9am (not significant)
ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp9am)) + 
  geom_boxplot() +
  labs(title = 'Temp9am comparison between class of target variable')

ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp9am)) + 
  geom_boxplot()+ facet_wrap(~Location)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp9am)) + 
  geom_boxplot()+ facet_wrap(~WindDir9am)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp9am)) + 
  geom_boxplot()+ facet_wrap(~WindDir3pm)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp9am)) + 
  geom_boxplot()+ facet_wrap(~RainToday)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp9am)) + 
  geom_boxplot()+ facet_wrap(~Month)

## Explore by Temp3pm (Selected)
ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp3pm)) + 
  geom_boxplot() +
  labs(title = 'Temp3pm comparison between class of target variable')

ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp3pm)) + 
  geom_boxplot()+ facet_wrap(~Location)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp3pm)) + 
  geom_boxplot()+ facet_wrap(~WindDir9am)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp3pm)) + 
  geom_boxplot()+ facet_wrap(~WindDir3pm)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp3pm)) + 
  geom_boxplot()+ facet_wrap(~RainToday)

ausrain %>% ggplot(aes(x = RainTomorrow, y = Temp3pm)) + 
  geom_boxplot()+ facet_wrap(~Month)

##
summary(ausrain)
str(ausrain)

###### Model Fitting
cl <- makePSOCKcluster(detectCores())
registerDoParallel(cl)

set.seed(1, sample.kind = 'Rounding')

TrainIndex <- createDataPartition(ausrain$RainTomorrow, times = 1, p = 0.80, list = FALSE)
Trainset <- ausrain[TrainIndex,]
Testset <- ausrain[-TrainIndex,]

## Train control
TrainCon <- trainControl(method = "cv", 
                         number = 5, 
                         search = 'grid',
                         summaryFunction = twoClassSummary,
                         classProbs = TRUE,
                         savePredictions = TRUE)

## Initiate variable importance function.
importance <- function(a){
  df <- as.data.frame(a)
  df <- df %>% mutate(name = rownames(df)) 
  df1 <- df %>% mutate(name = ifelse(str_detect(df$name, "^Location"), 'Location', name),
                       name = ifelse(str_detect(df$name, "^WindDir3pm"), 'WindDir3pm', name),
                       name = ifelse(str_detect(df$name, "^WindDir9am"), 'WindDir9am', name),
                       name = ifelse(str_detect(df$name, "^Month"), 'Month', name),
                       name = ifelse(str_detect(df$name, "^RainToday"), 'RainToday', name))
  df1 <- df1 %>% group_by(name) %>% summarize(importance = sum(Overall)) %>% arrange(desc(importance))
  return(df1)
}

### GLM 
fitGLM <- train(RainTomorrow ~ .,
               data = Trainset %>% select(-Date),
               method = "glm",
               trControl = TrainCon)

fitGLM
confusionMatrix(fitGLM, norm = 'none')

# GLM varImp
varImp(fitGLM, scale = FALSE)
varImpGLM <- varImp(fitGLM, scale = FALSE)
importance(varImpGLM$importance) %>% 
  ggplot(aes(x = reorder(name, -importance), y = importance)) + 
  geom_col() + 
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = 'Variable importance from Logistic Regression') +
  xlab(NULL)

# GLM testset prediction
predGLM <- predict(fitGLM, Testset)
postResample(pred = predGLM, obs = Testset$RainTomorrow)
confusionMatrix(data = predGLM, reference = Testset$RainTomorrow, positive = 'Yes')

## GLM ROC
# GLM ROC trainset prediction
roctrainGLM <- roc(response = fitGLM$pred[,'obs'],
                    predictor = fitGLM$pred[,'Yes'])
plot(roctrainGLM, print.thres = 'best')

auctrainGLM <- auc(roctrainGLM)
print(auctrainGLM)

thresholdGLM = coords(roctrainGLM, "best")[1,'threshold']

# GLM ROC testset prediction
probtestGLM <- predict(fitGLM, Testset, type = 'prob')
head(probtestGLM)

roctestGLM <- roc(response = Testset$RainTomorrow,
                     predictor = probtestGLM[, "Yes"],
                     levels = rev(levels(Testset$RainTomorrow)))
plot(roctestGLM, print.thres = 'best')

auc(roctestGLM)

predtestGLM <- as.factor(ifelse(probtestGLM["Yes"] >= thresholdGLM, 'Yes', 'No'))
confMatGLM <- confusionMatrix(data = predtestGLM, reference = Testset$RainTomorrow, positive = 'Yes')
print(confMatGLM)

auctestGLM <- auc(roctestGLM)
print(auctestGLM)

result <- data_frame(Model = 'Logistic Regression', 
                     TestAUC = auctestGLM,
                     TrainAUC = auctrainGLM,
                     Sensitivity = confMatGLM$byClass['Sensitivity'], 
                     Specificity = confMatGLM$byClass['Specificity'],
                     Accuracy = confMatGLM$overall['Accuracy'])
result %>% knitr::kable()

### GLMNET
# Tune grid
tuneGridGLMNET <- expand.grid(alpha = c(seq(0,1,0.2)),
                        lambda = c(seq(0,1,0.2)))
# Fit
fitGLMNET <- train(RainTomorrow ~ .,
                data = Trainset %>% select(-Date),
                method = "glmnet",
                trControl = TrainCon,
                tuneGrid = tuneGridGLMNET,
                family = 'binomial')

fitGLMNET
plot(fitGLMNET)
confusionMatrix(fitGLMNET, norm = 'none')

# GLMNET varImp
varImp(fitGLMNET, scale = FALSE)
varImpGLMNET <- varImp(fitGLMNET, scale = FALSE)
importance(varImpGLMNET$importance) %>% 
  ggplot(aes(x = reorder(name, -importance), y = importance)) + 
  geom_col() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = 'Variable importance from Regularized Logistic Regression') +
  xlab(NULL)

# GLMNET testset prediction
predGLMNET <- predict(fitGLMNET, Testset)
postResample(pred = predGLMNET, obs = Testset$RainTomorrow)
confusionMatrix(data = predGLMNET, reference = Testset$RainTomorrow, positive = 'Yes')

## GLMNET ROC
# GLMNET ROC trainset prediction
roctrainGLMNET <- roc(response = fitGLMNET$pred[,'obs'],
                      predictor = fitGLMNET$pred[,'Yes'])
plot(roctrainGLMNET, print.thres = 'best')

auctrainGLMNET <- auc(roctrainGLM)
print(auctrainGLMNET)

thresholdGLMNET = coords(roctrainGLMNET, "best")[1,'threshold']
print(thresholdGLMNET)

# GLMNET ROC testset prediction
probtestGLMNET <- predict(fitGLMNET, Testset, type = 'prob')
probtestGLMNET
roctestGLMNET <- roc(response = Testset$RainTomorrow,
                 predictor = probtestGLMNET[, "Yes"],
                 levels = rev(levels(Testset$RainTomorrow)))
plot(roctestGLMNET, print.thres = 'best')
roctestGLMNET

predtestGLMNET <- as.factor(ifelse(probtestGLMNET["Yes"] >= thresholdGLMNET, 'Yes', 'No'))
confMatGLMNET <- confusionMatrix(data = predtestGLMNET, reference = Testset$RainTomorrow, positive = 'Yes')
print(confMatGLMNET)

auctestGLMNET <- auc(roctestGLMNET)
print(auctestGLMNET)

result <- rbind(result, data.frame(Model = 'Regularized Logistic Regression',
                                   TestAUC = auctestGLMNET,
                                   TrainAUC = auctrainGLMNET,
                                   Sensitivity = confMatGLMNET$byClass['Sensitivity'],
                                   Specificity = confMatGLMNET$byClass['Specificity'],
                                   Accuracy = confMatGLMNET$overall['Accuracy']))
rownames(result) <- NULL
result %>% knitr::kable()
### Random Forest
# Tune Grid
tuneGridRF <- expand.grid(minNode = c(1,5), predFixed = c(2,3,4))
# Fit
fitRF <- train(RainTomorrow ~ .,
               data = Trainset %>% select(-Date),
               method = "Rborist",
               importance=TRUE,
               nTree = 500,
               tunegrid = tuneGridRF,
               trControl = TrainCon)

fitRF
plot(fitRF)
confusionMatrix(fitRF, norm = 'none')

# RF varImp
varImp(fitRF, scale = FALSE)
varImpRF <- varImp(fitRF, scale = FALSE)
importance(varImpRF$importance) %>% 
  ggplot(aes(x = reorder(name, -importance), y = importance)) + 
  geom_col() +
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = 'Variable importance from Random Forest') +
  xlab(NULL)

# RF testset prediction
predRF <- predict(fitRF, Testset)
postResample(pred = predRF, obs = Testset$RainTomorrow)
confusionMatrix(data = predRF, reference = Testset$RainTomorrow, positive = 'Yes')

## RF ROC
# RF ROC trainset prediction
roctrainRF <- roc(response = fitRF$pred[,'obs'],
                  predictor = fitRF$pred[,'Yes'],
                  levels = rev(levels(Trainset$RainTomorrow)))
plot(roctrainRF, print.thres = 'best')

auctrainRF <- auc(roctrainRF)
print(auctrainRF)

thresholdRF = coords(roctrainRF, "best")[1,'threshold']
print(thresholdRF)

# RF ROC testset prediction
probtestRF <- predict(fitRF, Testset, type = 'prob')
probtestRF

roctestRF <- roc(response = Testset$RainTomorrow,
                 predictor = probtestRF[, "Yes"],
                 levels = rev(levels(Testset$RainTomorrow)))
plot(roctestRF, print.thres = 'best')

roctestRF

predtestRF <- as.factor(ifelse(probtestRF["Yes"] >= thresholdRF, 'Yes', 'No'))
confMatRF <- confusionMatrix(data = predtestRF, reference = Testset$RainTomorrow, positive = 'Yes')
print(confMatRF)

auctestRF <- auc(roctestRF)
print(auctestRF)

result <- rbind(result, data.frame(Model = 'Random Forest',
                                   TestAUC = auctestRF,
                                   TrainAUC = auctrainRF,
                                   Sensitivity = confMatRF$byClass['Sensitivity'],
                                   Specificity = confMatRF$byClass['Specificity'],
                                   Accuracy = confMatRF$overall['Accuracy']))
rownames(result) <- NULL
result %>% knitr::kable()

### XGBoost
# Tune Grid
tuneGridXGB <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = c(0.1, 0.3, 0.5),
  gamma = c(0, 10, 30, 50),
  colsample_bytree = c(0.1,0.3,0.5,0.7,1),
  min_child_weight = 1,
  subsample = 1
)
# Fit
fitXGB <- train(RainTomorrow ~ .,
                data = Trainset,
                trControl = TrainCon,
                tuneGrid = tuneGridXGB,
                method = "xgbTree",
                verbose = TRUE
)

fitXGB
plot(fitXGB)
confusionMatrix(fitXGB, norm = 'none')

# XGB varImp
varImp(fitXGB, scale = FALSE)
importance(varImpXGB$importance) %>% 
  ggplot(aes(x = reorder(name, -importance), y = importance)) + 
  geom_col()+
  scale_x_discrete(guide = guide_axis(angle = 90)) +
  labs(title = 'Variable importance from Extreme Gradient Boosting') +
  xlab(NULL)

# XGB testset prediction
predXGB <- predict(fitXGB, Testset)
postResample(pred = predXGB, obs = Testset$RainTomorrow)
confusionMatrix(data = predXGB, reference = Testset$RainTomorrow, positive = 'Yes')

## XGB ROC
# XGB ROC trainset prediction
roctrainXGB <- roc(response = fitXGB$pred[,'obs'],
                  predictor = fitXGB$pred[,'Yes'],
                  levels = rev(levels(Trainset$RainTomorrow)))
plot(roctrainXGB, print.thres = 'best')

auctrainXGB <- auc(roctrainXGB)
print(auctrainXGB)

thresholdXGB = coords(roctrainXGB, "best")[1,'threshold']
print(thresholdXGB)

# XGB ROC testset prediction
probtestXGB <- predict(fitXGB, Testset, type = 'prob')
probtestXGB

roctestXGB <- roc(response = Testset$RainTomorrow,
                 predictor = probtestXGB[, "Yes"],
                 levels = rev(levels(Testset$RainTomorrow)))
plot(roctestXGB, print.thres = 'best')

roctestXGB

predtestXGB <- as.factor(ifelse(probtestXGB["Yes"] >= thresholdXGB, 'Yes', 'No'))
confMatXGB <- confusionMatrix(data = predtestXGB, reference = Testset$RainTomorrow, positive = 'Yes')
print(confMatXGB)

auctestXGB <- auc(roctestXGB)
print(auctestXGB)

result <- rbind(result, data.frame(Model = 'Extreme Gradient Boosting',
                                   TestAUC = auctestXGB,
                                   TrainAUC = auctrainXGB,
                                   Sensitivity = confMatXGB$byClass['Sensitivity'],
                                   Specificity = confMatXGB$byClass['Specificity'],
                                   Accuracy = confMatXGB$overall['Accuracy']))
rownames(result) <- NULL
result %>% knitr::kable()

registerDoSEQ()
stopCluster(cl)
gc(reset = TRUE)

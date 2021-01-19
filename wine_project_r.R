### Logistic Regression Project 

# Import packages and link them to project
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(dplyr)
library(reshape2)
library(caret)
library(car)
library(MASS)
# STEP 1: Import and clean data
## Sep = ";" removed the semicolon seperator values from the dataset 
## Check for na values 
sum(is.na(wine))
# None!
wine <- read.csv("~/Desktop/Grad School/Quantitative Methods/Project/wine_csv.csv", sep=";")
# plot everything vs everything and eyeball trends 
ggplot(wine, aes(x=fixed.acidity, y=quality)) + geom_point() + 
  geom_smooth(method = 'lm')+ 
  labs(title = 'Quality vs Fixed Acidity',
       x='Fixed Acidity', y='Quality') +
  theme_classic()

ggplot(wine, aes(x=volatile.acidity, y=quality)) + geom_point() + 
  geom_smooth(method = 'lm')+ 
  labs(title = 'Quality vs Volatile Acidity',
       x='Volatile Acidity', y='Quality') +
  theme_classic()

ggplot(wine, aes(x=citric.acid, y=quality)) + geom_point() + 
  geom_smooth(method = 'lm')+ 
  labs(title = 'Quality vs Citric Acid',
       x='Citric Acidity', y='Quality') +
  theme_classic()

ggplot(wine, aes(x=residual.sugar, y=quality)) + geom_point() + 
  geom_smooth(method = 'lm')+ 
  labs(title = 'Residual Sugar vs Quality',
       x='Residual Sugar', y='Quality') +
  theme_classic()

ggplot(wine, aes(x=chlorides, y=quality)) + geom_point() + 
  geom_smooth(method = 'lm')+ 
  labs(title = 'Chlorides vs Quality',
       x='pH', y='Quality') +
  theme_classic()

ggplot(wine, aes(x=free.sulfur.dioxide, y=quality)) + geom_point() + 
  geom_smooth(method = 'lm')+ 
  labs(title = 'Free Sulfur Dioxide vs Quality',
       x='Free Sulfur Dioxide', y='Quality') +
  theme_classic()

ggplot(wine, aes(x=total.sulfur.dioxide, y=quality)) + geom_point() + 
  geom_smooth(method = 'lm')+ 
  labs(title = 'Free Sulfur Dioxide vs Quality',
       x='Total Sulfur Dioxide', y='Quality') +
  theme_classic()

ggplot(wine, aes(x=density, y=quality)) + geom_point() + 
  geom_smooth(method = 'lm')+ 
  labs(title = 'Density vs Quality',
       x='pH', y='Quality') +
  theme_classic()

ggplot(wine, aes(x=pH, y=quality)) + geom_point() + 
  geom_smooth(method = 'lm')+ 
  labs(title = 'pH vs Quality',
       x='pH', y='Quality') +
  theme_classic()

ggplot(wine, aes(x=sulphates, y=quality)) + geom_point() + 
  geom_smooth(method = 'lm')+ 
  labs(title = 'Sulphates vs Quality',
       x='Sulphates', y='Quality') +
  theme_classic()

ggplot(wine, aes(x=alcohol, y=quality)) + geom_point() + 
  geom_smooth(method = 'lm')+ 
  labs(title = 'Alcohol vs Quality',
       x='Alcohol', y='Quality') +
  theme_classic()

# check Pearson correlations
wineCol <- colnames(wine)
wine_pearCor <- c(cor(wine$quality, wine$fixed.acidity , method = 'pearson'),
cor(wine$quality, wine$volatile.acidity , method = 'pearson'),
cor(wine$quality, wine$citric.acid, method = 'pearson'),
cor(wine$quality, wine$residual.sugar, method = 'pearson'),
cor(wine$quality, wine$chlorides , method = 'pearson'),
cor(wine$quality, wine$free.sulfur.dioxide , method = 'pearson'),
cor(wine$quality, wine$total.sulfur.dioxide , method = 'pearson'),
cor(wine$quality, wine$density , method = 'pearson'),
cor(wine$quality, wine$pH , method = 'pearson'),
cor(wine$quality, wine$sulphates , method = 'pearson'),
cor(wine$quality, wine$alcohol , method = 'pearson'),
cor(wine$quality, wine$quality , method = 'pearson')
)
wineStats2<- t(wineStats)
wineStats2

# Check the Variance Inflaction Factor VIF 
# split the data into training and test set 
set.seed(123)
training.samples <- wine$quality %>%
  createDataPartition(p = .08, list = FALSE)
train.data <- wine[training.samples, ]
test.data <- wine[-training.samples, ]
# Build the regression model 
VIF_wineModel <- lm(quality ~., data = train.data)
# Make predictions
predicitons <- VIF_wineModel %>% predict(test.data)
# Model performance 
data.frame(
  RMSE = RMSE(predicitons, test.data$quality),
  R2 = R2(predicitons, test.data$quality)
)
# Check the VIF using vif()
wineModel1 <- as.data.frame(vif(VIF_wineModel))
wineModel1
# Fixed Acidity and Density have high VIF, meaning that we should remove them from our model.
# We use stepAIC to improve the model further
wineMLR <- lm(quality ~ ., data = wine)
wineModel <- stepAIC(wineMLR, direction = "both")
summary(wineMLR)
wineCoef <- as.data.frame(summary(wineMLR)$coefficients[,4])
wineCoef
head(wine)
wineMLR_data <- subset(wine, select=-c(density, fixed.acidity, residual.sugar, citric.acid))
head(wineMLR_data)
# Run multiple linear regression
summary(lm(quality~ ., data = wineMLR_data))


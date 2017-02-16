# Load packages
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
library('mice') # imputation
library('randomForest') # classification algorithm

train <- read.csv('/Users/nitingangasagar/Desktop/BA with R/Project/train.csv', stringsAsFactors = F)
test  <- read.csv('/Users/nitingangasagar/Desktop/BA with R/Project/test.csv', stringsAsFactors = F)

full  <- bind_rows(train,test)

table(full$Sex, full$Title)

table(full$Pclass, full$Titles)
full$Titles <- gsub("Dona|Lady|Madame|the Countess", "Lady", full$Titles)
full$Titles <- gsub("Don|Jonkheer|Sir", "Sir", full$Titles)

unique(full$Titles)

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

table(full$Sex, full$Title)

few_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% few_title]  <- 'Rare Title'



table(full$Sex, full$Title)


full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

str(full)

full$Titles = NULL

full$Familysize <- full$SibSp + full$Parch + 1

full$Family <- paste(full$Surname, full$Familysize, sep='_')


ggplot(full[1:891,], 
  aes(x = Familysize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()


scatter.ggplot <- ggplot(full, aes(x=Familysize, y=count, colour=Survived), scale_x_continuous(breaks=c(1:11))) + geom_point()
scatter.ggplot


scatter.ggvis <- full %>% ggvis(x = ~Familysize, y = ~count, fill = ~Survived) %>% 
  layer_points() %>% set_options(width = img.width, height = img.height)
scatter.ggvis

hist(full$Familysize, breaks=12, col="red")

install.packages("ggvis")

full$FamilysizeD[full$Familysize == 1] <- 'singleton'
full$FamilysizeD[full$Familysize < 5 & full$Familysize > 1] <- 'small'
full$FamilysizeD[full$Familysize > 4] <- 'large'


mosaicplot(table(full$FamilysizeD, full$Survived), main='Family Size by Survival', shade=TRUE)


full[c(62, 830), 'Embarked']



embark_fare <- full %>% filter(PassengerId != 62 & PassengerId != 830)

install.packages('ggthemes')


ggplot(embark_fare, aes(x = Embarked, y = Fare, fill = factor(Pclass))) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()



full$Embarked[c(62, 830)] <- 'C'


full[1044, ]




ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
       aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
             colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(labels=dollar_format()) +
  theme_few()


full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)


sum(is.na(full$Age))



factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FamilysizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))


set.seed(129)

mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 


mice_output <- complete(mice_mod)


par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', 
     col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
     col='lightgreen', ylim=c(0,0.04))


full$Age <- mice_output$Age

sum(is.na(full$Age))

fullImp <- varImp(full, scale = FALSE)


tempData <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')],m=5,maxit=10,meth='pmm',seed=128)
summary(tempData)

str(full)

mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='pmm') 


install.packages('clusterGeneration')
install.packages('mnormt')
install.packages('corrplot')

set.seed(1)
n=500
library(clusterGeneration)
library(mnormt)
S=genPositiveDefMat("eigen",dim=15)
S=genPositiveDefMat("unifcorrmat",dim=15)
X=rmnorm(n,varcov=S$Sigma)
library(corrplot)
corrplot(cor(X), order = "hclust")

P=exp(full)/(1+exp(full))

install.packages('ranger')
install.packages('caret')
install.packages('glmnet')

library(caret)
library(glmnet)
library(ranger)
library(e1071)

predicted_age <- train(
  Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title ,
  tuneGrid = data.frame(mtry = c(2, 3, 7)),
  data = full[!is.na(full$Age), ],
  method = "ranger",
  trControl = trainControl(
    method = "cv", number = 10,
    repeats = 10, verboseIter = TRUE),
  importance = 'impurity'
)


gbmImp <- varImp(full, scale = FALSE)
gbmImp
plot(gbmImp, top = 20)
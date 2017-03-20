# This R environment comes with all of CRAN preinstalled, as well as many other helpful packages
# The environment is defined by the kaggle/rstats docker image: https://github.com/kaggle/docker-rstats
# For example, here's several helpful packages to load in 

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function

# Input data files are available in the "../input/" directory.
# For example, running this (by clicking run or pressing Shift+Enter) will list the files in the input directory

system("ls ../input")

# Any results you write to the current directory are saved as output.

library(readr)
train <- read_csv('../input/train.csv')
head(train)
test <- read_csv('../input/test.csv')

library(stringi)
names(train)
dim(train)
names(test)
dim(test)
#merging all rows to create sentiments
library(stringi)
all <- rbind(train[,c('question1', 'question2')], test[, c('question1', 'question2')])
library(syuzhet)
all_first <- stri_extract_first_words(all$question1)
all_second <- stri_extract_first_words(all$question2)

all_first1 <- stri_extract_last_words(all$question1)
all_second1 <- stri_extract_last_words(all$question2)

all$first = ifelse(all_first == all_second, 1, 0)
all$second = ifelse(all_first1 == all_second1, 1, 0)

all_sentiment1 = get_nrc_sentiment(all$question1)
all_sentiment2 = get_nrc_sentiment(all$question2)

all = cbind(all, all_sentiment1, all_sentiment2)

dim(all)
names(all)
dim(train)
dim(test)

#New train and test set
newtrain <- all[1:404290, ] 
newtest <- all[404291:2750086, ]
newtrain[,c(1:23)] = lapply(newtrain[,c(1:23)],as.factor)
newtest[, c(1:23)] = lapply(newtest[, c(1:23)], as.factor)
names(train)
newtrain$is_duplicate <- train$is_duplicate
newtrain <- na.omit(newtrain)
newtest <- na.omit(newtest)

str(newtrain)
newtrain$negative <- as.numeric(newtrain$negative)
str(newtrain$negative)
names(newtrain)
names(newtest)

#Building model
library(nnet)
summary(newtrain)
ideal <- class.ind(newtrain$is_duplicate)
model1 <- nnet(newtrain[,-25], ideal, size = 10, softmax = TRUE)

pred <- predict(model1, newdata = newtest, type = 'class')

dat1 = data.frame(newtest$id = test$id, is_duplicate = pred)
show less
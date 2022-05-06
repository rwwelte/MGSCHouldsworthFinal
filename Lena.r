library(tidytext)
library(ggplot2)
library(randomForest)
library(Hmisc)
library(psych)
library(GPArotation)
library(randomForest)
library(NbClust)
require(caTools)
library(usethis)
library(openxlsx)
library(multcomp)
library(party)
library(partykit)
library('sjPlot') 
library(dplyr)
library(rsample)
library(ggplot2)

Sys.setenv('R_MAX_VSIZE'=32000000000)
Sys.getenv('R_MAX_VSIZE')
usethis::edit_r_environ()

 subscriberData <- read.csv("subscriptionData.csv")
numericData <- read.csv("numeric_dataset.csv")


model <- lm(formula = subscriptionLength_months ~ Subscription.Type + Demo.User + 
              Auto.Renew + Free.Trial.User + Email.Subscriber, data = subscriberData)
summary(model)
summary(model)$coefficient
confint(model)



model2 <- lm(formula = Purchase.Amount ~ Subscription.Type + Demo.User + 
               Auto.Renew + Free.Trial.User + Email.Subscriber, data = subscriberData)
summary(model2)


subscriberDF <- read.csv("numeric_dataset.csv")
describe(subscriberDF)

head(subscriberDF)
summary(subscriberDF)
library('ggcorrplot')


describe(subscriberDF)



ggcorrplot(round(cor(subscriberDF),2),
           type = "lower", insig = "blank",
           show.diag = TRUE, lab = TRUE,
           colors = c("red", "white", "blue"))


head(subscriberData)
set.seed(55)

iosUSDF <- read.csv("Book6.csv")

data <- transform(
  iosUSDF,
  ID=as.integer(ID),
  App.Session.Platform = as.factor(App.Session.Platform),
  App.Activity.Type = as.factor(App.Activity.Type),
  Language = as.factor(Language),
  Subscription.Type = as.factor(Subscription.Type),
  Subscription.Event.Type = as.factor(Subscription.Event.Type),
  Purchase.Store = as.factor(Purchase.Store),
  Purchase.Amount = as.integer(Purchase.Amount),
  Currency = as.factor(Currency),
  Demo.User = as.factor(Demo.User),
  Free.Trial.User = as.factor(Free.Trial.User),
  Auto.Renew = as.factor(Auto.Renew),
  Country = as.factor(Country),
  Lead.Platform = as.factor(Lead.Platform),
  Email.Subscriber = as.factor(Email.Subscriber),
  Push.Notifications = as.factor(Push.Notifications),
  Send.Count = as.integer(Send.Count),
  Unique.Open.Count = as.integer(Unique.Open.Count),
  Unique.Click.Count = as.integer(Unique.Click.Count),
  subscriptionLength_months = as.integer(subscriptionLength_months),
  UniqueOpenRate = as.integer(UniqueOpenRate),
  User.Type = as.factor(User.Type),
  UniqueClickRate = as.integer(UniqueClickRate)
)

sapply(data, class)
summary(data)

data[ data == "?"] <- NA
colSums(is.na(data))

sample = sample.split(data$subscriptionLength_months, SplitRatio = .75)
train = subset(data, sample == TRUE)
test  = subset(data, sample == FALSE)
dim(train)
dim(test)

data <- na.omit(train)

rf_fit <- randomForest(
  subscriptionLength_months ~ Purchase.Amount + Demo.User + Free.Trial.User + Auto.Renew +
    Email.Subscriber + Push.Notifications + Send.Count + Unique.Open.Count + Unique.Click.Count
  + UniqueOpenRate + UniqueClickRate,
  data = subscriberDF,
  type = classification,
  mtry = 3,
  na.action = na.roughfix,
  ntree = 600,
  importance = TRUE
)

rf_fit2 <- randomForest(
  subscriptionLength_months ~ .,
  data = subscriberDF,
  type = classification,
  mtry = 3,
  na.action = na.roughfix,
  ntree = 600,
  importance = TRUE
)
  
print(rf_fit)

rf_mods <- list()
oob_err <- NULL
test_err <- NULL
for(mtry in 1:9){
  rf_fit <- randomForest(
    subscriptionLength_months ~ Purchase.Amount + Demo.User + Free.Trial.User + Auto.Renew +
      Email.Subscriber + Push.Notifications + Send.Count + Unique.Open.Count + Unique.Click.Count
    + UniqueOpenRate + UniqueClickRate,
    data = subscriberDF,
    mtry = mtry,
    na.action = na.roughfix,
    ntree = 600)
  oob_err[mtry] <- rf_fit$err.rate[600]
  cat(mtry, " ")
}


results_DF <- data.frame(mtry = 1:9, oob_err)
ggplot(results_DF, aes(x = mtry, y = oob_err)) + geom_point() + theme_minimal()


plot(rf_fit)

cleanData <- read.xlsx("Book6.xlsx")
summary(cleanData)

data <- transform(
  cleanData,
  ID=as.integer(ID),
  App.Session.Platform = as.factor(App.Session.Platform),
  App.Activity.Type = as.factor(App.Activity.Type),
  Language = as.factor(Language),
  Subscription.Type = as.factor(Subscription.Type),
  Subscription.Event.Type = as.factor(Subscription.Event.Type),
  Purchase.Store = as.factor(Purchase.Store),
  Purchase.Amount = as.integer(Purchase.Amount),
  Currency = as.factor(Currency),
  Demo.User = as.factor(Demo.User),
  Free.Trial.User = as.factor(Free.Trial.User),
  Auto.Renew = as.factor(Auto.Renew),
  Country = as.factor(Country),
  Lead.Platform = as.factor(Lead.Platform),
  Email.Subscriber = as.factor(Email.Subscriber),
  Push.Notifications = as.factor(Push.Notifications),
  Send.Count = as.integer(Send.Count),
  Unique.Open.Count = as.integer(Unique.Open.Count),
  Unique.Click.Count = as.integer(Unique.Click.Count),
  subscription_length_months = as.integer(subscription_length_months),
  UniqueOpenRate = as.integer(UniqueOpenRate),
  User.Type = as.factor(User.Type),
  UniqueClickRate = as.integer(UniqueClickRate)
)



#this one for linear regression
model <- lm(formula = subscription_length_months ~ Subscription.Type + Demo.User + 
              Auto.Renew + Free.Trial.User + Email.Subscriber +
              Send.Count + Unique.Open.Count + UniqueOpenRate + UniqueClickRate, data = data)

summary(model)



model2 <- glm(formula = Auto.Renew ~ Subscription.Type + Demo.User + 
                Free.Trial.User + Email.Subscriber +
               Send.Count + Unique.Open.Count + UniqueOpenRate + UniqueClickRate, data = data,
              family = "binomial")
summary(model2)



library(car)
avPlots(model)



model_split <- initial_split(data, prop = 0.75)
model_train <- training(model_split)
model_test <- testing(model_split)
dim(model_train)
dim(model_test)

preds_train <- predict(model, newdata = model_train)
preds_test <- predict(model, newdata = model_test)

get_rmse <- function(true, predictions){
  sqrt(mean((true-predictions)^2))
}

get_rmse(model_train$subscription_length_months, preds_train)
get_rmse(model_test$subscription_length_months, preds_test)

mod2 <- lm(log(subscription_length_months) ~ Subscription.Type + Demo.User + 
             Auto.Renew + Free.Trial.User + Email.Subscriber +
             Send.Count + Unique.Open.Count + UniqueOpenRate + UniqueClickRate,
           data = model_train)

# auto renew model

summary(mod2)

correlationData <- read.csv("correlationData.csv")



ggcorrplot(round(cor(correlationData),2),
           type = "lower", insig = "blank",
           show.diag = TRUE, lab = TRUE,
           colors = c("red", "white", "blue"))



subscriberData <- read.xlsx("final_dataset.xlsx")

data2 <- transform(
  subscriberData,
  ID=as.integer(ID),
  App.Session.Platform = as.factor(App.Session.Platform),
  App.Activity.Type = as.factor(App.Activity.Type),
  Language = as.factor(Language),
  Subscription.Type = as.factor(Subscription.Type),
  Subscription.Event.Type = as.factor(Subscription.Event.Type),
  Purchase.Store = as.factor(Purchase.Store),
  Purchase.Amount = as.integer(Purchase.Amount),
  Currency = as.factor(Currency),
  Demo.User = as.factor(Demo.User),
  Free.Trial.User = as.factor(Free.Trial.User),
  Auto.Renew = as.factor(Auto.Renew),
  Country = as.factor(Country),
  Lead.Platform = as.factor(Lead.Platform),
  Email.Subscriber = as.factor(Email.Subscriber),
  Push.Notifications = as.factor(Push.Notifications),
  Send.Count = as.integer(Send.Count),
  Unique.Open.Count = as.integer(Unique.Open.Count),
  Unique.Click.Count = as.integer(Unique.Click.Count),
  subscriptionLength_months = as.integer(subscriptionLength_months),
  UniqueOpenRate = as.integer(UniqueOpenRate),
  User.Type = as.factor(User.Type),
  UniqueClickRate = as.integer(UniqueClickRate)
)

model3 <- lm(formula = subscriptionLength_months ~ Demo.User + 
               Auto.Renew + Free.Trial.User + Email.Subscriber +
               Send.Count + Unique.Open.Count + UniqueOpenRate + UniqueClickRate, data = data2)

summary(model3)


subscriberData_numeric <- read.xlsx("final_dataset_numeric.xlsx")
ggcorrplot(round(cor(subscriberData_numeric),2),
           type = "lower", insig = "blank",
           show.diag = TRUE, lab = TRUE,
           colors = c("red", "white", "blue"))

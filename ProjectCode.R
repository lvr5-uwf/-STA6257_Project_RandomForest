#this file is for coding and troubleshooting before putting into blocks on the qmd file

library(tidyverse)
#library(dplyr)
library(rpart)
library(rpart.plot)
library(gtsummary)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(randomForest)

library(splitTools)
library(mlbench)
library(caret)
library(ranger)

randseed <- 123456
set.seed(randseed)

#data types for data frame columns
coltypes <- c("f",#Diabetes_012
              "f",#HighBP
              "f",#HighChol
              "f",#CholCheck
              "n",#BMI
              "f",#Smoker
              "f",#Stroke
              "f",#HeartDiseaseorAttack
              "f",#PhysActivity
              "f",#Fruits
              "f",#Veggies
              "f",#HvyAlcoholConsump
              "f",#AnyHealthcare
              "f",#NoDocbcCost
              "f",#GenHlth
              "n",#MentHlth
              "n",#PhysHlth
              "f",#DiffWalk
              "f",#Sex
              "f",#Age
              "f",#Education
              "f" #Income
              )
coltypes_collapsed <- paste(coltypes, collapse="")

#read the full dataset into a dataframe - full dataset
#df <- read_csv("diabetes_012_health_indicators_BRFSS2015.zip", col_types = coltypes_collapsed)
#df <- df %>% mutate(Diabetes_YN = as_factor(case_when(Diabetes_012 == "0.0" ~ 0, TRUE ~ 1)))

#read the full dataset into a dataframe - 50/50 split dataset
df <- read_csv("diabetes_binary_5050split_health_indicators_BRFSS2015.zip", col_types = coltypes_collapsed)

#colnames(df)
#summary(df)

#print gt summary
# smr <-
#   tbl_summary(
#     df,
#     include = c("Diabetes_binary", "HighBP", "HighChol","CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies","HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk","Sex", "Age", "Education", "Income"),
#     type = list(BMI ~ "continuous2", MentHlth  ~ "continuous2", PhysHlth  ~ "continuous2"),
#     statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}")
#   ) %>%
#   add_n() %>% # add column with total number of non-missing observations
#   bold_labels()
# smr

#reload data with all columns as numeric (the default)
# df.2 <- read_csv("diabetes_binary_5050split_health_indicators_BRFSS2015.zip")
# df.corr <- rcorr(as.matrix(df.2))
# corrplot(df.corr$r)
# hist.data.frame(df.corr)

# 
# #https://www.guru99.com/r-decision-trees.html
# #create this function variable so it is easier to change it as needed
# model_fn <- Diabetes_binary ~ HighBP + GenHlth + BMI + HighChol
# 
# #create model.  note rpart.control cp value needed for full dataset due to class imbalance
# dt_model <- rpart(model_fn, data = df, method="class",
#                   control = rpart.control(minsplit=2, cp=0.0000001, maxdepth = 4))
# rpart.plot(dt_model, box.palette = "Greens", main="Decision Tree", extra = 106)
# 
# #older plot 
# prp(dt_model, faclen=0, cex=0.8, type=0, extra=106, main="Decision Tree")

#create random forest using model, data frame df, and number of trees ntree
diabetes.forest <- randomForest(Diabetes_binary ~ HighBP + GenHlth + BMI + HighChol, data=df, ntree=300)

#votes (probability per row) is in the random forest model votes field
diabetes.forest$votes

#split data and stratify on class label
parts <- splitTools::partition(df$Diabetes_binary, p=c(sample = 0.1, remainder = 0.9))
df.sample <- df[parts$sample,]

#create actual random forest with full list of variables for analysis
diabetes.forest <- ranger(Diabetes_binary ~ ., data=df, importance = 'impurity')
print(diabetes.forest)

#show importance measures
sort(round(diabetes.forest$variable.importance, 2), decreasing = T)

#now try with specific parameters
diabetes.forest <- ranger(Diabetes_binary ~ ., data=df, mtry=4, num.trees=2000, importance = 'impurity')
print(diabetes.forest)

#ref https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
metric <- "Accuracy"

# Random Search
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="random")
rf_random <- train(Diabetes_binary~., data=df.sample, method="ranger", metric=metric, tuneLength=10, trControl=control)
print(rf_random)
plot(rf_random)

control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(1:10), .splitrule='gini', .min.node.size=1)
rf_gridsearch <- train(Diabetes_binary~., data=df.sample, method="ranger", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=4, .splitrule='gini', .min.node.size=1)
modellist <- list()
for (ntree in c(500, 1000, 1500, 2000, 2500)) {
  set.seed(randseed)
  fit <- train(Diabetes_binary~., data=df.sample, method="ranger", metric=metric, tuneGrid=tunegrid, trControl=control, num.trees=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

#show pie chart of variable distribution
HighBP <- c(nrow(df[df$HighBP == '1.0',]),nrow(df[df$HighBP == '0.0',]))
HighBP_Pct <- round(100*HighBP/sum(HighBP), 1)
pie(HighBP, labels = HighBP_Pct)

# CreatePieChart <- function(data, field_name, class_names, class_values){
#   fields <- nrow(data[data[field_name] == class_values])
# }
# 
# SelectClass <- function(data, field_name, field_value) {
#   return (data[data[field_name] == field_value,])
# }
# sapply(df, SelectClass)


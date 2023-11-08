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
library(vip)
library(ranger)
library(lessR)
library(gridExtra)

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

#set the factor levels
df$Age <- factor(df$Age, levels = c("1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0", "8.0", "9.0", "10.0", "11.0", "12.0", "13.0"))
df$GenHlth <- factor(df$GenHlth, levels = c("1.0", "2.0", "3.0", "4.0", "5.0"))
df$Education <- factor(df$Education, levels = c("1.0", "2.0", "3.0", "4.0", "5.0", "6.0"))
df$Income <- factor(df$Income, levels = c("1.0", "2.0", "3.0", "4.0", "5.0", "6.0", "7.0", "8.0"))

binaryLevels <- c("0.0", "1.0")
#set all binary factor fields up the same way
df$Diabetes_binary <- factor(df$Diabetes_binary, levels = binaryLevels)
df$HighBP <- factor(df$HighBP, levels = binaryLevels)
df$HighChol <- factor(df$HighChol, levels = binaryLevels)
df$CholCheck <- factor(df$CholCheck, levels = binaryLevels)
df$Smoker <- factor(df$Smoker, levels = binaryLevels)
df$Stroke <- factor(df$Stroke, levels = binaryLevels)
df$HeartDiseaseorAttack <- factor(df$HeartDiseaseorAttack, levels = binaryLevels)
df$PhysActivity <- factor(df$PhysActivity, levels = binaryLevels)
df$Fruits <- factor(df$Fruits, levels = binaryLevels)
df$Veggies <- factor(df$Veggies, levels = binaryLevels)
df$HvyAlcoholConsump <- factor(df$HvyAlcoholConsump, levels = binaryLevels)
df$AnyHealthcare <- factor(df$AnyHealthcare, levels = binaryLevels)
df$NoDocbcCost <- factor(df$NoDocbcCost, levels = binaryLevels)
df$DiffWalk <- factor(df$DiffWalk, levels = binaryLevels)
df$Sex <- factor(df$DiffWalk, levels = binaryLevels)

#colnames(df)
#summary(df)

#print gt summary
smr <-
  tbl_summary(
    df,
    include = c("Diabetes_binary", "HighBP", "HighChol","CholCheck", "BMI", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies","HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "GenHlth", "MentHlth", "PhysHlth", "DiffWalk","Sex", "Age", "Education", "Income"),
    type = list(BMI ~ "continuous2", MentHlth  ~ "continuous2", PhysHlth  ~ "continuous2"),
    statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}")
  ) %>%
  add_n() %>% # add column with total number of non-missing observations
  bold_labels()
smr

#create lists for charts
binary_chart_list <- list()
multiple_chart_list <- list()

#loop field stats from internals of summary data
for (field in smr$meta_data$df_stats) {
  #print(field)
  #if this is a field with a probability field
  if ("p" %in% names(field)) {
    #add chart to correct list
    if (nrow(field) == 2) {
      #create ggplot for field with binary values
      field_chart <- ggplot(field, aes(x = label, y = p, fill = label)) +
        geom_col() +
        xlab(field$variable[1]) + ylab("Percent") +
        theme(legend.position = "none") + 
        scale_fill_manual(values=c("#E74C3C","#1D8348")) +
        scale_x_discrete(limits = field$label)
      
      binary_chart_list[[field$variable[1]]] <- field_chart
    } else {
      #create ggplot for field with multiple values
      field_chart <- ggplot(field, aes(x = label, y = p, fill="#2980B9")) +
        geom_col() +
        xlab(field$variable[1]) + ylab("Percent") +
        theme(legend.position = "none") + 
        scale_fill_manual(values="#2980B9") +
        scale_x_discrete(limits = field$label)
      
      multiple_chart_list[[field$variable[1]]] <- field_chart
    }
  }
}

#print charts in grid
grid.arrange(grobs=binary_chart_list, ncol=4)
grid.arrange(grobs=multiple_chart_list, ncol=2)

smr$meta_data$df_stats[[1]]$variable[1]

field_chart <- ggplot(smr$meta_data$df_stats[[20]], aes(x = label, y = p, fill = label)) +
  geom_col() +
  geom_text(aes(label = label),position = position_stack(vjust = 0.5), show.legend = F) +
  xlab(smr$meta_data$df_stats[[20]]$variable[1]) + ylab("Pct") +
theme(legend.position = "none") +
  scale_x_discrete(limits = smr$meta_data$df_stats[[20]]$label)
field_chart

field_chart <- ggplot(smr$meta_data$df_stats[[2]], aes(x = "", y = p, fill = label)) +
  geom_col() + 
  geom_text(aes(label = paste(100*round(p, 3), "%")), position = position_stack(vjust = 0.5), show.legend = F) +
  ylab(smr$meta_data$df_stats[[2]]$variable[1]) + xlab("") +
  coord_polar(theta = "y") +
  guides(fill = guide_legend(title = "Value")) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(), 
        panel.background = element_rect(fill = "white"))
field_chart


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
parts <- splitTools::partition(df$Diabetes_binary, p=c(sample = 0.7, remainder = 0.3))
df.sample <- df[parts$sample,]

#create actual random forest with full list of variables for analysis
diabetes.forest <- ranger(Diabetes_binary ~ ., data=df, importance = 'impurity')
print(diabetes.forest)

#show importance measures
sort(round(diabetes.forest$variable.importance, 2), decreasing = T)

#create vi object for variable importance graph
vi_values <- vi(diabetes.forest)
#create variable importance graph
vip(vi_values, aesthetics = list(fill="#2980B9"))

#now try with specific parameters
diabetes.forest <- ranger(Diabetes_binary ~ ., data=df, mtry=3, num.trees=2000, importance = 'impurity')
print(diabetes.forest)

#use reduced list of parameters found by assessing variable importance
diabetes.forest.reduced <- ranger(Diabetes_binary ~ BMI + HighBP + GenHlth + Age,
                                  num.trees=2000, data=df)
print(diabetes.forest.reduced)

#split data and stratify on class label
parts <- splitTools::partition(df$Diabetes_binary, p=c(train = 0.7, test = 0.3))
df.train <- df[parts$train,]
df.test <- df[parts$test,]

#train rf with train set
diabetes.forest <- ranger(Diabetes_binary ~ ., data=df.train)

#predict using test set
preds <- predict(diabetes.forest, data = df.test, seed=randseed)

#create confusion matrix of prediction vs actual on test set
confusion.test <- confusionMatrix(data=df.test$Diabetes_binary, reference = preds$predictions)
#show confusion matrix plot, ref: https://www.delftstack.com/howto/r/visualize-confusion-matrix-in-r/
fourfoldplot(confusion.test$table,color = c("red", "green"),
             conf.level = 0, margin = 1, main = "Confusion Matrix")
#show full confusion matrix output with accuracy values etc
confusion.test

#create function to output a chart from a confusion matrix
ConfusionMatrixChart <- function(conf_matrix){
  #create dataframe to use for the chart
  cf <- data.frame(cfm = conf_matrix$table)
  #add percentage field
  cf <- cf %>%  mutate(cfm.Percent = cfm.Freq / sum(cfm.Freq))
  #add labels for TN, FN, TP, FP
  cf <- cf %>%  mutate(cfm.Label = 
                         case_when(cfm.Prediction == "0.0" & cfm.Reference == "0.0" ~ "TN",
                                   cfm.Prediction == "0.0" & cfm.Reference == "1.0" ~ "FN", 
                                   cfm.Prediction == "1.0" & cfm.Reference == "1.0" ~ "TP",
                                   cfm.Prediction == "1.0" & cfm.Reference == "0.0" ~ "FP"))
  
  #add color field to go with the labels - green is good and red is bad
  cf <- cf %>%  mutate(cfm.Label.Color = 
                         case_when(cfm.Prediction == "0.0" & cfm.Reference == "0.0" ~ "#1D8348",
                                   cfm.Prediction == "0.0" & cfm.Reference == "1.0" ~ "#E74C3C", 
                                   cfm.Prediction == "1.0" & cfm.Reference == "1.0" ~ "#1D8348",
                                   cfm.Prediction == "1.0" & cfm.Reference == "0.0" ~ "#E74C3C"))
  #tried to set it to output in the order I wanted but it seems to have it's own order
  #this did nothing for the sort order of the output chart
  #cf <- cf %>% mutate(cfm.Label = factor(cfm.Label, levels = c("TN", "FN", "TP", "FP")))
  
  #output the chart
  PieChart(x = cfm.Label, y = cfm.Percent, hole = 0, values = "%", data = cf, 
           clockwise = T, quiet = T, fill = cf$cfm.Label.Color,  width = 3,
           main = "Confusion Matrix")  
}
ConfusionMatrixChart(confusion.test)


cf <- data.frame(cfm = confusion.test$table)
#add percentage field
cf <- cf %>%  mutate(cfm.Percent = cfm.Freq / sum(cfm.Freq))
#add labels for TN, FN, TP, FP
cf <- cf %>%  mutate(cfm.Label = 
                       case_when(cfm.Prediction == "0.0" & cfm.Reference == "0.0" ~ "TN",
                                 cfm.Prediction == "0.0" & cfm.Reference == "1.0" ~ "FN", 
                                 cfm.Prediction == "1.0" & cfm.Reference == "1.0" ~ "TP",
                                 cfm.Prediction == "1.0" & cfm.Reference == "0.0" ~ "FP"))

#add color field to go with the labels - green is good and red is bad
cf <- cf %>%  mutate(cfm.Label.Color = 
                       case_when(cfm.Prediction == "0.0" & cfm.Reference == "0.0" ~ "#1D8348",
                                 cfm.Prediction == "0.0" & cfm.Reference == "1.0" ~ "#E74C3C", 
                                 cfm.Prediction == "1.0" & cfm.Reference == "1.0" ~ "#1D8348",
                                 cfm.Prediction == "1.0" & cfm.Reference == "0.0" ~ "#E74C3C"))
#tried to set it to output in the order I wanted but it seems to have it's own order
#this did nothing for the sort order of the output chart
cf <- cf %>% mutate(cfm.Label = factor(cfm.Label, levels = c("TN", "FN", "TP", "FP")))

#trying ggplot pie chart - still not as good as the other
ggplot(cf, aes(x = "", y = cfm.Percent, fill = cfm.Label)) +
  geom_col() +
  geom_text(aes(label = cfm.Label),position = position_stack(vjust = 0.5), show.legend = F) +
  xlab("") + ylab("Confusion Matrix") +
  coord_polar(theta = "y") +
  guides(fill = guide_legend(title = "Value")) +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(), 
        panel.background = element_rect(fill = "white")) +
  scale_fill_manual(values = c("#1D8348","#E74C3C","#1D8348","#E74C3C")) 

#pie(x = cf$cfm.Percent, y = cf$cfm.Label)

plot(confusion.test$table)

control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(1:6), .splitrule='gini', .min.node.size=1)
rf_gridsearch <- train(reduced_model, data=df.sample, method="ranger", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)

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


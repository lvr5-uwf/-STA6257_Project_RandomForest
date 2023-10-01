#this file is for coding and troubleshooting before putting into blocks on the qmd file

library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)

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
              "n",#GenHlth
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

#https://www.guru99.com/r-decision-trees.html

#create this function variable so it is easier to change it as needed
model_fn <- Diabetes_binary ~ HighBP + GenHlth + BMI + HighChol

#create model.  note rpart.control cp value needed for full dataset due to class imbalance
dt_model <- rpart(model_fn, data = df, method="class",
                  control = rpart.control(minsplit=2, cp=0.0000001, maxdepth = 4))
rpart.plot(dt_model, box.palette = "Greens", main="Decision Tree", extra = 106)

#older plot 
prp(dt_model, faclen=0, cex=0.8, type=0, extra=106, main="Decision Tree")


#Name: Shruti Kamble
#Course: Intermediate Analytics
#Date: 04/15/2021
#Module 1:Regression Diagnostics with R

library(readr)
library(tidyr)
library(psych)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(RColorBrewer)
library(ggthemes)
library(pastecs)
library(tidyverse)
library(dplyr)
library(lmSubsets)

#Task 1: Load the Ames housing dataset
AmesHousing <- read_csv("DataSet/AmesHousing.csv")
View(AmesHousing)
colnames(AmesHousing)

#Renaming zone types 
AmesHousing$`MS Zoning`[AmesHousing$`MS Zoning` %in% c("C (all)", "I (all)", "A (agr)")] <- c("CM", "IN", "AG")

#Replacing NA with respective meaning
AmesHousing$Alley[is.na(AmesHousing$Alley)] <- "No Alley"
AmesHousing$Fence[is.na(AmesHousing$Fence)] <- "No Fence"
AmesHousing$`Misc Feature`[is.na(AmesHousing$`Misc Feature`)] <- "None"
AmesHousing$`Fireplace Qu`[is.na(AmesHousing$`Fireplace Qu`)] <- "No Fireplace"

#Replacing NA with common variable for selected column
AmesHousing[c("Garage Type", "Garage Finish","Garage Qual", "Garage Cond")][is.na(AmesHousing[c("Garage Type", "Garage Finish", "Garage Qual", "Garage Cond")])] <- "No Garage"
AmesHousing[c("Bsmt Qual", "Bsmt Cond", "Bsmt Exposure", "BsmtFin Type 1", "BsmtFin Type 2")][is.na(AmesHousing[c("Bsmt Qual", "Bsmt Cond", "Bsmt Exposure", "BsmtFin Type 1", "BsmtFin Type 2")])] <- "No Basement"

AmesHousing$`Pool QC` = NULL     #Dropping column as NULL values
str(AmesHousing)                 #Checking string of each column


#Task 2: Perform Exploratory Data Analysis and use descriptive statistics to describe the data.
ggplot(AmesHousing, aes(`MS SubClass`, fill = `MS Zoning`)) + geom_histogram(binwidth = 10) + 
      ggtitle("AmesHousing dwellings by zone") + labs(x = "Dwellings", y = "Frequency of Dwellings") + theme_fivethirtyeight()

AmesHousing %>%
  select(c(4,5,6,7,21,77,79,80,81)) %>%
  str() 

AmesHousing %>%
  select(c(4,5,6,7,21,77,79,80,81)) %>%
  psych::describe(.,IQR = F, skew = F, ranges = F) %>% 
  paged_table()


#Task 3: Prepare the dataset for modeling by imputing missing values with the variable's mean value or any other value that you prefer.

sapply(AmesHousing,function(x)sum(is.na(x)))[sapply(AmesHousing,function(x)sum(is.na(x)))>0]

AmesHousing$`Lot Frontage`[which(is.na(AmesHousing$`Lot Frontage`))] <- mean(AmesHousing$`Lot Frontage`)
AmesHousing$`Mas Vnr Area`[which(is.na(AmesHousing$`Mas Vnr Area`))] <- mean(AmesHousing$`Mas Vnr Area`)
AmesHousing$`BsmtFin SF 1`[which(is.na(AmesHousing$`BsmtFin SF 1`))] <- mean(AmesHousing$`BsmtFin SF 1`)
AmesHousing$`BsmtFin SF 2`[which(is.na(AmesHousing$`BsmtFin SF 2`))] <- mean(AmesHousing$`BsmtFin SF 2`)
AmesHousing$`Total Bsmt SF`[which(is.na(AmesHousing$`Total Bsmt SF`))] <- mean(AmesHousing$`Total Bsmt SF`)
AmesHousing$`Garage Area`[which(is.na(AmesHousing$`Garage Area`))] <- mean(AmesHousing$`Garage Area`)


#Task 4: Use the "cor()" function to produce a correlation matrix of the numeric values.

AmesHousing_New <- data.frame(AmesHousing$`Lot Frontage`,AmesHousing$`Lot Area`,AmesHousing$`Mas Vnr Area`,
                 AmesHousing$`Total Bsmt SF`,AmesHousing$`Gr Liv Area`,AmesHousing$`Wood Deck SF`,
                 AmesHousing$`Misc Val`,AmesHousing$SalePrice, AmesHousing$`Pool Area`, AmesHousing$`Garage Area`,
                 AmesHousing$`BsmtFin SF 1`, AmesHousing$`BsmtFin SF 2`, AmesHousing$`Open Porch SF`, AmesHousing$`Enclosed Porch`,
                 AmesHousing$`Screen Porch`)

t(cor(AmesHousing_New,use="complete.obs")) %>%
  kbl(caption = "Correaltion Plot for All Numeric Values",
      align = 'c') %>%
  kable_classic_2(full_width = T,
                  position = "center",
                  fixed_thead = T)%>% 
  scroll_box(width = "100%", 
             height = "100%")

#Task 5: Produce a plot of the correlation matrix, and explain how to interpret it.(hint - check the corrplot or ggcorrplot plot libraries)

corrplot(cor(AmesHousing_New, use="complete.obs"),
         method = "shade",
         order = "FPC",           
         addrect = 20
         )

#Task 6:Make a scatter plot for the X continuous variable with the highest correlation with SalePrice. 
#Do the same for the X variable that has the lowest correlation with SalePrice. 
#Finally, make a scatter plot between X and Sale#Price with the correlation closest to 0.5. 
#Interpret the scatter plots and describe how the patterns differ.

ggplot(AmesHousing_New, aes(x= AmesHousing..Gr.Liv.Area., y = AmesHousing.SalePrice)) + geom_point() +
  labs(x = "living area square feet", y = "Sale Price") + labs(title = "Highest Correlation with Sale Price") + 
  theme(plot.title = element_text(hjust = 0.5))

  
ggplot(AmesHousing_New, aes(x= AmesHousing..BsmtFin.SF.2. , y = AmesHousing.SalePrice)) + geom_point() +
  labs(x = "Type 2 basement finished area sq.ft", y = "Sale Price") + labs(title = "Lowest Correlation with Sale Price") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(AmesHousing_New, aes(x=AmesHousing..Mas.Vnr.Area., y= AmesHousing.SalePrice)) + 
  geom_point()+ labs(x = "Masonry veneer area in sq.ft.", y= "Sale Price") + labs(title = "Moderate Correlation with Sale Price" ) +
  theme(plot.title = element_text(hjust = 0.5))


#Task 7: Using at least 3 continuous variables, fit a regression model in R.
Reg_model <- lm(AmesHousing.SalePrice ~ AmesHousing..Gr.Liv.Area. + AmesHousing..Mas.Vnr.Area. + AmesHousing..BsmtFin.SF.2. + AmesHousing..Misc.Val., data = AmesHousing_New)
Reg_model
summary(Reg_model)$coefficient

#Task 8: Report the model in equation form and interpret each coefficient of the model in the context of this problem.
y = 25344.10+ 95.57*x1 +  95.57*x2 + 118.96*x3 +  9.80*x4

#Task 9: Use the "plot()" function to plot your regression model.Interpret the four graphs that are produced.
plot(Reg_model)

#Task 10: Check your model for multicollinearity and report your findings.What steps would you take to correct multicollinearity if it exists?
library(car)
vif(Reg_model)

#Task 11: Check your model for outliers and report your findings. Should these observations be removed from the model?
outlierTest(model = Reg_model)

#Task 12: Attempt to correct any issues that you have discovered in your model. Did your changes improve the model, why or why not?
#Task 13: Use the all subsets regression method to identify the "best" model. State the preferred model in equation form.
#Generating subsets by feature selection by determining the stepwise direction


Reg_model <- lm(AmesHousing.SalePrice ~ AmesHousing..Gr.Liv.Area. + AmesHousing..Mas.Vnr.Area. + AmesHousing..BsmtFin.SF.2. + AmesHousing..Misc.Val., data = AmesHousing_New)
AIC <- stepAIC(Reg_model, direction = "both")

Reg_mod01 <- lm(formula = AmesHousing.SalePrice ~ AmesHousing..Wood.Deck.SF. + 
                  AmesHousing..Lot.Area., data = AmesHousing_New)
summary(Reg_mod01)
AIC01 <- stepAIC(Reg_mod01, direction = "both")

Reg_mod02 <- lm(formula = AmesHousing.SalePrice ~ AmesHousing..Misc.Val.+ AmesHousing..Wood.Deck.SF.+ 
                  AmesHousing..Mas.Vnr.Area., data = AmesHousing_New)
summary(Reg_mod02)
AIC02 <- stepAIC(Reg_mod02, direction = "both")

                
#Task 14: Compare the preferred model from step 13 with your model from step 12. 
#How do they differ? Which model do you prefer and why?
#Explanation is in the report

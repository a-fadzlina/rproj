## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
#loading needed packages

library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(labelled)

#loading dataset
cat <- read_csv("experiment.csv")
cat<-cat[ , 2:3]
cat

#creating absolut and relative proportions
prop<-table(cat)
prop

prop_abs<-addmargins(prop)
prop_abs

prop_rel<-prop.table(prop, 1) # divides cell value by sum of row
prop_rel

#rounding off values, addin margins to rows
prop_rel<-round(addmargins(prop_rel, 2), 2)
prop_rel

#setting scientific values
options(scipen = 999)

#inserting original proportion table into prop.test function - testing significance of the difference using chi-square test of proportions
prop.test(prop)

#building a barplot to visualise diferrence in clicks between groups

#wranglig data
cat$clicked_adopt_today<-factor(cat$clicked_adopt_today, labels=c("No","Yes"))
cat$condition<-as.factor(cat$condition)

#creating bar plot
ggplot(cat, aes(x=condition, fill=clicked_adopt_today))+
  geom_bar(position="dodge", width=0.5)+
  geom_text(aes(label = ..count..), stat = "count", vjust=1.6, color="white", position = position_dodge(0.5))+
  scale_x_discrete(labels=c("Old image", "New image"))+
  theme_minimal() +
  scale_fill_brewer(palette="Dark2")+
  theme(axis.title.y=element_blank())+
  labs(x="User group", fill="Clicked Adoption button")+
  ggtitle("Adoption button clicks", subtitle="By group")+
  theme(plot.title = element_text(family = "sans", size = 14, margin=margin(0,0,10,0)),
        plot.subtitle=element_text(family = "sans", size = 12, margin=margin(0,0,10,0)),
        axis.title.x = element_text(family = "sans", size = 11, margin=margin(10,0,0,0)))
  


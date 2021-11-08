## ----------------------------------------------------------------------------------------------------------------------------------------------------------------
#Importing required packages

library(dplyr)
library(ggplot2)
library(cluster)

#Importing the "Mall_Customers.csv" data 
customer <- read.csv("A:/RStudio/R_Projects/projects/cust_seg/Mall_Customers.csv")

#Check the names of columns and structure of the dataset
names(customer)
str(customer)

##Rename some column names
customer <- rename(customer, annual_income=Annual.Income..k..,
       spending_score=Spending.Score..1.100.)

##Summarise the data
summary(customer)

# Creating a histogram to show dispersion of mall customers based on age
ggplot(customer,aes(x=Age)) +
  geom_histogram() +
  labs(title="Histogram showing distribution of Age")

# Creating a histogram to show dispersion of mall customers based on age groups
ggplot(customer, aes(x = Age)) +
  geom_vline(aes(xintercept = mean(Age)), color = "blue",  #adding an intercept to indicate mean age
             linetype = "dashed", size = 1.0) +
  geom_histogram(binwidth = 5, aes(y = ..density..), 
                 color = "black", fill = "white") +
  geom_density(alpha = 0.4, fill = "red") +  #adding density plot
  labs(title = "Histogram to Show Density of Age Groups")

# Creating a barplot to assess gender distribution of my sample of customers.
ggplot(customer,aes(x= Gender)) +
  geom_bar(stat="count",width=0.5,fill="steelblue") +
  theme_minimal()+
  labs(title="Barplot to display Gender Comparison", xlab="Gender")

## Create a histogram for the variable "Age" by Gender
ggplot(customer,aes(x=Age, fill=Gender, color=Gender))+
  geom_histogram(bins = 10, position = "identity",alpha=0.5) +
  labs(title="Histogram showing distribution of Gender by Age")

# Creating density plot to show customer's annual income
ggplot(customer, aes(x = annual_income)) +
  geom_density(alpha=0.4, fill="blue") +
  scale_x_continuous(breaks = seq(0, 200, by = 10)) +
  labs(title="Density Plot for Annual Income")

# Create a boxplot to understand cutomer's spending score by gender.
ggplot(customer, aes(x = spending_score, y= Gender)) +
  geom_boxplot() +
  labs(title = "Boxplot showing customers' Spending Score by Gender")

#Determine gap statistic
# Setting seed to 125 for reproducibility
set.seed(125)

#using the gap-statistics to get the optimal number of clusters
stat_gap<-clusGap(customer[,3:5], FUN=kmeans, nstart=25, K.max = 10, B=50)

#Plot the optimal number of clusters based on the gap statistic
plot(stat_gap)

#Creating the customer clusters with KMeans
k6<-kmeans(customer[,3:5], 6, iter.max = 100, nstart=50,
           algorithm = "Lloyd")
k6 #Printing the result
clusplot(customer, k6$cluster, color=TRUE, shade=TRUE, labels=0, lines=0) #Showing the six KMeans clusters


#Perform Principal Component Analysis
pcclust<-prcomp(customer[, 3:5], scale=FALSE)
summary(pcclust) #Checking the summary of the PCA model
pcclust$rotation[, 1:2] # Applying the PCA model on the data


#creating plot of customer segments based on results from pca
# Set seed to 1
set.seed(1)

#Create a plot of the customers segments
ggplot(customer, aes(x = annual_income , y = spending_score)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", 
                                "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", 
          subtitle = "Using K-means Clustering")


#Creating formal plot that is easier to understand and consume
ggplot(customer, aes(x = annual_income , y = spending_score)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = " ", 
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("High Income, Low Spending", "Low Income, Low Spending", "Medium Income, Medium Spending", 
                                "Medium Income, Medium Spending", "Low Income, High Spending","High Income, High Spending")) +
  labs(x="Annual Income", y="Spending Score") +
  ggtitle("Segments of Mall X Customers", 
          subtitle = "Using K-means Clustering")


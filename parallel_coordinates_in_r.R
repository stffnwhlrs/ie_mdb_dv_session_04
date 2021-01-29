library(ggplot2)
library(ggthemes)
library(GGally)
library(ggdark)
library(dplyr)

#Defining the general colors to avoid hard coding  
fill_color = '#111111'
decoration_color = '#cccccc'
main1_color = '#f20675'
main2_color = '#1ce3cd'

theme_set(theme_bw())
#Set dark theme for black background
theme_set(dark_theme_gray())

theme_base()

# colors for for values
color_1 = "#478adb"
color_2 = "#cccccc"
color_3 = "#f20675"
color_4 = "#bcc048"
color_5 = "#1ce3cd"
color_6 = "#878484"

#Parallel coordinates
?ggparcoord

#Check the data 
names(iris)
head(iris, n=10)
str(iris)
summary(iris)

#Simple chart
ggparcoord(iris, columns=1:4, alphaLines=0.3)

#Adding color as a visual encoding 
ggparcoord(iris, columns=1:4, groupColumn=5, alphaLines=0.3) + 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd"))

#Adding a box plot 
ggparcoord(iris, columns=1:4, groupColumn=5, alphaLines=0.3, boxplot=TRUE) + 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd"))

#Open the x axis order  
ggparcoord(iris, columns=1:4, groupColumn=5, alphaLines=0.3, order="anyClass") + 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd"))

#Change Scaling 

#Standardize to Min=0 and Max=1
ggparcoord(iris, columns=1:4, groupColumn=5, order="anyClass",
           alphaLines=0.3, scale="uniminmax") + 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd"))

#Normalize univariately (substract mean & divide by sd)
ggparcoord(iris, columns=1:4, groupColumn=5, order="anyClass",
           alphaLines=0.3, scale="std") + 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd"))

#Standardize and center variables
ggparcoord(iris, columns=1:4, groupColumn=5, order="anyClass",
           alphaLines=0.3, scale="center")+ 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd"))

#No scaling
ggparcoord(iris, columns=1:4, groupColumn=5, order="anyClass",
           alphaLines=0.3, scale="globalminmax") + 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd"))

#Showing points, changing transparency and color
ggparcoord(iris, columns=1:4, groupColumn=5, order="anyClass",
           showPoints=TRUE, alphaLines = 0.3, scale="globalminmax")+ 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd"))

#Adding the points to the graph
ggparcoord(iris, columns=1:4, groupColumn=5, order="anyClass",
           showPoints=TRUE, alphaLines=0.3, scale="globalminmax") + 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd"))

#With spline factor 
ggparcoord(iris, columns=1:4, groupColumn=5, order="anyClass", splineFactor=10,
           showPoints=TRUE, alphaLines=0.3, scale="globalminmax") + 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd"))

#Highlighting one group
ggparcoord(iris, columns=1:4, groupColumn=5, order="anyClass",
           showPoints=TRUE, alphaLines=0.3, scale="globalminmax") + 
  scale_color_manual(values=c("#5d5959", "#f20675", "#5d5959"))

#Create a small multiple for easy pattern comparison
ggparcoord(iris, columns=1:4, groupColumn=5, order="anyClass",
           showPoints=TRUE, alphaLines=0.2, scale="globalminmax") + 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd")) +   
  facet_wrap(. ~ Species, ncol=1)

#Now it's your turn to apply the same flow to another dataset, for example the gapminder (e.g. grouping by continent), diamonds (e.g. grouping by cut) 


# 1) INSPECT THE DATASET

# Check basic stats / information
data <- diamonds
names(data)
str(data)
summary(data)


# Histogram to check price distribution
ggplot(data,aes(price)) +
  geom_histogram(colour= "black", fill=main2_color)


# 2) FEATURE ENGINEERING

# Transform price into a categorical variable
# create thresholds
breaks <- c(0,5000,10000,15000,20000)
# create labels
tags <-c("very cheap", "cheap","expensive", "very expensive")
# create factor variable
price_groups <- cut(data$price, breaks=breaks, include.lowest=TRUE, right=FALSE, labels=tags)
# order factors
price_groups <- factor(price_groups, levels=tags, ordered = TRUE)
# add to dataset
data$price_group <- price_groups

# Check result
# get total number of rows
nrow(data)
# check distribution of price_group
table(data$price_group)
str(data)

# 3) DECREASE DATASET TO IMPROVE visual exploration
# Our goal is to reduce the number of probes and get the same number of probes for each price_group in our final data

# prepare very cheap
data_very_cheap <- data %>% filter(price_group == "very cheap")
data_very_cheap_sample <- data_very_cheap[sample(nrow(data_very_cheap),200),]
nrow(data_very_cheap_sample)
head(data_very_cheap_sample)

# prepare cheap
data_cheap <- data %>% filter(price_group == "cheap")
data_cheap_sample <- data_cheap[sample(nrow(data_cheap),200),]
nrow(data_cheap_sample)
head(data_cheap_sample)

# prepare expensive
data_expensive <- data %>% filter(price_group == "expensive")
data_expensive_sample <- data_expensive[sample(nrow(data_expensive),200),]
nrow(data_expensive_sample)
head(data_expensive_sample)

# prepare very expensive
data_very_expensive <- data %>% filter(price_group == "very expensive")
data_very_expensive_sample <- data_very_expensive[sample(nrow(data_very_expensive),200),]
nrow(data_very_expensive_sample)
head(data_very_expensive_sample)

# combine the samples
sample <- rbind(data_very_cheap_sample, data_cheap_sample, data_expensive_sample, data_very_expensive_sample)
table(sample$price_group)

# Histogram to check price distribution again
ggplot(sample,aes(price)) +
  geom_freqpoly(colour= "white")


# ANALYSIS

# Inital graph
ggparcoord(sample, columns=c(1,2,4,5,6), alphaLines=0.3)

#Adding color as a visual encoding 
ggparcoord(sample, columns=c(1,2,4,5,6), groupColumn=11, alphaLines=0.3) + 
  scale_color_manual(values=c(color_1, color_2, color_3, color_4
                              ))

table(data$cut)
table(data$clarity)

#Adding color as a visual encoding + the x axis order
ggparcoord(data, columns=c(1,2,4,5,6), groupColumn=11, alphaLines=0.3, order="anyClass") + 
  scale_color_manual(values=c(color_1, color_2, color_3, color_4, color_5))

#Normalize univariately (substract mean & divide by sd)
ggparcoord(data, columns=c(1,2,4,5,6), groupColumn=11, alphaLines=0.3, order="anyClass", scale="std") + 
  scale_color_manual(values=c(color_1, color_2, color_3, color_4, color_5))


#Standardize and center variables
ggparcoord(data, columns=c(1,2,4,5,6), groupColumn=11, alphaLines=0.3, order="anyClass", scale="center") + 
  scale_color_manual(values=c(color_1, color_2, color_3, color_4, color_5))

# NO scaling
ggparcoord(data, columns=c(1,2,4,5,6), groupColumn=11, alphaLines=0.3, order="anyClass", scale="globalminmax") + 
  scale_color_manual(values=c(color_1, color_2, color_3, color_4, color_5))

# Show points
ggparcoord(data, columns=c(1,2,4,5,6), groupColumn=11, alphaLines=0.3, order="anyClass", showPoints=TRUE,  scale="globalminmax") + 
  scale_color_manual(values=c(color_1, color_2, color_3, color_4, color_5))

#With spline factor 
ggparcoord(data, columns=c(1,2,4,5,6), 
           groupColumn=11, alphaLines=0.3, 
           order="anyClass", 
           #splineFactor = 5,  
           showPoints=TRUE,  
           scale="std") + 
  scale_color_manual(values=c(color_1, color_2, color_3, color_4, color_5))

# very cheap
ggparcoord(data, columns=c(1,2,4,5,6), 
           groupColumn=11, alphaLines=0.3, 
           order="anyClass", 
           #splineFactor = 5,  
           showPoints=TRUE,  
           scale="std") + 
  scale_color_manual(values=c(color_1, color_6, color_6, color_6, color_6))

# cheap
ggparcoord(data, columns=c(1,2,4,5,6), 
           groupColumn=11, alphaLines=0.3, 
           order="anyClass", 
           #splineFactor = 5,  
           showPoints=TRUE,  
           scale="std") + 
  scale_color_manual(values=c(color_6, color_2, color_6, color_6, color_6))


# cheap
ggparcoord(data, columns=c(1,2,4,5,6), 
           groupColumn=11, alphaLines=0.3, 
           order="anyClass", 
           #splineFactor = 5,  
           showPoints=TRUE,  
           scale="std") + 
  scale_color_manual(values=c(color_6, color_1, color_6, color_6, color_6))

# normal
ggparcoord(data, columns=c(1,2,4,5,6), 
           groupColumn=11, alphaLines=0.3, 
           order="anyClass", 
           #splineFactor = 5,  
           showPoints=TRUE,  
           scale="std") + 
  scale_color_manual(values=c(color_6, color_6, color_3, color_6, color_6))

# normal
ggparcoord(data, columns=c(1,2,4,5,6), 
           groupColumn=11, alphaLines=0.3, 
           order="anyClass", 
           #splineFactor = 5,  
           showPoints=TRUE,  
           scale="std") + 
  scale_color_manual(values=c(color_6, color_6, color_3, color_6, color_6))


# expensive
ggparcoord(data, columns=c(1,2,4,5,6), 
           groupColumn=11, alphaLines=0.3, 
           order="anyClass", 
           #splineFactor = 5,  
           showPoints=TRUE,  
           scale="std") + 
  scale_color_manual(values=c(color_6, color_6, color_6, color_4, color_6))


# very expensive
ggparcoord(data, columns=c(1,2,4,5,6), 
           groupColumn=11, alphaLines=0.3, 
           order="anyClass", 
           #splineFactor = 5,  
           showPoints=TRUE,  
           scale="std") + 
  scale_color_manual(values=c(color_6, color_6, color_6, color_6, color_1))





#Create a small multiple for easy pattern comparison
ggparcoord(data, columns=c(1,2,4,5,6), 
           groupColumn=11, 
           alphaLines=0.3, 
           order="anyClass", 
           #splineFactor = 10,  
           #showPoints=TRUE,  
           scale="std") + 
  scale_color_manual(values=c(color_1, color_2, color_3, color_4, color_5)) +
  facet_wrap(. ~ price_group, ncol=1)




#Create a small multiple for easy pattern comparison
ggparcoord(iris, columns=1:4, groupColumn=5, order="anyClass",
           showPoints=TRUE, alphaLines=0.2, scale="globalminmax") + 
  scale_color_manual(values=c("#478adb", "#f20675", "#1ce3cd")) +   
  facet_wrap(. ~ Species, ncol=1)

str(data)

table(data$price_group)

?diamonds

?cut

# NEXT STEPS
# - Equal binning / reduce binning
# - check variables
# - aesthetics

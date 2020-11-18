# Edx Capstone - Airbnb Analysis 
# Author: Eshna Airon
# Date: 2020-11-05

#############################
# Installing required packages
#############################
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(glue)) install.packages("glue", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(glue)) install.packages("glue", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("ggridges", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(hrbrthemes)) install.packages("hrbrthemes", repos = "http://cran.us.r-project.org")
if(!require(DescTools)) install.packages("DescTools", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(vip)) install.packages("vip", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(mboost)) install.packages("mboost", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(party)) install.packages("party", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(pls)) install.packages("pls", repos = "http://cran.us.r-project.org")
if(!require(ipred)) install.packages("ipred", repos = "http://cran.us.r-project.org")
if(!require(elasticnet)) install.packages("elasticnet", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")
if(!require(Cubist)) install.packages("Cubist", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(leaps)) install.packages("leaps", repos = "http://cran.us.r-project.org")
if(!require(earth)) install.packages("earth", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(mgcv)) install.packages("mgcv", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(car)) install.packages("car", repos = "http://cran.us.r-project.org")
if(!require(kknn)) install.packages("kknn", repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(parameters)) install.packages("parameters", repos = "http://cran.us.r-project.org")

#############################
# Loading required packages
#############################
library(RColorBrewer)
library(ggridges)
library(cowplot)
library(ggplot2)
library(dplyr)
library(glue)
library(hrbrthemes)
library(ggthemes)
library(DescTools)
library(httr)
library(glmnet)
library(e1071)
library(caret)
library(vip)
library(xgboost)
library(mboost)
library(ranger) 
library(rpart)
library(party)
library(randomForest)
library(pls)
library(ipred)
library(elasticnet)
library(gbm)
library(Cubist)
library(glmnet)
library(leaps)
library(earth)
library(kernlab)
library(caTools)
library(mgcv)
library(rattle)
library(rpart.plot)
library(ggcorrplot)
library(psych)
library(car)
library(kknn)
library(kernlab)
library(forcats) 
library(ggrepel)
library(parameters)
#########################
# Downloading the Dataset 
#########################

data<-read.csv("AB_NYC_2019.csv")

######################
# Data Exploartion
######################

dim(data)

head(data)

names(data)

summary(data)

c(unique(data["neighbourhood_group"]))

c(unique(data["neighbourhood"]))

c(unique(data["room_type"]))

glue("Minimum Price: {min(data$price)} | Maximum Price: {max(data$price)}")

glue("Minimum Longitude : {min(data$longitude)} | Maximum Longitude: {max(data$longitude)}")

glue("Minimum Latitude : {min(data$latitude)} | Maximum Latitude: {max(data$latitude)}")

###########################
# Descriptive Data Anaylsis
###########################

##### Frequency Distribution ##### 

#neighbourhood_group/Location Freq table
freq_location <- data.frame(cbind(Frequency = table(data$neighbourhood_group), Percent = prop.table(table(data$neighbourhood_group)) * 100))
freq_location <- freq_location[order(freq_location$Frequency),]
freq_location

#neighbourhood/Area Freq table
freq_area <- data.frame(cbind(Frequency = table(data$neighbourhood), Percent = prop.table(table(data$neighbourhood)) * 100))
freq_area <- freq_area[order(freq_area$Frequency),]
freq_area

#room_type Freq table
freq_roomtype <- data.frame(cbind(Frequency = table(data$room_type), Percent = prop.table(table(data$room_type)) * 100))
freq_roomtype <- freq_roomtype[order(freq_roomtype$Frequency),]
freq_roomtype

# With the frequency tables created above, we can conclude that the frequency and the representative percentage of the most frequent categories of the categorical variables 
# neighborhood_group, neighborhood and room_type are-
# neighbourhood_group -   Manhattan -> 21661(44.30%)
# neighborhood        -   Williamsburg -> 3920(8.01%)
# room_type           -   Entire home/apt -> 25409(51.96%)


##### Histogram For Price ######

tema <- theme(plot.background = element_rect(fill = "#FFFAFA", color = "#FFFAFA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=14, repr.plot.height=6)
a <- ggplot(data = data, mapping = aes(x = price)) +
  geom_histogram(fill = "cyan", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Price") +
  ggtitle("Price Histogram") +
  tema

plot(a)

#As we can see most of prices our under 1000

df <- data.frame(price = data["price"][data["price"] <= 1000])
b <- ggplot(data = df, mapping = aes(x = price)) +
  geom_histogram(fill = "cyan", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Price") +
  ggtitle("Price <= 1000 | Histogram") +
  tema

plot(b)

#We can see most of the Price lie under 1000

##### Histogram For Minimum_Nights #####
a <- ggplot(data = data, mapping = aes(x = minimum_nights)) +
  geom_histogram(fill = "yellow", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Minimum nights") +
  ggtitle("Minimum nights Histogram") +
  tema

plot(a)

#As we can see most of minimum nights our under 50

df <- data.frame(minimum_nights = data["minimum_nights"][data["minimum_nights"] <= 50])
b <- ggplot(data = df, mapping = aes(x = minimum_nights)) +
  geom_histogram(fill = "yellow", bins = 70, size = 1.3, color = "black") +
  theme_minimal() +
  ylab("Frequency") +
  xlab("Minimum nights") +
  ggtitle("Minimum nights <= 50 | Histogram") +
  tema
plot(b)

# We can see that the minimum number of nights for all reservations made on airbnb are concentrated below 10 with a small peak at 30.

#### Bar Graph for Room Type ####

tema <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6)
a <- ggplot(data = freq_roomtype, mapping = aes(x = Frequency, y = row.names(freq_roomtype))) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_roomtype), color = row.names(freq_roomtype)), alpha = .7, size = 1.1) +
  geom_label(mapping = aes(label=Frequency), fill = "#006400", size = 6, color = "white", fontface = "bold", hjust=.7) +
  ylab("") +
  ggtitle("Room type distribution") +
  theme_economist() +
  tema
plot(a)

#We can see most of the rooms belong private or entire apt/home category

##### Bar Graph of The 10 most frequent neighbourhood #####

tema <- theme(plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 10, angle=15, face = "bold"),
              axis.text.y = element_text(size = 10, angle=10, face = "bold"),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10),
              legend.text = element_text(size = 10, face = "bold"))
df <- data.frame(neighbourhood = row.names(tail(freq_area, 10)), Frequency = tail(freq_area, 10)$Frequency)

options(repr.plot.width=15, repr.plot.height=6)
ggplot(data = df, mapping = aes(x = neighbourhood, y = Frequency)) +
  theme_minimal() + 
  geom_point(size = 6, color = "green") +
  ggtitle("The 10 most frequent neighbourhood") +
  xlab("") +
  geom_line(color = "black", size = 2, linetype= 16, group = 1, alpha = .5) + 
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1.5) +   
  tema

#  Of all 221 neighbourhoods ,these are the top 10 neighborhoods, which are most requested by customers for advertisements and accommodation reservations on the airbnb website

#### Bar Graph for neighbourhood_group ####

tema <- theme(plot.background = element_rect(fill = "#DCFCE6", color = "#66CDAA"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.position = "none")
tema1 <- theme(plot.background = element_rect(fill = "#DCFCE6", color = "#66CDAA"),
               plot.title = element_text(size = 23, hjust = .5),
               axis.text.x = element_text(size = 15, face = "bold"),
               axis.text.y = element_text(size = 15, face = "bold"),
               axis.title.x = element_text(size = 15),
               axis.title.y = element_text(size = 15),
               legend.position = "none")
options(repr.plot.width=14, repr.plot.height=6)
a<- ggplot(data = freq_location, aes(x=row.names(freq_location), y=Frequency)) +
  geom_bar(stat = "identity", mapping = aes(fill = row.names(freq_location), color = row.names(freq_location)), alpha = .7, size = 1.1) +
  coord_flip() +
  theme_economist() +
  xlab("") +
  ggtitle("Location Distribution") +
  tema

plot(a)

# Manhattan is the most famous neighbouring group folllowed by Brooklyn, Queens, Bronx and Staten Island

#### Seprating Measures ####
p <- c()
for(i in 1:99){
  
  p <- c(p, i / 100)
  
}

percentil <- data.frame(Price = quantile(data$price, p), minimum_nights = quantile(data$minimum_nights, p))
percentil

# Some important points that you can describe in relation to the type of analysis are:
# 1) 25.00% of bookings made on airbnb are of values equal to or less than 69 dollars and 1 minimum night.
# 2) 50.00% of bookings made on airbnb are of values equal to or less than 106 dollars and 3 minimum nights.
# 3) 75.00% of bookings made on airbnb are of values equal to or less than 175 dollars and 5 minimum nights.
# 4) 99.00% of bookings made on airbnb are of values equal to or less than 799 dollars and 45 minimum nights, meaning only 1.0% of bookings are of values above 799 dollars with a maximum value of 10000 dollars and 1250 minimum nights.

###########################
# Exploratory Data Anaylsis
###########################

#### Average price per room type ####
mean_room_type <- aggregate(list(average_price = data$price), list(room_type = data$room_type), mean)
mean_room_type$Percent <- prop.table(mean_room_type$average_price) * 100
mean_room_type
tema <- theme(
  plot.title = element_text(size = 23, hjust = .5),
  axis.text.x = element_text(size = 19, face = "bold"),
  axis.text.y = element_text(size = 19, face = "bold"),
  axis.title.x = element_text(size = 19),
  axis.title.y = element_text(size = 19),
  legend.position = "none")



options(repr.plot.width=15, repr.plot.height=6)
options(warn=-1)
a <- ggplot(data = mean_room_type, aes(x=room_type, y=average_price)) +
  coord_flip() +
  geom_segment(aes(xend=room_type, yend=0, color = room_type), size = 2) +
  geom_point(size=7, mapping = aes(color = room_type)) +
  theme_minimal() +
  xlab("") +
  ylab("") +
  ggtitle("Average price per room type") +
  tema
plot(a)

# We can conclude that
# 1) The Entire home / apt type has an average price for reservations around 211.79 dollars , which represents 56.97 % of all types of rooms . We have the Entire home / apt has an average price of 32.82% more expensive than the Private room and 38.11% more expensive than the Shared room .
# 2) The Private room which has an average booking price of around 89.78 dollar, which represents 24.15% of all types of rooms . We have that the Private room has an average price 32.82% less than Entire home / apt and 5.29% larger than the Shared room.
# 3) The Shared room which has an average booking price of around 70.12 dollars , which represents 18.86% of all types of rooms . We have that the Shared room has an average price 38.1% less than Entire home / apt and 5.29% smaller than the Private room.

#Price behavior in relation to room types #

tema1 <- theme(
  plot.title = element_text(size = 23, hjust = .5),
  axis.text.x = element_text(size = 19, face = "bold"),
  axis.text.y = element_text(size = 19, face = "bold"),
  axis.title.x = element_text(size = 21),
  axis.title.y = element_text(size = 21))

df <- data.frame(price = data["price"][data["price"] <= 1000], room_type = data["room_type"][data["price"] <= 1000])
c <- ggplot(data = df, mapping = aes(x = price, fill = room_type)) +
  geom_density(mapping = aes(fill = room_type), bins = 70, size = 1.3, color = "black", alpha = .6, size = 1.5) +
  theme_minimal() +
  ylab("Density") +
  xlab("Price") +
  ggtitle("Price <= 1000 | Histogram") +
  tema1 +
  theme(legend.position="bottom", legend.text = element_text(colour="black", size=20, 
                                                             face="bold"))
plot(c)

#In addition to obtaining information such as the average price for reservations, it is interesting to know how these values that resulted in the average are distributed.

#### Price in Relation to Neighborhood ####

#The 10 most expensive neighborhoods to book on airbnb

top_10_neighbourhood <- aggregate(list(data$price), list(data$neighbourhood), mean)
colnames(top_10_neighbourhood) <- c("neighbourhood", "Average_price_per_neighborhood")
top_10_neighbourhood <- top_10_neighbourhood[order(top_10_neighbourhood$Average_price_per_neighborhood),]
top_10_neighbourhood <- tail(top_10_neighbourhood, 12)
top_10_neighbourhood <- head(top_10_neighbourhood, 10)
r <- c()
for(i in 10:1){r <- c(r, i)}
row.names(top_10_neighbourhood) <- r
top_10_neighbourhood


#Plotting a graph depecting the same
tema <- theme(
  plot.title = element_text(size = 23, hjust = .5),
  axis.text.x = element_text(size = 19, face = "bold"),
  axis.text.y = element_text(size = 19, face = "bold"),
  axis.title.x = element_text(size = 21),
  axis.title.y = element_text(size = 21),
  legend.position = "none")
tema1 <- theme(
  plot.title = element_text(size = 23, hjust = .5),
  axis.text.x = element_text(size = 7, face = "bold"),
  axis.text.y = element_text(size = 0, face = "bold"),
  axis.title.x = element_text(size = 5),
  axis.title.y = element_text(size = 5),
  legend.position="none")

options(repr.plot.width=15, repr.plot.height=11)
a <- ggplot(data = top_10_neighbourhood, mapping = aes(x = neighbourhood, y = Average_price_per_neighborhood)) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1.5) +
  geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 2)), size = 6, fill = "#F5FFFA", fontface = "bold") +
  coord_flip() +
  theme_ipsum() + 
  ggtitle("The 10 most expensive neighborhoods") +
  xlab("") +
  ylab("") +
  tema

plot(a)

b <- ggplot(data = top_10_neighbourhood, mapping = aes(x = neighbourhood, y = Average_price_per_neighborhood)) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1.5) +
  theme_ipsum() + 
  ggtitle("The 10 most expensive neighborhoods") +
  xlab("") +
  ylab("") +
  tema1

plot_grid(b + coord_polar())

# Just as we did in Price behavior in relation to room types, let's see how the prices of the 10 neighborhoods with the highest average price for airbnb reservations are distributed.
tema <- theme(plot.background = element_rect(fill = "white"),
              plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 21),
              axis.title.y = element_text(size = 21),
              legend.position = "none")

na <- c("Tribeca", "Sea Gate", "Riverdale", "Prince's Bay", "Battery Park City", "Flatiron District", "Randall Manor", "NoHo", "SoHo", "Midtown")
df <- data.frame(neighbourhood = data$neighbourhood[data$neighbourhood == na], price = data$price[data$neighbourhood == na])

ggplot(data = df, mapping = aes(x = price, y = neighbourhood)) +
  geom_density_ridges(mapping = aes(fill = neighbourhood), bandwidth = 130, alpha = .6, size = 1.5) +
  theme_economist() +
  xlab("Price") +
  ylab("") +
  ggtitle("Price behavior in relation to neighborhoods") +
  tema

# Viewing the graph above we can see that the Sea Gate neighborhood does not show any density behavior, due to the fact that it probably only appears once or very few times in the data set. 

#The 10 cheapest neighborhoods to book on airbnb
top_10_neighbourhood_b <- aggregate(list(data$price), list(data$neighbourhood), mean)
colnames(top_10_neighbourhood_b) <- c("neighbourhood", "Average_price_per_neighborhood")
top_10_neighbourhood_b <- top_10_neighbourhood_b[order(top_10_neighbourhood_b$Average_price_per_neighborhood),]
top_10_neighbourhood_b <- head(top_10_neighbourhood_b, 10)
r <- c()
for(i in 1:10){r <- c(r, i)}
row.names(top_10_neighbourhood_b) <- r
top_10_neighbourhood_b

#Plotting a graph for the same
tema <- theme(plot.title = element_text(size = 19, hjust = .5),
              axis.text.x = element_text(size = 12, angle=15, face = "bold"),
              axis.text.y = element_text(size = 12, angle=10, face = "bold"),
              axis.title.x = element_text(size = 12),
              axis.title.y = element_text(size = 12),
              legend.position = "none")

options(repr.plot.width=15, repr.plot.height=6.5)
a <- ggplot(data = top_10_neighbourhood_b, mapping = aes(x = neighbourhood, y = Average_price_per_neighborhood)) +
  geom_bar(stat = "identity", mapping = aes(fill = neighbourhood, color = neighbourhood), alpha = .8, size = 1.5) +
  geom_label(mapping = aes(label = round(Average_price_per_neighborhood, 2)), size = 6, fill = "#F5FFFA", fontface = "bold") +
  theme_ipsum() + 
  ggtitle("The 10 cheapest neighborhoods") +
  xlab("") +
  ylab("") +
  tema

plot(a)

#### Geographic analysis ####

#In this analysis I will explore the behavior of the price, neighborhood_group, minimum_nights and room_type through the coordinated latitude and longitude available in the airbnb data. This type of exploitation is extremely useful for understanding the behavior of the data on a geographic scale, consequently helping in decision making.
tema <- theme(plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.text = element_text(colour="black", size=19, face="bold"),
              legend.background = element_rect(fill="#F5FFFA", size=0.5, linetype="dashed", 
                                               colour ="black"))

ggplot(data = data, mapping = aes(x = latitude, y = longitude)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = price), size = 3) +
  ggtitle("") +
  tema

#Further exploring in terms of neighbourhood_group, room_type and minimum nights
tema <- theme(plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.text = element_text(colour="black", size=7, face="bold"),
              legend.title = element_text(colour="black", size=9, face="bold"),
              legend.background = element_rect(fill="#F5FFFA", size=0.1, linetype="dashed", 
                                               colour ="black"))
options(repr.plot.width=22, repr.plot.height=14)
a<-ggplot(data = data, mapping = aes(x = latitude, y = longitude)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = neighbourhood_group), size = 3) +
  ggtitle("") +
  xlab("Latitude") +
  ylab("Longitude") +
  tema
plot(a)
b<-ggplot(data = data, mapping = aes(x = latitude, y = longitude)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = price), size = 3) +
  ggtitle("") +
  xlab("Latitude") +
  ylab("Longitude") +
  tema
plot(b)
c<-ggplot(data = data, mapping = aes(x = latitude, y = longitude)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = room_type), size = 3) +
  ggtitle("") +
  xlab("Latitude") +
  ylab("Longitude") +
  tema
plot(c)
d<-ggplot(data = data, mapping = aes(x = latitude, y = longitude)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = minimum_nights), size = 3) +
  ggtitle("") +
  xlab("Latitude") +
  ylab("Longitude") +
  tema
plot(d)
# As our database prices are mostly below 100 dollars, we will filter the data to obtain only bookings below 100 dollars.

df <- data.frame(price = data["price"][data["price"] <= 100], room_type = data["room_type"][data["price"] <= 100], lat = data["latitude"][data["price"] <= 100], lon = data["longitude"][data["price"] <= 100],
                 neighbourhood_group = data["neighbourhood_group"][data["price"] <= 100], minimum_nights = data["minimum_nights"][data["price"] <= 100])
df$minimum_nights <- factor(df$minimum_nights)
ggplot(data = df, mapping = aes(x = lat, y = lon, color = price)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = price), size = 3) +
  ggtitle("Price <= 100 dollars ") +
  tema

# Understanding how this grouping behaves in relation to the coordinate.
tema <- theme(plot.title = element_text(size = 23, hjust = .5),
              axis.text.x = element_text(size = 19, face = "bold"),
              axis.text.y = element_text(size = 19, face = "bold"),
              axis.title.x = element_text(size = 19),
              axis.title.y = element_text(size = 19),
              legend.text = element_text(colour="black", size=7, face="bold"),
              legend.title = element_text(colour="black", size=9, face="bold"),
              legend.background = element_rect(fill="#F5FFFA", size=0.1, linetype="dashed", 
                                               colour ="black"))
options(repr.plot.width=22, repr.plot.height=14)
a<-ggplot(data = df, mapping = aes(x = lat, y = lon)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = neighbourhood_group), size = 3) +
  ggtitle("Price <= 100 dollars") +
  xlab("Latitude") +
  ylab("Longitude") +
  tema
plot(a)
b<-ggplot(data = df, mapping = aes(x = lat, y = lon)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = price), size = 3) +
  ggtitle("Price <= 100 dollars ") +
  xlab("Latitude") +
  ylab("Longitude") +
  tema
plot(b)
c<-ggplot(data = df, mapping = aes(x = lat, y = lon)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = room_type), size = 3) +
  ggtitle("Price <= 100 dollars ") +
  xlab("Latitude") +
  ylab("Longitude") +
  tema
plot(c)
d<-ggplot(data = df, mapping = aes(x = lat, y = lon)) +
  theme_minimal() +
  scale_fill_identity() +
  geom_point(mapping = aes(color = minimum_nights), size = 3) +
  ggtitle("Price <= 100 dollars ") +
  xlab("Latitude") +
  ylab("Longitude") +
  tema
plot(d)

###############################
# Data prepartion for modelling
###############################

#Some of the variables have been removed from the dataset due to several reasons.
# 1) id,name,host_id and host_name: We know that logically, we don't look at these variables when we select a place to stay during vacation
# 2) neighbourhood : This variable consisted of a large number of factor levels and this complicates the models and also some neighbourhoods have very less information and eventually will result in misleading results
# 3) last_review : This was a date variable and we have extracted sufficient information from this variable and these information is recorded in the variable year_cat.

df <- data %>% select(price, # place target variable first
                      #id,
                      #name,                          
                      #host_id,
                      #host_name,
                      neighbourhood_group,
                      #neighbourhood,                 
                      latitude, 
                      longitude,                     
                      room_type,                                               
                      minimum_nights, 
                      number_of_reviews,             
                      #last_review, 
                      reviews_per_month,             
                      calculated_host_listings_count,
                      availability_365)
# Replacing missing values of reviews_per_month by 0
df$reviews_per_month[is.na(df$reviews_per_month)] <- 0

# Converting string to Factors        
columns <- c("neighbourhood_group","room_type")
df[, columns] <- df %>% select(all_of(columns)) %>% lapply(as.factor)

df %>% select(all_of(columns)) %>% str()
df %>% head(5)

#### Identify & Remove outliers - Price  ####
Q <- quantile(df$price, probs=c(.25, .75), na.rm = T)
iqr <- IQR(df$price, na.rm = T)
df2 <- df %>% filter(price > (Q[1] - 1.5*iqr) & 
                       price < (Q[2] + 1.5*iqr))  


####  Identify & Remove outliers - Minimum Nights  ####


Q <- quantile(df2$minimum_nights, probs=c(.25, .75), na.rm = T)
iqr <- IQR(df2$minimum_nights, na.rm = T)
df2 <- df2 %>% filter(minimum_nights > (Q[1] - 1.5*iqr) & 
                        minimum_nights < (Q[2] + 1.5*iqr))  

#####   Identify & Remove outliers - Number of Reviews  #####

Q <- quantile(df2$number_of_reviews, probs=c(.25, .75), na.rm = T)
iqr <- IQR(df2$number_of_reviews, na.rm = T)
df2 <- df2 %>% filter(number_of_reviews > (Q[1] - 1.5*iqr) & 
                        number_of_reviews < (Q[2] + 1.5*iqr))  



######        Correlations       ######

c <- df2
cols = c("neighbourhood_group", "room_type" )
c[, cols] <- c %>% select_if(is.factor) %>% lapply(as.numeric)
# create correlation matrix
cor <- cor(c)
#names(c) # gather all the column names
cols <- c("price",
          "neighbourhood_group",           
          "latitude", 
          "longitude",                     
          "room_type",
          "minimum_nights",                
          "number_of_reviews", 
          "reviews_per_month",             
          "calculated_host_listings_count",
          "availability_365" ) 
# stack the correlations
cor <- as_tibble(reshape2::melt(cor, id = cols))
# rename the columns appropriately
colnames(cor) <- c("Target", "Variable", "Correlation")

#    Corr Plot    


ggcorrplot(mat.sort(cor(c)), lab = TRUE, 
           colors = c("aquamarine", "white", "dodgerblue"),
           show.legend = F, outline.color = "gray", type = "upper") +
  labs(fill = "Correlation")


######      Multicollinearity     ######

# Dropping response variable (Y)
c %>% select(everything(), - price)

# Choose a VIF cutoff under which a variable is retained 
# vif > 10  = multi-collinearity (Zuur et al. 2010)
#can also reject predictors with vf 5-10

fit1 <- lm(price ~., data = c); #summary(fit1)
vif(fit1) 

#### Splitting into training and test sets ####

# sample the data for faster run time

s <- sample_n(df, 5000)
s = sample_n(s, size = 1000, replace = TRUE)
s = sample_n(s, size = 1000, replace = TRUE)
trainIndex <- createDataPartition(s$price, p = .75, 
                                  list = FALSE, times = 1)
train = s[trainIndex, ]; #class(train)
test = s[-trainIndex, ]; #class(test)

# Training & Testing Splits ( X & Y)

x_train <- train[-1]; #class(x_train)
x_test <- test[-1]; #class(x_test)
y_train <- train$price; #class(y_train)
y_test <- test$price; #class(y_test)

# Training & Testing Splits ( X & Y) Matrix

train.X.m <- model.matrix(price ~ ., train)[,-1]; #class(train.X.m)
train.Y <- train$price; #class(train.Y)
test.X.m <- model.matrix(price ~ ., test)[,-1]; #class(test.X.m);
test.Y <- test$price; #class(test.Y)

###################
# Data Modelling  #
###################

#####     Linear Regression  - Math Model     #####


m.lm <- lm(price ~ 
             #neighbourhood_group + 
             #latitude +                      
             #longitude + 
             room_type +                     
             minimum_nights + 
             number_of_reviews +             
             #reviews_per_month  +  
             #calculated_host_listings_count +
             availability_365,
           data = train);
y.pred.lm = predict(m.lm, newdata = x_test);
MAE.lm = round(caret::MAE(y.pred.lm, y_test),3);
MAE.lm


######   Partial Least-Squares Regression (PLS)    ######

# center and scale the data
m.mvr <- mvr(price ~., data = train,
             scale = TRUE, center = TRUE, ncomp = 3);


#Variable Importance 

options(repr.plot.width=14, repr.plot.height=7)
vip::vip(m.mvr) + theme_bw(base_size = 18) +
  labs(title = "Variable importance ~ Price")
y.pred.mvr = predict(m.mvr, newdata = x_test, type = "response", ncomp = 3);
MAE.mvr = round(caret::MAE(y.pred.mvr, y_test),3);
MAE.mvr

####      Boosted Generalized Linear Model      ######



m.blackboost <- blackboost(price ~., data = train, 
                           family = Gaussian())
y.pred.blackboost = predict(m.blackboost, newdata = x_test, type = "response");
MAE.blackboost = round(caret::MAE(y.pred.blackboost, y_test),3);
MAE.blackboost


####         Pruned Tree Models         ####


m_tree = rpart(formula = price ~ .,
               data = train, method = "anova",
               control =rpart.control(minsplit = 5, cp=0.0));
m_ocp = m_tree$cptable[which.min(m_tree$cptable[,"xerror"]), "CP"]


m.rpart = rpart(formula = price ~ .,
                data = train, method = "anova",
                control = rpart.control(cp = 0.02528936))


m.rpart.prn <- prune(m_tree, m_ocp);


#Visualize models before and after pruning

options(repr.plot.width=14, repr.plot.height=7)
par(mfrow = c(1,2))
prp(m.rpart, main = "Tree Model - Best Tuned Model");
prp(m.rpart.prn, main = "Tree Model - after Pruning");
par(mfrow = c(1,1))
y.pred.rpart = predict(m.rpart, newdata = x_test, type = "vector");
MAE.rpart = round(caret::MAE(y.pred.rpart, y_test),3);
MAE.rpart
y.pred.rpart.prn = predict(m.rpart.prn, newdata = x_test, type = "vector");
MAE.rpart.prn = round(caret::MAE(y.pred.rpart.prn, y_test),3);
MAE.rpart.prn

####     Bagged CART     ######


set.seed(123) 
m.treebag <- bagging(price~., data=train);
y.pred.treebag = predict(m.treebag, newdata = x_test);
MAE.treebag = round(caret::MAE(y.pred.treebag, y_test),3);
MAE.treebag

####        Random Forest     ######

set.seed(123) 
m.randomForest <- randomForest(price ~., data = train,
                               ntrees = 500,
                               mtry = 2);
y.pred.randomForest = predict(m.randomForest, newdata = x_test);
MAE.randomForest = round(caret::MAE(y.pred.randomForest, y_test),3);
MAE.randomForest
# Visualize the Varaible Importance

vip::vip(m.randomForest) + theme_bw(base_size = 18) +
  labs(title = "Variable importance");


#######    Stochastic Gradient Boosting      ######


set.seed(123) # Stochastic Gradient Boosting
m.gbm <- gbm(price~., data=train,
             distribution = "gaussian", 
             verbose=FALSE,
             n.trees = 1000, 
             interaction.depth = 3, 
             shrinkage = 0.01,
             n.minobsinnode = 10);

#  Visualize the Varaible Importance 

options(repr.plot.width=14, repr.plot.height=7)
vip::vip(m.gbm) + theme_bw(base_size = 18) +
  labs(title = "Variable importance");
y.pred.gbm  = predict(m.gbm, newdata = x_test, n.trees = 1000);
MAE.gbm = round(caret::MAE(y.pred.gbm,  y_test),3);
MAE.gbm

####   k-Nearest Neighbour    ####


set.seed(123) 
m.knn <- kknn(price ~., train, test, k = 9);
y.pred.knn = predict(m.knn, newdata = x_test, neighbors = 5);
MAE.knn = round(caret::MAE(y.pred.knn, y_test),3);
MAE.knn



####   Bagged Multivariate Adaptive Regression Spline    ####

set.seed(123) 
m.bagEarth <- bagEarth(x = train.X.m, y = train.Y, 
                       nprune = 8, degree = 1);
y.pred.bagEarth = predict(m.bagEarth, newdata = test.X.m);
MAE.bagEarth = round(caret::MAE(y.pred.bagEarth, y_test), 3);
MAE.bagEarth

#######        Ridge Regression       ######


# create the set of lambda values (will be powers of 10)
grid <- seq(10, -3, length=100);
lamVals <- 10 ^ grid

# fit the ridge regression model (alpha = 0 for ridge)

m.ridge <- glmnet(x = train.X.m, y = train.Y, alpha = 0, 
                  lambda = lamVals, standardize=TRUE)

# Cross Validation for optimal lambda (for Prediction)    ######

m_cv_ridge <- cv.glmnet(x = train.X.m, y = train.Y, alpha = 0, 
                        lambda = lamVals, standardize=TRUE);
plot(m_cv_ridge, main = "Cross Validated Model")
y.pred.ridge = predict(m.ridge, s = m_cv_ridge$lambda.min,
                       newx = test.X.m);
MAE.ridge = round(caret::MAE(y.pred.ridge, y_test),3);
MAE.ridge

#######        Lasso Regression       ######


m.lasso <- glmnet(x = train.X.m, y = train.Y, alpha = 1, 
                  lambda = lamVals, standardize=TRUE)

# Cross Validation for optimal lambda (for Prediction)    ######


m_lasso <- cv.glmnet(x = train.X.m, y = train.Y, alpha = 1, 
                     lambda = lamVals, standardize=TRUE);
plot(m_lasso, main = "Cross Validated Model")
y.pred.lasso = predict(m.lasso, s = m_lasso$lambda.min,
                       newx = test.X.m);
MAE.lasso = round(caret::MAE(y.pred.lasso, y_test),3);
MAE.lasso

####   Visualize the Ridge & Lasso Models   ####


par(mfrow = c(1,2))
plot(m.ridge, xvar = "lambda", main = "Ridge Model")
plot(m.lasso, xvar = "lambda", main = "Lasso Model")
par(mfrow = c(1,1))



#######      Support Vector Machines with Linear Kernel    ######


m.svmLinear = svm(formula = price ~ ., data = train,
                  type = 'eps-regression', kernel = 'linear')
y.pred.svmLinear = predict(m.svmLinear, newdata = x_test);
MAE.svmLinear = round(caret::MAE(y.pred.svmLinear, y_test),3);
MAE.svmLinear

#######       Support Vector Machines      ######
#######   Radial Basis Function Kernel     ######



# Tuning parameters: sigma (Sigma) , C (Cost)
set.seed(123) # Support Vector Machines with Radial Basis Function Kernel
m.svmRadial <- ksvm(price ~ ., data = train, type = "eps-svr",
                    sigma = 0.04865976, C = 0.5, scaled = T);
y.pred.svmRadial = predict(m.svmRadial, newdata = x_test);
MAE.svmRadial = round(caret::MAE(y.pred.svmRadial, y_test),3);
MAE.svmRadial

####     Support Vector Tuned Models      #####


#Tune the Model to Improve Accuracy 

# using 10 fold Cross Validation
m.tune.1 <- tune(svm, price ~ ., data = train, 
                 ranges = list(gamma = 2^(-1:1), cost = 2^(2:4)),
                 tunecontrol = tune.control(sampling = "fix"))
# gamma as a sequence = seq(0.001, 1, length = 10)
m.tune.2 <- tune(svm, price ~ ., data = train, 
                 ranges = list(gamma = seq(0.001, 1, length = 10), cost = 2^(2:4)),
                 tunecontrol = tune.control(sampling = "fix"))

m.SVMtun.1 = svm(formula = price ~ ., data = train,
                 type = 'eps-regression',
                 kernel = 'radial',
                 gamma = 0.5, epsilon = 0.1, 
                 cost = 4)
y.pred.SVMtun.1 = predict(m.SVMtun.1, newdata = x_test);
MAE.SVMtun.1 = round(caret::MAE(y.pred.SVMtun.1, y_test),3);
MAE.SVMtun.1

m.SVMtun.2 = svm(formula = price ~ ., data = train,
                 type = 'eps-regression',
                 kernel = 'radial',
                 gamma = 0.445, epsilon = 0.1, 
                 cost = 16)


y.pred.SVMtun.2 = predict(m.SVMtun.2, newdata = x_test);
MAE.SVMtun.2 = round(caret::MAE(y.pred.SVMtun.2,y_test), 3);
MAE.SVMtun.2



#Calculating RMSE
RMSE.lm = round(caret::RMSE(y.pred.lm, y_test),3);
RMSE.mvr = round(caret::RMSE(y.pred.mvr, y_test),3);
RMSE.blackboost = round(caret::RMSE(y.pred.blackboost, y_test),3);
RMSE.rpart = round(caret::RMSE(y.pred.rpart, y_test),3);
RMSE.rpart.prn = round(caret::RMSE(y.pred.rpart.prn, y_test),3);
RMSE.treebag = round(caret::RMSE(y.pred.treebag, y_test),3);
RMSE.randomForest = round(caret::RMSE(y.pred.randomForest, y_test),3);
RMSE.gbm = round(caret::RMSE(y.pred.gbm,  y_test),3);
RMSE.knn = round(caret::RMSE(y.pred.knn, y_test),3);
RMSE.bagEarth = round(caret::RMSE(y.pred.bagEarth, y_test), 3);
RMSE.ridge = round(caret::RMSE(y.pred.ridge, test.Y),3);
RMSE.lasso = round(caret::RMSE(y.pred.lasso, test.Y),3);
RMSE.svmLinear = round(caret::RMSE(y.pred.svmLinear, y_test),3);
RMSE.svmRadial = round(caret::RMSE(y.pred.svmRadial, y_test),3);
RMSE.SVMtun.1 = round(caret::RMSE(y.pred.SVMtun.1, y_test),3);
RMSE.SVMtun.2 = round(caret::RMSE(y.pred.SVMtun.2,y_test), 3);

#R Squared
R2.lm = round(caret::R2(y.pred.lm, y_test),3);
R2.mvr = round(caret::R2(y.pred.mvr, y_test),3);
R2.blackboost = round(caret::R2(y.pred.blackboost, y_test),3);
R2.rpart = round(caret::R2(y.pred.rpart, y_test),3);
R2.rpart.prn = round(caret::R2(y.pred.rpart.prn, y_test),3);
R2.treebag = round(caret::R2(y.pred.treebag, y_test),3);
R2.randomForest = round(caret::R2(y.pred.randomForest, y_test),3);
R2.gbm = round(caret::R2(y.pred.gbm,  y_test),3);
R2.knn = round(caret::R2(y.pred.knn, y_test),3);
R2.bagEarth = round(caret::R2(y.pred.bagEarth, y_test), 3);
R2.ridge = round(caret::R2(y.pred.ridge, y_test),3);
R2.lasso = round(caret::R2(y.pred.lasso, y_test),3);
R2.svmLinear = round(caret::R2(y.pred.svmLinear, y_test),3);
R2.svmRadial = round(caret::R2(y.pred.svmRadial, y_test),3);
R2.SVMtun.1 = round(caret::R2(y.pred.SVMtun.1, y_test),3);
R2.SVMtun.2 = round(caret::R2(y.pred.SVMtun.2,y_test), 3);


##################
#     Result     #
##################

######  Compare Model Performance    #####


Model <- c("m.lm",
           "m.mvr",
           "m.blackboost",
           "m.rpart",
           "m.rpart.prn",
           "m.treebag",
           "m.randomForest",
           "m.gbm",
           "m.knn",
           "m.bagEarth",
           "m.ridge",
           "m.lasso",
           "m.svmLinear",
           "m.svmRadial",
           "m.SVMtun.1",
           "m.SVMtun.2")

Model.MAE = c(MAE.lm,
              MAE.mvr,
              MAE.blackboost,
              MAE.rpart,
              MAE.rpart.prn,
              MAE.treebag,
              MAE.randomForest,
              MAE.gbm,
              MAE.knn,
              MAE.bagEarth,
              MAE.ridge,
              MAE.lasso,
              MAE.svmLinear,
              MAE.svmRadial,
              MAE.SVMtun.1,
              MAE.SVMtun.2)


Model.RMSE <- c(RMSE.lm,
                RMSE.mvr,
                RMSE.blackboost,
                RMSE.rpart,
                RMSE.rpart.prn,
                RMSE.treebag,
                RMSE.randomForest,
                RMSE.gbm,
                RMSE.knn,
                RMSE.bagEarth,
                RMSE.ridge,
                RMSE.lasso,
                RMSE.svmLinear,
                RMSE.svmRadial,
                RMSE.SVMtun.1,
                RMSE.SVMtun.2)


Model.R2 <- c(R2.lm,
              R2.mvr,
              R2.blackboost,
              R2.rpart,
              R2.rpart.prn,
              R2.treebag,
              R2.randomForest,
              R2.gbm,
              R2.knn,
              R2.bagEarth,
              R2.ridge,
              R2.lasso,
              R2.svmLinear,
              R2.svmRadial,
              R2.SVMtun.1,
              R2.SVMtun.2)

# combine all into a data frame
mpc <- cbind.data.frame(Model, Model.MAE, Model.RMSE, Model.R2); mpc
mpc$Model <- as.factor(mpc$Model);
mpc %>% glimpse()


######       Final Model Comparisons        ##### 


a = ggplot(mpc, aes(x = fct_reorder(Model, Model.R2), Model.R2))  +
  geom_bar(stat = "identity", fill = "#C7E9B4", col = "black", lwd = 0.5) +
  theme_bw(base_size = 20) + labs(title = "R Squared",  x = "", y = "") +
  geom_text(aes(label=Model.R2), hjust="inward", nudge_y = -0.05,
            color="black", size=4, fontface = "italic") + coord_flip()

b = ggplot(mpc, aes(x = fct_reorder(Model, Model.R2), Model.RMSE))  +
  geom_bar(stat = "identity", fill = "#7FCDBB", col = "black", lwd = 1) +
  theme_bw(base_size = 20) + labs(title = "RMSE",  x = "", y = "") +
  geom_text(aes(label=Model.RMSE), hjust="inward",  nudge_y = -0.05,
            color="black", size=4, fontface = "italic") + coord_flip() +
  theme(axis.text.y=element_blank(),axis.ticks=element_blank())

c = ggplot(mpc, aes(x = fct_reorder(Model, Model.R2), Model.MAE)) +
  geom_bar(stat = "identity", fill = "#41B6C4", col = "black", lwd = 1) +
  theme_bw(base_size = 20) + labs(title = "MAE", x = "", y = "") +
  geom_text(aes(label=Model.MAE), hjust="inward",  nudge_y = -0.05,
            color="black", size=4, fontface = "italic") + coord_flip() +
  theme(axis.text.y=element_blank(),axis.ticks=element_blank())

options(repr.plot.width=16, repr.plot.height=20)
plot_grid(a,b,c, ncol = 3, nrow = 1,
          rel_widths = c(1.5,1,1))

####   Scatter plot of Model Performance   ####


options(repr.plot.width=16, repr.plot.height=10)
ggplot(mpc, aes(Model.RMSE, Model.R2, label = Model, color = Model)) + 
  geom_point(show.legend = F, pch = 17, size = 4, stroke = 5) + 
  geom_text_repel(show.legend = F, size = 4, nudge_y = 0.02, nudge_x = 0.03, 
                  segment.color = NA, fontface = "bold.italic") + 
  theme_bw(base_size = 18) + # ylim(0.4, 0.8) +
  labs(title = "Model Performance Comparison", x = "RMSE", y = "R Squared")

#### Performance of Random Forest ####
x <- data.frame("MAE" = MAE.randomForest, "RMSE" = RMSE.randomForest, "R-Squared" = R2.randomForest)
x

# Best Model- Random Forest
# RMSE = 161.849 and MAE = 46.501.

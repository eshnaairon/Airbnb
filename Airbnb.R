# Edx Capstone - Traffic Volume Interstate Analysis
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
#########################
# Downloading the Dataset 
#########################

data<-read.csv("C:/Users/eshna airon/Downloads/AB_NYC_2019.csv")

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

tema1 <- theme(plot.background = element_rect(fill = "#EEE8AA", color = "yellow"),
               plot.title = element_text(size = 23, hjust = .5),
               axis.text.x = element_text(size = 12, face = "bold"),
               axis.text.y = element_text(size = 12, face = "bold"),
               axis.title.x = element_text(size = 12),
               axis.title.y = element_text(size = 12),
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

# Tallying the fact that expensive neighbour hoods have less bookings
tema <- theme(
  plot.title = element_text(size = 10, hjust = .5),
  axis.text.x = element_text(size = 7, face = "bold"),
  axis.text.y = element_text(size = 7, face = "bold"),
  axis.title.x = element_text(size = 8),
  axis.title.y = element_text(size = 8),
  legend.position = "none",
  strip.text.x = element_text(margin = margin(10, 10, 10, 10), size = 7))
options(repr.plot.width=10, repr.plot.height=15)
ggplot(data = df, mapping = aes(x = price)) +
  geom_histogram(mapping = aes(fill = neighbourhood), bins = 70, size = 1, color = "black") +
  ylab("Frequency") +
  xlab("Price") +
  ggtitle("Price Histogram") +
  facet_wrap(~neighbourhood, nrow=4) +
  theme_ipsum() +
  tema

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
plot

###############################
# Data prepartion for modelling
###############################

# It is trivial that when the standards of a listing increases, the price tends to rise. Reviews by customers is a good indicator of this fact. Unfortunately the dataset gives only the number of reviews and their rates. Thus we do not have enough evidence to check if the reviews are good or bad. Hence, a sensible idea would be to see how recent the reviews are. This fact at least indicates if the listing is active. Therefore a categorical variable was created taking the year of last review into consideration.
#extracting years
year_last_review=format(as.Date(data$last_review),"%Y")

year_as_cat=year_last_review
year_as_cat=as.numeric(year_as_cat)
year_as_cat[which(is.na(year_as_cat))]=0
year_as_cat=as.factor(year_as_cat)
year_cat=c()
for (i in 1:length(year_as_cat)) {
  if(is.na(year_as_cat[i])){
    year_cat[i]="No reviews"
  }else if(year_as_cat[i]=="2019"){
    year_cat[i]="Last 6 months"
  }else if(year_as_cat[i]=="2018"){
    year_cat[i]="1 yr back"
  }else if(year_as_cat[i]=="2017"){
    year_cat[i]="2 yrs back"
  }else if(year_as_cat[i]=="2016"){
    year_cat[i]="3 yrs back"
  }else if(year_as_cat[i]=="2015"){
    year_cat[i]="4 yrs back"
  }else if(year_as_cat[i]=="2014"){
    year_cat[i]="5 yrs back"
  }else {
    year_cat[i]="6 or more yrs back"
  }
}

table(year_cat)
data1= cbind(data,year_cat)
#Removing the index column
data2=data1[,-1]
#Structure of the dataset
str(data2)

final <- data.frame(neighbourhood_group=data2$neighbourhood_group,room_type=data2$room_type,price=data2$price,minimum_nights=data2$minimum_nights,number_of_reviews=data2$number_of_reviews,calculated_host_listings_count=data2$calculated_host_listings_count,year_cat=data2$year_cat,stringsAsFactors = T)
str(final)

#Some of the variables have been removed from the dataset due to several reasons.
# 1) id,name,host_id : We know that logically, we don't look at these variables when we select a place to stay during vacation
# 2) neighbourhood : This variable consisted of a large number of factor levels and this complicates the models and also some neighbourhoods have very less information and eventually will result in misleading results
# 3) last_review : This was a date variable and we have extracted sufficient information from this variable and these information is recorded in the variable year_cat.
# 4) availability_365 : This variable is created once the listing is published in the website. The objective here is to give a suggestion to the hosts about the amount they could earn per night from their listings. Hence this variable is more likely to depend on the price suggested by the host. Thus, including this variable in the model will not be meaningful.

#### Splitting into training and test sets ####

# 80% traning data and 20% testing
set.seed(222)
ind=sample(2,nrow(final),replace = T,prob = c(0.8,0.2))
train=final[ind==1,]
test=final[ind==2,]


#Full model
X=model.matrix(price~.,final)[,-1]
Y=final$price

#Training data
x1=model.matrix(price~.,train)[,-1]
y1=train$price

#Test data
x2=model.matrix(price~.,test)[,-1]
y2=test$price

########################
# Modelling
########################

#Predicting the price of Airbnb listings as a suggestion for the hosts#

#### Ridge Regression ####
grid=10^seq(10,-2,length =100)
ridge.mod=glmnet(x1,y1,alpha =0,lambda =grid)
plot(ridge.mod,xvar = "lambda",label = T,lw=2)

set.seed(222)
cv.out =cv.glmnet(x1,y1,alpha =0)
plot(cv.out)
bestlam =cv.out$lambda.min

#MSE for ridge
ridge.pred=predict(ridge.mod,s=bestlam,newx=x2)
mean((ridge.pred-y2)^2)

# MSE ridge= 38105.17

#### Lasso Regression ####
lasso.mod =glmnet(x1,y1,alpha =1,lambda =grid)
plot(lasso.mod,xvar = "lambda",label = T,lw=2)

set.seed(222)
cv.out =cv.glmnet(x1,y1,alpha =1)
plot(cv.out)
bestlam1=cv.out$lambda.min

#MSE for Lasso
lasso.pred=predict(lasso.mod,s=bestlam1,newx=x2)
mean((lasso.pred -y2)^2)

# MSE Lasso = 38102.9

#### Elastic Net Regression ####
set.seed(222)
list.of.fits=list()

for(i in 0:10){
  fit.name=paste0("alpha",i/10)
  
  list.of.fits[[fit.name]]=cv.glmnet(x1,y1,type.measure = "mse",alpha=i/10)
}
results=data.frame()
for(i in 0:10){
  fit.name=paste0("alpha",i/10)
  
  predicted=predict(list.of.fits[[fit.name]],s=list.of.fits[[fit.name]]$lambda.min,newx=x2)
  
  mse=mean((y2-predicted)^2)
  bestlam=list.of.fits[[fit.name]]$lambda.min
  temp=data.frame(alpha=i/10,mse=mse,bestlam)
  results=rbind(results,temp)
}
results
#best model
results[which(results$mse==min(results[-1,]$mse)),]

#The best model under Elastic Net Regression is yielded when alpha = 0.9 MSE= 38100.21

#### Support Vector Regression ####
#radial kernel
svm1=svm(price~., data =train , type="nu-regression",kernel="radial", shrinking = T )
pred1=predict(svm1, newdata = test)
#MSE for radial SVM
mean((pred1 -test$price)^2)

#MSE radial SVM = 118363.3

#linear kernel
svm2=svm(price~., data =train , type="nu-regression",kernel="linear", shrinking = T )
pred2=predict(svm2, newdata = test)
#MSE for linear SVM
mean((pred2 -test$price)^2)

# MSE linear SVM  = 118132.8

#polynomial kernel
svm3=svm(price~., data =train , type="nu-regression",kernel="polynomial", shrinking = T )
pred3=predict(svm3, newdata = test)
#MSE for polynomial SVM
mean((pred3 -test$price)^2)

#MSE polynomial SVM = 121743.7

#sigmoid kernel
svm4=svm(price~., data =train , type="nu-regression",kernel="sigmoid", shrinking = T )
pred4=predict(svm4, newdata = test)
#MSE for sigmoid SVM
mean((pred4 -test$price)^2)

#MSE sigmod SVM = 270459.7

# By comparing the values obtained above, it is clear that the best MSE was given by Elastic Net Regression, which is 38100.21.


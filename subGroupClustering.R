## grouping clustering
library("scatterplot3d")
library(dplyr)
library(kernlab)
library(cluster)
## project prelims
setwd("/Users/riddhib/Desktop/fall2020/STA_141a/final")
travel_review <- read.csv("./google_review_ratings.csv", stringsAsFactors=FALSE)
head(travel_review)
## correcting for errors
error_lines= which(!is.na(travel_review$X))
# One line has Category.24 misplaced at another column
travel_review$Category.24[error_lines[1]] = travel_review$X[error_lines[1]]
# One line has put a character value at Category.11. Correct it to 2.  
travel_review$Category.11[error_lines[2]] = 2
# The line misplaced all columns after Category.12.
travel_review[error_lines[2],13:25] = travel_review[error_lines[2],14:26]
# Change Category.11 to numeric 
travel_review$Category.11 = as.numeric(travel_review$Category.11)
# Delete an extra column  
review_data = travel_review[ names(travel_review) != "X"]
# Delete the temporary variables/data
if(exists('travel_review')) 
    rm(travel_review)
if(exists('error_lines'))
    rm(error_lines)


data.mat = data.matrix(review_data)
data.mat = data.mat[,-1]

## feature combinations
#aman
# gym, swimming pools, bakeries
# resort, beach, spa
# church, art galleries, monuments

#Riddhi
# view points, park, beach
# theater, art galleries, cafe
# restaurants, bar, local service

#karl
# bar, burger pizza, juice bar
# zoo, hotel, dance clubs
# bakeries, cafe, park




review_data %>%rename(
    avg_church = Category.1,
    avg_resort = Category.2,
    avg_beach = Category.3,
    avg_park = Category.4,
    avg_theatre = Category.5,
    avg_museum = Category.6,
    avg_mall = Category.7,
    avg_zoo = Category.8,
    avg_restaurant = Category.9,
    avg_bar = Category.10,
    avg_local_service = Category.11,
    avg_burger_pizza = Category.12,
    avg_hotel = Category.13,
    avg_juice_bars = Category.14,
    avg_art_galleries = Category.15,
    avg_dance_clubs = Category.16,
    avg_swimming_pool = Category.17,
    avg_gym = Category.18,
    avg_bakeries = Category.19,
    avg_spa = Category.20,
    avg_cafe = Category.21,
    avg_view_points = Category.22,
    avg_monuments = Category.23,
    avg_gardens = Category.24
)

c1  = review_data %>% select(gym, bakeries, swimming_pool)
c1 = data.matrix(c1)
ggplot(c1, aes(x=swimming_pool, y=gym)) + 
    geom_point(aes(color=bakeries))

kmax=7
silList=rep(0,kmax-1)
for(i in 2:kmax){
    kmeans.out <- kmeans(c1,i,nstart=50,iter.max = 15)
    ss <- silhouette(kmeans.out$cluster, dist(data.mat))
    silList[i-1] = mean(ss[, 3])
}
silList

kmeans.out <- kmeans(c1,3,nstart=50,iter.max = 15)
c1 = cbind(c1,kmeans.out$cluster)
colnames(c1)[4] <- "cluster"
c1[,4] = as.factor(c1[,4])
ggplot(c1, aes(x=swimming_pool, y=gym, shape = cluster)) + 
    geom_point(aes(color=bakeries))





# view points, park, beach
c1  = review_data %>% select(park, view_points, beach)
#c1 = data.matrix(c1)
ggplot(c1, aes(x=view_points, y=park)) + 
    geom_point(aes(color=beach))
kmax=7
silList1=rep(0,kmax-1)
for(i in 2:kmax){
    kmeans.out <- kmeans(c1,i,nstart=50,iter.max = 15)
    ss <- silhouette(kmeans.out$cluster, dist(data.mat))
    silList1[i-1] = mean(ss[, 3])
}
silList1

# theater, art galleries, cafe
c1  = review_data %>% select(theatre, art_galleries, cafe)
c1 = data.matrix(c1)
kmax=7
silList2=rep(0,kmax-1)
for(i in 2:kmax){
    kmeans.out <- kmeans(c1,i,nstart=50,iter.max = 15)
    ss <- silhouette(kmeans.out$cluster, dist(data.mat))
    silList2[i-1] = mean(ss[, 3])
}
silList2

# restaurants, bar, local service
c1  = review_data %>% select(restaurant, bar, local_service)
c1 = data.matrix(c1)
kmax=7
silList3=rep(0,kmax-1)
for(i in 2:kmax){
    kmeans.out <- kmeans(c1,i,nstart=50,iter.max = 15)
    ss <- silhouette(kmeans.out$cluster, dist(data.mat))
    silList3[i-1] = mean(ss[, 3])
}
silList3


c1  = review_data %>% select(beach,art_galleries)
c1 = data.matrix(c1)
kmax=7
silList4=rep(0,kmax-1)
for(i in 2:kmax){
    kmeans.out <- kmeans(c1,i,nstart=50,iter.max = 15)
    ss <- silhouette(kmeans.out$cluster, dist(data.mat))
    silList4[i-1] = mean(ss[, 3])
}
silList4

scatterplot3d(c1, pch = 16, angle = )

ggplot(c1, aes(x=gym, y=bakeries)) + 
    geom_point(aes(size=swimming_pool))

ggplot(c1, aes(x=gym, y=bakeries)) + 
    geom_point(aes(color=swimming_pool))


## covarience matrix
#install.packages("reshape2")
library(reshape2)
cormat = cor(data.mat,y=data.mat)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))

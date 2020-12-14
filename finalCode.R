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

review_data = review_data %>% rename(
    church = Category.1,
    resort = Category.2,
    beach = Category.3,
    park = Category.4,
    theatre = Category.5,
    museum = Category.6,
    mall = Category.7,
    zoo = Category.8,
    restaurant = Category.9,
    bar = Category.10,
    local_service = Category.11,
    burger_pizza = Category.12,
    hotel = Category.13,
    juice_bars = Category.14,
    art_galleries = Category.15,
    dance_clubs = Category.16,
    swimming_pool = Category.17,
    gym = Category.18,
    bakeries = Category.19,
    spa = Category.20,
    cafe = Category.21,
    view_points = Category.22,
    monuments = Category.23,
    gardens = Category.24
)


#data.mat = data.matrix(review_data)
#data.mat = data.mat[,-1]


# view points, park, beach
# theater, art galleries, cafe
# restaurants, bar, local service
#24,18,20
#16,5,20
#2,19,8
#15,10,19
#22,6,12
#8,2,10
c1  = review_data %>% select(gym, swimming_pool, bakeries)

kmeans.out <- kmeans(c1,3,nstart=50,iter.max = 15)
c1 = cbind(c1,kmeans.out$cluster)
colnames(c1)[4] <- "cluster"
c1[,4] = as.factor(c1[,4])
scatterplot3d(c1[,1:3], pch = 16, angle = 55, color = c1[,4])
ggplot(c1, aes(x=swimming_pool, y=gym, shape = cluster)) + 
    geom_point(aes(color=bakeries))


silInds = c()
s1 = subClustering("view_points", "park", "beach", review_data)
silInds = append(silInds, s1$highC)
s2 = subClustering("theatre", "art_galleries", "cafe", review_data)
silInds = append(silInds, s2$highC)
s3 = subClustering("restaurant", "bar", "local_service", review_data)
silInds = append(silInds, s3$highC)
s4 = subClustering("gardens", "gym", "spa", review_data)
silInds = append(silInds, s4$highC)
s5 = subClustering("dance_clubs", "theatre", "spa", review_data)
silInds = append(silInds, s5$highC)
s6 = subClustering("resort", "bakeries", "zoo", review_data)
silInds = append(silInds, s6$highC)
s7 = subClustering("art_galleries", "bar", "bakeries", review_data)
silInds = append(silInds, s7$highC)
s8 = subClustering("view_points", "museum", "burger_pizza", review_data)
silInds = append(silInds, s8$highC)
s9 = subClustering("zoo", "resort", "bar", review_data)
silInds = append(silInds, s9$highC)


silInds = c()
s1 = subClustering("view_points", "park", "beach", review_data)
silInds = append(silInds, s1$highC)
s2 = subClustering("theatre", "art_galleries", "cafe", review_data)
silInds = append(silInds, s2$highC)
s3 = subClustering("restaurant", "bar", "local_service", review_data)
silInds = append(silInds, s3$highC)
s4 = subClustering("gardens", "gym", "spa", review_data)
silInds = append(silInds, s4$highC)
s5 = subClustering("dance_clubs", "theatre", "spa", review_data)
silInds = append(silInds, s5$highC)
s6 = subClustering("resort", "bakeries", "zoo", review_data)
silInds = append(silInds, s6$highC)
s7 = subClustering("art_galleries", "bar", "bakeries", review_data)
silInds = append(silInds, s7$highC)
s8 = subClustering("view_points", "museum", "burger_pizza", review_data)
silInds = append(silInds, s8$highC)
s9 = subClustering("zoo", "resort", "bar", review_data)
silInds = append(silInds, s9$highC)


ks1 = subClustering('bar', 'burger_pizza', 'juice_bars', review_data)
ks2 = subClustering('zoo', 'hotel', 'dance_clubs', review_data)
ks3 = subClustering('bakeries', 'cafe', 'park', review_data)
ks4 = subClustering('gardens', 'resort', 'swimming_pool', review_data)
ks5 = subClustering('restaurant', 'theatre', 'bakeries', review_data)
ks6 = subClustering('museum', 'spa', 'art_galleries', review_data)
ks7 = subClustering('park', 'burger_pizza', 'church', review_data)
ks8 = subClustering('dance_clubs', 'monuments', 'museum', review_data)
ks9 = subClustering('art_galleries', 'gym', 'bar', review_data)




as1= subClustering('gym','park','mall',review_data)
as2=subClustering('gym','swimming_pool','bakeries',review_data)
as3=subClustering('resort','beach','spa',review_data)
as4=subClustering('church','art_galleries','monuments',review_data)
as5=subClustering('gardens','hotel','monuments',review_data)
as6=subClustering('bar','gym','local_service',review_data)
as7=subClustering('theatre','view_points','museum',review_data)
as8=subClustering('theatre','museum','restaurant',review_data)
as9=subClustering('hotel','art_galleries','gardens',review_data)
as10=subClustering('art_galleries','monuments','gardens',review_data)


as1$highC
as2$highC
as3$highC
as4$highC
as5$highC
as6$highC
as7$highC
as8$highC
as9$highC
as10$highC

#three lowest
as1$highC
as8$highC
as7$highC
#three highest
as2$highC
as10$highC
as9$highC

#Combos to include in the report
high1 = subClustering('gym','swimming_pool','bakeries',review_data)
h1data = high1$datamat
plot = scatterplot3d(h1data[,1:3], pch = 16, angle = 115, 
                     color = h1data[,4], xlim = c(0,5), ylim = c(0,5), zlim= c(0,5))
high2 = subClustering("gardens", "gym", "spa", review_data)
high3 = subClustering('art_galleries','monuments','gardens',review_data)

low1 = subClustering("zoo", "resort", "bar", review_data)
low2 = subClustering('park', 'burger_pizza', 'church', review_data)
low3 = subClustering("resort", "bakeries", "zoo", review_data)

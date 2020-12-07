## grouping clustering

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

# check to see that all observations are complete
sum(!complete.cases(review_data))
#pairs(review_data[,2:25],pch = 19, lower.panel = NULL)
?pairs

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

c1  = review_data %>% select(Category.17, Category.18, Category.19)
c1 = data.matrix(c1)

kmax=7
silList=rep(0,kmax-1)
for(i in 2:kmax){
    kmeans.out <- kmeans(c1,i,nstart=50,iter.max = 15)
    ss <- silhouette(kmeans.out$cluster, dist(data.mat))
    silList[i-1] = mean(ss[, 3])
}
silList
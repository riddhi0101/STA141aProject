## grouping clustering function

library("scatterplot3d")
library(dplyr)
library(kernlab)
library(cluster)
library(ggplot2)
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

## input arguements: 3 catagories to cluster, wholw data frame, kmax(there is a default)
## output: list of the silhoutte index for 2-7 clusterings
subClustering <- function(cat1,cat2,cat3,datac,kmax = 7){
    c1  = datac %>% select(cat1, cat2, cat3)
    #c1 = data.matrix(c1)
    
    silList=rep(0,kmax-1)
    for(i in 2:kmax){
        kmeans.out <- kmeans(c1,i,nstart=50,iter.max = 15)
        ss <- silhouette(kmeans.out$cluster, dist(c1))
        silList[i-1] = mean(ss[, 3])
    }
    a = which(silList == max(silList)) + 1
    kmeans.out <- kmeans(c1,a,nstart=50,iter.max = 15)
    c1 = cbind(c1,kmeans.out$cluster)
    colnames(c1)[4] <- "cluster"
    c1[,4] = as.factor(c1[,4])
    a = ggplot(c1, aes(x=cat1, y=cat2, shape = cluster)) + 
        geom_point(aes(color=cat3))
    return(list(silList = silList, datamat = c1, plot = a))
}


ss = subClustering('gym','park','mall',review_data)
ss$plot

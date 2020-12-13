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







## plotting
scatterplot3d(c1, pch = 16, angle = 55)

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

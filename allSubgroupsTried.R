
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
high2 = subClustering("gardens", "gym", "spa", review_data)
high3 = subClustering('art_galleries','monuments','gardens',review_data)

low1 = subClustering("zoo", "resort", "bar", review_data)
low2 = subClustering('park', 'burger_pizza', 'church', review_data)
low3 = subClustering("resort", "bakeries", "zoo", review_data)

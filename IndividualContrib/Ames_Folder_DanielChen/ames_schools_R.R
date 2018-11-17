library(AmesHousing)

schools = ames_schools
#School    Longitude Latitude
#<chr>         <dbl>    <dbl>
#1 Edwards       -93.7     42.0
#2 Fellows       -93.6     42.0
#3 Meeker        -93.6     42.0
#4 Mitchell      -93.6     42.0
#5 Sawyer        -93.7     42.0
#6 Northwood     -93.6     42.1
#7 Middle        -93.7     42.0
#8 High          -93.6     42.0

geo = ames_geo
#PID        Longitude Latitude nearest_in_set2
#<chr>          <dbl>    <dbl>           <int>
#1 0526301100     -93.6     42.1               6
#2 0526350040     -93.6     42.1               6
#3 0526351010     -93.6     42.1               6
#4 0526353030     -93.6     42.1               6
#5 0527105010     -93.6     42.1               8
#6 0527105030     -93.6     42.1               8

ames = ames_raw


library(sp)
set1sp <- SpatialPoints(geo[2:3])
set2sp <- SpatialPoints(schools[2:3])
geo$nearest_in_set2 <- apply(gDistance(set2sp, set1sp, byid=TRUE), 1, which.min)

head(geo)
#PID        Longitude Latitude nearest_in_set2
#<chr>          <dbl>    <dbl>           <int>
#1 0526301100     -93.6     42.1               6
#2 0526350040     -93.6     42.1               6
#3 0526351010     -93.6     42.1               6
#4 0526353030     -93.6     42.1               6
#5 0527105010     -93.6     42.1               8
#6 0527105030     -93.6     42.1               8

schools$nearest_in_set2 = rownames(schools)


merged_geo = merge(geo, schools, by.x = "nearest_in_set2", by.y= "nearest_in_set2")
#nearest_in_set2         PID Longitude.x Latitude.x    School Longitude.y Latitude.y
#1                  1  0907418020   -93.68343   42.01576   Edwards   -93.68540   42.01546
#2                  1  0907203010   -93.68347   42.02181   Edwards   -93.68540   42.01546
#3                  1  0907412010   -93.68537   42.01397   Edwards   -93.68540   42.01546

merged_geo = merged_geo[c('PID','School')]
#           PID      School
# 1     0907418020   Edwards
# 2     0907203010   Edwards
# 3     0907412010   Edwards

merged_ames = merge(merged_geo, ames, by.x = "PID", by.y= "PID", all.x=TRUE)
head(merged_ames)

#unique(merged_ames['School'])
#merged_ames[merged_ames['School'] == "High",]
#anyDuplicated(merged_geo['PID'])
#anyDuplicated(ames['PID'])

merged_ames = merged_ames[c("School","Lot Frontage", "Lot Area", "Mo Sold", "Yr Sold", "SalePrice", "Total Bsmt SF")]
#     School      Lot Frontage Lot Area Mo Sold Yr Sold SalePrice Total Bsmt SF
#1    Northwood          141    31770       5    2010    215000          1080
#2    Northwood           NA    11027       5    2006    149900          1178
#3    Northwood           85    10533       8    2006    157500          1008

colnames(merged_ames) <- c("School", "LotFrontage", "LotArea", "MoSold", "YrSold", "SalePrice", "TotalBsmtSF")

write.csv(merged_ames, file = "ames_schools.csv")

table(merged_ames['School'])
# Edwards   Fellows      High    Meeker    Middle  Mitchell Northwood    Sawyer 
# 284       368       784       511       301       163       181       338 
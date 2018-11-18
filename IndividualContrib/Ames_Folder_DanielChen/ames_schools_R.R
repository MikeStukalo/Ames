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

 ################################################################################
geo = ames_geo
ames = ames_raw
ames$PID = as.numeric(ames$PID)
geo$PID = as.numeric(geo$PID)

ames_PID = data.frame(1:2930)
ames_PID$PID = ames$PID
ames_PID = ames_PID['PID']

#geo = na.omit(geo)
merged_PID = merge(ames_PID, geo, by.x = "PID", by.y= "PID", all.x= TRUE) #, all.x=TRUE)
sum(is.na(merged_PID$Longitude)) #12
#####################
geo2 = ames_geo
ames2 = ames_raw
geo2$PID = gsub(",*", "", geo2$PID) #remove commas 

ames2_PID = data.frame(1:2930)
ames2_PID$PID = ames$PID
ames2_PID = ames2_PID['PID']

merged2_PID = merge(ames2_PID, geo2, by.x = "PID", by.y= "PID", all.y= TRUE) #, all.x=TRUE)
sum(is.na(merged_PID$Longitude)) #12

geo2$PID = as.numeric(geo2$PID)
class(geo2$PID)
class(ames2_PID$PID)

####################
sum(is.na(ames$PID)) #0
sum(is.na(geo$PID)) #0

geo$PID = gsub(",*", "", geo$PID) #remove commas 

ames_PID = as.data.frame(as.numeric(ames$PID))
geo_PID = as.data.frame(as.numeric(geo$PID))

geo_PID$PID = as.data.frame(as.numeric(geo$PID))

merged_PID = merge(ames_PID, geo_PID, by.x = "as.numeric(ames$PID)", by.y= "as.numeric(geo$PID)", all.y= TRUE) #, all.x=TRUE)

merged_PID = merge(ames_PID, geo_PID, by = c("ames$PID","geo$PID"), all=TRUE) #, all.x=TRUE)

colnames(merged_PID) <- c("ames","geo123")

class(ames_PID[ames$PID,])
class(ames$PID)
class(geo$PID)
sum(is.na(merged_PID[as.numeric(ames$PID),]))

##########################################
#unable to perfect merge, 12 values do not match
geo = ames_geo
ames = ames_raw
schools = ames_schools

ames$PID = as.numeric(ames$PID)
geo$PID = as.numeric(geo$PID)
#geo = na.omit(geo)

library(sp)
spDistsN1()
schools[2:3][1,]

set1sp <- SpatialPoints(geo[2:3])
set2sp <- SpatialPoints(schools[2:3][1,])

head(spDistsN1(set1sp, set2sp, longlat = TRUE)) #distance between house and school #1


geo$EdwardsSchool = spDistsN1(set1sp, SpatialPoints(schools[2:3][1,]), longlat = TRUE)
geo$FellowsSchool = spDistsN1(set1sp, SpatialPoints(schools[2:3][2,]), longlat = TRUE)
geo$MeekerSchool = spDistsN1(set1sp, SpatialPoints(schools[2:3][3,]), longlat = TRUE)   
geo$MitchellSchool = spDistsN1(set1sp, SpatialPoints(schools[2:3][4,]), longlat = TRUE)
geo$SawyerSchool = spDistsN1(set1sp, SpatialPoints(schools[2:3][5,]), longlat = TRUE)
geo$NorthwoodSchool = spDistsN1(set1sp, SpatialPoints(schools[2:3][6,]), longlat = TRUE)
geo$MiddleSchool = spDistsN1(set1sp, SpatialPoints(schools[2:3][7,]), longlat = TRUE)
geo$HighSchool = spDistsN1(set1sp, SpatialPoints(schools[2:3][8,]), longlat = TRUE)

geo_merge = geo[-c(2,3)]

merged_ames = ames[c("PID", "Lot Frontage", "Year Built", "Lot Area", "Mo Sold", "Yr Sold", "SalePrice", "Total Bsmt SF", "1st Flr SF", "Neighborhood")]
colnames(merged_ames) <- c("PID", "LotFrontage", "YearBuilt", "LotArea", "MoSold", "YrSold", "SalePrice", "TotalBsmtSF", "1stFlrSF", "Neighborhood")

merged_PID = merge(merged_ames, geo_merge, by.x = "PID", by.y= "PID", all.x= TRUE) #merge
merged_PID = merged_PID[-c(1)]

write.csv(merged_PID, file = "ames_schoolsv2.csv")

sum(is.na(merged_PID$LotFrontage))



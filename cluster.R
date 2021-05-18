
cereals<- read.csv("/Users/Michael/Library/Mobile Documents/com~apple~CloudDocs/Download.Data/cereals.CSV")
cereals<- cereals[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,17,18,19,20,21,22,23)]
# turning characters into integers
cereals$Manuf <- as.integer(as.factor(cereals$Manuf))
cereals$Type <- as.integer(as.factor(cereals$Type))
#fixing the N/A problems
cereals$Sugars[which(is.na(cereals$Sugars))] <- mean(cereals$Sugars,na.rm = TRUE)
cereals$Carbo[which(is.na(cereals$Carbo))] <- mean(cereals$Carbo, na.rm = TRUE)
cereals$Potass[which(is.na(cereals$Potass))] <- mean(cereals$Potass, na.rm = TRUE)


cereals$Potass
# Normalize the data.
str(cereals)
normalize <- function(x) { ((x-min(x))/(max(x)-min(x)))}
cereals$Calories <-  normalize(cereals$Calories)
cereals$Protein <- normalize(cereals$Protein)
cereals$Fat <- normalize(cereals$Fat )
cereals$Sodium <- normalize(cereals$Sodium)
cereals$Fiber <- normalize(cereals$Fiber)
cereals$Carbo <- normalize(cereals$Carbo)
cereals$Sugars <- normalize(cereals$Sugars)
cereals$Potass <- normalize(cereals$Potass )
cereals$Vitamins <- normalize(cereals$Vitamins)
cereals$Shelf <- normalize(cereals$Shelf )
cereals$Weight <- normalize(cereals$Weight)
cereals$Cups <- normalize(cereals$Cups)
cereals$Cold <- normalize(cereals$Cold )
cereals$Nabisco <- normalize(cereals$Nabisco)
cereals$Quaker <- normalize(cereals$Quaker)
cereals$Kelloggs <- normalize(cereals$Kelloggs)
cereals$GeneralMills <- normalize(cereals$GeneralMills)
cereals$Ralston <- normalize(cereals$Ralston)
cereals$AHFP <- normalize(cereals$AHFP)

str(cereals)

#Using all the variables except name and rating, run the k-means algorithm with k=5 to identify clusters within data.
cereals.kmeans.5<- kmeans(cereals, 5)
?kmeans

cereals.kmeans.5

plot(cereals[c("Calories","Sugars")],col=cereals.kmeans.5$cluster)

#Develop cluster profiles that clearly describe the characteristics of the cereals within the clusters.
# i re read the chapter on this section and was stll struggling to understand the concepts.
cluster.1 <- cereals.kmeans.5$cluster[1:25]
cluster.1
cluster.2 <-  cereals.kmeans.5$cluster[26:51]
cluster.3 <- cereals.kmeans.5$cluster[52:77]
cluster.3
# cluster.1 - healthy cereals
#cluster.2 - average cereals
# cluster.3 unhealhty cereals 
#Rerun k-means with k=3. Which clustering solution do you prefer, and why?
cereals.kmeans.3<- kmeans(cereals, 3)
cereals.kmeans.3
plot(cereals[c("Calories","Sugars")],col=cereals.kmeans.3$cluster)  

# i personally prefefe having 5 clusters. This allows me to compare the data to more cereals whihc is something I prefer with the sample size given of cereals.

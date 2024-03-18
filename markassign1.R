install.packages("readxl")
install.packages("ggfortify")
library("readxl")
library("ggplot2")
library("ggfortify")


getwd()

setwd("C:/Users/kanik/Desktop/Marketing Analytics")

cus_data <- read_xlsx("Restaurant Data.xlsx")

cus_data <- cus_data[-1]

set.seed(14)

###Checking for null values
is.na(cus_data)
summary(cus_data)

##Scaling the dataset and calculating euclidean distance 
### Using complete linkage method
survey_hclust_comp <- hclust(dist(scale(cbind(cus_data$Food_Quality,cus_data$Beverages,cus_data$Location,cus_data$innovation, cus_data$Quality_of_Service, cus_data$Menu_Design,cus_data$Prioritize_Hygiene,cus_data$Interior_design, cus_data$Reasonable_Pricing,cus_data$Restaurant_Technology,cus_data$Brands,
                                         cus_data$Staff_behavior,cus_data$avg_order_size,cus_data$avg_order_freq,cus_data$Health,cus_data$Finc,
                                         cus_data$Sales, cus_data$Advt,cus_data$Edu,cus_data$Cons,cus_data$Eng,cus_data$Tech,cus_data$Retail,
                                         cus_data$SMB,cus_data$FB_Insta,cus_data$Twit,cus_data$Snap,
                                         cus_data$YouTube,cus_data$Pod_radio,cus_data$TV,cus_data$NewsP))), method="complete")

### Calling the model function
survey_hclust_comp

### creating elbow plots that determines the no. of clusters; 
##Usually the no. present at the kink of the chart is used to determine no. of clusters


x <- c(1:15)
y <- sort(survey_hclust_comp$height, decreasing = TRUE)[1:15] ## average distance between clusters
plot(x,y);lines(x,y,col = "PINK")

km_comp.out <- kmeans(x = data.frame(cus_data$Food_Quality,cus_data$Beverages,cus_data$Location,cus_data$innovation, cus_data$Quality_of_Service, cus_data$Menu_Design,cus_data$Prioritize_Hygiene,cus_data$Interior_design, cus_data$Reasonable_Pricing,cus_data$Restaurant_Technology,cus_data$Brands,
                                cus_data$Staff_behavior,cus_data$avg_order_size,cus_data$avg_order_freq,cus_data$Health,cus_data$Finc,
                                cus_data$Sales, cus_data$Advt,cus_data$Edu,cus_data$Cons,cus_data$Eng,cus_data$Tech,cus_data$Retail,
                                cus_data$SMB,cus_data$FB_Insta,cus_data$Twit,cus_data$Snap,
                                cus_data$YouTube,cus_data$Pod_radio,cus_data$TV,cus_data$NewsP), 3, nstart = 20)


km_comp.out 


comp_segments = km_comp.out$cluster
segment_comp_result <- cbind(cus_data, segments) #add new column to original dataset

##shows within cluster variation for every cluster

km_comp.out$withinss

##shows the total within cluster variations for different clusters

km_comp.out$tot.withinss


#########################################################################

### Using single linkage method
survey_hclust_single <- hclust(dist(scale(cbind(cus_data$Food_Quality,cus_data$Beverages,cus_data$Location,cus_data$innovation, cus_data$Quality_of_Service, cus_data$Menu_Design,cus_data$Prioritize_Hygiene,cus_data$Interior_design, cus_data$Reasonable_Pricing,cus_data$Restaurant_Technology,cus_data$Brands,
                                              cus_data$Staff_behavior,cus_data$avg_order_size,cus_data$avg_order_freq,cus_data$Health,cus_data$Finc,
                                              cus_data$Sales, cus_data$Advt,cus_data$Edu,cus_data$Cons,cus_data$Eng,cus_data$Tech,cus_data$Retail,
                                              cus_data$SMB,cus_data$FB_Insta,cus_data$Twit,cus_data$Snap,
                                              cus_data$YouTube,cus_data$Pod_radio,cus_data$TV,cus_data$NewsP))), method="single")

### Calling the model function
survey_hclust_single

### creating elbow plots that determines the no. of clusters; 
##Usually the no. present at the kink of the chart is used to determine no. of clusters


x <- c(1:15)
y <- sort(survey_hclust_single$height, decreasing = TRUE)[1:15] ## average distance between clusters
plot(x,y);lines(x,y,col = "ORANGE")

km_single.out <- kmeans(x = data.frame(cus_data$Food_Quality,cus_data$Beverages,cus_data$Location,cus_data$innovation, cus_data$Quality_of_Service, cus_data$Menu_Design,cus_data$Prioritize_Hygiene,cus_data$Interior_design, cus_data$Reasonable_Pricing,cus_data$Restaurant_Technology,cus_data$Brands,
                                     cus_data$Staff_behavior,cus_data$avg_order_size,cus_data$avg_order_freq,cus_data$Health,cus_data$Finc,
                                     cus_data$Sales, cus_data$Advt,cus_data$Edu,cus_data$Cons,cus_data$Eng,cus_data$Tech,cus_data$Retail,
                                     cus_data$SMB,cus_data$FB_Insta,cus_data$Twit,cus_data$Snap,
                                     cus_data$YouTube,cus_data$Pod_radio,cus_data$TV,cus_data$NewsP), 6, nstart = 20)


### Calling the model function
km_single.out 



single_segments = km_single.out$cluster
segment_single_result <- cbind(cus_data, single_segments) #add new column to original dataset

##shows within cluster variation for every cluster

km_single.out$withinss

##shows the total within cluster variations for different clusters

km_single.out$tot.withinss



### Using average linkage method
survey_hclust_avg <- hclust(dist(scale(cbind(cus_data$Food_Quality,cus_data$Beverages,cus_data$Location,cus_data$innovation, cus_data$Quality_of_Service, cus_data$Menu_Design,cus_data$Prioritize_Hygiene,cus_data$Interior_design, cus_data$Reasonable_Pricing,cus_data$Restaurant_Technology,cus_data$Brands,
                                                cus_data$Staff_behavior,cus_data$avg_order_size,cus_data$avg_order_freq,cus_data$Health,cus_data$Finc,
                                                cus_data$Sales, cus_data$Advt,cus_data$Edu,cus_data$Cons,cus_data$Eng,cus_data$Tech,cus_data$Retail,
                                                cus_data$SMB,cus_data$FB_Insta,cus_data$Twit,cus_data$Snap,
                                                cus_data$YouTube,cus_data$Pod_radio,cus_data$TV,cus_data$NewsP))), method="average")

### Calling the model function
survey_hclust_avg

### creating elbow plots that determines the no. of clusters; 
##Usually the no. present at the kink of the chart is used to determine no. of clusters

x <- c(1:15)
y <- sort(survey_hclust_avg$height, decreasing = TRUE)[1:15] ## average distance between clusters
plot(x,y);lines(x,y,col = "BLUE")

km_avg.out <- kmeans(x = data.frame(cus_data$Food_Quality,cus_data$Beverages,cus_data$Location,cus_data$innovation, cus_data$Quality_of_Service, cus_data$Menu_Design,cus_data$Prioritize_Hygiene,cus_data$Interior_design, cus_data$Reasonable_Pricing,cus_data$Restaurant_Technology,cus_data$Brands,
                                       cus_data$Staff_behavior,cus_data$avg_order_size,cus_data$avg_order_freq,cus_data$Health,cus_data$Finc,
                                       cus_data$Sales, cus_data$Advt,cus_data$Edu,cus_data$Cons,cus_data$Eng,cus_data$Tech,cus_data$Retail,
                                       cus_data$SMB,cus_data$FB_Insta,cus_data$Twit,cus_data$Snap,
                                       cus_data$YouTube,cus_data$Pod_radio,cus_data$TV,cus_data$NewsP), 3, nstart = 20)

### Calling the model function
km_avg.out 



avg_segments = km_avg.out$cluster
segment_avg_result <- cbind(cus_data, avg_segments) #add new column to original dataset

##shows within cluster variation for every cluster

km_avg.out$withinss

##shows the total within cluster variations for different clusters

km_avg.out$tot.withinss


### Using centroid linkage method
survey_hclust_centr <- hclust(dist(scale(cbind(cus_data$Food_Quality,cus_data$Beverages,cus_data$Location,cus_data$innovation, cus_data$Quality_of_Service, cus_data$Menu_Design,cus_data$Prioritize_Hygiene,cus_data$Interior_design, cus_data$Reasonable_Pricing,cus_data$Restaurant_Technology,cus_data$Brands,
                                             cus_data$Staff_behavior,cus_data$avg_order_size,cus_data$avg_order_freq,cus_data$Health,cus_data$Finc,
                                             cus_data$Sales, cus_data$Advt,cus_data$Edu,cus_data$Cons,cus_data$Eng,cus_data$Tech,cus_data$Retail,
                                             cus_data$SMB,cus_data$FB_Insta,cus_data$Twit,cus_data$Snap,
                                             cus_data$YouTube,cus_data$Pod_radio,cus_data$TV,cus_data$NewsP))), method="centroid")

### Calling the model function
survey_hclust_centr

### creating elbow plots that determines the no. of clusters; 
##Usually the no. present at the kink of the chart is used to determine no. of clusters

x <- c(1:15)
y <- sort(survey_hclust_centr$height, decreasing = TRUE)[1:15] ## average distance between clusters
plot(x,y);lines(x,y,col = "RED")

##Performing kmeans clustering 
km_centr.out <- kmeans(x = data.frame(cus_data$Food_Quality,cus_data$Beverages,cus_data$Location,cus_data$innovation, cus_data$Quality_of_Service, cus_data$Menu_Design,cus_data$Prioritize_Hygiene,cus_data$Interior_design, cus_data$Reasonable_Pricing,cus_data$Restaurant_Technology,cus_data$Brands,
                                    cus_data$Staff_behavior,cus_data$avg_order_size,cus_data$avg_order_freq,cus_data$Health,cus_data$Finc,
                                    cus_data$Sales, cus_data$Advt,cus_data$Edu,cus_data$Cons,cus_data$Eng,cus_data$Tech,cus_data$Retail,
                                    cus_data$SMB,cus_data$FB_Insta,cus_data$Twit,cus_data$Snap,
                                    cus_data$YouTube,cus_data$Pod_radio,cus_data$TV,cus_data$NewsP), 7, nstart = 20)

### Calling the model function
km_centr.out



centr_segments = km_centr.out$cluster
segment_centr_result <- cbind(cus_data, centr_segments) 

##shows within cluster variation for every cluster

km_centr.out$withinss

##shows the total within cluster variations for different clusters

km_centr.out$tot.withinss



##export excel file

write.csv(segment_centr_result, file = "C:/Users/kanik/Desktop/Marketing Analytics/centr_result.csv", row.names = FALSE)


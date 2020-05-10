##########################################################################################################################################################################################################################
# Title         : An Evaluation of Sentinel 1 and Sentinel 2 for Land Cover Classification using Machine Learning in the area of Antwerp, Belgium 
#
# Purpose       : This Script is written as a final project for the Programming and Geostatistical Analysis course http://eagle-science.org/project/programming-and-geostatistical-analysis/
#                 which is one of the courses that I took during my MSc. in Applied Earth Observation and Geoanalysis at JMU Wurzburg https://www.uni-wuerzburg.de/en/home/              
#
# Author        : Walid Ghariani (linkedin: https://www.linkedin.com/in/walid-ghariani-893365138/) (E-mail: walid.ghariani@stud-mail.uni-wuerzburg.de) 
#
# Preprocessing : Sentinel 1 product S1B_IW_GRDH_1SDV_20190420 was downloded and the following steps were conducted using the software SNAP: orbit correction, thermal noise removal, radiometric calibration, 
#                 speckle reduction with Lee Sigma on a window size (7 by 7) and finally, a range-Doppler terrain correction was applied, using the UTM zone 31 North WGS84 projection and the 30m SRTM, where 10-m 
#                 resampling was made to fit the integration requirements for Sentinel2; Both VV and VH bands were converted to a logarithmic scale
#                 Sentinel 2 product S2B_MSIL2A_20190421 was dowloded and a bilinear interpolation-based re-sampling to 10??10 m spatial resolution was performed using SNAP to achieve a common resolution for all bands. 
#
# Input         : Sentinel 1 data (VH and VV bands) and Sentinel 2 data (All 12 bands)
# Output        : Six land cover classification maps were generated according to the data integration method
#
# Reference		  : Wegmann M, Leutner B, Dech S (2016) Remote sensing and GIS for ecologists: using open source software. Pelagic Publishing, Exeter, UK.
#				          Kamusoko, C. (2019). Remote Sensing Image Classification in R. Springer.
#                 Robert J. Hijmans,  (2016-2020). Spatial Data Science with R
# Reference URL	: http://book.ecosens.org/RSEbook/ https://www.springer.com/gp/book/9789811380112 https://rspatial.org/#
##########################################################################################################################################################################################################################
#
#################### Install missing R packages if needed
list.of.packages <- c("sp","rgdal","raster","ggplot2","gridExtra","RStoolbox","rasterVis","RColorBrewer",
                      "ggspatial","glcm","caret","rasterVis", "scales","dplyr","tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) {
  print("installing : ")
  print(new.packages)
  install.packages(new.packages, repos = "http://cran.rstudio.com/", dependencies = TRUE)
}
#################### Set your working directory
# Make sure you have downloaded the folders containing the data of Sentinel 1, Sentinel 2, the shp. file for the training samples, and PA_UA.csv containing the producer's and user's accurracies
# Then Set your working directory according to your chosen path
setwd("D:/MB3/MB2_Script/S2_Data")
#
library(sp)
library(rgdal)
library(raster)
library(ggplot2)
library(gridExtra)
library(RStoolbox)
library(rasterVis)
library(RColorBrewer)
getwd()
#################### Land cover classification using Sentinel 2
#
# first create a list of raster layers/bands that will be imported.
S2 <- paste0('B', 1:12, ".tif")
S2
# Stack the raster layers
S2_Antwerp<- stack(S2)
## Check the properties of the RasterStack
S2_Antwerp

# Check the attributes and Image properties
## CRS: coordinate reference system (CRS)
crs(S2_Antwerp)
## Extent
extent(S2_Antwerp)
## spatial resolution
res(S2_Antwerp)
## Number of layers
nlayers(S2_Antwerp)
## Check the names of bands
names(S2_Antwerp)
## Change the names of bands accordingly 
names(S2_Antwerp)<- c("B1","B2", "B3", "B4", "B5", "B6",
                      "B7","B8", "B8A", "B9", "B11", "B12")
names(S2_Antwerp)

## Lets get the refelectance values of S2 bands by dividing by 10000 
S2_Antwerp<-S2_Antwerp/10000
S2_Antwerp

## visualise spectral bands
plot(S2_Antwerp)

gplot(S2_Antwerp)+
  geom_raster(aes(x=x, y=y, fill=value))+
  scale_color_viridis_c()+
  facet_wrap(~variable)+
  coord_equal()+
  theme_classic()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Spectral Bands of Sentinel 2",
       x="Longitude", y="Latitude")
gplot(S2_Antwerp)+
  geom_raster(aes(x=x, y=y, fill=value))+
  scale_fill_viridis_c()+
  facet_wrap(~variable)+
  coord_equal()+
  theme_classic()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Spectral Bands of Sentinel 2",
       x="Longitude", y="Latitude")  

# Display the composite RGB Senrinel 2
## Select the NIR:B4, red:B3, Blue:B2
S2_AntwerpRGB<- S2_Antwerp[[c(4,3,2)]]
plotRGB(S2_AntwerpRGB, stretch="lin", main="Sentinel 2 RGB", axes=TRUE)

ggRGB(S2_AntwerpRGB,1,2,3, stretch = "lin")+
  labs(title = "Sentinel 2 RGB", x="Longitude", y="Latitude")+
  theme(plot.title = element_text(hjust = 0.5))

ggRGB(S2_AntwerpRGB, 1,2,3, stretch = "lin")+   
  theme_classic()+
  theme(text = element_text(size = 14))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Sentinel 2 RGB")

# Load the training polygons samples
setwd("D:/MB3/MB2_Script/Training_Data")
td1<- readOGR(getwd(), "TRNG_S2")
td1
##Check the structure of the training
str(td1)
## PLot the traning samples
plot(td1) 
# Check the LC calsess and their IDs
td1@data

# Create a vector of unique LC attributs values
un1<- unique(td1$class)
un1
# Generate 400 random points from the training polygons samples
S2_seed<- (40)
set.seed(S2_seed)
for (i in 1:length(un1)){
  class_data1<- subset(td1, class == un1[i])
  classpts1 <- spsample(class_data1, type = "random", n=400)
  classpts1$class<- rep(un1[i], length(classpts1))
  if (i == 1){
    xy<- classpts1
  } else{
    xy<- rbind(xy, classpts1)
  }
}
points(xy)

# Extract the reflectance values from S2_Antwerp using generated training points (xy).
tdt<- raster::extract(S2_Antwerp, y= xy, cellnumbers=TRUE)
tdt
tdt<- data.frame(response=xy$class, tdt)
tdt
head(tdt[, 1:14])# check the head of the first columns

# Check if there are duplicates 
any(duplicated(tdt$cells))
tdt<- tdt[!duplicated(tdt), -2] # Get rid of the cell column (2 column)
tdt

names(tdt) 
table(tdt$response) # Check how many points we have for each LC class

library(dplyr)
library(tidyverse)
# Check the data set
tdt %>% 
  view()
# Visualize the Specral profile of Sentinel 2 
Spec_Prof<-tdt %>% 
  group_by(response) %>%
  summarize_at(vars(B1:B12), mean) %>%
  gather(metric, value, -response) %>%
  mutate(metric = factor(metric,levels = names(S2_Antwerp))) %>%
  rename("Land_Cover_Class" = response) %>% 
  ggplot(aes(metric, value,  col=Land_Cover_Class))+
  geom_line(aes(linetype=Land_Cover_Class, group=Land_Cover_Class))+
  geom_point()+
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        legend.title=element_text(size=12),
        legend.text=element_text(size=10))+
  labs(title = "Spectral Profile of Sentinel 2",
       subtitle = ("(Using the trainig data)"),
       x="Bands", y="Reflectance")

Spec_Prof

library(caret)
library(rasterVis)
# Prepare the training and testing data sets
## Here we split the data frame into 70% (Training), 30%(Validation) by class
set.seed(S2_seed)
inTraining_1 <- createDataPartition(tdt$response,
                                    p = .70, list = FALSE)
inTraining_1
head(inTraining_1)

# training_1 will be used as training data for optimal model parameters
training_1 <- tdt[inTraining_1,]
training_1
str(training_1) # > 1947 obs. of  13 variables
head(training_1)
summary(training_1)
training_1

# Using the training data (70%), lets examine the LC classes response to S2 reflectance values
training_1 %>% 
  gather(metric, value, -response) %>%
  ggplot(aes(response, value))+
  geom_boxplot()+
  facet_wrap(~metric)+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme(axis.line.y = element_blank(),
        plot.title = element_text(size=18, lineheight = 0.9))+#Get rid of the grid
  labs(title = "Land Cover Clases Reponse to S2 Reflectance",
       subtitle = ("(Using the trainig data)"),
       x="Land Cover Classes", y="Reflectance")

# testing_1 will be used for final accuracy assessment
testing_1 <- tdt[-inTraining_1,]
testing_1
str(testing_1) #829 obs. of  13 variables:
summary(testing_1)

# Set-up model tuning parameters
set.seed(S2_seed)
fitControl<-trainControl(method = "repeatedcv", # repeated cross-validation for training the data
                         number = 10, #Number of folds
                         repeats = 5) # NUmber of repeats
fitControl


#### Lets Train the Random Forest RF classifier for all bands in S2
## RF
set.seed(S2_seed) 
rf_S2<-train(response~., #"response"is the response/target variable & (response~.) is formula for using all attributes in our classifier,
             data=training_1, # training data 
             method="rf", # Random Forest classifier
             trControl=fitControl, # the tune control (trControl) is the ???fitControl??? 
             prox=TRUE,
             fitBest = FALSE,
             returnData = TRUE)
# check the RF model performance.
rf_S2
plot(rf_S2)

# Check the parameters of the best model
rf_S2$finalModel #-> The best model had an mtry value of 2 with anoverall accuracy of 98.26%
plot(rf_S2$finalModel, main="RF Final Model on S2")

# Display variables/bands importance using varImp()
rf_S2_vimp<-varImp(rf_S2,compete= FALSE)
rf_S2_vimp

# Visualize the bands importances
plot(rf_S2_vimp)

ggplot(rf_S2_vimp)+
  ggtitle("Bands imporatnce: RF on S2")+
  xlab("Bands") + ylab("Importance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line.y = element_blank(),
        legend.position = 0,            #get rid of the legend
        panel.grid = element_blank())  #Get rid of the grid


# Perform the prediction using the predict() function.
rf_S2_prd<- predict(rf_S2, newdata= testing_1) # newdata holds the testing data
rf_S2_prd
# Perform the Accuracy Assessemnt 
# we build a confusion matrix using the testing_1$response
confusionMatrix(rf_S2_prd, testing_1$response)


#################### Land cover classification using Sentinel 1
# 
# We will be working only with VV and VH VH Backscatter bands
# Set up the working directory for Sentinel 1 data
setwd("D:/MB3/MB2_Script/S1_Data") 
# first create a list of raster layers/bands that will be imported.
sar <- paste0('V', 1:2, ".tif")
sar
# Stack the raster layers
S1<- stack(sar)

res(S1) # Check the spatial resolution
nlayers(S1) # Number of layers
names(S1) # Check the names of S1 bands & Change the names of bands accordingly 
names(S1)<- c("VH_db", "VV_db")

#Extent
extent(S1) 
extent(S2_Antwerp)

# S1 bands need to be modified according to the extent of S2 
vh_db <- raster(vals=values(S1$VH_db),ext=extent(S2_Antwerp$B2),crs=crs(S2_Antwerp$B2),
                nrows=dim(S1$VH_db)[1],ncols=dim(S1$VH_db)[2])
vh_db
# Plot the VH band
plot(vh_db, main="Sentinel 1 VH Backscatter \nIntensity (db)")

vv_db <- raster(vals=values(S1$VV_db),ext=extent(S2_Antwerp$B2),crs=crs(S2_Antwerp$B2),
                nrows=dim(S1$VV_db)[1],ncols=dim(S1$VV_db)[2])
vv_db
# Plot the VV band
plot(vv_db, main="Sentinel 1 VV Backscatter \nIntensity (db)")

# Now we can stack the S1 bands with the correct extent 
S1_Antwerp<- stack(vh_db, vv_db)
S1_Antwerp

# change the names of the bands accordingly
names(S1_Antwerp)<- c("VH_db", "VV_db")

# Visualize S1 VH and VV bands 
plot(S1_Antwerp)

library(ggspatial)
gplot(S1_Antwerp)+
  geom_raster(aes(x=x, y=y, fill=value))+
  scale_fill_viridis_c()+
  facet_wrap(~variable)+
  coord_equal()+
  theme_classic()+
  theme(text = element_text(size = 14),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(angle = 90, hjust = 1, size=8))+
  theme(plot.title = element_text(hjust = 0.5))+
  annotation_scale(location = "bl", width_hint = 0.3, height = unit(0.1, "cm"),
                   pad_x = unit(0.4, "cm"), pad_y = unit(0.4, "cm"))+
  xlab("")+ylab("")

# Extract the backscatter values from S1_Antwerp using the generated training points (xy).
tdt2<- raster::extract(S1_Antwerp, y= xy, cellnumbers=TRUE)
tdt2

tdt2<- data.frame(response=xy$class, tdt2)
tdt2
head(tdt2[, 1:4],6)# check the head of the first columns

# Check if there are duplicates 
any(duplicated(tdt2$cells))

tdt2<- tdt2[!duplicated(tdt2), -2] # # Get rid of the cell column (2 column)

names(tdt2)
table(tdt2$response) # Check how many points we have for each LC class
tdt2

# Again we Prepare the training and testing data sets the same way as we did before
set.seed(S2_seed)
inTraining_2 <- createDataPartition(tdt2$response,
                                    p = .70, list = FALSE)
inTraining_2
head(inTraining_2)

# training_2 will be used as training data for optimal model parameters
training_2 <- tdt2[inTraining_2,]
training_2
#Check the proprities of the training_2 data
str(training_2) 
head(training_2)
summary(training_2)

# Using the training data (70%), Lets examine the LC classes response to S1 backscatter intensity
training_2 %>% 
  gather(metric, value, -response) %>%
  ggplot(aes(response, value))+
  geom_boxplot()+
  facet_wrap(~metric, ncol=2)+
  theme(axis.text.x = element_text(angle=65, vjust=0.6))+
  theme(text = element_text(size = 12),
        plot.title = element_text(size=18, lineheight = 0.9))+#Get rid of the grid
  labs(title = "Land Cover Classes Reponse to S1  Backscatter Intensity (db)",
       subtitle = ("(Using the trainig data)"),
       x="Land Cover Classes", y="Backscatter Intensity (db)")

# testing_2 will be used for final accuracy assessment
testing_2 <- tdt2[-inTraining_2,]
testing_2
#Check the proprities of the testing_2 data
str(testing_2) 
head(testing_2)
summary(testing_2)

#### Lets Train the Random Forest RF classifier for S1 bands(VH & VV)
## RF on S1
set.seed(S2_seed) 
rf_S1<-train(response~.,data=training_2,
             method="rf",
             trControl=fitControl,
             prox=TRUE,
             fitBest = FALSE,
             returnData = TRUE)
rf_S1

# Check the parameters of the best model
rf_S1$finalModel 
plot(rf_S1$finalModel, main= "RF Final Model on S1")

# Display S1 variables/bands importance using varImp()
rf_S1_vimp<-varImp(rf_S1,compete= FALSE)
rf_S1_vimp

# Visualize S1 bands importance
plot(rf_S1_vimp)
ggplot(rf_S1_vimp)+
  ggtitle("Bands imporatnce: RF on S1")+
  xlab("Bands") + ylab("Importance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line.y = element_blank(),
        legend.position = 0,            #get rid of the legend
        panel.grid = element_blank())  #Get rid of the grid

# Perform the prediction on S1 bands
rf_S1_prd<- predict(rf_S1, newdata= testing_2)
rf_S1_prd
# Perform the Accuracy Assessemnt 
# we build a confusion matrix using the testing_2$response
confusionMatrix(rf_S1_prd, testing_2$response)


#################### Land cover classification using Sentinel 1 & GLCM bands 
#
# In this section we will generate Grey Level Co-occurrence Matrix (GLCM)
# indices : the mean, variance, homogeneity and entropy from S1 based on a 7 by 7 moving window
# library(glcm): # designed to compute image textures derived from (GLCMs)
#
# Compute GLCM for S1 VH band
library(glcm)
VH_db_GLCM<- glcm(raster(S1_Antwerp, layer= 1),
                  window = c(7,7), statistics = c("mean", "variance", "homogeneity", "entropy"))
VH_db_GLCM
# Change the VH GLCM bands names accordingly
names(VH_db_GLCM)
names(VH_db_GLCM)<- c("VH_db_GLCM_mean", "VH_db_GLCM_variance", "VH_db_GLCM_homogeneity", "VH_db_GLCM_entropy")
# Visualize VV_db_GLCM
plot(VH_db_GLCM)

# Compute GLCM for S1 VV band
VV_db_GLCM<- glcm(raster(S1_Antwerp, layer=2),
                  window = c(7,7), statistics = c("mean", "variance", "homogeneity", "entropy"))
VV_db_GLCM
# Check VV GLCM bands names
names(VV_db_GLCM)
# Change the VV GLCM bands names accordingly
names(VV_db_GLCM)<- c("VV_db_GLCM_mean", "VV_db_GLCM_variance", "VV_db_GLCM_homogeneity", "VV_db_GLCM_entropy")
names(VV_db_GLCM)
# Visualize VV_db_GLCM
plot(VV_db_GLCM)

# Lets stack S1 and GLCM data
S1_GLCM_Antwerp<- stack(S1_Antwerp, VH_db_GLCM, VV_db_GLCM)
S1_GLCM_Antwerp
names(S1_GLCM_Antwerp)

# Extract the backscatter values from S1_GLCM_Antwerp using the generated training points (xy).
tdt3<- raster::extract(S1_GLCM_Antwerp, y= xy, cellnumbers=TRUE)
tdt3
tdt3<- data.frame(response=xy$class, tdt3)
tdt3

# Check if there are duplicates 
any(duplicated(tdt3$cells))

tdt3<- tdt3[!duplicated(tdt3), -2] # Get rid of the cell column (2 column)

names(tdt3)
table(tdt3$response) # Check how many points we have for each LC class
tdt3

# Prepare the training and testing data sets
set.seed(S2_seed)
inTraining_3 <- createDataPartition(tdt3$response,
                                    p = .70, list = FALSE)
inTraining_3
head(inTraining_3)

# training_3 will be used as training data for optimal model parameters
training_3 <- tdt3[inTraining_3,]
training_3
#Check the proprities of the training_3 data
str(training_3) 
head(training_3)
summary(training_3)

# testing_3 will be used for final accuracy assessment
testing_3 <- tdt3[-inTraining_3,]
testing_3
#Check the proprities of the testing_3 data
str(testing_3) 
head(testing_3)
summary(testing_3)

#### Lets Train the RF classifier for S1 bands (VH & VV) + GLCM bands
## RF on S1
set.seed(S2_seed) 
rf_S1_GLCM<-train(response~.,data=training_3,
                  method="rf",
                  trControl=fitControl,
                  prox=TRUE,
                  fitBest = FALSE,
                  returnData = TRUE)
rf_S1_GLCM

# Check the parameters of the best model
rf_S1_GLCM$finalModel
plot(rf_S1_GLCM$finalModel, main= "RF Final Model for S1 + GLCM")

# Display S1 & GLCM variables/bands importance using varImp()
rf_S1_GLCM_vimp<-varImp(rf_S1_GLCM,compete= FALSE)
plot(rf_S1_GLCM_vimp)
ggplot(rf_S1_GLCM_vimp)+
  ggtitle("Bands imporatnce: RF on S1+GLCM")+
  xlab("Bands") + ylab("Importance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line.y = element_blank(),
        legend.position = 0,            #get rid of the legend
        panel.grid = element_blank())  #Get rid of the grid

# Perform the prediction on S1 & GLCM bands
rf_S1_GLCM_prd<- predict(rf_S1_GLCM, newdata= testing_3)
rf_S1_GLCM_prd
# Perform the Accuracy Assessemnt 
# we build a confusion matrix using the testing_3$response
confusionMatrix(rf_S1_GLCM_prd, testing_3$response)


#################### Land cover classification using Sentinel 1 and Sentinel 2
#
# We will using Sentinel 1 bands (VH & VV) and Sentinel 2 bands (12 bands)
# First stack S1 and S2 data
S1_S2_Antwerp<-stack(S2_Antwerp, S1_Antwerp)
S1_S2_Antwerp
#Check the number of bands used in this data fuison
nlayers(S1_S2_Antwerp)
# Check S1_S2_Antwerp bands names 
names(S1_S2_Antwerp)

# Extract the backscatter values from S1_S2_Antwerp using the generated training points (xy).
tdt4<- raster::extract(S1_S2_Antwerp, y= xy, cellnumbers=TRUE)
tdt4

tdt4<- data.frame(response=xy$class, tdt4)
tdt4
head(tdt4[, 1:16], 6) # check the head of the first columns

any(duplicated(tdt4$cells))

tdt4<- tdt4[!duplicated(tdt4), -2]

names(tdt4)
table(tdt4$response) # Check how many points we have for each LC class

# Prepare the training and testing data sets
set.seed(S2_seed)
inTraining_4 <- createDataPartition(tdt4$response,
                                    p = .70, list = FALSE)
inTraining_4
training_4 <- tdt4[inTraining_4,]
training_4
#Check the proprities of the training_4 data
str(training_4) 
summary(training_4)
head(training_4)

testing_4 <- tdt4[-inTraining_4,]
testing_4
#Check the proprities of the testing_4 data
str(testing_4) 
summary(testing_4)
head(testing_4)

#### Lets Train Random Forest RF classifier on S1 + S2 data
##
set.seed(S2_seed) 
rf_S1_S2<-train(response~.,data=training_4,
                method="rf",
                trControl=fitControl,
                prox=TRUE,
                fitBest = FALSE,
                returnData = TRUE)
# check the RF model performance
rf_S1_S2
plot(rf_S1_S2)
# Check the parameters of the best model
rf_S1_S2$finalModel
plot(rf_S1_S2$finalModel, main="RF Final Model for S1 + S2")

# Display variables/bands importance using varImp()
rf_S1_S2_vimp<-varImp(rf_S1_S2,compete= FALSE)
rf_S1_S2_vimp
# Visualize the bands importances
plot(rf_S1_S2_vimp)
ggplot(rf_S1_S2_vimp)+
  ggtitle("(e)  Bands imporatnce: RF on S1+S2")+
  xlab("Bands") + ylab("Importance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line.y = element_blank(),
        legend.position = 0,            #get rid of the legend
        panel.grid = element_blank())  #Get rid of the grid

# Perform the prediction on S1 + S2 bands 
rf_S1_S2_prd<- predict(rf_S1_S2, newdata= testing_4)
rf_S1_S2_prd
# Perform the Accuracy Assessemnt 
# we build a confusion matrix using the testing_4$response
confusionMatrix(rf_S1_S2_prd, testing_4$response)

#################### Land cover classification using Sentinel 2 and VI (Vegetation Indices)
#
# We will be using Sentinel-2 bands(12 bands) + the VI bands
# First lets generate the VI (NDVI, SAVI, MSAVI, NDWI2) using the fucntion  spectralIndices() and calling the required bands
# Documenation about computing VI can be found: https://www.rdocumentation.org/packages/RStoolbox/versions/0.2.6/topics/spectralIndices

s2_vi<- spectralIndices(S2_Antwerp,
                        red="B4",
                        nir="B8A",
                        swir2 = "B11",
                        indices = c("NDVI", "SAVI", "MSAVI2","NDWI2"))

# Visualize all the VI 
plot(s2_vi)
# Build a customized visualization for the VI bands
gplot(s2_vi)+
  geom_raster(aes(x=x, y=y, fill=value))+
  scale_fill_viridis_c()+
  facet_wrap(~variable)+
  coord_equal()+
  theme_classic()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Vegetation Indices generated from Sentinel 2",
       x="Longitude", y="Latitude")

# Check the names of s2_vi bands names
names(s2_vi)
# Stack the data of S2 and VI 
S2_VI_Antwerp<- stack(S2_Antwerp, s2_vi)
S2_VI_Antwerp

#Check the number of bands used in this data fuison
nlayers(S2_VI_Antwerp) # > 16 bands 

# Check the names of S2_VI_Antwerp bands names
names(S2_VI_Antwerp)

# Extract the reflectance values from SS2_VI_Antwerp using the generated training points (xy).
tdt5<- raster::extract(S2_VI_Antwerp, y= xy, cellnumbers=TRUE)
tdt5
tdt5<- data.frame(response=xy$class, tdt5)
tdt5
head(tdt5[, 1:18], 7)

any(duplicated(tdt5$cells))

tdt5<- tdt5[!duplicated(tdt5), -2]

names(tdt5)
table(tdt5$response) # Check how many points we have for each LC class

# Prepare training and test data sets
set.seed(S2_seed)
inTraining_5 <- createDataPartition(tdt5$response,
                                    p = .70, list = FALSE)
inTraining_5

# Prepare the training and testing data sets
training_5 <- tdt5[inTraining_5,]
training_5
# Check the proprities of the training_5 data
str(training_5)
summary(training_5)
head(training_5)

testing_5 <- tdt5[-inTraining_5,]
testing_5
# Check the proprities of the testing_5 data
str(testing_5)
summary(testing_5)
head(testing_5)

#### Lets Train Random Forest RF classifier on S2 & VI data
##
set.seed(S2_seed) 
rf_S2_VI<-train(response~.,data=training_5,
                method="rf",
                trControl=fitControl,
                prox=TRUE,
                fitBest = FALSE,
                returnData = TRUE)
rf_S2_VI
plot(rf_S2_VI)
# Check the parameters of the best model
rf_S2_VI$finalModel
plot(rf_S2_VI$finalModel, main="RF Final Model on S2 + GLCM")

# Display S2+VI bands importance using varImp()
rf_S2_VI_vimp<-varImp(rf_S2_VI,compete= FALSE)
rf_S2_VI_vimp

# Visualize S2+VI bands importance
plot(rf_S2_VI_vimp)
ggplot(rf_S2_VI_vimp)+
  ggtitle("Bands imporatnce: RF on S2+VI")+
  xlab("Bands") + ylab("Importance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line.y = element_blank(),
        legend.position = 0,            #get rid of the legend
        panel.grid = element_blank())  #Get rid of the grid

# Perform the prediction on S2+VI bands
rf_S2_VI_prd<- predict(rf_S2_VI, newdata= testing_5)
rf_S2_VI_prd
# Perform the Accuracy Assessemnt 
# we build a confusion matrix using the testing_6$response
confusionMatrix(rf_S2_VI_prd, testing_5$response)


#################### Land cover classification using all products (S1 + GLCM + S2 + VI)
#
#
ALL_Antwerp<- stack(S2_VI_Antwerp, S1_GLCM_Antwerp)
ALL_Antwerp
# Check the numbers of the bands used in this data  fusion
nlayers(ALL_Antwerp) # > 26 bands 
# Check the names
names(ALL_Antwerp)

# Extract the bands values from ALL_Antwerp using the generated training points (xy).
tdt6<- raster::extract(ALL_Antwerp, y= xy, cellnumbers=TRUE)
tdt6

tdt6<- data.frame(response=xy$class, tdt6)
tdt6
head(tdt6[, 1:28], 7)

any(duplicated(tdt6$cells))

tdt6<- tdt6[!duplicated(tdt6), -2]

names(tdt6)
table(tdt6$response) # Check how many points we have for each LC class

# Prepare training and test data sets
set.seed(S2_seed)
inTraining_6 <- createDataPartition(tdt6$response,
                                    p = .70, list = FALSE)
inTraining_6

# Prepare the training and testing data sets
training_6 <- tdt6[inTraining_6,]
training_6
#Check the proprities of the training_6 data
str(training_6)
summary(training_6)
head(training_6)

testing_6 <- tdt6[-inTraining_6,]
testing_6
#Check the proprities of the training_6 data
str(testing_6)
summary(testing_6)
head(testing_6)

#### Lets Train Random Forest RF classifier on ALL the data combined (S1 + GLCM + S2 + VI)
##
set.seed(S2_seed) 
rf_ALL<-train(response~.,data=training_6,
              method="rf",
              trControl=fitControl,
              prox=TRUE,
              fitBest = FALSE,
              returnData = TRUE)
rf_ALL
plot(rf_ALL)
# Check the parameters of the best model
rf_ALL$finalModel
plot(rf_ALL$finalModel, main="RF Final Model on All Data\n(S1+GLCM+S2+V2)")

# Display variables/bands importance 
rf_ALL_vimp<-varImp(rf_ALL,compete= FALSE)
rf_ALL_vimp

# Visualize the bands importances
plot(rf_ALL_vimp)
ggplot(rf_ALL_vimp)+
  ggtitle("Bands imporatnce: RF on S1+GLCM+S2+VI")+
  xlab("Bands") + ylab("Importance")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(axis.line.y = element_blank(),
        legend.position = 0,            #get rid of the legend
        panel.grid = element_blank())  #Get rid of the grid

# Perform the prediction on All the integrated data  
rf_ALL_prd<- predict(rf_ALL, newdata= testing_6)
rf_ALL_prd
# Perform the Accuracy Assessemnt 
# we build a confusion matrix using the testing_4$response
confusionMatrix(rf_ALL_prd, testing_6$response)

#################### Examine the Producer's and User's Accuracies
#
# The following csv file contains all the relevant data that were generated from all the confuxion 
# matrices of the different RF models 
PA_UA<-read.csv("D:/MB3/MB2_Script/PA_UA/PA_UA.csv")
# Lets visualize the User's Accuracy : UA
PA_UA %>% 
  ggplot(aes(x=class, y=user_accuracy, fill=Method))+
  geom_col()+
  facet_wrap(vars(Method))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("UA for each LC class using RF") +
  xlab("Class") + ylab("User Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))

# Now lets visualize the Producer's Accuracy : PA
PA_UA %>% 
  ggplot(aes(x=class, y=producer_accuracy, fill=Method))+
  geom_col()+
  facet_wrap(vars(Method))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("PA for each LC class using RF") +
  xlab("Class") + ylab("Producer Accuracy")+
  theme(plot.title = element_text(hjust = 0.5))

####################  Lets Compare all the generated RF models
#
# Since all the RF models share a common set of resampled data sets, we use a resamples() function to compare them
# This is a comparison of the models training performance only!!
models_evaluation <- resamples(list("S1" = rf_S1,
                                    "S1+GLCM" = rf_S1_GLCM,
                                    "S2" = rf_S2,
                                    "S2+VI" = rf_S2_VI,
                                    "All products" = rf_ALL,
                                    "S1+S2" = rf_S1_S2))

models_evaluation
# Check the statistics
summary(models_evaluation)

# Visualize the results
bwplot(models_evaluation, layout = c(3, 1))

####################  Lets perfom the classifications
#
# This process will take a while to compute!!
timeStart<- proc.time() # measure computation time
class_S2 <- predict(S2_Antwerp, rf_S2)
class_S1 <- predict(S1_Antwerp, rf_S1)
class_S1_GLCM <- predict(S1_GLCM_Antwerp, rf_S1_GLCM)
class_S1_S2 <- predict(S1_S2_Antwerp,rf_S1_S2)
class_S2_VI <- predict(S2_VI_Antwerp,rf_S2_VI)
class_ALL <- predict(ALL_Antwerp,rf_ALL)
proc.time() - timeStart

#################### Visualize the Land cover classifcation
#
# Create the labels for the LC classes
labels<- c("Bare Soil", "Cropland", "Forest land","River sandbanks","Urban areas","Water","Wetland")

# Create the values for the colors 
values<- c("#FF9933", "#FFFF33", "#006600", "#FFCC99","#CC00CC", "#004C99","#66B2FF")

# Land cover classifcation using S2 data
LC_s2 <- gplot(class_S2)+
  geom_raster(aes(fill = factor(value, labels= labels)))+
  scale_fill_manual(values = values,name= "Land Cover")+
  ggtitle("RF on Sentinel 2")+
  theme(plot.title = element_text(lineheight=.4, face="bold"))+
  coord_equal()
LC_s2

# Land cover classifcation using S1 data
LC_S1 <- gplot(class_S1)+
  geom_raster(aes(fill = factor(value, labels= labels)))+
  scale_fill_manual(values = values,name= "Land Cover")+
  ggtitle("RF on Sentinel 1")+
  theme(plot.title = element_text(lineheight=.4, face="bold"))+
  coord_equal()
LC_S1

# Land cover classifcation using S1 and GLCM data
LC_S1_GLCM <- gplot(class_S1_GLCM)+
  geom_raster(aes(fill = factor(value, labels= labels)))+
  scale_fill_manual(values = values,name= "Land Cover")+
  ggtitle("RF on Sentinel 1 and GLCM")+
  theme(plot.title = element_text(lineheight=.4, face="bold"))+
  coord_equal()
LC_S1_GLCM

# Land cover classifcation using S1 and S2 data
LC_S1_S2 <- gplot(class_S1_S2)+
  geom_raster(aes(fill = factor(value, labels= labels)))+
  scale_fill_manual(values = values,name= "Land Cover")+
  ggtitle("RF on Sentinel 1 and Sentinel 2")+
  theme(plot.title = element_text(lineheight=.4, face="bold"))+
  coord_equal()
LC_S1_S2

# Land cover classifcation using S2 and VI data
LC_S2_VI <- gplot(class_S2_VI)+
  geom_raster(aes(fill = factor(value, labels= labels)))+
  scale_fill_manual(values = values,name= "Land Cover")+
  ggtitle("RF on Sentinel 2 and VI")+
  theme(plot.title = element_text(lineheight=.4, face="bold"))+
  coord_equal()
LC_S2_VI

# Land cover classifcation using All data
LC_ALL <- gplot(class_ALL)+
  geom_raster(aes(fill = factor(value, labels=labels)))+
  scale_fill_manual(values = values,name= "Land Cover")+
  ggtitle("RF on all the data")+
  theme(plot.title = element_text(lineheight=.4, face="bold"))+
  coord_equal()
LC_ALL

#################### Save the land cover classification maps
# First prepare a folder to save the classifcation maps
#
writeRaster(class_S2, 
            "D:/MB3/MB2_Script/CL_Classification/RF_S2.tif",
            type="raw",
            datatype='INT2U',
            index=1,
            na.rm=TRUE,
            progress="window",
            overwrite=TRUE)

writeRaster(class_S1, 
            "D:/MB3/MB2_Script/CL_Classification/RF_S1.tif",
            type="raw",
            datatype='INT2U',
            index=1,
            na.rm=TRUE,
            progress="window",
            overwrite=TRUE)

writeRaster(class_S1_GLCM, 
            "D:/MB3/MB2_Script/CL_Classification/RF_S1_GLCM.tif",
            type="raw",
            datatype='INT2U',
            index=1,
            na.rm=TRUE,
            progress="window",
            overwrite=TRUE)

writeRaster(class_S1_S2, 
            "D:/MB3/MB2_Script/CL_Classification/RF_S1_S2.tif",
            type="raw",
            datatype='INT2U',
            index=1,
            na.rm=TRUE,
            progress="window",
            overwrite=TRUE)

writeRaster(class_S2_VI, 
            "D:/MB3/MB2_Script/CL_Classification/RF_S2_VI.tif",
            type="raw",
            datatype='INT2U',
            index=1,
            na.rm=TRUE,
            progress="window",
            overwrite=TRUE)

writeRaster(class_ALL, 
            "D:/MB3/MB2_Script/CL_Classification/RF_All_Data.tif",
            type="raw",
            datatype='INT2U',
            index=1,
            na.rm=TRUE,
            progress="window",
            overwrite=TRUE)

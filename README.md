# MB2-Final-Project
Final Project for the Programming and Geostatistical Analysis course (MSc. Earth Observation and Geoanalysis)

This Script was written as a final project for the Programming and Geostatistical Analysis course [MB2](http://eagle-science.org/project/programming-and-geostatistical-analysis/) which is one of the courses that I took during my [MSc. in Applied Earth Observation and Geoanalysis (EAGLE)](http://eagle-science.org/) at [JMU Würzburg](https://www.uni-wuerzburg.de/en/home/)  

The purpose of the project is to evaluate Sentinel 1 and Sentinel 2 Data for Land Cover Classification using Machine Learning "Random Forest(RF)" in the area of Antwerp, Belgium.
The data were dowloded from [copernicus open access hub](https://scihub.copernicus.eu/dhus/#/home)  and the Preprocessing steps were conducted using the open source software [SNAP](http://step.esa.int/main/download/snap-download/)

Six land cover classification maps were generated according to the data integration method; Sentinel 1 only, Sentinel 2 only, Sentinel 1 with its VV and VH GLCM bands, Sentinel 1 with Sentinel 2, Sentinel 2 with its Vegetation Indices (VI) (NDVI, SAVI, MSAVI2, NDWI2) and finaly all the data together.
All the RF land cover classification models were evaluated by the accuracy assessment.

## Author
Walid Ghariani MSc. Student in [Applied Earth Observation and Geoanalysis (EAGLE)](http://eagle-science.org/) [linkedin](https://www.linkedin.com/in/walid-ghariani-893365138/) [E-mail](walid.ghariani@stud-mail.uni-wuerzburg.de)

## References

[Wegmann M, Leutner B, Dech S (2016) Remote sensing and GIS for ecologists using open source software. Pelagic Publishing, Exeter, UK.](http://book.ecosens.org/RSEbook/) 
[Kamusoko, C. (2019). Remote Sensing Image Classification in R. Springer](https://www.springer.com/gp/book/9789811380112)
[Robert J. Hijmans, (2016-2020). Spatial Data Science with R](https://rspatial.org/#)

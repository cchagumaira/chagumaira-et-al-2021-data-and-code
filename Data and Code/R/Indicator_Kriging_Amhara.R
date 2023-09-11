##################################################################################################
##################################################################################################
# R code for the Indicator Kriging for grain teff Se in Amhara, Ethiopia
# presented by Chagumaira et al "Communicating uncertainties in spatial 
# predictions of grainmicronutrient concentration"
#
#  The data provided with this script are anonymized and contain no personal information about
#  participants in the study.  This is in accordance with the terms under which ethical approval 
#  for the study which was granted by University of Nottingham School of Sociology and Social 
#  Policy Research Ethics subcommitee, references BIO1920-004, BIO1920-007 for the Malawi 
#  and Ethiopia exercises respectively.  We hold consent forms from all participants which record
#  that they understand and have given informed consent to these terms.  Note that oversight of 
#  research ethics for this work by the University of Nottingham was undertaken in agreement with 
#  research partners in Malawi and Ethiopia, where research into and capacity strengthening 
#  for research ethics for food system studies is part of the scope of the GeoNutrition Project 
#  (Bill and Melinda Gates Foundation, INV-009129).
#
#
##################################################################################################
##################################################################################################
# External Libraries: 


library(geoR)
library(ggplot2)
library(gridExtra)
library(gstat)
library(psych)
library(sp)

##################################################################################################

# read in data

teff.df<-read.table("amhara.txt", header=T)
names(teff.df)



# Exploratory data analysis

describe(teff.df$Se)
hist(teff.df$Se)


##################################################################################################

# Indicator Transformation 
# nutritionally signifant threshold 0.038 mg of Se, such that # a serving of 330g 
# of grain flour provides a third of the daily EAR of Se for 
# an adult woman. 

# grain Se concentration is less than 0.038 

ik<- teff.df$Se <= 0.038


# dataframe with the indicator

ik.df<-as.data.frame(cbind(teff.df,ik))
head(ik.df)

##################################################################################################


# Read the prediction grid points

grid<-read.table("amhara_gridpoints.dat", header=T)
head(grid)


# Convert to spatialdataframe

coordinates(ik.df)<-~x+y
coordinates(grid)<-~x+y



##################################################################################################

# Indicator Variograms (grain Se concentration is less than 0.038 )

vg<-variogram(ik~ 1, data = ik.df)

plot(vg)

# Intial parameter set by eye esitmation

m<-vgm(0.06,"Exp",38,0.13)

# least square fit

m.f<-fit.variogram(vg, m)

print(m.f)


#### Plot varigram and fitted model:

plot(vg, pl=F, 
     model=m.f,
     col="black", 
     cex=0.9, 
     lwd=0.5,
     lty=1,
     pch=19,
     main="Indicator Variogram / Se <= 0.038 mg",
     xlab="Distance (km)",
     ylab="Variance")


##############################################################
##############################################################
# Indicator kriging prediction at grid location 

pred<-krige(ik~ 1, 
              loc=ik.df,       
              newdata=grid,     
              model = m.f)

summary(pred)



# Limit the predicted probabilities to the range


pred$var1.pred <- pmin(1, pred$var1.pred)
pred$var1.pred <- pmax(0, pred$var1.pred)

summary(pred)

##################################### 

# export the data: 

grid2<-as.data.frame(grid)
head(grid2)

# Coordinates to metres from km 

grid2$Easting<-grid2$x
grid2$Northing<-grid2$y

# Add the probability 

grid2$Prob<-pred$var1.pred 

##############

head(grid2)

# Then data was exported to csv and the maps were plotted in ArcGIS 10.4.1 

# The locations on the maps we used evaluate the communication methods

# Site   Easting 	Northing
#  x     594464   1145397
#  y 	   351540   1317049
#  z     378830	1208482	


# END OF SCRIPT# 

##########################################################################################################################################################################################


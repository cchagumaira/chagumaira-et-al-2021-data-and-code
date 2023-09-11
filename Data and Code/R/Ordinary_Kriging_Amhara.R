##################################################################################################
##################################################################################################
#
# R code for the Ordinary Kriging for grain teff Se in Amhara, Ethiopia
# presented by Chagumaira et al "Communicating uncertainties in spatial 
# predictions of grain micronutrient concentration"
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
##################################################################################################
##################################################################################################
# External Libraries: 

library(geoR)
library(ggplot2)
library(mapdata       
library(MASS)
library(plyr)
library(psych)
library(rgdal) 
library(sp)



# Function

# Octile Skewness

ocskew<-function(x){
ocs<-quantile(x,probs=c(1/8,1/2,7/8))
ocsk<-as.numeric(
((ocs[3]-ocs[2])-(ocs[2]-ocs[1]))/(ocs[3]-ocs[1])
)
return(ocsk)
}



##################################################################################################


# Read in the data

teff.df<-read.table("amhara.txt", header=T)
names(teff.df)



# Exploratory Analysis 

describe(teff.df$Se)
ocskew(teff.df$Se)
hist(teff.df$Se)

describe(log(teff.df$Se))
ocskew(log(teff.df$Se))
hist(log(teff.df$Se))

# need log transformed Se

teff.df$logSe<-log(teff.df$Se)

# Post-plot of , to check for trends 

ggplot(teff.df, aes(x=x, y=y, size=Se))+
geom_point()+ labs(size = "Teff grain Se conc. mg/kg")+theme_bw() +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())

## We can deduce there is a visible trend in the sampled data.


##########################################################################
#
# make a geodata for 

Se.geo<-as.geodata(teff.df,coords.col=1:2, data.col="logSe")

plot(Se.geo)


# blue, green, yellow and red symbols correspond to 1st, 2nd, 3rd and 4th quartiles


# Experimental Variogram 

vario.Se<-variog(Se.geo,lambda=1, trend="1st", max.dist=160, uvec=10)

plot(vario.Se, pch=18,xlab="Distance /km",ylab="Variance")


#Variance Parameter Estimation using REML 

Sphmod<-likfit(Se.geo,trend="1st",lambda=1,
cov.model="spherical",ini.cov.pars=c(1.1,50),lik.method="REML")

Expmod<-likfit(Se.geo,trend="1st",lambda=1,
cov.model="exponential",ini.cov.pars=c(1.1,50),lik.method="REML")


lines(Sphmod, col="red")
lines(Expmod, col="blue")

legend("bottomright",legend=c("Spherical Model","Exponential Model"), 
col=c("red","blue"), lty=1:1,cex=1.0,box.lty=0)



# comparing two models using loglik 

Sphmod$loglik # has larger loglik -195.6688
Expmod$loglik


############################################
# cross-validate the model
#


Sphmod.xvl<-xvalid(Se.geo,model=Sphmod)


summary(Sphmod.xvl)

summary(Sphmod.xvl$krige.var)

theta<-((Sphmod.xvl$error)^2)/Sphmod.xvl$krige.var

describe(theta) # Median is 0.33 which is good 


##################################################################################################

# Read the prediction grid points

grid<-read.table("amhara_gridpoints.dat", header=T)
head(grid)


# Make make location matrixes 

coords.gr<-data.matrix(grid[1:2])


# Make predictions 

KC<-krige.control(obj.model=Sphmod,trend.d="1st",trend.l="1st")

Se.krg<- krige.conv(Se.geo, loc = coords.gr, krige = KC, borders=NULL)


# Combine the predictions and location grids

Se.pred<-data.frame(rbind(cbind(coords.gr,Se.krg$predict,Se.krg$krige.var)))

names(Se.pred)
colnames(Se.pred)<-c("Easting", "Northing", "predict", "krige.var")
head(Se.pred)


# Prediction Intervals 


Se.pred$krige.std<-sqrt(Se.pred$krige.var)

describe(Se.pred$krige.std)
hist(Se.pred$krige.std)


# Lower limit (Prediction - 1.96* krige.std)

Se.pred$lower<-(Se.pred$predict-(1.96*Se.pred$krige.std))

# Upper limit (Prediction + 1.96 * Kriging Standard Error)

Se.pred$upper<-(Se.pred$predict +(1.96*Se.pred$krige.std))



# back transform

Se.pred$Prediction<-(exp(Se.pred$predict))
Se.pred$Low<-(exp(Se.pred$lower))
Se.pred$Upp<-(exp(Se.pred$upper))


head(Se.pred)

###################

# Then data was exported to csv and the maps were plotted in ArcGIS 10.4.1 

# The locations on the maps we used evaluate the communication methods

# Site   Easting 	Northing
#  x     594464   1145397
#  y 	   351540   1317049
#  z     378830	1208482	

# END OF SCRIPT# 
##########################################################################################################################################################################################
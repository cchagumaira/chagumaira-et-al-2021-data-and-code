
##################################################################################################
##################################################################################################
#
#  R code for the analyses questionnaire contingency partition analysis  
#  presented by Chagumaira et al "Communicating uncertainties in spatial 
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


#############################
#  External library
library(MASS)



# Partition by Location of Meeting 

######################
# Interpretive Task  #
######################



# Question 1 (Table 4): Is it clear from the poster, that the statement below is true?   
# “Our confidence that grain selenium concentration exceeds 38 µg/kg is greater at x than at z”

MQ1_tab<-matrix(c(
8,	0,	6,	2,	8,
1,	1,	1,	0,	22,
5,	3,	3,	3,	11,
2,	2,	0,	3,	18,
1,	1,	0,	0,	22,
1,	9,	5,	7,	13,
0,	8,	4,	2,	20,
4,	6,	3,	2,	19,
1,	6,	5,	2,	22,
0,	4,	3,	3,	26), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("MWI Poster 1","MWI Poster 2", "MWI Poster 3",  "MWI Poster 4a", "MWI Poster 4b", 
"ETH Poster 1","ETH Poster 2","ETH Poster 3","ETH Poster 4a","ETH Poster 4b")))



loglm(~Response+Group,data=MQ1_tab) #93.33274

MQ1_tab2<-matrix(c(margin.table(MQ1_tab[,(1:5)],mar=1),
margin.table(MQ1_tab[,(6:10)],mar=1)),nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"),
Group=c("Malawi Meeting","Ethiopia Meeting")))


loglm(~Response+Group,data=MQ1_tab2) #22.83014 

loglm(~Response+Group,data=MQ1_tab[,(1:5)])#48.71818 malawi

loglm(~Response+Group,data=MQ1_tab[,(6:10)])#21.78442 ethiopia



# Further analysis pooled MQ1 (subtable for Ethiopia and Malawi)

ETH.MQ1_tab3<-matrix(c(1,9,5,7,13,0,8,4,2,20,4,6,3,2,19,1,6,5,2,22,0,4,3,3,26), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Poster 1","Poster 2", "Poster 3",  "Poster 4a", "Poster 4b")))

MWI.MQ1_tab3<-matrix(c(8,0,6,2,8,1,1,1,0,22,5,3,3,3,11,2,2,0,3,18,1,1,0,0,22), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Poster 1","Poster 2", "Poster 3",  "Poster 4a", "Poster 4b")))

loglm(~Response+Group,data=ETH.MQ1_tab3)#21.78442 no differences in poster effects
loglm(~Response+Group,data=MWI.MQ1_tab3)#48.71818 significant differences in poster effects

# Methods pooled "Threshold based" vs "General"

ETH.MQ1_pooled_methods<-matrix(c(5,15,8,9,32,1,18,12,7,68), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("General","Threshold")))

MWI.MQ1_pooled_methods<-matrix(c(13,3,9,5,19,4,4,1,3,62), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("General","Threshold")))

loglm(~Response+Group,data=ETH.MQ1_pooled_methods)#9.613548 
loglm(~Response+Group,data=MWI.MQ1_pooled_methods)#31.95231 


loglm(~Response+Group,data=ETH.MQ1_tab3[,c(2,4,5)])#5.074883  within threshold
loglm(~Response+Group,data=MWI.MQ1_tab3[,c(2,4,5)])#10.232391 within threshold

loglm(~Response+Group,data=ETH.MQ1_tab3[,c(1,3)])# 7.095987  within general
loglm(~Response+Group,data=MWI.MQ1_tab3[,c(1,3)])#6.533482 within general

#9.613548+5.074883+7.095987  =21.78442
#31.95231+10.232391+6.533482 =48.71818

# now compare "raw probability" with "guided"

ETH.MQ1_prob_vs_guided<-matrix(c(1,6,5,2,22,0,12,7,5,46), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Probability","Prob_with_guides")))

MWI.MQ1_prob_vs_guided<-matrix(c(2,2,0,3,18,2,2,1,0,44), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Probability","Prob_with_guides")))


loglm(~Response+Group,data=ETH.MQ1_prob_vs_guided)#2.643296 probability vs pictograph and verbal scale

loglm(~Response+Group,data=MWI.MQ1_prob_vs_guided)#8.866506 probability vs pictograph and verbal scale


loglm(~Response+Group,data=ETH.MQ1_tab3[,c(2,5)])#2.431587 pictograph vs verbal scale

loglm(~Response+Group,data=MWI.MQ1_tab3[,c(2,5)])#1.365885  pictograph vs verbal scale


# 3.897093+2.431587 =6.32868
# 8.866506+1.365885  =4.397756


# Question 2 (Table 5): Is it clear from the poster, that the statement below is true?  
# “Our confidence that grain selenium concentration does not exceed 38µg/kg is greater at z than at y”

MQ2_tab<-matrix(c(
5,	1,	6,	3,	9,
5,	1,	1,	2,	16,
5,	0,	1,	5,	14,
3,	1,	3,	4,	14,
3,	1,	2,	0,	18,
7,	9,	4,	1,	14,
2,	6,	4,	3,	19,
6,	9,	6,	3,	10,
5,	8,	6,	2,	15,
3,	3,	7,	5,	18), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("MWI Poster 1","MWI Poster 2", "MWI Poster 3",  "MWI Poster 4a", "MWI Poster 4b", 
"ETH Poster 1","ETH Poster 2","ETH Poster 3","ETH Poster 4a","ETH Poster 4b")))

loglm(~Response+Group,data=MQ2_tab) #60.65521

MQ2_tab2<-matrix(c(margin.table(MQ2_tab[,(1:5)],mar=1),
margin.table(MQ2_tab[,(6:10)],mar=1)),nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"),
Group=c("Malawi Meeting","Ethiopia Meeting")))



loglm(~Response+Group,data=MQ2_tab2) #24.41828

loglm(~Response+Group,data=MQ2_tab[,(1:5)])#20.02401

loglm(~Response+Group,data=MQ2_tab[,(6:10)])#16.21291 

60.65521
24.41828+20.02401+16.21291 


# Further analysis pooled MQ2

ETH.MQ2_tab3<-matrix(c(7,9,4,1,14,2,6,4,3,19,6,9,6,3,10,5,8,6,2,15,3,3,7,5,18), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Poster 1","Poster 2", "Poster 3",  "Poster 4a", "Poster 4b")))

MWI.MQ2_tab3<-matrix(c(5,1,6,3,9,5,1,1,2,16,5,0,1,5,14,3,1,3,4,14,3,1,2,0,18), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Poster 1","Poster 2", "Poster 3",  "Poster 4a", "Poster 4b")))

loglm(~Response+Group,data=ETH.MQ2_tab3)#16.21291 no differences in poster effects
loglm(~Response+Group,data=MWI.MQ2_tab3)#20.02401 no differences in poster effects

# Methods pooled "Threshold based" vs "General"

ETH.MQ2_pooled_methods<-matrix(c(13,18,10,4,24,10,17,17,10,52), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("General","Threshold")))

MWI.MQ2_pooled_methods<-matrix(c(10,1,7,8,23,11,3,6,6,48), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("General","Threshold")))

loglm(~Response+Group,data=ETH.MQ2_pooled_methods)#7.593908 
loglm(~Response+Group,data=MWI.MQ2_pooled_methods)#5.335337


loglm(~Response+Group,data=ETH.MQ2_tab3[,c(2,4,5)])#6.437504 within threshold
loglm(~Response+Group,data=MWI.MQ2_tab3[,c(2,4,5)])#7.759334within threshold

loglm(~Response+Group,data=ETH.MQ2_tab3[,c(1,3)])#2.181500  within general
loglm(~Response+Group,data=MWI.MQ2_tab3[,c(1,3)])#1.107219  within general


# 7.593908+6.437504+2.181500 = 16.21291
# 5.335337+7.759334+6.929344 = 20.02401


# now compare "raw probability" with "guided"

ETH.MQ2_prob_vs_guided<-matrix(c(5,8,6,2,15,5,9,11,8,37), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Probability","Prob_with_guides")))

MWI.MQ2_prob_vs_guided<-matrix(c(3,1,3,4,14,8,2,3,2,34), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Probability","Prob_with_guides")))


loglm(~Response+Group,data=ETH.MQ2_prob_vs_guided)#3.912893 probability vs pictograph and verbal scale
loglm(~Response+Group,data=MWI.MQ2_prob_vs_guided)#4.044298 probability vs pictograph and verbal scale


loglm(~Response+Group,data=ETH.MQ2_tab3[,c(2,5)])#2.524611 pictograph vs verbal scale
loglm(~Response+Group,data=MWI.MQ2_tab3[,c(2,5)])#3.715035 pictograph vs verbal scale

# 3.912893  + 2.524611 = 6.437504
# 4.044298  +3.715035 = 7.759333



# Question 3 (Table 6): Is it clear from the poster, that the statement below is true?  
# “Our confidence that grain selenium concentration does not exceed 38µg/kg is greater at y than at x”

MQ3_tab<-matrix(c(
8,	0,	5,	3,	8,
4,	2,	3,	0,	15,
8,	2,	3,	5,	6,
5,	1,	4,	3,	11,
3,	1,	4,	1,	14,
4,	9,	3,	6,	13,
1,	4,	4,	3,	21,
4,	7,	4,	5,	14,
3,	4,	5,	2,	22,
1,	3,	6,	4,	22), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("MWI Poster 1","MWI Poster 2", "MWI Poster 3",  "MWI Poster 4a", "MWI Poster 4b", 
"ETH Poster 1","ETH Poster 2","ETH Poster 3","ETH Poster 4a","ETH Poster 4b")))

loglm(~Response+Group,data=MQ3_tab) #60.36379

MQ3_tab2<-matrix(c(margin.table(MQ3_tab[,(1:5)],mar=1),
margin.table(MQ3_tab[,(6:10)],mar=1)),nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"),
Group=c("Malawi Meeting","Ethiopia Meeting")))


loglm(~Response+Group,data=MQ3_tab2) #21.93238

loglm(~Response+Group,data=MQ3_tab[,(1:5)])#21.83190 
loglm(~Response+Group,data=MQ3_tab[,(6:10)])#16.59951  

60.36379
21.93238+21.83190+16.59951 


# Further analysis pooled MQ3

ETH.MQ3_tab3<-matrix(c(4,9,3,6,13,1,4,4,3,21,4,7,4,5,14,3,4,5,2,22,1,3,6,4,22), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Poster 1","Poster 2", "Poster 3",  "Poster 4a", "Poster 4b")))

MWI.MQ3_tab3<-matrix(c(8,0,5,3,8,4,2,3,0,15,8,2,3,5,6,5,1,4,3,11,3,1,4,1,14), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Poster 1","Poster 2", "Poster 3",  "Poster 4a", "Poster 4b")))

loglm(~Response+Group,data=ETH.MQ3_tab3)#16.59951 no differences in poster effects
loglm(~Response+Group,data=MWI.MQ3_tab3)#21.83190 no differences in poster effects

# Methods pooled "Threshold based" vs "General"

ETH.MQ3_pooled_methods<-matrix(c(8,16,7,11,27,5,11,15,9,65), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("General","Threshold")))

MWI.MQ3_pooled_methods<-matrix(c(16,2,8,8,14,12,4,11,4,40), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("General","Threshold")))

loglm(~Response+Group,data=ETH.MQ3_pooled_methods)#13.48024
loglm(~Response+Group,data=MWI.MQ3_pooled_methods)#11.66795 


loglm(~Response+Group,data=ETH.MQ3_tab3[,c(2,4,5)])#2.611679 within threshold
loglm(~Response+Group,data=MWI.MQ3_tab3[,c(2,4,5)])#6.093980 within threshold

loglm(~Response+Group,data=ETH.MQ3_tab3[,c(1,3)])#0.5075892 within general
loglm(~Response+Group,data=MWI.MQ3_tab3[,c(1,3)])#4.069969   within general

# 13.48024+2.611679+0.5075892 = 16.59951
# 11.66795 + 6.093980 + 4.069969 = 21.83190  


# now compare "raw probability" with "guided"

ETH.MQ3_prob_vs_guided<-matrix(c(3,4,5,2,22,2,7,10,7,43), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Probability","Prob_with_guides")))

MWI.MQ3_prob_vs_guided<-matrix(c(5,1,4,3,11,7,3,7,1,29), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Probability","Prob_with_guides")))


loglm(~Response+Group,data=ETH.MQ3_prob_vs_guided)#2.029493 probability vs pictograph and verbal scale
loglm(~Response+Group,data=MWI.MQ3_prob_vs_guided)#4.067982 probability vs pictograph and verbal scale


loglm(~Response+Group,data=ETH.MQ3_tab3[,c(2,5)])#0.5821864  pictograph vs verbal scale
loglm(~Response+Group,data=MWI.MQ3_tab3[,c(2,5)])#2.025998  pictograph vs verbal scale





##########################################################################
##########################################################################

#######################
# Adequate Information# 
#######################

# Question 4 (Table 7): Does the poster provide adequate information for you to determine how 
# likely it is that an intervention programme is needed at any given location?

MQ4_tab<-matrix(c(
9,	12,	3,
2,	20,	2,
7,	15,	1,
4,	19,	1,
4,	17,	2,
15,	16,	3,
9,	20,	4,
16,	13,	3,
11,	23,	2,
13,	19,	4), nrow=3,
dimnames=list(Response=c("Inadequate","Adequate", "Too much"), 
Group= c("MWI Poster 1","MWI Poster 2", "MWI Poster 3",  "MWI Poster 4a", "MWI Poster 4b", 
"ETH Poster 1","ETH Poster 2","ETH Poster 3","ETH Poster 4a","ETH Poster 4b")))

loglm(~Response+Group,data=MQ4_tab) #25.69786

MQ4_tab2<-matrix(c(margin.table(MQ4_tab[,(1:5)],mar=1),
margin.table(MQ4_tab[,(6:10)],mar=1)),nrow=3,
dimnames=list(Response=c("Inadequate","Adequate", "Too much"),
Group=c("Malawi Meeting","Ethiopia Meeting")))


loglm(~Response+Group,data=MQ4_tab2) #9.138249

loglm(~Response+Group,data=MQ4_tab[,(1:5)])#10.087174 
loglm(~Response+Group,data=MQ4_tab[,(6:10)])#6.472434


25.69786
9.138249+10.087174+6.472434



# Further analysis pooled MQ4

ETH.MQ4_tab3<-matrix(c(15,16,3,9,20,4,16,13,3,11,23,2,13,19,4),nrow=3,
dimnames=list(Response=c("Inadequate","Adequate", "Too much"),
Group=c("Poster 1","Poster 2", "Poster 3",  "Poster 4a", "Poster 4b")))

MWI.MQ4_tab3<-matrix(c(9,12,3,2,20,2,7,15,1,4,19,1,4,17,2),nrow=3,
dimnames=list(Response=c("Inadequate","Adequate", "Too much"),
Group=c("Poster 1","Poster 2", "Poster 3",  "Poster 4a", "Poster 4b")))

loglm(~Response+Group,data=ETH.MQ4_tab3)# 6.472434
loglm(~Response+Group,data=MWI.MQ4_tab3)# 10.087174  



ETH.MQ4_pooled_methods<-matrix(c(31,29,6,33,62,10),nrow=3,
dimnames=list(Response=c("Inadequate","Adequate", "Too much"), 
Group= c("General","Threshold")))

MWI.MQ4_pooled_methods<-matrix(c(16,27,4,10,56,5),nrow=3,
dimnames=list(Response=c("Inadequate","Adequate", "Too much"), 
Group= c("General","Threshold")))



loglm(~Response+Group,data=ETH.MQ4_pooled_methods)# 4.343876
loglm(~Response+Group,data=MWI.MQ4_pooled_methods)# 6.942406


loglm(~Response+Group,data=ETH.MQ4_tab3[,c(2,4,5)])# 1.846009   within threshold
loglm(~Response+Group,data=MWI.MQ4_tab3[,c(2,4,5)])# 1.534872   within threshold

loglm(~Response+Group,data=ETH.MQ4_tab3[,c(1,3)])# 0.2825491 within general
loglm(~Response+Group,data=MWI.MQ4_tab3[,c(1,3)])# 1.609896 within general


# 4.343876 +1.846009+0.2825491 = 6.472434
# 6.942406 +1.534872+1.609896 = 10.087174  


ETH.MQ4_prob_vs_guided<-matrix(c(11,23,2,22,39,8), nrow=3,
dimnames=list(Response=c("Inadequate","Adequate", "Too much"), 
Group= c("Probability","Prob_with_guides")))

MWI.MQ4_prob_vs_guided<-matrix(c(4,19,1,6,37,4), nrow=3,
dimnames=list(Response=c("Inadequate","Adequate", "Too much"), 
Group= c("Probability","Prob_with_guides")))


loglm(~Response+Group,data=ETH.MQ4_prob_vs_guided)# 1.219507  probability vs pictograph and verbal scale
loglm(~Response+Group,data=MWI.MQ4_prob_vs_guided)# 0.6330439 probability vs pictograph and verbal scale


loglm(~Response+Group,data=ETH.MQ4_tab3[,c(2,5)]) # 0.6265016  pictograph vs verbal scale
loglm(~Response+Group,data=MWI.MQ4_tab3[,c(2,5)]) # 0.9018284  pictograph vs verbal scale

# 1.219507    + 0.6265016 =1.846009
# 0.6330439 + 0.9018284 =1.534872


##########################################################################
##########################################################################

#################################
# Straight Forward to Interpret #
#################################


# Question 5 (Table 8): Is the way this poster communicates the uncertainty about grain selenium content straightforward to interpret?

MQ5_tab<-matrix(c(
4,	4,	3,	5,	5,
2,	1,	2,	4,	12,
5,	4,	4,	2,	8,
1,	2,	6,	5,	8,
2,	3,	1,	5,	11,
3,	10,	7,	2,	13,
8,	6,	7,	11,	32,
5,	8,	5,	7,	8,
4,	7,	3,	7,	13,
1,	8,	3,	9,	14), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("MWI Poster 1","MWI Poster 2", "MWI Poster 3",  "MWI Poster 4a", "MWI Poster 4b", 
"ETH Poster 1","ETH Poster 2","ETH Poster 3","ETH Poster 4a","ETH Poster 4b")))

loglm(~Response+Group,data=MQ5_tab) #40.93078

MQ5_tab2<-matrix(c(margin.table(MQ5_tab[,(1:5)],mar=1),
margin.table(MQ5_tab[,(6:10)],mar=1)),nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"),
Group=c("Malawi Meeting","Ethiopia Meeting")))


loglm(~Response+Group,data=MQ5_tab2) #2.554371 

loglm(~Response+Group,data=MQ5_tab[,(1:5)])#16.74546 
loglm(~Response+Group,data=MQ5_tab[,(6:10)])#21.63094



40.93078

21.63094+16.74546+2.554371 

# Further analysis pooled MQ5

MQ5_tab3<-matrix(c(7,14,10,7,18,2,9,8,11,23,10,12,9,9,16,5,9,9,12,21,3,11,4,14,25), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Poster 1","Poster 2", "Poster 3",  "Poster 4a", "Poster 4b")))


loglm(~Response+Group,data=MQ5_tab3)#17.73504 no differences in poster effects

# Methods pooled "Threshold based" vs "General"

MQ5_pooled_methods<-matrix(c(17,26,19,16,34,10,29,21,37,69), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("General","Threshold")))

loglm(~Response+Group,data=MQ5_pooled_methods)#12.23007 , signficant differences between general methods and threshold based


loglm(~Response+Group,data=MQ5_tab3[,c(2,4,5)])#4.397756 within threshold
loglm(~Response+Group,data=MQ5_tab3[,c(1,3)])#1.107219  within general

#12.23007+4.397756+1.107219=17.73504

# now compare "raw probability" with "guided"

MQ5_prob_vs_guided<-matrix(c(5,9,9,12,21,5,20,12,25,48), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("Probability","Prob_with_guides")))

loglm(~Response+Group,data=MQ5_prob_vs_guided)#2.338134 probability vs pictograph and verbal scale

loglm(~Response+Group,data=MQ5_tab3[,c(2,5)])#2.059622 pictograph vs verbal scale

# 2.338134+2.059622 = 4.397756




##########################################################################
##########################################################################

#################################
# Understanding Uncertainty     #
#################################


# Question 6 (Table 9): Do you think that the poster helped you to understand the uncertainty in the predictions?



MQ6_tab<-matrix(c(
10,	11,
17,	3,
13,	9,
17,	5,
18,	4,
23,	10,
31,	3,
21,	13,
31,	4,
32,	4), nrow=2,
dimnames=list(Response=c("Yes","No"), 
Group= c("MWI Poster 1","MWI Poster 2", "MWI Poster 3",  "MWI Poster 4a", "MWI Poster 4b", 
"ETH Poster 1","ETH Poster 2","ETH Poster 3","ETH Poster 4a","ETH Poster 4b")))

loglm(~Response+Group,data=MQ6_tab) #29.08157

MQ6_tab2<-matrix(c(margin.table(MQ6_tab[,(1:5)],mar=1),
margin.table(MQ6_tab[,(6:10)],mar=1)),nrow=2,
dimnames=list(Response=c("Yes","No"),
Group=c("Malawi Meeting","Ethiopia Meeting")))


loglm(~Response+Group,data=MQ6_tab2) #2.554371 

loglm(~Response+Group,data=MQ6_tab[,(1:5)])#16.74546 
loglm(~Response+Group,data=MQ6_tab[,(6:10)])#21.63094


29.08157
3.692663+10.37064+15.01827


# Further analysis pooled MQ6

MQ6_tab3<-matrix(c(33,21,48,6,34,22,48,9,50,8), nrow=2,
dimnames=list(Response=c("Yes","No"), 
Group= c("Poster 1","Poster 2", "Poster 3",  "Poster 4a", "Poster 4b")))


MQ6_pooled_methods<-matrix(c(67,43,146,23), nrow=2,
dimnames=list(Response=c("Yes","No"), 
Group= c("General","Threshold")))


loglm(~Response+Group,data=MQ6_tab3)#24.12505 

loglm(~Response+Group,data=MQ6_pooled_methods)# 23.59773 


loglm(~Response+Group,data=MQ6_tab3[,c(2,4,5)])# 0.5255027   within threshold
loglm(~Response+Group,data=MQ6_tab3[,c(1,3)])# 0.001818198 within general


# 23.59773 +0.5255027+0.001818198 = 24.12505


MQ6_prob_vs_guided<-matrix(c(48,9,98,14), nrow=2,
dimnames=list(Response=c("Yes","No"), 
Group= c("Probability","Prob_with_guides")))

loglm(~Response+Group,data=MQ6_prob_vs_guided)# 0.3408770  probability vs pictograph and verbal scale

loglm(~Response+Group,data=MQ6_tab3[,c(2,5)])#0.1846258  pictograph vs verbal scale


#0.3408770+0.1846258=0.5255027


##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################
##########################################################################


# Partition by Professional Group 



######################
# Interpretive Task  #
######################


# Question 1: Is it clear from the poster, that the statement below is true?   
# “Our confidence that grain selenium concentration exceeds 38 µg/kg is greater at x than at z”


Q1_tab<-matrix(c(
2,	2,	3,	2,	2,
0,	0,	1,	0,	11,
3,	0,	1,	1,	7,
1,	1,	2,	0,	8,
0,	0,	0,	1,	11,
3,	2,	6,	5,	9,
1,	3,	2,	1,	18,
4,	5,	3,	3,	10,
1,	3,	2,	4,	15,
0,	1,	3,	0,	20,
4,	5,	2,	2,	10,
0,	6,	2,	1,	13,
2,	4,	2,	1,	13,
1,	4,	1,	1,	17,
1,	3,	1,	2,	17), nrow=5, 
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("AG Poster 1", "AG Poster 2","AG Poster 3","AG Poster 4a","AG Poster 4b",
"SS Poster 1", "SS Poster 2", "SS Poster 3", "SS Poster 4a", "SS Poster 4b",
"NHP Poster 1", "NHP Poster 2", "NHP Poster 3", "NHP Poster 4a", "NHP Poster 4b")))


loglm(~Response+Group,data=Q1_tab) #82.91648 



Q1_tab2<-matrix(c(margin.table(Q1_tab[,(1:5)],mar=1),
margin.table(Q1_tab[,(6:10)],mar=1), margin.table(Q1_tab[,(11:15)],mar=1)),nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"),
Group=c("Agronomy","Soil Scientist", "Nutrition/Health")))



loglm(~Response+Group,data=Q1_tab2) #11.71268 

loglm(~Response+Group,data=Q1_tab[,(1:5)])#30.72848
loglm(~Response+Group,data=Q1_tab[,(6:10)])#28.87730
loglm(~Response+Group,data=Q1_tab[,(11:15)]) #11.59802 


82.91648

11.71268 +30.72848+28.87730+11.59802 


# Question 2: Is it clear from the poster, that the statement below is true?  
# “Our confidence that grain selenium concentration does not exceed 38µg/kg is greater at z than at y”

Q2_tab<-matrix(c(
3,	1,	0,	1,	6,
4,	0,	1,	1,	6,
3,	1,	2,	1,	5,
3,	1,	1,	2,	5,
3,	0,	2,	2,	5,
3,	4,	7,	3,	8,
2,	2,	1,	3,	17,
4,	5,	3,	5,	8,
4,	3,	4,	1,	13,
1,	2,	3,	2,	16,
6,	5,	3,	0,	9,
1,	5,	3,	1,	12,
4,	3,	2,	2,	11,
1,	5,	4,	3,	11,
2,	2,	4,	1,	15), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("AG Poster 1", "AG Poster 2","AG Poster 3","AG Poster 4a","AG Poster 4b",
"SS Poster 1", "SS Poster 2", "SS Poster 3", "SS Poster 4a", "SS Poster 4b",
"NHP Poster 1", "NHP Poster 2", "NHP Poster 3", "NHP Poster 4a", "NHP Poster 4b")))


loglm(~Response+Group,data=Q2_tab) #57.83417 


Q2_tab2<-matrix(c(margin.table(Q2_tab[,(1:5)],mar=1),
margin.table(Q2_tab[,(6:10)],mar=1), margin.table(Q2_tab[,(11:15)],mar=1)),nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"),
Group=c("Agronomy","Soil Scientist", "Nutrition/Health")))



loglm(~Response+Group,data=Q2_tab2) #14.94812 

loglm(~Response+Group,data=Q2_tab[,(1:5)])#7.643894
loglm(~Response+Group,data=Q2_tab[,(6:10)])#19.49621
loglm(~Response+Group,data=Q2_tab[,(11:15)]) #15.74595

57.83417 

14.94812 +7.643894 +19.49621+15.74595


# Question 3: Is it clear from the poster, that the statement below is true?  
# “Our confidence that grain selenium concentration does not exceed 38µg/kg is greater at y than at x”

Q3_tab<-matrix(c(
4,	2,	2,	1,	2,
2,	0,	2,	0,	7,
4,	1,	2,	0,	4,
1,	0,	3,	1,	6,
2,	0,	2,	2,	5,
3,	3,	4,	4,	11,
2,	2,	3,	2,	15,
3,	6,	2,	6,	8,
5,	1,	2,	3,	14,
2,	3,	3,	1,	15,
5,	4,	2,	4,	8,
1,	4,	2,	1,	14,
5,	2,	3,	4,	8,
2,	4,	4,	1,	13,
0,	1,	5,	4,	14), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"), 
Group= c("AG Poster 1", "AG Poster 2","AG Poster 3","AG Poster 4a","AG Poster 4b",
"SS Poster 1", "SS Poster 2", "SS Poster 3", "SS Poster 4a", "SS Poster 4b",
"NHP Poster 1", "NHP Poster 2", "NHP Poster 3", "NHP Poster 4a", "NHP Poster 4b")))

loglm(~Response+Group,data=Q3_tab) # 65.38231


Q3_tab2<-matrix(c(margin.table(Q3_tab[,(1:5)],mar=1),
margin.table(Q3_tab[,(6:10)],mar=1), margin.table(Q3_tab[,(11:15)],mar=1)),nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"),
Group=c("Agronomy","Soil Scientist", "Nutrition/Health")))



loglm(~Response+Group,data=Q3_tab2) #10.00860  

loglm(~Response+Group,data=Q3_tab[,(1:5)])#17.02145
loglm(~Response+Group,data=Q3_tab[,(6:10)])#15.19432
loglm(~Response+Group,data=Q3_tab[,(11:15)]) #23.15795

65.38231

10.00860+17.02145+15.19432+23.15795





##########################################################################
##########################################################################

#######################
# Adequate Information# 
#######################

# Question 4 Does the poster provide adequate information for you to determine how 
# likely it is that an intervention programme is needed at any given location?


Q4_tab<-matrix(c(
4,	5,	2,
2,	8,	1,
5,	6,	0,
3,	7,	1,
1,	9,	1,
8,	12,	4,
2,	19,	3,
8,	15,	2,
4,	20,	1,
7,	14,	3,
12,	11,	0,
7,	13,	2,
10,	7,	2,
8,	15,	1,
9,	13,	2), nrow=3,
dimnames=list(Response=c("Inadequate", "Adequate", "More than Enough"),
Group= c("AG Poster 1", "AG Poster 2","AG Poster 3","AG Poster 4a","AG Poster 4b",
"SS Poster 1", "SS Poster 2", "SS Poster 3", "SS Poster 4a", "SS Poster 4b",
"NHP Poster 1", "NHP Poster 2", "NHP Poster 3", "NHP Poster 4a", "NHP Poster 4b")))



loglm(~Response+Group,data=Q4_tab) #35.26985


Q4_tab2<-matrix(c(margin.table(Q4_tab[,(1:5)],mar=1),
margin.table(Q4_tab[,(6:10)],mar=1), margin.table(Q4_tab[,(11:15)],mar=1)),nrow=3,
dimnames=list(Response=c("Inadequate", "Adequate", "More than Enough"),
Group=c("Agronomy","Soil Scientist", "Nutrition/Health")))


loglm(~Response+Group,data=Q4_tab2) #8.955839

loglm(~Response+Group,data=Q4_tab[,(1:5)])#7.808848 
loglm(~Response+Group,data=Q4_tab[,(6:10)])#10.722647
loglm(~Response+Group,data=Q4_tab[,(11:15)]) #7.782515

35.26985

8.955839+7.808848+10.722647+7.782515



##########################################################################
##########################################################################

#################################
# Straight Forward to Interpret #
#################################


# Question 5 Is the way this poster communicates the uncertainty about grain selenium content straightforward to interpret?


Q5_tab<-matrix(c(
1,	1,	4,	2,	3,
0,	3,	1,	3,	3,
2,	2,	1,	2,	4,
0,	2,	2,	3,	4,
0,	2,	1,	2,	6,
3,	5,	4,	2,	8,
0,	3,	3,	6,	10,
3,	7,	5,	4,	5,
4,	4,	3,	4,	8,
2,	5,	2,	5,	10,
3,	8,	2,	3,	7,
2,	3,	4,	2,	10,
5,	3,	3,	3,	7,
1,	3,	4,	5,	9,
1,	4,	1,	7,	9), nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"),
Group= c("AG Poster 1", "AG Poster 2","AG Poster 3","AG Poster 4a","AG Poster 4b",
"SS Poster 1", "SS Poster 2", "SS Poster 3", "SS Poster 4a", "SS Poster 4b",
"NHP Poster 1", "NHP Poster 2", "NHP Poster 3", "NHP Poster 4a", "NHP Poster 4b")))


loglm(~Response+Group,data=Q5_tab) #43.64238


Q5_tab2<-matrix(c(margin.table(Q5_tab[,(1:5)],mar=1),
margin.table(Q5_tab[,(6:10)],mar=1), margin.table(Q5_tab[,(11:15)],mar=1)),nrow=5,
dimnames=list(Response=c("Not Clear","Took a while", "Can be misinterpreted", "More information need", "Message Clear"),
Group=c("Agronomy","Soil Scientist", "Nutrition/Health")))



loglm(~Response+Group,data=Q5_tab2) #2.353286 

loglm(~Response+Group,data=Q5_tab[,(1:5)])#11.99458 
loglm(~Response+Group,data=Q5_tab[,(6:10)])#13.68950 
loglm(~Response+Group,data=Q5_tab[,(11:15)]) #15.60502

43.64238

2.353286+11.99458+13.68950+15.60502


##########################################################################
##########################################################################

#################################
# Understanding Uncertainty     #
#################################


# Question 6 Do you think that the poster helped you to understand the uncertainty in the predictions?



Q6_tab<-matrix(c(
5,	5,
8,	1,
7,	3,
7,	2,
11,	1,
15,	6,
20,	2,
15,	8,
20,	4,
18,	5,
13,	10,
20,	3,
12,	11,
21,	3,
21,	3), nrow=2,
dimnames=list(Response=c("Yes","No"), 
Group= c("AG Poster 1", "AG Poster 2","AG Poster 3","AG Poster 4a","AG Poster 4b",
"SS Poster 1", "SS Poster 2", "SS Poster 3", "SS Poster 4a", "SS Poster 4b",
"NHP Poster 1", "NHP Poster 2", "NHP Poster 3", "NHP Poster 4a", "NHP Poster 4b")))


loglm(~Response+Group,data=Q6_tab) #28.08621


Q6_tab2<-matrix(c(margin.table(Q6_tab[,(1:5)],mar=1),
margin.table(Q6_tab[,(6:10)],mar=1), margin.table(Q6_tab[,(11:15)],mar=1)),nrow=2,
dimnames=list(Response=c("Yes","No"),
Group=c("Agronomy","Soil Scientist", "Nutrition/Health")))


loglm(~Response+Group,data=Q6_tab2) #0.3914897 

loglm(~Response+Group,data=Q6_tab[,(1:5)])#6.330010 
loglm(~Response+Group,data=Q6_tab[,(6:10)])#5.471193
loglm(~Response+Group,data=Q6_tab[,(11:15)]) #15.89351 

28.08621
0.3914897 +6.330010 +5.471193+15.89351 

##########################################################################
##########################################################################
##########################################################################


# Analysis of Q7: evidence against null hypothesis of random ranking



f1<-function(n,k,r){
X<-12*n/(k*(k+1))*sum((r-((k+1)/2))^2)
p<-1-pchisq(X,(k-1))
op<-list("X"=X,"P.value"=p)
return(op)
}

 
# 1.All respondents

# poster 1 - 2.84
# poster 2-  3.27
# poster 3 - 2.32
# poster 4a -3.16
# poster 4b -3.41

r<-c(2.84,3.27,2.32,3.16,3.41)

f1(56,5,r) 

# X - 16.90304  
# P.value - 0.002018613

################################
################################

# 2. Location of Meeting

# Malawi 

# poster 1 - 2.65
# poster 2-  3.35
# poster 3 - 2.43
# poster 4a -3.09
# poster 4b -3.48

r<-c(2.65,3.35,2.43,3.09,3.48)

f1(23,5,r) 

# X - 7.43728
# P.value - 0.114507


# Ethiopia 

# poster 1 - 3.27
# poster 2-  3.64
# poster 3 - 2.85
# poster 4a -3.39
# poster 4b -3.85

r<-c(3.27,3.64,2.85,3.39,3.85)
f1(33,5,r)

# X-18.21072
# P.value-0.001122391

###########################
###########################

# Professional Group 

# Agronomist 


# poster 1 - 2.60
# poster 2-  2.90
# poster 3 - 2.30
# poster 4a -3.70
# poster 4b -3.50

r<-c(2.60,2.90,2.30,3.70,3.50)
f1(10,5,r)

# X-5.6
# P.value-0.2310782


# Soil Scientist 


# poster 1 - 3.26
# poster 2-  3.39
# poster 3 - 2.39
# poster 4a -2.74
# poster 4b -3.22

r<-c(3.26,3.39,2.39,2.74,3.22)
f1(23,5,r)

#X - 6.51176
#P.value- 0.1640509


# Nutritionists mean rank 2.52,3.30,2.26,3.35,3.57


# poster 1 - 2.52
# poster 2-  3.30
# poster 3 - 2.26
# poster 4a -3.35
# poster 4b -3.57

r<-c(2.52,3.30,2.26,3.35,3.57) 
f1(23,5,r)

# X - 12.10168
# P.value - 0.01661095


##########################################################################
##########################################################################

# End of Script 


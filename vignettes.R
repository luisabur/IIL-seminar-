# script for calculating D-coefficient for my vignettes
# LB, 24.11.23

#install.packages('AlgDesign')
library(AlgDesign)

#install.packages('conf.design')
library(conf.design)

#generate matrix 

#all levels 
datl<-gen.factorial(c(2,3,2,2,2,2,2,2),varNames=c("Specificity","Level","Training","Recency","Mobility","Leadership","Class","Gender"))
#without leadership
datwl<-gen.factorial(c(2,3,2,2,2,2,2),varNames=c("Specificity","Level","Training","Recency","Mobility","Class","Gender"))
#with class as 4 levels
datc<-gen.factorial(c(2,3,2,2,2,4,2),varNames=c("Specificity","Level","Training","Recency","Mobility","Class","Gender"))

#2-way interactions 
#rep(8,10) means 10 blocks with 8 vignettes 
#leadership (8 Dimensions)
b_nrl<-optBlock(~.^2, datl, rep(8,20),criterion="D", nR=100) 
eval.blockdesign(~(.)^2,b_nrl$design,rep(8,20))
eval.blockdesign(~(.)^2,b_nrl$design,rep(8,20), confounding=TRUE)

# without leadership (7 Dimensions)
b_nrwl<-optBlock(~.^2, datwl, rep(8,16),criterion="D", nR=100) 
b_nrwl
eval.blockdesign(~(.)^2,b_nrwl$design,rep(8,10))
eval.blockdesign(~(.)^2,b_nrwl$design,rep(8,16), confounding=TRUE)

# without leadership but with 4 level class (7 Dimensions)
b_nrc<-optBlock(~.^2, datc, rep(8,14),criterion="D", nR=100) 
b_nrc
eval.blockdesign(~(.)^2,b_nrc$design,rep(8,14))
eval.blockdesign(~(.)^2,b_nrc$design,rep(8,14), confounding=TRUE)


#3-way interactions 
#leadership (8 Dimensions)
b_nrl<-optBlock(~.^3, datl, rep(8,10),criterion="D", nR=100) 
b_nrl
eval.blockdesign(~(.)^3,b_nrl$design,rep(8,10))
eval.blockdesign(~(.)^3,b_nrl$design,rep(8,16), confounding=TRUE)

# without leadership (7 Dimensions)
b_nrwl<-optBlock(~.^3, datwl, rep(8,10),criterion="D", nR=100) 
b_nrwl
eval.blockdesign(~(.)^3,b_nrwl$design,rep(8,10))
eval.blockdesign(~(.)^3,b_nrwl$design,rep(8,16), confounding=TRUE)

# without leadership but with 4 level class (7 Dimensions)
b_nrc<-optBlock(~.^3, datc, rep(8,10),criterion="D", nR=100) 
b_nrc
eval.blockdesign(~(.)^3,b_nrc$design,rep(8,10))
eval.blockdesign(~(.)^3,b_nrc$design,rep(8,16), confounding=TRUE)




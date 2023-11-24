#install.packages('AlgDesign')
library(AlgDesign)

#install.packages('conf.design')
library(conf.design)

#generate matrix 

#all levels 
datl<-gen.factorial(c(2,3,2,2,2,2,2,2),varNames=c("Specificity","Level","Training","Recency","Mobility","Leadership","Class","Gender"))
#without leadership
datwl<-gen.factorial(c(2,3,2,2,2,2,2),varNames=c("Specificity","Level","Training","Recency","Mobility","Class","Gender"))
#without class 
datac<-gen.factorial(c(2,3,2,2,2,4,2),varNames=c("Specificity","Level","Training","Recency","Mobility","Class","Gender"))

#2-way interactions 
b_nr<-optBlock(~.^2, dat, rep(8,20),criterion="D", nR=100) 
eval.blockdesign(~(.)^2,b_nr$design,rep(8,16))
eval.blockdesign(~(.)^2,b_nr$design,rep(8,16), confounding=TRUE)

#3-way interactions 
eval.blockdesign(~.^2,b_nr$design,c(8,8,8,8,8,8,8,8,8))
eval.blockdesign(~(.)^2,b_nr$design,rep(8,8),confounding=TRUE)
bk<-data.matrix(b_nr$Blocks$B1)
t(bk)%*%bk
bk<-data.matrix(b_nr$design)
t(bk)%*%bk






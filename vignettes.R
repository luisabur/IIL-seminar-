install.packages('AlgDesign')
library(AlgDesign)

install.packages('conf.design')
library(conf.design)


dat<-gen.factorial(c(2,3,2,2,2,2,2),varNames=c("Specificity","Level","Training", "Recency", "Class", "Gender", "Age"))
b_nr<-optBlock(~.^2, dat,c(8,8,8,8,8,8,8,8,8),criterion="D", nR=100)
eval.blockdesign(~.^2,b_nr$design,c(8,8,8,8,8,8,8,8,8))
eval.blockdesign(~(.)^2,b_nr$design,rep(8,8),confounding=TRUE)
bk<-data.matrix(b_nr$Blocks$B1)
t(bk)%*%bk
bk<-data.matrix(b_nr$design)
t(bk)%*%bk






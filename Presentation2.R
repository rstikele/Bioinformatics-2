a=read.csv("data1.csv", header = T)
attach(a)
names(a)


as.factor(size1)
size12<-factor(size1,levels=c("10","20","30","40",">50"))

library(car)
library(ggplot2)
library(pwr)
library(agricolae)

sample.size.a= aggregate(.~size1, data=a, length)
sample.size.a=sample.size.a[order(sample.size.a$size1),]

#Step 1, illustrates that dataset 1 is visually non-linear
boxplot(rich1~size12,xlab="Sizes",ylab="Species", main = "dataset 1")

out11=aov(rich1~size12)
summary(out11) #Size is significantly different among samples
TukeyHSD(x=out11, 'size12', conf.level = 0.95) #Shows which sizes are significantly different from each other

qqPlot(out11$resid,ylab="Z value", main = "dataset 1") #NPP test, looks pretty normal, but there is some slight tailing at the ends
shapiro.test(out11$resid) #Results indicate that the data is normal

par(mfrow = c(2, 2))  
plot(out11, main = "dataset 1") #Everything looks about normal, but scale-location looks a little non-linear. That test assumes homoscedasticity, and if the assumption is met, then the line is flat, but if it is not met, then the line will be curved some way.

bartlett.test(rich1~size12)#Results indicate unequal variance among samples

anova(out11) #Confirms that there is a difference between the groups

out11.mod = data.frame(Fitted = fitted(out11), Residuals = resid(out11), Treatment = size12)
ggplot(out11.mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point() #Confirms visually variation diffences among samples by their visual spread from each other

oneway.test(rich1~size12, var.equal = F) #Final test based on stat tree, Welch's

square.between= function(x) (mean(x) - grand.mean.a)^2
group.mean.a<- aggregate(.~size1, data=a, mean)
as.factor(group.mean.a$size1)
group.mean.a$size1<-factor(group.mean.a$size1,levels=c("10","20","30","40",">50"))
group.mean.a[order(group.mean.a$size1), ]

grand.mean.a<- sum(group.mean.a[,2]*sample.size.a[,2])/sum(sample.size.a[,2])



ss.between=sum(sample.size.a[,2]*aggregate(.~size1,data=a,square.between)[,2])

square.within= function(x) sum((x-mean(x))^2)
ss.within= sum(aggregate(.~size1, data=a, square.within)[,2])
ss.total= ss.between + ss.within

N=sum(sample.size.a[,2])
k=5

cohen.f= sqrt(ss.between/N/ (ss.within/(N-k))) #Standardized by degrees of freedom N-k


#SS from anova
output1=lm(rich1~size12)
SSG=anova(output1)[1,2]
SSE=anova(output1)[2,2]
cohen.f=SSG/SSE

cohen.f2=sqrt((SSG/N) / (SSE/(N-k))) #Equivalent to by hand
pwr.anova.test(k=5, n= 20, f=cohen.f, sig.level = 0.05, power = NULL)

#Sqrt Transformation, does not fix unequal variance
dev.off()
sqrtrich1<-sqrt(rich1)
boxplot(sqrtrich1~size12,xlab="Sizes",ylab="Species", main = "sqrt dataset 1") #Still visually non-linear

out11sqrt=aov(sqrtrich1~size12)
summary(out11sqrt) #Size is still significantly different among samples
TukeyHSD(x=out11sqrt, 'size12', conf.level = 0.95) #Shows which sizes are significantly different from each other

qqPlot(out11sqrt$resid,ylab="Z value") #NPP test, looks pretty normal, but the tranformation created an outlier
shapiro.test(out11sqrt$resid) #Results indicate that the data is normal; p>0.05 is a normal dataset

par(mfrow = c(2, 2))  
plot(out11sqrt) #Everything looks about normal, but scale-location looks a little non-linear. That test assumes homoscedasticity, and if the assumption is met, then the line is flat, but if it is not met, then the line will be curved some way.
#QQplot now has 2 outliers
#Gaps between variances are objectively smaller though

bartlett.test(sqrtrich1~size12) #p-value less than 0.05 means unequal variance
#Unequal variance here, still

oneway.test(sqrtrich1~size12, var.equal = F) #Final test based on stat tree, Welch's


#True mean is greater than zero
pairwise.t.test(rich1,size12,p.adj="bonf")#Good way to see pairwise differences by group on raw data
pairwise.t.test(sqrtrich1,size12,p.adj="bonf") #Good way to see pairwise differences by group on transformed data
dev.off()

out1<-HSD.test(aov(rich1~size12),"size12",group=T,console=T) #Compare sig differences between raw and transformed data
sqrtout1<-HSD.test(aov(sqrtrich1~size12),"size12",group=T,console=T)
SNK.test(aov(rich1~size12),"size12",group=T,console=T)
SNK.test(aov(sqrtrich1~size12),"size12",group=T,console=T)

output2=lm(sqrtrich1~size12)
SSG2=anova(output2)[1,2]
SSE2=anova(output2)[2,2]
cohen.f3=SSG2/SSE2
pwr.anova.test(k=5, n= 20, f=cohen.f3, sig.level = 0.05, power = NULL)
#Experimental test results are very strong



#For dataset 2 untransformed
b=read.csv("data2.csv", header = T)
attach(b)
names(b)


as.factor(size2)
size22<-factor(size2,levels=c("10","20","30","40",">50"))

sample.size.b= aggregate(.~size2, data=b, length)
sample.size.b=sample.size.b[order(sample.size.b$size2),]

boxplot(rich2~size22,xlab="Sizes",ylab="Species", main = "dataset 2")

out22=aov(rich2~size22)
summary(out22)

qqPlot(out22$resid,ylab="Z value", main = "dataset 2")
shapiro.test(out22$resid)

par(mfrow = c(2, 2))  
plot(out22, main = "dataset 2")

bartlett.test(rich2~size22) #p-value less than 0.05 means unequal variance

summary(aov(lm(rich2~size22)))#ANOVA on dataset 2, Final test

out22.mod = data.frame(Fitted = fitted(out22), Residuals = resid(out22), Treatment = size22)
ggplot(out22.mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point()

square.between= function(x) (mean(x) - grand.mean.b)^2
group.mean.b<- aggregate(.~size2, data=b, mean)
as.factor(group.mean.b$size2)
group.mean.b$size2<-factor(group.mean.b$size2,levels=c("10","20","30","40",">50"))
group.mean.b[order(group.mean.b$size2), ]

grand.mean.b<- sum(group.mean.b[,2]*sample.size.b[,2])/sum(sample.size.b[,2])



ss.between=sum(sample.size.b[,2]*aggregate(.~size2,data=b,square.between)[,2])

square.within= function(x) sum((x-mean(x))^2)
ss.within= sum(aggregate(.~size2, data=b, square.within)[,2])
ss.total= ss.between + ss.within

N=sum(sample.size.b[,2])
k=5

cohen.f= sqrt(ss.between/N/ (ss.within/(N-k))) #Standardized by degrees of freedom N-k


#SS from anova
output1=lm(rich2~size22)
SSG=anova(output1)[1,2]
SSE=anova(output1)[2,2]
cohen.f=SSG/SSE

cohen.f2=sqrt((SSG/N) / (SSE/(N-k))) #Equivalent to by hand

#Dataset 2 Transformed, sqrt

dev.off()
sqrtrich2<-sqrt(rich2)
boxplot(sqrtrich2~size22,xlab="Sizes",ylab="Species", main = "sqrt dataset 2") #Visually linear

out22sqrt=aov(sqrtrich2~size22)
summary(out22sqrt) #Size is still significantly different among samples
TukeyHSD(x=out22sqrt, 'size22', conf.level = 0.95) #Shows which sizes are significantly different from each other

qqPlot(out22sqrt$resid,ylab="Z value") #NPP test, normal
shapiro.test(out22sqrt$resid) #Results indicate that the data is normal; p>0.05 is a normal dataset

par(mfrow = c(2, 2))  
plot(out22sqrt) #Everything looks about normal, but scale-location looks a little non-linear. That test assumes homoscedasticity, and if the assumption is met, then the line is flat, but if it is not met, then the line will be curved some way. This is homoscedastic.
#Gaps between variances are objectively smaller though

bartlett.test(sqrtrich2~size22) #p-value less than 0.05 means unequal variance
#Equal variance here

#ANOVA test is the final test to run given the stat tree
summary(lm(aov(sqrtrich2~size22)))

pairwise.t.test(rich2,size22,p.adj="bonf") #Good way to see pairwise differences by group on raw data
pairwise.t.test(sqrtrich2,size22,p.adj="bonf")  #Good way to see pairwise differences by group on transformed data

out2<-HSD.test(aov(rich2~size22),"size22",group=T,console=T)  #Compare sig differences between raw and transformed data
sqrtout2<-HSD.test(aov(sqrtrich2~size22),"size22",group=T,console=T)
SNK.test(aov(rich2~size22),"size22",group=T,console=T) 
SNK.test(aov(sqrtrich2~size22),"size22",group=T,console=T)

output3=lm(sqrtrich2~size22)
SSG3=anova(output3)[1,2]
SSE3=anova(output3)[2,2]
cohen.f4=SSG3/SSE3
pwr.anova.test(k=5, n= 20, f=cohen.f4, sig.level = 0.05, power = NULL)
#Experimental test results are very strong

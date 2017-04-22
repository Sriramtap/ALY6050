#1
f1 <- read.csv("D:\\NEU\\ALY6050\\Project1.csv", header = TRUE)
#Descriptive Statistic of the weights
summary(f1[8])

#or we could use the psych library by installing in the R package using the 

library(psych)

describe(f1[8])
#Histogram

hist(f1$Weight, xlab = "Weight", main = "Histogram of Weights", freq = F)

# to compare the reulatant histogram in order to determine which distribution 
#it follows.

lines(density(f1$Weight), col = "blue")

#Lineplot

Weight <- f1$Weight..grams.
breaks <- seq(46.52, 49.44, by = 0.43)
Weight.cut <- cut(Weight, breaks, right = FALSE)
Weight.freq <- table(Weight.cut)
Weight.cumfreq <- cumsum(Weight.freq)
Weight.relCumFreq <- Weight.cumfreq/ nrow(f1)

plot(Weight.relCumFreq, type = "o", col = "blue")

#2

#Method 1 to find out descriptive statistics

summary(f1$Total)

#Method 2 to find out descriptive statistics

library(psych)
describe(f1$Total)

#Histogram
hist(f1$Total, xlab = "Total candies", col = "blue", main = "Histogram of total", freq = F)
lines(density(f1$Total),col="red")

#Line
Total <- f1$Total
breaks <- seq(51, 63, by = 1.6)
Total.cut <- cut(Total, breaks, right = FALSE)
Total.freq <- table(Total.cut)
Total.cumfreq <- cumsum(Total.freq)
Total.relCumFreq <- Total.cumfreq/ nrow(f1)

plot(Total.relCumFreq, type = "o", col = "blue")

#3
boxplot(f1$Red)
boxplot(f1$Orange)
boxplot(f1$Yellow)
boxplot(f1$Green)
boxplot(f1$Blue)
boxplot(f1$Brown)

#4

prop.table(table(f1$Defective.Total))

TotlalDefectivePercent <- (f1$Defective.Total/f1$Total)*100
TotlalDefectivePercent
#Summary
describe(TotlalDefectivePercent)

#Histogram
hist(TotlalDefectivePercent, xlab = "Total Defective Percentage", col = "blue", main = "Histogram of total defective percentage", freq = F)
lines(density(TotlalDefectivePercent),col="red")
mean(f1$Weight..grams.)
#Cumulative relative frequency line plot
breaks <- seq(0, 17.54, by = 2.7)
TotlalDefectivePercent.cut <- cut(TotlalDefectivePercent, breaks, right = FALSE)
TotlalDefectivePercent.freq <- table(TotlalDefectivePercent.cut)
TotlalDefectivePercent.cumfreq <- cumsum(TotlalDefectivePercent.freq)
TotlalDefectivePercent.relCumFreq <- TotlalDefectivePercent.cumfreq/ nrow(f1)
plot(TotlalDefectivePercent.relCumFreq, type = "o", col = "blue")

#6


chisq.test(f1$Weight..grams.)
#since P value is greater than the level of significance 0.5 so null is not rejected

#7
#Random sampling
r1<- f1[sample(nrow(f1), 30), ]
r2<- f1[sample(nrow(f1), 30), ]
r3<- f1[sample(nrow(f1), 30), ]
r4<- f1[sample(nrow(f1), 30), ]
r5<- f1[sample(nrow(f1), 30), ]
r6<- f1[sample(nrow(f1), 30), ]
r7<- f1[sample(nrow(f1), 30), ]
r8<- f1[sample(nrow(f1), 30), ]
r9<- f1[sample(nrow(f1), 30), ]
r10<-f1[sample(nrow(f1), 30), ]
r11<-f1[sample(nrow(f1), 30), ]
r12<-f1[sample(nrow(f1), 30), ]
r13<-f1[sample(nrow(f1), 30), ]
r14<-f1[sample(nrow(f1), 30), ]
r15<-f1[sample(nrow(f1), 30), ]
r16<-f1[sample(nrow(f1), 30), ]
r17<-f1[sample(nrow(f1), 30), ]
r18<-f1[sample(nrow(f1), 30), ]
r19<-f1[sample(nrow(f1), 30), ]
r20<-f1[sample(nrow(f1), 30), ]
#Finding mean of the samples

m1<-mean(r1$Defective.Total)
m2<-mean(r2$Defective.Total)
m3<-mean(r3$Defective.Total)
m4<-mean(r4$Defective.Total)
m5<-mean(r5$Defective.Total)
m6<-mean(r6$Defective.Total)
m7<-mean(r7$Defective.Total)
m8<-mean(r8$Defective.Total)
m9<-mean(r9$Defective.Total)
m10<-mean(r10$Defective.Total)
m11<-mean(r11$Defective.Total)
m12<-mean(r12$Defective.Total)
m13<-mean(r13$Defective.Total)
m14<-mean(r14$Defective.Total)
m15<-mean(r15$Defective.Total)
m16<-mean(r16$Defective.Total)
m17<-mean(r17$Defective.Total)
m18<-mean(r18$Defective.Total)
m19<-mean(r19$Defective.Total)
m20<-mean(r20$Defective.Total)
#Equating to a table
t1 <-table(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20)
library(xlsx)
#Writing the data into an csv
write.csv(t1, "D:\\NEU\\ALY6050\\P1.csv",row.names=FALSE)
#Reading it back
vf <- read.csv("D:\\NEU\\ALY6050\\p1.csv")
#There is a collumn named Freq which I think is a ghost value
vf$Freq <-NULL
vf
#Transposing the collumn to row inorder to find the histogram
vf <-t(vf)
vf
hist(vf, xlab = "Total Defective which are greater than 0", col = "blue", main = "frequency histogram of the sample means", freq = F)
lines(density(vf),col="red")

#varience
v1<-var(r1$Defective.Total)
v2<-var(r2$Defective.Total)
v3<-var(r3$Defective.Total)
v4<-var(r4$Defective.Total)
v5<-var(r5$Defective.Total)
v6<-var(r6$Defective.Total)
v7<-var(r7$Defective.Total)
v8<-var(r8$Defective.Total)
v9<-var(r9$Defective.Total)
v10<-var(r10$Defective.Total)
v11<-var(r11$Defective.Total)
v12<-var(r12$Defective.Total)
v13<-var(r13$Defective.Total)
v14<-var(r14$Defective.Total)
v15<-var(r15$Defective.Total)
v16<-var(r16$Defective.Total)
v17<-var(r17$Defective.Total)
v18<-var(r18$Defective.Total)
v19<-var(r19$Defective.Total)
v20<-var(r20$Defective.Total)
#loading the varience frequency into a table
t2 <- table(v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12,v13,v14,v15,v16,v17,v18,v19,v20)
t2
#writing the table into a csv
write.csv(t2, "D:\\NEU\\ALY6050\\p1.csv")
#reading it back
ta <- read.csv("D:\\NEU\\ALY6050\\p1.csv")
ta
#Deleting the frequency collumn
ta$Freq <- NULL
ta
#Transposing the data
ta <-t(ta)
ta
#Finding histogram
hist(ta, xlab = "Total Defective which are greater than 0", col = "blue", main = "frequency histogram of the sample variances", freq = F)
lines(density(ta),col="red")
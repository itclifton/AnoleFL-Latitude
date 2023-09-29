

## Packages and Functions ----
library(ggplot2)
st.err = function(x) {sd(x)/sqrt(length(x))}

## Data Management ----
data1<-read.csv("AnoleLatitudePhenotypicPlasticity.csv")
data1<-subset(data1, Treatment=="PP")

data1$ChangeCTmax<-data1$CTmax.2-data1$CTmax
aggregate(ID~Sex+Latitude, length, data=data1)

## Explorations ----
aggregate(SVL~Sex+Latitude, mean, data=data1)

aggregate(CTmax~Latitude, mean, data=data1)
aov1<-aov(CTmax~Latitude, data=data1)
summary(aov1)
TukeyHSD(aov1)
aggregate(CTmax.2~Latitude, mean, data=data1)

aggregate(ChangeCTmax~Latitude, mean, data=data1)
aggregate(ChangeCTmax~Latitude, st.err, data=data1)
aov2<-aov(ChangeCTmax~Latitude, data=data1)
summary(aov2)
TukeyHSD(aov2)

aggregate(Pant~Latitude, mean, data=data1)
aggregate(Pant.2~Latitude, mean, data=data1)

aggregate(ChangeCTmax~Sex+Latitude, mean, data=data1)
aggregate(CTmax.2~Sex+Latitude, mean, data=data1)

aggregate(Pant~Sex+Latitude, mean, data=data1)
aggregate(Pant.2~Sex+Latitude, mean, data=data1)



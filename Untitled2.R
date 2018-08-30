x <- prep_cellphone_brand$scoreNN 
h<-hist(x, breaks=1000, col="red", xlab="Miles Per Gallon", 
        main="Histogram with Normal Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

headphone_3$scoreNN <- headphone_3$scoreNN + 5
headphone_3$scoreNN <- log(headphone_3$scoreNN)

d <- density(headphone_5$scoreNN) # returns the density data 
d <- density(headphone_3$scoreNN) # returns the density data 

plot(d)

headphone_3 <- prep_cellphone_brand %>% filter(overall == 3) %>% select(scoreNN)
headphone_5 <- prep_cellphone_brand %>% filter(overall == 5) %>% select(scoreNN)

test.score <- headphone_1$scoreNN
summary(test.score)
ks.test(test.score,y='pnorm',alternative='two.sided')



ks.test(headphone_3)

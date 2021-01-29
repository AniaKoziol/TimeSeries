df <- read.table("WIG20.mst", header = T, sep= ",")
df <- df[ which(df$X.DTYYYYMMDD.>= 20190101),]
attach(df)

#### podzial na czesc testowa dla przyszlych cen akcji #####
test <- df[ ( (nrow(df)-4) : nrow(df)),]
tren <- df[ 1: (nrow(df)-5) , ]
test <- test[,6]
tren <- tren[,6]

#### zrobienie obiektoru klasy ts#######
szereg <- ts(df$X.CLOSE. ) 
ts.plot(szereg)
# c)  
Ntr <- length(df$X.CLOSE.)-5
Ntest <- 5
n <- Ntr+Ntest
znext <- ts(szereg[(Ntr+1):n],start=Ntr+1,end=n)
zt <- ts(szereg[1:Ntr],start=1,end=Ntr) 
ts.plot(zt)
acf(zt) # maleje 
#pacf(zt) #oscyluja wokol zera

# d) 

zroznicowany_szereg <- diff(zt )
pacf(zroznicowany_szereg)
acf(zroznicowany_szereg)

# e) 
# mozemy wnioskowac pacf -> AR(0)
# mozemy wnioskowac acf -> MA(0)
model_ar <- ar(zroznicowany_szereg,aic=TRUE,method = "mle", order.max=NULL)
model_ar #wybrany model: AR(0)
modelar1 <- arima(zroznicowany_szereg, order = c(0,0,0))
Box.test(modelar1$resid,lag=10,type="Ljung") #p-value 0.775 -> bialy szum 

# f)
aic <- matrix(0,6,6)
bic <- matrix(0,6,6)

aic_min <- Inf
bic_min <- Inf

i_aic_min <--1
j_aic_min <--1

i_bic_min <--1
j_bic_min <--1

for(i in 0:5)
{
  for(j in 0:5)
  {
    cat("                        \r")
    cat("i=",i,"j=",j)
    aic[i+1,j+1] <- AIC(arima(zroznicowany_szereg,c(i,0,j),method = "ML",optim.control = list(maxit=10^5))) #ML bo f wiarygodnosci
    if(aic[i+1,j+1]<aic_min)
    {
      aic_min <- aic[i+1,j+1]
      i_aic_min <-i
      j_aic_min <-j
    }
    bic[i+1,j+1] <- AIC(arima(zroznicowany_szereg,c(i,0,j),method = "ML",optim.control = list(maxit=10^5)),k=log(Ntr))
    if(bic[i+1,j+1]<bic_min)
    {
      bic_min <- bic[i+1,j+1]
      i_bic_min <-i
      j_bic_min <-j
    }
  }
}

i_aic_min  
j_aic_min 

i_bic_min  
j_bic_min


arma_AIC <- arima(zroznicowany_szereg,c(4,0,4), method="ML")
arma_BIC <- arima(zroznicowany_szereg,c(0,0,0),  method="ML")

# wybieramy model 
Box.test(arma_AIC$resid,lag=10,type="Ljung")  #0.991 bialy szum
Box.test(arma_BIC$resid,lag=10,type="Ljung") #0.775 bialy szum 




n.ahead <- Ntest
predBIC<-predict(arma_BIC,n.ahead=n.ahead)$pred
seBIC<-predict(arma_BIC,n.ahead=n.ahead)$se
plot(NULL,xlim=c(0.98*Ntr,n),ylim=c(-70,70),xlab="t",ylab="z_diff",main="Predykcja BIC") 
lines((0.98*Ntr):(Ntr-1),zroznicowany_szereg[(0.98*Ntr):(Ntr-1)],lty=1,col="black") 
lines((Ntr-1):(n-1),c(zroznicowany_szereg[Ntr-1],diff(c(zt[Ntr],znext))),lty=1,col="blue") 
lines((Ntr-1):(n-1),c(zroznicowany_szereg[Ntr-1],predBIC),lty=3,col="black") 
lines((Ntr-1):(n-1),c(zroznicowany_szereg[Ntr-1],predBIC+2*seBIC),lty=3,col="red") 
lines((Ntr-1):(n-1),c(zroznicowany_szereg[Ntr-1],predBIC-2*seBIC),lty=3,col="red") 


predAIC<-predict(arma_AIC,n.ahead=n.ahead)$pred
seAIC<-predict(arma_AIC,n.ahead=n.ahead)$se
plot(NULL,xlim=c(0.98*Ntr,n),ylim=c(-78,78),xlab="t",ylab="z_diff",main="Predykcja AIC") 
lines((0.98*Ntr):(Ntr-1),zroznicowany_szereg[(0.98*Ntr):(Ntr-1)],lty=1,col="black") 
lines((Ntr-1):(n-1),c(zroznicowany_szereg[Ntr-1],diff(c(zt[Ntr],znext))),lty=1,col="blue") 
lines((Ntr-1):(n-1),c(zroznicowany_szereg[Ntr-1],predAIC),lty=3,col="black") 
lines((Ntr-1):(n-1),c(zroznicowany_szereg[Ntr-1],predAIC+2*seAIC),lty=3,col="red") 
lines((Ntr-1):(n-1),c(zroznicowany_szereg[Ntr-1],predAIC-2*seAIC),lty=3,col="red")
#granatowa linia to co co na prawde
#czerwone to przedzial ufnosci
#czarne przerywane to predycja 

# wybieram model arma_AIC i rysujê predykcjê nastepnych 15 obserwacji 
n.ahead <- 15
predAIC<-predict(arma_AIC,n.ahead=n.ahead)$pred
seAIC<-predict(arma_AIC,n.ahead=n.ahead)$se

plot(NULL,xlim=c(0.98*Ntr,n+10),ylim=c(-78,78),xlab="t",ylab="z_diff",main="Predykcja AIC") 
lines((0.98*Ntr):(Ntr-1),zroznicowany_szereg[(0.98*Ntr):(Ntr-1)],lty=1,col="black") 
lines((Ntr-1):(n-1),c(zroznicowany_szereg[Ntr-1],diff(c(zt[Ntr],znext))),lty=1,col="blue") 
lines((Ntr-1):(n-1+10),c(zroznicowany_szereg[Ntr-1],predAIC),lty=3,col="black") 
lines((Ntr-1):(n-1+10),c(zroznicowany_szereg[Ntr-1],predAIC+2*seAIC),lty=3,col="red") 
lines((Ntr-1):(n-1+10),c(zroznicowany_szereg[Ntr-1],predAIC-2*seAIC),lty=3,col="red")


### kolejne 10 notowañ dla spó³ek ##

ostatnia_znana <- test[length(test)]
notowanie <- numeric(10)
for( i in 1:10){
notowanie[i] <- ostatnia_znana + predAIC[5+i] 
ostatnia_znana <- notowanie[i]
}
notowanie 



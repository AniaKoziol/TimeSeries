dane<- read.csv2("file:///C:/Users/Ania/Desktop/Studia-SMAD/szeregi czasowe/metr.csv",h=T,sep=";")
head(dane)

t <- seq(0.1,10,0.01) # ta³
x <- dane$x
ph <- numeric(length(t)) #fi 

for(i in 1:length(t)){
  ph[i] <- sum(cos(2*pi*t[i]*x))
}
head(ph)
q <- 1/(t[which.max(ph)]) # q=1/ta³ 
q









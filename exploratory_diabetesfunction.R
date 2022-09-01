data = diabetes_function
data
B=data$Outcome
A=data$`Diabetes Function`
nrow(data) #banyak baris data diabetes
ncol(data) #banyak kolom data diabetes
dim(data) #dimensi data diabetes, terdiri atas 768 baris dan 2 kolom
d.fac <- factor(B, levels=0:1, labels = c("No","Yes"))
d.fac
data[100,1] #Diabetes pedigree function pasien ke 100
P=data.frame(data[d.fac =="No",1])
P

nrow(P)
Q=data.frame(data[d.fac =="Yes",1])
Q
nrow(Q)
#Mean, median dan trimmed mean untuk pasien tidak terdeteksi diabetes (No)
mean(P$Diabetes.Function)
mean(P$Diabetes.Function,trim=.10)
median(P$Diabetes.Function)
#Mean, median dan trimmed mean untuk pasien terdeteksi diabetes (Yes)
mean(Q$Diabetes.Function)
mean(Q$Diabetes.Function,trim=.10)
median(Q$Diabetes.Function)

#Ukuran penyebaran untuk pasien tidak terdeteksi diabetes (No)
sd(P$Diabetes.Function)
IQR(P$Diabetes.Function)
IQR(P$Diabetes.Function)/1.349
mad(P$Diabetes.Function)

#Ukuran penyebaran untuk pasien terdeteksi diabetes (Yes)
sd(Q$Diabetes.Function)
IQR(Q$Diabetes.Function)
IQR(Q$Diabetes.Function)/1.349
mad(Q$Diabetes.Function)

##Korelasi gatau. Beda jumah data jadi gabisa dikurangi
meanNo <- apply(data[d.fac=="No",], 1, mean)
meanNo
meanYes <- apply(data[d.fac=="Yes",], 1, mean) 
meanYes
o <- order(abs(meanYes-meanNo), decreasing=TRUE) 
biomarkers50 = data[o[1:50],] 
dim(biomarkers50)

#data pasien non-diabetes setelah diurutkan
oP<-order(P,decreasing = TRUE)
oP
#data pasien diabetes setelah diurutkan
oQ<-order(Q,decreasing = TRUE)


datafactortable<-table(d.fac)
datafactortable
d.fac
#Pie chart
percentLabels <- round(100*datafactortable/sum(datafactortable), 1)
percentLabels
d.fac

pielabels<-paste(percentLabels,"%", sep="")
pie(datafactortable, main="Kategori pasien diabetes", cex=0.8, col=topo.colors(2), labels=pielabels)
legend("right", c("No", "Yes"), text.col = "red", cex=0.8, fill=topo.colors(2))

#scatter plot
plot(A, pch=19, cex.lab=1.5, xlab="Nomor pasien", 
     ylab="Diabetes Pedigree Function", col="blue")
#strip chart
stripchart(A~d.fac, method="jitter",
           cex.lab=1.5, vertical=TRUE, col=c("red","darkgreen"),
           xlab="Deteksi", ylab="Diabetes Pedigree Function")
#histogram
hist(P$Diabetes.Function, cex.lab=1, main="Pasien tidak terdeteksi diabetes", xlab="Diabetes Pedigree Function", col="yellow")
hist(Q$Diabetes.Function, cex.lab=1, main="Pasien terdeteksi diabetes", xlab="Diabetes Pedigree Function", col="green")
#boxplot
boxplot(A~d.fac, cex.lab=0.8, main="Pasien yang diperiksa", xlab="deteksi", ylab="Diabetes Pedigree Function", col=c("purple","orange"))

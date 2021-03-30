#Mata Kuliah  : Topik dalam Pengenalan Pola
#Tugas        : Praktikum
#Topik        : Klasifikasi Bayes

#Nama         : Hery Wibowo
#NIM          : G6501201012

library(EBImage)
x <- readImage(system.file('images','shapes.png', package='EBImage'))
x <- x[110:312,1:130]
y <- bwlabel(x)
display(y, title='Binary')

#Unduh citra buah (cth:alpukat) dari internet,dan baca melalui R
original_image <- readImage(file.choose())
display(original_image)
r = channel(original_image,"r")
g = channel(original_image,"g")
b = channel(original_image,"b")
new_image = 0.2126*r+0.7152*g+0.0722*b
display(new_image)

Dataimage <- new_image@.Data 
Subdata1 <- Dataimage[110:312,130:200] 
display(Subdata1) 
Subdata2<- Dataimage[c(1:40, 100:150, 350:400 ), c(1:40, 100:150, 250:300 )] 
display(Subdata2)


# Unduh citra buah lain, dan lakukakan langkah yang sama dengan sebelumnya
# Ekstrak nilai citra dengan nama DataImage2

original_image <- readImage(file.choose())
display(original_image)
r = channel(original_image,"r")
g = channel(original_image,"g")
b = channel(original_image,"b")
new_image = 0.2126*r+0.7152*g+0.0722*b
display(new_image)

Dataimage2 <- new_image@.Data 
Subdata1 <- Dataimage2[110:312,130:200] 
display(Subdata1) 
Subdata2<- Dataimage2[c(1:40, 100:150, 350:400 ), c(1:40, 100:150, 250:300 )] 
display(Subdata2)


Dataimage2 <- Dataimage2[1:dim(Dataimage)[1], 1:dim(Dataimage)[2]] 
obs1 <- as.vector(t(Dataimage)) 
obs2 <- as.vector(t(Dataimage2)) 
obs_gabung <- rbind(obs1,obs2) 
dataset_buah <- as.data.frame(obs_gabung) 
klas<- c("alpokat", "apel") 
dataset_buah_baru<-cbind(dataset_buah, klas) 
dim(dataset_buah_baru) 
dataset_buah_baru[1,640001] 
dataset_buah_baru[2,640001]

#klasifikasi bayes

#membuat data training 80% dan data testing 20% 
URL<-"https://vincentarelbundock.github.io/Rdatasets/csv/MASS/Pima.te.csv"
data <- read.table(URL, sep=',', header=TRUE)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.8, 0.2))
# create training set and testing set
trainData <- data[ind==1,]
testData <- data[ind==2,]


#membuat data training dan data testing menggunakan cross validation k=8
URL<-"https://vincentarelbundock.github.io/Rdatasets/csv/MASS/Pima.te.csv"
dataset <- read.table(URL, sep=',', header=TRUE)
folds<-cut(seq(1,nrow(dataset)),breaks=8, labels=FALSE)
for(i in 1:8){
  testIndexes <- which(folds==i, arr.ind=TRUE)
  testData2 <- dataset[testIndexes, ]
  trainData2 <-  dataset[-testIndexes,]
}


#bayes
 library(e1071)
 data(iris)
 head(iris)
 datalatih<-iris[c(1:40, 51:90, 101:140),]
 datauji<-iris[-c(1:40, 51:90, 101:140),]
 # Membuat model Naïve Bayes menggunakan datalatih
 model.nB <- naiveBayes(Species ~ ., datalatih)
 # Prediksi datauji / data baru
 predict(model.nB, datauji[,-5])
 predict(model.nB, datauji[,-5], type="raw")
 table(predict=predict(model.nB, datauji[, -5]), true=datauji[,5])

 

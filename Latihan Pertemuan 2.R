#vektor
x <- c("abc", "def", "hij"); class(x)
x[2]<- NA
x <- x[!is.na(x)]

#Matrix
matrix(1:20, ncol=4)
matrix(1:20, nrow=4, byrow= TRUE)

#List
my.list<-list(
  buah = c("jeruk", "apel", "mangga", "pepaya"),
  jumlah = matrix(round(runif(4,10,23)), nrow=1, byrow=TRUE) 
)
my.list[[1]];my.list[[2]]

#data.Frame
df<-data.frame(
  ID=c("001","002","003", "004", "005", "006"),
  Sakit=c("diabet","DBD","malaria","diabet","DBD","Sakit pinggang"),
  Umur=c(37,10,17,45,23,65)
)
df
df$ID; df$Umur
subset (df, Sakit=="diabet")
list.pasien<-list(
  Data = df,
  Status=c("sembuh","parah","parah",
           "parah","parah","parah")
)
list.pasien$data
list.pasien[[1]][2,]

# Membaca data dari file dengan format CSV
data<-read.csv(file.choose())
data<-read.table(file.choose(), header=T, sep=",")
data<-read.table(file="D:/Magister Ilmu Komputer IPB/Matkul/Topik dalam Pengenalan Pola/Pertemuan 2/latih.csv",header=TRUE, sep=",")
class (data)
# Membaca data dari file dengan format text (delimeter)
data2<-read.delim(file.choose(), header=T)
data3<-read.table(file.choose(), header=T, sep=",")
# Membaca data dari URL
URL<-"http://www.exploredata.net/ftp/Spellman.csv"
pivot <- read.table(URL, sep=',', header=TRUE)
head(pivot)
tail(pivot)
names(pivot)


# Operasi image pada R
# Instalasi paket
#install.packages("BiocManager")
#BiocManager::install("EBImage")
 library(EBImage)
# load Image (dengan contoh pada library)
x<-readImage(system.file('images','sample.png',
                         package='EBImage'))
x <- x[0:312,1:130]
y <- bwlabel(x)
display(y, title='Binary')
# pilihan load image (contoh logo ipb)
x <- readImage(file.choose()) 
#display(x, title='Binary')

# konversi image
 x = readImage(system.file("images", "shapes.png",
                            package="EBImage"))
 display(x)
 y = channel(x, 'asgreen')
 display(y)
 x <- readImage(file.choose())
 yy <- channel(x, 'luminance') #yy <- channel(x, 'gray')
 display(yy)
 
 r = channel(x,"r")
 g = channel(x,"g")
 b = channel(x,"b")
 gray1 = 0.2126*r+0.7152*g+0.0722*b
 display(gray1)
 
 # mengambil nilai matriksnya
   Dataimage <- yy@.Data
   display(Dataimage)
   a<-dim(Dataimage); a[1];a[2] 
   bagianatas <- head(Dataimage)
   bagianbawah <-tail(Dataimage)
   mat1<-Dataimage[100:150,] ; display(mat1)
   mat2<-Dataimage[1:40, 1:40] ; display(mat2)
   mat2a<-Dataimage[100:150, 100:150] ; display(mat2a)
   mat2b<-Dataimage[350:400, 350:400] ; display(mat2b)
   mat3 <- Dataimage[c(1:40, 100:150, 350:400 ),]; display(mat3)
   mat4<- Dataimage[c(1:40, 100:150, 350:400 ), c(1:40, 100:150, 350:400)]; display(mat4)
   mat5 <- Dataimage[-(1:300),]; display(mat5)
   mat6<- Dataimage[-(c(1:40, 100:150, 350:400 )),-( c(1:40, 100:150, 250:300 ))] ; display(mat6)
   
  # Ubah matriks menjadi submatriks, kemudian konversikan
  #masing-masing kedalam array 1 dimensi dan gabungkan
  #(concatanate) kedua matriks tersebut
   Matriks1<-Dataimage[1:100,1:100]; display(Matriks1)
   Matriks2<-Dataimage[101:200, 101:200]; display(Matriks2)
   v1 <- as.vector(t(Matriks1))
   v2 <-as.vector(t(Matriks2))
   gabungan <- rbind(v1,v2)
   save(gabungan, file = "gabungan.Rda")
   load ("gabungan.Rda")
  ##setting directory
  getwd()
  setwd("C:/Users/hebho/Documents")
  
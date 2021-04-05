#Mata Kuliah  : Topik dalam Pengenalan Pola
#Tugas        : Praktikum
#Topik        : K-Nearest Neighbour

#Nama         : Hery Wibowo
#NIM          : G6501201012

#Iris

jarak_euclid <- function(objk, Smpl) 
{ 
  jarak <- 0.0 
  for(i in 1:length(objk))
  { jarak = jarak + (objk[i]- Smpl[i])^2 
  } 
  return(as.numeric(sqrt(jarak))) 
}

dataset_contoh<-read.table(choose.files(), header = T, sep = ",")

get_neigbours = function(dataset,obj,K){
  Val_jarak <- sapply(1:nrow(dataset),
                      function(j) jarak_euclid(obj,
                                               dataset[j,1:length(dataset)-1]))
  euclid_sort<-sort(Val_jarak, decreasing = F, index.return=TRUE)
  sort_index<-euclid_sort$ix
  neighbours<-dataset[sort_index[1:K],,drop=F]
}

tetangga<-get_neigbours(dataset_contoh, c(4.6, 3.4, 1.4, 0.3), 5)

class_predict<- function(neighbours){
  jum<-length(unique(neighbours$variety))
  kelas<-c()
  rank<-c()
  for(i in 1:jum){
    val <- unique(neighbours$variety)[i]
    kl<-sum(rowSums(subset(neighbours, variety == val) == val))
    rank<-c(rank,val)
    kelas<-c(kelas,kl)
  }
  return(data.frame(Class=rank, Majority=kelas))
}
class_predict(tetangga)

#Dataset Cryotherapy

jarak_euclid <- function(objk, Smpl) 
{ 
  jarak <- 0.0 
  for(i in 1:length(objk))
  { jarak = jarak + (objk[i]- Smpl[i])^2 
  } 
  return(as.numeric(sqrt(jarak))) 
}

dataset_Cryotherapy<-read.table(choose.files(), header = T, sep='\t')

get_neigbours_Cryotherapy = function(dataset,obj,K){
  Val_jarak <- sapply(1:nrow(dataset),
                      function(j) jarak_euclid(obj,
                                               dataset[j,1:length(dataset)-1]))
  euclid_sort<-sort(Val_jarak, decreasing = F, index.return=TRUE)
  sort_index<-euclid_sort$ix
  neighboursCryotherapy<-dataset[sort_index[1:K],,drop=F]
}

tetangga_Cryotherapy<-get_neigbours_Cryotherapy(dataset_Cryotherapy, c(1, 35, 12.00, 5, 1, 100), 3)

class_predict<- function(neighboursCryotherapy){
  jum<-length(unique(neighboursCryotherapy$Result_of_Treatment))
  kelas<-c()
  rank<-c()
  for(i in 1:jum){
    val <- unique(neighboursCryotherapy$Result_of_Treatment)[i]
    kl<-sum(rowSums(subset(neighboursCryotherapy, Result_of_Treatment == val) == val))
    rank<-c(rank,val)
    kelas<-c(kelas,kl)
  }
  return(data.frame(Class=rank, Majority=kelas))
}
class_predict(tetangga_Cryotherapy)

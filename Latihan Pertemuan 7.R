#Mata Kuliah  : Topik dalam Pengenalan Pola
#Tugas        : Praktikum
#Topik        : K-Nearest Neighbour

#Nama         : Hery Wibowo
#NIM          : G6501201012


jarak_euclid <- function(objk, Smpl) 
{ 
  jarak <- 0.0 
  for(i in 1:length(objk))
  { jarak = jarak + (objk[i]- Smpl[i])^2 
  } 
  return(as.numeric(sqrt(jarak))) 
}

dataset_contoh<-read.table(choose.files(), header = T, sep = ",")

objk1<- dataset_contoh[1,1:length(dataset_contoh)-1]

Val_jarak<-c() 
for(i in 1:nrow(dataset_contoh)){ 
  jarak<-jarak_euclid(objk1, dataset_contoh[i,1:2]) 
  Val_jarak<-c(Val_jarak,jarak) 
}

print(Val_jarak)

Val_jarak <- sapply(1:nrow(dataset_contoh), 
                    function(j) jarak_euclid(objk1, 
                                             dataset_contoh[j,1:length(dataset_contoh)-1])) 
print(Val_jarak)

euclid_sort<-sort(Val_jarak, decreasing = F, index.return=TRUE)

sort_index<-euclid_sort$ix

K<-3
neighbours<-dataset_contoh[sort_index[1:K],,drop=F]

get_neigbours = function(dataset,obj,K){
  Val_jarak <- sapply(1:nrow(dataset),
                      function(j) jarak_euclid(obj,
                                               dataset[j,1:length(dataset)-1]))
  euclid_sort<-sort(Val_jarak, decreasing = F, index.return=TRUE)
  sort_index<-euclid_sort$ix
  neighbours<-dataset[sort_index[1:K],,drop=F]
}

tetangga<-get_neigbours(dataset_contoh, c(5.3235, 4.5135),5)

class_predict<- function(neighbours){
  jum<-length(unique(neighbours$Y))
  kelas<-c()
  rank<-c()
  for(i in 1:jum){
    val <- unique(neighbours$Y)[i]
    kl<-sum(rowSums(subset(neighbours, Y == val) == val))
    rank<-c(rank,val)
    kelas<-c(kelas,kl)
  }
  return(data.frame(Class=rank, Majority=kelas))
}
class_predict(tetangga)

#KNN dengan library class

knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)

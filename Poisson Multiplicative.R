multiplicative_RNG<-function(a,z0,m,n) {
  xj<-matrix(NA,n,4)
  colnames(xj)<-c("aZ","Xj","Uj","Distribusi P")
  for (j in 1:n)
  {
    xj[j,1]<-(a*z0)
    xj[j,2]<-xj[j,1]%%m
    U<- xj[j,3]<-xj[j,2]/m
    z0<-xj[j,2]
    p<-0.5
    R<-U
    xj[j,4]<- log(1-R)/log(1-p)
  }
  View(xj)
}
multiplicative_RNG(35,11123,138,100)

#' @title The Impact of Items
#' @author Nery Sofia Huerta-Pacheco
#'
#' @description This function returns an estimation based on the patterns of items.
#'
#'
#' Introduce a set of categorical data set classifed as numerical data
#' @details
#' This function returns a multivariate analysis of the impact of items to identify a bias in the questionnaire validation.
#' It estimates the impact of items
#'
#' This funtion takes a set of values produced by the IMPACT
#' functions returns estimations for each item provided in the input x matrix.
#' @param x is a data set
#' @param y is a null value
#' @return IMPACT a summary table with the impact of items
#' @examples \dontrun{
#' library(IMPACT)
#' ##Reads a likert-type scale dataset
#' x<-matrix(c(2, 5, 5, 4, 4, 5, 4, 4, 5, 1, 3, 4, 4, 1, 5, 2, 2, 4, 3, 5,
#' 5, 1, 1, 4, 5, 2, 2, 4, 4, 5, 2, 4, 2, 5, 3, 4, 3, 3, 5, 3,
#' 3, 5, 5, 5, 5, 5, 5, 5, 5, 3, 4, 3, 5, 3, 4, 5, 4, 4, 4, 5,
#' 4, 1, 2, 2, 3, 1, 1, 3, 2, 5, 3, 2, 1, 5, 2, 2, 4, 1, 5, 1,
#' 2, 4, 4, 4, 3, 5, 5, 4, 2, 2, 4, 3, 5, 2, 4, 5, 4, 4, 1, 5,
#' 4, 1, 2, 3, 3, 1, 2, 5, 4, 5, 4, 3, 1, 4, 1, 3, 4, 2, 4, 2,
#' 4, 1, 2, 3, 4, 1, 1, 4, 4, 5, 3, 3, 1, 1, 1, 4, 4, 2, 4, 1,
#' 5, 1, 3, 3, 4, 5, 3, 5, 4, 5, 4, 4, 2, 5, 2, 4, 4, 4, 4, 4,
#' 4, 4, 2, 3, 1, 3, 2, 3, 3, 1, 1, 2, 4, 1, 5, 2, 2, 3, 3, 4),20,10)
#' ##Put names of variables
#' colnames(x)<-c(paste("A","-",1:10))
#' ##Declare a null value
#' y<-3
#' IMPACT(x,y)
#' }
#'
#' @references
#' Juniper, E. F., Guyatt, G. H., Streiner, D. L., & King, D. R. (1997). Clinical impact
#' versus factor analysis for quality of life questionnaire construction. Journal of
#' ClinicalEpidemiology, 233-238.
#'
#' Allen, F., & Locker, D. (2002). A Modified Short Version of the Oral Health Impact
#' Profile for Assessing Health-Related Quality of Life in Edentulous Adults. The
#' International Journal of Prosthodontics, 15(5), 446-450.
#'
#' Vicente Galindo, E. D. (2011). Analisis del Impacto frente a Teoria de Respuesta
#' al Item (Trabajo Fin de Master). Master Universitario en Analisis Avanzado de Datos
#' Multivariantes, Statistics Department, University of Salamanca, Spain.
#' @export
#' @import graphics

IMPACT<-function(x,y)-{
  Y<-as.numeric(y)
  #PARAMETRS
  datos<-x
  c<-ncol(datos)
  a<-datos
  CN<-colnames(a)
  N<-nrow(a)
  M<-ncol(a)
  print("The Impact of Different Item-Selection",quote=FALSE)
  print("----------------------------",quote=FALSE)
  print("Null Value",quote=FALSE)
  print(Y)

  ##MATRIX
  b<-matrix(1,N,M)
  n0<-matrix(0,1,M)
  k<-0

  ##ROUND
  for(i in 1:N){
    for(j in 1:M){
      #CONDICIONAL
      if(a[i,j]==Y){
        b[i,j]<-0
        n0[1,j]<-n0[1,j]+1
      }
    }
  }

  ni<-N-n0
  nb<-max(ni)
  p<-matrix(,N,M)
  v<-matrix(,N,M)

  ##POSITION

  for(i in 1:N){
    for(j in 1:M){
      if(a[i,j]==Y){
      }else{
        p[i,j]<-i
        v[i,j]<-a[i,j]
      }
    }
  }

  MED<-matrix(,1,M)

  ## MEANS
  for(j in 1:M){
    MED[1,j]<-mean(v[,j],na.rm =T)
  }

  ##OPERATIONS
  FREC<-t((ni/N)*100)
  IMPO<-t(MED)
  IMPA<-(FREC*IMPO)

  ##GENERAL MATRIX
  print("Summary Table",quote=FALSE)
  print("----------------------------",quote=FALSE)
  mgimp<-matrix(M,3)
  mgimp<-cbind(FREC,IMPO,IMPA)
  mgI<-matrix(sprintf("%.4f",mgimp),M,3)
  colnames(mgI)<-c("Frequency","Importance","Impact")
  rownames(mgI)<-CN
  print(mgI,quote=FALSE)

  ##LINES
  par(mfrow=c(1,3))
  FE<-sprintf("%.1f",FREC)
  ##PLOT DEL FREQUENCY
  gF<-plot(FREC,bg="black",main="Frequency",cex.main=1,xlab="Variables",ylab="Frequency",lty=3,lwd=1,type="l",xaxt="n")
  points(FREC,col="black",bg="black",pch=20)
  text(FREC,labels=FE,adj=c(.7,1.4),cex=.6,col="black")
  axis(1,1:M,CN,cex.axis=.9)

  PA<-sprintf("%.1f",IMPO)
  ##PLOT IMPORTANCE
  gP<-plot(PA,bg="black",main="Importance",cex.main=1,xlab="Variables",ylab="Importance",lty=3,lwd=1,type="l",xaxt="n")
  points(PA,col="black",bg="black",pch=20)
  text(PA,labels=PA,adj=c(.7,1.4),cex=.6,col="black")
  axis(1,1:M,CN,cex.axis=.9)

  I<-sprintf("%.1f",IMPA)
  ##PLOT IMPACT
  gI<-plot(IMPA,bg="black",main="Impact",cex.main=1,xlab="Variables",ylab="Impact",lty=3,lwd=1,type="l",xaxt="n")
  points(IMPA,col="black",bg="blue",pch=20)
  text(IMPA,labels=I,adj=c(.7,1.4),cex=.6,col="black")
  axis(1,1:M,CN,cex.axis=.9)
}

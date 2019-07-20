#' @title Outlier Detection for the variables in a data set
#'
#' @description This package includes function which accepts data frame and checks if there are any outliers in each variable of the data set
#'
#' @param symbol
#'
#' @return matrix with 4 columns giving the numberof values in UpperBound, LowerBound, Log transformed values' upperbound and lowerbound
#'
#' @examples  OutlierDetection(data,logtrans=0)
#'
#' @export OutlierDetection


OutlierDetection <- function(data,logtrans=0) #default no log tranformation
{
  OutlierUBLB=function(x) # function to find the count of outliers in upperbound and lowerBound
  {
    quant=as.vector(quantile(x,c(0.25,0.75))) #initializing vector for 25th and 75th percentile
    IQR=quant[2]-quant[1] #finding inter quartile range
    UpperBound=quant[2]+(1.5*IQR)
    lowerBound=quant[1]-(1.5*IQR)
    return (c(length(which(x>UpperBound)),(length(which(x<lowerBound)))))
  }
  #remove missing values
  data=na.omit(data)
  outl <- apply(data, 2, OutlierUBLB)

  #check if log transformation has to be takenor not

  if(logtrans==1)
  {
    data=log(data+1)
    logoutl<-apply(data, 2, OutlierUBLB)
    out=rbind(outl,logoutl)
    out <- as.matrix(out)
    out=t(out)
    colnames(out) <- c("UpperBound","Lower Bound","logUpperBound","loglowerBound")
    return(out)
  }
  #combine data with log transmission and without log transmission
  outl <- as.matrix(outl)
  outl=t(outl)
  colnames(outl) <- c("UpperBound","Lower Bound")
  return(outl)
}

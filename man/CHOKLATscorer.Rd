\name{CHOKLATscorer}
\alias{CHOKLATscorer}
\title{Score the CHO-KLAT}
\usage{
CHOKLATscorer(df,version=version,crosswalk=FALSE)
version
#c("2.0","3.0")
}

\description{
Calculate the CHO-KLAT Summary score
}

\arguments{
\item{df}{A data frame with the CHO-KLAT items, appropriately-named}
\item{version}{CHO-KLAT version}
\item{crosswalk}{Logical, if TRUE the function returns estimates the  CHO-KLAT2.0 score based on CHO-KLAT3.0 score (or the other way around)}

}
\examples{
set.seed(1234)
Dat=data.frame(cbind(ID=paste0("ID",1:200),replicate(21,sample(1:5,200,replace=TRUE)),replicate(14,sample(1:6,200,replace=TRUE))))
Dat[,2:36] <- sapply(Dat[,2:36],as.integer)
names(Dat)=c("ID","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23","Q24","Q25","Q26","Q27","Q28","Q29","Q30","Q31","Q32","Q33","Q34","Q35")
CHOKLATscorer(Dat,version="2.0",crosswalk = FALSE)

}

#' @title CHOKLATscorer
#' @description Calculate the CHO-KLAT Summary score
#'@param df A data frame with the CHO-KLAT items, appropriately-named.
#'@param version Specify version number
#'@param crosswalk Logical, if TRUE the function returns an additional predicted 2.0 or 3.0 variable from the data using the cross walk function
#'
#'@return CHOKLAT summary score
#'
#'@example
#'set.seed(1234)
#'Dat=data.frame(cbind(ID=paste0("ID",1:200),replicate(21,sample(1:5,200,replace=TRUE)),replicate(14,sample(1:6,200,replace=TRUE))))
#'Dat[,2:36] <- sapply(Dat[,2:36],as.integer)
#'names(Dat)=c("ID","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23","Q24","Q25","Q26","Q27","Q28","Q29","Q30","Q31","Q32","Q33","Q34","Q35")
#'CHOKLATscorer(Dat,version="2.0",crosswalk = FALSE)
#'
#' @export
CHOKLATscorer=function(df,version,crosswalk=FALSE){
  if(version=="2.0"){
    if(nrow(df[df$Q1>5&!(is.na(df$Q1))| df$Q2>5&!(is.na(df$Q2))| df$Q3>5&!(is.na(df$Q3))| df$Q4>5&!(is.na(df$Q4))| df$Q5>5&!(is.na(df$Q5))| df$Q6>5&!(is.na(df$Q6))| df$Q7>5&!(is.na(df$Q7))| df$Q8>5&!(is.na(df$Q8))| df$Q9>5&!(is.na(df$Q9))| df$Q10>5&!(is.na(df$Q10))|df$Q11>5&!(is.na(df$Q11))|df$Q12>5&!(is.na(df$Q12))|df$Q13>5&!(is.na(df$Q13))|df$Q14>5&!(is.na(df$Q14))|df$Q15>5&!(is.na(df$Q15))|df$Q16>5&!(is.na(df$Q16))|df$Q17>5&!(is.na(df$Q17))|df$Q18>5&!(is.na(df$Q18))|df$Q19>5&!(is.na(df$Q19))|df$Q20>5&!(is.na(df$Q20))|df$Q21>5&!(is.na(df$Q21))|df$Q22>6&!(is.na(df$Q22))|df$Q23>6&!(is.na(df$Q23))|df$Q24>6&!(is.na(df$Q24))|df$Q25>6&!(is.na(df$Q25))|df$Q26>6&!(is.na(df$Q26))|df$Q27>6&!(is.na(df$Q27))|df$Q28>6&!(is.na(df$Q28))|df$Q29>6&!(is.na(df$Q29))|df$Q30>6&!(is.na(df$Q30))|df$Q31>6&!(is.na(df$Q31))|df$Q32>6&!(is.na(df$Q32))|df$Q33>6&!(is.na(df$Q33))|df$Q34>6&!(is.na(df$Q34))|df$Q35>6&!(is.na(df$Q35)),])>0)
    {print("Error: There are invalid data. Response must be 1-5 for Q1-Q21, 1-6 for Q22-Q35")
    }else{
      CHOKLAT=data.frame(df$ID,df$Q1,df$Q2,df$Q3,df$Q4,df$Q5,df$Q6,df$Q7,df$Q8,df$Q9,df$Q10,df$Q11,df$Q12,df$Q13,df$Q14,df$Q15,df$Q16,df$Q17,df$Q18,df$Q19,df$Q20,df$Q21,df$Q22,df$Q23,df$Q24,df$Q25,df$Q26,df$Q27,df$Q28,df$Q29,df$Q30,df$Q31,df$Q32,df$Q33,df$Q34,df$Q35)
      names(CHOKLAT)=c("ID","Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18","Q19","Q20","Q21","Q22","Q23","Q24","Q25","Q26","Q27","Q28","Q29","Q30","Q31","Q32","Q33","Q34","Q35")
      CHOKLAT$NAcount=apply(CHOKLAT, 1, function(x) sum(is.na(x)))
      CHOKLAT$T1=CHOKLAT$Q1-1
      CHOKLAT$T2=CHOKLAT$Q2-1
      CHOKLAT$T3=5-CHOKLAT$Q3
      CHOKLAT$T4=5-CHOKLAT$Q4
      CHOKLAT$T5=5-CHOKLAT$Q5
      CHOKLAT$T6=5-CHOKLAT$Q6
      CHOKLAT$T7=CHOKLAT$Q7-1
      CHOKLAT$T8=5-CHOKLAT$Q8
      CHOKLAT$T9=5-CHOKLAT$Q9
      CHOKLAT$T10=5-CHOKLAT$Q10
      CHOKLAT$T11=5-CHOKLAT$Q11
      CHOKLAT$T12=CHOKLAT$Q12-1
      CHOKLAT$T13=CHOKLAT$Q13-1
      CHOKLAT$T14=CHOKLAT$Q14-1
      CHOKLAT$T15=5-CHOKLAT$Q15
      CHOKLAT$T16=5-CHOKLAT$Q16
      CHOKLAT$T17=5-CHOKLAT$Q17
      CHOKLAT$T18=5-CHOKLAT$Q18
      CHOKLAT$T19=CHOKLAT$Q19-1
      CHOKLAT$T20=CHOKLAT$Q20-1
      CHOKLAT$T21=CHOKLAT$Q21-1
      CHOKLAT$T22=ifelse(CHOKLAT$Q22==6,NA,CHOKLAT$Q22-1)
      CHOKLAT$T23=ifelse(CHOKLAT$Q23==6,4,5-CHOKLAT$Q23)
      CHOKLAT$T24=ifelse(CHOKLAT$Q24==6,4,5-CHOKLAT$Q24)
      CHOKLAT$T25=ifelse(CHOKLAT$Q25==6,NA,5-CHOKLAT$Q25)
      CHOKLAT$T26=ifelse(CHOKLAT$Q26==6,4,5-CHOKLAT$Q26)
      CHOKLAT$T27=ifelse(CHOKLAT$Q27==6,4,5-CHOKLAT$Q27)
      CHOKLAT$T28=ifelse(CHOKLAT$Q28==6,4,5-CHOKLAT$Q28)
      CHOKLAT$T29=ifelse(CHOKLAT$Q29==6,4,CHOKLAT$Q29-1)
      CHOKLAT$T30=ifelse(CHOKLAT$Q30==6,NA,CHOKLAT$Q30-1)
      CHOKLAT$T31=ifelse(CHOKLAT$Q31==6,NA,5-CHOKLAT$Q31)
      CHOKLAT$T32=ifelse(CHOKLAT$Q32==6,NA,5-CHOKLAT$Q32)
      CHOKLAT$T33=ifelse(CHOKLAT$Q33==6,NA,5-CHOKLAT$Q33)
      CHOKLAT$T34=ifelse(CHOKLAT$Q34==6,4,5-CHOKLAT$Q34)
      CHOKLAT$T35=ifelse(CHOKLAT$Q35==6,NA,CHOKLAT$Q35-1)
      CHOKLAT$SummaryScore2_0=round(25*rowMeans(CHOKLAT[,c(38:72)],na.rm=TRUE),1)
      CHOKLAT$SummaryScore2_0=ifelse(CHOKLAT$NAcount>8,NA,CHOKLAT$SummaryScore)

      if(crosswalk==TRUE){
        CHOKLAT$SummaryScore3_0=NA
        print(CHOKLAT[,c("ID","SummaryScore2_0","SummaryScore3_0")])
        }else{
          print(CHOKLAT[,c("ID","SummaryScore2_0")])
          }
    }

  }else{
    if(version=="3.0"){
      print("Under development")
      }else{
        print("please use valid version number(i.e. 2.0 or 3.0)")
        }
    }
}

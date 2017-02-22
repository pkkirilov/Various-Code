#Ranking hospitals in all states
rankall <- function(outcome, num = "best") {
 
  rs<-read.csv("outcome-of-care-measures.csv",colClasses = "character")  
  
  if(outcome=="heart attack"){col=11}
  else if(outcome=="heart failure"){col=17}
  else if(outcome=="pneumonia"){col=23}
  else {stop("invalid outcome")}
  
  
  states<-unique(rs[,7])
  
  #loop through the states
  for(i in 1:NROW(states))
  {
    rs1<-rs[which(rs[,7]==states[i] & rs[,col]!='Not Available'),c(2,7,col)]
    rs2<-rs1[order(rs1[,1]),]#sort by hospitalname
    rs3<-transform(rs2,rank1=rank(as.numeric(rs2[,3]),ties.method="first"))#add rank1 column
    rs4<-na.omit(rs3)#omit NA's from the data
    
    #allow for 'best' and 'worst' num argument values
    rkmax<-max(rs4[,4])
    if(num=='best'){ranknum=1}
    else if(num=='worst'){ranknum=rkmax}
    else {ranknum=num}
    
    if(i==1)
    {
   
      if(nrow(rs4)<ranknum)
      {
        rep<-cbind("NA",rs4[1,2])
      } else
      {
        rep<-rs4[which(rs4[,4]==ranknum),1:2]
      }
    } else 
    {
      
      if(nrow(rs4)<ranknum)
      {
        s<-cbind("NA",rs4[1,2])
      } else
      {
        s<-rs4[which(rs4[,4]==ranknum),1:2]
      }
      rep<-rbind(as.matrix(rep),as.matrix(s))
    }
  }
 

  rep<-as.data.frame(rep[order(rep[,2]),])
   names(rep) <- c("hospital", "state")
   rep
}
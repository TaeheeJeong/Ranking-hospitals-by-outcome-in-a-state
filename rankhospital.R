rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <-read.csv("outcome-of-care-measures.csv",colClasses="character")
        
        
        ## Check that state and outcome are valid
        state_check<-data$State==state
        
        if(sum(state_check)<=0){
                stop("invalid state")
        }
        
        if(outcome =="heart attack") {colnum<-11}
        else if(outcome =="heart failure") {colnum<-17}
        else if(outcome =="pneumonia") {colnum<-23}
        else stop("invalid outcome")
        
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        data_state<-data[data$State==state,]
        data_state[,colnum]<-as.numeric(data_state[,colnum])
        filtered <- complete.cases(data_state[,colnum])
        data_clean <- data_state[filtered,]
        data_sorted<-data_clean[order(data_clean[,colnum],data_clean$Hospital.Name),]
        
        if(num=="best"){mum<-1}
        else if(num=="worst"){num<- nrow(data_clean)}
        else if(num>nrow(data_sorted)){stop("NA")}
        return(data_sorted$Hospital.Name[num])
        
}
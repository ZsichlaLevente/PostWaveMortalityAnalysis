processCOVerAGE<-function(data){
  
  data<-tibble(data[data$Measure%in%c("Deaths") & data$Metric=="Count" & data$Region=="All",]) # filter down to death counts on the country level
  data_agebin<-data%>%
    mutate(Date=as.Date(Date,format = "%d.%m.%Y"), # convert from string to date
           Age_low=as.numeric(Age), # convert from string to numeric
           Age_high=Age_low+AgeInt-1, # compute max of age interval reported
           Age_bin=cut(Age_low, breaks=c(0, 15, 65, 75, 85,120),right=F), # bin data to the stmf and owid data bins
           Age_bin_low=as.numeric(str_extract(Age_bin, "[:digit:]+")), # extract lower end of bins
           Age_bin_high=as.numeric(str_sub(str_extract(Age_bin, ",[:digit:]+"),2,-1))-1 # extract higher end of bins
           )%>%
    group_by(Code, Date, Sex, Measure)%>%
    filter(all(Age_high<=Age_bin_high))%>% # filter out those bins which belong to multiple stmf bins
    group_by(Country, Date, Sex, Measure,Age_bin)%>%
    summarize(Value=sum(Value))%>% # sum data inside bins
    group_by(Country, Sex, Measure,Age_bin)%>%
    mutate(Value_weekly=c(Value[1],diff(Value))) # compute daily/weekly data from cumulative
  
  data_tot<-data%>%
    filter(Age=="TOT")%>% # filter out data for the whole population
    select(Country, Date, Sex, Measure, Value)%>% # select important columns
    group_by(Country, Sex, Measure)%>%
    mutate(Date=as.Date(Date,format = "%d.%m.%Y"), # convert date
           Value_weekly=c(Value[1],diff(Value)), # compute daily/weekly data from cumulative
           Age_bin="TOT")%>% # make an extra column before combining tables
    filter(Date>min(Date)) # filter out the first data point for every country
  
  dataOut<-rbind(data_agebin,data_tot)

  return(dataOut)
}




processCOVerAGE<-function(data){
  
  data<-COVerAGE
  
  ## preprocessing of dataset, filtering out unusable data
  data<-tibble(data[data$Measure=="Deaths" & data$Metric=="Count" & data$Region=="All" ,]) # filter down to death counts on the country level
  
  data_agebin<-data%>%
    filter(Age!="TOT")%>% # filter out the totals (see later)
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
    mutate(Value=c(Value[1],diff(Value))) # compute daily/weekly data from cumulative
  
  data_tot<-data%>%
    filter(Age=="TOT")%>% # filter out data for the whole population
    select(Country, Date, Sex, Measure, Value)%>% # select important columns
    group_by(Country, Sex, Measure)%>%
    mutate(Date=as.Date(Date,format = "%d.%m.%Y"), # convert date
           Value=c(Value[1],diff(Value)), # compute daily/weekly data from cumulative
           Age_bin="TOT")%>% # make an extra column before combining tables
    filter(Date>min(Date)) # filter out the first data point for every country
  
  dataOut<-rbind(data_agebin,data_tot)%>% # bind the two parts together
    ungroup()%>%
    select(-Measure)%>%
    filter(Value>=0)

  ## filtering countries with unusable data after manual inspection
  countryList<-readxl::read_xlsx("CountryList.xlsx") # reading in data about selected countries
  
  dataOut<-dataOut%>%
    filter(Country%in%countryList$COVerAGE_name) # filter out unselected countries

  ### compute missing information in the selected countries
  ## calculate sex-neutral age-stratified data if sex-stratified is available
  # detect if information is missing
  countryList<-countryList%>%
    mutate(Astr_Sstr=COVerAGE_name%in%unique(filter(dataOut,Sex!="b",Age_bin!="TOT")$Country),
           Astr_Sn=COVerAGE_name%in%unique(filter(dataOut,Sex=="b",Age_bin!="TOT")$Country))
  missingData<-filter(countryList,Astr_Sstr & !Astr_Sn)$COVerAGE_name
  
  dataOut<-dataOut%>%
    filter(Country%in%missingData,Age_bin!="TOT")%>% # filtering
    group_by(Country,Date,Age_bin)%>% # grouping without gender
    summarize(Value=sum(Value))%>% # sum according to gender
    mutate(Sex="b", .after= Date)%>% # add Sex column
    rbind(dataOut) # add information to the original data frame
  
  ## calculate sex stratified data for the total population
  # detect if information is missing
  countryList<-countryList%>%
    mutate(Astr_Sstr=COVerAGE_name%in%unique(filter(dataOut,Sex!="b",Age_bin!="TOT")$Country),
           An_Sstr=COVerAGE_name%in%unique(filter(dataOut,Sex!="b",Age_bin=="TOT")$Country))
  missingData<-filter(countryList,Astr_Sstr & !An_Sstr)$COVerAGE_name
   
  dataOut<-dataOut%>%
    filter(Country%in%missingData,Age_bin!="TOT",Sex!="b")%>% # filtering
    group_by(Country,Date,Sex)%>% # grouping without age bins
    summarize(Value=sum(Value))%>% # sum according to age bins
    mutate(Age_bin="TOT", .after= Sex)%>% # add Age_bins column
    rbind(dataOut) # add information to the original data frame
  
  ## calculate sex neutral data for the total population
  # detect if information is missing
  countryList<-countryList%>%
    mutate(An_Sstr=COVerAGE_name%in%unique(filter(dataOut,Sex!="b",Age_bin=="TOT")$Country),
           An_Sn=COVerAGE_name%in%unique(filter(dataOut,Sex=="b",Age_bin=="TOT")$Country))
  missingData<-filter(countryList,An_Sstr & !An_Sn)$COVerAGE_name
  
  dataOut<-dataOut%>%
    filter(Country%in%missingData,Age_bin=="TOT",Sex!="b")%>% # filtering
    group_by(Country,Date)%>% # grouping without age bins and sex
    summarize(Value=sum(Value))%>% # sum according to age bins
    mutate(Sex="b", .after=Date)%>%
    mutate(Age_bin="TOT", .after= Sex)%>% # add Sex and Age_bins columns
    rbind(dataOut) # add information to the original data frame
  
  # check if all information is recovered
  countryList<-countryList%>%
    mutate(Astr_Sstr=COVerAGE_name%in%unique(filter(dataOut,Sex!="b",Age_bin!="TOT")$Country),
           Astr_Sn=COVerAGE_name%in%unique(filter(dataOut,Sex=="b",Age_bin!="TOT")$Country),
           An_Sstr=COVerAGE_name%in%unique(filter(dataOut,Sex!="b",Age_bin=="TOT")$Country),
           An_Sn=COVerAGE_name%in%unique(filter(dataOut,Sex=="b",Age_bin=="TOT")$Country))
  
  ## smoothing data by lowering the resolution to weekly reports (on Sunday)
  dataOut<- dataOut%>%
    mutate(Date=ceiling_date(Date, unit="week"))%>% #sunday of every week
    group_by(Country,Sex,Age_bin,Date)%>% # group with weekly resolution
    summarise(Value=sum(Value))%>% # summation of deaths within one week
    ungroup()
  
  return(list(dataOut,countryList)) # return multiple objects from function
}



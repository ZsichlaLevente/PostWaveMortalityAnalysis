# Libraries----
library(tidyverse)


# Functions----
read_group_csv <- function(filenames, objectnames, file_location, wd, sep, dec, header) {
  setwd(file_location)
  group <- ""
  objInd <- 1
  temp <- tibble()
  for (i in 1:length(filenames)) {
    obj <- read.csv(filenames[i], sep = sep, dec = dec, header = header)
    obj <- obj %>%
      mutate(c_code = str_sub(filenames[i], -6, -5))
    if (i == length(filenames)) {
      if (str_sub(filenames[i], 1, -7) == group) {
        temp <- bind_rows(temp, obj)
      } else {
        temp <- obj
      }
      assign(objectnames[objInd], temp, envir = globalenv())
    } else if (str_sub(filenames[i], 1, -7) == group) {
      temp <- bind_rows(temp, obj)
    } else if (i == 1) {
      group <- str_sub(filenames[i], 1, -7)
      temp <- obj
    } else {
      assign(objectnames[objInd], temp, envir = globalenv())
      objInd <- objInd + 1
      group <- str_sub(filenames[i], 1, -7)
      temp <- obj
    }
  }
  setwd(wd)
}


# Data----
read_group_csv(
  filenames = c("combIFRMatrixFR.csv", "combIFRMatrixUK.csv", "combIFRMatrixUS.csv", "frailtyMatrixFR.csv", "frailtyMatrixUK.csv", "frailtyMatrixUS.csv"),
  objectnames = c("deathrate.matrices", "frailty.matrices"),
  file_location = "/Users/ASUS/Documents/MyDocs/Work/COVID-19/Project/Covid_RGP_firstGit/dataExtractedGender",
  wd = "/Users/ASUS/Documents/MyDocs/Work/COVID-19/Project/Covid_RGP_firstGit",
  sep = " ",
  dec = ".",
  header = T
)


# Main code----
fatality.matrices <- tibble(IFR_it = numeric(), it = numeric(), id = character(), sse = numeric(), gender = character(), age = numeric(), YPL = numeric(), deaths_m_comp = numeric(), deaths_m_orig = numeric(),deaths_f_orig=numeric(), c_code = character())
IFR.diff.vectors<-tibble(IFR_it = numeric(), it = numeric(), id = character(), sse = numeric(), gender = character(), age = numeric(), IFR_m_comp = numeric(), IFR_m_orig = numeric(),IFR_f_orig=numeric(),explained_diff=numeric(), c_code = character())


for (c in unique(deathrate.matrices$c_code)) {
  
  frail_grouped <- frailty.matrices %>%
    tibble()%>%
    filter(c_code == c)%>%
    group_split(gender)
  frail_male<-frail_grouped[[2]]
  frail_female<-frail_grouped[[1]]
  
    for (i in unique(filter(deathrate.matrices,c_code==c)$id)) {
      
      filt_grouped <- deathrate.matrices %>%
        tibble()%>%
        filter(id == i, c_code == c)%>%
        group_split(gender)
      filt_male<-filt_grouped[[2]]
      filt_female<-filt_grouped[[1]]
      
      fatality.matrix<-filt_male%>%
        mutate(deaths_m_comp=filt_male$mort*frail_female$value,
               deaths_m_orig=filt_male$mort*frail_male$value,
               deaths_f_orig=filt_female$mort*frail_female$value)%>%
        select(-mort)
      
      IFR.vector<-tibble(
        IFR_it = unique(fatality.matrix$IFR_it),
        it = unique(fatality.matrix$it),
        id = unique(fatality.matrix$id),
        sse = unique(fatality.matrix$sse),
        gender = unique(fatality.matrix$gender),
        age = unique(fatality.matrix$age),
        IFR_m_comp = summarize(group_by(fatality.matrix,age),sum=sum(deaths_m_comp))$sum/summarize(group_by(frail_female,Var1),sum=sum(value))$sum,
        IFR_m_orig=summarize(group_by(fatality.matrix,age),sum=sum(deaths_m_orig))$sum/summarize(group_by(frail_male,Var1),sum=sum(value))$sum,
        IFR_f_orig=summarize(group_by(fatality.matrix,age),sum=sum(deaths_f_orig))$sum/summarize(group_by(frail_female,Var1),sum=sum(value))$sum,
        explained_diff=(IFR_m_orig-IFR_m_comp)/(IFR_m_orig-IFR_f_orig),
        c_code = unique(fatality.matrix$c_code)
      )
        
      fatality.matrices <- bind_rows(fatality.matrices, fatality.matrix)
      IFR.diff.vectors <- bind_rows(IFR.diff.vectors, IFR.vector)
    }
}
rm("c","i","IFR.vector","frail_male","frail_female","frail_grouped","filt_male","filt_grouped","filt_female","fatality.matrix")


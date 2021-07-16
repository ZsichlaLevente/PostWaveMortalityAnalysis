library(tidyverse)
library(reshape2)

#wd_list<-c("0417_results_ODriscoll_US","0415_results_ODriscoll_UK","0415_results_ODriscoll_FR")
wd_list<-c("0622_results_ODriscoll_UK")

for(wd in wd_list){
  rm(list=setdiff(ls(), c("wd_list","wd")))
  
  setwd(paste("/Users/ASUS/Documents/MyDocs/Work/COVID-19/Project/Data tables/Results.file/",wd,sep=""))
  load('Results.RData',envir = .GlobalEnv)
  
  setwd(paste("/Users/ASUS/Documents/MyDocs/Work/COVID-19/Project/Covid_RGP_firstGit/dataExtractedGender",sep=""))
  
  #Deathrate matrices
  #write.table(deathrate.matrices.comb.results,paste("combIFRMatrix",str_sub(wd,-2),".csv",sep=""),sep=" ",dec=".")
  write.table(deathrate.matrices.results,paste("combIFRMatrix",str_sub(wd,-2),".csv",sep=""),sep=" ",dec=".")
  
  #Frailty matrix by gender
  # Calculation----
  frailty_matrix_male <- dem.matrix(YPL, age)
  frailty_matrix_female <- dem.matrix(YPL, age)
  
  frailty_matrix_male[1, ] <- dem$Males * dem$mort.Males
  frailty_matrix_female[1, ] <- dem$Females * dem$mort.Females
  
  for (R in 2:length(YPL)) {
    for (C in 1:length(age)) {
      if ((C + R - 1) > length(dem$Age)) {
        frailty_matrix_male[R, C] <- (dem$Males[C] - sum(frailty_matrix_male[, C][1:(R - 1)])) * dem$mort.Males[length(dem$Age)]
        frailty_matrix_female[R, C] <- (dem$Females[C] - sum(frailty_matrix_female[, C][1:(R - 1)])) * dem$mort.Females[length(dem$Age)]
      } else {
        frailty_matrix_male[R, C] <- (dem$Males[C] - sum(frailty_matrix_male[, C][1:(R - 1)])) * dem$mort.Males[C + R - 1]
        frailty_matrix_female[R, C] <- (dem$Females[C] - sum(frailty_matrix_female[, C][1:(R - 1)])) * dem$mort.Females[C + R - 1]
      }
    }
  }
  #----
  frailty_matrix<-melt(t(frailty_matrix_male))%>%
    mutate(gender="m")%>%
    bind_rows(.,mutate(melt(t(frailty_matrix_female)),gender="f"))
  write.table(frailty_matrix,paste("frailtyMatrix",str_sub(wd,-2),".csv",sep=""),sep=" ",dec=".")
}

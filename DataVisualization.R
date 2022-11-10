library(tidyverse)
library(ggpubr)
library(lubridate)
library(tidyquant)
library(ggformula)

##common theme for plots
myTheme<-function(){
  theme_bw() %+replace%
    theme( text=element_text(size=18),strip.background = element_rect(fill = "white"),axis.text.y=element_text(colour="black",margin=margin(l=10)),
          panel.spacing = unit(1.5, "lines"), plot.margin=margin(t = 0, r = 10, b = 0, l = 0, unit = "pt"),
          panel.background = element_rect(fill = "white",colour = "white",size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray90"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))
}

## visualize stmf dataset (make plots for all age categories)
# Args: country or region (options: AUS, AUT, BEL BGR, CAN, CHE, CHL, CZE, DEUTNP, DNK, ESP, EST, FIN, FRATNP, GBRTENW, GBR_NIR, GBR_SCO, GRC, HRV, HUN, ISL, ISR, ITA, KOR, LTU, LUX, LVA, NLD, NOR, NZL_NP, POL, PRT, RUS, SVK, SVN, SWE, TWN, USA)
#       sex (options: b,m,f)
#       target year (options: 2020-2022)
#       measure (options: deaths, deathrates)
#       reference years (options: 1990-2019)
#       reference level calculation (options: week-specific averages (need more?))
# show: reference line (blue); 
#       previous years; 
#       target year (black); 
#       deficits (grey)

showExcess<-function(CCode, targetYear, measure, refYears,sex,scaleAll=F){
  
if(measure=="deaths"){
  measureCats<-c("D0_14","D15_64","D65_74","D75_84","D85p","DTotal")
}else{
  measureCats<-c("R0_14","R15_64","R65_74","R75_84","R85p","RTotal")
}

myplots <- vector('list', 6)
for(i in 1:length(measureCats)){
  myplots[[i]]<-local({
    #message(i)
    i<-i
    p1<-ggplot(data=NULL)+
      geom_line(data=filter(stmf,CountryCode==CCode, Year==targetYear, Sex==sex),aes(x=Week, y=eval(parse(text=measureCats[i]))),lwd=1.2)+ #target year
      geom_line(data=filter(stmf,CountryCode==CCode, Year%in%refYears, Sex==sex),aes(x=Week, y=eval(parse(text=measureCats[i])),group=Year),color="grey")+ # reference years
      geom_line(data=summarize(group_by(filter(stmf,CountryCode==CCode, Year%in%refYears, Sex==sex),Week),meanVar=mean(eval(parse(text=measureCats[i])))),aes(x=Week, y=meanVar),lwd=1.1,color="lightskyblue")+
      scale_x_continuous(breaks= c(1, seq(5, 45, 5), 50, 52), labels=c(1, seq(5, 45, 5), "", 52),expand = c(0, 0))+
      labs(y=measureCats[i])+
      theme(legend.position="top", text=element_text(size=18),strip.background = element_rect(fill = "white"),axis.text.y=element_text(colour="black",margin=margin(l=10)),
            panel.spacing = unit(1.5, "lines"), plot.margin=margin(t = 0, r = 10, b = 0, l = 0, unit = "pt"),
            panel.background = element_rect(fill = "white",colour = "white",size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray90"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
      guides(fill = guide_legend(override.aes = list(colour = "black")))
    
    if(scaleAll){
      if(measure=="deaths"){
        p1<-p1+
          lims(y=c(
            min(select(filter(stmf,CountryCode==CCode, Year==targetYear, Sex==sex),starts_with("D"))),
            max(select(filter(stmf,CountryCode==CCode, Year==targetYear, Sex==sex),starts_with("D")))
          ))
      }else{
        p1<-p1+
          lims(y=c(
            min(select(filter(stmf,CountryCode==CCode, Year==targetYear, Sex==sex),starts_with("R"))),
            max(select(filter(stmf,CountryCode==CCode, Year==targetYear, Sex==sex),starts_with("R")))
          ))
      }
    }
    print(p1)
  })
}
plot1<-ggarrange(myplots[[1]],myplots[[2]],myplots[[3]],myplots[[4]],myplots[[5]],myplots[[6]], ncol = 3, nrow = 2)
print(annotate_figure(plot1, top = text_grob(paste("(country=",CCode,", sex=",sex,", target year=",targetYear," and reference years=",refYears[1],"-",refYears[length(refYears)],")",sep=""), face = "bold", size = 14)))
}

## show p scores by age group from the OWID excess mortality table
# Args: country or region (options: unique(owidEM$location))
#       target year (options: 2020-2022)
#       measure (options: p_scores, p_proj)
# show: reference line (blue); 
#       target year (black); 

showPScores<-function(Location, targetYear, measure,scaleAll=F){
  
  if(measure=="p_scores"){
    measureCats<-c("p_scores_0_14","p_scores_15_64","p_scores_65_74","p_scores_75_84","p_scores_85plus","p_scores_all_ages")
  }else{
    measureCats<-c("p_proj_0_14","p_proj_15_64","p_proj_65_74","p_proj_75_84","p_proj_85p","p_proj_all_ages")
  }
  
  myplots <- vector('list', 6)
  for(i in 1:length(measureCats)){
    myplots[[i]]<-local({
      #message(i)
      i<-i
      p1<-ggplot(data=NULL)+
        geom_line(data=filter(owidEM,location==Location, year(date)==targetYear),aes(x=time, y=eval(parse(text=measureCats[i]))),lwd=1.2)+ #target year
        geom_hline(yintercept=0,lwd=1.1,color="lightskyblue")+
        scale_x_continuous(breaks= c(1, seq(5, 45, 5), 50, 52), labels=c(1, seq(5, 45, 5), "", 52),expand = c(0, 0))+
        labs(y=measureCats[i])+
        theme(legend.position="top", text=element_text(size=18),strip.background = element_rect(fill = "white"),axis.text.y=element_text(colour="black",margin=margin(l=10)),
              panel.spacing = unit(1.5, "lines"), plot.margin=margin(t = 0, r = 10, b = 0, l = 0, unit = "pt"),
              panel.background = element_rect(fill = "white",colour = "white",size = 0.5, linetype = "solid"),
              panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "gray90"), 
              panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "white"))+
        guides(fill = guide_legend(override.aes = list(colour = "black")))
      
      if(scaleAll){
        if(measure=="p_scores"){
          p1<-p1+
            lims(y=c(
              min(select(filter(owidEM,location==Location, year(date)==targetYear),starts_with("p_scores_"))),
              max(select(filter(owidEM,location==Location, year(date)==targetYear),starts_with("p_scores_")))
            ))
        }else{
          p1<-p1+
            lims(y=c(
              min(select(filter(owidEM,location==Location, year(date)==targetYear),starts_with("p_proj_"))),
              max(select(filter(owidEM,location==Location, year(date)==targetYear),starts_with("p_proj_")))
            ))
        }
      }
      print(p1)
    })
  }
  plot1<-ggarrange(myplots[[1]],myplots[[2]],myplots[[3]],myplots[[4]],myplots[[5]],myplots[[6]], ncol = 3, nrow = 2)
  print(annotate_figure(plot1, top = text_grob(paste("(country=",Location," and target year=",targetYear,")",sep=""), face = "bold", size = 14)))
}


## compare reported number of deaths in the who and COVerAGE datasets
# Args: none
# show: who (black);
#       COVerAGE (red)

showCOVerAGErestoration<-function(Location){

  countryList<-readxl::read_xlsx("COVerAGEManualInspection.xlsx")
  
  who2<-who%>%
    filter(Country%in%countryList$WHO_name)%>% # filter selected countries in the who dataset
    mutate(Date=ceiling_date(Date_reported,unit = "week"))%>% # sunday of every week
    left_join(select(countryList,WHO_name,COVerAGE_name),by=c('Country'='WHO_name'))%>% #join with country table to show COVerAGE names if they do not match
    select(-Country)%>% # remove WHO names
    rename(Country=COVerAGE_name)%>% # renaming
    group_by(Date,Country)%>%
    summarise(Value=sum(New_deaths)) # summation of deaths within one week
  
  p1<-ggplot(data=NULL)+
    geom_point(data=filter(COVerAGE,Age_bin=="TOT",Sex!="b"),aes(Date,Value,color=Sex),alpha=0.2)+
    geom_spline(data=filter(COVerAGE,Age_bin=="TOT",Sex!="b"),aes(Date,Value,color=Sex),lwd=1.2)+
    facet_wrap(Sex~Country,scales="free_y")+
    myTheme()
  
  p2<-ggplot(data=NULL)+
    geom_point(data=filter(COVerAGE,Sex=="b",Age_bin!="TOT"),aes(Date,Value,color=Age_bin,group=Sex),alpha=0.2)+
    geom_spline(data=filter(COVerAGE,Sex=="b",Age_bin!="TOT"),aes(Date,Value,color=Age_bin,group=Sex),lwd=1.2)+
    facet_wrap(Age_bin~Country,scales="free_y")+
    myTheme()
  
  p3<-ggplot(data=NULL)+
    geom_point(data=filter(COVerAGE,Age_bin=="TOT",Sex=="b"),aes(Date,Value),alpha=0.2,color="red")+
    geom_spline(data=filter(COVerAGE,Age_bin=="TOT",Sex=="b"),aes(Date,Value),lwd=1.2,color="red")+
    geom_point(data=filter(who2),aes(Date,Value),alpha=0.2,color="black")+
    geom_spline(data=filter(who2),aes(Date, Value),lwd=1.2,color="black")+
    facet_wrap(~Country,scales="free_y")+
    myTheme()
  
  
  print(p1)
  print(p2)
  print(p3)
}


## show COVerAGE death counts and excess mortality p scores in every age category
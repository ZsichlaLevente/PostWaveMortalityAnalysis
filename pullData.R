setwd("C:/Users/leven/OneDrive/Dokumentumok/Dokumentumok/Work/Projects/COVID-19/YPL Project/_PostWaveMortalityAnalysis/Data")

# Download the Short-Term Mortality Fluctuations dataset
download.file(url="https://mortality.org/File/GetDocument/Public/STMF/Outputs/stmf.csv",destfile = "stmf.csv")
download.file(url="https://mortality.org/File/GetDocument/Public/STMF_DOC/STMFNote.pdf",destfile = "STMFNote.pdf")

# Download the Our World In Data COVID-19 data tables
download.file(url="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/excess_mortality/excess_mortality.csv",destfile = "owid_excess_mortality.csv")
download.file(url="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/excess_mortality/README.md",destfile = "owid_excess_mortality_README.md")
download.file(url="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv",destfile = "owid_covid_data.csv")
download.file(url="https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/README.md",destfile = "owid_covid_README.md")

# Download ECDC case and death counts
download.file(url="https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",destfile = "ecdc.csv")
download.file(url="https://www.ecdc.europa.eu/sites/default/files/documents/Description-and-disclaimer_daily_reporting.pdf",destfile = "ecdc_README.pdf")

# Download WHO case and death counts
download.file(url="https://covid19.who.int/WHO-COVID-19-global-data.csv",destfile = "who.csv")

# Download COVerAGE database
#download.file(url="https://osf.io/download/9dsfk/",destfile = "inputDB.zip") # for now you have to download this file manually!


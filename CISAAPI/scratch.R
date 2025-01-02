library(tidyverse)
library(openxlsx)
library(jsonlite)


getwd()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

list.files()

df<-tibble(read.csv("./CISA API/2023-07-27_CISA_APIData.csv"))
df2<-tibble(read.csv("./CISA API/CISA_ICS_ADV_Master_20230727.csv"))

column_names<-c(colnames(df))

unique(df$criteria)


data_cpe<-fromJSON("https://services.nvd.nist.gov/rest/json/cpes/2.0")


df2$url<-paste0("https://www.cisa.gov/news-events/ics-advisories/",df2$ICS.CERT_Number)
df2$cve_url<-paste0("https://nvd.nist.gov/vuln/detail/",df2$CVE_Number)


df2%>%
  select(ICS.CERT_Number,ICS.CERT_Advisory_Title,url,CVE_Number,cve_url)%>%
  View()

#Tests for app processing----

surveydata<-read.xlsx("Flat File V1.xlsx")
surveydata<-surveydata[1:9,]

vulndata<-read.csv("CISA_ICS_ADV_Master_20230727.csv")

vulndata%>%
  right_join(surveydata, by=c("Vendor","Product"="Device.Type"))%>%
  View()

vulndata%>%
  filter(Vendor=="Rockwell Automation")%>%
  View()

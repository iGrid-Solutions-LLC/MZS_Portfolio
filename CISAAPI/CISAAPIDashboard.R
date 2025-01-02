library(tidyverse)
library(jsonlite)

data<-fromJSON("https://services.nvd.nist.gov/rest/json/cves/2.0")


timestamp<-data[[6]]
timestamp<-as.character(str_split(timestamp,"T")[[1]][1])

cve<-as_tibble(data[[7]][[1]]) #as_tibble does not turn this into a dataframe for some reason

#First Round of unnesting
cve<-cve%>%
  unnest(descriptions)%>%
  unnest(metrics)%>%
  select(-9,-10)%>%
  unnest(cvssMetricV2)%>%
  unnest(cvssData)

#Create weaknesses dataframe - May not need this later
weaknesses<-cve[[27]]

#name each dataframe with the id for the weakness
ids<-as_tibble(cve$id)
names(weaknesses)<-ids$value


#Unnest the descriptions for each weakness and add id column that matches id
weaknesses<-pmap(list(weaknesses,names(weaknesses)),function(x,y){
  x<-as_tibble(x)
  x<-x%>%
    unnest(description)%>%
    mutate(id=y)
})

#Collapse dataframes into one
weaknesses<-bind_rows(weaknesses)
weaknesses<-weaknesses%>%
  rename("weakness_enumeration"="value")

#Grab distinct id/weakness enumerations in preparations for joining
weaknesses<-weaknesses%>%
  group_by(id,weakness_enumeration)%>%
  distinct()

#Join weaknesses to cve, removing origional weaknesses column
cve<-cve%>%
  select(-weaknesses)%>%
  full_join(weaknesses,by=c("id","lang","source","type"))

#Remove weaknesses dataframe
rm(weaknesses)

#Unnest configurations and reference columns
cve<-cve%>%
  unnest(configurations)%>%
  unnest_wider(nodes,names_sep = "_")%>%
  unnest_longer(nodes_cpeMatch)%>%
  unnest(nodes_cpeMatch)%>%
  unnest(nodes_operator)%>%
  unnest(nodes_negate)%>%
  unnest_wider(references,names_sep = "_")%>%
  unnest_longer(references_url)%>%
  unnest_longer(references_source)%>%
  unnest_longer(references_tags)%>%
  unnest(references_tags)%>%
  unnest_wider(vendorComments,names_sep = "_")

#Write csv file
write.csv(cve,paste0(timestamp,"_CISA_APIData.csv"))




#Old Code delete later if not needed----
# 
# configurations<-cve$configurations
# 
# #Remake ids variable
# ids<-as_tibble(cve$id)
# 
# names(configurations)<-ids$value
# 
# # configurations[[1]]%>%
# #   unnest(nodes)%>%
# #   unnest(cpeMatch)%>%
# #   View()
# # 
# # configurations[[1]]%>%
# #   View()
# 
# 
# 
# configurations<-pmap(list(configurations,names(configurations)),function(x,y){
#   
#   if(is.null(x)){
#     df<-tibble(operator=c(NA),negate=c(NA),vulnerable=c(NA),criteria=c(NA),matchCriteriaId=c(NA),id=y)
# 
#   }else{
# 
#     df1<-x[[1]][[1]][1:2]
#     df2<-x[[1]][[1]][3][[1]][[1]]
#     df<-cbind(df1,df2)
#     df<-as_tibble(df)%>%
#       mutate(id=y)
#   }
#   
#   return(df)
# })
# 
# configurations<-bind_rows(configurations)
# 
# columnames<-colnames(configurations)
# 
# configurations%>%
#   filter(df1=="AND")%>%
#   View()
# 
# configurations%>%
#   select(-df1,-df2)%>%
#   group_by(operator,negate,vulnerable,criteria,matchCriteriaId,id,versionEndIncluding,versionEndExcluding,
#            versionStartIncluding)%>%
#   distinct()%>%
#   View()
# 
# 
# testdf<-cve%>%
#   unnest(configurations)%>%
#   unnest_wider(nodes,names_sep = "_")
# 
# testdf[,29]
# 
# # testdf2<-
#   
#   testdf%>%
#   unnest_longer(nodes_cpeMatch)%>%
#   unnest(nodes_cpeMatch)%>%
#   unnest_wider(references,names_sep = "_")%>%
#   unnest_longer(references_url)%>%
#   unnest_longer(references_source)%>%
#   unnest(references_tags)%>%
#   View()
#   
#   
#   # unnest_longer(references_tags)%>%
# 
# # colnames(testdf2)
# 
# testdf2[1,38][[1]]
# 
# testdf2%>%
#   filter(is.null(references_tags))%>%
#   # select(id,references_tags)%>%
#   # unnest(references_tags)%>%
#   View()
# 
# testdf2[2,]
# 
# sum(is.null(testdf2$references_tags))

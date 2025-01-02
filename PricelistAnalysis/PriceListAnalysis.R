library(tidyverse)
library(openxlsx)
library(gt)

setwd("C://Users//12567//Desktop//R//Ransera")
getwd()

data<-read.xlsx("PriceList.xlsx",sheet="PriceList")
PL<-tibble(data)
rm(data)

#Count of currency use 
PL%>%
  count(Currency)%>%
  rename("Count"=n)%>%
  gt()%>%
  tab_header(
    title="Count of Currency Use",
    subtitle="Number of times a currency is used"
  )

#Price distribution per currency by Category
PL%>%
  group_by(Currency,Category)%>%
  count()%>%
  rename("Count"=n)%>%
  ggplot(aes(x=Category,y=Count,fill=Currency))+
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=90,size=8,hjust=0.95,vjust=.5))+
  ggtitle("Currency Distribution by Category")

#Highest and Lowest prices in each category 
PL_max<-PL%>%
  group_by(Category)%>%
  filter(Category!="Professions",Currency=="gp")%>%
  filter(Price==max(Price))%>%
  distinct(Category,Price,Currency)%>%
  mutate(Min_Max="Max")


PL_min<-PL%>%
  group_by(Category)%>%
  filter(Category!="Professions")%>%
  filter(Price==min(Price))%>%
  mutate(lowest=ifelse(Currency=="gp",3,
                       ifelse(Currency=="sp",2,
                              ifelse(Currency=="cp",1,NA))))%>%
  filter(lowest==min(lowest))%>%
  select(-lowest)%>%
  distinct(Category,Price,Currency)%>%
  mutate(Min_Max="Min")

PL_minmax<-rbind(PL_max,PL_min)
rm(PL_max,PL_min)

#Table of Min and Max
PL_minmax%>%
  select(Min_Max,Category,Price,Currency)%>%
  arrange(Category,Min_Max)%>%
  gt()%>%
  tab_header(
    title="Minimum and Maximum Price per Category"
  )%>%
  tab_style(
    locations = cells_column_labels(columns=everything()),
    cell_text(weight="bold")
  )%>%
  tab_style(
    locations=cells_title(groups="title"),
    cell_text(weight="bold")
  )%>%
  tab_style(
    locations=cells_group(),
    cell_text(weight="bold")
  )%>%
  tab_options(
    table.width = px(550),
    data_row.padding = px(1),
    row_group.padding = px(1),
    column_labels.padding = px(1),
    heading.padding = px(1)
  )


#Distribution of pricing per category minus Professions, Naval and Land/Structures
PL%>%
  filter(Category!="Professions")%>%
  filter(Category!="Land_and_Structures")%>%
  filter(Category!="Naval")%>%
  group_by(Category,Currency)%>%
  ggplot(aes(x=Price,fill=Currency))+
  geom_histogram(bins=30)+
  xlim(0,1000)+
  facet_wrap(~Category)

PL%>%
  filter(Category=="Foodstuffs")%>%
  group_by(Currency)%>%
  mutate(Currency=factor(Currency, levels=c("cp","sp","gp")))%>%
  ggplot(aes(x=Price,fill=Currency))+
  geom_histogram(bins=40)+
  facet_wrap(~Currency)+
  ggtitle("Foodstuff Price Distribution")+
  theme(legend.position = "none",
        plot.title = element_text(size=18))


PL%>%
  filter(Category=="Slaves")%>%
  group_by(Currency)%>%
  mutate(Currency=factor(Currency, levels=c("cp","sp","gp")))%>%
  ggplot(aes(x=Price,fill=Currency))+
  geom_histogram(bins=30)+
  facet_wrap(~Currency)+
  ggtitle("Slaves Price Distribution")+
  theme(legend.position = "none",
        plot.title = element_text(size=18))

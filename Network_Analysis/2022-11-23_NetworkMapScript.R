#####################################################################
# Author: Z.Smith
# Creation Date: 10/24/2022
# Updated: 11/20/2022
# This script is designed to generate a basic network map starting from a .csv fine. The .csv file is assumed to be generated
# from a PCAP file from the tool Wireshark. 
#####################################################################

library(tidyverse)
library(openxlsx)
library(visNetwork)
library(networkD3)
library(gt)
library(DT)
library(plotly)

#Read data in
folder<-"pcap_files"
file<-"5678.csv"

data<-read.csv(paste0(folder,"/",file))

#Create Node List----
#Separate Source and Destination columns to get unique lists
source_u<-data%>%
  select(Source)%>%
  rename(label=Source)%>%
  distinct()

destination_u<-data%>%
  select(Destination)%>%
  rename(label=Destination)%>%
  distinct()

#Combine Source and Destinations into one unique "Node" list
nodes<-rbind(source_u,destination_u)
nodes<-nodes%>%
  distinct()

#Add unique id to each node
nodes<-nodes%>%
  rowid_to_column("id")

#Create Edge List----
#Group by Source and Destination and summarise to get a "weight" column which is the count of how how many times this grouping shows up
edge_1<-data%>%
  group_by(Source,Destination)%>%
  summarise(weight=n())%>%
  ungroup()


#Add unique id by joining with node df
edges<-edge_1%>%
  left_join(nodes,by=c("Source"="label"))%>%
  rename(from=id)

edges<-edges%>%
  left_join(nodes,by=c("Destination"="label"))%>%
  rename(to=id)

#Drop Named COlumns (causes error with network)
edges<-edges%>%select(-Source,-Destination)

rm(destination_u,source_u,edge_1)


#Create an interactive visual----

#Visual network with visNetwork
visNetwork(nodes, edges)

#Adjust so you can see the weights included in visual
# edges <- mutate(edges, width = weight/5 + 1)
edges<-edges%>%
  mutate(color=ifelse(weight<=5,"grey",
                      ifelse(weight>5 & weight<=10,"blue",
                             ifelse(weight > 10 & weight<=20,"green",
                                    ifelse(weight > 20 & weight <=30,"yellow","red")))),
         width=5)
# edges<-edges%>%select(-width)

TestNetworkGraph<-visNetwork(nodes, edges,width = "100%",height="1000") %>% 
  visIgraphLayout(layout = "layout_with_fr") %>% 
  visEdges(arrows = "middle")

visNetwork(nodes,edges,main = "Network Visualization") %>%
  visIgraphLayout(layout = "layout_nicely")%>%
  visEdges(arrows="middle")%>%
  visNodes(color = list(highlight="lightgreen"))%>%
  visOptions(nodesIdSelection=list(enabled = TRUE, 
              style = 'width: 200px; height: 26px;
               background: #f8f8f8;
               color: darkblue;
               border:none;
               outline:none;'),
              highlightNearest = list(enabled = T, degree = 1, hover = T))

#Visualize with networkD3
#Prep
nodes_d3 <- mutate(nodes, id = id - 1)
edges_d3 <- mutate(edges, from = from - 1, to = to - 1)


Test2Network_forceNetwork<-forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
                                        NodeID = "label", Group = "id", Value = "weight", 
                                        opacity = 1, fontSize = 16, zoom = TRUE)


Test2Network_sankeyNetwork<-sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
                                          NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")

sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
              NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")

#Graphs----
# Test2Network_ggraph
TestNetworkGraph
Test2Network_forceNetwork
Test2Network_sankeyNetwork

# #Save Graphs----
# #Save visNetwork graph
# visSave(TestNetworkGraph,paste0("Images/",Sys.Date(),"_","VisNetwork_Test2NetworkGraph.html"))
# 
# #Save networkD3 forceNetwork
# saveNetwork(Test2Network_forceNetwork, "Images/networkD3_Test2NetworkGraph_forceNetwork.html", selfcontained = TRUE)
# 
# #Save netwokD3 sankey
# saveNetwork(Test2Network_sankeyNetwork, "Images/networkD3_Test2NetworkGraph_sankeyNetwork.html", selfcontained = TRUE)


#Scratch Analysis----
data%>%
  View()

data%>%
  group_by(Source,Destination)%>%
  distinct(SRC.Port,DST.Port,Protocol)%>%
  # distinct(Protocol)%>%
  arrange(Source,Destination,Protocol,SRC.Port,DST.Port)%>%
  # arrange(Source,Destination,Protocol)%>%
  View()

data%>%
  group_by(Source,Destination)%>%
  distinct(Protocol)%>%
  arrange(Protocol,Source,Destination)%>%
  ungroup()%>%
  gt()%>%
  tab_header(
    title=md("**Network IP Connections and Protocol**"),
  )%>%
  cols_align(
    align="left"
  )


data%>%
  group_by(Source,SRC.Port,Destination,DST.Port,Protocol)%>%
  summarise(NumberOfConnections=n())%>%
  arrange(Protocol,Source,Destination,SRC.Port,DST.Port)%>%
  ungroup()%>%
  gt()%>%
  tab_header(
    title=md("**Network IP Connections**"),
    subtitle="Number of connections by port and protocol"
  )%>%
  cols_align(
    align="center"
  )

#List of distinct ip and MAC addresses on network
s_distinct<-data%>%
  distinct(Source,Source.MAC)%>%
  rename(ip="Source",MAC="Source.MAC")
d_distinct<-data%>%
  distinct(Destination,Dst.MAC)%>%
  rename(ip="Destination",MAC="Dst.MAC")
devices<-rbind(s_distinct,d_distinct)
devices<-devices%>%
  distinct(ip,MAC)%>%
  arrange(MAC)
rm(s_distinct,d_distinct)

devices%>%
  gt()%>%
  tab_header(
    title=md("**List of ip and associated MAC addresses**"),
    subtitle = md("**Arranged by MAC address**")
  )

#List of protocols used
data%>%
  distinct(Protocol)%>%
  arrange(Protocol)%>%
  gt()%>%
  tab_header(
    title=md("**List of Protocols**")
  )%>%
  cols_align(
    align="center"
  )%>%
  tab_options(
    column_labels.hidden = TRUE
  )

#List of ports by IP and MAC
s_distinct<-data%>%
  distinct(Source,Source.MAC,SRC.Port)%>%
  rename(ip="Source",MAC="Source.MAC",Port="SRC.Port")
d_distinct<-data%>%
  distinct(Destination,Dst.MAC,DST.Port)%>%
  rename(ip="Destination",MAC="Dst.MAC",Port="DST.Port")
devices_wports<-rbind(s_distinct,d_distinct)
devices_wports<-devices_wports%>%
  distinct(ip,MAC,Port)%>%
  arrange(MAC,ip)
rm(s_distinct,d_distinct)

devices_wports%>%
  gt()%>%
  tab_header(
    title=md("**List of ip and MAC addresses with ports used**"),
    subtitle = md("**Ports used in connections in the network**")
  )

#Average Time Between Packages by SOurce and Destination
data%>%
  group_by(Source,Destination)%>%
  summarise(AverageTimeBetweenPackages=mean(Delta))%>%
  ungroup()%>%
  arrange(desc(AverageTimeBetweenPackages))%>%
  gt()%>%
  tab_header(
    title=md("**Average Time Between Packages**"),
    subtitle="Grouped by Source and Destination and arranged in Decending Order by average time"
  )%>%
  cols_align(
    align="center"
  )

#Average Time Between Packages by SOurce and Destination
data%>%
  group_by(Source,Destination)%>%
  summarise(AveragePackageSize=mean(Length),MaxPackageSize=max(Length),MinPackageSize=min(Length))%>%
  ungroup()%>%
  arrange(desc(AveragePackageSize))%>%
  gt()%>%
  tab_header(
    title=md("**Average Package Size**"),
    subtitle="Grouped by Source and Destination and arranged in Decending Order by average size"
  )%>%
  cols_align(
    align="center"
  )


#Visual of Packet Size per Source
data%>%
  group_by(Source,Destination)%>%
  ggplot(aes(x=Source,y=Destination,fill=Length))+
  geom_tile()

data%>%
  group_by(Protocol)%>%
  ggplot(aes(x=Destination,y=Source,size=Length,color=Protocol))+
  geom_point()+
  ggtitle("Packet Size Visual")+
  theme(axis.text.x = element_text(angle = 90,hjust=1,vjust=.25),
        plot.title = element_text(size=22))+
  guides(size=guide_legend(title="Packet Size"))


data%>%
  group_by(Source,Protocol)%>%
  ggplot(aes(x=Length))+
  geom_histogram()+
  facet_wrap(~Protocol)

data%>%
  group_by(Source,Destination,Protocol)%>%
  summarise(AverageSize=mean(Length),MinSize=min(Length),MaxSize=max(Length))%>%
  mutate(StoD=paste(Source,"to",Destination))%>%
  ungroup()%>%
  View()

data%>%
  distinct(Source)



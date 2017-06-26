
library(dplyr)
library(ggplot2)
library(chron)
library(highcharter)
library(ggmap)
library(geosphere)
library(maptools)
library(ggplot2)
library(gpclib)
library(rgdal)
library(devtools)
library(mapproj)
library(leaflet)
library(maps)

#### 2017.06.24/25 #######

cus <- read.csv("/Users/apple/nicole/R code/competition/personal.csv", fileEncoding = "big5")
table(cus$X7.里別) 
table(cus$職稱)
cus %>% 
  filter(職稱 != "照顧秘書") ->A
levels(cus$X17.服務員)

summarise_each(cus, funs(sum(is.na)))
## 算每欄遺漏值
tmp <- lapply(cus, function(x) sum(is.na(x)))  %>% unlist() %>% as.data.frame()

cus[unique(cus$編號),] %>% 
  group_by(X7.里別) %>% 
  summarise(t = n()) %>% 
  ggplot(aes(x=reorder(X7.里別,-t), y=t)) +
  geom_bar(stat = "identity")+ 
  labs(title="各里個案數",x="里",y="個案量")+
  theme_grey(base_family="STHeiti")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

### 里 vs 失能程度 ####
cus %>% 
  group_by(X7.里別) %>% 
  summarise(t=length(unique(X2.案主姓名)) )->C

cus%>% 
  group_by(X7.里別,X21.失能程度) %>% 
  summarise(amount = length(unique(X2.案主姓名)) ) %>% 
  merge(C) %>% 
  ggplot(aes(x=reorder(X7.里別,-t), y=amount, fill = X21.失能程度)) +
  geom_bar(stat = "identity")+ 
  labs(title="各里個案數",x="里",y="個案量")+
  theme_grey(base_family="STHeiti")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  

cus %>% 
  group_by(X17.服務員) %>% 
  summarise(t=length(unique(X2.案主姓名)) )%>% 
  ggplot(aes(x=reorder(X17.服務員,-t), y=t)) +
  geom_bar(fill='snow', color='black',stat = "identity")+ 
  labs(title="各服務員服務個案數",x="服務員",y="個案量")+
  theme_grey(base_family="STHeiti")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


cus[unique(cus$編號),] %>% 
  group_by(X21.失能程度) %>%
  summarise(t = n()) %>% 
  ggplot(aes(x=X21.失能程度, y=t,fill = X21.失能程度)) +
  geom_bar( stat = "identity")+ 
  theme_grey(base_family="STHeiti")+
  labs(title="失能程度",x="失能程度",y="個案量")

### 服務員 vs失能程度 ####
cus %>% 
  group_by(X17.服務員) %>% 
  summarise(t=length(unique(X2.案主姓名)) )->B

cus$X21.失能程度 <- factor(cus$X21.失能程度, levels = levels(cus$X21.失能程度)[order(levels(cus$X21.失能程度), decreasing = TRUE)])
cus$X21.失能程度 <- addNA(cus$X21.失能程度)

cus %>% 
  group_by(X17.服務員,X21.失能程度) %>% 
  summarise(amount = length(unique(X2.案主姓名)) )%>%
  merge(B) %>% 
  ggplot(aes(x = reorder(X17.服務員, -t), y = amount, fill = X21.失能程度)) +
  geom_bar(stat = 'identity', position = 'stack')+ 
  labs(title="各服務員服務個案數",x="服務員",y="個案量")+
  theme_grey(base_family="STHeiti")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

cus %>% 
  filter(X21.失能程度 == "重度") %>% 
  group_by(X17.服務員) %>% 
  summarise(amount = length(unique(X2.案主姓名))) %>% 
  ggplot(aes(x = reorder(X17.服務員, -amount), y = amount)) +
  geom_bar(fill = "#2ca25f",stat = 'identity', position = 'stack')+ 
  labs(title="各服務員服務個案數",x="服務員",y="個案量")+
  scale_colour_discrete(name  ="失能程度" ,labels= "重度") +
  theme_grey(base_family="STHeiti")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


cus %>% 
  filter(職稱 == "照顧秘書") %>% 
  group_by(X17.服務員,X23.障礙程度) %>% 
  summarise(amount = n()) %>% 
  ggplot(aes(x = reorder(X17.服務員,-amount), y = amount, fill = X23.障礙程度)) +
  geom_bar(stat = 'identity', position = 'stack')+ 
  labs(title="各服務員服務個案數",x="服務員",y="個案量")+
  theme_grey(base_family="STHeiti")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

##### 班表 將全部sheet讀入 #######

require(XLConnect)
wb <- loadWorkbook("/Users/apple/nicole/R code/competition/sca_time.xlsx")
lst = readWorksheet(wb, sheet = getSheets(wb))
sheet_names <- getSheets(wb)

timecase <- NULL
for(i in 1:30){
  timecase <- rbind(timecase,readxl::read_excel("/Users/apple/nicole/R code/competition/西屯班表新增照顧員.xlsx" ,sheet = i))
}

timecase %>% 
  mutate(month = substr(Start, start = 1, stop = 2)) ->timecase
# 匯出
write.table( timecase, "班表.csv",row.names = FALSE ,sep = ",", fileEncoding = "big-5")

timecase %>% 
  filter(照顧員=="傅英月",month=="01") -> B 

substr(B$Duration, start = 1,stop = 2) %>% as.numeric() %>% sum()


#### 地圖 ####

sfn <- readOGR(dsn='/Users/apple/nicole/R code/mission/村里界圖',layer="Village_NLSC_1050715")
sfn@data %>% head
sfn@data$CT_Name <- paste0(sfn@data$C_Name,sfn@data$T_Name)
tic=sfn[sfn@data$CT_Name %in% c("臺中市西屯區"),]
sfn.f <- tic %>% fortify(region='V_Name') 

ggplot() +
  geom_map(data=sfn.f, map=sfn.f ,aes(x=long, y=lat, map_id=id), size=0.8, color="black", fill="gray")+
  geom_point(data = cus, aes(x=lng, y=lat, col=X21.失能程度))+
  theme_grey(base_family="STHeiti")+
  theme(panel.border = element_blank(),panel.background = element_blank(),
  axis.ticks = element_blank(),axis.text = element_blank())


hs.map <- get_map(location = "Taiwan", zoom = 12, source = "google")
ggmap(hs.map)


### leaflet #######
getColor <- function(quakes) {
  sapply(cus$X21.失能程度, function(mag) {
    if(mag == "重度") {
      "red"
    } else if(mag == "中度") {
      "blue"
    } else {
      "orange"
    } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(cus)
)

pou <- paste0("個案姓名：", cus$X2.案主姓名,"<br>失能程度：", cus$X21.失能程度)
leaflet(data = tic) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(color="gray", weight = 1, smoothFactor = 0.5,   opacity = 0.8) %>% 
  addCircleMarkers(lng=cus$lng, lat=cus$lat, 
                   radius = 3,color=getColor(cus),popup=pou) 
# 圖標 
addAwesomeMarkers(lng=cus$lng, lat=cus$lat, icon=icons, popup=pou)


###### distence #####

mapdist(c(paste(da[,1])),c(paste(da[,2])),'driving')




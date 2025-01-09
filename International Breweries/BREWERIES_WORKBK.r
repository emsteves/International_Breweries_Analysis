library(tidyverse)


ib
x<-ib$PROFIT
sum(ib$PROFIT)
ib
AngloPhone<-ib%>%filter(COUNTRIES=="Ghana" |COUNTRIES=="Nigeria")
AngloPhone

select(ib,BRANDS,PROFIT,COUNTRIES,MONTHS,YEARS)
ib %>%
  filter(COUNTRIES=="Ghana" | COUNTRIES=="Nigeria")
  
x<-AngloPhone$PROFIT
sum(AngloPhone$PROFIT)

FrancoPhone<-ib%>%filter(COUNTRIES=="Benin" |COUNTRIES=="Senegal" | COUNTRIES=="Togo")
FrancoPhone

x<-FrancoPhone$PROFIT
sum(FrancoPhone$PROFIT)

filter(ib,YEARS==2019)%>%
  x<-ib$PROFIT 
ib%>%
group_by(YEARS==2019, MONTHS=="January"|MONTHS=="February")%>%
  summarise(sum(PROFIT))

ib%>%
  group_by(YEARS==2018)%>%
  summarise(sum(PROFIT))


ib%>%
  group_by(YEARS==2017, MONTHS=="January")%>%
  summarise(sum(PROFIT,TRUE)
##profits
ib%>%
  filter(YEARS==2018)%>%
  arrange(desc(PROFIT))



ib%>%
  filter(YEARS==2019)%>%
  arrange(desc(PROFIT),
##
          
ib%>%
  group_by(YEARS==2019)%>%
  summarise(total=sum(PROFIT))%>%
  group_by(MONTHS)%>%
  summarise(m_total=sum(PROFIT)),

ib%>%
  filter(YEARS==2019)%>%
  group_counts<-table(MONTHS)%>%
  total_count<-sum(group_counts)%>%
percentage_by_group<-(group_counts/total_counts)*100),
##

##Percentage table
MONTHS_PERC <- tibble(
MONTHS=c("January","February","March","April","May","June","July","August","September","October","November","December"),
TOTAL_PROFIT=c(32613160,1366880,2530620,2851470,2573040,2669080,2945340,2982800,1892600,2220870,2675610,2048780),
PERCENT=c(10.87,4.55,8.43,9.50,8.57,8.90,9.81,9.94,6.30,7.40,8.91,6.82),
)
MONTHS_PERC

##Brand Analysis

## top 3 brands

FrancoPhone%>%
  group_by(YEARS!=2018,BRANDS=="Hero")%>%
  summarise(sum(QUANTITY))
FrancoPhone%>%
  group_by(YEARS!=2018,BRANDS=="Trophy")%>%
  summarise(sum(QUANTITY))
 

FrancoPhone%>%
  filter(YEARS!=2017 |BRANDS=="Hero")%>%
  arrange(desc(QUANTITY))

##Top2 choice in Ghana
 
AngloPhone%>%
  filter(COUNTRIES=="Ghana")%>%
  arrange(desc(QUANTITY))
  
##beer consumed

beer<-AngloPhone%>%filter(BRANDS=="hero" | BRANDS=="trophy" | BRANDS=="castle lite" | BRANDS=="eagle lager" | BRANDS=="budweiser")
x<-beer$QUANTITY
sum(beer$QUANTITY)
beer
##malt choice in anglophone countries

AngloPhone%>%
  group_by(BRANDS)%>%
  summarise(sum(QUANTITY))
## highest sale in 2019

AngloPhone%>%
  group_by(YEARS==2019, BRANDS, COUNTRIES=="Nigeria")%>%
  summarise(sum(QUANTITY))
##
AngloPhone%>%
  group_by(YEARS==2019, BRANDS, COUNTRIES=="Nigeria",REGION=="southsouth")%>%
  summarise(sum(QUANTITY)),
##beer consumed in Nigeria
beer_n<-beer%>%filter(COUNTRIES=="Nigeria")
x<-beer_n$QUANTITY
sum(beer_n$QUANTITY)
beer_n

beer_n%>%
  group_by(BRANDS=="budweiser",REGION)%>%
  summarise(level_of_consumption=sum(QUANTITY))

## country analysis

##1

beer_c<-ib%>%filter(BRANDS=="hero" | BRANDS=="trophy" | BRANDS=="castle lite" | BRANDS=="eagle lager" | BRANDS=="budweiser")
x<-beer_c$QUANTITY
sum(beer_c$QUANTITY)

beer_c%>%
  group_by(BRANDS,COUNTRIES)%>%
  summarise(Consumption=sum(QUANTITY))

beer_c%>%
  group_by(COUNTRIES)%>%
  summarise(Consumption=sum(QUANTITY))
##2

FrancoPhone%>%
  group_by(SALES_REP,COUNTRIES=="Senegal")%>%
  summarise(sales=sum(QUANTITY))

##3 

ib%>%
  group_by(COUNTRIES,MONTHS)%>%
  summarise(Profit_Q=sum(PROFIT))

fourth_q<-ib%>%filter(YEARS==2019, MONTHS=="October"|MONTHS=="November"|MONTHS=="December")
fourth_q

fourth_q%>%
  group_by(COUNTRIES,MONTHS)%>%
  summarise(Profit_q=sum(PROFIT))
  
fourth_q%>%
  group_by(COUNTRIES)%>%
  summarise(sum(PROFIT))

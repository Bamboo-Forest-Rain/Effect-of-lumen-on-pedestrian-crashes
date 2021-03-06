---
title: "Modeling pedestrian safety in dark conditions in Philadelphia"
author: "Yihong Hu & Shujing Yi"
date: "4/22/2022"
output:
  html_document:
    keep_md: yes
    toc: yes
    theme: united
    toc_float: yes
    code_folding: hide
    number_sections: yes
    df_print: paged
    fig_align: 'center'
  pdf_document:
    toc: yes
---



# Introduction

Pedestrian fatalities have been accelerating for the past decade in the US. Fatality and Injury Reporting System (FARS) reports that 6515 pedestrians died in vehicle crashes in 2020, accounting for 18% of traffic fatalities. The number grew by 50% from 2010, when total traffic fatalities have only increased by 18%. 

In addition, 75% of these pedestrian crashes occurred during dark conditions. This is a more critical issue in low-income neighborhoods where most people would at least walk for a portion of their commuting trips. They include seniors and children, who rely on walking and public transit to access food, health, education, and other public services. It is also an equity issue. A 2022 study reveals that Black and Native American pedestrians have much higher chance to involve in fatal crashes at night comparing to other races. 

In Philadelphia, between 2016 to 2020, around 33% of the pedestrian crashes happened over night (18:00 - 7:00). The number jumps to 62% when looking into fatality cases. 



```r
#Philly Street Light Poles
Street_Poles_Data <- st_read("https://opendata.arcgis.com/datasets/9059a7546b6a4d658bef9ce9c84e4b03_0.geojson") 

Street_Poles_Lumen <- Street_Poles_Data %>% select(LUM_SIZE) %>% na.omit() %>% st_transform(crs = 4326)

# Philly Block Group Census
Philly_Census <- get_acs(geography = "block group",
                         state = 42,
                         county = 101,
                         year = 2020,
                         geometry = TRUE,
                         output="wide",
                         variable =(c(Pop = "B19013_001",
                                      PopBelow5M = "B01001_003",
                                      Pop_5_9M = "B01001_004",
                                      PopBeloq5F = "B01001_027",
                                      Pop_5_9F = "B01001_028")))

Philly_Pop_Above60 <- get_acs(geography = "block group",
                         state = 42,
                         county = 101,
                         year = 2020,
                         geometry = TRUE,
                         output = "wide",
                         variable =(c("B01001_018"	,
                                      "B01001_019"	,
                                      "B01001_020"	,
                                      "B01001_021"	,
                                      "B01001_022"	,
                                      "B01001_023"	,
                                      "B01001_024"	,
                                      "B01001_025",
                                      "B01001_042"	,
                                      "B01001_043"	,
                                      "B01001_044"	,
                                      "B01001_045"	,
                                      "B01001_046"	,
                                      "B01001_047"	,
                                      "B01001_048"	,
                                      "B01001_049"	)))

Philly_commute <- get_acs(geography = "tract",
                         state = 42,
                         county = 101,
                         output = "wide",
                         year = 2020,
                         geometry = TRUE,
                         variable =(c(Bike	=	"B08006_014"	,
                                      Walk	=	"B08006_015"	,
                                      Public_Trans	=	"B08006_008")))

Philly_Race <- read.csv("2020Census.csv")
Philly_Race$GEO_ID<-gsub("1500000US","",as.character(Philly_Race$GEO_ID))

Report_311_2020 <-
  read.csv("public_cases_2020.csv")%>%
  filter(service_name=="Street Light Outage")

Report_311_2019 <-
  read.csv("public_cases_2019.csv") %>%
  filter(service_name=="Street Light Outage")

Report_311_2018 <-
  read.csv("public_cases_fc_2018.csv") %>%
  filter(service_name=="Street Light Outage")

Report_311_2017 <-
  read.csv("public_cases_fc_2017.csv") %>%
  filter(service_name=="Street Light Outage")

Report_311_2016 <-
  read.csv("public_cases_fc_2016.csv") %>%
  filter(service_name=="Street Light Outage")

Report_light_out_full <-
  rbind(Report_311_2018,Report_311_2019,Report_311_2020,Report_311_2017,Report_311_2016)%>%na.omit %>%
  st_as_sf(coords = c("lon","lat"),crs = 4326) %>%
  distinct(updated_datetime, .keep_all = T) 

#Open Case with lumen
Report_light_out_open <-
  Report_light_out_full %>%
  filter(status == "Open")

# Crash Data between 2018 - 2020
Crash <-
  st_read("https://opendata.arcgis.com/api/v3/datasets/e703eb63ec484aa6beae1268372efa53_0/downloads/data?format=geojson&spatialRefId=4326")

head(Crash)

Crash_Ped <-
  Crash %>%
  filter(PED_COUNT > 0)

Crash_Ped$HOUR_OF_DA <- as.character(Crash_Ped$HOUR_OF_DA)

# Hour of crashes
Crash_Ped %>%
  mutate(HOUR_DA_FACTOR = factor(HOUR_OF_DA, levels = c(as.character(seq(0,24)))))%>%
  group_by(HOUR_DA_FACTOR)%>%
  tally()%>%
  group_by(HOUR_DA_FACTOR)%>%
  summarise(SUM_HOUR = sum(n)) %>%
  filter(!HOUR_DA_FACTOR == "99")%>%
ggplot(aes(HOUR_DA_FACTOR, SUM_HOUR, fill = "steelblue" )) + 
         geom_bar(position = "dodge", stat = "summary", fun.y = "mean")+ xlab("Hour of the day") + ylab("Total Crash") + labs(title = "Pedestrian crashes by hours between 2016 - 2020") + theme_minimal() + theme(legend.position = "none")
```

<img src="Final_Project_files/figure-html/cars-1.png" width="60%" height="60%" style="display: block; margin: auto;" />



```r
#Pedestrian crashes by time of the day
Crash_Ped$HOUR_OF_DA <- as.numeric(Crash_Ped$HOUR_OF_DA)

Crash_Ped_PennDot_sum <-
  Crash_Ped %>%
  mutate(TIME_OF_DAY = case_when(HOUR_OF_DA < 7 | HOUR_OF_DA > 18 ~ "Overnight",
                                 HOUR_OF_DA >= 7 & HOUR_OF_DA < 10 ~ "AM Rush",
                                 HOUR_OF_DA >= 10 & HOUR_OF_DA < 15 ~ "Mid-Day",
                                 HOUR_OF_DA >= 15 & HOUR_OF_DA <= 18 ~ "PM Rush"))%>%
  group_by(HOUR_OF_DA, TIME_OF_DAY) %>%
  tally() %>%
  group_by(TIME_OF_DAY) %>%
  summarise(sum_crash = sum(n))%>%
  mutate(pct_crash = sum_crash/sum(sum_crash)*100)

Crash_Ped_PennDot_sum %>%
  ggplot(aes(TIME_OF_DAY, sum_crash))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE, fill = "wheat2", width = 0.5) + xlab("Time of the day")+ylab("Total Crash Counts") + labs(title = "Pedestrian Crashes by Time of the Day") + theme_minimal()
```

<img src="Final_Project_files/figure-html/catagorical-1.png" width="60%" height="60%" style="display: block; margin: auto;" />



```r
#Fatal Pedestrian Crashes by time of the day

Crash_Ped_PennDot_Fatal <-
  Crash_Ped %>%
  filter(FATAL_COUN > 0)

Crash_Ped_PennDot_Fatal_sum <-
Crash_Ped_PennDot_Fatal %>%
  mutate(TIME_OF_DAY = case_when(HOUR_OF_DA < 7 | HOUR_OF_DA > 18 ~ "Overnight",
                                 HOUR_OF_DA >= 7 & HOUR_OF_DA < 10 ~ "AM Rush",
                                 HOUR_OF_DA >= 10 & HOUR_OF_DA < 15 ~ "Mid-Day",
                                 HOUR_OF_DA >= 15 & HOUR_OF_DA <= 18 ~ "PM Rush"))%>%
  group_by(HOUR_OF_DA, TIME_OF_DAY) %>%
  tally() %>%
  group_by(TIME_OF_DAY) %>%
  summarise(sum_crash = sum(n))%>%
  mutate(pct_crash = sum_crash/sum(sum_crash)*100)

Crash_Ped_PennDot_Fatal_sum %>%
  ggplot(aes(TIME_OF_DAY, sum_crash, fill = TIME_OF_DAY))+
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean", show.legend = FALSE, fill = "tomato2", width = 0.5) + xlab("Time of the day")+ylab("Total Crash Counts") + labs(title = "Fatal Pedestrian Crashes by Time of the Day", fill = "Time of the Day")+theme_minimal()+scale_fill_brewer(palette="PuRd")
```

<img src="Final_Project_files/figure-html/unnamed-chunk-1-1.png" width="60%" height="60%" style="display: block; margin: auto;" />

The goal of this report is to discover the correlation of physical, demographic, and environmental factors to the number of pedestrian crashes at night in Philadelphia, PA. This report seeks to see whether there is a need to improve pedestrian infrastructure at night. The data is gathered from PennDOT, OpenData Philly, Philadelphia Tree Inventory, Philadelphia Police District, and 5-year American Community Survey. The first section discusses the lighting conditions in Philadelphia. Area brightness is thought to be one of the major factors contributing to night pedestrian crashes.

The second section introduces the analysis method, this include data collection process and regression models that are chosen for the analysis.

The third section displays the results, and the forth section interprets the results and draws implications. Lastly, this report ends with a conclusion.  

## Mapping Street Light Poles in Philly

The map below shows the brightness given by the street poles in Philadelphia. Center City has the brightest lights. However, overall, majority of the city is still dark, with lumen sizes below 100. 



```r
#Philly_boundary
Philly_boundary <-
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson")%>%
  st_transform(crs = 4326)

#Mapping out Lumens in Philly
ggplot()+
  geom_sf(data = Philly_boundary,fill = "white", color = "grey75")+
  geom_sf(data = Street_Poles_Lumen, aes(color = LUM_SIZE),alpha=0.5) +  labs(x = NULL, 
         y = NULL, 
         title = "Light Brightness by Lumen in Philadelphia", 
         caption = "Fig. 1",
          color = "Lumens") + scale_color_viridis_b(option = "A")+theme_map()
```

![](Final_Project_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The red dots on the map below are disfunctional lights. We filtered out lights that have 0 as their lumen size and lights that are reported broken by 311 calls. They will be excluded from the analysis.  


```r
#Mapping out dis-functioning lights

lum_0 <-
  Street_Poles_Lumen %>%
  filter(LUM_SIZE == 0) 

lum_0_geo <-
  lum_0 %>%
  select(geometry)

disfunction_light_geo <-
  Report_light_out_open %>%
  select(geometry) %>%
  rbind(.,lum_0_geo)

ggplot()+
  geom_sf(data = Philly_boundary,color = "grey75", fill = "white")+
  geom_sf(data = Street_Poles_Lumen,color = "aquamarine3",alpha=0.5) +  
  geom_sf(data = disfunction_light_geo,color = "red", alpha = 0.5 ) +
  labs(x = NULL, 
         y = NULL, 
         title = "Disfunctioning light in Philly", 
         caption = "Fig. 2",  color = "Lumens") + theme_map()
```

![](Final_Project_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

# Method

## Data Collection

The crash data is given by PennDOT on its website. We also collected and caluclated various relevant variables that might be associated with pedestrian crashes in an given area. To summerize, we collected data from:

| (1) PennDOT public crash data 2016 - 2020
|           a. Crash count 
|                       i. Pedestrian crashes during dark conditions and daylight
|                      ii. Fatal pedestrian crashes in dark conditions
|                     iii. Fatal pedestrian crashes during daylight
|                      iv. Total Pedestrian crashes
|           b. Driver age
|           c. Speed Limit
|           d. Binary variables
|                       i. Presence of drunk drivers 
|                      ii. Presence of young drivers 
|                     iii. Presence of clear weather
|                      iv. Whether the creash occured on weekend
|                       v. Pedestrian Gender (Female = 1, Male = 0)
|
| (2) Street Light Poles - OpenData Philly
|           a. Lumen size
|           b. Light pole count
|  
| (3) 2020 5-year block level American Community Survey (ACS):
|           a. Total Population
|           b. Age (Below 10 and Above 65) 
|           c. Black, White, Asian Population
|           d. Percent of Black, White, Aisian Population
|           e. Commute Pattern (People who choose to commute with car versus other modes)
|  
| (4) Philadelphia Tree Inventory 2021
|           a. Tree Count
|
| (5) Philadelphia Police Department 2016 - 2020
|           a. Crime Count
|           b. Crime Rate (based on the ACS population data)

## Multivariant Regression

Part I evaluates the variables that contribute to the possibility of a night pedestrian crash. We chose to develop a multivariate regression model, because it considers the interaction among all relevant variables. Each variable is evaluated while holding other relevant variables constant.

## Binomial Logistic Regression

Part II evaluates binomial outcomes in three ways: 
  1. The odds of a fatal pedestrian crash versus non-fatal crash in dark conditions.
  2. The odds of a fatal pedestrian crash versus non-fatal crash during daylight.
  3. The odds of a pedestrian crash in dark conditions versus non-dark conditions 

The purpose of this analysis is to see if there is really a difference in the factors that contribute to fatal crashes in dark conditions or during daylight. 

In addition to fatal crashes, we also want to see what factors contribute to a dark-condition pedestrian crash the most. 

This part of the analysis follows the method of another study by University of Wisconsin and Safe Streets Research & Consulting in California surrounding the issue of night pedestrian safety. 

We believe that binomial logistic regression is appropriate in this case. Multivariate regression or bi-variate regression cannot deliver accurate results due to limited cases of fatal pedestrian crashes at Philly. 

**The variables will be selected through backward selection method and tested by variance inflation factor (vif) to eliminate co-founding variables.**



## Fishent

The first step to our analysis is to build a fishnet over Philadelphia. Each cell is 500ft * 500ft. Fishnet creates grids that smooths out the irregular boundaries given by census data. 



### Data wrangling


#### Light Counts

The map below shows the street light pole counts in each fishnet cell.


```r
#fishnet
Philly_boundary2 <-
  st_read("https://opendata.arcgis.com/datasets/405ec3da942d4e20869d4e1449a2be48_0.geojson")%>%
  st_transform("ESRI:102271")

fishnet <- 
  st_make_grid(Philly_boundary2,
               cellsize = 500, 
               square = TRUE) %>%
  .[Philly_boundary2] %>%            
  st_sf() %>%
  mutate(uniqueID = rownames(.))%>%
  st_transform(crs = 4326)

#Light in Fishnet
light_net <- 
  Street_Poles_Lumen %>%
  st_transform(crs = 4326)%>%
  filter(!LUM_SIZE == 0)%>%
  select(-LUM_SIZE) %>%
  mutate(countLight = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countLight = replace_na(countLight, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

#Light Count in each fishnet cell
ggplot() +
  geom_sf(data = light_net, aes(fill = countLight), color = NA) +
  scale_fill_viridis_b(option = "A") +
  labs(title = "Street Light Pole Count in each Fishnet Cell", fill = "Street Pole\n Count", caption="Fig.3") +
  theme_map()
```

![](Final_Project_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

It is not surprising that Center City has the highest number of street lights. However, this map does not correspond to Figure 1. North of Center City has 100 to 200 street poles, most of the them are low in lumen sizes. 

#### Crash Counts

Fig 4 shows the pedestrian crash count in each fishnet cell between 2016 - 2020.


```r
Agg_age_below_10_f <-
  Philly_Census %>%
  mutate(Age_below_10 = PopBelow5ME + Pop_5_9ME + PopBeloq5FE +Pop_5_9FE)%>%
  select(-ends_with("M"),-ends_with("E"))%>%
  st_transform(crs = 4326)%>%
  mutate(Legend = "Below10") %>%
  rename(Count = Age_below_10)%>%
  select(GEOID,Count,Legend) %>%
  st_centroid() %>%
  st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(CountBelow10 = sum(Count))

fishnet1 <-
  Agg_age_below_10_f %>%
  st_drop_geometry()%>%
  left_join(fishnet)%>%
  st_as_sf

Agg_above_60 <-
  Philly_Pop_Above60 %>%
  mutate(Age_Above60 = B01001_018E + B01001_019E +
                                      B01001_020E+                
                                      B01001_021E +
                                      B01001_022E +
                                      B01001_023E+
                                      B01001_024E+
                                      B01001_025E+
                                      B01001_042E+
                                      B01001_043E+
                                      B01001_044E+
                                      B01001_045E+
                                      B01001_046E+
                                      B01001_047E+
                                      B01001_048E+
                                      B01001_049E)%>%
  select(GEOID,geometry,Age_Above60) %>%
  st_transform(crs = 4326)%>%
  mutate(Legend = "Above60") %>%
  rename(Count = Age_Above60)%>%
  select(GEOID,Count,Legend) %>%
  st_centroid()%>%
   st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(CountAbove60 = sum(Count))

fishnet2 <-
  Agg_above_60 %>%
  st_drop_geometry()%>%
  left_join(fishnet)%>%
  st_as_sf()

No_car_commute <- 
  Philly_commute %>%
  mutate(Non_auto_commute = BikeE, Public_TransE, WalkE) %>%
  select(geometry, Non_auto_commute, GEOID) %>%
  st_transform(crs = 4326) %>%
  mutate(Legend = "NoCar") %>%
  rename(Count = Non_auto_commute)%>%
  select(GEOID,Count,Legend) %>%
  st_centroid()%>%
   st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(CountNoCar = sum(Count))

fishnet3 <-
No_car_commute %>% 
  st_drop_geometry()%>%
  left_join(fishnet)%>%
  st_as_sf()
  

#Crash Count in each block Group

Crash_geo <-
Crash_Ped %>%
  select(geometry) %>%
  st_transform(crs = 4326)%>%
  mutate(CrashCount = 1)%>%
  st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(CrashCount = sum(CrashCount))

fishnet4 <-
  Crash_geo %>% 
  st_drop_geometry()%>%
  left_join(fishnet)%>%
  st_as_sf()

ggplot()+
  geom_sf(data = Philly_boundary,color = "grey75", fill = "white")+
  geom_sf(data = fishnet4, aes(fill = CrashCount), color = "grey50")+
  scale_fill_viridis_b(option = "A", direction = -1,na.value = "grey30") + labs(fill = "Crash Count",title = "Pedestrian Crashes per Fishnet Cell\n 2016 - 2020", caption = "Fig. 4") + theme_map()
```

![](Final_Project_files/figure-html/data cleaning-1.png)<!-- -->

Most crashes occurred in Center City, perhaps because center city has the highest traffic flow and the densest population. We can also observe that most crashes occurred along Broad Street, Market Street, and Roosevelt Boulevard.


```r
Crash_dark <-
Crash_Ped %>%
  filter(ILLUMINATI == 2 | ILLUMINATI ==3) %>%
  select(geometry) %>%
  st_transform(crs = 4326)%>%
  mutate(CrashCount = 1)%>%
  st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(DarkConditionCount = sum(CrashCount))

fishnet12 <-
  Crash_dark %>% 
  st_drop_geometry()%>%
  left_join(fishnet)%>%
  st_as_sf()

ggplot()+
  geom_sf(data = Philly_boundary,color = "grey75", fill = "white")+
  geom_sf(data = fishnet12, aes(fill = DarkConditionCount), color = "grey50")+
  scale_fill_viridis_b(option = "A", direction = -1,na.value = "grey30") + labs(fill = "Crash Count",title = "Pedestrian Crashes over dark conditions\n per Fishnet Cell\n 2016 - 2020", caption = "Fig. 5") + theme_map()
```

![](Final_Project_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

When looking into crashes that happened over dark conditions, the cases were more spread out over the city. 


#### Other data

We wrangled other data and dropped them into fishnet cell. These include:
For ACS group level data, we attached the data to the center point of the block group, and spatial joined it with the fishnet. These datasets include information on trees, age, race, and population.

We also extracted "flag" variables that we think are most relevant to the analysis. They are crashes related to drinking drivers, young drivers, weather, and weekends.


```r
#Read Tree Data
Tree <-
read.csv("PPR_Tree_Inventory_2021.csv")%>%
  st_as_sf(coords = c("LOC_X","LOC_Y"),crs = 4326) %>%
  st_transform(crs = 4326)%>%
  mutate(TreeCount = 1)

fishnet_5 <-
  Tree %>%
  st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(CountTree = sum(TreeCount))%>% st_drop_geometry()%>%
  left_join(fishnet)%>%
  st_as_sf()
```


```r
#Read Demographic Data
block_geo <-
  Philly_Census %>%
  select(geometry,GEOID)

Philly_Race <-
Philly_Race %>%
  rename(GEOID=GEO_ID)%>%
  left_join(block_geo)%>%
  st_as_sf()
  
Philly_Race <-
  Philly_Race %>%
  st_transform(crs = 4326)

Philly_White <-
  Philly_Race %>%
  select(WHITE,GEOID)%>%
  dplyr::rename(Count = WHITE)%>%
  st_centroid()%>%
   st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
   dplyr::summarize(CountWhite = sum(Count))

fishnet6 <-
  Philly_White %>%
  st_drop_geometry()%>%
  left_join(.,fishnet)%>%
  st_as_sf()
  
Philly_Black <-
 Philly_Race %>%
  select(BLACK,GEOID)%>%
  rename(Count = BLACK)%>%
  st_centroid()%>%
   st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(CountBlack = sum(Count))

fishnet10 <-
  Philly_Black %>%
  st_drop_geometry()%>%
  left_join(.,fishnet)%>%
  st_as_sf()

Philly_Asian <-
  Philly_Race %>%
  select(ASIAN,GEOID)%>%
  rename(Count = ASIAN)%>%
  st_centroid()%>%
   st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(CountAsian = sum(Count))

fishnet7 <-
  Philly_Asian %>%
  st_drop_geometry()%>%
  left_join(.,fishnet)%>%
  st_as_sf()

Philly_Total <-
   Philly_Race %>%
  select(TOTAL,GEOID)%>%
  rename(Count = TOTAL)%>%
  st_centroid()%>%
   st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(CountTotal = sum(Count))

fishnet8 <-
  Philly_Total %>%
  st_drop_geometry()%>%
  left_join(.,fishnet)%>%
  st_as_sf()
```


```r
#Read Crime Data
Crime_2020 <- read.csv("Crime_Incidents_2020.csv")

colnamCleaning<-c("the_geom", "cartodb_id", "the_geom_webmercator")
Crime_2020 <-Crime_2020[ , -which(names(Crime_2020) %in% colnamCleaning)]

Crime_2019 <- read.csv("Crime_Incidents_2019.csv")
Crime_2018 <- read.csv("Crime_Incidents_2018.csv")
Crime_2017 <- read.csv("Crime_Incidents_2017.csv")
Crime_2016 <- read.csv("Crime_Incidents_2016.csv")

#Crime <-
 # rbind(Crime_2020,Crime_2019,Crime_2018, Crime_2017, Crime_2016) %>%na.omit()%>%
#   st_as_sf(coords = c("point_x","point_y"),crs = 4326)%>%
 # mutate(CrimeCount=1)%>%
#  st_join(fishnet)%>%
 # group_by(uniqueID)%>%
  # summarise(CountCrime = sum(CrimeCount))

Crime <-
  read.csv("crime_st_drop.csv")

Crime$uniqueID <- as.character(Crime$uniqueID)

fishnet9 <-
  Crime %>%
  left_join(.,fishnet)%>%
  select(-X) %>%
  st_as_sf()

ggplot()+
  geom_sf(data = Philly_boundary)+
  geom_sf(data = fishnet9, aes(fill = CountCrime))+
  scale_fill_viridis(option = "D", direction = -1)+
  labs(title = "Crime Count in Philly") +
  theme_map()
```

![](Final_Project_files/figure-html/crime-1.png)<!-- -->


```r
# Filter out fatal pedestrian crashes

Ped_Fatal_fish <-
  Crash_Ped_PennDot_Fatal %>%
  st_as_sf(coords = c("LONGITUDE","LATITUDE"),crs = 4326) %>%
  mutate(Count =1 )%>%
  st_join(fishnet)%>%
  group_by(uniqueID) %>%
  summarise(FatalCount = sum(Count))

fishnet11 <-
  Ped_Fatal_fish %>%
  st_drop_geometry()%>%
  left_join(.,fishnet)%>%
  st_as_sf()
```


```r
# Read driver and pedestrian information

crash_flag_2016 <- read.csv("FLAG_PHILADELPHIA_2016.csv")
crash_flag_2017 <- read.csv("FLAG_PHILADELPHIA_2017.csv")
crash_flag_2018 <- read.csv("FLAG_PHILADELPHIA_2018.csv")
crash_flag_2019 <- read.csv("FLAG_PHILADELPHIA_2019.csv")
crash_flag_2020 <- read.csv("FLAG_PHILADELPHIA_2020.csv")

#Mutate a column with a binary variable indicating the presence of a young driver
crash_flag_all <- rbind(crash_flag_2016,crash_flag_2017,crash_flag_2018,crash_flag_2019,crash_flag_2020) %>%
  select(CRN, DRINKING_DRIVER, DRIVER_16YR, DRIVER_17YR, DRIVER_18YR,DRIVER_19YR,DRIVER_20YR)%>%
  mutate(young_driver = case_when(DRIVER_16YR == 1 ~ 1,
                                  DRIVER_17YR == 1 ~ 1,
                                  DRIVER_18YR == 1 ~ 1,
                                  DRIVER_19YR == 1 ~ 1,
                                  DRIVER_20YR == 1 ~ 1,
                                  TRUE ~ 0 ))%>%
  select(CRN, young_driver, DRINKING_DRIVER)

Crash_Ped_drink_young <-
  left_join(Crash_Ped, crash_flag_all, by = "CRN")

# Variable indicating a drunk driver
Crash_Fish_drunk <-
  Crash_Ped_drink_young %>%
  filter(DRINKING_DRIVER == 1) %>%
    select(geometry) %>%
  st_transform(crs = 4326)%>%
  mutate(CrashCount = 1)%>%
  st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(CountDrunk = sum(CrashCount))

fishnet17 <-
  Crash_Fish_drunk %>% 
  st_drop_geometry()%>%
  left_join(fishnet)%>%
  st_as_sf()

Crash_young <-
   Crash_Ped_drink_young %>%
  filter(young_driver == 1) %>%
    select(geometry) %>%
  st_transform(crs = 4326)%>%
  mutate(CrashCount = 1)%>%
  st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(CountYoung = sum(CrashCount))

fishnet13 <-
  Crash_young %>% 
  st_drop_geometry()%>%
  left_join(fishnet)%>%
  st_as_sf()
```

```r
#Speed Limit
crash_road_2016 <- read.csv("ROADWAY_PHILADELPHIA_2016.csv")
crash_road_2017 <- read.csv("ROADWAY_PHILADELPHIA_2017.csv")
crash_road_2018 <- read.csv("ROADWAY_PHILADELPHIA_2018.csv")
crash_road_2019 <- read.csv("ROADWAY_PHILADELPHIA_2019.csv")
crash_road_2020 <- read.csv("ROADWAY_PHILADELPHIA_2020.csv")

crash_road_all <- rbind(crash_road_2016,crash_road_2017,crash_road_2018,crash_road_2019,crash_road_2020) %>%
  select(CRN, SPEED_LIMIT) %>% na.omit %>% distinct(CRN, .keep_all = TRUE)

Crash_Ped_Road <-
  left_join(Crash_Ped, crash_road_all, by = "CRN")

Crash_Speed <-
  Crash_Ped_Road %>%
   st_transform(crs = 4326)%>%
  st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(MeanSpeed = mean(SPEED_LIMIT))
  

fishnet14 <-
  Crash_Speed %>% 
  st_drop_geometry()%>%
  left_join(fishnet)%>%
  st_as_sf()
```


```r
#Weather 
Crash_sunny <-
  Crash_Ped %>%
  filter(WEATHER == 1) %>%
    select(geometry) %>%
  st_transform(crs = 4326)%>%
  mutate(CrashCount = 1)%>%
  st_join(.,fishnet)%>%
  group_by(uniqueID) %>%
  summarize(SunnyCount = sum(CrashCount))

fishnet15 <-
  Crash_sunny %>% 
  st_drop_geometry()%>%
  left_join(fishnet)%>%
  st_as_sf()
```

```r
Crash_Ped <-
  Crash_Ped %>%
  mutate(weekend = case_when(DAY_OF_WEE == 6 | DAY_OF_WEE == 7 ~ 1, TRUE ~ 0))

Crash_weekend <-
  Crash_Ped %>%
  filter(weekend == 1) %>%
  mutate(WeekendCount = 1) %>% 
  st_join(fishnet) %>%
  group_by(uniqueID) %>%
  summarise(WeekendCount = sum(WeekendCount)) 

fishnet16 <-
  Crash_weekend %>% 
  st_drop_geometry()%>%
  left_join(fishnet)%>%
  st_as_sf()
```


```r
# Creating Final Fishnet
library(purrr)

light_net2 <-
  Street_Poles_Lumen %>%
  filter(!LUM_SIZE ==0) %>%
  mutate(Light = 1) %>%
  st_join(.,fishnet)

getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}


light_net2 <- light_net2 %>% 
  group_by(uniqueID)%>%
  dplyr::summarise(mean_lumen = mean(LUM_SIZE),
                   mode_lumen = getmode(LUM_SIZE),
                   LightCount = sum(Light))

dfs <- list(
  st_drop_geometry(fishnet1), st_drop_geometry(fishnet2), st_drop_geometry(fishnet3), st_drop_geometry(fishnet4), st_drop_geometry(fishnet_5),st_drop_geometry(fishnet6),st_drop_geometry(fishnet7),st_drop_geometry(fishnet8),st_drop_geometry(fishnet9),st_drop_geometry(fishnet10),st_drop_geometry(light_net2),st_drop_geometry(fishnet11),st_drop_geometry(fishnet12), st_drop_geometry(fishnet13),st_drop_geometry(fishnet14), st_drop_geometry(fishnet15), st_drop_geometry(fishnet16),st_drop_geometry(fishnet17))
  
  
fishnet_final <-
  purrr::reduce(dfs, dplyr::left_join, by = 'uniqueID')%>%
  right_join(fishnet)%>%
  st_as_sf()

fishnet_final[is.na(fishnet_final)] <- 0

fishnet_final2 <-
  gather(fishnet_final, Variable,value, -geometry,-uniqueID)

#vars <- unique(fishnet_final2$Variable)

#mapList <- list()
#for(i in vars){
#  mapList[[i]] <- 
#    ggplot() +
#      geom_sf(data = filter(fishnet_final2, Variable == i), #aes(fill=value), colour=NA) +
#      scale_fill_viridis_b(option = "A", direction = -1) +
#      labs(title=i) + theme_map()}

#library(grid)
#library(gridExtra)

#do.call(grid.arrange,c(mapList, ncol=3, top="Variable Counts by Fishnet", bottom = "Fig.6"))
```



### Correlation

The correlation among variables are displayed in the correlation plot below by Pearson's R. The darker the red, the stronger the positive correlation. 


```r
library(ggcorrplot)

fishnet_nu <-
  fishnet_final %>%
  st_drop_geometry()%>%
  select(-uniqueID, -X)

cor(fishnet_nu)%>%
  ggcorrplot(hc.order = TRUE,
                      outline.color = "white")
```

![](Final_Project_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



The correlation plot shows that pedestrian crashes are strongly correlated with crime. The correlation between pedestrian crashes occurred during dark condition and total pedestrian crashes is strong, suggesting that the proportion of night crashes over total crashes is consistent throughout the fishnet.


We can look into pedestrian crashes occurred in dark condition and in daylight and other time separately.


```r
Crash_Ped_all <-
  merge(Crash_Ped, st_drop_geometry(Crash_Ped_drink_young))

Crash_Ped_all <- Crash_Ped_all %>% mutate(dark = case_when(ILLUMINATI == 2 | ILLUMINATI == 3 ~ 1, TRUE ~ 0))
                                          
Crash_Ped_all <- Crash_Ped_all %>% 
  mutate(sunny = case_when(WEATHER == 1 ~ 1, TRUE ~ 0))

Crash_Ped_all <- crash_road_all %>% 
  select(SPEED_LIMIT,CRN) %>%
  right_join(Crash_Ped_all)

Crash_Ped_all <- Crash_Ped_all %>%st_as_sf %>%
  st_transform(crs = 4326) %>%
  st_join(fishnet_final)

Crash_Ped_night <-
  Crash_Ped_all %>%
  filter(dark == 1)

Crash_Ped_daylight <-
  Crash_Ped_all %>%
  filter(dark == 0)

Crash_Ped_Fish_dark <-
  Crash_Ped_night %>%
  st_join(fishnet_final) 

Crash_Ped_Fish_daylight <-
  Crash_Ped_daylight %>%
  st_join(fishnet_final)

Crash_Ped_Fish_crash <-
  Crash_Ped_night %>%
  mutate(NightCrash = 1) %>%
  group_by(uniqueID) %>%
  summarise(NightCrash = sum(NightCrash))

Crash_Ped_Fish_crash <-
  Crash_Ped_Fish_crash %>%
  st_drop_geometry() %>%
  right_join(fishnet_final)%>%
  st_as_sf()

Crash_Ped_Fish_crash$NightCrash[is.na(Crash_Ped_Fish_crash$NightCrash)] <- 0

Crash_Ped_Fish_crash <-
  Crash_Ped_Fish_crash %>%
  st_drop_geometry() %>%
  left_join(light_net2) %>%
  st_as_sf

Crash_Ped_Fish_crash$LightCount[is.na(Crash_Ped_Fish_crash$LightCount)] <- 0
  

fishnet_nu_2 <-
  Crash_Ped_Fish_crash %>%
  st_drop_geometry()%>%
  select(-uniqueID, -DarkConditionCount, -X)

cor(fishnet_nu_2)%>%
  ggcorrplot(hc.order = TRUE, outline.color = "white")
```

![](Final_Project_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
fishnet_long_night <-
  Crash_Ped_Fish_crash %>%
  st_drop_geometry()%>%
  select(-uniqueID, -DarkConditionCount,-CrashCount, -SunnyCount, -WeekendCount, -FatalCount)%>%
  gather(Variable, value, -NightCrash)

fishnet_cor_night <-
  Crash_Ped_Fish_crash %>%
  st_drop_geometry()%>%
  dplyr::select(-uniqueID, -DarkConditionCount,-CrashCount, -SunnyCount, -WeekendCount, -FatalCount)%>%
  gather(Variable, value, -NightCrash)%>%
  dplyr::group_by(Variable) %>%
  dplyr::summarize(correlation = cor(value, NightCrash, use = "complete.obs"))

ggplot(fishnet_long_night, aes(value, NightCrash)) +
  geom_point(size = 0.1, color = "grey50", alpha = 0.5) +
  geom_text(data = fishnet_cor_night, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(title = "Night Crash count as a function of variables") +
  theme_map()
```

![](Final_Project_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

Crime still has the highest correlation with pedestrian crashes at dark conditions. Number of lights has the second strongest correlation. Lumen size, black population, and young drivers ranked the third on correlation strength with dark condition pedestrian crashes. 



# Results

## Multivariate Regression

Multivariate regression is made with "pedestrian crashes at dark conditions" as the dependent variable. 


```r
# Considering population density and calculate percent population

z <- Crash_Ped_Fish_crash %>% st_drop_geometry()%>%
  mutate(CrimeRate = CountCrime/CountTotal,
         Percent_60 = CountAbove60/CountTotal,
         Percent_10 = CountBelow10/CountTotal,
         Percent_Black = CountBlack/CountTotal,
         Percent_White = CountWhite/CountTotal,
         Percent_Asian = CountAsian/CountTotal,
         Percent_NoCar = CountNoCar/CountTotal)

z$CrimeRate[is.infinite(z$CrimeRate)] <- 0
z$Percent_60[is.infinite(z$Percent_60)] <- 0
z$Percent_10[is.infinite(z$Percent_60)] <- 0
z$Percent_White[is.infinite(z$Percent_White)] <- 0
z$Percent_Black[is.infinite(z$Percent_Black)] <- 0
z$Percent_Asian[is.infinite(z$Percent_Asian)] <- 0
z$Percent_NoCar[is.infinite(z$Percent_NoCar)] <- 0

str(Crash_Ped_Fish_crash)
# All variables

library(FNN)
library(car)

model1 <- lm(NightCrash ~ CountBelow10 + CountAbove60 + CountNoCar + CountTree + CountWhite + CountAsian + CountCrime + CrimeRate + CountBlack + mean_lumen + MeanSpeed + LightCount, data = z)

summary(model1)

#Remove co-founding variables
model2 <- lm(NightCrash ~ Percent_60 +Percent_10 + Percent_Black + Percent_Asian + CrimeRate + mean_lumen + MeanSpeed + LightCount + CountTree + CountNoCar, data = z)

summary(model2)

model3 <- lm(NightCrash ~ Percent_60 +Percent_10 + Percent_Black + Percent_Asian + CrimeRate + mean_lumen + MeanSpeed + LightCount + CountTree + Percent_NoCar, data = z)

summary(model3)

#Backward Selection

step(lm(NightCrash ~ Percent_60 +Percent_10 + Percent_Black + Percent_Asian + CrimeRate + mean_lumen + MeanSpeed + LightCount + CountTree + Percent_NoCar, data = z))

model4 <- lm(NightCrash ~ Percent_60  + Percent_10 + Percent_Black  + CrimeRate + mean_lumen + LightCount + CountTree + Percent_NoCar, data = z)

library(stargazer)

summary(model4)
```


```r
stargazer(model1,model2,model3,model4, type = "text",style = "ajps")
```
In model 1, we used variables without considering the effect of population. In model 2, 3, and 4, we considered population by making all relevant variables into "percent of the population" in each fishnet cell.

Model 4 has all the significant variables. We can see that percent of people commuting without cars has the greatest impact on pedestrian safety. For every percent increase in population commuting without cars, there is an increase of 3.4 crashes in the area. 

Percent of white population and percent of black population are co-founding factors, so we only included percent of Black population in the analysis. Black population is the most significant variable among Black, White, and Asian population. The model shows that for every percent increase in the Black population , the number of crashes will increase by 1.0 in a given area. 

Interestingly, according to the model, a lower number of elders above the age 60 is associated with a higher number of crashes. Every increase in the number of children is associated with an increase of 2 pcases of pedestrian crashes in dark conditions.

One would expect that the lumen size and light counts would be negatively associated with pedestrian crashes over dark conditions. Our result shows differently. The result shows that, though very sightly, the higher the mean lumen size in a fishnet cell, the more chance of pedestrian crashes over dark conditions, while holding other variables constant. 

Tree count demonstrates a negative association with night pedestrian crashes. This means a lower number of trees contributes to a higher number of crashes in a given area. 

In addition, our results show that areas with higher amount of crime is significant associated with higher amount of pedestrian crashes in dark conditions. 

The number of fatal deaths occur at night is limited. Instead of multivariate, we will run a binomial logit model.

Overall, the R-squared for the final model is 31%, which we believe it shows a good fit for explaining real-life data where so many circumstances could be involved.



### Moran's I


```r
library(scales)
library(grid)
library(gridExtra)
library(spdep)

z_fish <-
  z %>%
  right_join(fishnet)%>%
  st_as_sf()

z_fish$NightCrash[is.na(z_fish$NightCrash)] <- 0

final_net.nb <- poly2nb(as_Spatial(z_fish), queen=TRUE)

final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)

final_net.localMorans <- 
  cbind(
    as.data.frame(localmoran(z_fish$NightCrash, final_net.weights)),
    as.data.frame(z_fish)) %>% st_sf()%>% 
  dplyr::select("Night Crash Count" =  NightCrash, "Local Morans I" = Ii,
                    P_Value = "Pr(z != E(Ii))") %>%
      mutate("Significant Hotspots" = ifelse(P_Value <= 0.0000001, 1, 0))%>%
      gather(Variable, Value, -geometry)

vars <- unique(final_net.localMorans$Variable)
varList <- list()

for(i in vars){
  varList[[i]] <- 
    ggplot() +
      geom_sf(data = filter(final_net.localMorans, Variable == i), 
              aes(fill = Value), colour=NA) +
      scale_fill_viridis(option = "A") +
      labs(title=i) +
      theme_map() + theme(legend.position="bottom")}

do.call(grid.arrange,c(varList, ncol = 4, top = "Local Morans I statistics, Night Crash"))
```

![](Final_Project_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Moran's I indicates that there are some very strong clustering of crashes (p-value < 0.0000001). This might show some neighborhood effects. Certainly there is a clustering at the Center City and along major streets, such as Market Street and Board Street. 



## Binomial Logit Model

All models are cleaned and trimmed by backward selection methods to have the lowest Akaike Information Criterion (AIC) number, as well as Variance Inflation Factor tests to make sure there is no confounding factors. Please unfold the code to see more details. 

### Fatal pedestrian crashes at night


```r
#Night and Fatal crashes marked with 1
Crash_Ped_fatal_night <-
  Crash_Ped_night %>%
  mutate(Fatal_night = case_when(FATAL_COUN > 0 ~ 1))%>%
  filter(Fatal_night == 1) %>%
  select(CRN, Fatal_night)

Crash_Ped_fatal_night_w_all <-
 Crash_Ped_all %>%
  left_join(st_drop_geometry(Crash_Ped_fatal_night))%>%
  st_as_sf

Crash_Ped_fatal_night_w_all$Fatal_night[is.na(Crash_Ped_fatal_night_w_all$Fatal_night)] <- 0
```


```r
#Adding speed limit variable
Crash_Ped_fatal_night_w_all <-
  crash_road_all %>%
  select(SPEED_LIMIT,CRN) %>%
  right_join(Crash_Ped_fatal_night_w_all)

z2 <- Crash_Ped_fatal_night_w_all %>% 
 mutate(CrimeRate = CountCrime/CountTotal,
         Percent_60 = CountAbove60/CountTotal,
         Percent_10 = CountBelow10/CountTotal,
         Percent_Black = CountBlack/CountTotal,
         Percent_White = CountWhite/CountTotal,
         Percent_Asian = CountAsian/CountTotal,
         Percent_NoCar = CountNoCar/CountTotal)%>%
  st_as_sf()

crash_person_2016 <- read.csv("PERSON_PHILADELPHIA_2016.csv")
crash_person_2017 <- read.csv("PERSON_PHILADELPHIA_2017.csv")
crash_person_2018 <- read.csv("PERSON_PHILADELPHIA_2018.csv")
crash_person_2019 <- read.csv("PERSON_PHILADELPHIA_2019.csv")
crash_person_2020 <- read.csv("PERSON_PHILADELPHIA_2020.csv")


#Pedestrian Gender 
crash_person_all <- rbind(crash_person_2016,crash_person_2017,crash_person_2018,crash_person_2019,crash_person_2020) %>% select(CRN, AGE,SEX,PERSON_TYPE) %>% na.omit %>% mutate(GenderF = case_when(SEX == "F" & PERSON_TYPE == 7 ~ 1, TRUE~0))%>%  distinct(CRN, .keep_all = TRUE) %>% select(CRN,GenderF)

#Driver_age
crash_person_all_driver_age <- rbind(crash_person_2016,crash_person_2017,crash_person_2018,crash_person_2019,crash_person_2020) %>% select(CRN, AGE,SEX,PERSON_TYPE) %>% filter(PERSON_TYPE == 1) %>%  distinct(CRN, .keep_all = TRUE) %>% mutate(drive_age = AGE)%>%select(CRN,drive_age)



z2_1 <- crash_person_all %>%
  right_join(z2) %>%
  left_join(crash_person_all_driver_age) %>%
  filter(dark ==1)

crash_flag_all_2 <- rbind(crash_flag_2016,crash_flag_2017,crash_flag_2018,crash_flag_2019,crash_flag_2020)%>%
  select(CRN,ILLUMINATION_DARK) 

z2_1 <- crash_flag_all_2 %>%
  right_join(z2_1)

#Add No light variable
z2_1 <- z2_1 %>%
  mutate(nolight = case_when(ILLUMINATI == 3 ~1,TRUE~0))

z2_1$drive_age[is.na(z2_1$drive_age)] <- 0


model_bi_1 <-
  glm(Fatal_night ~ weekend + DRINKING_DRIVER + sunny + young_driver + mean_lumen + HOUR_OF_DA + DAY_OF_WEE + SPEED_LIMIT + Percent_10 + Percent_60 + Percent_Black + Percent_White + Percent_Asian + Percent_NoCar + CountTree + GenderF + CrimeRate + drive_age, family = "binomial", data = z2_1) 

summary(model_bi_1)

vif(model_bi_1)

model_bi_2 <-
  glm(Fatal_night ~ weekend + DRINKING_DRIVER + sunny + young_driver + mean_lumen  + HOUR_OF_DA + SPEED_LIMIT + Percent_10 + Percent_60 + Percent_Black + Percent_NoCar + Percent_Asian + CountTree + GenderF + nolight + drive_age, family = "binomial", data = z2_1) 

summary(model_bi_2)

#backward selection
step(glm(Fatal_night ~ weekend + DRINKING_DRIVER + sunny + young_driver + mean_lumen + SPEED_LIMIT + Percent_10 + Percent_60 + Percent_Black + Percent_NoCar + CountTree + GenderF + CrimeRate + nolight+ drive_age+ Percent_Asian,family="binomial",data = z2_1))

model_bi_3 <-
  glm(formula = Fatal_night ~ DRINKING_DRIVER + mean_lumen  + 
    SPEED_LIMIT + Percent_Black + CountTree,family = "binomial",data= z2_1)

summary(model_bi_3)

#All clear under 5
vif(model_bi_3)
```


```r
stargazer(model_bi_1,model_bi_2,model_bi_3, type = "html", column.sep.width = "10pt", align=TRUE)
```


<table style="text-align:center"><tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="3"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="3" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="3">Fatal_night</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">weekend</td><td>0.635</td><td>0.144</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.462)</td><td>(0.250)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">DRINKING_DRIVER</td><td>1.688<sup>***</sup></td><td>1.701<sup>***</sup></td><td>1.826<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.448)</td><td>(0.444)</td><td>(0.421)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">sunny</td><td>-0.019</td><td>-0.006</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.242)</td><td>(0.241)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">young_driver</td><td>0.281</td><td>0.328</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.768)</td><td>(0.770)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">mean_lumen</td><td>0.011<sup>***</sup></td><td>0.009<sup>**</sup></td><td>0.008<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.004)</td><td>(0.004)</td><td>(0.004)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">HOUR_OF_DA</td><td>-0.013</td><td>-0.015</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.011)</td><td>(0.011)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">DAY_OF_WEE</td><td>-0.123</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.101)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">SPEED_LIMIT</td><td>0.089<sup>***</sup></td><td>0.086<sup>***</sup></td><td>0.088<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.022)</td><td>(0.022)</td><td>(0.021)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_10</td><td>-0.260</td><td>0.673</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(1.230)</td><td>(1.155)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_60</td><td>-0.362</td><td>-0.823</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(1.240)</td><td>(1.207)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_Black</td><td>-2.343<sup>***</sup></td><td>-1.158<sup>**</sup></td><td>-1.016<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.633)</td><td>(0.452)</td><td>(0.424)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_White</td><td>-1.965<sup>**</sup></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.809)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_Asian</td><td>-3.145<sup>**</sup></td><td>-2.008</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(1.595)</td><td>(1.536)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_NoCar</td><td>-2.256</td><td>-2.834</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(3.208)</td><td>(3.814)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">CountTree</td><td>-0.002<sup>*</sup></td><td>-0.004<sup>***</sup></td><td>-0.004<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.001)</td><td>(0.001)</td><td>(0.001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">GenderF</td><td>-0.780</td><td>-0.851</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.526)</td><td>(0.525)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">CrimeRate</td><td>-0.004</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.003)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">nolight</td><td></td><td>1.063</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(1.021)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">drive_age</td><td>0.002</td><td>0.003</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.004)</td><td>(0.004)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-4.531<sup>***</sup></td><td>-6.569<sup>***</sup></td><td>-5.726<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(1.011)</td><td>(1.372)</td><td>(0.782)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>2,008</td><td>2,008</td><td>2,008</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-298.698</td><td>-302.148</td><td>-308.562</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>635.397</td><td>638.296</td><td>629.124</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="3" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


```r
exp(coef(model_bi_3))
```

```
##     (Intercept) DRINKING_DRIVER      mean_lumen     SPEED_LIMIT   Percent_Black 
##     0.003258663     6.206263002     1.007791629     1.091847800     0.361929969 
##       CountTree 
##     0.995837781
```

```r
100 * (exp(coef(model_bi_3))-1)
```

```
##     (Intercept) DRINKING_DRIVER      mean_lumen     SPEED_LIMIT   Percent_Black 
##     -99.6741337     520.6263002       0.7791629       9.1847800     -63.8070031 
##       CountTree 
##      -0.4162219
```

The binomial logit model shows that drunk diver and speed limit are the most significant factors to predict the possibility of a fatal pedestrian crash (versus crashes without fatal outcomes) at night.

If the driver were drunk, the odds of a pedestrian having a fatal outcome would be almost five times more likely comparing to a normal driver. 

With every increase in speed limit on the road, the odds of a fatal pedestrian crash would increase by 9%.

Fatal pedestrian at night is also strongly related to tree counts. With each increase in the number of tree within the fishnet area of the crashes, the odds of fatal pedestrian crash is reduced by 0.4%. 

The percent of Black population is also a significant factor. In Philadelphia, most fatal crashes occurred in places where the majority of the population is white. It is worth to note that with every increase in the percent of Black population, the odds of a fatal pedestrian crash is reduced by 64%. 

Surprisingly, the lumen size, though shows a significant correlation with fatal pedestrian crash, is positively related to fatal pedestrian crashes. This means, with every increase of lumen size in the crash area, the odds of a fatal pedestrian crash is increased by 0.77. The result is counterintuatitive, as it is expected that the darker the area, the more likely the fatal crash would occur. 



### Fatal pedestrian crashes during the day


```r
## Filter Out all crashes during the day
z4 <- crash_person_all %>%
  right_join(z2) %>%
  left_join(crash_person_all_driver_age) %>%
  filter(dark ==0)

z4$drive_age[is.na(z4$drive_age)]<-0

crash_flag_all_2 <- rbind(crash_flag_2016,crash_flag_2017,crash_flag_2018,crash_flag_2019,crash_flag_2020)%>%
  select(CRN,ILLUMINATION_DARK) 

z4 <- crash_flag_all_2 %>%
  right_join(z4)

z4 <- z4 %>%
  mutate(nolight = case_when(ILLUMINATI == 3 ~ 1,TRUE~0), fatal_day = case_when(FATAL_COUN > 0  ~1,TRUE~0))
         
         
model_bi3_1 <-
  glm(fatal_day ~ weekend + DRINKING_DRIVER + sunny + young_driver + mean_lumen  + HOUR_OF_DA + DAY_OF_WEE + SPEED_LIMIT + Percent_10 + Percent_60 + Percent_Black + Percent_White + Percent_Asian + Percent_NoCar + CountTree + GenderF + CrimeRate +drive_age, family = "binomial", data = z4) 

summary(model_bi3_1)

model_bi3_2 <-
  glm(fatal_day ~ weekend + DRINKING_DRIVER + sunny + young_driver + mean_lumen + sunny + HOUR_OF_DA + SPEED_LIMIT + Percent_10 + Percent_60 + Percent_Black + Percent_NoCar + ILLUMINATION_DARK + CountTree + GenderF, family = "binomial", data = z4) 

summary(model_bi3_2)

#backward selection
step(glm(fatal_day ~ weekend + DRINKING_DRIVER + sunny + young_driver + mean_lumen + SPEED_LIMIT + Percent_10 + Percent_60 + Percent_Black + Percent_NoCar + ILLUMINATION_DARK + CountTree + GenderF + CrimeRate + nolight + drive_age, family="binomial",data = z4))

model_bi3_3 <-
  glm(formula = fatal_day ~ DRINKING_DRIVER + young_driver + 
    SPEED_LIMIT + Percent_NoCar, family = "binomial", data = z4)


summary(model_bi3_3)

vif(model_bi3_3)
#All clear under 5
```


```r
stargazer(model_bi_1,model_bi_2,model_bi_3, type = "html", align=TRUE)
```


<table style="text-align:center"><tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="3"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="3" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="3">Fatal_night</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td><td>(3)</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">weekend</td><td>0.635</td><td>0.144</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.462)</td><td>(0.250)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">DRINKING_DRIVER</td><td>1.688<sup>***</sup></td><td>1.701<sup>***</sup></td><td>1.826<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.448)</td><td>(0.444)</td><td>(0.421)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">sunny</td><td>-0.019</td><td>-0.006</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.242)</td><td>(0.241)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">young_driver</td><td>0.281</td><td>0.328</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.768)</td><td>(0.770)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">mean_lumen</td><td>0.011<sup>***</sup></td><td>0.009<sup>**</sup></td><td>0.008<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.004)</td><td>(0.004)</td><td>(0.004)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">HOUR_OF_DA</td><td>-0.013</td><td>-0.015</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.011)</td><td>(0.011)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">DAY_OF_WEE</td><td>-0.123</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.101)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">SPEED_LIMIT</td><td>0.089<sup>***</sup></td><td>0.086<sup>***</sup></td><td>0.088<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.022)</td><td>(0.022)</td><td>(0.021)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_10</td><td>-0.260</td><td>0.673</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(1.230)</td><td>(1.155)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_60</td><td>-0.362</td><td>-0.823</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(1.240)</td><td>(1.207)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_Black</td><td>-2.343<sup>***</sup></td><td>-1.158<sup>**</sup></td><td>-1.016<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.633)</td><td>(0.452)</td><td>(0.424)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_White</td><td>-1.965<sup>**</sup></td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.809)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_Asian</td><td>-3.145<sup>**</sup></td><td>-2.008</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(1.595)</td><td>(1.536)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_NoCar</td><td>-2.256</td><td>-2.834</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(3.208)</td><td>(3.814)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">CountTree</td><td>-0.002<sup>*</sup></td><td>-0.004<sup>***</sup></td><td>-0.004<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.001)</td><td>(0.001)</td><td>(0.001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">GenderF</td><td>-0.780</td><td>-0.851</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.526)</td><td>(0.525)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">CrimeRate</td><td>-0.004</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.003)</td><td></td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">nolight</td><td></td><td>1.063</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td>(1.021)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">drive_age</td><td>0.002</td><td>0.003</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.004)</td><td>(0.004)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-4.531<sup>***</sup></td><td>-6.569<sup>***</sup></td><td>-5.726<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(1.011)</td><td>(1.372)</td><td>(0.782)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td><td></td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>2,008</td><td>2,008</td><td>2,008</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-298.698</td><td>-302.148</td><td>-308.562</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>635.397</td><td>638.296</td><td>629.124</td></tr>
<tr><td colspan="4" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="3" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


```r
exp(coef(model_bi3_3))
```

```
##     (Intercept) DRINKING_DRIVER    young_driver     SPEED_LIMIT   Percent_NoCar 
##    5.897469e-04    7.249156e+00    6.923820e-07    1.116592e+00    1.174305e+01
```

```r
100 * (exp(coef(model_bi3_3))-1)
```

```
##     (Intercept) DRINKING_DRIVER    young_driver     SPEED_LIMIT   Percent_NoCar 
##       -99.94103       624.91563       -99.99993        11.65923      1074.30544
```

Drunk drivers and speed limit are also the two significant factors related to fatal pedestrian crashes during the day. The odds of a fatal pedestrian crash is six times higher when a drunk driver involved. Speed limit becomes a more significant factor during the day accounting for fatal pedestrian crashes. For every increase in speed limit, the odds of fatal pedestrian crashes during the day is increased by 11%. 

Percent of population without cars become the most significant factors in this case. For every percent increase in population without car in the areas where crashes occur, the odds of them being fatal are almost 1074% higher. 



### The odds of pedestrian crashes at night versus druing the day


```r
z3 <- crash_person_all %>%
  right_join(z2) %>% 
  left_join(crash_person_all_driver_age) %>%
  mutate(darkcondition = case_when(ILLUMINATI == 2 | ILLUMINATI == 3 ~ 1, TRUE ~ 0))

z3$drive_age[is.na(z3$drive_age)] <- 0 

model_bi2_1 <-
  glm(darkcondition ~ weekend + DRINKING_DRIVER + sunny + young_driver + mean_lumen + WEATHER + SPEED_LIMIT + Percent_10 + Percent_60 + Percent_Black + Percent_White + Percent_Asian + Percent_NoCar + CountTree + GenderF + drive_age, family = "binomial", data = z3) 

vif(model_bi2_1)

#Delete co-founding and insignificant variables

model_bi2_2 <-
  glm(darkcondition ~ weekend + DRINKING_DRIVER + sunny + SPEED_LIMIT + Percent_10 + Percent_Black + Percent_NoCar + CountTree + drive_age, family = "binomial", data = z3) 

summary(model_bi2_2)

#Backward selection
step(glm(darkcondition ~ weekend + DRINKING_DRIVER + sunny + SPEED_LIMIT + Percent_10 + Percent_Black + Percent_NoCar + CountTree + drive_age, family = "binomial", data = z3))

#Backward selection shows that second model is the best already.

vif(model_bi2_2)

#All clear out, vif below 5.
```


```r
stargazer(model_bi2_1, model_bi2_2, type = "html", align=TRUE)
```


<table style="text-align:center"><tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"></td><td colspan="2"><em>Dependent variable:</em></td></tr>
<tr><td></td><td colspan="2" style="border-bottom: 1px solid black"></td></tr>
<tr><td style="text-align:left"></td><td colspan="2">darkcondition</td></tr>
<tr><td style="text-align:left"></td><td>(1)</td><td>(2)</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">weekend</td><td>0.164<sup>***</sup></td><td>0.167<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.061)</td><td>(0.061)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">DRINKING_DRIVER</td><td>1.448<sup>***</sup></td><td>1.451<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.261)</td><td>(0.260)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">sunny</td><td>-0.511<sup>***</sup></td><td>-0.512<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.056)</td><td>(0.056)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">young_driver</td><td>0.072</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.187)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">mean_lumen</td><td>0.0004</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.001)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">WEATHER</td><td>0.020</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.019)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">SPEED_LIMIT</td><td>0.043<sup>***</sup></td><td>0.042<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.006)</td><td>(0.006)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_10</td><td>0.528</td><td>0.699<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.325)</td><td>(0.300)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_60</td><td>-0.234</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.289)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_Black</td><td>-0.045</td><td>0.231<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.172)</td><td>(0.091)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_White</td><td>-0.502<sup>**</sup></td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.219)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_Asian</td><td>-0.225</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.391)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Percent_NoCar</td><td>1.187<sup>***</sup></td><td>1.145<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.430)</td><td>(0.422)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">CountTree</td><td>-0.0003</td><td>-0.001<sup>**</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.0003)</td><td>(0.0002)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">GenderF</td><td>-0.016</td><td></td></tr>
<tr><td style="text-align:left"></td><td>(0.084)</td><td></td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">drive_age</td><td>0.002<sup>***</sup></td><td>0.003<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.001)</td><td>(0.001)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td style="text-align:left">Constant</td><td>-1.671<sup>***</sup></td><td>-1.906<sup>***</sup></td></tr>
<tr><td style="text-align:left"></td><td>(0.252)</td><td>(0.192)</td></tr>
<tr><td style="text-align:left"></td><td></td><td></td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left">Observations</td><td>6,083</td><td>6,083</td></tr>
<tr><td style="text-align:left">Log Likelihood</td><td>-3,750.425</td><td>-3,754.468</td></tr>
<tr><td style="text-align:left">Akaike Inf. Crit.</td><td>7,534.851</td><td>7,528.937</td></tr>
<tr><td colspan="3" style="border-bottom: 1px solid black"></td></tr><tr><td style="text-align:left"><em>Note:</em></td><td colspan="2" style="text-align:right"><sup>*</sup>p<0.1; <sup>**</sup>p<0.05; <sup>***</sup>p<0.01</td></tr>
</table>


```r
exp(coef(model_bi2_2))
```

```
##     (Intercept)         weekend DRINKING_DRIVER           sunny     SPEED_LIMIT 
##       0.1487251       1.1822476       4.2685858       0.5989996       1.0432619 
##      Percent_10   Percent_Black   Percent_NoCar       CountTree       drive_age 
##       2.0113227       1.2594519       3.1426920       0.9994772       1.0025734
```

```r
100 * (exp(coef(model_bi2_2))-1)
```

```
##     (Intercept)         weekend DRINKING_DRIVER           sunny     SPEED_LIMIT 
##    -85.12748664     18.22476215    326.85858176    -40.10004232      4.32618529 
##      Percent_10   Percent_Black   Percent_NoCar       CountTree       drive_age 
##    101.13226928     25.94518860    214.26920302     -0.05227817      0.25733914
```

When we look at the possibility for a pedestrian crash at night, drunk driver, speed limit, percent of population, percent of population commuting without cars, and tree counts again appear to be significant variables. A drunk driver increases the odds of a night pedestrian night crash by 327%. Every increase in speed limit increases the odd by 4%. It is interesting to note that black population here is more vulnerable to crash at night: every percent increase in black population increases the odd of night pedestrian by 25%. In addition, every percent increase in population commuting without cars, the odds increase by 214%. Finally, every increase in tree count is associated with a reduction of 0.05% in pedestrian crashes at night.

Some new variables have become significant when we look at all the pedestrian crashes together. A clear, sunny weather is associated with a 40% reduction in odds of night pedestrian crashes. Every percent increase in number of children below age 10 increases the odds by 109%. We expect that age would have a negative association with the odds of night pedestrian crashes. Based on the model, however, every increase in the age of driver is associated with a 0.25% increase in the odds of a night pedestrian crash. 

A night pedestrian crash is more likely during weekend with an increased odds of 18%.





# Discussion

We summarized the results into 7 takeaways???

1. Drunk driving and speed limits still appear to be the most significant variables throughout the analysis. They increase the risk of pedestrian crashes and fatal pedestrian crashes in both dark condition and during daylight.

2. Our results also finds the significance of *tree count* in relation to pedestrian safety in dark conditions. Higher number of trees are associated with lower number of pedestrian crashes both in dark conditions and during daylight. This might implies that trees might contribute to pedestrian safety beyond asethetic and environmental values. 

3. Areas with higher percentage of commuters who use bike, walk, and have significantly higher chance to pedestrian crashes overall. This, **however**, is not a significant variable when comes to fatal pedestrian crashes in dark conditions.

4. Areas with higher percentage of Black population are more prone to night pedestrian crash. Nevertheless, these crashes have less chance to turn into fatal crashes. 

5. Lumen size is a significant variable when looking at pedestrian crash cases at night. The results show that the higher the lumen size, the more pedestrian crashes and fatal pedestrian crashes occur in dark conditions. One reason could be that night pedestrian crashes occur along main roads were already pretty well lit, this means there are other factors more significant than brightness that contribute to pedestrian crashes at night. 

6. Pedestrian gender is not a significant variable based on our results. There might be an assumption that women are more volunerable to crashes, but our models do not show that. 

7. Areas with more children are more prone to pedestrian crashes at night. 






# Conclusion 

We acknowledge many limitations of this study. First, the size of the data set might not allow substantive analysis. The fatal crashes at night between 2016 - 2020 in Philadelphia only counts to 119 cases. This could be solved by expanding the study time frame or expanding the geography of the study area. Second, the models do not account for neighborhood effects. Though we do account factors such as racial composition, age, and crime, the analysis could also include variables such as income and car ownership that have showed tendency of clustering. The next step is to have a spacial generalizability test. 

Our results may provide some insights into pedestrian safety in dark conditions. First, tree planting might have values beyond environmental or aesthetic reasons. As shown by the results, it is related to pedestrian safety at night. So far, very limited literature have looked into the relation of greens related to pedestrian safety. Second, we found out that areas with higher percentage of Black population have higher risk of pedestrian crashes. This finding aligns with most of the literature. These crashes are, however, not fatal. In fact, the higher percentage of Black population is related significantly to lower number of fatal crashes in dark conditions. Third, our results do show that places with high non-vehicular commuters demonstrate strong association high risk in pedestrian crashes, which is not too surprising. Again, quality infrastructure in these places are required to reduce pedestrian risks, as most people do not have other options but required to walk, bike, or take public transit to work. 



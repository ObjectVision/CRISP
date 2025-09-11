# ------------------- backcast data aggregator
# 
# Chris Jacobs-Crisioni, 3/1/2025
#
# code to take the backcast data, and turn it in a dataset consistent with the 
# loaded boundaryset
# 
# presupposes: 
#      reportingdir<-"D:/ProjDir/2BURP/reporting/"
#      boundaryset<-"UN_intermediate_regions" #or Countries, UN_countries, UN_intermediate_regions, Continents, Continents_1RUS_uint32, World
# fields: year,pop,smod,LocID,iso3c,UNname
# ------------------------------------------------
library(readxl)
library(dplyr)

# create an empty dataframe to which rows can be added if available
out_columns <- subset(indata, ru_label==-9999)

# fill out_columns only if the boundaryset can be aggregated from the countries encoded in 
if(
    (toupper(classification)==toupper("DegUrba_lvl1") | toupper(classification)==toupper("DegUrba_lvl2")) & 
    (boundaryset=="Continents_1RUS_uint32" | 
     boundaryset=="UN_intermediate_regions" | 
     boundaryset=="UN_countries" | 
     boundaryset=="World")
  ) {
  
  print(paste0("Adding backcast data ", classification))
  
  #rawdata<-read.csv(paste0(reportingdir, "/", "df_smod_est_v7.csv"), stringsAsFactors = FALSE)
  if (toupper(classification)==toupper("DegUrba_lvl1")) {
      rawdata<-read.csv(paste0(reportingdir, "/", "degurba_est__RESC_1__RUR_RES_0__URB_INTERP_MODEL_UN_v2025_05_L1_1July.csv"), stringsAsFactors = FALSE) 
  } else {
    rawdata<-read.csv(paste0(reportingdir, "/", "degurba_est__RESC_1__RUR_RES_0__URB_INTERP_MODEL_UN_v2025_05_L2_1July.csv"), stringsAsFactors = FALSE)
  }
  
  rawdata<-subset(rawdata, year<1975 & smod != "wpp" & smod !="wup")
  #rawdata$pop<-rawdata$pop * (1000*1000) #no longer multiplication needed
  #un_regions<-read.csv("countries_20250206.csv", sep=";", fileEncoding="UTF-8-BOM", stringsasfactors = FALSE)
  un_regions<-read_xlsx(paste0(reportingdir, "unbnda_countries_with_recoding.xlsx"), sheet="unbnda")
  
  # make sure that iso3cds only occur once
  un_regions<-un_regions %>%
    group_by(iso3cd) %>%
    filter(row_number()==1)
  
  merged<-merge(x=rawdata, y=un_regions, by.x="iso3c", by.y="iso3cd", all.x=TRUE, all.y=FALSE)
  
  # set specifics of merging field
  if(boundaryset=="UN_intermediate_regions") {
    merged$agg_field<-merged$`UN intermediate region`
    merged$name_field<-merged$`UN intermediate region`
  } else if(boundaryset=="UN_countries") {
    merged$agg_field<-merged$iso3c
    merged$name_field<-merged$UNname
  } else if(boundaryset=="World") {
    merged$agg_field<-"World"
    merged$name_field<-"World"
  } else if(boundaryset=="Continents_1RUS_uint32") {
    merged$agg_field<-merged$Continent
    merged$name_field<-merged$Continent
  }

    
  temp<-aggregate(merged$pop, by=list(merged$agg_field, merged$name_field, merged$smod, merged$year), FUN="sum")
  
  temp$ru_code<-temp$Group.1
  temp$ru_label<-temp$Group.2
  temp$cls_label<-temp$Group.3
  temp$cls_code<-NA
  temp$year<-temp$Group.4
  temp$area<-NA
  temp$pop<-temp$x
  temp$builtup<-NA
  #temp$abandoned<-NA
  #temp$emerged<-NA
  
  temp$Group.1<-NULL
  temp$Group.2<-NULL
  temp$Group.3<-NULL
  temp$Group.4<-NULL
  temp$x<-NULL
  
  # values conversion
  temp$cls_label[temp$cls_label=="cities"]<-"Urban centre"
  temp$cls_code[temp$cls_label=="Urban centre"]<-30
  temp$cls_label[temp$cls_label=="UC"]<-"Urban centre"
  temp$cls_code [temp$cls_label=="Urban centre"]<-30
  temp$cls_label[temp$cls_label=="DUC"]<-"Dense urban cluster"
  temp$cls_code [temp$cls_label=="Dense urban cluster"]<-23
  temp$cls_label[temp$cls_label=="SDUC"]<-"Semi-dense urban cluster"
  temp$cls_code [temp$cls_label=="Semi-dense urban cluster"]<-22
  temp$cls_label[temp$cls_label=="SBRB"]<-"Suburban grid cell"
  temp$cls_code [temp$cls_label=="Suburban grid cell"]<-21
  temp$cls_label[temp$cls_label=="towns"]<-"Urban cluster"
  temp$cls_code[temp$cls_label=="Urban cluster"]<-20
  temp$cls_label[temp$cls_label=="UCL"]<-"Urban cluster"
  temp$cls_code [temp$cls_label=="Urban cluster"]<-20
  temp$cls_label[temp$cls_label=="RC"]<-"Rural cluster"
  temp$cls_code [temp$cls_label=="Rural cluster"]<-13
  temp$cls_label[temp$cls_label=="LDR"]<-"Low density rural grid cell"
  temp$cls_code [temp$cls_label=="Low density rural grid cell"]<-12
  temp$cls_label[temp$cls_label=="VLDR"]<-"Very low density grid cell"
  temp$cls_code [temp$cls_label=="Very low density grid cell"]<-11
  temp$cls_label[temp$cls_label=="rural"]<-"Rural grid cells"
  temp$cls_code[temp$cls_label=="Rural grid cells"]<-10
  temp$cls_label[temp$cls_label=="WATER"]<-"Water"
  temp$cls_code [temp$cls_label=="Water"]<-00
  
  out_columns<-rbind(out_columns, temp)
  
  write.csv(out_columns, row.names=FALSE, file=paste0("backcast_", boundaryset, "_",classification,".csv"))
  
}

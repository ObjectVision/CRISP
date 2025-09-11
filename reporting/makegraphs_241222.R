################### specifics of run
inputdir<-"C:/Users/asobi/Documents/Werk/2023_global_lu_model/results/20250505_Marcello"
reportingdir<-"D:/ProjDir/2BURP/reporting/"
boundaryset<-"UN_countries" #or Countries, UN_countries, UN_intermediate_regions, Continents_1RUS_uint32, Continents_uint32, World
classification<-"DegUrba_lvl1"
runset<-"" #"_v9_IntMigr-0.01_PopRSuitScale-0.9_PopShareNewBU-1_autoresolved_calib_20241205_nobu_Marcello" # new calibration, preferred spec?

settlement_graphs<-FALSE

################### set outputfolder (stepwise!!!)
rootoutputdir<-paste0(inputdir, "/graphs")
outputdir<-rootoutputdir
#### make folder structure

if (!dir.exists(outputdir)) dir.create(outputdir)
outputdir<-paste0(outputdir,"/",runset)
if (!dir.exists(outputdir)) dir.create(outputdir)
outputdir<-paste0(outputdir,"/",boundaryset)
if (!dir.exists(outputdir)) dir.create(outputdir)
outputdir<-paste0(outputdir,"/",classification,"/")
if (!dir.exists(outputdir)) dir.create(outputdir)

source(paste0(reportingdir, "supporting_scripts.r"))
source(paste0(reportingdir, "graph_functions.r"))

font_size<-20 # font size override (default font size = 14)

################### load datafiles
#indata<-read.csv(paste0(inputdir,"/","Indicators_",boundaryset,"_",classification,runset,".csv"))
indata<-read.csv(paste0(inputdir,"/",boundaryset,"_",classification,runset,".csv"))

#indata$ru_code<-indata$ru_label.1 temp fix unnecessary now

indata<-subset(indata, select = c(ru_label, ru_code, cls_label, cls_code, year, area, pop, builtup))

# fix typo issues with degurba label
indata$cls_label<-as.character(indata$cls_label)
indata$cls_label[indata$cls_label=="Low-density rural grid cell"]<-"Low density rural grid cell"
indata$cls_label[indata$cls_label=="Very low-density rural grid cell"]<-"Very low density grid cell"
indata$cls_label<-factor(indata$cls_label)

#indata<-read.csv(paste0(inputdir,"/","Indicators_",boundaryset,"_",classification,runset,".csv"), sep=";")
if(settlement_graphs) {settldata<-read.csv(paste0(inputdir,"/","Indicators_",boundaryset,runset,".csv"))}

source(paste0(reportingdir, "treat_backcasting.r"))
indata<-rbind(indata, out_columns)

# loop over all unique geographies
codeslist<-unique(indata$ru_code)
for (incode in codeslist) {
  
  # internal output to track progress
  print(incode)

  plotdata<-subset(indata, ru_code==incode)
    
  # only execute if unit has population at one point in time (to avoid empty plots)
  if (sum(plotdata$pop) > 0) {

    name<-as.character(plotdata$ru_label[[1]])
    
    # print("Start plot")
    
    # in intermediate regions, 
    # set name so that it matches the original UN names (assumes that numerics < 10 are padded with one zero)
    if (boundaryset=="UN_intermediate_regions") {
      incode=name  
      name=substring(name, 4)
    }
    
    #### prep total pop and popdensity graphs by putting them into long form (required by ggplot)

    #popplotdata<-subset(poponlydata, as.character(ru_label)==as.character(name) | ru_code==incode)
    popplotdata<-subset(plotdata, as.character(ru_label)==as.character(name) | ru_code==incode | ru_code==as.character(name))
      
    # created indexed version for areas
    area_data<-subset(plotdata, year>=1975)
    pop_density_graph(area_data, printname = name, storename = incode, cvar = factor(area_data$cls_label, level=order))
    
    area_graph<-subset(plotdata, year>=1975, select=c("year","cls_code","cls_label","area"))
    index_areas<-subset(plotdata, year==2020, select=c("cls_code", "area"))
    area_graph<-merge(area_graph, index_areas, by="cls_code")
    
    indexed_area_graph(area_graph, xvar=area_graph$year, printname = name, storename = incode, yvar=(area_graph$area.x / area_graph$area.y), cvar=factor(area_graph$cls_label, level=order))
    
    popplotdata<-popplotdata[order(popplotdata$year), ]
    popplotdata$pop_cumu<-ave(popplotdata$pop, popplotdata$cls_label, FUN=cumsum)
    popplotdata$cumu_is_value<-as.integer(popplotdata$pop == popplotdata$pop_cumu)
    popplotdata<-popplotdata[order(popplotdata$year, decreasing = TRUE), ]
    popplotdata$rev_first<-ave(popplotdata$cumu_is_value, popplotdata$cls_label, FUN=cumsum)
    popplotdata$sel<-popplotdata$pop_cumu > 0 | popplotdata$rev_first == 2
    
    absolute_pop_graph(popplotdata[popplotdata$sel,], printname = name, storename = incode, cvar = factor(popplotdata$cls_label[popplotdata$sel], level=order))
  
    stackdata<-subset(popplotdata, year %% 10 == 0)
    stacked_pop_graph(stackdata, printname = name, storename = incode, xvar = stackdata$year - 5, cvar = factor(stackdata$cls_label, level=rev_order))
    
    if(settlement_graphs) {
      settlements<-subset(settldata, ru_code==incode, select= -c(ru_label, ru_code))
        
      settlements_long<-settlements %>%  pivot_longer(!year, names_to="Variable", values_to = "Value")
      index_settlements<-subset(settlements_long, year==2020, select= -c(year))
      settlements_long<-merge(settlements_long, index_settlements, by="Variable")       
      settlements_long$Indicator<-substr(settlements_long$Variable, 1, regexpr("_", settlements_long$Variable) - 1)
      settlements_long$Settlement<-substr(settlements_long$Variable, regexpr("_", settlements_long$Variable) + 1, nchar(settlements_long$Variable))
      
      settlements_graph(settlements_long, printname = name, storename = incode, 
          xvar=settlements_long$year, yvar=settlements_long$Value.x / settlements_long$Value.y,
          cvar=factor(settlements_long$Settlement), tvar=factor(settlements_long$Indicator), 
          soutputdir=paste0(rootoutputdir,"/",runset,"/",boundaryset,"/"))
    }
  } else {
    print(paste(incode, " has no population, skipped"))
  }
}



write.csv(indata, paste0(outputdir,"/","results.csv"))






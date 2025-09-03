library(tidyr)

filename_add<-"20250901"
inputdir<-"Z:/20_Werk/2017_JRC/LUISA/2025_CRISP_validation/data_20250812/"
outputdir<-"F:/Projdir/2burp/Data/Calibration/"

setwd(inputdir)

adapt_model<-function (invector) {
  inlist<-strsplit(invector, " ")[[1]]
  inlist<-inlist[inlist!="outcome" & inlist!="="]
  inlist<-as.vector(unlist(inlist))
  inlist[which(!is.na(as.numeric(inlist)))[1]]<-paste0(inlist[which(!is.na(as.numeric(inlist)))[1]],"f * Intercept")
  inlist[which(!is.na(as.numeric(inlist)))]<-paste0(", ", inlist[which(!is.na(as.numeric(inlist)))],"f * ")
  outlist<-paste(inlist, collapse="")
  outlist<-paste0("add(", outlist, ")")
  return(outlist)
}

geographies<-c("Africa", "Asia", "Australia_Oceania", "Europe", "North_America", "South_America")
for (geog in geographies) {
  
  print(geog)
  
  intext<-readLines("cubist_fit_THRESH_output.txt", warn = FALSE)
  
  # split rules, do initial cutting
  rawrules<-strsplit(intext, "Rules")
  rawrules<-as.vector(unlist(rawrules))
  rawrules<-gsub("\t", "", rawrules) # drop tab character
  rawrules<-gsub("  ", "", rawrules) # drop many spaces
  rawrules<-data.frame(rawrules)
  
  #identify submodels
  rawrules$rule<-NA
  rawrules$rule[grep("Rule", rawrules$rawrules)]<-grep("Rule", rawrules$rawrules, value=TRUE)
  rawrules$rule[grep("Evaluation ", rawrules$rawrules)]<-grep("Evaluation ", rawrules$rawrules, value=TRUE)
  rawrules$rule<-gsub(":.*", "", rawrules$rule)
  rawrules<-rawrules %>% fill(rule, .direction = "down") 
  rawrules<-rawrules[grepl("Rule", rawrules$rule),]
  
  #select whether line is part of if statement or then statement, characterise all parts
  rawrules$ruledef<-NA
  rawrules$ruledef[grep("if", rawrules$rawrules)]<-grep("if", rawrules$rawrules, value=TRUE)
  rawrules$ruledef[grep("then", rawrules$rawrules)]<-grep("then", rawrules$rawrules, value=TRUE)
  rawrules<-rawrules %>% fill(ruledef, .direction = "down") 
  
  #drop lines without actual modelling content
  rawrules<-rawrules[!grepl("Rule", rawrules$rawrules),]
  rawrules<-rawrules[!grepl("if", rawrules$rawrules),]
  rawrules<-rawrules[!grepl("then", rawrules$rawrules),]
  
  # adapt subsetdefs
  subsetdefs<-subset(rawrules, ruledef=="if")
  subsetdefs$rawrules<-paste0(subsetdefs$rawrules,"f")
  subsetdefs$last<-subsetdefs$rule!=subsetdefs$rule[-1]
  subsetdefs$rawrules[!subsetdefs$last]<-paste0(subsetdefs$rawrules[!subsetdefs$last], " && ")
  subsets<-aggregate(subsetdefs$rawrules, list(subsetdefs$rule), FUN=paste, collapse= "")
  
  # adapt modeldefs
  modeldefs<-subset(rawrules, ruledef=="then")
  modeldefs$rawrules<-gsub("\\+ ", "", modeldefs$rawrules)
  modeldefs$rawrules<-gsub("\\- ", "-", modeldefs$rawrules)
  models<-aggregate(modeldefs$rawrules, list(modeldefs$rule), FUN=paste, collapse=" ")
  models$eval<-""
  for (i in 1:nrow(models))  {
    models$eval[i]<-adapt_model(models$x[i])
  }
  models$x<-NULL
  
  
  
  # create export set
  outframe<-subsets
  outframe$rule<-outframe$Group.1
  outframe$condition<-outframe$x
  outframe$x<-NULL
  outframe<-merge(x = outframe, y = models, by = "Group.1", all.x = TRUE)
  outframe$Group.1<-NULL
  
  # export cubist model
  #quote=FALSE,
  write.csv2(outframe, paste0(outputdir, "cubist_",geog,"_",filename_add,".csv"), row.names = FALSE)
}


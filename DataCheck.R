#This code was written to check the provinical NOS dataset for anomolaies before proceeding with the data manipulation

###StateProvince Check
list(unique(in.data$StateProvince))

#Ontario is both ON and Ontario

#There is some SK assignment in the MBOWL dataset. This may be correct.
SK<-in.data %>% filter (StateProvince=="SK")

#The NL and ME surveys are from Quebec?
NL<-in.data %>% filter (StateProvince=="NL")
ME<-in.data %>% filter (StateProvince=="ME")

###Protocol ID Check
list(unique(in.data$ProtocolCode))
NOCTOWLS<-subset(in.data, ProtocolCode=="NOCTOWLS") #7507 records are from Ontario, 

#the rest are Newfoundland. This does not pose a problem at the moment because these data will not be used for the national trends analysis.
#Newfoundland<-in.data %>% filter (StateProvince=="Newfoundland") #prtocol code is NOCTOWLS 

unk<-in.data %>% filter (ProtocolCode==100) # this is SKOWL data. Added to the analysis parameters file. 

###Missing RouteIdentifier
route<-in.data %>% select(RouteIdentifier, SiteCode, collection)
route<-subset(route, is.na(RouteIdentifier))  
route<-route %>% distinct()

#NA Route Identifier for Newfoundland. Not an issues at the moment, but will need fixes. 
#NA Route Identifier for Manitoba. 

###Missing SiteCode
site<-in.data %>% select(RouteIdentifier, SiteCode, collection)
site<-subset(site, is.na(SiteCode))  
site<-site %>% distinct()
#There are many missing. 

###Missing calendar (y-m-d) data. 

date<-in.data %>% select(SamplingEventIdentifier, RouteIdentifier, collection, survey_year, survey_month, survey_day)
date<-subset(date, is.na(survey_year) | is.na(survey_month)  | is.na(survey_day)) 

### Missing locational information

local<-in.data %>% select(RouteIdentifier, SiteCode, collection, latitude, UTMNorthing)
local<-subset(local, is.na(latitude) & is.na(UTMNorthing)) 
local<-local %>% distinct()
#missing 2111 coordinates some of which don't have site codes and willl therefore not be easy to fix
local<-local %>% filter(!is.na(SiteCode))
#this leaves us with 2017 sites without coodinates
localsum<- local %>% group_by(collection) %>% summarise(n=length(SiteCode))


##create a table for each province with missing data for QAQC

prov<-unique(in.data$collection)

for(k in 1:length(prov)) {

# for testing one province at a time  
# k<-6

dat<-in.data %>%  filter(collection == prov[k])
dat<-dat %>%  select (SamplingEventIdentifier, RouteIdentifier, SiteCode, survey_year, survey_month, survey_day, latitude, UTMNorthing)
dat<-subset(dat, is.na(survey_year) | is.na(survey_month)  | is.na(survey_day) | is.na(RouteIdentifier) | is.na(SiteCode))

write.table(dat, file = paste(out.dir, "QAQC.",
            prov[k], ".csv", sep = ""), row.names = FALSE, append = FALSE, 
            quote = FALSE, sep = ",", col.names = TRUE)

}


##Try mapping point locations per region


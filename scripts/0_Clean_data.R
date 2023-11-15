######################################################################################
# Clean and Split data from Google Doc on Fish Response to 
# Also calculates effect sizes
#
# by Jarrett Byrnes & Alejandro Perez Matus 
#
# Last Updated Feb 8, 2023
#
# changelog
# February 8, 2023: Cleanup for github
# October 23, 2022: Use the 0_data_clean_split as reference 
# October 10, 2022: Prepare a new Code and run de he 1aFIshAuxFunctions and prepare a code with the new data from CSV run the Celan Data version 
# September 30, 2022: Run the analysis with the same data but from CSV.  
# July, 2022: Updated manually the data from multiple species names and re-cjhack the values and papers incorporated. 
# June 30, 2022: Download the Google Doc and incorporate the last studies. 
# April 5, 2018: Changed adding region code to speed efficiency, fixed Carr bug
# April 16, 2014: Added way to filter out year of publication
# Feb 6, 2014: Added kelp information
# Jan 1, 2014: Added cleaning for pelagic or demersal
# Jan 2, 2014: A lot of data cleaning and added in the Change Hedge's G (AKA D)
######################################################################################
#
setwd(here::here())

#load necessary libraries
library(ggplot2)
library(Hmisc)
library(tidyverse)
library(metafor)
library(plyr)
library(RCurl)
library(rgdal) 


masterFish <- read.csv2("data/FishKelpMetanalaysis.csv") %>% filter(Authors..Year.!="Levin and Hay (2002)", Mean.Depth..m. != "intertidal", )


str(masterFish)
dimnames(masterFish)
names(masterFish)
head(masterFish)


unique(masterFish$Authors..Year.)

##Evaluating NAs in principal variables
##Mean Kelp & no kelp
NAs<- masterFish %>% filter(is.na(Mean.kelp)) %>% summarise(Auth=unique(Authors..Year.));NAs
NAs<- masterFish %>% filter(is.na(Mean.no.Kelp)) %>% summarise(Auth=unique(Authors..Year.));NAs
NAs<- masterFish %>% filter(is.na(Error.Kelp)) %>% summarise(Auth=unique(Authors..Year.));NAs
NAs<- masterFish %>% filter(is.na(Error.no.Kelp)) %>% summarise(Auth=unique(Authors..Year.));NAs
NAs<- masterFish %>% filter(is.na(Error.Type)) %>% summarise(Auth=unique(Authors..Year.));NAs


#create a SD column for with and without kelp ##REVISAR
unique(masterFish$Error.Type)
dosSE<-masterFish %>% filter(Error.Type=="2SE"); dim(dosSE)
SE<-masterFish %>% filter(Error.Type=="SE"); dim(SE)
NAs<-masterFish %>% filter(is.na(Error.Type)); dim(NAs)



seIDX <- grep("SE", masterFish$Error.Type)
se2IDX <- grep("2SE", masterFish$Error.Type)

masterFish$sd_kelp <- masterFish$Error.Kelp
masterFish$sd_kelp[seIDX] <- masterFish$Error.Kelp[seIDX] * sqrt(masterFish$N..Kelp.[seIDX])
masterFish$sd_kelp[se2IDX] <- masterFish$Error.Kelp[se2IDX]/2 * sqrt(masterFish$N..Kelp.[se2IDX])

#create a SD column for with and without kelp
masterFish$sd_nokelp <- masterFish$Error.no.Kelp
masterFish$sd_nokelp[seIDX] <- masterFish$Error.no.Kelp[seIDX] * sqrt(masterFish$N..no.kelp.[seIDX])
masterFish$sd_nokelp[se2IDX] <- masterFish$Error.no.Kelp[se2IDX]/2 * sqrt(masterFish$N..no.kelp.[se2IDX])

#######
# Calculate Effect Sizes
#######

masterFish$rawLR <- log(masterFish$Mean.kelp+1) - log(masterFish$Mean.no.Kelp+1)

#check
masterFish %>% 
  ggplot(aes(x = log(Mean.kelp+1))) +
  geom_histogram()

#look at result
names(masterFish)
#masterFish$Mean.kelp+1
#masterFish$Mean.no.Kelp+1


# Removing NAs for model selection functions, checking to 
# make sure everything is OK after manipulation

masterFish <- masterFish[!is.na(masterFish$Mean.kelp), ]
dim(masterFish)
masterFish <- masterFish[!is.na(masterFish$Mean.no.Kelp), ]
dim(masterFish)

masterFish <- as.data.frame(escalc("ROM", m2i = Mean.no.Kelp+1, m1i = Mean.kelp+1, 
                                   sd2i = sd_nokelp, sd1i = sd_kelp, 
                                   n2i = N..no.kelp., n1i = N..Kelp., data=masterFish, var.names=c("LR", "VLR")))

masterFish <- as.data.frame(escalc("SMD", m2i = Mean.no.Kelp, m1i = Mean.kelp, 
                                   sd2i = sd_nokelp, sd1i = sd_kelp, 
                                   n2i = N..no.kelp., n1i = N..Kelp., data=masterFish, 
                                   var.names=c("Hedges.G", "VHedges.G")))
unique(masterFish$Authors..Year.)


# clean the inconsistencies in masterFish variables from typos
# next time use data validation!

masterFish$Trophic.group= as.factor(masterFish$Trophic.group)
levels(factor(masterFish$Trophic.group))

#Invertivore
masterFish$Trophic.group <- gsub("invertivore ", "Invertivore", 
                                 masterFish$Trophic.group)
masterFish$Trophic.group <- gsub("invertivore", "Invertivore", 
                                 masterFish$Trophic.group)
#Carnivores
masterFish$Trophic.group <- gsub("Piscivore", "Carnivore", 
                                 masterFish$Trophic.group)
masterFish$Trophic.group <- gsub("piscivore", "Carnivore", 
                                 masterFish$Trophic.group)
#Omnivore
masterFish$Trophic.group <- gsub("omnivore", "Omnivore", 
                                 masterFish$Trophic.group)
masterFish$Trophic.group <- gsub("Herbivore and Invertivore", "Omnivore", 
                                 masterFish$Trophic.group)

#Herbivore
masterFish$Trophic.group <- gsub("herbivore", "Herbivore", 
                                 masterFish$Trophic.group)
#Planctivore, Detritivore and Invertivore remained the same
levels(factor(masterFish$Trophic.group))
write.csv(masterFish, "masterFish_clean.csv", row.names=F)


#Type.of.Study
levels(masterFish$Type.of.Study..Experimental_ObservatioNAl.)
masterFish$Type.of.Study..Experimental_ObservatioNAl. <- gsub("Experimental ", "Experimental", 
                                 masterFish$Type.of.Study..Experimental_ObservatioNAl.)
masterFish$Type.of.Study..Experimental_ObservatioNAl. <- gsub("Observational ", "Observational", 
                                                              masterFish$Type.of.Study..Experimental_ObservatioNAl.)
masterFish$Type.of.Study..Experimental_ObservatioNAl. <- gsub("experimental", "Experimental", 
                                                              masterFish$Type.of.Study..Experimental_ObservatioNAl.)
levels(factor(masterFish$Type.of.Study..Experimental_ObservatioNAl.))
#during data entry, some SDs got flipped in sign

masterFish$sd_kelp <- abs(masterFish$sd_kelp)
masterFish$sd_nokelp <- abs(masterFish$sd_nokelp)

#During data entry, some % signs were added by accident
#removePercent <- function(x){
 # x <- as.character(x)
#  x <- gsub("%", "", x)
#  return(as.numeric(x))
#}
#masterFish$Mean.kelp.t0 <- removePercent(masterFish$Mean.kelp.t0)
#masterFish$Error.Kelp.t0 <- removePercent(masterFish$Error.Kelp.t0)
#masterFish$Error.no.Kelp.t0 <- removePercent(masterFish$Error.no.Kelp.t0)
#people had creative interpretations of the words yes and no
#cleanYesNo <- function(x){
 # x <- as.character(x)
  #x[grep("[y,Y]",x)] <- "yes"
  #x[grep("[n,N]", x)] <- "no"
  #x[grep("[o,O]", x)] <- "no" #some entries are 'bo'
  #x
#}

#Yes, no --> Single.Species...yes.no.
levels(masterFish$Single.Species...yes.no.)
masterFish$Single.Species...yes.no. <- gsub("Yes", "yes", masterFish$Single.Species...yes.no.)
masterFish$Single.Species...yes.no. <- gsub("no ", "no", masterFish$Single.Species...yes.no.)
masterFish$Total.Abundance...yes.no. <- gsub("No", "no", masterFish$Total.Abundance...yes.no.)
masterFish$Species.Richness...yes.no. <- gsub("No", "no", masterFish$Species.Richness...yes.no.)
masterFish$Biomass...yes.no. <- gsub("No", "no", masterFish$Biomass...yes.no.)
masterFish$Biomass...yes.no. <- gsub("no ", "no", masterFish$Biomass...yes.no.)
masterFish$Biomass...yes.no. <- gsub("no ", "no", masterFish$Biomass...yes.no.)
write.csv(masterFish, "masterFish_clean.csv", row.names=F)

#Mean.Depth
#prune out intertidal studies as we're talking subtidal
#Manually the mean Depth were changed to... 
#Lortesen (2010) changed to 15
#Villegas changed to 15
#NAs changed to 1 
#To deal with ranges
masterFish$Mean.Depth..m.  <- gsub("2 to 14", "8", masterFish$Mean.Depth..m. )
masterFish$Mean.Depth..m.  <- gsub("2 to 16", "9", masterFish$Mean.Depth..m. )
masterFish$Mean.Depth..m.  <- gsub("2 to 22", "12", masterFish$Mean.Depth..m. )
masterFish$Mean.Depth..m. <- as.numeric(masterFish$Mean.Depth..m.)


#Fix inferred mechanism. Only valid words are
#experiment, storm, heat wave, barren, kelp addition
levels(masterFish$Inferred.Mechanism.of.Clearing..experiment_storm_heat.wave_barren_kelp.addition.)
fixInfer <- masterFish$Inferred.Mechanism.of.Clearing..experiment_storm_heat.wave_barren_kelp.addition.
levels(fixInfer)
fixInfer <- gsub("Barren vs. Kelp", "barren", fixInfer)
fixInfer <- gsub("Barren", "barren", fixInfer)
fixInfer <- gsub("Experimental (subcanopy)", "experiment", fixInfer)
fixInfer <- gsub("experimental (subcanopy)", "experiment", fixInfer)
fixInfer <- gsub("Experiment", "experiment", fixInfer)
fixInfer <- gsub("kelp Addition ", "kelp addition", fixInfer)
fixInfer <- gsub("Kelp Addition ", "kelp addition", fixInfer)
fixInfer <- gsub("kelp addition", "kelp addition", fixInfer)
fixInfer <- gsub("experimental (subcanopy)", "experiment", fixInfer)
levels(factor(fixInfer))
fixInfer -> masterFish$Inferred.Mechanism.of.Clearing..experiment_storm_heat.wave_barren_kelp.addition.
levels(masterFish$Authors..Year.)

#Fix bad spatial/temporal entries
#Spatial, Temporal only valid entries
levels(masterFish$Observational.Study.Type..Spatial..Temporal.)
masterFish$Observational.Study.Type..Spatial..Temporal.  <- gsub("Temporal ", "Temporal", masterFish$Observational.Study.Type..Spatial..Temporal. )
masterFish$Observational.Study.Type..Spatial..Temporal.  <- gsub("Spatial ", "Spatial", masterFish$Observational.Study.Type..Spatial..Temporal. )
masterFish$Observational.Study.Type..Spatial..Temporal.  <- gsub("temporal", "Temporal", masterFish$Observational.Study.Type..Spatial..Temporal. )
masterFish$Observational.Study.Type..Spatial..Temporal.  <- gsub("spatial", "Spatial", masterFish$Observational.Study.Type..Spatial..Temporal. )
masterFish$Observational.Study.Type..Spatial..Temporal.  <- as.factor(masterFish$Observational.Study.Type..Spatial..Temporal.)
levels(masterFish$Authors..Year.)

#Clearing.Response..algae..barren..kelp. cleanup
#allowed words
#algae, barren, kelp
#I'm making some assumptions until Ale fixes this in the raw data
levels(masterFish$Clearing.Response..algae_barren_kelp.)
masterFish$Clearing.Response..algae_barren_kelp. <- gsub("Kelp", "kelp", masterFish$Clearing.Response..algae_barren_kelp.)
masterFish$Clearing.Response..algae_barren_kelp. <- gsub("No", "no", masterFish$Clearing.Response..algae_barren_kelp.)
masterFish$Clearing.Response..algae_barren_kelp.<-as.factor(masterFish$Clearing.Response..algae_barren_kelp.)

levels(masterFish$Authors..Year.)

#Survey type
#transect, quadrat, point count, 
#Net/Trap (SMURF/light trap, Net)
#RUV (video, ROV, transect,video, Video)
fixSurvey <- masterFish$Survey.Type..quadrat..transect..video..photo..Net.Trap.
fixSurvey <- as.factor(fixSurvey)
levels(fixSurvey)
fixSurvey <- gsub("Transect", "transect", fixSurvey)
fixSurvey <- gsub("transect ", "transect", fixSurvey)
fixSurvey <- gsub("Quadrat", "quadrat", fixSurvey)
fixSurvey <- gsub("Point Count ", "point count", fixSurvey)
fixSurvey <- gsub("SMURF/light trap", "Net/Trap", fixSurvey)
fixSurvey <- gsub("Net", "Net/Trap", fixSurvey)
fixSurvey <- gsub("Net/Trap/Trap", "Net/Trap", fixSurvey)
fixSurvey <- gsub("transect, video", "RUV", fixSurvey)
fixSurvey <- gsub("ROV", "RUV", fixSurvey)
fixSurvey <- gsub("Transect and video", "RUV", fixSurvey)
fixSurvey <- gsub("transect, video", "RUV", fixSurvey)
fixSurvey <- gsub("Video", "RUV", fixSurvey)
fixSurvey <- gsub("transectand video", "RUV", fixSurvey)
fixSurvey <-as.factor(fixSurvey)
levels(fixSurvey)
masterFish$Survey.Type..quadrat..transect..video..photo..Net.Trap. -> fixSurvey
levels(masterFish$Authors..Year.)


#clean masterFish$Habitat.Association
#Benthic, Demersal, Pelagic, Benthopelagic, Benthodemersal, Pelagic-Oceanic, Reef associated 
levels(factor(masterFish$Habitat.Association..Benthic.Pelagic.Demersal.))
fixHabitat <- masterFish$Habitat.Association..Benthic.Pelagic.Demersal.
fixHabitat <- gsub("BenthoDemersal", "Demersal", fixHabitat)
fixHabitat <- gsub("benthopelagic", "Demersal", fixHabitat)
fixHabitat <- gsub("Benthopelagic", "Demersal", fixHabitat)
fixHabitat <- gsub("Pelagic", "Pelagic SA", fixHabitat)
fixHabitat <- gsub("Pelagic-Oceanic", "Pelagic movile", fixHabitat)
fixHabitat <- gsub("Reef associated", "Benthic", fixHabitat)
fixHabitat <- gsub("demersal", "Demersal", fixHabitat)
fixHabitat <- gsub("BenthoDemersal", "Demersal", fixHabitat)
levels(factor(fixHabitat))
masterFish$Habitat.Association..Benthic.Pelagic.Demersal. <- fixHabitat
masterFish$Habitat.Association..Benthic.Pelagic.Demersal.<- as.factor(masterFish$Habitat.Association..Benthic.Pelagic.Demersal.)
levels(masterFish$Habitat.Association..Benthic.Pelagic.Demersal.)
levels(masterFish$Authors..Year.)

write.csv(masterFish, "data/masterFish_clean.csv", row.names=F)

#clean adult/juvenile/both

levels(masterFish$Juvenile..Adult..Both)

masterFish$Juvenile..Adult..Both <- gsub("Adults", "Adult", masterFish$Juvenile..Adult..Both)
masterFish$Juvenile..Adult..Both <- gsub("Adult ", "Adult", masterFish$Juvenile..Adult..Both)
masterFish$Juvenile..Adult..Both <- gsub("Both ", "Both", masterFish$Juvenile..Adult..Both)
masterFish$Juvenile..Adult..Both<- as.factor(masterFish$Juvenile..Adult..Both)
levels(masterFish$Juvenile..Adult..Both)

levels(factor(masterFish$Authors..Year.))




### This was done using the time 0 in experiments where they counted fish before the kelp removal, this is not relevant 
#pooledSD <- function(sd1, sd2, n1, n2){
#  v1 <- sd1^2
#  v2 <- sd2^2
#  vpool <- ((n1-1)*v1+(n2-1)*v2)/(n1+n2-2)
#  sqrt(vpool)
#}
  # Removing NAs for at Time 0 for model selection
  
 # masterFish <- masterFish[!is.na(masterFish$Mean.kelp.t0), ]
# dim(masterFish)
 # masterFish <- masterFish[!is.na(masterFish$Mean.no.Kelp.t0 ), ]
 # dim(masterFish)

#  masterFish
#masterFish <- within(masterFish, {
#  kelpDiff <-  - Mean.kelp - Mean.kelp.t0
#  kelpDiffSD <-  pooledSD(sd_kelp, sd_kelp.t0,N.kelp,N.kelp)
#  noKelpDiff <-   Mean.no.Kelp - Mean.no.Kelp.t0  
#  noKelpDiffSD <-  pooledSD(sd_nokelp, sd_nokelp.t0,N..no.kelp.,N..no.kelp.)
#})

#masterFish <- as.data.frame(escalc("SMD", m2i = noKelpDiff, m1i = kelpDiff, 
                               #    sd2i = noKelpDiffSD, sd1i = kelpDiffSD, 
                              #     n2i = 2*N..no.kelp., n1i = 2*N..Kelp., data=masterFish, 
                                #   var.names=c("Change.Hedges.G", "Change.VHedges.G")))



########## Merge in Kelp Life History info

#kelpCSV <- getURL("https://docs.google.com/spreadsheet/pub?key=0AoWXIZo5tNL1dEJxXzF4TnRBOXdVZExHMjRLSlhSY1E&single=true&gid=0&output=csv")
#### Incorporated the variables of kelp. 
masterKelp<-read.csv("data/Fish-Kelp Kelp Taxon Information - Sheet1.csv") 
#masterKelp <- read.csv(textConnection(kelpCSV), na.strings=c("na", "NA", "", "Na", "nA", "N/A", " "))
#write.csv(masterKelp, "data/masterKelp.csv")
#masterKelp <- read.csv("data/masterKelp.csv")
names(masterKelp)
#rm(masterKelp)
#first, fix it up!!!!!

levels(factor(masterKelp$Single.or.Multi.Stipe))
masterKelp$Single.or.Multi.Stipe <- gsub("Multi/Single/Single/Multi/Multi", "Multi", masterKelp$Single.or.Multi.Stipe)
masterKelp$Single.or.Multi.Stipe <- gsub("Multi/Single" , "Multi", masterKelp$Single.or.Multi.Stipe)
masterKelp$Single.or.Multi.Stipe <- gsub("Multi/Single/Single/Multi/Multi" , "Multi", masterKelp$Single.or.Multi.Stipe)


levels(masterKelp$Erect.or.Prostrate)
masterKelp$Erect.or.Prostrate<-gsub("Errect/Prostrate","Both", masterKelp$Erect.or.Prostrate)
masterKelp$Erect.or.Prostrate<-gsub("Prostate","Prostrate", masterKelp$Erect.or.Prostrate) 
dim(masterFish)

#make some fixes for Kelp taxon
#Dean et al 2000 is in masterFish but not in masterKelp
Dean2000 <- data.frame(Kelp.Taxon = "Agarum cribosum, Saccharina latissima",
                       Patch = "Mixed",
                       Height..m.="1.50",
                       Single.or.Multi.Stipe="Single",
                       Erect.or.Prostrate="Prostrate",
                       Epibiont.Load.Potential="Low",
                       Comment="")
Loretsen2010 <- data.frame(Kelp.Taxon = "Laminaria hyperborea",
                       Patch = "Single",
                       Height..m.="3.60",
                       Single.or.Multi.Stipe="Single",
                       Erect.or.Prostrate="Prostrate",
                       Epibiont.Load.Potential="High",
                       Comment="")
                       
masterKelp<-rbind(masterKelp,Dean2000)
masterKelp<-rbind(masterKelp,Loretsen2010)

#masterfish
masterFish$Kelp.Taxon <- gsub("Cystophora retroflexa", "Cystophora spp", masterFish$Kelp.Taxon)                         
masterFish$Kelp.Taxon <- gsub("Ecklonia radiata\n", "Ecklonia radiata", masterFish$Kelp.Taxon)                         
masterFish$Kelp.Taxon <- gsub("Macrocystis", "Macrocystis pyrifera", masterFish$Kelp.Taxon)                
masterFish$Kelp.Taxon <- gsub("Macrocystis pyrifera pyrifera " ,"Macrocystis pyrifera",masterFish$Kelp.Taxon)
masterFish$Kelp.Taxon <- gsub("Macrocystis pyrifera pyrifera" ,"Macrocystis pyrifera",masterFish$Kelp.Taxon)
masterFish$Kelp.Taxon <- gsub("Macrocystis pyriferapyrifera" ,"Macrocystis pyrifera",masterFish$Kelp.Taxon)
masterFish$Kelp.Taxon <- gsub("Macrocystis pyrifera pyrifera, Carphophyllum maschalocarpum","Macrocystis pyrifera, Carphophyllum maschalocarpum",masterFish$Kelp.Taxon)
masterFish$Kelp.Taxon <- gsub("Macrocystis pyrifera, Pterygophora, Lainaria farlowii, Eisenia arborea, Egrezia menziesee, Cystoceira osmundancea","Macrocystis, Pterygophora, Laminaria farlowii, Eisenia arborea, Egregia menziesii, Cystoseira osmundacea",masterFish$Kelp.Taxon)
masterFish$Kelp.Taxon <- gsub("Pterygophora california, Laminaria farlowii","Pterygophora californica, Laminaria farlowii",masterFish$Kelp.Taxon)

kelp<-levels(factor(masterKelp$Kelp.Taxon))
fish<-levels(factor(masterFish$Kelp.Taxon))




#masterFish$Kelp.Taxon <- gsub("Macrocystis pyrifera, Pterygophora california, Laminaria farlowii", "Macrocystic, Pterygophora californica & Laminaria farlowii" , masterFish$Kelp.Taxon) 
#masterFish$Kelp.Taxon <- gsub("Pterygophora california, Laminaria farlowii", "Pterygophora californica & Laminaria farlowii", masterFish$Kelp.Taxon)  


#masterkelp
masterKelp$Kelp.Taxon <- gsub("Carpophyllum flexuosum and Ecklonia radiata","Carpophyllum flexuosum, Ecklonia radiata", masterKelp$Kelp.Taxon)                         
masterKelp$Kelp.Taxon <- gsub("Laminaria longicruris & Laminaria digitata", "Laminaria longicruris, Laminaria digitata", masterKelp$Kelp.Taxon)                         
masterKelp$Kelp.Taxon <- gsub("Macrocystic, Pterygophora californica & Laminaria farlowii","Macrocystis pyrifera, Pterygophora california, Laminaria farlowii", masterKelp$Kelp.Taxon)
masterKelp$Kelp.Taxon <- gsub("Macrocystis pyrifera& Carphophyllum maschalocarpum", "Macrocystis pyrifera, Carphophyllum maschalocarpum", masterKelp$Kelp.Taxon)                
masterKelp$Kelp.Taxon <- gsub("Macrocystis pyrifera& Cystophora spp", "Macrocystis pyrifera, Cystophora spp", masterKelp$Kelp.Taxon)
masterKelp$Kelp.Taxon <- gsub("Macrocystis pyrifera, Carpophyllum maschalocarpum & Cystophora spp","Macrocystis pyrifera, Carpophyllum maschalocarpum , Cystophora spp",  masterKelp$Kelp.Taxon)
masterKelp$Kelp.Taxon <- gsub("Macrocystis pyriferaand Lessonia trabeculata", "Macrocystis pyrifera, Lessonia trabeculata", masterKelp$Kelp.Taxon)
masterKelp$Kelp.Taxon <- gsub("Pterygophora californica & Laminaria farlowii"  , "Pterygophora californica, Laminaria farlowii", masterKelp$Kelp.Taxon)
masterKelp$Kelp.Taxon <- gsub("Macrocystis pyrifera, Pterygophora california, Laminaria farlowii","Macrocystis pyrifera, Pterygophora californica, Laminaria farlowii",masterKelp$Kelp.Taxon)
kelp<-levels(factor(masterKelp$Kelp.Taxon))
fish<-levels(factor(masterFish$Kelp.Taxon))

#then, merge it with the main file
masterFish <- merge(masterFish, masterKelp, by="Kelp.Taxon")

#write.csv(masterFish, "data/masterFish_clean.csv", row.names=F)


###########
# Add biogeography
###########
#source("./0a_getRegions.R")
library(meowR)
data(regions)
# they take a while, as the raw data file is large...must be a better way
##correction for long in Anderson and Millar (2004)




getRegions(masterFish$Latititude.in.Decimal.Degrees[1], masterFish$Longitude.in.Decimal.Degrees[1])
unique(masterFish$Latititude.in.Decimal.Degrees)
unique(masterFish$Longitude.in.Decimal.Degrees)

#get the unique lat/long pairs - speeds things up 5x!
uniqueLatLong <- ddply(masterFish, .(Latititude.in.Decimal.Degrees, Longitude.in.Decimal.Degrees), summarise, len = length(paste(Latititude.in.Decimal.Degrees,Longitude.in.Decimal.Degrees )))
#uniqueLatLong <- na.omit(uniqueLatLong)
allRegions <- lapply(1:nrow(uniqueLatLong), function(i)  getRegions(uniqueLatLong$Latititude.in.Decimal.Degrees[i], uniqueLatLong$Longitude.in.Decimal.Degrees[i]))
allRegions <- ldply(allRegions, function(x) sapply(x[1,], as.character))
allRegions <- cbind(uniqueLatLong, allRegions)



masterFish <- left_join(masterFish, allRegions) 
d <- masterFish %>% filter(is.na(ECOREGION))

#write.csv(masterFish[sort(masterFish$series, index.return=TRUE)$ix,], "../latLongMasterFishCheck.csv", row.names=FALSE)

####### Output

#filter so that there is only the first and last time point for each study
#filter to final data point for each study/species
#make sure to do it for each type of data file we'll be writing out at the end
#library(plyr)
#masterFishTimefiltered <- ddply(masterFish, 
        #                        .(Authors.Year, Site, Fish.Species, 
        #                          Type.of.study,
        #                          Single.species,
        #                          Total.Abundance,
        #                          Species.Richness,
         #                         Biomass), 
                                
       #                         function(adf){
                                  #if there's only one point, kick that back
          #                        if(nrow(adf)==1) return(adf)
                                  
                                  #if there are NAs, we cannot judge this properly, so kick it back
          #                        if(sum(is.na(adf$Time.Until.Resampling..days.))>0) return(adf)
                                  
                                  #otherwise, get the last data point
           #                       retIDX <- which(adf$Time.Until.Resampling..days. == max(adf$Time.Until.Resampling..days.))
                                  
           #                       return(adf[retIDX,])
           #                     })

#masterFishTimefiltered$Pub.Year <- as.character(masterFishTimefiltered$Authors.Year)
#levels(factor(masterFishTimefiltered$Pub.Year))



#masterFishTimefiltered$Pub.Year <- gsub("\\.", "", masterFishTimefiltered$Pub.Year)
#masterFishTimefiltered$Pub.Year<- gsub("\\&", "", masterFishTimefiltered$Pub.Year)
#masterFishTimefiltered$Pub.Year<- gsub("\\'", "", masterFishTimefiltered$Pub.Year)
#masterFishTimefiltered$Pub.Year<- gsub("\\)", "", masterFishTimefiltered$Pub.Year)
#masterFishTimefiltered$Pub.Year<- gsub("\\(", "", masterFishTimefiltered$Pub.Year)
#masterFishTimefiltered$Pub.Year <- gsub("\\-", "", masterFishTimefiltered$Pub.Year)
#masterFishTimefiltered$Pub.Year <- gsub("([A-Z,a-z])*", "", masterFishTimefiltered$Pub.Year)
#masterFishTimefiltered$Pub.Year <- gsub("[[:space:]]*", "", masterFishTimefiltered$Pub.Year)


#write the masterFish to a csv file
write.csv(masterFish, "data/masterFish_clean.csv", row.names=F)



#########
# Create filtered data sets by Response type
# Total abundance, Individual Species, Species Richness
single_sp_abund_clean<-subset(masterFish, masterFish$Single.Species...yes.no.=="yes") %>%  filter(!is.na(VHedges.G ))
#########
write.csv(subset(masterFish, masterFish$Single.Species...yes.no.=="yes"), "data/single_sp_abund_clean.csv", row.names=F)
write.csv(subset(masterFish, masterFish$Total.Abundance...yes.no.=="yes"), "data/total_abund_clean.csv", row.names=F)
write.csv(subset(masterFish, masterFish$Species.Richness...yes.no.=="yes"), "data/sp_rich_clean.csv", row.names=F)
write.csv(subset(masterFish, masterFish$Biomass...yes.no.=="yes"), "data/biomass_clean.csv", row.names=F)


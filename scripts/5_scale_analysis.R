############
# Analyses and Figures of Effects of Scale on Richness and Abundance
############

#Load libraries and Data
library(metafor)
library(dplyr)
library(tibble)
library(glue)
library(broom)
library(broom.mixed)
library(ggplot2)
library(patchwork)
theme_set(theme_classic(base_size = 24) )

source("scripts/open_all_data_for_analysis.R")

#### Analysis of scale effects on richness
rma.mv(Hedges.G~ Clearing.Size..m.2., 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data=richness %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental"),
       control=list(optimizer="nlminb"))

rma.mv(Hedges.G~ sample.unit.area..m.2., 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data = richness %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"),
       control=list(optimizer="nlminb"))

#### Analysis of scale effects on total abundance
rma.mv(Hedges.G~ Clearing.Size..m.2., 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data=totalabund %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental"),
       control=list(optimizer="nlminb"))

rma.mv(Hedges.G~ sample.unit.area..m.2., 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data = totalabund %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"),
       control=list(optimizer="nlminb"))

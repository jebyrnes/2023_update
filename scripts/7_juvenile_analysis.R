############
# Analyses and Figures of Effects of Study Type and Morphology on
# Juveniles
############

#Load libraries and Data
library(metafor)
library(dplyr)
library(tibble)
library(glue)
library(broom)
library(broom.mixed)
library(ggplot2)
library(wesanderson)
theme_set(theme_classic(base_size = 20) )

source("scripts/open_all_data_for_analysis.R")

juveniles <- singleSpAbund |>
  filter(Juvenile..Adult..Both == "Juvenile")

# Analysis of Study Type 
juvHedges <- rma.mv(Hedges.G~ 0 + 
                           Type.of.Study..Experimental_ObservatioNAl., 
                         VHedges.G, 
                         random=list(~1|Authors..Year., ~Authors..Year.|Site), 
                         struct="HCS",rho=0,
                         data=juveniles,
                         verbose=TRUE, control=list(rel.tol=1e-8))
tidy(juvHedges)

# Analysis of Morphology
juvMorphologyHedges <- rma.mv(Hedges.G~ 0 + 
                      Single.or.Multi.Stipe, 
                    VHedges.G, 
                    random=list(~1|Authors..Year., ~Authors..Year.|Site), 
                    struct="HCS",rho=0,
                    data=juveniles)

tidy(juvMorphologyHedges)

# Analysis of Study Type and Morphology

juvHedgesObs <- rma.mv(Hedges.G~ 0 + 
                         Single.or.Multi.Stipe, 
                       VHedges.G, 
                       random=list(~1|Authors..Year., ~Authors..Year.|Site), 
                       struct="HCS",rho=0,
                       data=juveniles |>
                         filter(Type.of.Study..Experimental_ObservatioNAl. == "Observational"))


juvHedgesExp <- rma.mv(Hedges.G~ 0 + 
                         Single.or.Multi.Stipe, 
                    VHedges.G, 
                    random=list(~1|Authors..Year., ~Authors..Year.|Site), 
                    struct="HCS",rho=0,
                    data=juveniles |>
                      filter(Type.of.Study..Experimental_ObservatioNAl. != "Observational"))
# results
tidy(juvHedgesObs) |>
  mutate(term =  gsub("Single.or.Multi.Stipe", "", term))

tidy(juvHedgesExp)|>
  mutate(term =  gsub("Single.or.Multi.Stipe", "", term))

juveniles |>
  group_by(Type.of.Study..Experimental_ObservatioNAl., 
           Single.or.Multi.Stipe) |>
  summarize(n = n(), .groups = "drop")

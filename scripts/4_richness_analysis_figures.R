############
# Analyses and Figures of Effects on Richness
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
source("scripts/make_three_panel_plot.R")

#Data for Figure Fig 3a, Richness 
richnessHedges <- rma.mv(Hedges.G~ 0 + 
                           Type.of.Study..Experimental_ObservatioNAl., 
                         VHedges.G, 
                         random=list(~1|Authors..Year., ~Authors..Year.|Site), 
                         struct="HCS",rho=0,
                         data=richness,
                         verbose=TRUE, control=list(rel.tol=1e-8))

summary(richnessHedges)
tidy(richnessHedges)


#### Fig 3b and 3c Richness and single v. multi for observational and exp data
rich_morphology_obs <- rma.mv(Hedges.G~ Single.or.Multi.Stipe+0, 
                              VHedges.G, 
                              random=list(~1|Authors..Year.),
                              data=richness %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"),
                              control=list(optimizer="nlminb"))

rich_morphology_exp <- rma.mv(Hedges.G~ Single.or.Multi.Stipe+0, 
                              VHedges.G, 
                              random=list(~1|Authors..Year.),
                              data=richness %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental"),
                              control=list(optimizer="nlminb"))
summary(rich_morphology_obs) # 72 studies - 21 multi, 51 single
summary(rich_morphology_exp) # 14 studies - only 4 are single stipe



### Make figs from models
fig_3_panels <- get_model_figs(richnessHedges,
                               rich_morphology_obs,
                               rich_morphology_exp,
                               richness,
                               ylim_vals = c(-6,7))

### Combine fig panels
make_3_panel_fig((fig_3_panels[[1]] + 
                    ylab("Effect Size of Kelp on\nFish Species Richness (Hedge's G)")) +
                   fig_3_panels[[2]] +
                   fig_3_panels[[3]]) 

ggsave(file = "figures/Figure_3_Richness_type_morphology.jpg",
       dpi=300,
       width = 17,
       height = 10)


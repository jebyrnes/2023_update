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
library(patchwork)
theme_set(theme_classic(base_size = 24) )

source("scripts/open_all_data_for_analysis.R")
source("scripts/make_three_panel_plot.R")


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

### Make Figure 5 - 3 panel figure
fig_5_panels <- get_model_figs(juvHedges,
                               juvHedgesObs,
                               juvHedgesExp,
                               juveniles,
                               ylim_vals = c(-5,5))

### Combine fig panels
make_3_panel_fig(fig_5_panels)

ggsave(file = "figures/Figure_5_juveniles_type_morphology.jpg",
       dpi=300,
       width = 15)


# check errant values
juveniles |>
  filter(Hedges.G > 5) |>
  select(3:4, Mean.kelp ,
         Mean.no.Kelp,
         sd_kelp,
         sd_nokelp,
         N..Kelp.,
         N..no.kelp.,
         Hedges.G) |> View()




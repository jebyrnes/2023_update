############
# Analyses and Figures of Total Abundance Effects
############

#Load libraries and Data
library(metafor)
library(dplyr)
library(tibble)
library(glue)
library(broom)
library(broom.mixed)
library(ggplot2)
theme_set(theme_classic(base_size = 24) )

source("scripts/open_all_data_for_analysis.R")
source("scripts/make_three_panel_plot.R")

# Analysis by ecoregion
# using -1 so that we have no reference/intercept region
totalHedgesEco <-  rma.mv(Hedges.G ~ ECOREGION-1,
                          VHedges.G, 
                          method="REML",
                          struct="CS", 
                          random=list(~1|Authors..Year., ~Authors..Year.|Site),
                          data=totalabund,
                          control=list(optimizer="nlminb"))
# output
summary(totalHedgesEco)

broom.mixed::tidy(totalHedgesEco) |>
  mutate(term = gsub("ECOREGION", "", term))


### Supp Figure 6, effect size by ecoregion 
smseco <- coef(summary(totalHedgesEco)) |>
  rownames_to_column(var = "Type") |>
  mutate(Type = gsub("ECOREGION", "", Type)) |>
  as_tibble()

total_samp <- totalabund |>
  group_by(ECOREGION) |>
  summarize(n = n(), .groups = "drop") |>
  filter(!is.na(ECOREGION)) |>
  rename(Type = ECOREGION)

smseco <- left_join(smseco, total_samp) |>
  mutate(Type = glue("{Type} ({n})"))


ggplot(smseco, aes(x = estimate, y=Type)) +
  geom_errorbarh(aes(xmax = ci.ub, xmin=ci.lb, height=0.2)) +
  geom_point(data=smseco, mapping=aes(y=Type, x=estimate), 
             colour='red', size=3) +
  labs(y = "", x = "Hedge's G") +
  geom_vline(xintercept=0, lty = 2) #+
# geom_point(data = totalabund,
#            aes(x = Hedges.G, y = ECOREGION),
#            color = "lightgrey", alpha = 1)
ggsave(file = "figures/Supp_6_TotalAbundance_Ecoregion.jpg",
       dpi=300)



##### Analysis/Figure 3a, Observational v. Experimental effects on total abundance
totalHedges <-  rma.mv(Hedges.G~ 0 + 
                         Type.of.Study..Experimental_ObservatioNAl., intercept=TRUE,
                       VHedges.G,struct="CS", verbose = TRUE,
                       random=list(~1|Authors..Year., ~Authors..Year.|Site),
                       data=totalabund, control=list(rel.tol=1e-8))

summary(totalHedges)
tidy(total(Hedges))


##### Analysis/Figure 3b Single Stipe v. Multi-stipe effects on total abundance
##### using only observational data
singleMultiObs <- rma.mv(Hedges.G~ 0 + Single.or.Multi.Stipe, 
                         VHedges.G, 
                         random=list(~1|Authors..Year.),
                         data=totalabund %>% 
                           filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"),
                         control=list(optimizer="nlminb"))
summary(singleMultiObs)
tidy(singleMultiObs)

####

##### Analysis/Figure 3c Single Stipe v. Multi-stipe effects on total abundance
##### using only experimental data
singleMultiTotalExp <- rma.mv(Hedges.G~ 0 + Single.or.Multi.Stipe, 
                              VHedges.G, 
                              random=list(~1|Authors..Year.),
                              data=totalabund %>% 
                                filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental"),
                              control=list(optimizer="nlminb"))
summary(singleMultiTotalExp)
tidy(singleMultiTotalExp)


### Make figs from models
fig_3_panels <- get_model_figs(totalHedges,
                               singleMultiObs,
                               singleMultiTotalExp,
                               totalabund,
                               ylim_vals = c(-5,5))

### Combine fig 3a and 3b
make_3_panel_fig(fig_3_panels)

ggsave(file = "figures/Figure_3_TotalAbundance_type_morphology.jpg",
       dpi=300,
       width = 15)


# check errant values
totalabund |>
  filter(Hedges.G > 3) |>
  select(3:4, Mean.kelp ,
         Mean.no.Kelp,
         sd_kelp,
         sd_nokelp,
         N..Kelp.,
         N..no.kelp.,
         Hedges.G) |> View()

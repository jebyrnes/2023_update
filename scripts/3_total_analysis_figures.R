
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


###Figure 2, effect size by ecoregion 
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
ggsave(file = "figures/TotalAbundance_Ecoregion.jpg",
       dpi=300)



#####
totalObsResponse <- totalabund %>%  
  group_by(Type.of.Study..Experimental_ObservatioNAl.,
           Clearing.Response..algae_barren_kelp.) %>%
  dplyr::summarise(n=sum(!is.na(VHedges.G))) %>%
  dplyr::rename(Type=Type.of.Study..Experimental_ObservatioNAl.) |>
  ungroup()

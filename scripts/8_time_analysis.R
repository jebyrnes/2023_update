############
# Analyses and Figures of Time Effects
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

# we are going to look at juveniles
juveniles <- singleSpAbund |>
  filter(Juvenile..Adult..Both == "Juvenile")

source("scripts/open_all_data_for_analysis.R")
source("scripts/make_three_panel_plot.R")

total_resampling <-  rma.mv(Hedges.G ~ Time.Until.Resampling..days.,
                          VHedges.G, 
                          method="REML",
                          struct="CS", 
                          random=list(~1|Authors..Year., ~Authors..Year.|Site),
                          data=totalabund|> filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental"),
                          control=list(optimizer="nlminb"))


rich_resampling <-  rma.mv(Hedges.G ~ Time.Until.Resampling..days.,
                            VHedges.G, 
                            method="REML",
                            struct="CS", 
                            random=list(~1|Authors..Year., ~Authors..Year.|Site),
                            data=richness|> filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental"),
                            control=list(optimizer="nlminb"))

juv_resampling <-  rma.mv(Hedges.G ~ Time.Until.Resampling..days.,
                            VHedges.G, 
                            method="REML",
                            struct="CS", 
                            random=list(~1|Authors..Year., ~Authors..Year.|Site),
                            data=juveniles |> filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental"),
                            control=list(optimizer="nlminb"))
# output
bind_rows(tidy(total_resampling) |> mutate(type = "Total"),
tidy(rich_resampling)|> mutate(type = "Richness"),
tidy(juv_resampling)|> mutate(type = "Juveniles"),
) |>
  filter(term != "intercept") |>
  select(type, estimate, std.error, statistic, p.value) |>
  rename(Model = type,
         `Effect of Time` = estimate,
         `Std. Error` = std.error,
         `p` = p.value) |>
  readr::write_csv("tables/supp_table_1_time_effect.csv")


## 
# Make some plots
##

dat_to_plot <- bind_rows(totalabund |> mutate(type = "Total", ID = as.character(ID)), 
                         richness|> mutate(type = "Richness", ID = as.character(ID)), 
                         juveniles|> mutate(type = "Juveniles", ID = as.character(ID))) |>
  filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental") |>
  select(ID, Authors..Year., Site, type, Hedges.G, VHedges.G, N..Kelp., N..no.kelp., Time.Until.Resampling..days.)

# check for balanced sample sizes
dat_to_plot$N..no.kelp. - dat_to_plot$N..Kelp.

# Show effect size against time
ggplot(dat_to_plot,
       aes(x = Time.Until.Resampling..days., y = Hedges.G)) +
  geom_point(alpha = 0.5, size = 4) +
  facet_wrap(~type) 
  labs(size = "Sample Size", 
         x = "Time Until Resampling (days)",
         y = "Hedge's G")


ggsave(file = "figures/Supp_7_time_hedges.jpg",
       dpi=300,
       width =  12,
       height = 5)


# Show distributions of time and sample size
ggplot(dat_to_plot,
       aes(x = Time.Until.Resampling..days.,
           y = N..Kelp.)) +
  geom_point(alpha = 0.5, size = 4) +
  facet_wrap(~type)  +
  scale_y_log10() +
  labs(y = "Sample Size", 
       x = "Time Until Resampling (days)")


ggsave(file = "figures/Supp_8_time_n.jpg",
       dpi=300,
       width =  12,
       height = 5)

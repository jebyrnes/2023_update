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


### Figure 2, effect size by ecoregion 
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
ggsave(file = "figures/Figure_2_TotalAbundance_Ecoregion.jpg",
       dpi=300)



##### Figure 3a, Observational v. Experimental effects on total abundance
totalHedges <-  rma.mv(Hedges.G~ 0 + 
                         Type.of.Study..Experimental_ObservatioNAl., intercept=TRUE,
                       VHedges.G,struct="CS", verbose = TRUE,
                       random=list(~1|Authors..Year., ~Authors..Year.|Site),
                       data=totalabund, control=list(rel.tol=1e-8))

summary(totalHedges)
sms2 <- coef(summary(totalHedges))
sms2$Type <- rownames(sms2)
levels(factor(sms2$Type))
sms2$Type <- gsub("Type.of.Study..Experimental_ObservatioNAl.", "", sms2$Type)

TotalAb <- totalabund %>% group_by(Type.of.Study..Experimental_ObservatioNAl.) %>%
  dplyr::summarise(n=sum(!is.na(VHedges.G))) %>%
  dplyr::rename(Type = Type.of.Study..Experimental_ObservatioNAl.)

sms2 <- left_join(sms2, TotalAb) |>
  mutate(exp_obs_with_samplesize = glue("{`Type`}\n({n})"))


#Fig 3a new 
fig_3a <- ggplot(sms2, aes(x=Type, y=estimate)) +
  geom_jitter(data=totalabund, mapping=aes(x=Type.of.Study..Experimental_ObservatioNAl., 
                                           y=Hedges.G), 
              alpha=0.7,
              color = "grey",
              position=position_jitter(width = .1), 
              size=3) +
  
   geom_point(size=7, color="red", position=position_dodge(width=0.5)) +
  geom_linerange(mapping = aes(ymin=ci.lb, ymax=ci.ub),
                 position=position_dodge(width=0.5),
                 linewidth = 1) +
   geom_hline(yintercept=0, lwd=1.4, lty=2) +
  ylim(c(-3, 3)) +
  ylab("Hedge's G") + xlab("Study Type") +
  scale_x_discrete(labels = sms2$exp_obs_with_samplesize)

fig_3a

##### Figure 3b Single Stipe v. Multi-stipe effects on total abundance
##### using only observational data
singleMultiTotal <- rma.mv(Hedges.G~ 0 + Single.or.Multi.Stipe, 
                           VHedges.G, 
                           random=list(~1|Authors..Year.),
                           data=totalabund %>% 
                             filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"),
                           control=list(optimizer="nlminb"))
summary(singleMultiTotal)

ctabSM <- coef(summary(singleMultiTotal))
ctabSM[["Kelp Morphology"]] <- c("Multi", "Single")

totalMorphSummary <- totalabund %>% 
  filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational") %>%
  group_by(Single.or.Multi.Stipe) %>%
  dplyr::summarise(n=sum(!is.na(VHedges.G))) %>%
  dplyr::rename(`Kelp Morphology` = Single.or.Multi.Stipe)

ctabSM <- left_join(ctabSM, totalMorphSummary) |>
  mutate(morph_with_samplesize = glue("{`Kelp Morphology`}\n({n})"))

# Fig 3.b plot
fig_3b <- ggplot(data = ctabSM, 
       aes(x=`Kelp Morphology`, y=estimate)) +
  geom_jitter(data = totalabund |> 
                filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"), 
              mapping=aes(x=Single.or.Multi.Stipe, 
                          y=Hedges.G), 
              alpha=0.7, 
              color = "grey",
              position=position_jitter(width = .1), 
              size = 3)+
  geom_point(size = 7, 
             color = "red", 
             position = position_dodge(width=0.5)) +
  geom_linerange(mapping = aes(ymin=ci.lb, ymax=ci.ub),
    position = position_dodge(width=0.5),
    linewidth = 1) +
  geom_hline(yintercept=0, lwd=1.4, lty=2)+
  ylim(c(-3, 3)) +
  ylab("Hedge's G") +
  scale_x_discrete(labels = ctabSM$morph_with_samplesize)

fig_3b


### Combine fig 3a and 3b
library(patchwork)
fig_3a + (fig_3b + labs(y = NULL))
ggsave(file = "figures/Figure_3_TotalAbundance_type_morphology.jpg",
       dpi=300,
       width = 10)

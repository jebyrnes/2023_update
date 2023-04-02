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

#Data for Figure Fig 4, Richness 
richnessHedges <- rma.mv(Hedges.G~ 0 + 
                           Type.of.Study..Experimental_ObservatioNAl., 
                         VHedges.G, 
                         random=list(~1|Authors..Year., ~Authors..Year.|Site), 
                         struct="HCS",rho=0,
                         data=richness,
                         verbose=TRUE, control=list(rel.tol=1e-8))

summary(richnessHedges)
sms3 <- coef(summary(richnessHedges))
sms3$Type <- rownames(sms3)
levels(factor(sms3$Type))
sms3$Type <- gsub("Type.of.Study..Experimental_ObservatioNAl.", "", sms2$Type)

TotalRichness <- richness %>% group_by(Type.of.Study..Experimental_ObservatioNAl.) %>%
  dplyr::summarise(n=sum(!is.na(VHedges.G))) %>%
  dplyr::rename(Type = Type.of.Study..Experimental_ObservatioNAl.)

sms3 <- left_join(sms3, TotalRichness) |>
  mutate(exp_obs_with_samplesize = glue("{`Type`}\n({n})"))


#Fig 4 Species Richness
ggplot(sms3, aes(x=Type, y=estimate)) +
  ylab("Hedge's G") + xlab("") +
  geom_jitter(data=richness, 
              mapping=aes(x=Type.of.Study..Experimental_ObservatioNAl., 
                          y=Hedges.G), 
              alpha=0.7,
              color = "grey",
              position=position_jitter(width = .1), size=3) +
  geom_point(size=7, color="red", position=position_dodge(width=0.5)) +
  geom_linerange(mapping = aes(ymin=ci.lb, ymax=ci.ub),
                 linewidth = 1) +
  ylim(c(-3, 3)) +
  geom_hline(yintercept=0, lwd=1, lty=2) +
  scale_x_discrete(labels = sms3$exp_obs_with_samplesize)


ggsave("figures/Figure_4_TotalRichness_Type.jpg",
       dpi = 300)


#### Richness and single v. multi for observational data
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

#coefficient table
rich_morph_coefs <- bind_rows(
  coef(summary(rich_morphology_obs)) |>
    mutate(`Type` = "Observational") |>
    tibble::rownames_to_column(var = "Kelp Morphology"),
  coef(summary(rich_morphology_exp)) |>
    mutate(`Type` = "Experimental") |>
    tibble::rownames_to_column(var = "Kelp Morphology")
) |>
  dplyr::rename(Hedges.G = estimate) |>
  mutate(`Kelp Morphology` = gsub("Single.or.Multi.Stipe", "",
                                  `Kelp Morphology`))

rich_n <- richness %>% 
  group_by(Type.of.Study..Experimental_ObservatioNAl., Single.or.Multi.Stipe) |>
  dplyr::summarize(n = n(), .groups = "drop") |>
  dplyr::rename(Type = Type.of.Study..Experimental_ObservatioNAl.,
                `Kelp Morphology` = Single.or.Multi.Stipe)

richness |>
  dplyr::rename(Type = Type.of.Study..Experimental_ObservatioNAl.,
                `Kelp Morphology` = Single.or.Multi.Stipe) |>
ggplot(aes(y = Hedges.G, x = Type)) +
  geom_jitter(alpha=0.7,
              color = "grey",
              position=position_jitter(width = .1), 
              size=3) +
  geom_point(data = rich_morph_coefs,
             size=7, color="red") +
  geom_linerange(data = rich_morph_coefs,
                 mapping = aes(ymin=ci.lb, ymax=ci.ub),
                 linewidth = 1) +
  geom_text(data = rich_n,
            aes(label = n),
            y = -4,
            size = 6) +
  facet_wrap(vars(`Kelp Morphology`)) +
  geom_hline(yintercept=0, lwd=1, lty=2) +
  labs(x=NULL, y = "Hedge's G")

ggsave("figures/Figure_X_Richness_Type_Morphology.jpg",
       dpi = 300, 
       width = 10)
  
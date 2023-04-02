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

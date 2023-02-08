
#ale Figures and analysis linked to the paper 


#Load libraries and Data
library(metafor)
library(nlme)
library(ggplot.multistats)
library(ggplot2)
library(gridExtra)
library(plyr)
library(dplyr)
library(meowR)
library(knitr)

singleSpAbund <- read.csv("data/single_sp_abund_clean.csv") %>% 
  filter(!is.na(VHedges.G )) 

totalabund<-read.csv("data/total_abund_clean.csv") %>% 
  filter(!is.na(VHedges.G )) %>% filter(Authors..Year.!="Steele et al. (2002)")

richness<-read.csv("data/sp_rich_clean.csv") %>% 
  filter(!is.na(VHedges.G )) 

biomass<-read.csv("data/biomass_clean.csv") 

masterData <- read.csv("data/masterFish_clean.csv")%>% 
  filter(!is.na(VHedges.G )) %>% filter(Authors..Year.!="Steele et al. (2002)")

############
#Data from papers (recall there are two unpublisehd papers) 
############
#number of publications used
total_papers<- masterData %>% summarise(n=length(unique(Authors..Year.)))
#number of publications used by Type of Study
papers_EXP_OBS<- masterData %>% group_by(Type.of.Study..Experimental_ObservatioNAl.) %>% summarise(n=length(unique(Authors..Year.)))
#number of publications that evaluate Richness
Richness_papers<-richness %>% summarise(n=length(unique(Authors..Year.)))
#number of publications that evaluate total abundance
totalabund_papers<- totalabund %>% summarise(n=length(unique(Authors..Year.)))
#number of publications that evaluate Abundance of specific sp
singleSpAbund_papers<- singleSpAbund %>%  summarise(n=length(unique(Authors..Year.)))




############
#Maps
############

makeDataSummary <- function(adf){
  ret <- adf %>%
    group_by(ECOREGION) %>%
    dplyr::summarise(Studies = length(unique(Authors..Year.)),
                     `Data Points` = length(Authors..Year.),
                     log_data_points=log(`Data Points`))
  #ret$Studies <- factor(ret$Studies, levels=1:max(ret$Studies))
  ret
  
}

##Fig 1. Updated Map using data points by ecoregion
jpeg("figures/Figure1data_point.jpg", width=1024, height=768)
makeMEOWmap(makeDataSummary(masterData), "Data Points",
            add.worldmap = TRUE, trans="log10")+ ylab("Latitude") +
  xlab("Longitude") + ggtitle("Data Points by Ecoregion")
dev.off()


############
# Analyses and Figures
############
#Data para Fig 3a
singleMultiTotal <- rma.mv(Hedges.G~ 0 + Single.or.Multi.Stipe, 
                           VHedges.G, 
                           random=list(~1|Authors..Year.),
                           data=totalabund, #%>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"),
                           control=list(optimizer="nlminb"))
summary(singleMultiTotal)

ctabSM <- coef(summary(singleMultiTotal))
ctabSM[["Kelp Morphology"]] <- c("Multi", "Single")

totalMorphSummary <- totalabund %>% 
  filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational") %>%
  group_by(Single.or.Multi.Stipe) %>%
  dplyr::summarise(n=sum(!is.na(VHedges.G))) %>%
  dplyr::rename(`Kelp Morphology` = Single.or.Multi.Stipe)

ctabSM <- merge(ctabSM, totalMorphSummary)

#Fig 3.b
jpeg("figures/morphology_abundance_Fig3b.jpg", width=1024, height=768)
ggplot(ctabSM, 
       aes(x=`Kelp Morphology`, y=estimate, ymin=ci.lb, ymax=ci.ub)) +
  geom_jitter(data=totalabund %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"), mapping=aes(x=Single.or.Multi.Stipe, 
                                                                                                                   y=Hedges.G, ymin=Hedges.G, ymax=Hedges.G), 
              alpha=0.5, position=position_jitter(width = .1), size=3)+
  geom_point(size=7, color="red", position=position_dodge(width=0.5)) +
  geom_linerange(position=position_dodge(width=0.5)) +
  geom_text( y=-2, mapping = aes(label=n), size=10,
             position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, lwd=1.4, lty=2)+
  ylim(c(-2, 2)) +
  ylab("Hedge's G")+ theme_bw()+ theme_classic()
dev.off()



richness2<- richness %>% filter (Authors..Year. != "Morton and Gladstone (2011)")

singleMultiRich <- rma.mv(Hedges.G~ 0+Single.or.Multi.Stipe, 
                          VHedges.G, 
                          random=list(~1|Authors..Year.),
                          data=richness, #%>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"),
                          control=list(optimizer="nlminb"))

summary(singleMultiRich)


richtabSM <- coef(summary(singleMultiRich))
richtabSM[["Kelp Morphology"]] <- c("Multi", "Single")

richlMorphSummary <- richness %>% 
  filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational") %>%
  group_by(Single.or.Multi.Stipe) %>%
  dplyr::summarise(n=sum(!is.na(VHedges.G))) %>%
  filter(Single.or.Multi.Stipe != "Both") %>%
  dplyr::rename(`Kelp Morphology` = Single.or.Multi.Stipe)

richtabSM <- merge(richtabSM, richlMorphSummary)

#Fig ### Not for the paper from previous version 
jpeg("figures/morphology_richness_Fig2b.jpg", width=1024, height=768)
ggplot(richtabSM, 
       aes(x=`Kelp Morphology`, y=estimate, ymin=ci.lb, ymax=ci.ub)) +
  geom_jitter(data=richness2, mapping=aes(x=Single.or.Multi.Stipe, 
                                          y=Hedges.G, ymin=Hedges.G, ymax=Hedges.G), 
              alpha=0.5, position=position_jitter(width = .1), size=3)+
  geom_point(size=7, color="red", position=position_dodge(width=0.5)) +
  geom_linerange(position=position_dodge(width=0.5)) +
  geom_text( y=-4, mapping = aes(label=n), size=10,
             position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0,  lwd=1.4, lty=2) +
  ylim(c(-4, 4)) +
  ylab("Hedge's G")+ theme_bw()+ theme_classic()
dev.off()


##data fig 3 ###Not for the paper (the inclusion of more studies resulted in variable effects of juveniles) 
singleHedges <- rma.mv(Hedges.G~ 0 + 
                         Type.of.Study..Experimental_ObservatioNAl.:Juvenile..Adult..Both, 
                       VHedges.G, method="REML",
                       random=list(~1|Authors..Year., ~ Authors..Year.|Fish.Species), 
                       struct="HCS", data=singleSpAbund,
                       verbose=TRUE, control=list(optimizer="nlminb"))


summary(singleHedges)

sms1 <- coef(summary(singleHedges))
sms1$trt <- rownames(sms1)
levels(factor(sms1$trt))
sms1 <- tidyr::separate(sms1, trt, sep=":", c("Type", "Age.Class"))

singleMorphology <- singleSpAbund %>% group_by(Type.of.Study..Experimental_ObservatioNAl.,
                                               Juvenile..Adult..Both) %>%
  dplyr::summarise(n = length(Hedges.G), meanHG=mean(Hedges.G)) %>%
  dplyr::rename(Type = Type.of.Study..Experimental_ObservatioNAl.,
                Age.Class = Juvenile..Adult..Both)

summary(singleMorphology)
levels(factor(sms1$Type))
sms1$Type <- gsub("Type.of.Study..Experimental_ObservatioNAl.Experimental", "Experimental", sms1$Type)
sms1$Type <- gsub("Type.of.Study..Experimental_ObservatioNAl.Observational", "Observational", sms1$Type)

levels(factor(sms1$Age.Class))
sms1$Age.Class <- gsub("Juvenile..Adult..BothAdult", "Adult", sms1$Age.Class)
sms1$Age.Class <- gsub("Juvenile..Adult..BothBoth", "Both", sms1$Age.Class)
sms1$Age.Class <- gsub("Juvenile..Adult..BothJuvenile", "Juvenile", sms1$Age.Class)
sms1 <- merge(sms1, singleMorphology)

#
jpeg("figures/morphology_single_exp.jpg", width=1024, height=768)
ggplot(sms1, aes(x=Type, y=estimate, ymin=ci.lb, ymax=ci.ub, shape=Age.Class)) +
  geom_point(size=7, color="red", position=position_dodge(width=0.5)) +
  geom_linerange(position=position_dodge(width=0.5)) +
  geom_text( y=-2, mapping = aes(label=n), size=10,
             position=position_dodge(width=0.5)) + 
  geom_hline(yintercept=0, lwd=1.4, lty=2) +
  ylim(c(-2, 2)) +
  ylab("Hedge's G") + xlab("") +
  scale_shape_discrete(guide=guide_legend(title="Age Class"))+
  geom_point(data=singleSpAbund, mapping=aes(x=Type.of.Study..Experimental_ObservatioNAl.,
                                             y=Hedges.G, ymin=Hedges.G, ymax=Hedges.G, shape=Juvenile..Adult..Both ), 
             alpha=0.1, position=position_dodge2(width =0.5), size=3)

dev.off()


#Data for Total Fish Abundance, definitely for the paper 
### Siddon, was revised and not included 
totalabund1<- totalabund %>% filter(Authors..Year.!="Siddon et al. (2008)" | Hedges.G > -1)
totalHedges <-  rma.mv(Hedges.G~ 0 + 
                         Type.of.Study..Experimental_ObservatioNAl., intercept=TRUE,
                       VHedges.G,struct="CS", verbose = TRUE,
                       random=list(~1|Authors..Year., ~Authors..Year.|Site),
                       data=totalabund, control=list(rel.tol=1e-8))

summary(totalHedges)
sms2 <- coef(summary(totalHedges))
sms2$Type <- rownames(sms2)
levels(factor(sms2$Type))
sms2$Type <- gsub("Type.of.Study..Experimental_ObservatioNAl.Experimental", "Experimental", sms2$Type)
sms2$Type <- gsub("Type.of.Study..Experimental_ObservatioNAl.Observational", "Observational", sms2$Type)

TotalAb <- totalabund %>% group_by(Type.of.Study..Experimental_ObservatioNAl.) %>%
  dplyr::summarise(n=sum(!is.na(VHedges.G))) %>%
  dplyr::rename(Type = Type.of.Study..Experimental_ObservatioNAl.)

sms2 <- merge(sms2, TotalAb)

#Fig 3a new 
jpeg("figures/TotalAb_TypeStudy_Fig3a.jpg", width=1024, height=768)
ggplot(sms2, aes(x=Type, y=estimate, ymin=ci.lb, ymax=ci.ub)) +
  geom_point(size=7, color="red", position=position_dodge(width=0.5)) +
  geom_linerange(position=position_dodge(width=0.5)) +
  geom_text( y=-2.9, mapping = aes(label=n), size=10,
             position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, lwd=1.4, lty=2) +
  ylim(c(-3, 3)) +
  ylab("Hedge's G") + xlab("") +
  geom_jitter(data=totalabund, mapping=aes(x=Type.of.Study..Experimental_ObservatioNAl., 
                                           y=Hedges.G, ymin=Hedges.G, ymax=Hedges.G), 
              alpha=0.5, position=position_jitter(width = .1), size=3)+ theme_bw()+ theme_classic()
dev.off()

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
sms3$Type <- gsub("Type.of.Study..Experimental_ObservatioNAl.Experimental", "Experimental", sms3$Type)
sms3$Type <- gsub("Type.of.Study..Experimental_ObservatioNAl.Observational", "Observational", sms3$Type)

TotalRichness <- richness %>% group_by(Type.of.Study..Experimental_ObservatioNAl.) %>%
  dplyr::summarise(n=sum(!is.na(VHedges.G))) %>%
  dplyr::rename(Type = Type.of.Study..Experimental_ObservatioNAl.)

sms3 <- merge(sms3, TotalRichness)

#Fig 4 Species Richness
jpeg("figures/TotalRichness_TypeStudy_Fig4.jpg", width=1024, height=768)
ggplot(sms3, aes(x=Type, y=estimate, ymin=ci.lb, ymax=ci.ub)) +
  geom_point(size=7, color="red", position=position_dodge(width=0.5)) +
  geom_linerange(position=position_dodge(width=0.5)) +
  geom_text( y=-2.9, mapping = aes(label=n), size=10,
             position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, lwd=1.4, lty=2) +
  ylim(c(-3, 3)) +
  ylab("Hedge's G") + xlab("") +
  geom_jitter(data=richness, mapping=aes(x=Type.of.Study..Experimental_ObservatioNAl., 
                                         y=Hedges.G, ymin=Hedges.G, ymax=Hedges.G), 
              alpha=0.5, position=position_jitter(width = .1), size=3)+ theme_bw()+ theme_classic()
dev.off()

###### Fig 5. Trophic Model
singleSpAbund <- read.csv("data/single_sp_abund_clean.csv") %>% 
  filter(!is.na(VHedges.G ))

singleSpAbund$Trophic.level<-as.character(singleSpAbund$Trophic.level)
singleSpAbund$Trophic.level<-gsub(",",".",singleSpAbund$Trophic.level)
singleSpAbund$Trophic.level<-as.numeric(singleSpAbund$Trophic.level)

singleSpAbund$Total.Lenght..cm.<-as.character(singleSpAbund$Total.Lenght..cm.)
singleSpAbund$Total.Lenght..cm.<-gsub(",",".",singleSpAbund$Total.Lenght..cm.)
singleSpAbund$Total.Lenght..cm.<-as.numeric(singleSpAbund$Total.Lenght..cm.)


##Experimental
model.trophic.exp <- rma.mv(Hedges.G~Trophic.level*Total.Lenght..cm.,
                            VHedges.G,
                            random=list(~1|Authors..Year., ~Authors..Year.|Fish.Species), 
                            struct="HCS", rho=0,
                            data=singleSpAbund %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental") %>% 
                              filter(Juvenile..Adult..Both != "Juvenile"),
                            control=list(optimizer="nlminb"))
summary(model.trophic.exp)



pred <- expand.grid(Trophic.level = 2:4, length = seq(4,80, .1))
resp <- sapply(1:nrow(pred), function(i) {
  predict(model.trophic.exp, newmods=cbind(pred[i,1],pred[i,2],pred[i,1]*pred[i,2]),
          level=95)
})

#Predicted Values for 'rma' Objects --> https://wviechtb.github.io/metafor/reference/predict.rma.html

pred$eff <- as.numeric(resp[1,])
pred$ci.lb <- as.numeric(resp[3,])
pred$ci.ub <- as.numeric(resp[4,])


#Herbivores, Mesopredators, Top predators
singleSpAbund$Trophic.level2<- round(singleSpAbund$Trophic.level)


singleSpAbund<-singleSpAbund %>% mutate (Trophic.group2= as.factor(case_when(Trophic.level2 =="2" ~ "Herbivores",
                                                                             Trophic.level2=="3" ~ "Mesopredators",
                                                                             Trophic.level2 =="4"  ~ "Top Predators")))

pred <- pred %>%  mutate (Trophic.group2= as.factor(case_when(Trophic.level =="2" ~ "Herbivores",
                                                              Trophic.level=="3" ~ "Mesopredators",
                                                              Trophic.level =="4"  ~ "Top Predators")))

###Figure 5 included in the paper
jpeg("figures/fish_trophic_model.jpg", width=1024, height=768)
ggplot(singleSpAbund %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental") %>% 
         filter(Juvenile..Adult..Both == "Adult"),
       aes(x=Total.Lenght..cm., y=Hedges.G, color=factor(Trophic.level))) + 
  geom_point(mapping=aes(size=1/VHedges.G)) +
  #geom_hline(yinterecept=0, lty=2, size=2) +
  geom_line(data=pred, mapping=aes(x=length, y=eff, color=factor(Trophic.level)),
            size=2) + 
  geom_ribbon(data=pred, 
              mapping=aes(x=length, y=eff, ymax=ci.ub, ymin=ci.lb),
              alpha=0.3, color=NA)+
  facet_wrap(~Trophic.group2)+
  xlab("Total Fish Length (cm)") + ylab("Hedge's G") +
  scale_color_discrete(guide=guide_legend(title="Trophic Level"))+
  scale_size_continuous(range=c(3,8), guide=guide_legend(title="Precision"))+
  theme_bw()+ theme_classic()+theme_linedraw()
dev.off()


#Ecoregion (like fig from Krumhansl) and Kelp Now Figure 2 


totalHedgesEco <-  rma.mv(Hedges.G~ ECOREGION,
                          VHedges.G, intercept=TRUE, method="REML",struct="CS", 
                          random=list(~1|Authors..Year., ~Authors..Year.|Site),
                          data=totalabund,
                          control=list(optimizer="nlminb"))
summary(totalHedgesEco)

totalObsResponse <- totalabund %>%  group_by(totalabund, Type.of.Study..Experimental_ObservatioNAl.,
                                             Clearing.Response..algae_barren_kelp.) %>%
  dplyr::summarise(n=sum(!is.na(VHedges.G))) %>%
  dplyr::rename(Type=Type.of.Study..Experimental_ObservatioNAl.)

###Figure 2, new 
smseco <- coef(summary(totalHedgesEco))
smseco$Type <- rownames(smseco)
levels(factor(smseco$Type))

jpeg("figures/TotalAbundance_Ecoregion.jpg", width=1024, height=768)
ggplot(smseco, aes(x=estimate, y=Type, xmin=ci.lb, xmax=ci.ub)) +
  geom_errorbarh(aes(xmax = ci.ub, xmin=ci.lb, height=0.2)) +
  geom_point(data=smseco, mapping=aes(y=Type, x=estimate), colour='red', size=3) +theme_bw()+ theme_classic()
  
geom_vline(xintercept=0)+xlim(c(-4,5)) 
dev.off()
  
#Correlation between sampling area and HedgesG 


###############
# Covariates profit #### AREA of SAMPLING 
###############


### Total covariates for experiments?
rma.mv(Hedges.G~ Clearing.Size..m.2., 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data=totalabund %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental"),
       control=list(optimizer="nlminb"))

rma.mv(Hedges.G~ sample.unit.area..m.2., 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data=totalabund %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="ObservatioNAl."),
       control=list(optimizer="nlminb"))
library(tidyr)
rma.mv(Hedges.G~ Single.or.Multi.Stipe, 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data=totalabund %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"),
       control=list(optimizer="nlminb"))


View(totalabund)
rma.mv(Hedges.G~ Epibiont.Load.Potential, 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data=totalabund %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"),
       control=list(optimizer="nlminb"))



### Richness covariates for experiments
#rma.mv(Hedges.G~ Clearing.Size..m.2., 
#VHedges.G, 
#random=list(~1|Authors..Year.),
#data=richness %>% filter(Type.of.Study..Experimental..Observational.=="Experimental"),
#control=list(optimizer="nlminb"))


### Richness covariates for experiments?

rma.mv(Hedges.G~ Total.Area.Sampled.per.Treatment..m.2..n.replicate.area., 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data=richness, #%>% filter(Type.of.Study..Experimental..Observational.=="Observational"),
       control=list(optimizer="nlminb"))

rma.mv(Hedges.G~ sample.unit.area..m.2., 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data=richness, #%>% filter(Type.of.Study..Experimental_ObservatioNAl.=="ObservatioNAl."),
       control=list(optimizer="nlminb"))


rma.mv(Hedges.G~ Single.or.Multi.Stipe+0, 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data=richness,#%>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental"),
       control=list(optimizer="nlminb"))


richness$Single.or.Multi.Stipe
rma.mv(Hedges.G~ Epibiont.Load.Potential, 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data=richness %>% filter(Type.of.Study..Experimental_ObservatioNAl.=="Experimental"),
       control=list(optimizer="nlminb"))


names(richness)

rma.mv(Hedges.G~ 0+Epibiont.Load.Potential.x, 
       VHedges.G, 
       random=list(~1|Authors..Year.),
       data=richness %>% 
         filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational"),
       control=list(optimizer="nlminb"))



jpeg("figures/Epibiontload_richness.jpg", width=1024, height=768)
ggplot(richtabSM, 
       aes(x=`Epibiont.Load.Potential.x`, y=estimate, ymin=ci.lb, ymax=ci.ub)) +
  geom_point(size=7, color="red", position=position_dodge(width=0.5)) +
  geom_linerange(position=position_dodge(width=0.5)) +
  geom_text( y=-4, mapping = aes(label=n), size=10,
             position=position_dodge(width=0.5)) +
  geom_hline(yintercept=0, lwd=1.4, lty=2) +
  ylim(c(-4, 4)) +
  ylab("Hedge's G")

names()
dev.off()

richEpiSummary <- richness %>% 
  filter(Type.of.Study..Experimental_ObservatioNAl.=="Observational") %>%
  group_by(Epibiont.Load.Potential.x) %>%
  dplyr::summarise(n=sum(!is.na(VHedges.G))) 

richEpiSummary





#### Figure S1 Electronic Supp
##Extra Number of studies / year - Accumulated 
#Publications by year and acumulated curve
papers_by_year<- masterData %>% select(Authors..Year.) %>% group_by(Authors..Year.) %>% summarise(n=length(unique(Authors..Year.)))
papers_by_year <-masterData %>% select(Authors..Year.) %>% separate()
##Extra figs...
#Publications by year and acumulated curve
papers_by_year<- masterData %>% select(Authors..Year.) %>% group_by(Authors..Year.) %>% summarise(n=length(unique(Authors..Year.)))
papers_by_year$Author<-papers_by_year$Authors..Year.
papers_by_year<-papers_by_year %>% separate(Author, sep = "[;()]", into=c("Author", "Year"))
papers_by_year<-papers_by_year %>% mutate(Year=case_when(Author=="O'Connor and Anderson 2010" ~"2010", TRUE~ Year)) %>% group_by(Year) %>% summarise(n=sum(n))
papers_by_year<-papers_by_year %>% select(Year, n) %>% arrange(Year) %>% mutate(Acumulated=cumsum(n))

scale=10 
ggplot(papers_by_year, aes(x=Year)) +
  geom_bar( aes(x=Year,y=n), stat="identity")  + 
  geom_line(aes(x=Year,y=Acumulated/scale),inherit.aes = FALSE, size=1,color = "#FF9900") +
  geom_point(aes(x=Year,y=Acumulated/scale),color = "#FF9900") +
  scale_y_continuous(
    # Features of the first axis
    name = "Publications",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*scale, name="Acummulated")) + 
  scale_x_continuous(breaks=seq(1984,2019,1))+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))+
  theme_classic() 



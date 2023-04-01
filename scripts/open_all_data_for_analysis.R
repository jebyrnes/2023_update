
singleSpAbund <- read.csv("data/single_sp_abund_clean.csv") %>% 
  filter(!is.na(VHedges.G )) 

totalabund<-read.csv("data/total_abund_clean.csv") %>% 
  filter(!is.na(VHedges.G )) %>% filter(Authors..Year.!="Steele et al. (2002)")

richness<-read.csv("data/sp_rich_clean.csv") %>% 
  filter(!is.na(VHedges.G )) 

# biomass<-read.csv("data/biomass_clean.csv") %>% 
#   filter(!is.na(VHedges.G )) 

masterData <- read.csv("data/masterFish_clean.csv")%>% 
  filter(!is.na(VHedges.G )) %>% filter(Authors..Year.!="Steele et al. (2002)")

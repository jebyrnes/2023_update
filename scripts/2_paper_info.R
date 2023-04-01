#' =========================
#' Information on Papers
#' =========================

library(dplyr)
library(meowR)
library(ggplot2)
library(sf)
library(meowR)
library(rnaturalearth)

theme_set(theme_classic(base_size = 25) )


############
#Data from papers (recall there are two unpublisehd papers) 
############


#number of publications used
total_papers <-
  masterData %>% summarise(n = length(unique(Authors..Year.)))

#number of publications used by Type of Study
papers_EXP_OBS <-
  masterData %>% group_by(Type.of.Study..Experimental_ObservatioNAl.) %>% 
  summarise(n = length(unique(Authors..Year.)))

#number of publications that evaluate Richness
Richness_papers <-
  richness %>% summarise(n = length(unique(Authors..Year.)))

#number of publications that evaluate total abundance
totalabund_papers <-
  totalabund %>% summarise(n = length(unique(Authors..Year.)))

#number of publications that evaluate Abundance of specific sp
singleSpAbund_papers <-
  singleSpAbund %>%  summarise(n = length(unique(Authors..Year.)))

#number of publications that evaluate biomass
biomass_papers <-
  biomass %>%  summarise(n = length(unique(Authors..Year.)))




#### Figure S1 Electronic Supp
##Extra Number of studies / year - Accumulated
#Publications by year and acumulated curve

papers_by_year <- masterData |>
  select(Authors..Year.) |>
  mutate(Year = str_extract(Authors..Year., "[0-9]{4}")) |>
  group_by(Year) |>
  dplyr::summarize(n = length(unique(Authors..Year.))) |>
  ungroup() |>
  mutate(Year = as.numeric(Year)) |>
  arrange(Year) |>
  mutate(Accumulated = cumsum(n)) 


# save out
jpeg("figures/Sup_1_paper_accumulation.jpg", width=1024, height=768)

ggplot(data = papers_by_year,
       aes(x = Year, y = n)) +
  geom_bar(stat = "identity") +
  geom_line(mapping = aes(x=Year, y=Accumulated/10),
            group = 1,
            linewidth = 1,
            color = "#FF9900") +
  geom_point(mapping = aes(x = Year,y = Accumulated/10),
             color = "#FF9900") +
  scale_x_continuous(breaks = seq(1984, 2019, 1))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_y_continuous(
    # Features of the first axis
    name = "Publications",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*10, name="Acummulated")) 
  
  
dev.off()

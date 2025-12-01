#############################################################
#### A code to collate data and perform calculations for ####
#### a regional carbon budget for kelp and seagrass      ####
#### ecosystems in Nova Scotia, Canada                   ####
#### by Kira Krumhansl, Fisheries and Oceans Canada      ####
#### Kira.Krumhansl@dfo-mpo.gc.ca                        ####
#############################################################

#load necessary libraries

library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)



#### Carbon Stock Estimates ####

# Load and reformat Kelp Biomass and Density Data


biomass_density_2023<-read.csv("Kelp_data_2021_2024/kelp_quadrat_Sober_Island_Broad_Cove.csv")
biomass_density_2023<-separate(biomass_density_2023, Date, c("Day", "Month", "Year"), sep="/")
biomass_density_2023$Month<-as.numeric(biomass_density_2023$Month)
biomass_density_2023<-biomass_density_2023 %>% 
  select(Month, Year, Site, Quadrat_Number, Number_Saccharina_latissima, Number_Laminaria_digitata) %>% 
  rename("Laminaria digitata"=Number_Laminaria_digitata, "Saccharina latissima"=Number_Saccharina_latissima)
biomass_density_2023<-biomass_density_2023 %>% 
  gather(Species, Density_no_0.25m2, 5:6)
biomass_density_2023$Biomass_0.25m2<-NA
biomass_density_2023$Density_no_1m2<-biomass_density_2023$Density_no_0.25m2*4
biomass_density_2023$Biomass_kg_dw_1m2<-NA

biomass_density_2021_2022<-read.csv("Kelp_data_2021_2024/L-bag-data.csv")
biomass_density_2021_2022<-separate(biomass_density_2021_2022, date, c("Month", "Day", "Year"), sep="/")
biomass_density_2021_2022$Month<-as.numeric(biomass_density_2021_2022$Month)
biomass_density_2021_2022<-biomass_density_2021_2022 %>% 
  select(Month, Year, site, quadrat, species, num_0_25m2, biomass_kg_dry_wt) %>% 
  rename(Site=site, Quadrat_Number=quadrat, Species=species, Density_no_0.25m2=num_0_25m2, Biomass_0.25m2=biomass_kg_dry_wt)
biomass_density_2021_2022$Density_no_1m2<-biomass_density_2021_2022$Density_no_0.25m2*4
biomass_density_2021_2022$Biomass_kg_dw_1m2<-biomass_density_2021_2022$Biomass_0.25m2*4

biomass_density_contemp<-rbind(biomass_density_2021_2022, biomass_density_2023)

biomass_density_contemp <- biomass_density_contemp %>% 
  filter(Species==c("Laminaria digitata")| 
           Species==c("Saccharina latissima")) 

biomass_density_contemp$Season[biomass_density_contemp$Month == 8]<- "Summer"
biomass_density_contemp$Season[biomass_density_contemp$Month == 11]<- "Fall"
biomass_density_contemp$Season[biomass_density_contemp$Month == 9]<- "Summer"
biomass_density_contemp$Season[biomass_density_contemp$Month == 10]<- "Fall"
biomass_density_contemp$Season[biomass_density_contemp$Month == 6]<- "Spring"
biomass_density_contemp$Season[biomass_density_contemp$Month == 7]<- "Summer"
biomass_density_contemp$Season[biomass_density_contemp$Month == 12]<- "Winter"
biomass_density_contemp$Season[biomass_density_contemp$Month == 5]<- "Spring"

biomass_density_contemp$Biomass_per_ind_kg_dw<-biomass_density_contemp$Biomass_kg_dw_1m2/biomass_density_contemp$Density_no_1m2

biomass_per_ind<-biomass_density_contemp %>% 
  group_by(Species) %>% 
  summarise(mean_biomass_per_ind_kg_dw=mean(Biomass_per_ind_kg_dw, na.rm = TRUE), sd_biomass_per_ind_kg_dw=sd(Biomass_per_ind_kg_dw, na.rm = TRUE))

biomass_density_contemp<-left_join(biomass_density_contemp, biomass_per_ind)
biomass_density_contemp_u<-biomass_density_contemp

#some seasons are missing biomass data all together so an average of only summer and fall biomass will 
#underestimate per unit area biomass. So, performed an interpolation to predict biomass from density 
#in winter and summer based on per individual biomass for seasons when we have both multiplied by 
#density

biomass_density_contemp$Biomass_kg_dw_1m2[is.na(biomass_density_contemp$Biomass_kg_dw_1m2)] <- 
  biomass_density_contemp$Density_no_1m2[is.na(biomass_density_contemp$Biomass_kg_dw_1m2)] * 
  biomass_density_contemp$mean_biomass_per_ind_kg_dw[is.na(biomass_density_contemp$Biomass_kg_dw_1m2)]

biomass_density_contemp_month<-biomass_density_contemp %>% 
  group_by(Species, Month) %>% 
  summarize(mean_Density_no_1m2=mean(Density_no_1m2, na.rm=TRUE), 
            sd_Density_no_1m2=sd(Density_no_1m2, na.rm=TRUE),
            mean_Biomass_kg_dw_1m2=mean(Biomass_kg_dw_1m2, na.rm=TRUE), 
            sd_Biomass_kg_dw_1m2=sd(Biomass_kg_dw_1m2, na.rm=TRUE))

biomass_density_contemp_season<-biomass_density_contemp %>% 
  group_by(Species, Season) %>% 
  summarize(mean_Density_no_1m2=mean(Density_no_1m2, na.rm=TRUE), 
            sd_Density_no_1m2=sd(Density_no_1m2, na.rm=TRUE),
            mean_Biomass_kg_dw_1m2=mean(Biomass_kg_dw_1m2, na.rm=TRUE), 
            sd_Biomass_kg_dw_1m2=sd(Biomass_kg_dw_1m2, na.rm=TRUE))


biomass_density_contemp_annual <- biomass_density_contemp%>% 
  group_by(Species) %>% 
  summarise(mean_Density_no_1m2=mean(Density_no_1m2, na.rm=TRUE), 
            sd_Density_no_1m2=sd(Density_no_1m2, na.rm=TRUE),
            mean_Biomass_kg_dw_1m2=mean(Biomass_kg_dw_1m2, na.rm=TRUE), 
            sd_Biomass_kg_dw_1m2=sd(Biomass_kg_dw_1m2, na.rm=TRUE))


kelp_area<-read.csv("Kelp_data_2021_2024/kelp_area_Thomas.csv") #Krumhansl et al. in review model output area kelp scotian shelf bioregion

standing_stock_kelp_annual<-left_join(biomass_density_contemp_annual, kelp_area)
standing_stock_kelp_annual$Area_bioregion_m2<-standing_stock_kelp_annual$Kelp_area_bioregion_km2*1000000
standing_stock_kelp_annual$Standing_Stock_Tg_dw_bioregion<-(standing_stock_kelp_annual$mean_Biomass_kg_dw_1m2*standing_stock_kelp_annual$Area_bioregion_m2)/1000000000
standing_stock_kelp_annual$sd_Standing_Stock_Tg_dw_bioregion<-(standing_stock_kelp_annual$sd_Biomass_kg_dw_1m2*standing_stock_kelp_annual$Area_bioregion_m2)/1000000000

standing_stock_kelp_annual$Standing_Stock_Tg_C_bioregion<-standing_stock_kelp_annual$Standing_Stock_Tg_dw_bioregion*0.3
standing_stock_kelp_annual$sd_Standing_Stock_Tg_C_bioregion<-standing_stock_kelp_annual$sd_Standing_Stock_Tg_dw_bioregion*0.3

standing_stock_kelp_annual$Standing_Stock_kg_C_m2<-standing_stock_kelp_annual$mean_Biomass_kg_dw_1m2*0.3
standing_stock_kelp_annual$sd_Standing_Stock_kg_C_m2<-standing_stock_kelp_annual$sd_Biomass_kg_dw_1m2*0.3


standing_stock_kelp_annual_c<-select(standing_stock_kelp_annual, Species, Area_bioregion_m2, Standing_Stock_Tg_dw_bioregion, sd_Standing_Stock_Tg_dw_bioregion, Standing_Stock_Tg_C_bioregion, sd_Standing_Stock_Tg_C_bioregion, Standing_Stock_kg_C_m2, sd_Standing_Stock_kg_C_m2)


#### Growth, erosion, and dislodgement ####
#2011
growth_erosion_2011<-read.csv("Krumhansl 2012 data/Kelp growth and erosion.csv") # Data from Krumhansl & Scheibling 2011

growth_erosion_2011<-separate(growth_erosion_2011, Date.Tagged, c("Month", "Day", "Year"), sep=" ")
growth_erosion_2011$Month[growth_erosion_2011$Month=="July"]<-7
growth_erosion_2011$Month[growth_erosion_2011$Month=="May"]<-5
growth_erosion_2011$Month[growth_erosion_2011$Month=="February"]<-2
growth_erosion_2011$Month[growth_erosion_2011$Month=="June"]<-6
growth_erosion_2011$Month[growth_erosion_2011$Month=="September"]<-9
growth_erosion_2011$Month[growth_erosion_2011$Month=="November"]<-11
growth_erosion_2011$Month<-as.numeric(growth_erosion_2011$Month)

growth_erosion_2011$Season<-NA
growth_erosion_2011$Season[growth_erosion_2011$Month == 7]<- "Summer"
growth_erosion_2011$Season[growth_erosion_2011$Month == 5]<- "Spring"
growth_erosion_2011$Season[growth_erosion_2011$Month == 2]<- "Winter"
growth_erosion_2011$Season[growth_erosion_2011$Month == 6]<- "Spring"
growth_erosion_2011$Season[growth_erosion_2011$Month == 9]<- "Fall"
growth_erosion_2011$Season[growth_erosion_2011$Month == 11]<- "Fall"

dislodgement_season_species_2011<-growth_erosion_2011 %>% 
  group_by(Month, Year, Site, Species, Season) %>% 
  summarise(Num_found=n())
dislodgement_season_species_2011$prop_dislodged_uncorrected<-1-(dislodgement_season_species_2011$Num_found/20)
dislodgement_season_species_2011$prop_dislodged<-dislodgement_season_species_2011$prop_dislodged_uncorrected-(dislodgement_season_species_2011$prop_dislodged_uncorrected*0.25) #assuming a 25% loss rate associated with not being able to find tagged plants


dislodgement_season_species_2011<-dislodgement_season_species_2011[dislodgement_season_species_2011$prop_dislodged >= 0, ] 

dislodgement_season_species_2011<-dislodgement_season_species_2011 %>% 
  mutate(prop_dislodged=ifelse(Species=="Saccharina latissima",
                               prop_dislodged*0.77,
                               prop_dislodged*0.50)) #Added a correction for the amount of mortality from a given year's production. This was done so that the mortality is represened as an annual rate, to better relate to NPP. Estimates are from Chapman 1984 and are very similar to the present study.


#2023-2024
growth_erosion_2023<-read.csv("Kelp_data_2021_2024/kelp_growth_field_revised.csv")
growth_erosion_2023<-select(growth_erosion_2023, -Date_tag, -Blade_length_initial_cm_2, -Note, -Hole.punch, -Recovered)

growth_erosion_2023<-separate(growth_erosion_2023, Date_punched, c("Month", "Day", "Year"), sep="/")
growth_erosion_2023$Month<-as.numeric(growth_erosion_2023$Month)

growth_erosion_2023$Season<-NA
growth_erosion_2023$Season[growth_erosion_2023$Month == 8]<- "Summer"
growth_erosion_2023$Season[growth_erosion_2023$Month == 5]<- "Spring"
growth_erosion_2023$Season[growth_erosion_2023$Month == 12]<- "Winter"
growth_erosion_2023$Season[growth_erosion_2023$Month == 6]<- "Spring"
growth_erosion_2023$Season[growth_erosion_2023$Month == 9]<- "Fall"
growth_erosion_2023$Season[growth_erosion_2023$Month == 10]<- "Fall"

punched_kelp_dislodgement<-growth_erosion_2023

growth_erosion_2023 <- growth_erosion_2023[!is.na(growth_erosion_2023$Days_measured), ] 

growth_erosion_2023$growth_cm<-growth_erosion_2023$Hole_length_final_cm-10
growth_erosion_2023$dw_cm<-growth_erosion_2023$Kelp_DW_g/growth_erosion_2023$growth_cm
growth_erosion_2023$growth_g_d<-growth_erosion_2023$Kelp_DW_g/growth_erosion_2023$Days_measured

growth_erosion_2023$growth_g_d[growth_erosion_2023$growth_cm == 0]<- 0 #correcting inf values generated by dividing 0 by a number

growth_erosion_2023$erosion_cm<-(growth_erosion_2023$Blade_length_initial_cm+growth_erosion_2023$growth_cm)-growth_erosion_2023$Blade_length_final_cm
growth_erosion_2023$erosion_g_d<-(growth_erosion_2023$erosion_cm*growth_erosion_2023$dw_cm)/growth_erosion_2023$Days_measured

growth_erosion_2023_ec<-growth_erosion_2023 %>% #removing some wonky values, assuming incorrect blades/holes were measured
  filter(erosion_g_d >= 0) %>% 
  filter(erosion_g_d<6)
  
dislodgement_punched_season_species_2023 <- punched_kelp_dislodgement %>%
  group_by(Month, Year, Site, Species, Season) %>%
  summarise(
    Num_punched = n(),
    Num_recovered = sum(!is.na(Days_measured))
  )
dislodgement_punched_season_species_2023$Species[dislodgement_punched_season_species_2023$Species == "Laminaria_digitata"]<- "Laminaria digitata"
dislodgement_punched_season_species_2023$Species[dislodgement_punched_season_species_2023$Species == "Saccharina_latissima"]<- "Saccharina latissima"


dislodgement_2023<-read.csv("Kelp_data_2021_2024/Dislodgment_kelp_revised.csv")
dislodgement_2023<-separate(dislodgement_2023, Date.Tagged, c("Month_Tagged", "Day_Tagged", "Year_Tagged"), sep="/")
dislodgement_2023$Month_Tagged<-as.numeric(dislodgement_2023$Month_Tagged)

dislodgement_2023$Season<-NA
dislodgement_2023$Season[dislodgement_2023$Month_Tagged == 8]<- "Summer"
dislodgement_2023$Season[dislodgement_2023$Month_Tagged == 7]<- "Summer"
dislodgement_2023$Season[dislodgement_2023$Month_Tagged == 5]<- "Spring"
dislodgement_2023$Season[dislodgement_2023$Month_Tagged == 12]<- "Winter"
dislodgement_2023$Season[dislodgement_2023$Month_Tagged == 6]<- "Spring"
dislodgement_2023$Season[dislodgement_2023$Month_Tagged == 9]<- "Fall"
dislodgement_2023$Season[dislodgement_2023$Month_Tagged == 10]<- "Fall"

dislodgement_2023<-separate(dislodgement_2023, Date.Collected, c("Month_Collected", "Day_Collected", "Year_Collected"), sep="/")

dislodgement_season_species_2023 <- dislodgement_2023 %>%
  group_by(Month_Tagged, Year_Tagged, Site, Species, Season) %>%
  summarise(
    Num_punched = n(),
    Num_recovered = sum(!is.na(Day_Collected))
  ) %>% 
  rename(Month=Month_Tagged, Year=Year_Tagged)

dislodgement_season_species_2023$Species[dislodgement_season_species_2023$Species == "L_digitata"]<- "Laminaria digitata"
dislodgement_season_species_2023$Species[dislodgement_season_species_2023$Species == "S_latissima"]<- "Saccharina latissima"

dislodgement_season_species_2023_all<-rbind(dislodgement_season_species_2023, dislodgement_punched_season_species_2023)

dislodgement_season_species_2023_all_c<- dislodgement_season_species_2023_all %>% 
  group_by(Month, Year, Site, Species, Season) %>% 
  summarise(Num_punched=sum(Num_punched), Num_found=sum(Num_recovered))

dislodgement_season_species_2023_all_c$prop_dislodged_uncorrected<-1-(dislodgement_season_species_2023_all_c$Num_found/dislodgement_season_species_2023_all_c$Num_punched)
dislodgement_season_species_2023_all_c$prop_dislodged<-dislodgement_season_species_2023_all_c$prop_dislodged_uncorrected-(dislodgement_season_species_2023_all_c$prop_dislodged_uncorrected*0.25)

dislodgement_season_species_2023_all_c<-dislodgement_season_species_2023_all_c %>% 
  mutate(prop_dislodged=ifelse(Species=="Saccharina latissima",
                               prop_dislodged*0.77,
                               prop_dislodged*0.50)) #Added a correction for the amount of mortality from a given year's production. This was done so that the mortality is represened as an annual rate, to better relate to NPP. Estimates are from Chapman 1984 and are very similar to the present study.


#combining growth, erosion, and dislodgement for 2011 and 2023

growth_erosion_2011_c<-growth_erosion_2011 %>% 
  select(Site, Species, Season, Year, Growth.Rate..g.dw.d.1., Erosion.Rate..g.DW.d.1.) 

growth_erosion_2023_c<-growth_erosion_2023_ec %>% 
  select(Site, Species, Season, Year, growth_g_d, erosion_g_d) %>% 
  rename(Growth.Rate..g.dw.d.1.= growth_g_d, Erosion.Rate..g.DW.d.1.= erosion_g_d) 

growth_erosion_2011_2023<-rbind(growth_erosion_2011_c, growth_erosion_2023_c)

growth_erosion_2011_2023$Species[growth_erosion_2011_2023$Species=="Laminaria_digitata"]<-"Laminaria digitata"
growth_erosion_2011_2023$Species[growth_erosion_2011_2023$Species=="Saccharina_latissima"]<-"Saccharina latissima"

growth_erosion_season_species_2011_2023<-growth_erosion_2011_2023 %>% 
  group_by(Species, Season) %>% 
  summarise(mean_growth_g_dw_d=mean(Growth.Rate..g.dw.d.1., na.rm=TRUE), 
            sd_growth_g_dw_d=sd(Growth.Rate..g.dw.d.1., na.rm=TRUE),
            mean_erosion_g_dw_d=mean(Erosion.Rate..g.DW.d.1., na.rm=TRUE),
            sd_erosion_g_dw_d=sd(Erosion.Rate..g.DW.d.1., na.rm=TRUE))


dislodgement_2011_2023<-rbind(dislodgement_season_species_2023_all_c, dislodgement_season_species_2011)

dislodgement_season_species_2011_2023<-dislodgement_2011_2023 %>% 
  group_by(Species, Season) %>% 
  summarise(mean_prop_dislodged=mean(prop_dislodged, na.rm=TRUE), 
            sd_prop_dislodged=sd(prop_dislodged, na.rm=TRUE))


#### Calculating NPP ####

productivity_contemp<-left_join(biomass_density_contemp_season, growth_erosion_season_species_2011_2023)
productivity_contemp<-left_join(productivity_contemp, dislodgement_season_species_2011_2023)

productivity_contemp$NPP_g_dw_m2_d_tissue<-productivity_contemp$mean_Density_no_1m2*productivity_contemp$mean_growth_g_dw_d
productivity_contemp$NPP_g_dw_m2_d<-productivity_contemp$NPP_g_dw_m2_d_tissue*(1/0.45) #hatcher compared hole punch growth to chamber productivity and found that 45% of net carbon assimilation goes into new growth. So, multiplying by 1/0.45 to scale up to total NPP

productivity_contemp$sd_NPP_g_dw_m2_d_tissue<-sqrt((productivity_contemp$sd_growth_g_dw_d/productivity_contemp$mean_growth_g_dw_d)^2 +
                                          (productivity_contemp$sd_Density_no_1m2/productivity_contemp$mean_Density_no_1m2)^2)*productivity_contemp$NPP_g_dw_m2_d_tissue #https://www.geol.lsu.edu/jlorenzo/geophysics/uncertainties/Uncertaintiespart2.html#muldiv
productivity_contemp$sd_NPP_g_dw_m2_d<-productivity_contemp$sd_NPP_g_dw_m2_d_tissue*(1/0.45)

productivity_contemp$NPP_g_dw_m2_season<-productivity_contemp$NPP_g_dw_m2_d*91
productivity_contemp$sd_NPP_g_dw_m2_season<-productivity_contemp$sd_NPP_g_dw_m2_d*91
productivity_contemp$NPP_gC_m2_season<-productivity_contemp$NPP_g_dw_m2_season*0.3
productivity_contemp$sd_NPP_gC_m2_season<-productivity_contemp$sd_NPP_g_dw_m2_season*0.3

productivity_contemp<-left_join(productivity_contemp, kelp_area)
productivity_contemp$Kelp_area_bioregion_m2<-productivity_contemp$Kelp_area_bioregion_km2*1000000

productivity_contemp$NPP_gC_region_season<-productivity_contemp$NPP_gC_m2_season*productivity_contemp$Kelp_area_bioregion_m2 
productivity_contemp$sd_NPP_gC_region_season<-productivity_contemp$sd_NPP_gC_m2_season*productivity_contemp$Kelp_area_bioregion_m2


#### DOC Production and decay #### 

DOC_Expt<-read.csv("C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Carbon Budget/Summary_DOC_season_annual.csv")

DOC_Expt$Species[DOC_Expt$Species == "L_digitata"]<- "Laminaria digitata"
DOC_Expt$Species[DOC_Expt$Species == "S_latissima"]<- "Saccharina latissima"
DOC_Expt$Species[DOC_Expt$Species == "Z_marina"]<- "Zostera marina"

DOC_Total<-select(DOC_Expt, Season, Species, total_season_DOC_mg_evol_per_ind, sd_total_season_DOC_mg_evol_per_ind)
productivity_contemp<-left_join(productivity_contemp, DOC_Total)

productivity_contemp$DOC_prod_mgC_m2_season<-productivity_contemp$mean_Density_no_1m2*productivity_contemp$total_season_DOC_mg_evol_per_ind
productivity_contemp$sd_DOC_prod_mgC_m2_season<-sqrt((productivity_contemp$sd_Density_no_1m2/productivity_contemp$mean_Density_no_1m2)^2 +
                                                       (productivity_contemp$sd_total_season_DOC_mg_evol_per_ind/
                                                          productivity_contemp$total_season_DOC_mg_evol_per_ind)^2)*productivity_contemp$DOC_prod_mgC_m2_season

productivity_contemp$RDOC_prod_mgC_m2_season<-productivity_contemp$DOC_prod_mgC_m2_season*0.375
productivity_contemp$sd_RDOC_prod_mgC_m2_season<-productivity_contemp$sd_DOC_prod_mgC_m2_season*0.375

productivity_contemp$DOC_prod_mgC_region_season<-productivity_contemp$DOC_prod_mgC_m2_season*productivity_contemp$Kelp_area_bioregion_m2
productivity_contemp$sd_DOC_prod_mgC_region_season<-productivity_contemp$sd_DOC_prod_mgC_m2_season*productivity_contemp$Kelp_area_bioregion_m2

productivity_contemp$DOC_prod_TgC_region_season<-productivity_contemp$DOC_prod_mgC_region_season/1000000000000000
productivity_contemp$sd_DOC_prod_TgC_region_season<-productivity_contemp$sd_DOC_prod_mgC_region_season/1000000000000000

productivity_contemp$RDOC_prod_TgC_region_season<-productivity_contemp$DOC_prod_TgC_region_season*0.375 #percent refractory after 150 d from Gao et al.
productivity_contemp$sd_RDOC_prod_TgC_region_season<-productivity_contemp$sd_DOC_prod_TgC_region_season*0.375

productivity_contemp$BDOC_prod_TgC_region_season<-productivity_contemp$DOC_prod_TgC_region_season-productivity_contemp$RDOC_prod_TgC_region_season
productivity_contemp$sd_BDOC_prod_TgC_region_season<-sqrt(productivity_contemp$sd_DOC_prod_TgC_region_season^2+
                                                            productivity_contemp$sd_RDOC_prod_TgC_region_season^2)

productivity_contemp_total<-productivity_contemp %>% 
  group_by(Species) %>% 
  summarise(NPP_gC_region_season=sum(NPP_gC_region_season),
            sd_NPP_gC_region_season=sqrt(sum(sd_NPP_gC_region_season^2)),
            DOC_prod_TgC_region_season=sum(DOC_prod_TgC_region_season),
            sd_DOC_prod_TgC_region_season=sqrt(sum(sd_DOC_prod_TgC_region_season^2)),
            NPP_gC_m2_season=sum(NPP_gC_m2_season),
            sd_NPP_gC_m2_season=sqrt(sum(sd_NPP_gC_m2_season^2)),
            DOC_prod_mgC_m2_season=sum(DOC_prod_mgC_m2_season),
            sd_DOC_prod_mgC_m2_season=sqrt(sum(sd_DOC_prod_mgC_m2_season^2)),
            RDOC_prod_mgC_m2_season=sum(RDOC_prod_mgC_m2_season),
            sd_RDOC_prod_mgC_m2_season=sqrt(sum(sd_RDOC_prod_mgC_m2_season^2)))

productivity_contemp_total$Season<-"Annual"

productivity_contemp_summary<-rbind(productivity_contemp, productivity_contemp_total)

productivity_contemp_summary$NPP_TgC_region_season<-productivity_contemp_summary$NPP_gC_region_season/1000000000000
productivity_contemp_summary$sd_NPP_TgC_region_season<-productivity_contemp_summary$sd_NPP_gC_region_season/1000000000000

BDOC_decay<-select(productivity_contemp, Species, Season, BDOC_prod_TgC_region_season)
RDOC_decay<-select(productivity_contemp, Species, Season, RDOC_prod_TgC_region_season, sd_RDOC_prod_TgC_region_season)

decay_function = function(BDOC_prod_TgC_region_season){
  t=1:91
  BDOC_t=BDOC_prod_TgC_region_season*exp(-0.06*t)
}

DOC_decay<-t(mapply(decay_function,  BDOC_decay$BDOC_prod_TgC_region_season))

colnames(DOC_decay)<-paste("BDOC TgC", seq(1:91), " d")
DOC_decay<-as.data.frame(DOC_decay)

BDOC_decay<-bind_cols(BDOC_decay, DOC_decay)
BDOC_decay<-select(BDOC_decay, -BDOC_prod_TgC_region_season)
BDOC_decay<-BDOC_decay %>% 
  gather("code", "BDOC_TgC_region_species", 3:92) %>% 
  separate(code, c("a", "b", "Time", "c", "units"), sep=" ") %>% 
  select(-a, -b, -c, -units)

BDOC_decay$Time<-as.numeric(BDOC_decay$Time)

#### Seagrass ####

#### NPP, biomass, and DOC #### 

seagrass_productivity_contemp<-read.csv("C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Carbon Budget/zostera_NPP_erosion_revisedJan2025.csv")

seagrass_area_bioregion_m2<-239780000 #O'Brien et al. 2022 model output area eelgrass scotian shelf bioregion

seagrass_productivity_contemp$NPP_gC_m2_season_tissue<-seagrass_productivity_contemp$NPP_gC_m2_d*91
seagrass_productivity_contemp$NPP_gC_m2_season<-seagrass_productivity_contemp$NPP_gC_m2_season_tissue*(1/0.61) #scaling based on field:lab NPP from O2 measured by Melisa in Spring and Summer

seagrass_productivity_contemp$sd_NPP_gC_m2_season_tissue<-seagrass_productivity_contemp$sd_NPP_gC_m2_d*91
seagrass_productivity_contemp$sd_NPP_gC_m2_season<-seagrass_productivity_contemp$sd_NPP_gC_m2_season_tissue*(1/0.61)

seagrass_productivity_contemp$NPP_gC_region_season<-seagrass_productivity_contemp$NPP_gC_m2_season*seagrass_area_bioregion_m2
seagrass_productivity_contemp$sd_NPP_gC_region_season<-seagrass_productivity_contemp$sd_NPP_gC_m2_season*seagrass_area_bioregion_m2

seagrass_productivity_contemp<-left_join(seagrass_productivity_contemp, DOC_Total)

seagrass_density<-read.csv("C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Carbon Budget/Wong_ecosphere_data/Ecosphere_Density_Leaf_Area_Index.csv")

seagrass_density <- subset(seagrass_density, Site_Location %in% c("PH", "TH Shallow"))

seagrass_density<-separate(seagrass_density, Date, into = c("Month", "Day", "Year"), sep="/")
seagrass_density$Season[seagrass_density$Month == 4]<- "Spring"
seagrass_density$Season[seagrass_density$Month == 5]<- "Spring"
seagrass_density$Season[seagrass_density$Month == 6]<- "Spring"
seagrass_density$Season[seagrass_density$Month == 7]<- "Summer"
seagrass_density$Season[seagrass_density$Month == 8]<- "Summer"
seagrass_density$Season[seagrass_density$Month == 9]<- "Summer"
seagrass_density$Season[seagrass_density$Month == 10]<- "Fall"
seagrass_density$Season[seagrass_density$Month == 11]<- "Fall"
seagrass_density$Season[seagrass_density$Month == 12]<- "Winter"

seagrass_density_summary<-seagrass_density %>% 
  group_by(Season) %>% 
  summarize(mean_Density_no_1m2=mean(Den_total_m2), sd_Density_no_1m2=sd(Den_total_m2))

seagrass_productivity_contemp<-left_join(seagrass_productivity_contemp, seagrass_density_summary)

seagrass_productivity_contemp$DOC_prod_mgC_m2_season<-seagrass_productivity_contemp$mean_Density_no_1m2*seagrass_productivity_contemp$total_season_DOC_mg_evol_per_ind
seagrass_productivity_contemp$sd_DOC_prod_mgC_m2_season<-sqrt((seagrass_productivity_contemp$sd_Density_no_1m2/seagrass_productivity_contemp$mean_Density_no_1m2)^2 +
                                                       (seagrass_productivity_contemp$sd_total_season_DOC_mg_evol_per_ind/
                                                          seagrass_productivity_contemp$total_season_DOC_mg_evol_per_ind)^2)*seagrass_productivity_contemp$DOC_prod_mgC_m2_season

seagrass_productivity_contemp$RDOC_prod_mgC_m2_season<-seagrass_productivity_contemp$DOC_prod_mgC_m2_season*0.1#percent refractory Pellikaan 1988
seagrass_productivity_contemp$sd_RDOC_prod_mgC_m2_season<-seagrass_productivity_contemp$sd_DOC_prod_mgC_m2_season*0.1

seagrass_productivity_contemp$DOC_prod_mgC_region_season<-seagrass_productivity_contemp$DOC_prod_mgC_m2_season*seagrass_area_bioregion_m2
seagrass_productivity_contemp$sd_DOC_prod_mgC_region_season<-seagrass_productivity_contemp$sd_DOC_prod_mgC_m2_season*seagrass_area_bioregion_m2

seagrass_productivity_contemp$DOC_prod_TgC_region_season<-seagrass_productivity_contemp$DOC_prod_mgC_region_season/1000000000000000
seagrass_productivity_contemp$sd_DOC_prod_TgC_region_season<-seagrass_productivity_contemp$sd_DOC_prod_mgC_region_season/1000000000000000

seagrass_productivity_contemp$RDOC_prod_TgC_region_season<-seagrass_productivity_contemp$DOC_prod_TgC_region_season*0.1 #percent refractory Pellikaan 1988
seagrass_productivity_contemp$sd_RDOC_prod_TgC_region_season<-seagrass_productivity_contemp$sd_DOC_prod_TgC_region_season*0.1 #percent refractory Pellikaan 1988

seagrass_productivity_contemp$BDOC_prod_TgC_region_season<-seagrass_productivity_contemp$DOC_prod_TgC_region_season-seagrass_productivity_contemp$RDOC_prod_TgC_region_season
seagrass_productivity_contemp$sd_BDOC_prod_TgC_region_season<-sqrt(seagrass_productivity_contemp$sd_DOC_prod_TgC_region_season^2+seagrass_productivity_contemp$sd_RDOC_prod_TgC_region_season^2)

seagrass_productivity_contemp_total<-seagrass_productivity_contemp %>% 
  group_by(Species) %>% 
  summarise(NPP_gC_region_season=sum(NPP_gC_region_season),
            sd_NPP_gC_region_season=sqrt(sum(sd_NPP_gC_region_season^2)),
            DOC_prod_TgC_region_season=sum(DOC_prod_TgC_region_season),
            sd_DOC_prod_TgC_region_season=sqrt(sum(sd_DOC_prod_TgC_region_season^2)),
            NPP_gC_m2_season=sum(NPP_gC_m2_season),
            sd_NPP_gC_m2_season=sqrt(sum(sd_NPP_gC_m2_season^2)),
            DOC_prod_mgC_m2_season=sum(DOC_prod_mgC_m2_season),
            sd_DOC_prod_mgC_m2_season=sqrt(sum(sd_DOC_prod_mgC_m2_season^2)),
            RDOC_prod_mgC_m2_season=sum(RDOC_prod_mgC_m2_season),
            sd_RDOC_prod_mgC_m2_season=sqrt(sum(sd_RDOC_prod_mgC_m2_season^2)))

seagrass_productivity_contemp_total$Season<-"Annual"
seagrass_productivity_contemp_total<-select(seagrass_productivity_contemp_total, 
                                            Species, 
                                            Season, 
                                            NPP_gC_region_season, 
                                            sd_NPP_gC_region_season, 
                                            DOC_prod_TgC_region_season, 
                                            sd_DOC_prod_TgC_region_season,
                                            NPP_gC_m2_season,
                                            sd_NPP_gC_m2_season,
                                            DOC_prod_mgC_m2_season,
                                            sd_DOC_prod_mgC_m2_season,
                                            RDOC_prod_mgC_m2_season,
                                            sd_RDOC_prod_mgC_m2_season)

seagrass_productivity_contemp_summary<-bind_rows(seagrass_productivity_contemp, seagrass_productivity_contemp_total)
seagrass_productivity_contemp_summary$NPP_TgC_region_season<-seagrass_productivity_contemp_summary$NPP_gC_region_season/1000000000000
seagrass_productivity_contemp_summary$sd_NPP_TgC_region_season<-seagrass_productivity_contemp_summary$sd_NPP_gC_region_season/1000000000000

productivity_contemp_summary_all<-rbind(productivity_contemp_summary, seagrass_productivity_contemp_summary)

seagrass_BDOC_decay<-select(seagrass_productivity_contemp, Species, Season, BDOC_prod_TgC_region_season)
seagrass_RDOC_decay<-select(seagrass_productivity_contemp, Species, Season, RDOC_prod_TgC_region_season, sd_RDOC_prod_TgC_region_season)

decay_function = function(BDOC_prod_TgC_region_season){
  t=1:90
  BDOC_t=BDOC_prod_TgC_region_season*exp(-0.01*t)
}

seagrass_DOC_decay<-t(mapply(decay_function,  seagrass_BDOC_decay$BDOC_prod_TgC_region_season))

colnames(seagrass_DOC_decay)<-paste("BDOC TgC", seq(1:90), " d")
seagrass_DOC_decay<-as.data.frame(seagrass_DOC_decay)

seagrass_BDOC_decay<-bind_cols(seagrass_BDOC_decay, seagrass_DOC_decay)
seagrass_BDOC_decay<-select(seagrass_BDOC_decay, -BDOC_prod_TgC_region_season)
seagrass_BDOC_decay<-seagrass_BDOC_decay %>% 
  gather("code", "BDOC_TgC_region_species", 3:92) %>% 
  separate(code, c("a", "b", "Time", "c", "units"), sep=" ") %>% 
  select(-a, -b, -c, -units)

seagrass_BDOC_decay$Time<-as.numeric(seagrass_BDOC_decay$Time)

#Standing stock
seagrass_biomass<-read.csv("C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Carbon Budget/Wong_ecosphere_data/Ecosphere_Above_BelowGround_Ratio.csv") #core area is 91.6 cm^2 (10.8 cm diameter). 

seagrass_biomass$Species<-"Zostera marina"

standing_stock_seagrass_annual <- seagrass_biomass%>% 
  group_by(Species) %>% 
  summarise(mean_AG_Biomass_kg_dw_0_00916m2=(mean(AG_DW_g, na.rm=TRUE)/1000), 
            sd_AG_Biomass_kg_dw_0_00916m2=sd(AG_DW_g, na.rm=TRUE)/1000,
            mean_BG_Biomass_kg_dw_0_00916m2=mean(BG_DW_g, na.rm=TRUE)/1000, 
            sd_BG_Biomass_kg_dw_0_00916m2=sd(BG_DW_g, na.rm=TRUE)/1000)

standing_stock_seagrass_annual <- standing_stock_seagrass_annual%>% 
  group_by(Species) %>% 
  summarise(mean_AG_Biomass_kg_dw_1m2=mean_AG_Biomass_kg_dw_0_00916m2*109.17, 
            sd_AG_Biomass_kg_dw_1m2=sd_AG_Biomass_kg_dw_0_00916m2*109.17, #conversion factor from core area to 1m2 = 109.17
            mean_BG_Biomass_kg_dw_1m2=mean_BG_Biomass_kg_dw_0_00916m2*109.17, 
            sd_BG_Biomass_kg_dw_1m2=sd_BG_Biomass_kg_dw_0_00916m2*109.17)

standing_stock_seagrass_annual$Area_bioregion_m2<-239780000

standing_stock_seagrass_annual_above<-select(standing_stock_seagrass_annual, mean_AG_Biomass_kg_dw_1m2, sd_AG_Biomass_kg_dw_1m2, Area_bioregion_m2, Species)
standing_stock_seagrass_annual_above$Position<-"Above"

standing_stock_seagrass_annual_below<-select(standing_stock_seagrass_annual, mean_BG_Biomass_kg_dw_1m2, sd_BG_Biomass_kg_dw_1m2, Area_bioregion_m2, Species)
standing_stock_seagrass_annual_below$Position<-"Below"

standing_stock_seagrass_annual_above$Standing_Stock_Tg_dw_bioregion<-(standing_stock_seagrass_annual_above$mean_AG_Biomass_kg_dw_1m2*standing_stock_seagrass_annual_above$Area_bioregion_m2)/1000000000
standing_stock_seagrass_annual_above$sd_Standing_Stock_Tg_dw_bioregion<-(standing_stock_seagrass_annual_above$sd_AG_Biomass_kg_dw_1m2*standing_stock_seagrass_annual_above$Area_bioregion_m2)/1000000000

standing_stock_seagrass_annual_below$Standing_Stock_Tg_dw_bioregion<-(standing_stock_seagrass_annual_below$mean_BG_Biomass_kg_dw_1m2*standing_stock_seagrass_annual_below$Area_bioregion_m2)/1000000000
standing_stock_seagrass_annual_below$sd_Standing_Stock_Tg_dw_bioregion<-(standing_stock_seagrass_annual_below$sd_BG_Biomass_kg_dw_1m2*standing_stock_seagrass_annual_below$Area_bioregion_m2)/1000000000

standing_stock_seagrass_annual_above$Standing_Stock_Tg_C_bioregion<-standing_stock_seagrass_annual_above$Standing_Stock_Tg_dw_bioregion*0.36 #Conversion from Postlethwaite et al. 2018
standing_stock_seagrass_annual_above$sd_Standing_Stock_Tg_C_bioregion<-standing_stock_seagrass_annual_above$sd_Standing_Stock_Tg_dw_bioregion*0.36

standing_stock_seagrass_annual_below$Standing_Stock_Tg_C_bioregion<-standing_stock_seagrass_annual_below$Standing_Stock_Tg_dw_bioregion*0.36
standing_stock_seagrass_annual_below$sd_Standing_Stock_Tg_C_bioregion<-standing_stock_seagrass_annual_below$sd_Standing_Stock_Tg_dw_bioregion*0.36

standing_stock_seagrass_annual_above$Standing_Stock_kg_C_m2<-standing_stock_seagrass_annual_above$mean_AG_Biomass_kg_dw_1m2*0.36 #Conversion from Postlethwaite et al. 2018
standing_stock_seagrass_annual_above$sd_Standing_Stock_kg_C_m2<-standing_stock_seagrass_annual_above$sd_AG_Biomass_kg_dw_1m2*0.36

standing_stock_seagrass_annual_below$Standing_Stock_kg_C_m2<-standing_stock_seagrass_annual_below$mean_BG_Biomass_kg_dw_1m2*0.36
standing_stock_seagrass_annual_below$sd_Standing_Stock_kg_C_m2<-standing_stock_seagrass_annual_below$sd_BG_Biomass_kg_dw_1m2*0.36

standing_stock_seagrass_annual_above_c<-select(standing_stock_seagrass_annual_above, -mean_AG_Biomass_kg_dw_1m2, -sd_AG_Biomass_kg_dw_1m2)
standing_stock_seagrass_annual_below_c<-select(standing_stock_seagrass_annual_below, -mean_BG_Biomass_kg_dw_1m2, -sd_BG_Biomass_kg_dw_1m2)

standing_stock_seagrass_annual_c<-rbind(standing_stock_seagrass_annual_above_c, standing_stock_seagrass_annual_below_c)

standing_stock_seagrass_annual_c_biomass<-standing_stock_seagrass_annual_c %>% 
  group_by(Area_bioregion_m2, Species) %>% 
  summarise(Standing_Stock_Tg_C_bioregion=sum(Standing_Stock_Tg_C_bioregion),
            sd_Standing_Stock_Tg_C_bioregion=sqrt(sum(sd_Standing_Stock_Tg_C_bioregion^2)),
            Standing_Stock_Tg_dw_bioregion=sum(Standing_Stock_Tg_dw_bioregion), 
            sd_Standing_Stock_Tg_dw_bioregion=sqrt(sum(sd_Standing_Stock_Tg_dw_bioregion^2)),
            Standing_Stock_kg_C_m2=sum(Standing_Stock_kg_C_m2),
            sd_Standing_Stock_kg_C_m2=sqrt(sum(sd_Standing_Stock_kg_C_m2^2)))

#sediment carbon
sediment_carbon<-read.csv(file="C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Carbon Budget/zostera_sediment_carbon_content_rev1m.csv")

sediment_carbon$Area_bioregion_m2<-239780000

sediment_carbon$Standing_Stock_kg_C_m2<-sediment_carbon$mean_sediment_carbon_gC_m2/1000
sediment_carbon$sd_Standing_Stock_kg_C_m2<-sediment_carbon$sd_sediment_carbon_gC_m2/1000

sediment_carbon$Standing_Stock_Tg_C_bioregion<-(sediment_carbon$mean_sediment_carbon_gC_m2*sediment_carbon$Area_bioregion_m2)/1000000000000
sediment_carbon$sd_Standing_Stock_Tg_C_bioregion<-(sediment_carbon$sd_sediment_carbon_gC_m2*sediment_carbon$Area_bioregion_m2)/1000000000000

sediment_carbon_c<-select(sediment_carbon, Species, Position, Area_bioregion_m2, Standing_Stock_Tg_C_bioregion, sd_Standing_Stock_Tg_C_bioregion, Standing_Stock_kg_C_m2, sd_Standing_Stock_kg_C_m2)
sediment_carbon_c$Standing_Stock_Tg_dw_bioregion<-NA #Don't have dw data
sediment_carbon_c$sd_Standing_Stock_Tg_dw_bioregion<-NA

standing_stock_seagrass_annual_c<-rbind(standing_stock_seagrass_annual_c, sediment_carbon_c)

standing_stock_seagrass_annual_c_combined<-standing_stock_seagrass_annual_c %>% 
  group_by(Area_bioregion_m2, Species) %>% 
  summarise(Standing_Stock_Tg_C_bioregion=sum(Standing_Stock_Tg_C_bioregion),
            sd_Standing_Stock_Tg_C_bioregion=sqrt(sum(sd_Standing_Stock_Tg_C_bioregion^2)),
            Standing_Stock_Tg_dw_bioregion=sum(Standing_Stock_Tg_dw_bioregion), 
            sd_Standing_Stock_Tg_dw_bioregion=sqrt(sum(sd_Standing_Stock_Tg_dw_bioregion^2)),
            Standing_Stock_kg_C_m2=sum(Standing_Stock_kg_C_m2), 
            sd_Standing_Stock_kg_C_m2=sqrt(sum(sd_Standing_Stock_kg_C_m2^2)))


#combining standing stock values with kelp
standing_stock_annual<-rbind(standing_stock_seagrass_annual_c_combined, standing_stock_kelp_annual_c)


#### merging DOC particle export with DOC amounts ####
setwd("C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Particle Tracking/Revised_POC_DOC_experiments/DOC/Compiled_outputs/Particle_shelf_by_Day") #data from Krumhansl et al. in review

temp = list.files(pattern="*.txt", )
myfiles = lapply(setNames(temp, make.names(gsub("*.txt$", "", temp))), 
                 read.table, header = FALSE, sep=",")

DOC_particle_export<-bind_rows(myfiles, .id="model")
DOC_particle_export<-DOC_particle_export %>% 
  separate(model, c("Species", "Genus", "Carbon", "Domain", "Season", "Num_days", "unit", "Percent", "Shelf", "by", "day"), sep="_") %>% 
  select(-Num_days, -unit, -Percent, -Shelf, -by, -day)
DOC_particle_export$Region<-NA
DOC_particle_export$Region[DOC_particle_export$Domain == ".East"]<- "East"
DOC_particle_export$Region[DOC_particle_export$Domain == ".West"]<- "West"
DOC_particle_export$Region[DOC_particle_export$Domain == "West"]<- "West"
DOC_particle_export$Region[DOC_particle_export$Domain == "East"]<- "East"

DOC_particle_export<-DOC_particle_export %>% 
  select(-Domain) %>% 
  rename(Time=V1, Percent_shelf=V2)

DOC_particle_export<-DOC_particle_export %>%
  group_by(Species, Genus, Season, Region) %>%
  mutate(Percent_shelf_dt = Percent_shelf - lag(Percent_shelf))

DOC_particle_export<-DOC_particle_export[!(DOC_particle_export$Time==0),]

DOC_particle_export$Species_s<-paste(DOC_particle_export$Species, DOC_particle_export$Genus, sep=" ")
DOC_particle_export<-ungroup(DOC_particle_export)
DOC_particle_export<-DOC_particle_export %>% 
  select(-Species, -Genus) %>% 
  rename(Species=Species_s)

DOC_particle_export_summary<-DOC_particle_export %>%
  subset(Time==90) %>% 
  group_by(Species, Season) %>% 
  summarise(mean_DOC_particle_export=mean(Percent_shelf),
            sd_DOC_particle_export=sd(Percent_shelf)) 

DOC_particle_export_total<-DOC_particle_export %>% 
  subset(Time==90) %>% 
  group_by(Species) %>% 
  summarise(mean_DOC_particle_export=mean(Percent_shelf),
            sd_DOC_particle_export=sd(Percent_shelf))  
  
DOC_particle_export_total$Season<-"Annual"

DOC_particle_export_summary<-rbind(DOC_particle_export_summary, DOC_particle_export_total)
DOC_particle_export_summary$Season<-factor(DOC_particle_export_summary$Season, levels=c("Winter", "Spring", "Summer", "Fall", "Annual"))

DOC_particle_export_summary$Species[DOC_particle_export_summary$Species=="Zostera Marina"]<-"Zostera marina"
DOC_particle_export$Species[DOC_particle_export$Species=="Zostera Marina"]<-"Zostera marina"

BDOC_decay_all<-rbind(BDOC_decay, seagrass_BDOC_decay)

BDOC_decay_all_m<-left_join(BDOC_decay_all, DOC_particle_export)


BDOC_decay_all_m<-ungroup(BDOC_decay_all_m)

BDOC_decay_all_summary<-BDOC_decay_all_m %>% 
  group_by(Species, Season, Time, Region, BDOC_TgC_region_species) %>% 
  summarise(mean_Percent_shelf_dt=mean(Percent_shelf_dt))

BDOC_decay_all_summary$DOC_sequestered<-NA

DOC_loss <- function(df) {

  for (i in 2:nrow(df)) {
    df$DOC_sequestered[i] <- df$BDOC_TgC_region_species[i] * (df$mean_Percent_shelf_dt[i]/100)
    
    if (i < nrow(df)) {
     df$BDOC_TgC_region_species[i + 1] <- df$BDOC_TgC_region_species[i+1] - df$DOC_sequestered[i]
    }
  }

  return(df)
}

BDOC_decay_all_summary <- BDOC_decay_all_summary %>%
  group_by(Species, Season, Region) %>%
  do(DOC_loss(.))

BDOC_decay_all_summary<-BDOC_decay_all_summary %>% 
  group_by(Species, Season, Region) %>% 
  summarise(BDOC_sequestered_TgC_season_coast=sum(DOC_sequestered, na.rm=TRUE))

BDOC_decay_all_Rsummary<-BDOC_decay_all_summary %>% 
  group_by(Species, Season) %>% 
  summarise(mean_BDOC_sequestered_TgC_season_coast=mean(BDOC_sequestered_TgC_season_coast, na.rm=TRUE),
            sd_BDOC_sequestered_TgC_season_coast=sd(BDOC_sequestered_TgC_season_coast, na.rm=TRUE)) %>% 
  rename(BDOC_sequestered_TgC_season_coast=mean_BDOC_sequestered_TgC_season_coast)

RDOC_decay_all<-rbind(RDOC_decay, seagrass_RDOC_decay)

DOC_seq_all_summary<-left_join(BDOC_decay_all_Rsummary, RDOC_decay_all)
DOC_seq_all_summary$TDOC_sequestered_TgC_season_coast<-DOC_seq_all_summary$BDOC_sequestered_TgC_season_coast+DOC_seq_all_summary$RDOC_prod_TgC_region_season
DOC_seq_all_summary$sd_TDOC_sequestered_TgC_season_coast<-sqrt(DOC_seq_all_summary$sd_BDOC_sequestered_TgC_season_coast^2+DOC_seq_all_summary$sd_RDOC_prod_TgC_region_season^2)

DOC_seq_all_total<-DOC_seq_all_summary %>% 
  group_by(Species) %>% 
  summarise(TDOC_sequestered_TgC_season_coast=sum(TDOC_sequestered_TgC_season_coast), 
            BDOC_sequestered_TgC_season_coast=sum(BDOC_sequestered_TgC_season_coast), 
            RDOC_prod_TgC_region_season=sum(RDOC_prod_TgC_region_season),
            sd_TDOC_sequestered_TgC_season_coast=sqrt(sum(sd_TDOC_sequestered_TgC_season_coast^2)), 
            sd_BDOC_sequestered_TgC_season_coast=sqrt(sum(sd_BDOC_sequestered_TgC_season_coast^2)), 
            sd_RDOC_prod_TgC_region_season=sqrt(sum(sd_RDOC_prod_TgC_region_season^2)))

DOC_seq_all_total$Season<-"Annual"

DOC_seq_all_summary<-bind_rows(DOC_seq_all_summary, DOC_seq_all_total)
DOC_seq_all_summary$Season<-factor(DOC_seq_all_summary$Season, levels=c("Winter", "Spring", "Summer", "Fall", "Annual"))


#### Kelp and Seagrass POC Production and Export ####
#dislodgement

productivity_contemp<-left_join(productivity_contemp, biomass_per_ind)

productivity_contemp$Dislodgement_ind_lost_season<-productivity_contemp$mean_Density_no_1m2*productivity_contemp$mean_prop_dislodged
productivity_contemp$sd_Dislodgement_ind_lost_season<-sqrt((productivity_contemp$sd_Density_no_1m2/productivity_contemp$mean_Density_no_1m2)^2+ 
                                                             (productivity_contemp$sd_prop_dislodged/productivity_contemp$mean_prop_dislodged)^2)*
                                                              productivity_contemp$Dislodgement_ind_lost_season


productivity_contemp$Dislodgement_kg_dw_m2_season<-productivity_contemp$Dislodgement_ind_lost_season*productivity_contemp$mean_biomass_per_ind_kg_dw
productivity_contemp$sd_Dislodgement_kg_dw_m2_season<-sqrt((productivity_contemp$sd_Dislodgement_ind_lost_season/productivity_contemp$Dislodgement_ind_lost_season)^2+ 
                                                             (productivity_contemp$sd_biomass_per_ind_kg_dw/productivity_contemp$mean_biomass_per_ind_kg_dw)^2)*
                                                              productivity_contemp$Dislodgement_kg_dw_m2_season


productivity_contemp$Dislodgement_kgC_m2_season<-productivity_contemp$Dislodgement_kg_dw_m2_season*0.3
productivity_contemp$sd_Dislodgement_kgC_m2_season<-productivity_contemp$sd_Dislodgement_kg_dw_m2_season*0.3

productivity_contemp$Dislodgement_TgC_m2_season<-productivity_contemp$Dislodgement_kgC_m2_season/1000000000
productivity_contemp$sd_Dislodgement_TgC_m2_season<-productivity_contemp$sd_Dislodgement_kgC_m2_season/1000000000

productivity_contemp$Dislodgement_TgC_region_season<-productivity_contemp$Dislodgement_TgC_m2_season*productivity_contemp$Kelp_area_bioregion_m2 
productivity_contemp$sd_Dislodgement_TgC_region_season<-productivity_contemp$sd_Dislodgement_TgC_m2_season*productivity_contemp$Kelp_area_bioregion_m2 

#Kelp Erosion

productivity_contemp$Density_eroded_no_1m2<-productivity_contemp$mean_Density_no_1m2-productivity_contemp$Dislodgement_ind_lost_season
productivity_contemp$sd_Density_eroded_no_1m2<-sqrt(productivity_contemp$sd_Density_no_1m2^2+productivity_contemp$sd_Dislodgement_ind_lost_season^2)


productivity_contemp$Erosion_g_dw_m2_d<-productivity_contemp$Density_eroded_no_1m2*productivity_contemp$mean_erosion_g_dw_d
productivity_contemp$sd_Erosion_g_dw_m2_d<-sqrt((productivity_contemp$sd_Density_eroded_no_1m2/productivity_contemp$Density_eroded_no_1m2)^2+ 
                                                (productivity_contemp$sd_erosion_g_dw_d/productivity_contemp$mean_erosion_g_dw_d)^2)*
                                                 productivity_contemp$Erosion_g_dw_m2_d


productivity_contemp$Erosion_g_dw_m2_season<-productivity_contemp$Erosion_g_dw_m2_d*91
productivity_contemp$sd_Erosion_g_dw_m2_season<-productivity_contemp$sd_Erosion_g_dw_m2_d*91

productivity_contemp$Erosion_gC_m2_season<-productivity_contemp$Erosion_g_dw_m2_season*0.3
productivity_contemp$sd_Erosion_gC_m2_season<-productivity_contemp$sd_Erosion_g_dw_m2_season*0.3

productivity_contemp$Erosion_gC_region_season<-productivity_contemp$Erosion_gC_m2_season*productivity_contemp$Kelp_area_bioregion_m2 
productivity_contemp$sd_Erosion_gC_region_season<-productivity_contemp$sd_Erosion_gC_m2_season*productivity_contemp$Kelp_area_bioregion_m2 

productivity_contemp$Erosion_TgC_region_season<-productivity_contemp$Erosion_gC_region_season/1000000000000
productivity_contemp$sd_Erosion_TgC_region_season<-productivity_contemp$sd_Erosion_gC_region_season/1000000000000

# calculating average stipe:blade biomass ratio to split off stipe weight for RPOC calculation
stipe_blade<-read.csv("C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Carbon Budget/kelp_blade_stipe_weight.csv")
stipe_blade$stipe_blade_ratio<-stipe_blade$Stipe_weight_gww/stipe_blade$Blade_weight_gww
stipe_blade$sd_stipe_blade_ratio<-sqrt((stipe_blade$sd_Stipe_weight_gww/stipe_blade$Stipe_weight_gww)^2+
                                       (stipe_blade$sd_Blade_weight_gww/stipe_blade$Blade_weight_gww)^2)*
                                        stipe_blade$stipe_blade_ratio

stipe_blade_summary<-stipe_blade %>% 
  group_by(Species) %>% 
  summarise(mean_stipe_blade_ratio=mean(stipe_blade_ratio),
            SD_stipe_blade_ratio=sqrt(sum(sd_stipe_blade_ratio/stipe_blade_ratio^2))*mean_stipe_blade_ratio) 

#### Combining components of seagrass POC production to total POC
#Export
seagrass_productivity_contemp$Export_gC_m2_season<-seagrass_productivity_contemp$Export_gC_m2_d*91
seagrass_productivity_contemp$sd_Export_gC_m2_season<-seagrass_productivity_contemp$sd_Export_gC_m2_d*91

seagrass_productivity_contemp$Export_gC_region_season<-seagrass_productivity_contemp$Export_gC_m2_season*seagrass_area_bioregion_m2
seagrass_productivity_contemp$sd_Export_gC_region_season<-seagrass_productivity_contemp$sd_Export_gC_m2_season*seagrass_area_bioregion_m2

seagrass_productivity_contemp$Export_TgC_region_season<-seagrass_productivity_contemp$Export_gC_region_season/1000000000000
seagrass_productivity_contemp$sd_Export_TgC_region_season<-seagrass_productivity_contemp$sd_Export_gC_region_season/1000000000000

#Leaf fall
seagrass_productivity_contemp$Leaf_fall_gC_m2_season<-seagrass_productivity_contemp$Leaf_fall_gC_m2_d*91
seagrass_productivity_contemp$sd_Leaf_fall_gC_m2_season<-seagrass_productivity_contemp$sd_Leaf_fall_gC_m2_d*91

seagrass_productivity_contemp$Leaf_fall_gC_region_season<-seagrass_productivity_contemp$Leaf_fall_gC_m2_season*seagrass_area_bioregion_m2
seagrass_productivity_contemp$sd_Leaf_fall_gC_region_season<-seagrass_productivity_contemp$sd_Leaf_fall_gC_m2_season*seagrass_area_bioregion_m2

seagrass_productivity_contemp$Leaf_fall_TgC_region_season<-seagrass_productivity_contemp$Leaf_fall_gC_region_season/1000000000000
seagrass_productivity_contemp$sd_Leaf_fall_TgC_region_season<-seagrass_productivity_contemp$sd_Leaf_fall_gC_region_season/1000000000000


#combining both species and calculating the total POC production 
#(Erosion and Dislodgement for kelp, Leaf export for seagrass, #Feb 3 edit)
productivity_contemp_POC_all<-rbind(productivity_contemp, seagrass_productivity_contemp)

productivity_contemp_POC_all<-select(productivity_contemp_POC_all, Species, Season, 
                                     Leaf_fall_TgC_region_season,
                                     Leaf_fall_gC_m2_season,
                                     sd_Leaf_fall_TgC_region_season,
                                     sd_Leaf_fall_gC_m2_season,
                                     Export_TgC_region_season,
                                     Export_gC_m2_season,
                                     sd_Export_TgC_region_season,
                                     sd_Export_gC_m2_season,
                                     Erosion_TgC_region_season,
                                     Erosion_gC_m2_season,
                                     sd_Erosion_TgC_region_season,
                                     sd_Erosion_gC_m2_season,
                                     Dislodgement_TgC_region_season,
                                     Dislodgement_kgC_m2_season,
                                     sd_Dislodgement_TgC_region_season,
                                     sd_Dislodgement_kgC_m2_season
                                     )

productivity_contemp_POC_all<-left_join(productivity_contemp_POC_all, stipe_blade_summary)

productivity_contemp_POC_all<-productivity_contemp_POC_all %>% 
  group_by(Species, Season) %>% 
  mutate(POC_total_TgC_region_season = if_else(Species == "Zostera marina",
                                               Leaf_fall_TgC_region_season,
                                               Erosion_TgC_region_season + Dislodgement_TgC_region_season)) %>% 
  mutate(sd_POC_total_TgC_region_season = if_else(Species== "Zostera marina",
                                                 sd_Leaf_fall_TgC_region_season,
                                                 sqrt(sd_Erosion_TgC_region_season^2 + sd_Dislodgement_TgC_region_season^2))) %>% 
  mutate(POC_total_gC_m2_season = if_else(Species == "Zostera marina",
                                          Leaf_fall_gC_m2_season,
                                          Erosion_gC_m2_season + (Dislodgement_kgC_m2_season*1000))) %>% 
  mutate(sd_POC_total_gC_m2_season = if_else(Species== "Zostera marina",
                                                  sd_Leaf_fall_gC_m2_season,
                                                  sqrt(sd_Erosion_gC_m2_season^2 + (sd_Dislodgement_kgC_m2_season*1000)^2)))
  
productivity_contemp_POC_all<-productivity_contemp_POC_all %>% 
  group_by(Species, Season) %>% 
  mutate(RPOC_total_TgC_region_season = if_else(Species == "Zostera marina",
                                                POC_total_TgC_region_season*0.055, #% refractory from Kuwae & Hori 2019, Tarutani et al. 2012, Trevathan-Tackett et al. 2017
                                                Dislodgement_TgC_region_season*mean_stipe_blade_ratio*0.14)) %>% # calculating the refractory component as only stipes lost through dislodgement, corrected with a % refractory value of stipes from Pedersen et al. 2021, assuming Aerobic and an average of 4 and 10 C
  mutate(sd_RPOC_total_TgC_region_season = ifelse(Species == "Zostera marina",
                                                  sd_POC_total_TgC_region_season*0.055, #no SD on the # refractory value
                                                  sqrt((sd_Dislodgement_TgC_region_season/Dislodgement_TgC_region_season)^2+
                                                         (SD_stipe_blade_ratio/mean_stipe_blade_ratio)^2+
                                                         (0.088/0.14)^2)*RPOC_total_TgC_region_season)) %>% #SD from Pedersen 
  mutate(RPOC_total_gC_m2_season = if_else(Species == "Zostera marina",
                                           POC_total_gC_m2_season*0.055,
                                           Dislodgement_kgC_m2_season*mean_stipe_blade_ratio*0.14)) %>%
  mutate(sd_RPOC_total_gC_m2_season = if_else(Species == "Zostera marina",
                                              sd_POC_total_TgC_region_season*0.055,
                                              sqrt((sd_Dislodgement_kgC_m2_season/Dislodgement_kgC_m2_season)^2+
                                                     (SD_stipe_blade_ratio/mean_stipe_blade_ratio)^2+
                                                     (0.088/0.14)^2)*RPOC_total_gC_m2_season))
  
productivity_contemp_POC_all$BPOC_total_TgC_region_season<-productivity_contemp_POC_all$POC_total_TgC_region_season-productivity_contemp_POC_all$RPOC_total_TgC_region_season
productivity_contemp_POC_all$sd_BPOC_total_TgC_region_season<-sqrt(productivity_contemp_POC_all$sd_POC_total_TgC_region_season^2+productivity_contemp_POC_all$sd_RPOC_total_TgC_region_season^2)

productivity_contemp_POC_all$BPOC_total_gC_m2_season<-productivity_contemp_POC_all$POC_total_gC_m2_season-productivity_contemp_POC_all$RPOC_total_gC_m2_season
productivity_contemp_POC_all$sd_BPOC_total_gC_m2_season<-sqrt(productivity_contemp_POC_all$sd_POC_total_gC_m2_season^2+productivity_contemp_POC_all$sd_RPOC_total_gC_m2_season^2)


####DOC production from POC decay####

productivity_contemp_POC_all<-productivity_contemp_POC_all %>% 
  group_by(Species, Season) %>% 
  mutate(DOC_frPOC_total_TgC_region_season = if_else(Species == "Zostera marina",
                                                      POC_total_TgC_region_season*0.25, # 25% of carbon is leached DOC from Pellikaan 1984.
                                                      POC_total_TgC_region_season*0.59)) %>%  # 59% of carbon released as DOC from degrading POC of E. radiata Perkins et al. 2022
  mutate(sd_DOC_frPOC_total_TgC_region_season = ifelse(Species == "Zostera marina",
                                                        sd_POC_total_TgC_region_season*0.25, #No sd in the paper
                                                  sqrt((sd_POC_total_TgC_region_season/POC_total_TgC_region_season)^2
                                                        +(0.41/0.59)^2)*DOC_frPOC_total_TgC_region_season)) %>% #SD from Perkins et al. 2022
  mutate(DOC_frPOC_total_gC_m2_season = if_else(Species == "Zostera marina",
                                                 POC_total_gC_m2_season*0.25,
                                                 POC_total_gC_m2_season*0.59)) %>%
  mutate(sd_DOC_frPOC_total_gC_m2_season = if_else(Species == "Zostera marina",
                                                sd_POC_total_gC_m2_season*0.25,
                                                sqrt((sd_POC_total_gC_m2_season/POC_total_gC_m2_season)^2+
                                                     (0.41/0.59)^2)*DOC_frPOC_total_gC_m2_season))

productivity_contemp_POC_all<-productivity_contemp_POC_all %>% 
  group_by(Species, Season) %>% 
  mutate(RDOC_frPOC_total_TgC_region_season = if_else(Species == "Zostera marina",
                                                       DOC_frPOC_total_TgC_region_season*0.1, # refractory DOC 
                                                       DOC_frPOC_total_TgC_region_season*0.375)) %>%  # refractory DOC 
  mutate(sd_RDOC_frPOC_total_TgC_region_season = ifelse(Species == "Zostera marina",
                                                         sd_DOC_frPOC_total_TgC_region_season*0.1, # refractory DOC
                                                         sd_DOC_frPOC_total_TgC_region_season*0.375)) %>% #SD from Perkins et al. 2022
  mutate(RDOC_frPOC_total_gC_m2_season = if_else(Species == "Zostera marina",
                                                  DOC_frPOC_total_gC_m2_season*0.1,
                                                  DOC_frPOC_total_gC_m2_season*0.375)) %>%
  mutate(sd_RDOC_frPOC_total_gC_m2_season = if_else(Species == "Zostera marina",
                                                     sd_DOC_frPOC_total_gC_m2_season*0.1,
                                                     sd_DOC_frPOC_total_gC_m2_season*0.375))

productivity_contemp_POC_all$BDOC_frPOC_total_TgC_region_season<-productivity_contemp_POC_all$DOC_frPOC_total_TgC_region_season-productivity_contemp_POC_all$RDOC_frPOC_total_TgC_region_season
productivity_contemp_POC_all$sd_BDOC_frPOC_total_TgC_region_season<-sqrt(productivity_contemp_POC_all$sd_DOC_frPOC_total_TgC_region_season^2+productivity_contemp_POC_all$sd_RDOC_frPOC_total_TgC_region_season^2)

productivity_contemp_POC_all$BDOC_frPOC_total_gC_m2_season<-productivity_contemp_POC_all$DOC_frPOC_total_gC_m2_season-productivity_contemp_POC_all$RDOC_frPOC_total_gC_m2_season
productivity_contemp_POC_all$sd_BDOC_frPOC_total_gC_m2_season<-sqrt(productivity_contemp_POC_all$sd_DOC_frPOC_total_gC_m2_season^2+productivity_contemp_POC_all$sd_RDOC_frPOC_total_gC_m2_season^2)


#Modeling BDOC decay
# Select the relevant columns
BDOC_decay <- select(productivity_contemp_POC_all, Species, Season, BDOC_frPOC_total_TgC_region_season)
RDOC_decay <- select(productivity_contemp_POC_all, Species, Season, RDOC_frPOC_total_TgC_region_season, sd_RDOC_frPOC_total_TgC_region_season)

# Define the decay function to predict BDOC values at each time t
decay_function <- function(Species, BDOC_frPOC_total_TgC_region_season, t) {
  if (Species == "Zostera marina") {
    # Apply decay equation for Zostera marina
    return(BDOC_frPOC_total_TgC_region_season * exp(-0.06 * t))
  } else {
    # Apply decay equation for other species
    return(BDOC_frPOC_total_TgC_region_season * exp(-0.01 * t))
  }
}

# Apply the function across all rows and all time points (t = 1:90)
t_values <- 1:90
DOC_decay <- sapply(t_values, function(t) {
  mapply(decay_function, BDOC_decay$Species, BDOC_decay$BDOC_frPOC_total_TgC_region_season, MoreArgs = list(t = t))
})


colnames(DOC_decay)<-paste("BDOC frPOC TgC", seq(1:90), "d")
DOC_decay<-as.data.frame(DOC_decay)

BDOC_decay<-bind_cols(BDOC_decay, DOC_decay)
BDOC_decay<-select(BDOC_decay, -BDOC_frPOC_total_TgC_region_season)
BDOC_decay<-BDOC_decay %>% 
  gather("code", "BDOC_frPOC_TgC_region_species", 3:92) %>% 
  separate(code, c("a", "b", "c", "Time", "units"), sep=" ") %>% 
  select(-a, -b, -c, -units)

BDOC_decay$Time<-as.numeric(BDOC_decay$Time)


# Calculate Export of BDOC from POC
BDOC_frPOC_decay_m<-left_join(BDOC_decay, DOC_particle_export)

BDOC_frPOC_decay_m<-ungroup(BDOC_frPOC_decay_m)

BDOC_frPOC_decay_m_summary<-BDOC_frPOC_decay_m %>% 
  group_by(Species, Season, Time, Region, BDOC_frPOC_TgC_region_species) %>% 
  summarise(mean_Percent_shelf_dt=mean(Percent_shelf_dt))#, 
#sd_Percent_shelf_dt=sd(Percent_shelf_dt))

BDOC_frPOC_decay_m_summary$BDOC_frPOC_sequestered<-NA

DOC_loss <- function(df) {
  
  for (i in 2:nrow(df)) {
    df$BDOC_frPOC_sequestered[i] <- df$BDOC_frPOC_TgC_region_species[i] * (df$mean_Percent_shelf_dt[i]/100)
    
    if (i < nrow(df)) {
      df$BDOC_frPOC_TgC_region_species[i + 1] <- df$BDOC_frPOC_TgC_region_species[i+1] - df$BDOC_frPOC_sequestered[i]
    }
  }
  
  return(df)
}

BDOC_frPOC_decay_m_summary <- BDOC_frPOC_decay_m_summary %>%
  group_by(Species, Season, Region) %>%
  do(DOC_loss(.))

BDOC_frPOC_decay_m_summary<-BDOC_frPOC_decay_m_summary %>% 
  group_by(Species, Season, Region) %>% 
  summarise(BDOC_frPOC_sequestered_TgC_season_coast=sum(BDOC_frPOC_sequestered, na.rm=TRUE))

BDOC_frPOC_decay_m_Rsummary<-BDOC_frPOC_decay_m_summary %>% 
  group_by(Species, Season) %>% 
  summarise(mean_BDOC_frPOC_sequestered_TgC_season_coast=mean(BDOC_frPOC_sequestered_TgC_season_coast, na.rm=TRUE),
            sd_BDOC_frPOC_sequestered_TgC_season_coast=sd(BDOC_frPOC_sequestered_TgC_season_coast, na.rm=TRUE)) %>% 
  rename(BDOC_frPOC_sequestered_TgC_season_coast=mean_BDOC_frPOC_sequestered_TgC_season_coast)


DOC_frPOC_seq_all_summary<-left_join(BDOC_frPOC_decay_m_Rsummary, RDOC_decay)
DOC_frPOC_seq_all_summary$TDOC_frPOC_sequestered_TgC_season_coast<-DOC_frPOC_seq_all_summary$BDOC_frPOC_sequestered_TgC_season_coast+DOC_frPOC_seq_all_summary$RDOC_frPOC_total_TgC_region_season
DOC_frPOC_seq_all_summary$sd_TDOC_frPOC_sequestered_TgC_season_coast<-sqrt(DOC_frPOC_seq_all_summary$sd_BDOC_frPOC_sequestered_TgC_season_coast^2+DOC_frPOC_seq_all_summary$sd_RDOC_frPOC_total_TgC_region_season^2)

DOC_frPOC_seq_all_total<-DOC_frPOC_seq_all_summary %>% 
  group_by(Species) %>% 
  summarise(TDOC_frPOC_sequestered_TgC_season_coast=sum(TDOC_frPOC_sequestered_TgC_season_coast), 
            BDOC_frPOC_sequestered_TgC_season_coast=sum(BDOC_frPOC_sequestered_TgC_season_coast), 
            RDOC_frPOC_total_TgC_region_season=sum(RDOC_frPOC_total_TgC_region_season),
            sd_TDOC_frPOC_sequestered_TgC_season_coast=sqrt(sum(sd_TDOC_frPOC_sequestered_TgC_season_coast^2)), 
            sd_BDOC_frPOC_sequestered_TgC_season_coast=sqrt(sum(sd_BDOC_frPOC_sequestered_TgC_season_coast^2)), 
            sd_RDOC_frPOC_total_TgC_region_season=sqrt(sum(sd_RDOC_frPOC_total_TgC_region_season^2)))

DOC_frPOC_seq_all_total$Season<-"Annual"

DOC_frPOC_seq_all_summary<-bind_rows(DOC_frPOC_seq_all_summary, DOC_frPOC_seq_all_total)
DOC_frPOC_seq_all_summary$Season<-factor(DOC_frPOC_seq_all_summary$Season, levels=c("Winter", "Spring", "Summer", "Fall", "Annual"))


## POC Export to shelf


setwd("C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Particle Tracking/Revised_POC_DOC_experiments/POC_small/Compiled_outputs/Particle_shelf_by_Day")

temp = list.files(pattern="*.txt", )
myfiles = lapply(setNames(temp, make.names(gsub("*.txt$", "", temp))), 
                 read.table, header = FALSE, sep=",")

POC_particle_export<-bind_rows(myfiles, .id="model")
POC_particle_export<-POC_particle_export %>% 
  separate(model, c("Species", "Genus", "Carbon", "Domain", "Season", "Num_days", "unit", "Percent", "Shelf", "by", "day", "corrected"), sep="_") %>% 
  select(-Num_days, -unit, -Percent, -Shelf, -by, -day, -corrected)
POC_particle_export$Region<-NA
POC_particle_export$Region[POC_particle_export$Domain == ".East"]<- "East"
POC_particle_export$Region[POC_particle_export$Domain == ".West"]<- "West"
POC_particle_export$Region[POC_particle_export$Domain == "West"]<- "West"
POC_particle_export$Region[POC_particle_export$Domain == "East"]<- "East"

POC_particle_export<-POC_particle_export %>% 
  select(-Domain) %>% 
  rename(Time=V1, Percent_shelf=V2)

POC_particle_export<-POC_particle_export %>%
  group_by(Species, Genus, Season, Region) %>%
  mutate(Percent_shelf_dt = Percent_shelf - lag(Percent_shelf))

POC_particle_export<-POC_particle_export[!(POC_particle_export$Time==0),]

POC_particle_export$Species_s<-paste(POC_particle_export$Species, POC_particle_export$Genus, sep=" ")
POC_particle_export<-ungroup(POC_particle_export)
POC_particle_export<-POC_particle_export %>% 
  select(-Species, -Genus) %>% 
  rename(Species=Species_s)

POC_particle_export_summary<-POC_particle_export %>%
  group_by(Species, Season, Region) %>% 
  summarise(Percent_shelf=max(Percent_shelf)) %>% 
  group_by(Species, Season) %>% 
  summarise(mean_POC_particle_export=mean(Percent_shelf),
            sd_POC_particle_export=sd(Percent_shelf)) 

POC_particle_export_total<-POC_particle_export_summary %>%
  group_by(Species) %>% 
  summarise(mean_POC_particle_export=mean(mean_POC_particle_export),
            sd_POC_particle_export=sd(sd_POC_particle_export))  

POC_particle_export_total$Season<-"Annual"

POC_particle_export_summary<-rbind(POC_particle_export_summary, POC_particle_export_total)
POC_particle_export_summary$Season<-factor(POC_particle_export_summary$Season, levels=c("Winter", "Spring", "Summer", "Fall", "Annual"))

productivity_contemp_POC_all_s<-select(productivity_contemp_POC_all, Species, Season, POC_total_TgC_region_season, BPOC_total_TgC_region_season, RPOC_total_TgC_region_season)

POC_particle_export<-left_join(POC_particle_export, productivity_contemp_POC_all_s)

POC_particle_export<-ungroup(POC_particle_export)

POC_particle_export$BPOC_sequestered<-NA
POC_particle_export$RPOC_sequestered<-NA

POC_loss <- function(df) {
  
  for (i in 2:nrow(df)) {
    df$RPOC_sequestered[i] <- df$RPOC_total_TgC_region_season[i] * (df$Percent_shelf_dt[i]/100)
    df$BPOC_sequestered[i] <- df$BPOC_total_TgC_region_season[i] * (df$Percent_shelf_dt[i]/100)
    
    if (i < nrow(df)) {
      df$RPOC_total_TgC_region_season[i + 1] <- df$RPOC_total_TgC_region_season[i+1] - df$RPOC_total_TgC_region_season[i]
      df$BPOC_total_TgC_region_season[i + 1] <- df$BPOC_total_TgC_region_season[i+1] - df$BPOC_total_TgC_region_season[i]
    }
  }
  
  return(df)
}

POC_particle_export <- POC_particle_export %>%
  group_by(Species, Season, Region) %>%
  do(POC_loss(.))

POC_particle_export_season<-POC_particle_export %>% 
  group_by(Species, Season, Region) %>% 
  summarise(RPOC_sequestered_TgC_season_coast=sum(RPOC_sequestered, na.rm=TRUE),
            BPOC_sequestered_TgC_season_coast=sum(BPOC_sequestered, na.rm=TRUE))

POC_particle_export_season<-POC_particle_export_season %>% 
  group_by(Species, Season) %>% 
  summarise(mean_RPOC_sequestered_TgC_season_coast=mean(RPOC_sequestered_TgC_season_coast, na.rm=TRUE),
            sd_RPOC_sequestered_TgC_season_coast=sd(RPOC_sequestered_TgC_season_coast, na.rm=TRUE),
            mean_BPOC_sequestered_TgC_season_coast=mean(BPOC_sequestered_TgC_season_coast, na.rm=TRUE),
            sd_BPOC_sequestered_TgC_season_coast=sd(BPOC_sequestered_TgC_season_coast, na.rm=TRUE)) %>% 
  rename(RPOC_sequestered_TgC_season_coast=mean_RPOC_sequestered_TgC_season_coast,
         BPOC_sequestered_TgC_season_coast=mean_BPOC_sequestered_TgC_season_coast)

productivity_contemp_POC_all<-left_join(productivity_contemp_POC_all, POC_particle_export_season)
productivity_contemp_POC_all<-left_join(productivity_contemp_POC_all, POC_particle_export_summary)


#calculating POC deposited in muddy and seagrass seds
POC_substrate_summary<-read.csv("C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Carbon Budget/POC_substrate_summary.csv")

POC_substrate_summary<-select(POC_substrate_summary, -X)

productivity_contemp_POC_all<-left_join(productivity_contemp_POC_all, POC_substrate_summary)

#POC
productivity_contemp_POC_all$POC_deposited_seagrass_TgC_season_coast<-
  (productivity_contemp_POC_all$mean_percent_seagrass/100)*
  productivity_contemp_POC_all$POC_total_TgC_region_season

productivity_contemp_POC_all$sd_POC_deposited_seagrass_TgC_season_coast<-
  sqrt(((productivity_contemp_POC_all$sd_percent_seagrass/100)/(productivity_contemp_POC_all$mean_percent_seagrass/100))^2+
         (productivity_contemp_POC_all$sd_POC_total_TgC_region_season/productivity_contemp_POC_all$POC_total_TgC_region_season)^2)*
  productivity_contemp_POC_all$POC_deposited_seagrass_TgC_season_coast


productivity_contemp_POC_all$POC_deposited_mud_TgC_season_coast<-
  (productivity_contemp_POC_all$mean_percent_mud/100)*
  productivity_contemp_POC_all$POC_total_TgC_region_season

productivity_contemp_POC_all$sd_POC_deposited_mud_TgC_season_coast<-
  sqrt(((productivity_contemp_POC_all$sd_mud/100)/(productivity_contemp_POC_all$mean_percent_mud/100))^2+
         (productivity_contemp_POC_all$sd_POC_total_TgC_region_season/productivity_contemp_POC_all$POC_total_TgC_region_season)^2)*
  productivity_contemp_POC_all$POC_deposited_mud_TgC_season_coast

#BPOC
productivity_contemp_POC_all$BPOC_deposited_seagrass_TgC_season_coast<-
  (productivity_contemp_POC_all$mean_percent_seagrass/100)*
  productivity_contemp_POC_all$BPOC_total_TgC_region_season

productivity_contemp_POC_all$sd_BPOC_deposited_seagrass_TgC_season_coast<-
  sqrt(((productivity_contemp_POC_all$sd_percent_seagrass/100)/(productivity_contemp_POC_all$mean_percent_seagrass/100))^2+
         (productivity_contemp_POC_all$sd_BPOC_total_TgC_region_season/productivity_contemp_POC_all$BPOC_total_TgC_region_season)^2)*
  productivity_contemp_POC_all$BPOC_deposited_seagrass_TgC_season_coast


productivity_contemp_POC_all$BPOC_deposited_mud_TgC_season_coast<-
  (productivity_contemp_POC_all$mean_percent_mud/100)*
  productivity_contemp_POC_all$BPOC_total_TgC_region_season

productivity_contemp_POC_all$sd_BPOC_deposited_mud_TgC_season_coast<-
  sqrt(((productivity_contemp_POC_all$sd_mud/100)/(productivity_contemp_POC_all$mean_percent_mud/100))^2+
         (productivity_contemp_POC_all$sd_BPOC_total_TgC_region_season/productivity_contemp_POC_all$BPOC_total_TgC_region_season)^2)*
  productivity_contemp_POC_all$BPOC_deposited_mud_TgC_season_coast

#RPOC
productivity_contemp_POC_all$RPOC_deposited_seagrass_TgC_season_coast<-
  (productivity_contemp_POC_all$mean_percent_seagrass/100)*
  productivity_contemp_POC_all$RPOC_total_TgC_region_season

productivity_contemp_POC_all$sd_RPOC_deposited_seagrass_TgC_season_coast<-
  sqrt(((productivity_contemp_POC_all$sd_percent_seagrass/100)/(productivity_contemp_POC_all$mean_percent_seagrass/100))^2+
         (productivity_contemp_POC_all$sd_RPOC_total_TgC_region_season/productivity_contemp_POC_all$RPOC_total_TgC_region_season)^2)*
  productivity_contemp_POC_all$RPOC_deposited_seagrass_TgC_season_coast


productivity_contemp_POC_all$RPOC_deposited_mud_TgC_season_coast<-
  (productivity_contemp_POC_all$mean_percent_mud/100)*
  productivity_contemp_POC_all$RPOC_total_TgC_region_season

productivity_contemp_POC_all$sd_RPOC_deposited_mud_TgC_season_coast<-
  sqrt(((productivity_contemp_POC_all$sd_mud/100)/(productivity_contemp_POC_all$mean_percent_mud/100))^2+
         (productivity_contemp_POC_all$sd_RPOC_total_TgC_region_season/productivity_contemp_POC_all$RPOC_total_TgC_region_season)^2)*
  productivity_contemp_POC_all$RPOC_deposited_mud_TgC_season_coast


# Recalculated total based on RPOC deposited in mud and seagrass, plus BPOC and RPOC exported
productivity_contemp_POC_all$POC_sequestered_TgC_season_coast<-
  productivity_contemp_POC_all$RPOC_deposited_mud_TgC_season_coast +
  productivity_contemp_POC_all$RPOC_deposited_seagrass_TgC_season_coast +
  productivity_contemp_POC_all$BPOC_sequestered_TgC_season_coast +
  productivity_contemp_POC_all$RPOC_sequestered_TgC_season_coast 
productivity_contemp_POC_all$sd_POC_sequestered_TgC_season_coast<-
  sqrt(productivity_contemp_POC_all$sd_RPOC_deposited_mud_TgC_season_coast^2 +
       productivity_contemp_POC_all$sd_RPOC_deposited_seagrass_TgC_season_coast^2+
       productivity_contemp_POC_all$sd_BPOC_sequestered_TgC_season_coast^2+
       productivity_contemp_POC_all$sd_RPOC_sequestered_TgC_season_coast^2)

productivity_contemp_POC_all$POC_exported_TgC_season_coast<-
  productivity_contemp_POC_all$BPOC_sequestered_TgC_season_coast +
  productivity_contemp_POC_all$RPOC_sequestered_TgC_season_coast 
productivity_contemp_POC_all$sd_POC_exported_TgC_season_coast<-
  sqrt(productivity_contemp_POC_all$sd_BPOC_sequestered_TgC_season_coast^2+
         productivity_contemp_POC_all$sd_RPOC_sequestered_TgC_season_coast^2)
  

#
productivity_contemp_POC_export_total<-productivity_contemp_POC_all %>%
  group_by(Species) %>% 
  summarise(POC_total_TgC_region_season=sum(POC_total_TgC_region_season),
            sd_POC_total_TgC_region_season=sqrt(sum(sd_POC_total_TgC_region_season^2)),
            POC_total_gC_m2_season=sum(POC_total_gC_m2_season),
            sd_POC_total_gC_m2_season=sqrt(sum(sd_POC_total_gC_m2_season^2)),
            RPOC_total_TgC_region_season=sum(RPOC_total_TgC_region_season),
            sd_RPOC_total_TgC_region_season=sqrt(sum(sd_RPOC_total_TgC_region_season^2)),
            RPOC_total_gC_m2_season=sum(RPOC_total_gC_m2_season),
            sd_RPOC_total_gC_m2_season=sqrt(sum(sd_RPOC_total_gC_m2_season^2)),
            BPOC_total_TgC_region_season=sum(BPOC_total_TgC_region_season),
            sd_BPOC_total_TgC_region_season=sqrt(sum(sd_BPOC_total_TgC_region_season^2)),
            BPOC_total_gC_m2_season=sum(BPOC_total_gC_m2_season),
            sd_BPOC_total_gC_m2_season=sqrt(sum(sd_BPOC_total_gC_m2_season^2)),
            Erosion_TgC_region_season=sum(Erosion_TgC_region_season),
            sd_Erosion_TgC_region_season=sqrt(sum(sd_Erosion_TgC_region_season^2)),
            Dislodgement_TgC_region_season=sum(Dislodgement_TgC_region_season),
            sd_Dislodgement_TgC_region_season=sqrt(sum(sd_Dislodgement_TgC_region_season^2)),
            Export_TgC_region_season=sum(Export_TgC_region_season),
            sd_Export_TgC_region_season=sqrt(sum(sd_Export_TgC_region_season^2)),
            Leaf_fall_TgC_region_season=sum(Leaf_fall_TgC_region_season),
            sd_Leaf_fall_TgC_region_season=sqrt(sum(sd_Leaf_fall_TgC_region_season^2)),
            POC_sequestered_TgC_season_coast=sum(POC_sequestered_TgC_season_coast),
            sd_POC_sequestered_TgC_season_coast=sqrt(sum(sd_POC_sequestered_TgC_season_coast^2)),
            RPOC_sequestered_TgC_season_coast=sum(RPOC_sequestered_TgC_season_coast),
            sd_RPOC_sequestered_TgC_season_coast=sqrt(sum(sd_RPOC_sequestered_TgC_season_coast^2)),
            BPOC_sequestered_TgC_season_coast=sum(BPOC_sequestered_TgC_season_coast),
            sd_BPOC_sequestered_TgC_season_coast=sqrt(sum(sd_BPOC_sequestered_TgC_season_coast^2)),
            POC_exported_TgC_season_coast=sum(POC_exported_TgC_season_coast),
            sd_POC_exported_TgC_season_coast=sqrt(sum(sd_POC_exported_TgC_season_coast^2)),
            mean_POC_particle_export=mean(mean_POC_particle_export),
            sd_POC_particle_export=sqrt(sum(sd_POC_particle_export^2)),
            DOC_frPOC_total_TgC_region_season=sum(DOC_frPOC_total_TgC_region_season),
            sd_DOC_frPOC_total_TgC_region_season=sqrt(sum(sd_DOC_frPOC_total_TgC_region_season^2)),
            DOC_frPOC_total_gC_m2_season=sum(DOC_frPOC_total_gC_m2_season),
            sd_DOC_frPOC_total_gC_m2_season=sqrt(sum(sd_DOC_frPOC_total_gC_m2_season^2)),
            RDOC_frPOC_total_TgC_region_season=sum(RDOC_frPOC_total_TgC_region_season),
            sd_RDOC_frPOC_total_TgC_region_season=sqrt(sum(sd_RDOC_frPOC_total_TgC_region_season^2)),
            RDOC_frPOC_total_gC_m2_season=sum(RDOC_frPOC_total_gC_m2_season),
            sd_RDOC_frPOC_total_gC_m2_season=sqrt(sum(sd_RDOC_frPOC_total_gC_m2_season)),
            BDOC_frPOC_total_TgC_region_season=sum(BDOC_frPOC_total_TgC_region_season),
            sd_BDOC_frPOC_total_TgC_region_season=sqrt(sum(sd_BDOC_frPOC_total_TgC_region_season^2)),
            BDOC_frPOC_total_gC_m2_season=sum(BDOC_frPOC_total_gC_m2_season),
            sd_BDOC_frPOC_total_gC_m2_season=sqrt(sum(sd_BDOC_frPOC_total_gC_m2_season^2)),
            RPOC_deposited_mud_TgC_season_coast=sum(RPOC_deposited_mud_TgC_season_coast),
            sd_RPOC_deposited_mud_TgC_season_coast=sqrt(sum(sd_RPOC_deposited_mud_TgC_season_coast^2)),
            RPOC_deposited_seagrass_TgC_season_coast=sum(RPOC_deposited_seagrass_TgC_season_coast),
            sd_RPOC_deposited_seagrass_TgC_season_coast=sqrt(sum(sd_RPOC_deposited_seagrass_TgC_season_coast^2)),
            BPOC_deposited_mud_TgC_season_coast=sum(BPOC_deposited_mud_TgC_season_coast),
            sd_BPOC_deposited_mud_TgC_season_coast=sqrt(sum(sd_BPOC_deposited_mud_TgC_season_coast^2)),
            BPOC_deposited_seagrass_TgC_season_coast=sum(BPOC_deposited_seagrass_TgC_season_coast),
            sd_BPOC_deposited_seagrass_TgC_season_coast=sqrt(sum(sd_BPOC_deposited_seagrass_TgC_season_coast^2)),
            POC_deposited_mud_TgC_season_coast=sum(POC_deposited_mud_TgC_season_coast),
            sd_POC_deposited_mud_TgC_season_coast=sqrt(sum(sd_POC_deposited_mud_TgC_season_coast^2)),
            POC_deposited_seagrass_TgC_season_coast=sum(POC_deposited_seagrass_TgC_season_coast),
            sd_POC_deposited_seagrass_TgC_season_coast=sqrt(sum(sd_POC_deposited_seagrass_TgC_season_coast^2)))
            
        
            
productivity_contemp_POC_export_total$Season<-"Annual"


productivity_contemp_POC_all_total<-rbind(productivity_contemp_POC_all, productivity_contemp_POC_export_total)
productivity_contemp_POC_all_total$Season<-factor(productivity_contemp_POC_all_total$Season, levels=c("Winter", "Spring", "Summer", "Fall", "Annual"))


#### Budget collation and additional calculations (Grazing) ####

ss_metrics_kelp<-select(standing_stock_kelp_annual_c,
                        Species, 
                        Area_bioregion_m2, 
                        Standing_Stock_Tg_C_bioregion, 
                        sd_Standing_Stock_Tg_C_bioregion) %>% 
  rename(Standing_Stock_Tg_C_bioregion_biomass = Standing_Stock_Tg_C_bioregion,
         sd_Standing_Stock_Tg_C_bioregion_biomass = sd_Standing_Stock_Tg_C_bioregion)

ss_metrics_kelp$Standing_Stock_Tg_C_bioregion_sediments<-NA
ss_metrics_kelp$sd_Standing_Stock_Tg_C_bioregion_sediments<-NA

ss_metrics_biomass_seagrass<-standing_stock_seagrass_annual_c_biomass %>% 
  select(Species,
         Area_bioregion_m2, 
         Standing_Stock_Tg_C_bioregion,
         sd_Standing_Stock_Tg_C_bioregion) %>% 
  rename(Standing_Stock_Tg_C_bioregion_biomass = Standing_Stock_Tg_C_bioregion,
         sd_Standing_Stock_Tg_C_bioregion_biomass = sd_Standing_Stock_Tg_C_bioregion)

ss_metrics_sediment_seagrass<-select(sediment_carbon_c, 
                                    Species, 
                                    Area_bioregion_m2, 
                                    Standing_Stock_Tg_C_bioregion, 
                                    sd_Standing_Stock_Tg_C_bioregion) %>% 
  rename(Standing_Stock_Tg_C_bioregion_sediments = Standing_Stock_Tg_C_bioregion,
         sd_Standing_Stock_Tg_C_bioregion_sediments = sd_Standing_Stock_Tg_C_bioregion)

ss_metrics_seagrass<-left_join(ss_metrics_biomass_seagrass, ss_metrics_sediment_seagrass)

ss_metrics<-rbind(ss_metrics_kelp, ss_metrics_seagrass)
ss_metrics$Season<-"Annual"

NPP_metrics<-select(productivity_contemp_summary_all,
                    Species,
                    Season,
                    NPP_TgC_region_season,
                    sd_NPP_TgC_region_season,
                    NPP_gC_m2_season,
                    sd_NPP_gC_m2_season)

DOC_prod_metrics<-select(productivity_contemp_summary_all,
                         Species,
                         Season,
                         DOC_prod_TgC_region_season,
                         sd_DOC_prod_TgC_region_season,
                         BDOC_prod_TgC_region_season,
                         sd_BDOC_prod_TgC_region_season,
                         DOC_prod_mgC_m2_season,
                         sd_DOC_prod_mgC_m2_season)


DOC_prod_metrics<-DOC_prod_metrics[!(DOC_prod_metrics$Season=="Annual"),]


DOC_prod_metrics_total<-DOC_prod_metrics %>%
  group_by(Species) %>% 
  summarise(DOC_prod_TgC_region_season=sum(DOC_prod_TgC_region_season, na.rm=TRUE), 
            sd_DOC_prod_TgC_region_season=sqrt(sum(sd_DOC_prod_TgC_region_season^2, na.rm=TRUE)),
            BDOC_prod_TgC_region_season=sum(BDOC_prod_TgC_region_season, na.rm=TRUE), 
            sd_BDOC_prod_TgC_region_season=sqrt(sum(sd_BDOC_prod_TgC_region_season^2, na.rm=TRUE)),
            DOC_prod_mgC_m2_season=sum(DOC_prod_mgC_m2_season, na.rm=TRUE), 
            sd_DOC_prod_mgC_m2_season=sqrt(sum(sd_DOC_prod_mgC_m2_season^2, na.rm=TRUE)))

DOC_prod_metrics_total$Season<-"Annual"
DOC_prod_metrics<-rbind(DOC_prod_metrics, DOC_prod_metrics_total)

DOC_particle_export_total$Species_group<-NA
DOC_particle_export_total$Species_group[(DOC_particle_export_total$Species=="Laminaria digitata")]<-"Kelp"
DOC_particle_export_total$Species_group[(DOC_particle_export_total$Species=="Saccharina latissima")]<-"Kelp"
DOC_particle_export_total$Species_group[(DOC_particle_export_total$Species=="Zostera marina")]<-"Seagrass"

DOC_particle_export_species_total<-DOC_particle_export_total %>% 
  group_by(Species_group) %>% 
  summarise(mean_DOC_particle_export=sum(mean_DOC_particle_export),
            sd_DOC_particle_export=sqrt(sum(sd_DOC_particle_export^2)))
            

NPP_DOC_prod_metrics<-left_join(NPP_metrics, DOC_prod_metrics)

POC_prod_seq_metrics<-select(productivity_contemp_POC_all_total,
                             Species,
                             Season,
                             Erosion_TgC_region_season,
                             sd_Erosion_TgC_region_season,
                             Dislodgement_TgC_region_season,
                             sd_Dislodgement_TgC_region_season,
                             Export_TgC_region_season,
                             sd_Export_TgC_region_season,
                             Leaf_fall_TgC_region_season,
                             sd_Leaf_fall_TgC_region_season,
                             POC_total_TgC_region_season,
                             sd_POC_total_TgC_region_season,
                             POC_deposited_seagrass_TgC_season_coast,
                             sd_POC_deposited_seagrass_TgC_season_coast,
                             POC_deposited_mud_TgC_season_coast,
                             sd_POC_deposited_mud_TgC_season_coast,
                             RPOC_deposited_seagrass_TgC_season_coast,
                             sd_RPOC_deposited_seagrass_TgC_season_coast,
                             RPOC_deposited_mud_TgC_season_coast,
                             sd_RPOC_deposited_mud_TgC_season_coast,
                             BPOC_deposited_seagrass_TgC_season_coast,
                             sd_BPOC_deposited_seagrass_TgC_season_coast,
                             BPOC_deposited_mud_TgC_season_coast,
                             sd_BPOC_deposited_mud_TgC_season_coast,
                             POC_sequestered_TgC_season_coast,
                             sd_POC_sequestered_TgC_season_coast,
                             RPOC_total_TgC_region_season,
                             sd_RPOC_total_TgC_region_season,
                             RPOC_sequestered_TgC_season_coast,
                             sd_RPOC_sequestered_TgC_season_coast,
                             BPOC_total_TgC_region_season,
                             sd_BPOC_total_TgC_region_season,
                             BPOC_sequestered_TgC_season_coast,
                             sd_BPOC_sequestered_TgC_season_coast,
                             RDOC_frPOC_total_TgC_region_season,
                             sd_RDOC_frPOC_total_TgC_region_season,
                             DOC_frPOC_total_TgC_region_season,
                             sd_DOC_frPOC_total_TgC_region_season,
                             BDOC_frPOC_total_TgC_region_season,
                             sd_BDOC_frPOC_total_TgC_region_season)

#POC_prod_seq_metrics[is.na(POC_prod_seq_metrics)]<-0

DOC_seq_metrics<-select(DOC_seq_all_summary,
                        Species,
                        Season,
                        BDOC_sequestered_TgC_season_coast,
                        sd_BDOC_sequestered_TgC_season_coast,
                        RDOC_prod_TgC_region_season,
                        sd_RDOC_prod_TgC_region_season,
                        TDOC_sequestered_TgC_season_coast,
                        sd_TDOC_sequestered_TgC_season_coast)

DOC_frPOC_seq_metrics<-select(DOC_frPOC_seq_all_summary,
                        Species,
                        Season,
                        BDOC_frPOC_sequestered_TgC_season_coast,
                        sd_BDOC_frPOC_sequestered_TgC_season_coast,
                        RDOC_frPOC_total_TgC_region_season,
                        sd_RDOC_frPOC_total_TgC_region_season,
                        TDOC_frPOC_sequestered_TgC_season_coast,
                        sd_TDOC_frPOC_sequestered_TgC_season_coast)


budget_all_metrics<-left_join(NPP_DOC_prod_metrics, ss_metrics)

budget_all_metrics<-left_join(budget_all_metrics, POC_prod_seq_metrics)

budget_all_metrics<-left_join(budget_all_metrics, DOC_seq_metrics)

budget_all_metrics<-left_join(budget_all_metrics, DOC_frPOC_seq_metrics)

budget_all_metrics<-left_join(budget_all_metrics, POC_particle_export_summary)

budget_all_metrics<-left_join(budget_all_metrics, DOC_particle_export_summary)


t<- DOC_frPOC_seq_metrics[(DOC_frPOC_seq_metrics$Season=="Annual"),] 
budget_all_metrics<-select(budget_all_metrics, -BDOC_frPOC_sequestered_TgC_season_coast,
                           -sd_BDOC_frPOC_sequestered_TgC_season_coast,
                           -RDOC_frPOC_total_TgC_region_season,
                           -sd_RDOC_frPOC_total_TgC_region_season,
                           -TDOC_frPOC_sequestered_TgC_season_coast,
                           -sd_TDOC_frPOC_sequestered_TgC_season_coast)

budget_all_metrics_annual_summary<- budget_all_metrics[(budget_all_metrics$Season=="Annual"),]

budget_all_metrics_annual_summary<-left_join(budget_all_metrics_annual_summary, t)

budget_all_metrics_annual_summary<-budget_all_metrics_annual_summary %>% 
  group_by(Species) %>% 
  mutate(total_sequestered_POC_export_TgC_region = RPOC_sequestered_TgC_season_coast+BPOC_sequestered_TgC_season_coast) %>%  
  mutate(sd_total_sequestered_POC_export_TgC_region = sqrt(sd_RPOC_sequestered_TgC_season_coast^2 + sd_BPOC_sequestered_TgC_season_coast^2)) %>%  
  mutate(total_sequestered_DOC_only_TgC_region = TDOC_sequestered_TgC_season_coast+TDOC_frPOC_sequestered_TgC_season_coast) %>%  
  mutate(sd_total_sequestered_DOC_only_TgC_region = sqrt(sd_TDOC_sequestered_TgC_season_coast^2 +  sd_TDOC_frPOC_sequestered_TgC_season_coast^2)) 


budget_all_metrics_annual_summary$Species_group<-NA

budget_all_metrics_annual_summary$Species_group[(budget_all_metrics_annual_summary$Species=="Laminaria digitata")]<-"Kelp"

budget_all_metrics_annual_summary$Species_group[(budget_all_metrics_annual_summary$Species=="Saccharina latissima")]<-"Kelp"

budget_all_metrics_annual_summary$Species_group[(budget_all_metrics_annual_summary$Species=="Zostera marina")]<-"Seagrass"


budget_all_metrics_annual_summary_species_totals<-budget_all_metrics_annual_summary %>% 
  group_by(Species_group) %>% 
  summarise(NPP_TgC_region_season=sum(NPP_TgC_region_season),
            sd_NPP_TgC_region_season=sqrt(sum(sd_NPP_TgC_region_season^2)),
            NPP_gC_m2_season=sum(NPP_gC_m2_season),
            sd_NPP_gC_m2_season=sqrt(sum(sd_NPP_gC_m2_season^2)),
            DOC_prod_TgC_region_season=sum(DOC_prod_TgC_region_season),
            sd_DOC_prod_TgC_region_season=sqrt(sum(sd_DOC_prod_TgC_region_season^2)),
            DOC_prod_mgC_m2_season=sum(DOC_prod_mgC_m2_season),
            sd_DOC_prod_mgC_m2_season=sqrt(sum(sd_DOC_prod_mgC_m2_season^2)),
            BDOC_prod_TgC_region_season=sum(BDOC_prod_TgC_region_season),
            sd_BDOC_prod_TgC_region_season=sqrt(sum(sd_BDOC_prod_TgC_region_season^2)),
            Standing_Stock_Tg_C_bioregion_biomass=sum(Standing_Stock_Tg_C_bioregion_biomass),
            sd_Standing_Stock_Tg_C_bioregion_biomass=sqrt(sum(sd_Standing_Stock_Tg_C_bioregion_biomass^2)),
            Standing_Stock_Tg_C_bioregion_sediments=sum(Standing_Stock_Tg_C_bioregion_sediments),
            sd_Standing_Stock_Tg_C_bioregion_sediments=sqrt(sum(sd_Standing_Stock_Tg_C_bioregion_sediments^2)),
            Erosion_TgC_region_season=sum(Erosion_TgC_region_season),
            sd_Erosion_TgC_region_season=sqrt(sum(sd_Erosion_TgC_region_season^2)),
            Dislodgement_TgC_region_season=sum(Dislodgement_TgC_region_season),
            sd_Dislodgement_TgC_region_season=sqrt(sum(sd_Dislodgement_TgC_region_season^2)),
            Export_TgC_region_season=sum(Export_TgC_region_season),
            sd_Export_TgC_region_season=sqrt(sum(sd_Export_TgC_region_season^2)),
            Leaf_fall_TgC_region_season=sum(Leaf_fall_TgC_region_season),
            sd_Leaf_fall_TgC_region_season=sqrt(sum(sd_Leaf_fall_TgC_region_season^2)),
            POC_total_TgC_region_season=sum(POC_total_TgC_region_season),
            sd_POC_total_TgC_region_season=sqrt(sum(sd_POC_total_TgC_region_season^2)),
            POC_sequestered_TgC_season_coast=sum(POC_sequestered_TgC_season_coast),
            sd_POC_sequestered_TgC_season_coast=sqrt(sum(sd_POC_sequestered_TgC_season_coast^2)),
            RPOC_total_TgC_region_season=sum(RPOC_total_TgC_region_season),
            sd_RPOC_total_TgC_region_season=sqrt(sum(sd_RPOC_total_TgC_region_season^2)),
            RPOC_sequestered_TgC_season_coast=sum(RPOC_sequestered_TgC_season_coast),
            sd_RPOC_sequestered_TgC_season_coast=sqrt(sum(sd_RPOC_sequestered_TgC_season_coast^2)),
            BPOC_total_TgC_region_season=sum(BPOC_total_TgC_region_season),
            sd_BPOC_total_TgC_region_season=sqrt(sum(sd_BPOC_total_TgC_region_season^2)),
            BPOC_sequestered_TgC_season_coast=sum(BPOC_sequestered_TgC_season_coast),
            sd_BPOC_sequestered_TgC_season_coast=sqrt(sum(sd_BPOC_sequestered_TgC_season_coast^2)),
            RPOC_deposited_seagrass_TgC_season_coast=sum(RPOC_deposited_seagrass_TgC_season_coast),
            sd_RPOC_deposited_seagrass_TgC_season_coast=sqrt(sum(sd_RPOC_deposited_seagrass_TgC_season_coast^2)),
            RPOC_deposited_mud_TgC_season_coast=sum(RPOC_deposited_mud_TgC_season_coast),
            sd_RPOC_deposited_mud_TgC_season_coast=sqrt(sum(sd_RPOC_deposited_mud_TgC_season_coast^2)),
            BPOC_deposited_seagrass_TgC_season_coast=sum(BPOC_deposited_seagrass_TgC_season_coast),
            sd_BPOC_deposited_seagrass_TgC_season_coast=sqrt(sum(sd_BPOC_deposited_seagrass_TgC_season_coast^2)),
            BPOC_deposited_mud_TgC_season_coast=sum(BPOC_deposited_mud_TgC_season_coast),
            sd_BPOC_deposited_mud_TgC_season_coast=sqrt(sum(sd_BPOC_deposited_mud_TgC_season_coast^2)),
            POC_deposited_seagrass_TgC_season_coast=sum(POC_deposited_seagrass_TgC_season_coast),
            sd_POC_deposited_seagrass_TgC_season_coast=sqrt(sum(sd_POC_deposited_seagrass_TgC_season_coast^2)),
            POC_deposited_mud_TgC_season_coast=sum(POC_deposited_mud_TgC_season_coast),
            sd_POC_deposited_mud_TgC_season_coast=sqrt(sum(sd_POC_deposited_mud_TgC_season_coast^2)),
            BDOC_sequestered_TgC_season_coast=sum(BDOC_sequestered_TgC_season_coast),
            sd_BDOC_sequestered_TgC_season_coast=sqrt(sum(sd_BDOC_sequestered_TgC_season_coast^2)),
            RDOC_prod_TgC_region_season=sum(RDOC_prod_TgC_region_season),
            sd_RDOC_prod_TgC_region_season=sqrt(sum(sd_RDOC_prod_TgC_region_season^2)),
            TDOC_sequestered_TgC_season_coast=sum(TDOC_sequestered_TgC_season_coast),
            sd_TDOC_sequestered_TgC_season_coast=sqrt(sum(sd_TDOC_sequestered_TgC_season_coast^2)),
            BDOC_frPOC_sequestered_TgC_season_coast=sum(BDOC_frPOC_sequestered_TgC_season_coast),
            sd_BDOC_frPOC_sequestered_TgC_season_coast=sqrt(sum(sd_BDOC_frPOC_sequestered_TgC_season_coast^2)),
            RDOC_frPOC_total_TgC_region_season=sum(RDOC_frPOC_total_TgC_region_season),
            sd_RDOC_frPOC_total_TgC_region_season=sqrt(sum(sd_RDOC_frPOC_total_TgC_region_season^2)),
            DOC_frPOC_total_TgC_region_season=sum(DOC_frPOC_total_TgC_region_season),
            sd_DOC_frPOC_total_TgC_region_season=sqrt(sum(sd_DOC_frPOC_total_TgC_region_season^2)),
            BDOC_frPOC_total_TgC_region_season=sum(BDOC_frPOC_total_TgC_region_season),
            sd_BDOC_frPOC_total_TgC_region_season=sqrt(sum(sd_BDOC_frPOC_total_TgC_region_season^2)),
            TDOC_frPOC_sequestered_TgC_season_coast=sum(TDOC_frPOC_sequestered_TgC_season_coast),
            sd_TDOC_frPOC_sequestered_TgC_season_coast=sqrt(sum(sd_TDOC_frPOC_sequestered_TgC_season_coast^2)),
            total_sequestered_POC_export_TgC_region=sum(total_sequestered_POC_export_TgC_region),
            sd_total_sequestered_POC_export_TgC_region=sqrt(sum(sd_total_sequestered_POC_export_TgC_region^2)),
            total_sequestered_DOC_only_TgC_region=sum(total_sequestered_DOC_only_TgC_region),
            sd_total_sequestered_DOC_only_TgC_region=sqrt(sum(sd_total_sequestered_DOC_only_TgC_region^2))
            )

budget_all_metrics_annual_summary_species_totals$BDOC_remineralized_TgC_season_coast<-
  budget_all_metrics_annual_summary_species_totals$BDOC_prod_TgC_region_season-
  budget_all_metrics_annual_summary_species_totals$BDOC_sequestered_TgC_season_coast

budget_all_metrics_annual_summary_species_totals$sd_BDOC_remineralized_TgC_season_coast<-
  sqrt(budget_all_metrics_annual_summary_species_totals$sd_BDOC_prod_TgC_region_season^2+
  budget_all_metrics_annual_summary_species_totals$sd_BDOC_sequestered_TgC_season_coast^2)


## Grazing ##

grazing_kelp<-read.csv("C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Carbon Budget/Krumhansl 2012 Data/Kelp growth and erosion.csv")

grazing_kelp<-grazing_kelp %>%
  summarise(mean_grazed_area=mean(X..Distal.Grazed, na.rm = TRUE),
            sd_grazed_area=sd(X..Distal.Grazed, na.rm = TRUE))

grazing_kelp$Species_group<-"Kelp"

grazing_zostera <- data.frame(Species_group="Seagrass",
                                 mean_grazed_area=0.075,
                              sd_grazed_area=0) # from Nienhuis & Groenendijk 1986, No SD given so setting to 0 so that calculation doesn't include it

grazing<-rbind(grazing_kelp, grazing_zostera)

budget_all_metrics_annual_summary_species_totals<-left_join(budget_all_metrics_annual_summary_species_totals, grazing)

budget_all_metrics_annual_summary_species_totals$Grazed_NPP_TgC_region_season<-
  budget_all_metrics_annual_summary_species_totals$NPP_TgC_region_season*
  budget_all_metrics_annual_summary_species_totals$mean_grazed_area

budget_all_metrics_annual_summary_species_totals$sd_Grazed_NPP_TgC_region_season<-
  sqrt((budget_all_metrics_annual_summary_species_totals$sd_grazed_area/budget_all_metrics_annual_summary_species_totals$mean_grazed_area)^2 +
         (budget_all_metrics_annual_summary_species_totals$sd_NPP_TgC_region_season/budget_all_metrics_annual_summary_species_totals$NPP_TgC_region_season)^2)*
  budget_all_metrics_annual_summary_species_totals$Grazed_NPP_TgC_region_season


#BDOC from POC remineralization

budget_all_metrics_annual_summary_species_totals$BDOC_frPOC_remineralized_TgC_season_coast<-
  budget_all_metrics_annual_summary_species_totals$BDOC_frPOC_total_TgC_region_season-
  budget_all_metrics_annual_summary_species_totals$BDOC_frPOC_sequestered_TgC_season_coast

budget_all_metrics_annual_summary_species_totals$sd_BDOC_frPOC_remineralized_TgC_season_coast<-sqrt(
  budget_all_metrics_annual_summary_species_totals$sd_BDOC_frPOC_total_TgC_region_season^2+
  budget_all_metrics_annual_summary_species_totals$sd_BDOC_frPOC_sequestered_TgC_season_coast^2)

#Burial
budget_all_metrics_annual_summary_species_totals$Burial_rate_gC_m2_y<-NA
budget_all_metrics_annual_summary_species_totals$sd_Burial_rate_gC_m2_y<-NA

budget_all_metrics_annual_summary_species_totals$Burial_rate_gC_m2_y[(budget_all_metrics_annual_summary_species_totals$Species_group=="Seagrass")]<-31.7 #Novak et al. 2020
budget_all_metrics_annual_summary_species_totals$sd_Burial_rate_gC_m2_y[(budget_all_metrics_annual_summary_species_totals$Species_group=="Seagrass")]<-7.38 #Novak et al. 2020

budget_all_metrics_annual_summary_species_totals$Burial_rate_TgC_region_year<-(budget_all_metrics_annual_summary_species_totals$Burial_rate_gC_m2_y*239780000)/1000000000000
budget_all_metrics_annual_summary_species_totals$sd_Burial_rate_TgC_region_year<-(budget_all_metrics_annual_summary_species_totals$sd_Burial_rate_gC_m2_y*239780000)/1000000000000

#POC Buried Seagrass


budget_all_metrics_annual_summary_species_totals<-budget_all_metrics_annual_summary_species_totals %>% 
  mutate(inSeagrass_Burial_rate_TgC_region_year = if_else(Species_group == "Seagrass",
                                                          Burial_rate_TgC_region_year*0.23,
                                                          0.0076*0.355)) %>% # macroalgal carbon burial rate in seagrass from Rohr et al. 2018 multipled by total carbon burial rate in seagrass
  mutate(sd_inSeagrass_Burial_rate_TgC_region_year = if_else(Species_group == "Seagrass",
                                                          sqrt((sd_Burial_rate_TgC_region_year/Burial_rate_TgC_region_year)^2+ 
                                                                 (0.088/0.23)^2)*inSeagrass_Burial_rate_TgC_region_year,
                                                          sqrt((0.0017/0.0076)^2+ 
                                                                 (0.332/0.355)^2)*inSeagrass_Burial_rate_TgC_region_year)) #sd macroalgal carbon burial rate in seagrass from Rohr et al. 2018 (sd 12,59)


budget_all_metrics_annual_summary_species_totals$Allochthonous_carbon_burial_TgC_season_coast<-
  budget_all_metrics_annual_summary_species_totals$Burial_rate_TgC_region_year-
  budget_all_metrics_annual_summary_species_totals$inSeagrass_Burial_rate_TgC_region_year

budget_all_metrics_annual_summary_species_totals$sd_Allochthonous_carbon_burial_TgC_season_coast<-sqrt(
  budget_all_metrics_annual_summary_species_totals$sd_Burial_rate_TgC_region_year^2+
  budget_all_metrics_annual_summary_species_totals$sd_inSeagrass_Burial_rate_TgC_region_year^2)

budget_all_metrics_annual_summary_species_totals$Autochthonous_carbon_burial_TgC_season_coast<-
  budget_all_metrics_annual_summary_species_totals$Burial_rate_TgC_region_year-
  budget_all_metrics_annual_summary_species_totals$Allochthonous_carbon_burial_TgC_season_coast

budget_all_metrics_annual_summary_species_totals$sd_Autochthonous_carbon_burial_TgC_season_coast<-sqrt(
  budget_all_metrics_annual_summary_species_totals$sd_Burial_rate_TgC_region_year^2+
    budget_all_metrics_annual_summary_species_totals$sd_Allochthonous_carbon_burial_TgC_season_coast^2)


#Totals with POC burial in seagrass
#
budget_all_metrics_annual_summary_species_totals<- budget_all_metrics_annual_summary_species_totals %>% 
  mutate(total_sequestered_all_POC_TgC_region = total_sequestered_POC_export_TgC_region +
                                                 inSeagrass_Burial_rate_TgC_region_year +
                                                 RPOC_deposited_mud_TgC_season_coast) %>% 
  mutate(sd_total_sequestered_all_POC_TgC_region =  sqrt(sd_total_sequestered_POC_export_TgC_region^2 +
                                                         sd_inSeagrass_Burial_rate_TgC_region_year^2 +
                                                         sd_RPOC_deposited_mud_TgC_season_coast^2)) %>% 
  mutate(total_sequestered_TgC_region = ifelse(Species_group == "Seagrass", 
                                                                  total_sequestered_POC_export_TgC_region +
                                                                  total_sequestered_DOC_only_TgC_region +
                                                                  inSeagrass_Burial_rate_TgC_region_year +
                                                                  RPOC_deposited_mud_TgC_season_coast +
                                                                  (Allochthonous_carbon_burial_TgC_season_coast-0.0017), #0.0017 is refractory kelp export to seagrass, already accounted for in RPOC
                                                                  total_sequestered_POC_export_TgC_region +
                                                                  total_sequestered_DOC_only_TgC_region +
                                                                  inSeagrass_Burial_rate_TgC_region_year +
                                                                  RPOC_deposited_mud_TgC_season_coast)) %>% 
  mutate(sd_total_sequestered_TgC_region = ifelse(Species_group == "Seagrass", 
                                                                  sqrt(sd_total_sequestered_POC_export_TgC_region^2 +
                                                                       sd_total_sequestered_DOC_only_TgC_region^2 +
                                                                       sd_inSeagrass_Burial_rate_TgC_region_year^2 +
                                                                       sd_RPOC_deposited_mud_TgC_season_coast^2 + 
                                                                    ((sqrt(sd_Allochthonous_carbon_burial_TgC_season_coast^2+0.003^2))^2)),
                                                                  sqrt(sd_total_sequestered_POC_export_TgC_region^2 +
                                                                        sd_total_sequestered_DOC_only_TgC_region^2 +
                                                                        sd_inSeagrass_Burial_rate_TgC_region_year^2 +
                                                                        sd_RPOC_deposited_mud_TgC_season_coast^2)))

budget_all_metrics_annual_summary_species_totals<-budget_all_metrics_annual_summary_species_totals %>% 
  mutate(total_sequestered_TgC_m2= ifelse(Species_group== "Seagrass",
                                          total_sequestered_TgC_region/seagrass_area_bioregion_m2,
                                          total_sequestered_TgC_region/(sum(kelp_area$Kelp_area_bioregion_km2)*1000000))) %>% 
  mutate(sd_total_sequestered_TgC_m2= ifelse(Species_group== "Seagrass",
                                                             sd_total_sequestered_TgC_region/seagrass_area_bioregion_m2,
                                                             sd_total_sequestered_TgC_region/(sum(kelp_area$Kelp_area_bioregion_km2)*1000000)))

budget_all_metrics_annual_summary_species_totals$total_sequestered_gC_m2<-
  budget_all_metrics_annual_summary_species_totals$total_sequestered_TgC_m2*1000000000000

budget_all_metrics_annual_summary_species_totals$sd_total_sequestered_gC_m2<-
  budget_all_metrics_annual_summary_species_totals$sd_total_sequestered_TgC_m2*1000000000000


budget_all_metrics_annual_summary_species_totals$Total_export_to_shelf_TgC_region_year<-
  budget_all_metrics_annual_summary_species_totals$RPOC_sequestered_TgC_season_coast+
  budget_all_metrics_annual_summary_species_totals$BPOC_sequestered_TgC_season_coast+
  budget_all_metrics_annual_summary_species_totals$BDOC_sequestered_TgC_season_coast+
  budget_all_metrics_annual_summary_species_totals$BDOC_frPOC_sequestered_TgC_season_coast

budget_all_metrics_annual_summary_species_totals$sd_Total_export_to_shelf_TgC_region_year<-sqrt(
  budget_all_metrics_annual_summary_species_totals$sd_RPOC_sequestered_TgC_season_coast^2+
  budget_all_metrics_annual_summary_species_totals$sd_BPOC_sequestered_TgC_season_coast^2+
  budget_all_metrics_annual_summary_species_totals$sd_BDOC_sequestered_TgC_season_coast^2+
  budget_all_metrics_annual_summary_species_totals$sd_BDOC_frPOC_sequestered_TgC_season_coast^2)



write.csv(budget_all_metrics_annual_summary_species_totals,"C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Carbon Budget/budget_all_metrics_species_group_totals.csv")

#Calculating percents


# Remove columns that start with "sd_"
budget_all_metrics_annual_summary_species_totals_sd <- budget_all_metrics_annual_summary_species_totals %>%
  select(starts_with("sd_"))

budget_all_metrics_annual_summary_species_totals_metrics <- budget_all_metrics_annual_summary_species_totals %>%
  select(-starts_with("sd_"), -Species_group)

divide_by_column <- function(df, divisor_column_name) {
  
  # Get the divisor column
  divisor <- df[[divisor_column_name]]
  
  # Divide all columns by the divisor column, except the divisor column itself
  df_divided <- df %>%
    mutate(across(.cols = everything(), 
                  .fns = ~ . / divisor, 
                  .names = "{.col}_percent")) %>%
    dplyr::select(-divisor_column_name)  # Remove the original divisor column
  
  return(df_divided)
}


budget_all_metrics_annual_summary_species_percents <- divide_by_column(
  budget_all_metrics_annual_summary_species_totals_metrics, 
  "NPP_TgC_region_season")

budget_all_metrics_annual_summary_species_percents<-cbind(
  budget_all_metrics_annual_summary_species_percents, 
  budget_all_metrics_annual_summary_species_totals_metrics, 
  budget_all_metrics_annual_summary_species_totals_sd)

###

divide_and_calculate_sd <- function(df, divisor_column_name) {
  # Get the divisor column
  divisor <- df[[divisor_column_name]]
  
  # Handle NA or 0 in the divisor column by replacing them with 1
  divisor[is.na(divisor) | divisor == 0] <- 1
  
  # Create a copy of the dataframe and divide all columns by the divisor column, except the divisor column itself
  df_divided <- df %>%
    mutate(across(.cols = everything(), 
                  .fns = ~ . / divisor, 
                  .names = "{.col}_percent"))
  
  # Loop through each column in the dataframe (excluding the original divisor column)
  for (col in setdiff(colnames(df), divisor_column_name)) {
    # Check if the ratio column exists
    ratio_col_name <- paste0(col, "_percent")
    if (!(ratio_col_name %in% colnames(df_divided))) {
      next
    }
    
    # Calculate the ratio for each column (avoid division by 0 or NA)
    ratio <- df_divided[[ratio_col_name]]
    
    # Check for missing values in the ratio column and handle them
    if (all(is.na(ratio))) {
      warning(paste("All values are NA for the ratio in column", col))
      next
    }
    
    # Ensure standard deviation column exists in the original dataframe
    sd_col_name <- paste0("sd_", col)
    if (!(sd_col_name %in% colnames(df))) {
      warning(paste("Standard deviation column for", col, "is missing"))
      next
    }
    
    # Calculate the standard deviation of the ratio
    df_divided[[paste0("sd_", col, "_percent")]] <- sqrt(
      (df[[sd_col_name]] / df[[col]])^2 + 
        (df[[paste0("sd_", divisor_column_name)]] / df[[divisor_column_name]])^2
    ) * ratio
  }
  
  return(df_divided)
}

budget_all_metrics_annual_summary_species_totals_r<-select(budget_all_metrics_annual_summary_species_totals, -Species_group)
budget_all_metrics_annual_summary_species_percents <- divide_and_calculate_sd(
  budget_all_metrics_annual_summary_species_totals_r, 
  "NPP_TgC_region_season"
)


budget_all_metrics_annual_summary_species_percents_c <- budget_all_metrics_annual_summary_species_percents %>%
  select(ends_with("_percent"))

write.csv(budget_all_metrics_annual_summary_species_percents_c,"C:/Users/KrumhanslK/Documents/AIS Group/Kelp Seagrass Blue Carbon/Carbon Budget/budget_all_metrics_annual_summary_species_percents.csv")

#Shelf burial rates total carbon
#GoM - 1.6mmol C m-2d--1 = 0.019216 g C m-2 d-1 = 7.013 g C m-2 y-1 
#0.48 Mol C/m2/yr = 5.76 gC m-2 y-1 Silverberg et al. 2000
#assuming 94193000000 m2 domain, this is 542551680000 g C shelf-1 y-1 = 0.54 Tg C shelf -1 y-1
#M. Kienast dispersal domain = 128,487,000,000 m2, 740085120000 g C shelf-1 y-1 = 0.741

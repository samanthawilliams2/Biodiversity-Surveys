library(lubridate)
library(tidyverse)
library(ggplot2)


##### coral data - 2019 #####
coral_data = read.csv("data/ClassDataset_2019_coral.csv", strip.white = T,  header = F)
head(coral_data)
coral_data=t(coral_data) # transpose the data
coral_data = coral_data[-1,] # remove row 1
colnames(coral_data) <-coral_data[1,] # assign column names
coral_data=as.data.frame(coral_data[-1:-2,-3:-4]) # remove rows and columns that won't be used
head(coral_data)

coral_data = coral_data %>%
  mutate(Site=factor(Site), # define site as a factor
         Date = dmy(paste0(Date, "-2019")), # define date as a date
         Replicate=factor(Replicate))

levels(coral_data$Site) # there are many errors in site name spelling. These must be fixed
coral_data.1 = coral_data %>%
  mutate(
    Site = fct_collapse(Site, # collapse the levels of Site 
                        "Cattle Bay" = c("Cattle Bay", "Cattle bay"),
                        "Clam Gardens" = c("Clam Gard.", "Clam Garden", "Clam Gardens"),
                        "Hazard Bay" = c("Hazard Bay", "Hazard"),
                        "Iris Point" = c("Ires Point", "Iris", "Iris Point", "Iris Pt", "Iris point"),
                        "Little Pioneer Bay South" = c("Little Pioneer Bay South", "Little Pioneer bay South", "Pioneer Bay"),
                        "Snapper Point" = c("Snapper point", "Snapper point", "Snapper Pont"),
                        "Southwest Pelorus" = c("South West Pelorus", "Southwest Pelorus vist #2", "SW Peloris", "SW Pelorus", "SW Pelorus visit #2")
    ))
levels(coral_data.1$Site) # check for duplicates

# each row is a transect. Get a count of replicate transects per site
reps.per.site.coral = coral_data.1 %>%
  group_by(Site) %>%
  summarise(nTransects = length(Site))
reps.per.site.coral

# make a figure
head(coral_data.1)

# group into benthic categories
colnames(coral_data.1)

taxa_groups <- rep(NA, ncol(coral_data.1))
taxa_groups[4:36] <- "Hard coral" # columns 4-36 are coral
taxa_groups[37:45] <- "Soft Corals"
taxa_groups[46:51] <- "Macroalgae"
taxa_groups[52:54] <- "Hard coral"
taxa_groups[55:56] <- "Soft Corals"
taxa_groups[57] <- "Hard coral"
taxa_groups[58] <- "Other" 
taxa_groups[59:61] <- "Hard coral"
taxa_groups[62:64] <- "Soft Corals"

# Name the vector with the actual column names
names(taxa_groups) <- colnames(coral_data.1)
taxon_cols <- names(taxa_groups)[!is.na(taxa_groups)]

df_long <- coral_data.1 %>%
  pivot_longer(cols = all_of(taxon_cols), names_to = "taxon", values_to = "value") %>%
  mutate(group = taxa_groups[taxon])

df_long = df_long %>%
  mutate(group=factor(group),
         value=as.numeric(value)) %>%
  group_by(Site,group,Replicate) %>%
  summarise(total = sum(value, na.rm = TRUE)) #%>%
  #pivot_wider(names_from = group, values_from = total) # run this line if you want to re-convert to wide format

# take a mean of the replicate transects
# make a function for standard error
std<- function(x) sd(x)/sqrt(length(x))

coral.mean = df_long %>%
  group_by(Site,group) %>%
  summarise(meanAbund= mean(total/10), # divide by 10 to get per m
            SE=std(total/10))
 
ggplot(coral.mean, aes(x = Site, y = meanAbund, fill = group)) + 
  geom_col() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(expression("Mean number of individuals "*m^-2*""))

ggplot(coral.mean, aes(x=Site,y=meanAbund, color=group)) + 
  geom_pointrange(aes(ymin=meanAbund-SE, ymax=meanAbund+SE),position=position_dodge(width=0.5)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))+
  scale_y_continuous(expression("Mean number of individuals "*m^-2*"" * "\u00B1" * "SE"))

##### coral data - 2025 #####
coral_2025 = read.csv("data/ClassData_CoralAbundance.csv", strip.white = T, header = T)
head(coral_2025)

# Replace column names with the first row of data
colnames(coral_2025) <- as.character(coral_2025[1, ])

# Remove that first row (since it’s now the column names)
coral_2025 <- coral_2025[-1, ]

# Reset row names
rownames(coral_2025) <- NULL

coral_2025 = coral_2025 %>%
  mutate(BuddyPair=factor(BuddyPair),
         Year=as.numeric(Year),
         Zone=factor(Zone),
         Site=factor(Site),
         Transect=factor(Transect),
         Type=factor(Type),
         Genus=factor(Genus),
         HealthStatus=factor(HealthStatus),
         Count=as.numeric(Count))

coral_2025 = coral_2025 %>%
  filter(BuddyPair != "Taylor_Gemma") # remove the mock data 

# get the data in the same format and then merge
head(coral_2025)
coral_2019 <- coral_data.1 %>%
  pivot_longer(cols = all_of(taxon_cols), names_to = "taxon", values_to = "value") %>%
  mutate(group = taxa_groups[taxon])
head(coral_2019)

coral_2019 = coral_2019 %>%
  mutate(Year = as.numeric(2019),
         Transect = Replicate,
         Genus = taxon,
         HealthStatus="Healthy",
         Count = as.numeric(value),
         Type = group)

head(coral_2019)
head(coral_2025)
coral_2019_2025 = full_join(coral_2025[,-1], coral_2019)
coral_2019_2025 = coral_2019_2025[,1:8] %>%
  mutate(Site=factor(Site))
head(coral_2019_2025) # you can use this merged dataset for analysis - note you may need to rename sites to match between years

coral_2019_2025 = coral_2019_2025 %>%
  mutate(Count = replace_na(Count, 0)) # remove NA values 

# think i need to change little pioneer in 2019 to pioneer



##### fish data from SPCs #####

fish_data = read.csv("data/ClassDataset_2019_fish.csv", strip.white = T,  header = F)
head(fish_data)
fish_data=t(fish_data)
fish_data = fish_data[-1,]
colnames(fish_data) <-fish_data[1,]
fish_data=as.data.frame(fish_data[-1:-2,-3:-5])
head(fish_data)

# the data frame has duplicate columns called "?" and "??" 
# we have no way of filling in these data, so omit
which(colnames(fish_data) == "?") 
which(colnames(fish_data) == "??")

fish_data = fish_data[, !colnames(fish_data) %in% c("?", "??")]

fish_data = fish_data %>%
  mutate(Site=factor(Site),
         Date = dmy(paste0(Date, "-2019")))

levels(fish_data$Site) # there are many errors in site name spelling. These must be fixed
fish_data.1 = fish_data %>%
  mutate(
    Site = fct_collapse(Site,
                        "Cattle Bay" = c("Cattle Bay"),
                        "Clam Gardens" = c( "Clam Garden", "Clam Gardens"),
                        "Hazard Bay" = c("Hazard Bay", "Harazrd Bay"),
                        "Iris Point" = c( "Iris Point", "Iris Pt"),
                        "Little Pioneer Bay North" = c("Little Pioneer Bay North", "Little Pioneer North", "Little PioneerBay North"),
                        "Little Pioneer Bay South" = c("Little Pioneer Bay S", "Little Pioneer Bay South", "Little Pioneer bay South"),
                        "Snapper Point" = c("Snapper point", "Snapper point"),
                        "Southwest Pelorus" = c("South West Polorus", "Southwest Pelorus #2", "Southwest Pelorus visit #2", "SW Peloris", "SW Pelorus", "SW Pelorous", "SW Pelorus 2nd visit")
    ))
levels(fish_data.1$Site)

# each row is a transect. Get a count of replicate transects per site
reps.per.site.fish = fish_data.1 %>%
  group_by(Site) %>%
  summarise(nTransects = length(Site))
reps.per.site.fish



##### coral cover data from PITs - 2016-2022 ##### 
# we don't have the raw data, just means of all transects per student group

coral.cover = read.csv("data/ClassDataset_2016-2022_coralcover.csv", strip.white = T, header = T)
head(coral.cover)

coral.cover = coral.cover %>%
  mutate(Year=factor(Year),
         Zone=factor(Zone),
         Site=factor(Site))

reps.per.site.cover = coral.cover %>%
  group_by(Site,Year) %>%
  summarise(nTransects = length(Site))
reps.per.site.cover

ggplot(reps.per.site.cover, aes(x=Year, y=nTransects, color=Site)) +
  geom_point() +
  geom_line( aes(group=Site)) +
  theme_classic()

reps.per.site.cover <- reps.per.site.cover %>%
  pivot_wider(
    names_from = Year,
    values_from = nTransects
  )

reps.per.site.cover


##### merge coral cover with 2025 data #####

coral.cover.2025 = read.csv("data/ClassData_PIT.csv", strip.white = T, header = T)
head(coral.cover.2025)

colnames(coral.cover.2025) <- as.character(coral.cover.2025[1, ])

coral.cover.2025 <- coral.cover.2025[-1, ]

rownames(coral.cover.2025) <- NULL

# merge with long-term data
head(coral.cover) # long-term data
# need to calculate percent cover
# then take a mean of the three transects per buddy pair to match the format of long-term data
# note the limitations of doing such a step

# Convert necessary columns to numeric
coral.cover.2025 <- coral.cover.2025 %>%
  mutate(across(c(Acropora, Massive, OtherHard, AllAlgae, SoftCoral, Other, Total), as.numeric))

coral.cover.2025.calc = coral.cover.2025 %>%
  mutate(Acropora=Acropora/Total,
         Massive=Massive/Total,
         AllHard=OtherHard/Total, # rename the column to AllHard to match the long-term data
         AllAlgae =AllAlgae/Total,
         SoftCoral = SoftCoral/Total,
         Other=Other/Total)
head(coral.cover.2025.calc)
  
coral.cover.2025.summary = coral.cover.2025.calc %>%
  group_by(BuddyPair, Year, Site) %>%
  mutate(Year = factor(Year)) %>%
  summarise(across(
    c(Acropora, Massive, AllHard, SoftCoral, AllAlgae, Other), mean, .names = "{.col}"))

head(coral.cover.2025.summary)
head(coral.cover)
# formats match, now merge
coral.cover.longterm = full_join(coral.cover.2025.summary[,-1], coral.cover)
head(coral.cover.longterm) # use this for your analysis, though NOTE you will need to rename sites to match. Relevant code for re-naming sites is included in this script

##### herbivore and planktivores, on PITs - 2016-2022 #####
herbs.planks = read.csv("data/ClassDataset_herbsPlanks_2016-2022.csv", strip.white = T, header = T)
head(herbs.planks)

herbs.planks = herbs.planks %>%
  mutate(Year=factor(Year),
         Zone=factor(Zone),
         Site=factor(Site))

reps.per.site.herbs = herbs.planks %>%
  group_by(Site,Year) %>%
  summarise(nTransects = length(Site))
reps.per.site.herbs

ggplot(reps.per.site.herbs, aes(x=Year, y=nTransects, color=Site)) +
  geom_point() +
  geom_line( aes(group=Site))

reps.per.site.herbs <- reps.per.site.herbs %>%
  pivot_wider(
    names_from = Year,
    values_from = nTransects
  )
reps.per.site.herbs


##### merge herbivore and planktivores with 2025 data #####
head(herbs.planks) # look at the old data

herbs.planks.2025 = read.csv("data/ClassData_Herbivores.csv", strip.white = T, header = F)
head(herbs.planks.2025)
# we need to get a total sum of all Herbivores and all Planktivores per transect
# and save the sums as "AllHerb" and "Planktiv"
# we need to tell R which ones are Herbivores and which are Planktivores, then sum. 

colnames(herbs.planks.2025) <- as.character(herbs.planks.2025[2, ])

herbs.planks.2025 <- herbs.planks.2025[-c(1, 2), ]
rownames(herbs.planks.2025) <- NULL

taxa_groups <- rep(NA, ncol(herbs.planks.2025))

# Only assign where columns exist
taxa_groups[6:19] <- "Planktivores"
taxa_groups[20:26] <- "Herbivores"
if (ncol(herbs.planks.2025) >= 27) {
  taxa_groups[27:ncol(herbs.planks.2025)] <- "Herbivores" # note, these are benthic feeders, but our inherited long term data do not 
  # distinguish. Consider how the assumption of grouping benthic feeders with herbivores affects your analysis
  
}


names(taxa_groups) <- colnames(herbs.planks.2025)

taxon_cols <- names(taxa_groups)[!is.na(taxa_groups)]

df_long <- herbs.planks.2025 %>%
  pivot_longer(cols = all_of(taxon_cols), names_to = "taxon", values_to = "value") %>%
  mutate(group = taxa_groups[taxon])

df_long <- df_long %>%
  mutate(group = factor(group),
         value = as.numeric(value)) %>%
  group_by(Site, group, Year, Transect, BuddyPair) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = group, values_from = total)

herbs.planks.2025.summary = df_long %>%
  mutate(AllHerb=Herbivores,
         Planktiv=Planktivores) %>% # rename the sum columnss as "AllHerb" and "Planktiv"
  mutate(Year=factor(Year))

# merge the two datasets
head(herbs.planks)
head(herbs.planks.2025.summary)

herbs.planks.longterm = full_join(herbs.planks,herbs.planks.2025.summary)
herbs.planks.longterm = herbs.planks.longterm[,1:5]

# this is your dataset for analysis, noting you might need to change Site names to match between years


##### piscivores, timed swim, long-term dataset #####
piscs = read.csv("data/ClassDataset_piscs_2016-2022.csv", strip.white = T, header = T)
head(piscs)

piscs = piscs %>%
  mutate(Year=factor(Year),
         Zone=factor(Zone),
         Site=factor(Site))

reps.per.site.piscs = piscs %>%
  group_by(Site,Year) %>%
  summarise(nTransects = length(Site))
reps.per.site.piscs

ggplot(reps.per.site.piscs, aes(x=Year, y=nTransects, color=Site)) +
  geom_point() +
  geom_line( aes(group=Site))

reps.per.site.piscs <- reps.per.site.piscs %>%
  pivot_wider(
    names_from = Year,
    values_from = nTransects
  )

reps.per.site.piscs

##### piscivores 2025 #####
piscs.2025 = read.csv("data/ClassData_piscivores.csv", strip.white = T, header = T)
head(piscs.2025)

colnames(piscs.2025) <- as.character(piscs.2025[1, ])

piscs.2025 <- piscs.2025[-c(1, 2), ]
rownames(piscs.2025) <- NULL

piscs.2025 = piscs.2025[-1:-3,1:20] %>% # remove mock data, remove empty columns at end of data frame
  mutate(Year=factor(Year),
         Site=factor(Site),
         Transect=factor(Transect))
# sum all piscivores per transect
piscs.2025.sum <- piscs.2025 %>%
  mutate(across(Apogon:Synodus, as.numeric)) %>%  # Ensure numeric
  group_by(BuddyPair, Year, Site) %>%
  mutate(Piscivores = rowSums(across(Apogon:Synodus), na.rm = TRUE)) %>%
  select(Year, Site, Piscivores)

head(piscs)

piscs.longterm = full_join(piscs, piscs.2025.sum)
# use this for your analysis, noting that you may need to adjust site names to match between old and new datasets

##### coral juveniles 2019-2021 #####
juveniles = read.csv("ClassDataset-coraljuveniles_2019-2021.csv")
head(juveniles)
juveniles=juveniles %>% mutate(Year=Time, Quadrat=factor(Quadrat))
sample.size = juveniles %>%
  group_by(Year, Site) %>%
  summarise(nQuads = length(Quadrat)/8) # divide by 8 since there are size categories and morphologies (8 observation categories per quadrat)

reps.per.site.juves <- sample.size %>%
  pivot_wider(
    names_from = Year,
    values_from = nQuads
  )


##### coral juveniles 2025 and merging #####
juveniles.2025 = read.csv("ClassData_CoralJuveniles.csv", strip.white = T, header = T)
head(juveniles.2025)
juveniles.2025 = juveniles.2025 %>%
  mutate(Quadrat=factor(Quadrat))

juveniles.longterm = full_join(juveniles[,-1], juveniles.2025[,-1])
# use this for analysis, noting that you may need to rename sites to match between datasets 



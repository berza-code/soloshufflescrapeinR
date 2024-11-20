library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(jsonlite)
library(data.table)
options(scipen=999)
library(car)
library(tidyverse)

#create a folder on your desktop called "soloshuffle" and put the merged.csv inside it

df <- read.csv(('location in the form of c:\\user\\x\\desktop\\soloshuffle\\merged.csv')
#data cleaning
df <- df %>% filter(href != 'href') # wipe the headings from copy *.csv merged.csv
df$entries.rank <- as.numeric(df$entries.rank)
df$entries.rating <- as.numeric(df$entries.rating)
df$entries.season_match_statistics.lost <- as.numeric(df$entries.season_match_statistics.lost)
df$entries.season_match_statistics.played <- as.numeric(df$entries.season_match_statistics.played)
df$entries.season_match_statistics.won <- as.numeric(df$entries.season_match_statistics.won)

#provide hex code colors for graphs in the dataset
healerdf <- df %>% mutate(color = case_when(#healers
                                             name == 'shuffle-druid-restoration' ~ '#FF7D0A',
                                             name == 'shuffle-evoker-preservation' ~ '#33937F',
                                             name == 'shuffle-monk-mistweaver'~ '#00FF96',
                                             name == 'shuffle-paladin-holy'~ '#F58CBA',
                                             name == 'shuffle-priest-holy'~ '#FFFFFF',
                                             name == 'shuffle-priest-discipline'~ '#FFFFFF',
                                             name == 'shuffle-shaman-restoration'~ '#0070DE',
                                             #casters
                                             name == 'shuffle-druid-balance' ~ '#FF7D0A',
                                             name == 'shuffle-evoker-devastation' ~ '#33937F',
                                             name == 'shuffle-mage-arcane' ~ '#69CCF0',
                                             name == 'shuffle-mage-fire' ~ '#69CCF0',
                                             name == 'shuffle-mage-frost' ~ '#69CCF0',
                                             name == 'shuffle-priest-shadow' ~ '#FFFFFF',
                                             name == 'shuffle-shaman-elemental' ~ '#0070DE',
                                             name == 'shuffle-warlock-affliction' ~ '#9482C9',
                                             name == 'shuffle-warlock-demonology' ~ '#9482C9',
                                             name == 'shuffle-warlock-destruction' ~ '#9482C9',
                                             name == 'shuffle-evoker-augmentation' ~ '#33937F',
                                             #ranged
                                             name == 'shuffle-hunter-beastmastery' ~ '#ABD473',
                                             name == 'shuffle-hunter-marksmanship' ~ '#ABD473',
                                                                                         
                                             #melee
                                             name == 'shuffle-deathknight-frost' ~ '#C41F3B',
                                             name == 'shuffle-deathknight-unholy' ~ '#C41F3B',
                                             name == 'shuffle-demonhunter-havoc' ~ '#A330C9',
                                             name == 'shuffle-druid-feral' ~ '#FF7D0A',
                                             name == 'shuffle-hunter-survival' ~ '#ABD473',
                                             name == 'shuffle-monk-windwalker' ~ '#00FF96',
                                             name == 'shuffle-paladin-retribution' ~ '#F58CBA',
                                             name == 'shuffle-rogue-assassination' ~ '#FFF569',
                                             name == 'shuffle-rogue-outlaw' ~ '#FFF569',
                                             name == 'shuffle-rogue-subtlety' ~ '#FFF569',
                                             name == 'shuffle-shaman-enhancement' ~ '#0070DE',
                                             name == 'shuffle-warrior-arms' ~ '#C79C6E',
                                             name == 'shuffle-warrior-fury' ~ '#C79C6E',
                                             #tanks
                                             name == 'shuffle-druid-guardian' ~ '#FF7D0A',
                                             name == 'shuffle-deathknight-blood' ~ '#C41F3B',
                                             name == 'shuffle-paladin-protection' ~ '#F58CBA',
                                             name == 'shuffle-warrior-protection'~ '#C79C6E',
                                             name == 'shuffle-monk-brewmaster' ~ '#00FF96',
                                             name == 'shuffle-demonhunter-vengeance' ~ '#A330C9'
                                             
                                             ))
#add the type of dps
healerdf <- healerdf %>% mutate(type = case_when(
                                            #healers
                                            name == 'shuffle-druid-restoration' ~ 'healer',
                                            name == 'shuffle-evoker-preservation' ~ 'healer',
                                            name == 'shuffle-monk-mistweaver'~ 'healer',
                                            name == 'shuffle-paladin-holy'~ 'healer',
                                            name == 'shuffle-priest-holy'~ 'healer',
                                            name == 'shuffle-priest-discipline'~ 'healer',
                                            name == 'shuffle-shaman-restoration'~ 'healer',
                                            #casters
                                            name == 'shuffle-druid-balance' ~ 'caster',
                                            name == 'shuffle-evoker-devastation' ~ 'caster',
                                            name == 'shuffle-mage-arcane' ~ 'caster',
                                            name == 'shuffle-mage-fire' ~ 'caster',
                                            name == 'shuffle-mage-frost' ~ 'caster',
                                            name == 'shuffle-priest-shadow' ~ 'caster',
                                            name == 'shuffle-shaman-elemental' ~ 'caster',
                                            name == 'shuffle-warlock-affliction' ~ 'caster',
                                            name == 'shuffle-warlock-demonology' ~ 'caster',
                                            name == 'shuffle-warlock-destruction' ~ 'caster',
                                            name == 'shuffle-evoker-augmentation' ~ 'caster',
                                            #ranged
                                            name == 'shuffle-hunter-beastmastery' ~ 'ranged',
                                            name == 'shuffle-hunter-marksmanship' ~ 'ranged',
                                            
                                            
                                            #melee
                                            name == 'shuffle-deathknight-frost' ~ 'melee',
                                            name == 'shuffle-deathknight-unholy' ~ 'melee',
                                            name == 'shuffle-demonhunter-havoc' ~ 'melee',
                                            name == 'shuffle-druid-feral' ~ 'melee',
                                            
                                            name == 'shuffle-hunter-survival' ~ 'melee',
                                            name == 'shuffle-monk-windwalker' ~ 'melee',
                                            name == 'shuffle-paladin-retribution' ~ 'melee',
                                            name == 'shuffle-rogue-assassination' ~ 'melee',
                                            name == 'shuffle-rogue-outlaw' ~ 'melee',
                                            name == 'shuffle-rogue-subtlety' ~ 'melee',
                                            name == 'shuffle-shaman-enhancement' ~ 'melee',
                                            name == 'shuffle-warrior-arms' ~ 'melee',
                                            name == 'shuffle-warrior-fury' ~ 'melee',
                                            #tanks
                                            name == 'shuffle-druid-guardian' ~ 'tank',
                                            name == 'shuffle-deathknight-blood' ~ 'tank',
                                            name == 'shuffle-paladin-protection' ~ 'tank',
                                            name == 'shuffle-warrior-protection'~ 'tank',
                                            name == 'shuffle-monk-brewmaster' ~ 'tank',
                                            name == 'shuffle-demonhunter-vengeance' ~ 'tank'

))
#add the role of each spec to the dataset
healerdf <- healerdf %>% mutate(role = case_when(
  #healers
  name == 'shuffle-druid-restoration' ~ 'healer',
  name == 'shuffle-evoker-preservation' ~ 'healer',
  name == 'shuffle-monk-mistweaver'~ 'healer',
  name == 'shuffle-paladin-holy'~ 'healer',
  name == 'shuffle-priest-holy'~ 'healer',
  name == 'shuffle-priest-discipline'~ 'healer',
  name == 'shuffle-shaman-restoration'~ 'healer',
  #casters
  name == 'shuffle-druid-balance' ~ 'dps',
  name == 'shuffle-evoker-devastation' ~ 'dps',
  name == 'shuffle-mage-arcane' ~ 'dps',
  name == 'shuffle-mage-fire' ~ 'dps',
  name == 'shuffle-mage-frost' ~ 'dps',
  name == 'shuffle-priest-shadow' ~ 'dps',
  name == 'shuffle-shaman-elemental' ~ 'dps',
  name == 'shuffle-warlock-affliction' ~ 'dps',
  name == 'shuffle-warlock-demonology' ~ 'dps',
  name == 'shuffle-warlock-destruction' ~ 'dps',
  name == 'shuffle-evoker-augmentation' ~ 'dps',
  #dps
  name == 'shuffle-hunter-beastmastery' ~ 'dps',
  name == 'shuffle-hunter-marksmanship' ~ 'dps',
  
  
  #dps
  name == 'shuffle-deathknight-frost' ~ 'dps',
  name == 'shuffle-deathknight-unholy' ~ 'dps',
  name == 'shuffle-demonhunter-havoc' ~ 'dps',
  name == 'shuffle-druid-feral' ~ 'dps',
  
  name == 'shuffle-hunter-survival' ~ 'dps',
  name == 'shuffle-monk-windwalker' ~ 'dps',
  name == 'shuffle-paladin-retribution' ~ 'dps',
  name == 'shuffle-rogue-assassination' ~ 'dps',
  name == 'shuffle-rogue-outlaw' ~ 'dps',
  name == 'shuffle-rogue-subtlety' ~ 'dps',
  name == 'shuffle-shaman-enhancement' ~ 'dps',
  name == 'shuffle-warrior-arms' ~ 'dps',
  name == 'shuffle-warrior-fury' ~ 'dps',
  
  #tank
  name == 'shuffle-druid-guardian' ~ 'tank',
  name == 'shuffle-deathknight-blood' ~ 'tank',
  name == 'shuffle-paladin-protection' ~ 'tank',
  name == 'shuffle-warrior-protection'~ 'tank',
  name == 'shuffle-monk-brewmaster' ~ 'tank',
  name == 'shuffle-demonhunter-vengeance' ~ 'tank'
))

healerdf <- healerdf %>% mutate(winrate = entries.season_match_statistics.won/entries.season_match_statistics.played)
healerdf$color <- as.factor(healerdf$color)
healerdf <- healerdf %>% group_by(name) %>% mutate(medianbyspec = median(entries.rating))
healerdf <- healerdf %>% ungroup()

totalplayers <- healerdf %>% count()

totalsub1600 <- healerdf %>% filter(between(entries.rating, 0,1600)) %>% count()
total1600 <- healerdf %>% filter(between(entries.rating, 1600,1800)) %>% count()
total1800 <- healerdf %>% filter(between(entries.rating, 1800,2100)) %>% count()
total2100 <- healerdf %>% filter(between(entries.rating, 2100,2400)) %>% count()
total2400 <- healerdf %>% filter(entries.rating >= 2400) %>% count()            

averagemmr <- mean(healerdf$entries.rating, na.rm=TRUE)            
medianmmr <- median(healerdf$entries.rating, na.rm=TRUE)
sdmmr <- sd(healerdf$entries.rating, na.rm=TRUE)
#non-placement filters out players that have less than 12 rounds won.
averagenonplacementmmr <- healerdf %>% filter(entries.season_match_statistics.won > 12) %>% select(entries.rating) %>% summarise(mean = mean(entries.rating))           
mediannonplacementmmr <- healerdf %>% filter(entries.season_match_statistics.won > 12) %>% select(entries.rating) %>% summarise(median = median(entries.rating))           
sdnonplacementmmr <- healerdf %>% filter(entries.season_match_statistics.won > 12) %>% select(entries.rating) %>% summarise(sd = sd(entries.rating))
totalmatchesplayed <- healerdf %>% select(entries.season_match_statistics.played) %>% sum()

# provides quantiles per spec for the current scraped dataset. This is used to check what percentile you are on the leaderboard for each spec(10th percentile = 90%, 20th percentile = 80%)
View(
  healerdf %>%
         group_by(name) %>%
         summarize(as_tibble_row(quantile(entries.rating, probs = c(seq(0.00,0.95,0.05),0.99,0.999))))
)

# provides a histogram of the ladder, colored by DPS(red), Healers(green) and Tanks(blue). Stats are also displayed, these are approximations due to top 5000 cutoffs.
metahist <- ggplot(healerdf,
       aes(x=entries.rating, col = 'black',fill=healerordps)) +
  geom_histogram(aes(x = entries.rating),bins=100,alpha=0.7) +
  geom_vline(xintercept = c(1600,1800,2100,2400,2700), col='blue',linetype='dashed') +
  ggtitle(paste('Solo Shuffle - TWW: Season ', season, ' , NA Region, Scraped on the ',date, ' ')) +
  xlim(c(0,3200)) +
  xlab('Rating(MMR)') +
  ylab('Number of Players in each Bin') +
  scale_color_manual(values=c('black')) +
  annotate("text", label = paste(' Total Toons: ',totalplayers$n,    '\n',
                                  'Sub 1600: ',totalsub1600$n, '\n',
                                  '1600-1800: ',total1600$n,'\n',
                                  '1800-2100: ',total1800$n, '\n',
                                  '2100-2400: ',total2100$n, '\n',
                                  '2400+: ',total2400$n, '\n',
                                  'Average MMR:', round(averagemmr), '\n',
                                  'Median MMR: ', round(medianmmr), '\n',
                                  'Non-placement Average MMR: ',round(averagenonplacementmmr$mean), '\n',
                                  'Non-placement Median MMR: ',round(mediannonplacementmmr$median), '\n',
                                  'Non-placement Standard Deviation MMR: ',round(sdnonplacementmmr), '\n',
                                  'Total Matches Played: ', totalmatchesplayed
                                  ),
           x = 2100, hjust = 0, y = Inf, vjust = 1, color = "Black", size = 8/.pt) +
  theme(legend.position="none")

#provides a boxplot of each spec and its spread along the MMR Rating system
metabox <- ggplot(healerdf, aes(y=reorder(as.factor(name),medianbyspec),x=entries.rating, col = 'black', fill=color,alpha = 0.0003)) +
  geom_boxplot() +
  geom_vline(xintercept = c(1600,1800,2100,2400,2700), col='red',linetype='dashed') +
  ggtitle(paste('Solo Shuffle - TWW: Season ', season, ' , NA Region, Scraped on the ',date, ' ')) +
  xlim(c(-100,3200)) +
  xlab('Rating(MMR)') +
  ylab('Class and Spec') +
  scale_fill_manual(values=c(levels(healerdf$color))) +
  scale_color_manual(values=c('black')) +
  theme(legend.position="none")  

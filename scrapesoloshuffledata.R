library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(jsonlite)
library(curl)
library(httr)

createAccessToken <- function(apiKey, apiSecret, region = "us") 
{
  response <- POST(
    paste("https://", region, ".battle.net/oauth/token", sep = ""),
    authenticate(apiKey, apiSecret),
    body = list(grant_type="client_credentials")
  )
  return(fromJSON(content(response, "text"), flatten = TRUE))
}

h <- new_handle()
handle_setheaders(h,
                  "Authorization" = paste0("Bearer ",createAccessToken('your API key goes here','your API secret goes here')$access_token)
)

season <- 38 # current season's ID as of november 2024

class_spec <- c(
                #healers
                'druid-restoration',
                'evoker-preservation',
                'monk-mistweaver',
                'paladin-holy',
                'priest-holy',
                'priest-discipline',
                'shaman-restoration',

                #dps
                'deathknight-frost',
                'deathknight-unholy',
                'demonhunter-havoc',
                'druid-balance',
                'druid-feral',
                'evoker-devastation',
                'evoker-augmentation',
                'hunter-beastmastery',
                'hunter-marksmanship',
                'hunter-survival',
                'mage-arcane',
                'mage-fire',
                'mage-frost',
                'monk-windwalker',
                'paladin-retribution',
                'priest-shadow',
                'rogue-assassination',
                'rogue-outlaw',
                'rogue-subtlety',
                'shaman-elemental',
                'shaman-enhancement',
                'warlock-affliction',
                'warlock-demonology',
                'warlock-destruction',
                'warrior-arms',
                'warrior-fury',
                
                #tanks
                'druid-guardian',
                'deathknight-blood',
                'paladin-protection',
                'warrior-protection',
                'monk-brewmaster',
                'demonhunter-vengeance'
                
                )


for (spec in class_spec){
  req <- curl_fetch_memory(paste0('https://us.api.blizzard.com/data/wow/pvp-season/',season,'/pvp-leaderboard/shuffle-',spec,'?namespace=dynamic-us&locale=en_US'), handle = h)
  req <- jsonlite::prettify(rawToChar(req$content))
  data <- fromJSON(req)
  write.csv(data, paste0('C:\\shuffle-',spec,'-',Sys.Date(),'.csv'))
}

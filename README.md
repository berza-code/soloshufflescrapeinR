This Project downloads the leaderboard at the current time for WoW PVP from the Blizzard API for each solo shuffle spec into a CSV using Rstudio.

Rstudio can be downloaded here: https://posit.co/download/rstudio-desktop/

You will need to setup an account with blizzard to get a key and a secret, try the following links:

https://develop.battle.net/documentation
https://develop.battle.net/access/clients

The key and secret need to be added to the code for you to be able to download the leaderboards.

You may combine the CSVs by going into the windows command prompt(cmd) and typing:

cd c:\
copy *.csv merged.csv

This will allow you to create one file called merged.csv which can be used for my other projects for visualisations and stats.

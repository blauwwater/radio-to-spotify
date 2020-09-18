#######################################################################################################
#                                                                                                     #
#                                   ### Radio2 playlist scraper ###                                   #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#

#Loading packages#
require("rvest")

require("dplyr")
require("remotes")
remotes::install_github("charlie86/spotifyr")
require(spotifyr)
require('knitr')
require("stringr")
require("jsonlite")
require(jsonlite)
require(purrr)
require(data.table)
require(httr)
require(magrittr)
require(rvest)
require(ggplot2)
#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#






#######################################################################################################
#                                                                                                     #
#                                   ### Scrape playlist page ###                                      #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#

#Get playlist url #
url <- "https://www.nporadio2.nl/playlist"

#Read HTML code from pagen#
webpage <- read_html(url)

#Get Artist and Title#
artist <- html_nodes(webpage, '.fn-artist')
title <- html_nodes(webpage, '.fn-song')

#Artist and Title to text#
artist_text <- html_text(artist)
title_text <- html_text(title)

#Artist and Title to dataframe#
artiest <- as.data.frame(artist_text)
titel_text <- as.data.frame(title_text)

#Make one dataframe#
radioplaylist <- cbind(artiest$artist_text, titel_text$title_text)
radioplaylist <- as.data.frame(radioplaylist) 
radioplaylist

#Rename columns#
colnames(radioplaylist)[1] <- "Artiest"
colnames(radioplaylist)[2] <- "Titel"
radioplaylist

#Remove duplicate songs#
radioplaylistuniek <- radioplaylist %>% distinct(Artiest, Titel, .keep_all = TRUE)

#Write to csv#
date <- Sys.Date()
date
write.csv(radioplaylistuniek, paste0("C://Users//Kantoor//Radio2playlists//playlist - ", date, ".csv"))
#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#









#######################################################################################################
#                                                                                                     #
#                                   ### Setup spotify API ###                                         #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#

#Set spotify API#
Sys.setenv(SPOTIFY_CLIENT_ID = 'jouwclientidhier')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'jouwclientsecrethier')
access_token <- get_spotify_access_token()

# Client and secret#
clientID <- "jouwclientidhier"
secret <- "jouwclientsecrethier"

# Get access token and write this to authorization header #
response = POST(
  'https://accounts.spotify.com/api/token',
  accept_json(),
  authenticate(clientID, secret),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)

token = content(response)$access_token
authorization.header = paste0("Bearer ", token)





#######################################################################################################
#                                                                                                     #
#                                   ### trackinfo batch 1 ###                                         #
#                                                                                                     #
#######################################################################################################
#                                                                                   #
# Getting the track info and pull the songs to spotify will be done in two batches. #
# This is because of the max GET requests from spotify                              #
#------------------------------------------------------------------------------------
#--------------------------------------#
# Generate new DF for tracks & DEL NA  #
#--------------------------------------# 

radioplaylistuniektest <- radioplaylistuniek[1:75,]
radioplaylistuniektest <- radioplaylistuniektest %>% filter(!is.na(Artiest))

#--------------------------------------#
# Replace character with encoding      #
#--------------------------------------# 
radioplaylistuniektest$Artiest <- gsub("\\.", "%20", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\.", "%20", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\'", "%27", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\'", "%27", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\&", "%26", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\&", "%26", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\/", "%2F", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\/", "%2F", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\-", "%2D", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\-", "%2D", radioplaylistuniektest$Titel)


#--------------------------------------#
# Generate urls for spotify request    #
#--------------------------------------# 

urls <- list(c("https://api.spotify.com/v1/search?q=track:")) %>% paste0(radioplaylistuniektest$Titel) %>% paste0(c("%20artist:")) %>% paste0(radioplaylistuniektest$Artiest) %>% paste(c("&type=track&limit=1"), sep = "")
urls <- str_replace_all(urls, " ", "%20")

#--------------------------------------#
#   GET request SPOTIFY                #
#--------------------------------------# 
# Get track information from SPOTIFY#
lijstwijk <- lapply(urls, GET, simplifyMatrix=TRUE, flatten=TRUE, config = add_headers(authorization = authorization.header, charset ="utf-8"))

#--------------------------------------#
#   Unlist results                     #
#--------------------------------------# 
# Unlist results #
responses <- unlist(lapply(lijstwijk, paste, collapse=" "))

# Results to dataframe #
responsesdf <- as.data.frame(responses)

# Get spotify:track string#
uriperurl <- data.frame(uri = str_extract(responsesdf$responses, "(spotify:track:)\\w+"))

# Remove NA Values#
uniekeuri <- uriperurl %>% filter(!is.na(uri))
uniekeuri$uri <- str_replace_all(uniekeuri$uri, "]", "")
uniekeuri
#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#









#######################################################################################################
#                                                                                                     #
#                                   ### Prepare data for spotify playlist ###                         #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#
#                                                                                   #
# pulling the songs to spotify will be done in two batches to.                      #
# This is because of the max GET requests from spotify                              #
#------------------------------------------------------------------------------------
#--------------------------------------#
#   Prepare Batch 1                    #
#--------------------------------------# 

#Preparing first batch#
uniekeuri50 <- uniekeuri[1:50,]
uniekeuri50 <- as.data.frame(uniekeuri50)
colnames(uniekeuri50)[1] <- "V1"
uniekeuri50 <- uniekeuri50 %>% filter(!is.na(V1))

#Merge columns into one#
merge50 <- uniekeuri50 %>% 
  summarise(V1 = paste(V1, collapse = ","))

#Make POST url#
posturl50 <- list(c("https://api.spotify.com/v1/playlists/afspeellijstIDhier/tracks?uris=")) %>% paste0(merge50$V1) 

#--------------------------------------#
#   Prepare Batch 2                    #
#--------------------------------------# 
uniekeuri100 <- uniekeuri[51:100,]
uniekeuri100 <- as.data.frame(uniekeuri100)
colnames(uniekeuri100)[1] <- "V1"
uniekeuri100 <- uniekeuri100 %>% filter(!is.na(V1))

#Merge columns into one#
merge100 <- uniekeuri100 %>% 
  summarise(V1 = paste(V1, collapse = ","))

#Make POST url#
posturl100 <- list(c("https://api.spotify.com/v1/playlists/afspeellijstIDhier/tracks?uris=")) %>% paste0(merge100$V1) 
#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#







#######################################################################################################
#                                                                                                     #
#                                   ### Get spotify user auth key  ###                                #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#

## Remove oauth file to get a new one#
file.remove("C://Users//Kantoor//Desktop//Radio2 scraper//.httr-oauth")

# GET user authorization code#
code <- get_spotify_authorization_code(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                       client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                       scope = "playlist-modify-public")
#Save code#
code2 = code[["credentials"]][["access_token"]]
usercode <- paste0("Bearer ", code2)

#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#



#######################################################################################################
#                                                                                                     #
#                                   ### Final: pull music to spotify ###                              #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#
### Add batches to playlist ###

#--------------------------------------#
#   Batch 1                            #
#--------------------------------------#

POST(posturl50, 
     encode="json",
     add_headers(Authorization = usercode),
     body = "{\"texts\":[\"A simple string\"]}")

#--------------------------------------#
#   Batch                             2#
#--------------------------------------#


POST(posturl100, 
     encode="json",
     add_headers(Authorization = usercode),
     body = "{\"texts\":[\"A simple string\"]}")

#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#






#######################################################################################################
#                                                                                                     #
#                                   ### Get trackinfo batch 2 ###                                     #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#

#######################################################################################################
#                                                                                                     #
#                                        HERE STARTS BATCH 2                                          #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#
#--------------------------------------#
# Generate new DF for tracks           #
#--------------------------------------# 

# DEFINING BATCH 2#
radioplaylistuniektest <- radioplaylistuniek[76:125,]
radioplaylistuniektest <- radioplaylistuniektest %>% filter(!is.na(Artiest))

#--------------------------------------#
# Replace character with encoding      #
#--------------------------------------# 
radioplaylistuniektest$Artiest <- gsub("\\.", "%20", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\.", "%20", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\'", "%27", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\'", "%27", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\&", "%26", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\&", "%26", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\/", "%2F", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\/", "%2F", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\-", "%2D", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\-", "%2D", radioplaylistuniektest$Titel)


#--------------------------------------#
# Generate urls for spotify request    #
#--------------------------------------# 

urls <- list(c("https://api.spotify.com/v1/search?q=track:")) %>% paste0(radioplaylistuniektest$Titel) %>% paste0(c("%20artist:")) %>% paste0(radioplaylistuniektest$Artiest) %>% paste(c("&type=track&limit=1"), sep = "")
urls <- str_replace_all(urls, " ", "%20")

#--------------------------------------#
#   GET request SPOTIFY                #
#--------------------------------------# 
# Get track information from SPOTIFY#
lijstwijk <- lapply(urls, GET, simplifyMatrix=TRUE, flatten=TRUE, config = add_headers(authorization = authorization.header, charset ="utf-8"))

#--------------------------------------#
#   Unlist results                     #
#--------------------------------------# 
# Unlist results #
responses <- unlist(lapply(lijstwijk, paste, collapse=" "))

# Results to dataframe #
responsesdf <- as.data.frame(responses)

# Get spotify:track string#
uriperurl <- data.frame(uri = str_extract(responsesdf$responses, "(spotify:track:)\\w+"))

# Remove NA Values#
uniekeuri <- uriperurl %>% filter(!is.na(uri))
uniekeuri$uri <- str_replace_all(uniekeuri$uri, "]", "")
uniekeuri
#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#









#######################################################################################################
#                                                                                                     #
#                                   ### Prepare data for spotify playlist ###                         #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#
#--------------------------------------#
#   Prepare Batch 1                    #
#--------------------------------------# 

#Preparing first batch#
uniekeuri50 <- uniekeuri[1:50,]
uniekeuri50 <- as.data.frame(uniekeuri50)
colnames(uniekeuri50)[1] <- "V1"
uniekeuri50 <- uniekeuri50 %>% filter(!is.na(V1))

#Merge columns into one#
merge50 <- uniekeuri50 %>% 
  summarise(V1 = paste(V1, collapse = ","))

#Make POST url#
posturl50 <- list(c("https://api.spotify.com/v1/playlists/afspeellijstIDhier/tracks?uris=")) %>% paste0(merge50$V1) 

#--------------------------------------#
#   Prepare Batch 2                    #
#--------------------------------------# 
uniekeuri100 <- uniekeuri[51:100,]
uniekeuri100 <- as.data.frame(uniekeuri100)
colnames(uniekeuri100)[1] <- "V1"
uniekeuri100 <- uniekeuri100 %>% filter(!is.na(V1))

#Merge columns into one#
merge100 <- uniekeuri100 %>% 
  summarise(V1 = paste(V1, collapse = ","))

#Make POST url#
posturl100 <- list(c("https://api.spotify.com/v1/playlists/afspeellijstIDhier/tracks?uris=")) %>% paste0(merge100$V1) 
#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#







#######################################################################################################
#                                                                                                     #
#                                   ### Get spotify user auth key  ###                                #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#
# GET user authorization code#
code <- get_spotify_authorization_code(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                       client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                       scope = "playlist-modify-public")
#Save code#
code2 = code[["credentials"]][["access_token"]]
usercode <- paste0("Bearer ", code2)


#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#



#######################################################################################################
#                                                                                                     #
#                                   ### Final: pull music to spotify ###                              #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#
### Add batches to playlist ###

#--------------------------------------#
#   Batch 1                            #
#--------------------------------------#

POST(posturl50, 
     encode="json",
     add_headers(Authorization = usercode),
     body = "{\"texts\":[\"A simple string\"]}")

#--------------------------------------#
#   Batch2                             #
#--------------------------------------#

POST(posturl100, 
     encode="json",
     add_headers(Authorization = usercode),
     body = "{\"texts\":[\"A simple string\"]}")

#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#



#######################################################################################################
#                                                                                                     #
#                                   ### Get trackinfo batch 3 ###                                     #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#

#######################################################################################################
#                                                                                                     #
#                                                                                                     #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#
#######################################################################################################
#                                                                                                     #
#                                        HERE STARTS BATCH 3                                          #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#
#--------------------------------------#
# Generate new DF for tracks           #
#--------------------------------------# 

# DEFINING BATCH 2#
radioplaylistuniektest <- radioplaylistuniek[126:200,]
radioplaylistuniektest <- radioplaylistuniektest %>% filter(!is.na(Artiest))

#--------------------------------------#
# Replace character with encoding      #
#--------------------------------------# 
radioplaylistuniektest$Artiest <- gsub("\\.", "%20", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\.", "%20", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\'", "%27", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\'", "%27", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\&", "%26", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\&", "%26", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\/", "%2F", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\/", "%2F", radioplaylistuniektest$Titel)
radioplaylistuniektest$Artiest <- gsub("\\-", "%2D", radioplaylistuniektest$Artiest)
radioplaylistuniektest$Titel <- gsub("\\-", "%2D", radioplaylistuniektest$Titel)


#--------------------------------------#
# Generate urls for spotify request    #
#--------------------------------------# 

urls <- list(c("https://api.spotify.com/v1/search?q=track:")) %>% paste0(radioplaylistuniektest$Titel) %>% paste0(c("%20artist:")) %>% paste0(radioplaylistuniektest$Artiest) %>% paste(c("&type=track&limit=1"), sep = "")
urls <- str_replace_all(urls, " ", "%20")

#--------------------------------------#
#   GET request SPOTIFY                #
#--------------------------------------# 
# Get track information from SPOTIFY#
lijstwijk <- lapply(urls, GET, simplifyMatrix=TRUE, flatten=TRUE, config = add_headers(authorization = authorization.header, charset ="utf-8"))

#--------------------------------------#
#   Unlist results                     #
#--------------------------------------# 
# Unlist results #
responses <- unlist(lapply(lijstwijk, paste, collapse=" "))

# Results to dataframe #
responsesdf <- as.data.frame(responses)

# Get spotify:track string#
uriperurl <- data.frame(uri = str_extract(responsesdf$responses, "(spotify:track:)\\w+"))

# Remove NA Values#
uniekeuri <- uriperurl %>% filter(!is.na(uri))
uniekeuri$uri <- str_replace_all(uniekeuri$uri, "]", "")
uniekeuri
#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#









#######################################################################################################
#                                                                                                     #
#                                   ### Prepare data for spotify playlist ###                         #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#
#--------------------------------------#
#   Prepare Batch 1                    #
#--------------------------------------# 

#Preparing first batch#
uniekeuri50 <- uniekeuri[1:50,]
uniekeuri50 <- as.data.frame(uniekeuri50)
colnames(uniekeuri50)[1] <- "V1"
uniekeuri50 <- uniekeuri50 %>% filter(!is.na(V1))

#Merge columns into one#
merge50 <- uniekeuri50 %>% 
  summarise(V1 = paste(V1, collapse = ","))

#Make POST url#
posturl50 <- list(c("https://api.spotify.com/v1/playlists/afspeellijstIDhier/tracks?uris=")) %>% paste0(merge50$V1) 

#--------------------------------------#
#   Prepare Batch 2                    #
#--------------------------------------# 
uniekeuri100 <- uniekeuri[51:100,]
uniekeuri100 <- as.data.frame(uniekeuri100)
colnames(uniekeuri100)[1] <- "V1"
uniekeuri100 <- uniekeuri100 %>% filter(!is.na(V1))

#Merge columns into one#
merge100 <- uniekeuri100 %>% 
  summarise(V1 = paste(V1, collapse = ","))

#Make POST url#
posturl100 <- list(c("https://api.spotify.com/v1/playlists/afspeellijstIDhier/tracks?uris=")) %>% paste0(merge100$V1) 
#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#







#######################################################################################################
#                                                                                                     #
#                                   ### Get spotify user auth key  ###                                #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#
# GET user authorization code#
code <- get_spotify_authorization_code(client_id = Sys.getenv("SPOTIFY_CLIENT_ID"),
                                       client_secret = Sys.getenv("SPOTIFY_CLIENT_SECRET"),
                                       scope = "playlist-modify-public")
#Save code#
code2 = code[["credentials"]][["access_token"]]
usercode <- paste0("Bearer ", code2)


#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#



#######################################################################################################
#                                                                                                     #
#                                   ### Final: pull music to spotify ###                              #
#                                                                                                     #
#######################################################################################################
#-----------------------------------------------------------------------------------------------------#
### Add batches to playlist ###

#--------------------------------------#
#   Batch 1                            #
#--------------------------------------#

POST(posturl50, 
     encode="json",
     add_headers(Authorization = usercode),
     body = "{\"texts\":[\"A simple string\"]}")

#--------------------------------------#
#   Batch2                             #
#--------------------------------------#

POST(posturl100, 
     encode="json",
     add_headers(Authorization = usercode),
     body = "{\"texts\":[\"A simple string\"]}")

#-------------------------------------------------------------------------------------------------------#
#                                             # EINDE #                                                 #
#-------------------------------------------------------------------------------------------------------#












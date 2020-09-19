# radio-scraper-to-spotify

# Working #
This script retrieves the songs played by radio x and moves them to a custom Spotify playlist. This way you can listen to the radio without commercials. 
With a change, the script can certainly also be used for other radio stations.
#

# How does the script work? #
1. Tracks played by radio x are retrieved and placed in a data frame. Any duplicate songs played will be removed.
2. An API connection is established with a self-created Spotify app.
3. The data frame from step 1 is divided into several batches. There is a limit on the number of songs that can be written to a playlist at once. Special characters in artist names and titles are replaced at the same time.
4. A request url is created for each number.
5. The uri (Uniform Resource Indicator) of each song is requested from Spotify via the API via the request url created.
6. A post url is created for each number containing the uri
7. The songs can now be saved to your Spotify playlist.

# Set or adjust yourself the first time #
Follow the steps below when using the script for the first time.
1. Create a Spotify app at https://developer.spotify.com.
2. Find and replace clientIDhere in the script with the clientID of your app
3. Find and replace clientsecrethere in the script clientsecrethere with the clientsecret of your app
4. Create a public playlist in Spotify and get the ID of the playlist.
5. Find and replace playlistidhere in the script with the ID of your playlist.
6. Replace the location on line 252 (Remove oauth file to get a new one) with the location of your R project.
7. Optional: do you want to keep track of which songs you have retrieved in a CSV file? then replace the location on line 75.

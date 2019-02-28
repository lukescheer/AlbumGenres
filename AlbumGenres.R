#written by Luke Scheer 02/27/2019
#call the function get_album_genres with the url of the wikipedia page of a band.
#the output is a matrix containing all of the band's albums and the genres that are associated with each album
#the first row of the output contains the genres that are found on the band's wikipedia page


install.packages("Rcrawler")   #run to install the Rcrawler library
library(Rcrawler)              #use the Rcrawler library

max_num_genres <- 8
correct_num_genres <- function(data){  #resrict number of genres to max_num_genres
  genres <- length(data)
  if(genres > max_num_genres){data <- data[1:max_num_genres]}
  else if(genres < max_num_genres){data <- c(data, rep(NA, max_num_genres-genres))}
  data
}

get_album_genres<- function(url){ 
  tryCatch({
    albums<-ContentScraper(Url = url, ManyPerPattern = TRUE,
                           XpathPatterns = c("//*/h2/span[@id='Discography']/parent::*/following-sibling::h2[1]/preceding-sibling::*[preceding-sibling::h2/span[@id='Discography']/parent::*]/descendant-or-self::ul//*[@href[contains(.,'wiki')]]/@href",
                                             "//*/th[text()[contains(.,'Genres')]]/parent::*/descendant::*[@href[contains(.,'wiki')]]",
                                             "//*/h2/span[@id='Discography']/parent::*/following-sibling::h2[1]/preceding-sibling::*[preceding-sibling::h2/span[@id='Discography']/parent::*]/descendant-or-self::ul//*[@href[contains(.,'wiki')]]",
                                             "//*[@id='firstHeading']") )
    band_genres <- albums[[2]]
    album_names <- albums[[3]]
    this_band_name <- albums[[4]]
    albums <- albums[[1]]
    album_genres <- matrix(nrow = length(albums)+1, ncol = max_num_genres)
    rownames(album_genres) <- c(this_band_name, album_names)
    
    band_genres <- tolower(band_genres)
    band_genres <- correct_num_genres(band_genres)
    album_genres[1,] <-  band_genres
    for(i in 1:length(albums)){
      url <- paste("https://en.wikipedia.org", albums[i], sep = '')
      tryCatch({
        data<-ContentScraper(Url = url, ManyPerPattern = TRUE,
                             XpathPatterns = "//*/th/child::*[text()[contains(.,'Genre')]]/parent::*/following-sibling::*/descendant::*[@href[contains(.,'wiki')]]")
        data <- data[[1]] 
        data <- tolower(data)
        data <- correct_num_genres(data)
        album_genres[i+1,] <- data
        
      }, error=function(e){ print(paste("ERROR: could not find", url)) } )
    }
    return(album_genres)
    #genres <- sort(unique(c(album_genres)))
    #album_genres
  }, error=function(e){ print(paste("ERROR: could not find", url)) } )
}


#example function calls
get_album_genres("https://en.wikipedia.org/wiki/Passion_Pit")
get_album_genres("https://en.wikipedia.org/wiki/The_Strokes")
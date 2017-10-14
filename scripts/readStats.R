library(rvest)
library(psych)

# stripping html tags
stripHtml <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# strip spaces and line breaks
stripOther <- function(htmlString) {
  htmlString <- gsub("\n", "", htmlString)
  htmlString <- gsub(" ", "", htmlString)
  return(htmlString)
}

# maplist
html <- read_html("http://195.93.242.155/~quake2/quake2/jump/_html/maps.html")
maplist <- html_nodes(html, "div")
maplist <- stripHtml(maplist) 
maplist <- stripOther(maplist)
maplist <- data.frame(matrix(maplist, ncol = 5, byrow = TRUE))
maplist <- maplist[-1,]
maplist <- data.frame(maplist$X2)

# map loop
length <- length(maplist[[1]])
prefix <- "http://195.93.242.155/~quake2/quake2/jump/_html/"
suffix <- ".html"
usermaps <- data.frame(NULL)
i <- 1
while (i < length + 1) {
  if (grepl("#", maplist[i,])) {}
  else {
    if (i==2622) {}
    else {
      html <- read_html(paste(prefix, maplist[i,], suffix, sep = ""))
      mapfile <- html_nodes(html, "div")
      mapfile <- stripHtml(mapfile)
      mapfile <- stripOther(mapfile)
      mapfile <- data.frame(matrix(mapfile, ncol = 5, byrow = TRUE))
      mapfile <- mapfile[-1, -c(4)]
      mapfile <- cbind(mapfile, maplist[i,])
      usermaps <- rbind(usermaps, mapfile)
    }
  }
  print(paste(i, "/", length, sep = ""))
  i <- i + 1
}


colnames(usermaps) <- c("pos", "name", "date", "time", "map")
write.csv(usermaps, file = "maptimes-aug1-17.csv")

ace <- subset(usermaps, name == "ace")
write.csv(ace, file = "ace_aug1-17.csv")

first <- subset(usermaps, pos == 1)
write.csv(first, file = "first-aug1-17.csv")



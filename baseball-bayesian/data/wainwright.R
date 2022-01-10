### Read in data for Adam Wainwright

# Read in data for each inning
cols <- c(1,3:5)
inn1 <- read.csv("data/wainwright1.csv", sep=",")[,cols]
inn2 <- read.csv("data/wainwright2.csv", sep=",")[,cols]
inn3 <- read.csv("data/wainwright3.csv", sep=",")[,cols]
inn4 <- read.csv("data/wainwright4.csv", sep=",")[,cols]
inn5 <- read.csv("data/wainwright5.csv", sep=",")[,cols]
inn6 <- read.csv("data/wainwright6.csv", sep=",")[,cols]
inn7 <- read.csv("data/wainwright7.csv", sep=",")[,cols]
inn8 <- read.csv("data/wainwright8.csv", sep=",")[,cols]
inn9 <- read.csv("data/wainwright9.csv", sep=",")[,cols]

# Combine for one dataset
ww <- rbind(inn1, inn2, inn3, inn4, inn5, inn6, inn7, inn8, inn9)
colnames(ww) <- c("pitches", "name", "date", "total")
ww$inning <- c(
  rep(1, nrow(inn1)),
  rep(2, nrow(inn2)),
  rep(3, nrow(inn3)),
  rep(4, nrow(inn4)),
  rep(5, nrow(inn5)),
  rep(6, nrow(inn6)),
  rep(7, nrow(inn7)),
  rep(8, nrow(inn8)),
  rep(9, nrow(inn9))
)

# cleanup
rm(inn1, inn2, inn3, inn4, inn5, inn6, inn7, inn8, inn9)
ww <- ww[order(ww$name, ww$date, ww$inning),]
rownames(ww) <- 1:nrow(ww)

# Remove the final inning of the pitcher's start from each game (usually is not a full inning)
ind <- which(ww$inning == 1)[-1] - 1
ww <- ww[-ind,]
ww
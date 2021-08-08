#spider packages
install.packages("fmsb")
library(fmsb)

#imoprt csv
PUNCHdf <- read.csv(file.choose())

head(PUNCHdf)

#get zscores transformed
PUNCHdf$PUNCHZ <- round(pnorm(PUNCHdf$PUNCHZ)*100,0)
PUNCHdf$OPP.SLG. <- round(pnorm(PUNCHdf$OPP.SLG.)*100,0)
PUNCHdf$BB.9 <- round(pnorm(PUNCHdf$BB.9)*100,0)
PUNCHdf$Whiff. <- round(pnorm(PUNCHdf$Whiff.)*100,0)

#max and min
minWhiff <- min(ORL$Whiff.)
maxWhiff <- max(ORL$Whiff.)
minBB <- min(ORL$BB.9)
maxBB <- max(ORL$BB.9)
minPunch <- min(ORL$PUNCHZ)
maxPunch <- max(ORL$PUNCHZ)
minSLG <- min(ORL$OPP.SLG.)
maxSLG <- max(ORL$OPP.SLG.)

#ORLDF Only
ORL <- PUNCHdf[PUNCHdf$TEAM == "ORL", ]
ORL
ORL$TEAM <- NULL
colnames(ORL) <- c("Player", "WhiffAbility", "WalkPreventionAbility", 
                   "DamagePreventionAbility", "PunchScore")

#ranges
max_min <- data.frame(
  WhiffAbility = c(0,100), WalkPreventionAbility = c(0,100), 
  DamagePreventionAbility = c(0,100), PunchScore = c(0,100))

#modifications for spider web
benoitDF <- ORL[ORL$Player == "Benoit", ]
benoitDF$Player <- NULL
benoitDF <- rbind(max_min, benoitDF)
rownames(benoitDF) <- c("Min", "Max", "Benoit")

cannonDF <- ORL[ORL$Player == "Cannon", ]
cannonDF$Player <- NULL
cannonDF <- rbind(max_min, cannonDF)
rownames(cannonDF) <- c("Min", "Max", "Cannon")

filbyDF <- ORL[ORL$Player == "Filby", ]
filbyDF$Player <- NULL
filbyDF <- rbind(max_min, filbyDF)
rownames(filbyDF) <- c("Min", "Max", "Filby")

kerkDF <- ORL[ORL$Player == "Kerkering", ]
kerkDF$Player <- NULL
kerkDF <- rbind(max_min, kerkDF)
rownames(kerkDF) <- c("Min", "Max", "Kerkering")

morrisDF <- ORL[ORL$Player == "Morris", ]
morrisDF$Player <- NULL
morrisDF <- rbind(max_min, morrisDF)
rownames(morrisDF) <- c("Min", "Max", "Morris")

netzDF <- ORL[ORL$Player == "Netz", ]
netzDF$Player <- NULL
netzDF <- rbind(max_min, netzDF)
rownames(netzDF) <- c("Min", "Max", "Netz")

rajcicDF <- ORL[ORL$Player == "Rajcic", ]
rajcicDF$Player <- NULL
rajcicDF <- rbind(max_min, rajcicDF)
rownames(rajcicDF) <- c("Min", "Max", "Rajcic")

saumDF <- ORL[ORL$Player == "Saum", ]
saumDF $Player <- NULL
saumDF <- rbind(max_min, saumDF)
rownames(benoitDF) <- c("Min", "Max", "Saum")

reillyDF <- ORL[ORL$Player == "Reilly", ]
reillyDF $Player <- NULL
reillyDF <- rbind(max_min, reillyDF)
rownames(reillyDF) <- c("Min", "Max", "Reilly")

schultzDF <- ORL[ORL$Player == "Schultz", ]
schultzDF$Player <- NULL
schultzDF <- rbind(max_min, schultzDF)
rownames(schultzDF) <- c("Min", "Max", "Schultz")

thomasDF <- ORL[ORL$Player == "Thomas", ]
thomasDF$Player <- NULL
thomasDF <- rbind(max_min, thomasDF)
rownames(thomasDF) <- c("Min", "Max", "Thomas")

thurmanDF <- ORL[ORL$Player == "Thurman", ]
thurmanDF$Player <- NULL
thurmanDF <- rbind(max_min, thurmanDF)
rownames(thurmanDF) <- c("Min", "Max", "Thurman")

wallyDF <- ORL[ORL$Player == "Wallerstedt", ]
wallyDF$Player <- NULL
wallyDF <- rbind(max_min, wallyDF)
rownames(wallyDF) <- c("Min", "Max", "Wallerstedt")

southardDF <- ORL[ORL$Player == "Southard", ]
southardDF$Player <- NULL
southardDF <- rbind(max_min, southardDF)
rownames(southardDF) <- c("Min", "Max", "Southard")




PlayerName <- read.csv(file.choose())

head(PlayerName)

colnames(PlayerName)
library(ggplot2)

#Strikezone and Home Plate
x <- c(-.95,.95,.95,-.95,-.95)
z <- c(1.6,1.6,3.5,3.5,1.6)
sz <- data.frame(x,z) 
plate_dimensons <- data.frame(x1 = 0, x2 = -.95, x3 = .95, y1 = 1, y2 = 0.5, y3 = 0)


#Pitch Movement
ggplot(data = PlayerName, aes(x = HorzBreak, y = InducedVertBreak, color = PitchType, size = RelSpeed)) + 
  geom_point(size = 1.5, alpha = 0.80) + 
  xlim(-25, 25) +
  ylim(-25, 25) + 
  labs(x = "Horizontal Break", y = "Induced Vertical Break") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  theme_minimal() + 
  ggtitle("PlayerName Pitch Movement") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), 
        strip.text = element_text(size = 12), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 8))

#KZone Heat Map
ggplot() +
  coord_fixed() +
  geom_density2d_filled(data = PlayerName, 
                        aes(x = PlateLocSide, y = PlateLocHeight, fill = after_stat(level)),
                        contour_var = "ndensity", 
                        show.legend = FALSE, 
                        alpha = 0.85) +
  scale_fill_viridis_d(option = "C") + 
  geom_path(data = sz, aes(x=x, y=z)) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), data = plate_dimensons) +
  geom_segment(aes(x = x2, y = y2, xend = x2, yend = y3), data = plate_dimensons) + 
  geom_segment(aes(x = x2, y = y3, xend = x3, yend = y3), data = plate_dimensons) +
  geom_segment(aes(x = x3, y = y3, xend = x3, yend = y2), data = plate_dimensons) + 
  geom_segment(aes(x = x3, y = y2, xend = x1, yend = y1), data = plate_dimensons) + 
  theme_minimal() +
  ggtitle("PlayerName Pitch Location", subtitle = "Pitcher POV") +
  xlab("Feet from Home Plate") +
  ylab("Feet Above Ground") + 
  xlim(-2.5, 2.5) +
  scale_size(range = c(0.01,3)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), 
        strip.text = element_text(size = 12)) + 
  facet_wrap(~ PitchType) 

#subsetting pitch types
PlayerNameSI <-subset(PlayerName, PitchType == "Sinker")
PlayerNameSL <-subset(PlayerName, PitchType == "Slider")
PlayerNameCH <-subset(PlayerName, PitchType == "Changeup")
PlayerNameCB <-subset(PlayerName, PitchType == "Curveball")


#Sinker Spin Rate
MLBsink <- 2150
PlayerNameAVGsink <- mean(PlayerNameSI$SpinRate)

ggplot(data = PlayerNameSI, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 12, fill = "cornflowerblue") + 
  ggtitle("PlayerName Sinker Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBsink, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = PlayerNameAVGsink, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBsink - 25, label = "MLB AVG", y = 12), 
            color = "red", 
            angle = 90, 
            text = element_text(size = 12)) + 
  geom_text(aes(x = PlayerNameAVGsink - 25, label = "PlayerName AVG", y = 12), 
            color = "black", 
            angle = 90, 
            text = element_text(size = 12)) + 
  theme_minimal() + 
  xlim(1750, 2300) + 
  ylim(0,20) + 
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

#Slider Spin Rate
MLBslider <- 2450
PlayerNameAVGslider <- mean(PlayerNameSL$SpinRate)

ggplot(data = PlayerNameSL, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 10, fill = "cornflowerblue") + 
  ggtitle("PlayerName Slider Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBslider, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = PlayerNameAVGslider, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBslider - 30, label = "MLB AVG", y = 8), 
            color = "red", 
            angle = 90, 
            text = element_text(size = 12)) + 
  geom_text(aes(x = PlayerNameAVGslider - 30, label = "PlayerName AVG", y = 8), 
            color = "black", 
            angle = 90, 
            text = element_text(size = 12)) + 
  theme_minimal() + 
  ylim(0,15) + 
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

#Changeup Spin Rate
MLBchange <- 1750
PlayerNameAVGchange <- mean(PlayerNameCH$SpinRate)

ggplot(data = PlayerNameCH, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 6, fill = "cornflowerblue") + 
  ggtitle("PlayerName Changeup Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBchange, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = PlayerNameAVGchange, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBchange - 25, label = "MLB AVG", y = 4), 
            color = "red", 
            angle = 90, 
            text = element_text(size = 12)) + 
  geom_text(aes(x = PlayerNameAVGchange - 25, label = "PlayerName AVG", y = 4), 
            color = "black", 
            angle = 90, 
            text = element_text(size = 12)) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

#Curveball Spin Rate
MLBcb <- 2550
PlayerNameAVGcb <- mean(PlayerNameCB$SpinRate)

ggplot(data = PlayerNameCB, aes(x = SpinRate)) +
  geom_histogram(color = "black", bins = 10, fill = "cornflowerblue") + 
  ggtitle("PlayerName Curveball Spin Rate Distribution") + 
  labs(x = "Spin Rate", y = "Count") +
  geom_vline(xintercept = MLBcb, 
             linetype = "dashed", 
             color = "red", 
             size = 2) + 
  geom_vline(xintercept = PlayerNameAVGcb, 
             linetype = "dashed", 
             color = "black", 
             size = 2) +
  geom_text(aes(x = MLBcb - 25, label = "MLB AVG", y = 5), 
            color = "red", 
            angle = 90, 
            text = element_text(size = 12)) + 
  geom_text(aes(x = PlayerNameAVGcb - 25, label = "PlayerName AVG", y = 5), 
            color = "black", 
            angle = 90, 
            text = element_text(size = 12)) + 
  theme_minimal() + 
  ylim(0,10) +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))

#tunneling graph
ggplot(data = PlayerName, aes(x = RelSide, y = RelHeight, color = PitchType)) + 
  geom_point(size = 1.5, alpha = 0.65) + 
  xlim(1.5,4.5) + 
  ylim(3.25, 6.25) + 
  theme_minimal() + 
  labs(x = "Release Side (ft)", y = "Release Height (ft)") + 
  ggtitle("PlayerName Pitch Tunneling") + 
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), 
        legend.title = element_text(size = 12), 
        legend.text = element_text(size = 10))

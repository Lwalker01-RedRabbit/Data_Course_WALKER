#YO! This is my ugly plot code! 

#install.packages("png")
#install.packages("ggimage")
#install.packages("vctrs")

library(ggimage)
library(png)
library(ggplot2)
library(gganimate)

# I will be using a .csv file with data about ski resorts/skiing. 

#here's the data going into a variable 
ski <- read.csv("https://gist.githubusercontent.com/Ewiseman/b251e5eaf70ca52a4b9b10dce9e635a4/raw/ski_resort_stats.csv")

View(ski)

str(ski)
#lots of data to pick from! 364 observations and 16 variables. 
#hmm. I think I'll do ski resort y and latitude x. Cause latitude means height right? and height means longer runs?

#plot(ski$lat, ski$resort_name), but we need to get ski$resort_name as numerical values. 

#get length (which should be 364) of column ski$resort_name
yValuesforSkiResorts <- 1:length(ski$resort_name)

plot(ski$lat,yValuesforSkiResorts)
#nice, works. Let's add labels using xlab and ylab. 
#I will need to also draw labels on the y axis, so I'll use yaxt and use axis to draw the correct labels.

plot(ski$lat,yValuesforSkiResorts,
     xlab = "LATitidde??",
     ylab = "resorts or whatev.")
axis(2, at = yValuesforSkiResorts, labels = ski$resort_name, las = 1,cex.axis = 0.4)
#still not drawing all the resort names, but that's fine I guess. 
#time to add png.
#png("snowymountain.jpg")
#png("SNOWmountain.png")
#okay don't do png(....) cause it overwrites whatever that png or jpg file was. 

sillyimage <- readPNG("SNOWmountainV2.png")

rasterImage(sillyimage,
            min(ski$lat),
            min(yValuesforSkiResorts),
            max(ski$lat),
            max(yValuesforSkiResorts))

plot(ski$lat,yValuesforSkiResorts,
     xlab = "LATitidde??",
     ylab = "resorts or whatev.")
axis(2, at = yValuesforSkiResorts, labels = ski$resort_name, las = 1,cex.axis = 0.04)

#list.files()
#for some reason the png is having a hard time showing up. 

#let's switch to ggplot2 code, maybe that will help us. 

#ski$frame variable, with 364 values counting up. this will help us for frames. 
ski$frame <- 1:nrow(ski)

#my image
sillyimage <- readPNG("SNOWmountainV2.png")
ski$doticon <- "cacas.png"


#following gganimate tutorial from discussions 


#clean up missing value from data !is.na will let us know if a value is "NA" in ski$lat. 
ski <- ski[!is.na(ski$lat), ]
ski$y_id <- 1:nrow(ski)
ski$frame <- 1:nrow(ski)

str(ski)
sum(is.na(ski$lat))


uggles <- ggplot(ski, aes(x = lat, y = y_id)) +
  annotation_raster(sillyimage,
    xmin = min(ski$lat),
    xmax = max(ski$lat),
    ymin = min(ski$y_id),
    ymax = max(ski$y_id)) +
  geom_image(aes(image = doticon), size = 0.3) +
  scale_y_continuous(
    breaks = ski$y_id,
    labels = ski$resort_name) +
  labs(
    x = "LATitideee??",
    y = "resorts or whatev.",
    title = "EPIC ! ") +
  theme_minimal()

uggles
print(uggles)
#looks great! 
#time to animate this fool.

animated_uggles <- uggles +
  transition_states(frame, transition_length = 0, state_length = 1) +
  shadow_mark(past = TRUE)
#run animate
animate(animated_uggles, width = 900, height = 1600, fps = 10)

#I was gonna animate a clearer graph, but that would be besides the point. 

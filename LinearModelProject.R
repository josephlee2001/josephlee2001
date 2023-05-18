#####################################################################################################
#This section is introducing the dataset into R#

library(foreign)
library(ggplot2)
library(gridExtra)
library(dplyr)

cars1 <- read.csv(file.choose(), header=TRUE)
summary(cars1)
#####################################################################################################
#This section cleans the data and removes values that aren't wanted in the modelling process#

unique(cars1$Color)

cars1 <- subset(cars1, Color!="Fuji" & Color!="Metallic" & Color!="Gun" & Color!="Light" & Color!="Cool" & Color!="BRITISH" & Color!="Other" & Color!="Pepper" & Color!="Stromboli" & Color!="Mercury" & Color!="Leatherette" & Color!="Graphite" & Color!="Deep" & Color!="Artic" & Color!="Frozen" & Color!="neon" & Color!="Pearl" & Color!="Dark" & Color!="Cirrus" & Color!="Learher" & Color!="Glacier" & Color!="Polar" & Color!="Atomic" & Color!="Patriot" & Color!="Lighting" & Color!="Sting" & Color!="TRUE" & Color!="Alpine" & Color!="Navarra" & Color!="Neptune" & Color!="Tenorite" & Color!="Slipstream" & Color!="Horizon" & Color!="Toy" & Color!="Liquid" & Color!="Harmony" & Color!="Lava" & Color!="Selenite" & Color!="light" & Color!="Mountain" & Color!="vivid" & Color!="Crystal" & Color!="Soul" & Color!="Meteor" & Color!="Autumn" & Color!="Seduce" & Color!="Vivid" & Color!="Granite" & Color!="Gecko" & Color!="Ceramic" & Color!="Summit" & Color!="Magnetic" & Color!="Mojave" & Color!="Race" & Color!="sunset" & Color!="Diamond" & Color!="Sky" & Color!="Vitamin" & Color!="Velvet" & Color!="Panthera" & Color!="pearl" & Color!="pearl" & Color!="Glossy" & Color!="Tan" & Color!="Steel" & Color!="Sapphire" & Color!="Melbourne" & Color!="Aurora" & Color!="Sparkling" & Color!="Moondust" & Color!="crystal" & Color!="corris" & Color!="Coverline" & Color!="Hornet" & Color!="selenite" & Color!="Daytona" & Color!="Amber" & Color!="WR" & Color!="ICE" & Color!="Metalic" & Color!="Matte" & Color!="Marble" & Color!="Sunset" & Color!="Aruba" & Color!="Oolong" & Color!="Tidal" & Color!="Some" & Color!="Champagne" & Color!="Bright" & Color!="Ice" & Color!="Fabric" & Color!="Nebula" & Color!="metallic" & Color!="Orkney" & Color!="Caribbean" & Color!="Magic" & Color!="Estoril" & Color!="Mineral" & Color!="Palladium" & Color!="Snowflake" & Color!="Waite" & Color!="Lunar" & Color!="Lightning" & Color!="Cosmos" & Color!="Oak" & Color!="Winter" & Color!="Edge" & Color!="Winning" & Color!="Celestite" & Color!="Fusion" & Color!="Rhodium" & Color!="Reflex" & Color!="Misano" & Color!="Phantom" & Color!="Plasma" & Color!="Flamenco" & Color!="Mica" & Color!="impulse" & Color!="New" & Color!="Aluminium" & Color!="candy" & Color!="Candy" & Color!="Jungle" & Color!="Gunmetal" & Color!="diamond" & Color!="British" & Color!="Charcol" & Color!="MINERAL" & Color!="Karma" & Color!="Nitrate" & Color!="Satin" & Color!="Dusty" & Color!="Galena" & Color!="Magnetite" & Color!="gunmetal" & Color!="Stoff" & Color!="Carbon" & Color!="Alabaster" & Color!="pre" & Color!="Jatoba" & Color!="Leather" & Color!="Clear" & Color!="Marine" & Color!="Greg" & Color!="night" & Color!="Starlight" & Color!="Onyx" & Color!="Conquer" & Color!="CRYSTAL" & Color!="Splice" & Color!="Cloth" & Color!="Heron" & Color!="Ocean" & Color!="Frosted" & Color!="Surfy" & Color!="Galapagos" & Color!="Pacific" & Color!="Smoke" & Color!="Iridium" & Color!="Off" & Color!="Forged" & Color!="Alto" & Color!="Onxy" & Color!="Glazier" & Color!="Perl" & Color!="Velocity" & Color!="Missano" & Color!="Space" & Color!="Velour" & Color!="Gloss" & Color!="Machine" & Color!="Car" & Color!="Petroleum")

unique(cars1$Color)

cars1["Color"][cars1["Color"] == 'Grey'] <- 'Grey'
cars1["Color"][cars1["Color"] == 'Black'] <- 'Black'
cars1["Color"][cars1["Color"] == 'White'] <- 'White'
cars1["Color"][cars1["Color"] == 'Blue'] <- 'Blue'
cars1["Color"][cars1["Color"] == 'Green'] <- 'Green'
cars1["Color"][cars1["Color"] == 'WHITE'] <- 'White'
cars1["Color"][cars1["Color"] == 'Silver'] <- 'Silver'
cars1["Color"][cars1["Color"] == 'silver'] <- 'Silver'
cars1["Color"][cars1["Color"] == 'Red'] <- 'Red'
cars1["Color"][cars1["Color"] == 'Gold'] <- 'Gold'
cars1["Color"][cars1["Color"] == 'grey'] <- 'Grey'
cars1["Color"][cars1["Color"] == 'Burgundy'] <- 'Burgundy'
cars1["Color"][cars1["Color"] == 'Orange'] <- 'Orange'
cars1["Color"][cars1["Color"] == 'Titanium'] <- 'Titanium'
cars1["Color"][cars1["Color"] == 'white'] <- 'White'
cars1["Color"][cars1["Color"] == 'Charcoal'] <- 'Charcoal'
cars1["Color"][cars1["Color"] == 'Brown'] <- 'Brown'
cars1["Color"][cars1["Color"] == 'Yellow'] <- 'Yellow'
cars1["Color"][cars1["Color"] == 'Beige'] <- 'Beige'
cars1["Color"][cars1["Color"] == 'Cream'] <- 'Cream'
cars1["Color"][cars1["Color"] == 'Darkblue'] <- 'Dark Blue'
cars1["Color"][cars1["Color"] == 'maroon'] <- 'Burgundy'
cars1["Color"][cars1["Color"] == 'charcoal'] <- 'Charcoal'
cars1["Color"][cars1["Color"] == 'blue'] <- 'Blue'
cars1["Color"][cars1["Color"] == 'gray'] <- 'Grey'
cars1["Color"][cars1["Color"] == 'green'] <- 'Green'
cars1["Color"][cars1["Color"] == 'Navy'] <- 'Navy'
cars1["Color"][cars1["Color"] == 'black'] <- 'Black'
cars1["Color"][cars1["Color"] == 'Whtie'] <- 'White'
cars1["Color"][cars1["Color"] == 'SILVER'] <- 'Silver'
cars1["Color"][cars1["Color"] == 'Bronze'] <- 'Bronze'
cars1["Color"][cars1["Color"] == 'Platinum'] <- 'Platinum'
cars1["Color"][cars1["Color"] == 'Cherry'] <- 'Red'
cars1["Color"][cars1["Color"] == 'BLUE'] <- 'Blue'
cars1["Color"][cars1["Color"] == 'Sliver'] <- 'Silver'
cars1["Color"][cars1["Color"] == 'While'] <- 'White'
cars1["Color"][cars1["Color"] == 'BLACK'] <- 'Black'
cars1["Color"][cars1["Color"] == 'Whte'] <- 'White'
cars1["Color"][cars1["Color"] == 'GOLD'] <- 'Gold'
cars1["Color"][cars1["Color"] == 'red'] <- 'Red'
cars1["Color"][cars1["Color"] == 'Gray'] <- 'Grey'
cars1["Color"][cars1["Color"] == 'Ebony'] <- 'Black'
cars1["Color"][cars1["Color"] == 'Crème'] <- 'Cream'
cars1["Color"][cars1["Color"] == 'Olive'] <- 'Green'
cars1["Color"][cars1["Color"] == 'Ruby'] <- 'Red'
cars1["Color"][cars1["Color"] == 'beige'] <- 'Beige'
cars1["Color"][cars1["Color"] == 'Baige'] <- 'Beige'
cars1["Color"][cars1["Color"] == 'Purple'] <- 'Purple'
cars1["Color"][cars1["Color"] == 'Maroon'] <- 'Burgundy'
cars1["Color"][cars1["Color"] == 'burgundy'] <- 'Burgundy'
cars1["Color"][cars1["Color"] == 'Magenta'] <- 'Pink'
cars1["Color"][cars1["Color"] == 'GREY'] <- 'Grey'
cars1["Color"][cars1["Color"] == 'gold'] <- 'Gold'
cars1["Color"][cars1["Color"] == 'Pink'] <- 'Pink'
cars1["Color"][cars1["Color"] == 'Silber'] <- 'Silver'
cars1["Color"][cars1["Color"] == 'NAVY'] <- 'Navy'
cars1["Color"][cars1["Color"] == 'Ivory'] <- 'White'
cars1["Color"][cars1["Color"] == 'RED'] <- 'Red'
cars1["Color"][cars1["Color"] == 'Emerald'] <- 'Green'
cars1["Color"][cars1["Color"] == 'Bugerney'] <- 'Burgundy'
cars1["Color"][cars1["Color"] == 'Rouge'] <- 'Red'
cars1["Color"][cars1["Color"] == 'SIlver'] <- 'Silver'
cars1["Color"][cars1["Color"] == 'yellow'] <- 'Yellow'

unique(cars1$Color)



unique(cars1$Type)

cars1 <- subset(cars1, Type!="Wagon" & Type!="Sportswagon" & Type!="Bus" & Type!="Coach" & Type!="Troop Carrier")

cars1["Type"][cars1["Type"] == 'Double Cab Pick Up'] <- 'Pickup'
cars1["Type"][cars1["Type"] == 'Club Cab Pickup'] <- 'Pickup'
cars1["Type"][cars1["Type"] == 'Space Cab Pickup'] <- 'Pickup'
cars1["Type"][cars1["Type"] == 'Super Cab Pickup'] <- 'Pickup'
cars1["Type"][cars1["Type"] == 'Crew Cab Pickup'] <- 'Pickup'
cars1["Type"][cars1["Type"] == 'X Cab Pickup'] <- 'Pickup'
cars1["Type"][cars1["Type"] == 'King Cab Pickup'] <- 'Pickup'
cars1["Type"][cars1["Type"] == 'King Cab Pick Up'] <- 'Pickup'
cars1["Type"][cars1["Type"] == 'Dual Cab Pick-up'] <- 'Pickup'
cars1["Type"][cars1["Type"] == 'Pickup'] <- 'Pickup'
cars1["Type"][cars1["Type"] == 'Space Cab Utility'] <- 'Utility'
cars1["Type"][cars1["Type"] == 'X Cab Utility'] <- 'Utility'
cars1["Type"][cars1["Type"] == 'Double Cab Utility'] <- 'Utility'
cars1["Type"][cars1["Type"] == 'Club Cab Utility'] <- 'Utility'
cars1["Type"][cars1["Type"] == 'Crew Cab Utility'] <- 'Utility'
cars1["Type"][cars1["Type"] == 'Freestyle Utility'] <- 'Utility'
cars1["Type"][cars1["Type"] == 'Dual Cab Utility'] <- 'Utility'
cars1["Type"][cars1["Type"] == 'Super Cab Utility'] <- 'Utility'
cars1["Type"][cars1["Type"] == 'Utility'] <- 'Utility'
cars1["Type"][cars1["Type"] == 'King Cab Utility'] <- 'Utility'
cars1["Type"][cars1["Type"] == 'Space Cab Chassis'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'Freestyle Cab Chassis'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'Club Cab Chassis'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'Leaf Cab Chassis'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'Double Cab Chassis'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'Crew Cab Chassis'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'Cab Chassis'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'X Cab Cab Chassis'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'Super Cab Chassis'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'Cab Chassis Tray'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'Dual Cab Chassis'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'Coil Cab Chassis'] <- 'Cab Chassis'
cars1["Type"][cars1["Type"] == 'Crew Van'] <- 'Van'
cars1["Type"][cars1["Type"] == 'Van'] <- 'Van'
cars1["Type"][cars1["Type"] == 'Panel Van'] <- 'Van'
cars1["Type"][cars1["Type"] == 'Crew Cab Van'] <- 'Van'
cars1["Type"][cars1["Type"] == 'Blind Van'] <- 'Van'

cars1 <- subset(cars1, Gearbox!="AWD" & Gearbox!="Front")

cars1 <- subset(cars1, Brand!="Cupra" & Brand!="Dodge" & Brand!="Genesis" & Brand!="Hino" & Brand!="Iveco" & Brand!="Mitsubishi Fuso" & Brand!="FPV" & Brand!="Foton")

cars1 <- cars1[cars1$Price<150000,]


#####################################################################################################

summary(cars1)

set.seed(1)
sample1 <- sample(1:nrow(cars1), 1600) 
sample1

sampcars1 <- cars1[sample1,]
sampcars1 <- as.data.frame(sampcars1)


summary(sampcars1)
sampcars1$Color <- as.factor(sampcars1$Color)
sampcars1$Fuel <- as.factor(sampcars1$Fuel)
sampcars1$Status <- as.factor(sampcars1$Status)
sampcars1$Gearbox <- as.factor(sampcars1$Gearbox)
sampcars1$Type <- as.factor(sampcars1$Type)
sampcars1$Brand <- as.factor(sampcars1$Brand)
sampcars1$Type<- as.factor(sampcars1$Type)
sampcars1$Gearbox <- as.factor(sampcars1$Gearbox)
summary(sampcars1)

#####################################################################################################

cars1$Color <- as.factor(cars1$Color)
cars1$Fuel <- as.factor(cars1$Fuel)
cars1$Status <- as.factor(cars1$Status)
cars1$Gearbox <- as.factor(cars1$Gearbox)
cars1$Type <- as.factor(cars1$Type)
cars1$Brand <- as.factor(cars1$Brand)
cars1$Type<- as.factor(cars1$Type)
cars1$Gearbox <- as.factor(cars1$Gearbox)

yearplot <- ggplot(cars1, aes(x = Year, y = Price)) + geom_point()
yearplot

ccplot <- ggplot(cars1, aes(x = CC, y = Price)) + geom_point() + stat_smooth(method="lm", se=F) 
ccplot

kiloplot <- ggplot(cars1, aes(x = Kilometers, y = Price)) + 
              geom_point() +
              xlim(0, 300000) 
kiloplot

fuelplot <- ggplot(cars1, aes(x = Fuel, y = Price)) + geom_boxplot(notch=FALSE) 
fuelplot

gearplot <- ggplot(cars1, aes(x = Gearbox, y = Price)) + geom_boxplot(notch=FALSE) 
gearplot

statusplot <- ggplot(cars1, aes(x = Status, y = Price)) + geom_boxplot(notch=FALSE) 
statusplot 

typeplot <- ggplot(cars1, aes(x = Type, y = Price)) + geom_boxplot(notch=FALSE) 
typeplot

colorplot <- ggplot(cars1, aes(x = Color, y = Price)) + geom_boxplot(notch=FALSE) 
colorplot

brandplot <- ggplot(cars1, aes(x = Brand, y = Price)) + geom_boxplot(notch=FALSE) + theme(axis.text.x = element_text(angle = 90)) + stat_smooth(method="lm", se=F)
brandplot

seatplot <- boxplot(Price ~ Seating.Capacity, data=cars1)
seatplot



#####################################################################################################


model <- lm(Price ~ (log(Kilometers) + Color + Fuel + Status + Gearbox + Type + Brand + Type + Year + CC), data=sampcars1)
summary(model)

hist(residuals(model), col="steelblue") #seems to follow a normal distribution


plot(fitted(model), residuals(model))
abline(h=0, lty=1)

logmodel <- lm(log(Price) ~ (log(Kilometers) + Color + Fuel + Status + Gearbox + Type + Brand + Type + Year + CC), data=sampcars1)
summary(logmodel)

hist(residuals(logmodel), col="steelblue") #seems to follow a normal distribution


plot(fitted(logmodel), residuals(logmodel))
abline(h=0, lty=1)

redmodel <- step(logmodel)
summary(redmodel)

par(mfrow=c(2,2))
plot(redmodel)













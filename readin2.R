library("readxl")

Game1 <- read_excel("ForestCity_05_28.xlsx")
Game2 <- read_excel("ForestCity_06_06.xlsx")
Game3 <- read_excel("ForestCity_06_12.xlsx")
Game4 <- read_excel("ForestCity_06_15.xlsx")
Game5 <- read_excel("ForestCity_06_24_1.xlsx")
Game6 <- read_excel("ForestCity_06_24_2.xlsx")
Game7 <- read_excel("ForestCitySplitSquad_06_04.xlsx")
Game8 <- read_excel("ForestCitySplitSquad_06_25.xlsx")
Game9 <- read_excel("ForestCitySplitSquad_06_26.xlsx")
Game10 <- read_excel("Macon_06_26.xlsx")
Game11 <- read_excel("Lexington_06_17.xlsx")
Game12 <- read_excel("Savannah_06_09.xlsx")
Game13 <- read_excel("Savannah_06_10.xlsx")
Game14 <- read_excel("Savannah_06_19.xlsx")



PitcherSpec1 <- Game1[c("Date", 
                        "Pitcher", 
                        "TaggedPitchType", 
                        "RelSpeed", 
                        "SpinRate" , 
                        "PitcherTeam", 
                        "Balls", 
                        "Strikes", 
                        "PitchCall",  
                        "PlayResult", 
                        "RelHeight", 
                        "RelSide", 
                        "PlateLocHeight", 
                        "PlateLocSide", 
                        "ExitSpeed",
                        "BatterSide")]

PitcherSpec2 <- Game2[c("Date", 
                        "Pitcher", 
                        "TaggedPitchType", 
                        "RelSpeed", 
                        "SpinRate" , 
                        "PitcherTeam", 
                        "Balls", 
                        "Strikes", 
                        "PitchCall", 
                        "PlayResult", 
                        "RelHeight", 
                        "RelSide", 
                        "PlateLocHeight", 
                        "PlateLocSide", 
                        "ExitSpeed",
                        "BatterSide")]
PitcherSpec3 <- Game3[c("Date", 
                        "Pitcher", 
                        "TaggedPitchType", 
                        "RelSpeed", 
                        "SpinRate" , 
                        "PitcherTeam", 
                        "Balls", 
                        "Strikes", 
                        "PitchCall", 
                        "PlayResult", 
                        "RelHeight", 
                        "RelSide", 
                        "PlateLocHeight", 
                        "PlateLocSide", 
                        "ExitSpeed",
                        "BatterSide")]
PitcherSpec4 <- Game4[c("Date", 
                        "Pitcher", 
                        "TaggedPitchType", 
                        "RelSpeed", 
                        "SpinRate" , 
                        "PitcherTeam", 
                        "Balls", 
                        "Strikes", 
                        "PitchCall", 
                        "PlayResult", 
                        "RelHeight", 
                        "RelSide", 
                        "PlateLocHeight", 
                        "PlateLocSide", 
                        "ExitSpeed",
                        "BatterSide")]
PitcherSpec5 <- Game5[c("Date", 
                        "Pitcher", 
                        "TaggedPitchType", 
                        "RelSpeed", 
                        "SpinRate" , 
                        "PitcherTeam", 
                        "Balls", 
                        "Strikes", 
                        "PitchCall",  
                        "PlayResult", 
                        "RelHeight", 
                        "RelSide", 
                        "PlateLocHeight", 
                        "PlateLocSide", 
                        "ExitSpeed",
                        "BatterSide")]
PitcherSpec6 <- Game6[c("Date", 
                        "Pitcher", 
                        "TaggedPitchType", 
                        "RelSpeed", 
                        "SpinRate" , 
                        "PitcherTeam", 
                        "Balls", 
                        "Strikes", 
                        "PitchCall", 
                        "PlayResult", 
                        "RelHeight", 
                        "RelSide", 
                        "PlateLocHeight", 
                        "PlateLocSide", 
                        "ExitSpeed",
                        "BatterSide")]
PitcherSpec7 <- Game7[c("Date", 
                        "Pitcher", 
                        "TaggedPitchType", 
                        "RelSpeed", 
                        "SpinRate" , 
                        "PitcherTeam", 
                        "Balls", 
                        "Strikes", 
                        "PitchCall",  
                        "PlayResult", 
                        "RelHeight", 
                        "RelSide", 
                        "PlateLocHeight", 
                        "PlateLocSide", 
                        "ExitSpeed",
                        "BatterSide")]
PitcherSpec8 <- Game8[c("Date", 
                        "Pitcher", 
                        "TaggedPitchType", 
                        "RelSpeed", 
                        "SpinRate" , 
                        "PitcherTeam", 
                        "Balls", 
                        "Strikes", 
                        "PitchCall", 
                        "PlayResult", 
                        "RelHeight", 
                        "RelSide", 
                        "PlateLocHeight", 
                        "PlateLocSide", 
                        "ExitSpeed",
                        "BatterSide")]
PitcherSpec9 <- Game9[c("Date", 
                        "Pitcher", 
                        "TaggedPitchType", 
                        "RelSpeed", 
                        "SpinRate" , 
                        "PitcherTeam", 
                        "Balls", 
                        "Strikes", 
                        "PitchCall",  
                        "PlayResult", 
                        "RelHeight", 
                        "RelSide", 
                        "PlateLocHeight", 
                        "PlateLocSide", 
                        "ExitSpeed",
                        "BatterSide")]
PitcherSpec10 <- Game10[c("Date", 
                          "Pitcher", 
                          "TaggedPitchType", 
                          "RelSpeed", 
                          "SpinRate" , 
                          "PitcherTeam", 
                          "Balls", 
                          "Strikes", 
                          "PitchCall", 
                          "PlayResult", 
                          "RelHeight", 
                          "RelSide", 
                          "PlateLocHeight", 
                          "PlateLocSide", 
                          "ExitSpeed",
                          "BatterSide")]
PitcherSpec11 <- Game11[c("Date", 
                          "Pitcher", 
                          "TaggedPitchType", 
                          "RelSpeed", 
                          "SpinRate" , 
                          "PitcherTeam", 
                          "Balls", 
                          "Strikes", 
                          "PitchCall", 
                          "PlayResult", 
                          "RelHeight", 
                          "RelSide", 
                          "PlateLocHeight", 
                          "PlateLocSide", 
                          "ExitSpeed",
                          "BatterSide")]
PitcherSpec12 <- Game12[c("Date", 
                          "Pitcher", 
                          "TaggedPitchType", 
                          "RelSpeed", 
                          "SpinRate" , 
                          "PitcherTeam", 
                          "Balls", 
                          "Strikes", 
                          "PitchCall", 
                          "PlayResult", 
                          "RelHeight", 
                          "RelSide", 
                          "PlateLocHeight", 
                          "PlateLocSide", 
                          "ExitSpeed",
                          "BatterSide")]
PitcherSpec13 <- Game13[c("Date", 
                          "Pitcher", 
                          "TaggedPitchType", 
                          "RelSpeed", 
                          "SpinRate" , 
                          "PitcherTeam", 
                          "Balls", 
                          "Strikes", 
                          "PitchCall", 
                          "PlayResult", 
                          "RelHeight", 
                          "RelSide", 
                          "PlateLocHeight", 
                          "PlateLocSide", 
                          "ExitSpeed",
                          "BatterSide")]
PitcherSpec14 <- Game14[c("Date", 
                          "Pitcher", 
                          "TaggedPitchType", 
                          "RelSpeed", 
                          "SpinRate" , 
                          "PitcherTeam", 
                          "Balls", 
                          "Strikes", 
                          "PitchCall", 
                          "PlayResult", 
                          "RelHeight", 
                          "RelSide", 
                          "PlateLocHeight", 
                          "PlateLocSide", 
                          "ExitSpeed",
                          "BatterSide")]

Comb1 <- rbind(PitcherSpec1, PitcherSpec2) 
Comb2 <- rbind(PitcherSpec3, PitcherSpec4) 
Comb3 <- rbind(PitcherSpec5, PitcherSpec6) 
Comb4 <- rbind(PitcherSpec9, PitcherSpec7) 
Comb5 <- rbind(PitcherSpec8, PitcherSpec10) 
Comb6 <- rbind(PitcherSpec11, PitcherSpec12) 

PitcherCombb1 <- rbind(PitcherSpec13, Comb1) 
PitcherCombb2 <- rbind(PitcherSpec14, Comb6) 



PitcherCombs2 <- rbind(Comb3, Comb4) 
PitcherCombs3 <- rbind(Comb5, PitcherCombb2) 


Combbs1 <- rbind(Comb2, PitcherCombs3) 
Combbs2 <- rbind(PitcherCombb1, PitcherCombs2) 



PitcherSpecs <- rbind(Combbs1, Combbs2)

BurgSpec <- subset(PitcherSpecs, PitcherTeam=="SPA_B")


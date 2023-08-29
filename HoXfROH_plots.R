install.packages("gridExtra")
install.packages("cowplot")

library(ggplot2)
library(gridExtra)
library(cowplot)

setwd("/Users/heatherclendenin/RoH/detectRuns/pop_summaries/")
####
####
###Brown bear Ho###
####
#load data and set as dateframe
Uamer_ROH <- read_excel("Uamer_ROH_popsumm.xlsx")
Uarct_ROH <- read_excel("Uarct_ROH_popsumm.xlsx")
Uamer_Ho <- read_excel("Uamer_Ho.xlsx")
Uarct_Ho <- read_excel("Uarct_Ho.xlsx")

Uamer_ROH <- data.frame(Uamer_ROH)
Uarct_ROH <- data.frame(Uarct_ROH)
Uamer_Ho <- data.frame(Uarct_Ho)
Uarct_Ho <- data.frame(Uarct_Ho)



#separating out LA; consider plotting w/ AZ & NV set apart
Uamer_numXsumROH <- ggplot(Uamer_ROH, aes(x=numROH, y=sumROH, colour=Region)) +
  geom_point(size=3, alpha=0.8) +                               
  scale_color_manual(values = c("Southeastern" = "dodgerblue",
                                "Great Lakes" = "skyblue",
                                "SE Alaskan" = "purple",
                                "West" = "magenta",
                                "Louisiana" = "black"))
#scatter plot Uamer_numXsumROH
Uamer_numXsumROH

Uamer_numXsumROH + xlim(0, 1500) + ylim(0, 1.6*(10**9))

Uarct_numXsumROH <- ggplot(Uarct_ROH, aes(x=numROH, y=sumROH, colour=Region)) +
  geom_point(size=3, alpha=0.8) +                                
  scale_color_manual(values = c("US (lower 48)" = "sienna",
                                "US (Alaska)" = "lightpink4",
                                "Japan" = "plum3",
                                "Europe" = "orange",
                                "Fennoscandia" = "yellow3"))
#scatter plot Uarct_numXsumROH
Uarct_numXsumROH

Uarct_numXsumROH + xlim(0, 1500) + ylim(0, 1.6*(10**9))


#plot numROH x sumROH for Uarct & Uamer as a 2 plot panel
numXsumROH <- grid.arrange(Uamer_numXsumROH  + xlim(0, 1500) + ylim(0, 1.6*(10**9)), arrangeGrob(Uarct_numXsumROH + xlim(0, 1500) + ylim(0, 1.6*(10**9))), ncol = 2)

#save plot
save_plot("numXsumROH.png", numXsumROH, base_aspect_ratio = 3)#
###
###
###
###
###
###
#
#plot fROHxRegion and HoXregion for Uamer & Uarct, create 4 plot panel: Uamer Ho & Uamer fROH one column, Uarct Ho & Uarct fROH other column

#plot Uamer_RegionXHo as violin plot with points
Uamer_RegionXHo <- ggplot(Uamer_Ho, aes(x=Region, y=Heterozygosity, colour=Region)) +
  geom_violin() +         #adding this makes it a violin plot                   
  scale_color_manual(values = c("Southeastern" = "dodgerblue",
                                "Great Lakes" = "skyblue",
                                "SE Alaskan" = "purple",
                                "West" = "magenta",
                                "Louisiana" = "black"))

#violin plot Uamer_RegionXHo with points
Uamer_RegionXHo + geom_jitter(shape=16, position=position_jitter(.001)) + ylim(0.05, .25)

##

#plot Uamer_RegionsXfROH as violin plot w points
Uamer_RegionXfROH <- ggplot(Uamer_ROH, aes(x=Region, y=FROH_genome, colour=Region)) +
  geom_violin() +         #adding this makes it a violin plot                   
  scale_color_manual(values = c("Southeastern" = "dodgerblue",
                                "Great Lakes" = "skyblue",
                                "SE Alaskan" = "purple",
                                "West" = "magenta",
                                "Louisiana" = "black"))
#violin plot Uarct_groupXfROH with points
Uamer_RegionXfROH + geom_jitter(shape=16, position=position_jitter(.001)) + ylim(0, .75)

##

#violin plot of Uarct_groupXHo
Uarct_RegionXHo <- ggplot(Uarct_Ho, aes(x=Region, y=Heterozygosity, colour=Region)) +
  geom_violin() +         #adding this makes it a violin plot                   
  scale_color_manual(values = c("US (lower 48)" = "sienna",
                                "US (Alaska)" = "lightpink4",
                                "Japan" = "plum3",
                                "Europe" = "orange",
                                "Fennoscandia" = "yellow3"))
#violin plot Uarct_groupXHo with points
Uarct_RegionXHo + geom_jitter(shape=16, position=position_jitter(.001)) + ylim(0.05, .25)

##

#plot Uarct_groupXfROH as violin w/ points
Uarct_RegionXfROH <- ggplot(Uarct_ROH, aes(x=Region, y=FROH_genome, colour=Region)) +
  geom_violin() +         #adding this makes it a violin plot                   
  scale_color_manual(values = c("US (lower 48)" = "sienna",
                                "US (Alaska)" = "lightpink4",
                                "Japan" = "plum3",
                                "Europe" = "orange",
                                "Fennoscandia" = "yellow3"))
#violin plot Uarct_groupXfROH violin
Uarct_RegionXfROH + geom_jitter(shape=16, position=position_jitter(.001)) + ylim(0, .75)

####
Uamer_Ho <- Uamer_RegionXHo + geom_jitter(shape=16, position=position_jitter(.001)) + ylim(0.05, .25)
Uamer_fROH <- Uamer_RegionXfROH + geom_jitter(shape=16, position=position_jitter(.001)) + ylim(0, .75)
Uarct_Ho <- Uarct_RegionXHo + geom_jitter(shape=16, position=position_jitter(.001)) + ylim(0.05, .25)
Uarct_fROH <- Uarct_RegionXfROH + geom_jitter(shape=16, position=position_jitter(.001)) + ylim(0, .75)
###


#make a panel of all four plots
HoXfROH <- grid.arrange(Uamer_Ho, Uarct_Ho, Uamer_fROH, Uarct_fROH, ncol=2, nrow =2)

#save as jpeg

save_plot("HoXfROH.png", HoXfROH, base_aspect_ratio = 3)


######################################################################

#load combined data and set as dateframe
#Combined_ROH <- read_excel(" xlsx")
#Combined_Ho <- read_excel("o.xlsx")
Combined_popsumROH <- read_excel("~/RoH/detectRuns/pop_summaries/Ursus_all_popsumROH_HO.xlsx")
View(Combined_popsumROH)
Combined_popsum_HO <- read_excel("~/RoH/detectRuns/pop_summaries/Ursus_all_popsumROH_HO.xlsx",
                                 +     sheet = "Combined_HO")
View(Combined_popsum_HO)


Combined_ROH  <- data.frame(Combined_popsumROH)
Combined_Ho <- data.frame(Combined_popsum_HO)

#plot Uamer_RegionXHo as violin plot with points
Combined_RegionXHo <- ggplot(Combined_Ho, aes(x=Group, y=Heterozygosity, colour=Group)) +
  geom_violin() +         #adding this makes it a violin plot                   
  scale_color_manual(values = c("Polar bear" = "royalblue3",
                                "Brown bear (US Alaska)" = "lightpink",
                                "Brown bear (Europe)" = "orange",
                                "Brown bear (US lower 48)" = "sienna",
                                "Brown bear (Japan)" =              "plum3",
                                "Brown bear (Fennoscandia)" =              "yellow3",
                                "Black bear (SE Alaskan)" =       "purple",
                                "Black bear (Western)" = "magenta",
                                "Black bear (Louisiana)" = "black",
                                "Black bear (Great Lakes)" = "skyblue",
                                "Black bear (Southeastern)" =  "dodgerblue"))


#violin plot Uamer_RegionXHo with points
Combined_RegionXHo + geom_jitter(shape=16, position=position_jitter(.001)) + ylim(0.05, .25)

##

#plot Uamer_RegionsXfROH as violin plot w points
Combined_RegionXfROH <- ggplot(Combined_ROH, aes(x=Group, y=Froh_genome, colour=Group)) +
  geom_violin() +         #adding this makes it a violin plot                   
  scale_color_manual(values = c("Polar bear" = "royalblue3",
                                "Brown bear (US Alaska)" = "lightpink",
                                "Brown bear (Europe)" = "orange",
                                "Brown bear (US lower 48)" = "sienna",
                                "Brown bear (Japan)" =              "plum3",
                                "Brown bear (Fennoscandia)" =              "yellow3",
                                "Black bear (SE Alaskan)" =       "purple",
                                "Black bear (Western)" = "magenta",
                                "Black bear (Louisiana)" = "black",
                                "Black bear (Great Lakes)" = "skyblue",
                                "Black bear (Southeastern)" =  "dodgerblue"))
#violin plot Uarct_groupXfROH with points
Combined_RegionXfROH + geom_jitter(shape=16, position=position_jitter(.001)) + ylim(0, .75)

##
#make a panel of both plots
Combined_HoXfROH <- grid.arrange(Combined_RegionXHo, Combined_RegionXfROH, ncol=1, nrow =2)

#save as jpeg

save_plot("Combined_HoXfROH.png", Combined_HoXfROH, base_aspect_ratio = 3)

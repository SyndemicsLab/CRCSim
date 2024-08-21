library(ggplot2)
library(data.table)

ECNS <- fread("../Data/evenCaptures_noStrata.csv")
ECS <- fread("../Data/evenCaptures_Strata.csv")
UECS <- fread("../Data/unevenCaptures_fiveStrata.csv")
UECNS <- fread("../Data/unevencaptures_noStrata.csv")

makeFig <- function(data) {
  if(length(unique(data$Group)) != 1) stratified <- TRUE else stratified <- FALSE
  DT <- data[, mgt := mean(GroundTruth), by = c("Group", "Method", "Model")]
  
  if(!stratified) {
    plabs <- labs(x = "")
    pticks <- theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank())
  } else {
    plabs <- labs(x = "Group")
    pticks <- theme()
  }
  
  
  p <- ggplot(DT, aes(y = Estimate, x = Group, group = Group)) + 
    geom_boxplot() + 
    geom_point(aes(y = mgt, color = "red")) + 
    guides(color = "none",
           size = "none") + 
    theme_bw() + 
    facet_grid(Method~Model) + 
    plabs + 
    pticks + 
    labs(y = "Estimate\nGround Truth (Red)")
  
  return(p)
}

makeFig(UECNS) + labs(title = "Uneven Capture Probabilities",
                      subtitle = "Total Population Estimate")
ggsave("../Figures/unevenCaptures_noStrata.png")

makeFig(ECNS) + labs(title = "Even Capture Probabilities",
                     subtitle = "Total Population Estimate")
ggsave("../Figures/evenCaptures_noStrata.png")

makeFig(UECS) + labs(title = "Uneven Capture Probabilities",
                     subtitle = "By-group Estimation")
ggsave("../Figures/unevenCptures_strata.png")

makeFig(ECS) + labs(title = "Even Capture Probabilities",
                    subtitle = "Total Population Estimate")
ggsave("../Figures/evenCptures_strata.png")

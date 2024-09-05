library(ggplot2)
library(data.table)

ECNS <- fread("../Data/evenCaptures_noStrata.csv")
ECS <- fread("../Data/evenCaptures_Strata.csv")
UECS <- fread("../Data/unevenCaptures_fiveStrata.csv")
UECNS <- fread("../Data/unevencaptures_noStrata.csv")

makeFig <- function(data) {
  if(length(unique(data$Group)) != 1) stratified <- TRUE else stratified <- FALSE
  DT <- data[, diff := Estimate - GroundTruth]
  
  if(!stratified) {
    plabs <- labs(x = "")
    pticks <- theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank())
  } else {
    plabs <- labs(x = "Group")
    pticks <- theme()
  }
  
  
  p <- ggplot(DT, aes(y = diff, x = Group, group = Group)) + 
    geom_boxplot() + 
    guides(color = "none",
           size = "none") + 
    theme_bw() + 
    facet_grid(Model~Method) + 
    plabs + 
    pticks + 
    labs(y = "Estimate and Ground Truth\nDifference")
  
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
ggsave("../Figures/unevenCptures_strata.png", width = 6, height = 16)

makeFig(ECS) + labs(title = "Even Capture Probabilities",
                    subtitle = "Total Population Estimate")
ggsave("../Figures/evenCptures_strata.png")


avgnobs <- copy(UECS)[, .(gt = mean(GroundTruth)), by = c("Group")]
pct <- copy(UECS)[, pct := (Estimate - GroundTruth)/GroundTruth*100
                  ][, Group := factor(Group, labels = round(avgnobs$gt[order(avgnobs$gt, decreasing = TRUE)]), levels = c(1, 4, 2, 3, 5), ordered = TRUE)]
ggplot(pct, aes(x = Group, y = pct, group = Group)) + 
  geom_boxplot() + 
  theme_bw() + 
  facet_grid(Model~Method) + 
  labs(y = "Percent Difference in\nEstimate Against Ground Truth",
       x = "Ground Truth Population")

ggsave("test.png", width = 8, height = 16)



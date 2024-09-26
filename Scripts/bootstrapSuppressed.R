library(CRCSim)
library(Syndemics)
library(data.table)

set.seed(2024)
nboot <- 1e3
data <- CRCSim:::create.data(3e5, 
                             p_captures = c(0.9, 0.2, 0.05, 0.02, 0.1, 0.4),
                             p_strata = c(.8, .05, 0.05, 0.15, 0.01))
ground_truth <- copy(data)[, tmp := sum(.SD), .SDcols = paste0('capture_', 1:6), by = .I
                           ][tmp == 0, 
                             ][, list(N_ID, strata)]

nboot <- 1e3
config <- list(
  f0.05 = list(direction = "forward", threshold = 0.05),
  f0.1 = list(direction = "forward", threshold = 0.1),
  b0.05 = list(direction = "backward", threshold = 0.05),
  b0.1 = list(direction = "backward", threshold = 0.1),
  fb0.05 = list(direction = "both", threshold = 0.05),
  fb0.1 = list(direction = "both", threshold = 0.1)
)
groups <- unique(data$strata)
boot.out <- c()

for(i in 1:nboot){
  set.seed(2024+i)
  DT <- copy(data)[N_ID <= 10, N_ID := sample(1:10, 1, replace = TRUE)]
  
  pois <- lapply(config, function(x){
    DT.list <- c()
    out.list <- c()
    for(j in seq_along(groups)){
      DT.list[[j]] <- DT[strata == groups[j], ]
    }
    for (j in seq_along(groups)) {
      out.list[[j]] <- Syndemics::crc(DT.list[[j]], "N_ID", paste0("capture_", 1:6), 
                                      method = "poisson", formula.selection = "stepwise", opts.stepwise = c(x, verbose = FALSE))
    }
    
    output <- data.table(gt = ground_truth$N_ID,
                           group = groups,
                           estimates = unlist(lapply(out.list, function(x) return(x$estimate))),
                           model = rep("poisson", length(groups)),
                           direction = rep(x$direction, length(groups)),
                           threshold = rep(x$threshold, length(groups)))
    return(output)
  })
  
  nb <- lapply(config, function(x){
    DT.list <- c()
    out.list <- c()
    for(j in seq_along(groups)){
      DT.list[[j]] <- DT[strata == groups[j], ]
    }
    for (j in seq_along(groups)) {
      out.list[[j]] <- Syndemics::crc(DT.list[[j]], "N_ID", paste0("capture_", 1:6), 
                                      method = "negbin", formula.selection = "stepwise", opts.stepwise = c(x, verbose = FALSE))
    }
    
    output <- data.table(gt = ground_truth$N_ID,
                         group = groups,
                         estimates = unlist(lapply(out.list, function(x) return(x$estimate))),
                         model = rep("NB", length(groups)),
                         direction = rep(x$direction, length(groups)),
                         threshold = rep(x$threshold, length(groups)))
    return(output)
  })
  
  boot.out[[i]] <- rbind(pois, nb)
}
final <- rbindlist(unlist(boot.out, recursive = FALSE))
write.csv(final, "../Data/PHDEmulation.csv", row.names = FALSE)

final <- fread("../Data/PHDEmulation.csv")

ground_truth <- setorderv(ground_truth, "N_ID", order = 1L)
plot_data <- copy(final)[, pct_diff := (estimates - gt) / gt * 100
                         ][, model := ifelse(model == "poisson", "Poisson", "NB")
                           ][, params := paste0(stringr::str_to_title(direction), "-", threshold)
                             ][, group := factor(group, levels = ground_truth$strata, labels = ground_truth$N_ID)]
library(ggplot2)
ggplot(plot_data[!direction %like% "both"], aes(x = group, y = pct_diff, group = group)) + 
  geom_hline(yintercept = 0, col = "red", linetype = "dashed") +
  geom_boxplot() + 
  facet_grid(params~model) + 
  labs(x = "Ground Truth Population",
       y = "Percent Difference in\nEstimate from Ground Truth",
       title = "Single Dataset Bootstrapping",
       subtitle = "Boostrapped Suppression Values ") +
  theme_bw() +
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 22),
        strip.text = element_text(size = 15))
ggsave("../Figures/PHDEmulation.png", width = 8, height = 8)

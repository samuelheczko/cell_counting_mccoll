library(ggplot2)
library(dplyr)  # for data manipulation
library(broom)  # for tidying model output
library(ggpubr)
library(rstatix)
BiocManager::install("lme4")

##define the t-value test function
perform_t_test <- function(df, timepoint) {
  # Filter the DataFrame for the current timepoint
  df_timepoint <- df %>% filter(Timepoint == timepoint)
  
  # Perform t-test
  print(length(df_timepoint$column))
  print(length(df_timepoint$Genotype))
#  print(df_timepoint)
  t_test_result <- t.test(Value ~ Genotype, data = df_timepoint)
  
  # Return the results as a list
  return(list(timepoint = timepoint,
              t_statistic = t_test_result$statistic,
              p_value = t_test_result$p.value))
}


##read in the files
cerebellum_df <- read.csv("../data/cerbellum_cell_counts_PV.csv")
cortex_df <- read.csv("../data/cortex_cell_counts_PV_dapi.csv")

cortex_df[cortex_df == ""] <- NA # Replace blank entries with NA

cortex_df_day <- cortex_df[!is.na(cortex_df$Day), ] 

stat_test = 'wilcox.test' #or 't.test'

# Box plot of PV over NEN cells
nen_plot <- ggboxplot(cortex_df_day, x = "Genotype", y = "nen_density",
                      color = "Genotype", palette = "jco",
                      add = "jitter",
                      facet.by = "Day", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
nen_plot <- nen_plot + 
  stat_compare_means(aes(group = Genotype), method = stat_test) +
  labs(title = "NEN Cells", x = "Genotype",  y = expression("Neurons per" ~ mm^2)) +
  theme(aspect.ratio = 1)  # Adjust the aspect ratio here (1/2 means the height is half the width)



cortex_df <- cortex_df[!is.na(cortex_df$Timepoint), ] ##filter out the datapoints with incomplete info


# Box plot of Purkinje cells
purk_per_mm_plot <- ggboxplot(cerebellum_df, x = "Genotype", y = "pv_per_mm",
                              color = "Genotype", palette = "jco",
                              add = "jitter",
                              facet.by = "Timepoint", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
purk_per_mm_plot <- purk_per_mm_plot + 
  stat_compare_means(aes(group = Genotype), method = stat_test) +
  labs(title = "Purkinje Cells per mm", x = "Genotype", y = "Purkinje Cells per mm")+
  theme(aspect.ratio = 1)  # Adjust the aspect ratio here (1/2 means the height is half the width)



# Box plot of PV neurons density
pv_density_plot <- ggboxplot(cortex_df, x = "Genotype", y = "pv_density",
                             color = "Genotype", palette = "jco",
                             add = "jitter",
                             facet.by = "Timepoint", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
pv_density_plot <- pv_density_plot + 
  stat_compare_means(aes(group = Genotype), method = stat_test) +
  labs(title = "PV Neurons Density", x = "Genotype", y = expression("PV positive neurons per" ~ mm^2))+
  theme(aspect.ratio = 1)  # Adjust the aspect ratio here (1/2 means the height is half the width)
print(pv_density_plot)


# Box plot of DAPI cell density
dapi_density_plot <- ggboxplot(cortex_df, x = "Genotype", y = "dapi_density",
                               color = "Genotype", palette = "jco",
                               add = "jitter",
                               facet.by = "Timepoint", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
dapi_density_plot <- dapi_density_plot + 
  stat_compare_means(aes(group = Genotype), method = stat_test) +
  labs(title = "DAPI Cell Density", x = "Genotype", y = expression("Cell bodies per" ~ mm^2))+
  theme(aspect.ratio = 1)  # Adjust the aspect ratio here (1/2 means the height is half the width)



# Box plot of PV over DAPI cells
pv_over_dapi_plot <- ggboxplot(cortex_df, x = "Genotype", y = "pv.dapi",
                               color = "Genotype", palette = "jco",
                               add = "jitter",
                               facet.by = "Timepoint", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
pv_over_dapi_plot <- pv_over_dapi_plot + 
  stat_compare_means(aes(group = Genotype), method = stat_test) +
  labs(title = "PV over DAPI Cells", x = "Genotype", y = "PV/DAPI Cell counts")+
  theme(aspect.ratio = 1)  # Adjust the aspect ratio here (1/2 means the height is half the width)




# Box plot of PV over NEN cells
pv_over_nen_plot <- ggboxplot(cortex_df, x = "Genotype", y = "pv.nen",
                              color = "Genotype", palette = "jco",
                              add = "jitter",
                              facet.by = "Timepoint", short.panel.labs = FALSE)
# Use only p.format as label. Remove method name.
pv_over_nen_plot <- pv_over_nen_plot + 
  stat_compare_means(aes(group = Genotype), method = stat_test) +
  labs(title = "PV over NEN Cells", x = "Genotype", y = "PV/NeN Cell counts") +
  theme(aspect.ratio = 1)  # Adjust the aspect ratio here



# Open a PDF device
pdf("../data/multiple_plots_wil.pdf")

# Plot and print each plot
print(purk_per_mm_plot)

print(pv_density_plot)

print(dapi_density_plot)

print(pv_over_dapi_plot)

print(pv_over_nen_plot)

print(nen_plot)

# Close the PDF device
dev.off()

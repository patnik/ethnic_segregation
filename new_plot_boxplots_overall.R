


library(dplyr)
library(tidyr)
raw_data <- read.csv("Data_coeff.csv")





liv_data <- subset(raw_data, TCITY15NM_2011 %in% c("London",
                                                   "Birmingham",
                                                   "Liverpool", 
                                                   "Bristol" ,
                                                   "Sheffield",
                                                   "Manchester",
                                                   "Leeds",
                                                   "Leicester",
                                                   "Bradford",
                                                   "Coventry",
                                                   "Nottingham",
                                                   "Newcastle upon Tyne"))





#liv_data2 <- subset(liv_data, quartile_div %in% c("1","4"))
liv_data2 <- liv_data


liv_unem <- subset(liv_data2, t_unempl_cat == "sig")
liv_LTI <- subset(liv_data2, t_LTI_cat == "sig")
liv_dens <- subset(liv_data2, t_density_cat == "sig")
liv_soc_rented <- subset(liv_data2, t_Socrented_cat == "sig")
liv_noqual <- subset(liv_data2, t_Noqual_cat == "sig")
liv_65plus <- subset(liv_data2, t_65plus_cat == "sig")
liv_gp <- subset(liv_data2, t_gp_dist_cat == "sig")
liv_gr_pas <- subset(liv_data2, t_gr_pas_cat == "sig")
liv_sch_rating1 <- subset(liv_data2, t_rating1_cat == "sig")
liv_sch_rating4 <- subset(liv_data2, t_rating4_cat == "sig")
liv_trips_day <- subset(liv_data2, t_trips_day_cat == "sig")
liv_no2 <- subset(liv_data2, t_no2_cat == "sig")


library(ggplot2)
library(viridis)
#scale_color_viridis(discrete=TRUE)
#scale_colour_manual(values = c('red','blue')) +

p1 <- ggplot(liv_unem, aes(x = amb_Unemployed, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "Unemployment",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()

p2 <- ggplot(liv_LTI, aes(x = amb_LTI, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "Long-term ill",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()


  
p3 <- ggplot(liv_dens, aes(x = amb_density, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "Population density",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()

p4 <- ggplot(liv_soc_rented, aes(x = amb_Socrented, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "Social rented housing",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()

p5 <- ggplot(liv_noqual, aes(x = amb_Noqual, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "No qualification",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()


p6 <- ggplot(liv_65plus, aes(x = amb_65plus, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "Age 65+",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()



p7 <- ggplot(liv_gp, aes(x = amb_gpp_dist, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "Access to GP",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()

p8 <- ggplot(liv_gr_pas, aes(x = amb_gr_pas, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "Green space presence",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()


p9 <- ggplot(liv_sch_rating1, aes(x = amb_sch_rating1, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "Best performing school rating",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()

p10 <- ggplot(liv_sch_rating4, aes(x = amb_sch_rating4, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "Worst performing school rating",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()

p11 <- ggplot(liv_trips_day, aes(x = amb_trips_day, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "Bus connectivity",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()



p12 <- ggplot(liv_no2, aes(x = amb_no2, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  scale_fill_viridis(option="magma", discrete=TRUE) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(title = "NO2 levels",
       colour = "Divergence quartile",
       x = "",
       y = "") +
  scale_x_continuous(trans='pseudo_log') +
  theme_minimal()


library(cowplot)

# arrange the three plots in a single row
plots_only <- plot_grid(
  p1 + theme(legend.position="none"),
  p2 + theme(legend.position="none"),
  p3 + theme(legend.position="none"),
  p4 + theme(legend.position="none"),
  p5 + theme(legend.position="none"),
  p6 + theme(legend.position="none"),
  p7 + theme(legend.position="none"),
  p8 + theme(legend.position="none"),
  p9 + theme(legend.position="none"),
  p10 + theme(legend.position="none"),
  p11 + theme(legend.position="none"),
  p12 + theme(legend.position="none"),
  nrow = 3
)
#plots_only

# # extract a legend that is laid out horizontally
# legend_b <- get_legend(
#   p1 + 
#     guides(color = guide_legend(nrow = 1)) +
#     theme(legend.position = "bottom")
# )
# 
# # add the legend underneath the row we made earlier. Give it 10%
# # of the height of one plot (via rel_heights).
# final_plot <- plot_grid(plots_only, legend_b, ncol = 1, rel_heights = c(1, .1))


png("final_plot_boxplots_noclass.png", units="in", width=15, height=10, res=300)
plots_only
dev.off() # Close the file




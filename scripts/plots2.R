library(ggpubr)
library(patchwork)
layout <- "
##AA##
BBCCCC
BBCCCC
BBCCCC
BBCCCC
BBCCCC
BBCCCC
BBCCCC
BBCCCC
BBCCCC
##EE##
"

# Widest diameter ----------------------------------------------------------------
a <- ggline(filter(df,!is.na(Formula)), 
            x = "Day",
            y = "Major",
            add = "mean_sd",
            color = "Supp",
            palette = "lancet",
            linetype = "Supp",
            shape = "Supp", 
            facet.by = c("Line", "Formula"),
            ylab = "Diameter [\u03BCm]") |>
  ggpar(legend.title = "") |>
  facet(facet.by = c("Line", "Formula"))+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())+
  #stat_compare_means(aes(group = Supp),label = "p.signif") 
  stat_compare_means(label = "p.signif", vjust=0.9)

c <- ggline(filter(df,is.na(Formula)), 
            x = "Day",
            y = "Major",
            add = "mean_sd",
            palette = "lancet",
            facet.by = c("Line", "Group"),
            ylab="Diameter [\u03BCm]") |>
  facet(facet.by = c("Line","Group"), nrow = 3, strip.position = "right") |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2)) 

wrap_plots(list(guide_area(), c, a, text_grob("Day")), design = layout, guides="collect") & 
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8)) &
  scale_y_continuous(limits = c(19,134))

ggsave(filename = "figures/diameter-long2.png", device = "png", width = 12, height = 5, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/diameter-long2.svg", device = "svg", width = 12, height = 5, units = "in", scale = 1, dpi=150)

# Shortest diameter ------------------------------------------------------------
a <- ggline(filter(df,!is.na(Formula)), 
            x = "Day",
            y = "Minor",
            add = "mean_sd",
            color = "Supp",
            palette = "lancet",
            linetype = "Supp",
            shape = "Supp", 
            facet.by = c("Line", "Formula"),
            ylab = "Diameter [\u03BCm]") |>
  ggpar(legend.title = "") |>
  facet(facet.by = c("Line", "Formula"))+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())+
  #stat_compare_means(aes(group = Supp),label = "p.signif") 
  stat_compare_means(label = "p.signif", vjust=0.9)

c <- ggline(filter(df,is.na(Formula)), 
            x = "Day",
            y = "Minor",
            add = "mean_sd",
            palette = "lancet",
            facet.by = c("Line", "Group"),
            ylab="Diameter [\u03BCm]") |>
  facet(facet.by = c("Line","Group"), nrow = 3, strip.position = "right") |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2)) 

wrap_plots(list(guide_area(), c, a, text_grob("Day")), design = layout, guides="collect") & 
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8)) &
  scale_y_continuous(limits = c(16,121))

ggsave(filename = "figures/diameter-short2.png", device = "png", width = 12, height = 5, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/diameter-short2.svg", device = "svg", width = 12, height = 5, units = "in", scale = 1, dpi=150)

# Total Area -----------------------------------------------------------------
a <- ggline(filter(df,!is.na(Formula)), 
            x = "Day",
            y = "Total Area",
            add = "mean_sd",
            color = "Supp",
            palette = "lancet",
            linetype = "Supp",
            shape = "Supp", 
            facet.by = c("Line", "Formula"),
            ylab = "Area [\u03BCm^2]") |>
  ggpar(legend.title = "") |>
  facet(facet.by = c("Line", "Formula"))+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())+
  #stat_compare_means(aes(group = Supp),label = "p.signif") 
  stat_compare_means(label = "p.signif", vjust=0.9)

c <- ggline(filter(df,is.na(Formula)), 
            x = "Day",
            y = "Total Area",
            add = "mean_sd",
            palette = "lancet",
            facet.by = c("Line", "Group"),
            ylab="Area [\u03BCm^2]") |>
  facet(facet.by = c("Line","Group"), nrow = 3, strip.position = "right") |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2)) 

wrap_plots(list(guide_area(), c, a, text_grob("Day")), design = layout, guides="collect") & 
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8)) &
  scale_y_continuous(limits = c(280,11800))

ggsave(filename = "figures/area2.png", device = "png", width = 12, height = 5, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/area2.svg", device = "svg", width = 12, height = 5, units = "in", scale = 1, dpi=150)

# Circularity --------------------------------------------------------------------
a <- ggline(filter(df,!is.na(Formula)), 
            x = "Day",
            y = "Circ.",
            add = "mean_sd",
            color = "Supp",
            palette = "lancet",
            linetype = "Supp",
            shape = "Supp", 
            facet.by = c("Line", "Formula"),
            ylab = "Circularity") |>
  ggpar(legend.title = "") |>
  facet(facet.by = c("Line", "Formula"))+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())+
  #stat_compare_means(aes(group = Supp),label = "p.signif") 
  stat_compare_means(label = "p.signif", vjust=0.9)

c <- ggline(filter(df,is.na(Formula)), 
            x = "Day",
            y = "Circ.",
            add = "mean_sd",
            palette = "lancet",
            facet.by = c("Line", "Group"),
            ylab="Circularity") |>
  facet(facet.by = c("Line","Group"), nrow = 3, strip.position = "right") |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2)) 

wrap_plots(list(guide_area(), c, a, text_grob("Day")), design = layout, guides="collect") & 
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8)) &
  scale_y_continuous(limits = c(0.06,0.92))

ggsave(filename = "figures/circ2.png", device = "png", width = 12, height = 5, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/circ2.svg", device = "svg", width = 12, height = 5, units = "in", scale = 1, dpi=150)

# Solidity ---------------------------------------------------------------------
a <- ggline(filter(df,!is.na(Formula)), 
            x = "Day",
            y = "Solidity",
            add = "mean_sd",
            color = "Supp",
            palette = "lancet",
            linetype = "Supp",
            shape = "Supp", 
            facet.by = c("Line", "Formula"),
            ylab = "Solidity") |>
  ggpar(legend.title = "") |>
  facet(facet.by = c("Line", "Formula"))+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())+
  #stat_compare_means(aes(group = Supp),label = "p.signif") 
  stat_compare_means(label = "p.signif", vjust=0.9)

c <- ggline(filter(df,is.na(Formula)), 
            x = "Day",
            y = "Solidity",
            add = "mean_sd",
            palette = "lancet",
            facet.by = c("Line", "Group"),
            ylab="Solidity") |>
  facet(facet.by = c("Line","Group"), nrow = 3, strip.position = "right") |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2)) 

wrap_plots(list(guide_area(), c, a, text_grob("Day")), design = layout, guides="collect") & 
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8)) &
  scale_y_continuous(limits = c(0.48,0.97))

ggsave(filename = "figures/solidity2.png", device = "png", width = 12, height = 5, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/solidity2.svg", device = "svg", width = 12, height = 5, units = "in", scale = 1, dpi=150)


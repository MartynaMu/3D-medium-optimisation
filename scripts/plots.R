library(ggpubr)

comp.days <- list(c("1","4"),
                  c("1", "7"),
                  c("1", "12"),
                  c("4", "7"),
                  c("4", "12"),
                  c("7", "12"))

library(patchwork)
layout <- "
##AAA##
BCCCCCC
BCCCCCC
BCCCCCC
BCCCCCC
BCCCCCC
BCCCCCC
BCCCCCC
BCCCCCC
BCCCCCC
##EEE##
"

# Widest diameter----------------------------------------------------------------
a <- ggline(filter(df,!is.na(Formula)), 
       x = "Day",
       y = "Major",
       add = "mean_sd",
       color = "Line",
       palette = "lancet",
       linetype = "Line",
       shape = "Line", 
       facet.by = c("Supp", "Formula"),
       ylab = "Diameter [\u03BCm]") |>
  ggpar(legend.title = "Cell line") |>
  facet(facet.by = c("Supp", "Formula"))+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) 


c <- ggline(filter(df,is.na(Formula)), 
            x = "Day",
            y = "Major",
            add = "mean_sd",
            color = "Line",
            palette = "lancet",
            linetype = "Line",
            shape = "Line", 
            facet.by = "Group",
            ylab="Diameter [\u03BCm]") |>
  facet(facet.by = "Group", nrow = 3, strip.position = "right") |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2)) 

wrap_plots(list(guide_area(), c, a, text_grob("Day")), design = layout, guides="collect") & 
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8)) &
  scale_y_continuous(limits = c(19,134))

ggsave(filename = "figures/diameter-long.png", device = "png", width = 11, height = 6.5, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/diameter-long.svg", device = "svg", width = 11, height = 6.5, units = "in", scale = 1, dpi=150)

# Shortest diameter------------------------------------------------------------------
a <- ggline(filter(df,!is.na(Formula)), 
       x = "Day",
       y = "Minor",
       add = "mean_sd",
       color = "Line",
       palette = "lancet",
       linetype = "Line",
       shape = "Line", 
       facet.by = c("Supp", "Formula"),
       ylab='Diameter [\u03BCm]') |>
  facet(facet.by = c("Supp", "Formula")) |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

c <- ggline(filter(df,is.na(Formula)), 
            x = "Day",
            y = "Minor",
            add = "mean_sd",
            color = "Line",
            palette = "lancet",
            linetype = "Line",
            shape = "Line", 
            facet.by = "Group",
            ylab="Diameter [\u03BCm]") |>
  facet(facet.by = "Group", nrow = 3, strip.position = "right") |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2))

wrap_plots(list(guide_area(), c, a, text_grob("Day")), design = layout, guides="collect") & 
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8)) & 
  scale_y_continuous(limits = c(18,121))

ggsave(filename = "figures/diameter-short.png", device = "png", width = 11, height = 6.5, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/diameter-short.svg", device = "svg", width = 11, height = 6.5, units = "in", scale = 1, dpi=150)

# Circularity--------------------------------------------------------------------
a <- ggline(filter(df,!is.na(Formula)), 
       x = "Day",
       y = "Circ.",
       add = "mean_sd",
       color = "Line",
       palette = "lancet",
       linetype = "Line",
       shape = "Line", 
       facet.by = c("Supp", "Formula"),
       ylab="Circularity") |>
  facet(facet.by = c("Supp", "Formula")) |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

c <- ggline(filter(df,is.na(Formula)), 
            x = "Day",
            y = "Circ.",
            add = "mean_sd",
            color = "Line",
            palette = "lancet",
            linetype = "Line",
            shape = "Line", 
            facet.by = "Group",
            ylab="Circularity") |>
  facet(facet.by = "Group", nrow = 3, strip.position = "right") |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2)) 

wrap_plots(list(guide_area(), c, a, text_grob("Day")), design = layout, guides="collect") & 
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8)) & 
  scale_y_continuous(limits = c(0.07,0.92))

ggsave(filename = "figures/circularity.png", device = "png", width = 11, height = 6.5, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/circularity.svg", device = "svg", width = 11, height = 6.5, units = "in", scale = 1, dpi=150)

#Solidity----------------------------------------------------------------------
a <- ggline(filter(df,!is.na(Formula)), 
       x = "Day",
       y = "Solidity",
       add = "mean_sd",
       color = "Line",
       palette = "lancet",
       linetype = "Line",
       shape = "Line", 
       facet.by = c("Supp", "Formula"),
       ylab="Solidity") |>
  facet(facet.by = c("Supp", "Formula")) |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

c <- ggline(filter(df,is.na(Formula)), 
       x = "Day",
       y = "Solidity",
       add = "mean_sd",
       color = "Line",
       palette = "lancet",
       linetype = "Line",
       shape = "Line", 
       facet.by = "Group",
       ylab="Solidity") |>
  facet(facet.by = "Group", nrow = 3, strip.position = "right") |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2)) 

wrap_plots(list(guide_area(), c, a, text_grob("Day")), design = layout, guides="collect") & 
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8)) & 
  scale_y_continuous(limits = c(0.48,0.97))

ggsave(filename = "figures/solidity.png", device = "png", width = 11, height = 6.5, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/solidity.svg", device = "svg", width = 11, height = 6.5, units = "in", scale = 1, dpi=150)

# Total area---------------------------------------------------------------------
a <- ggline(filter(df,!is.na(Formula)), 
       x = "Day",
       y = "Total Area",
       add = "mean_sd",
       color = "Line",
       palette = "lancet",
       linetype = "Line",
       shape = "Line", 
       facet.by = c("Supp", "Formula"),
       ylab="Area [\u03BCm^2]") |>
  facet(facet.by = c("Supp", "Formula")) |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

c <- ggline(filter(df,is.na(Formula)), 
            x = "Day",
            y = "Total Area",
            add = "mean_sd",
            color = "Line",
            palette = "lancet",
            linetype = "Line",
            shape = "Line", 
            facet.by = "Group", 
            ylab="Area [\u03BCm^2]") |>
  facet(facet.by = "Group", nrow = 3, strip.position = "right") |>
  ggpar(legend.title = "Cell line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                          linewidth = 0.2,
                                          linetype = 2))

wrap_plots(list(guide_area(), c, a, text_grob("Day")), design = layout, guides="collect") & 
  theme(axis.title.x = element_blank(),axis.text = element_text(size=8)) & 
  scale_y_continuous(limits = c(280,11800))

ggsave(filename = "figures/area.png", device = "png", width = 11, height = 6.5, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/area.svg", device = "svg", width = 11, height = 6.5, units = "in", scale = 1, dpi=150)



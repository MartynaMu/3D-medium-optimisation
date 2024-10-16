filter(df, !grepl(Group, pattern = "FGF"), !grepl(Formula, pattern = "Tumor plus")) %>%
  ggline(x = "Day",
         y = "Solidity",
         color = "Group",
         palette = "lancet",
         linetype = "Group",
         shape = "Group", 
         add = "mean",
         facet.by = "Line")+
  theme(panel.grid.major.y = element_line(color = "black", 
                                        linewidth = 0.2,
                                        linetype = 2))+
  stat_compare_means(aes(group = Group), label = "p.signif", method = "anova")

ggsave(filename = "figures/test.png", device = "png", width = 7, height = 5, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/test.svg", device = "svg", width = 7, height = 5, units = "in", scale = 1, dpi=150)

filter(df, !grepl(Group, pattern = "FGF|CTRL1|CTRL3"), !grepl(Formula, pattern = "Tumor plus")) %>%
  ggline(x = "Day",
         y = "Total Area",
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


my_comp <- list(c("F1", "F2"),
                c("F2", "F3"),
                c("F1", "F3"),
                c("CTRL1", "F1"),
                c("CTRL1", "F2"),
                c("CTRL1", "F3"))

df %>% 
  filter(!grepl(Group, pattern = "FGF"), !grepl(Formula, pattern = "Tumor plus")) %>%
  ggbarplot(x = "Group",
            y = "Solidity",
            ylab = "Solidity",
            palette = "lancet",
            fill = "Group",
            color = "Group",
            position = position_dodge(),
            facet.by = c("Day","Line"),
            short.panel.labs = FALSE,
            add = "mean_sd",
            add.params = list(color = "black")) |>
  facet(facet.by = c("Day","Line"), nrow=4)+
  stat_compare_means(comparisons = my_comp, method = "t.test", label = "p.signif")+
  theme(axis.text = element_text(size=8),
        legend.position = "none")

ggsave(filename = "figures/comp-solidity.png", device = "png", width = 6, height = 14, units = "in", scale = 1, dpi=150)
ggsave(filename = "figures/comp-solidity.svg", device = "svg", width = 6, height = 14, units = "in", scale = 1, dpi=150)

tar_load(fpca_data_loadings)

tt <- bind_rows(fpca_data_loadings[[1]],
                fpca_data_loadings[[3]],
                fpca_data_loadings[[5]],
                fpca_data_loadings[[7]])

tt %>% 
  ggplot(aes(x = state, y = abs(PC1_loading))) +
  # ggplot(aes(x = metric, y = PC1_loading)) + 
  # geom_point() +
  # geom_text_repel() +
  geom_violin(aes(fill=state), trim = TRUE) +
  geom_boxplot(width = 0.2)+
  # theme(legend.position = "none") + 
  # facet_wrap(~state) +
  facet_wrap(~metric) + 
  theme_cowplot() +
  labs(
    x = "",
    y = "Loadings for first fPCA in absolute value") + 
  theme(
    legend.position="none",
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    plot.title = element_text(size = 4),
    plot.subtitle = element_text(size = 3),
    plot.caption = element_text(size = 3),
    axis.title.y = element_text(size = 6),
    axis.text = element_text(size = 6),
    # axis.text.x = element_text(angle = 90),
    axis.title.x = element_text(size = 6),
    strip.text = element_text(size = 6),
    axis.line = element_line(colour = 'black', size = 0.1),
    axis.ticks = element_line(colour = "black", size = 0.1),
    panel.border = element_rect(colour = "grey69", fill=NA, size=0.1),
    panel.grid.major = element_line(colour = "grey69", size=0.1, linetype = "dashed")
  ) +
  guides(colour = guide_legend(override.aes = list(size=1.2)),
         fill = guide_legend(nrow=3, byrow=TRUE)) +
  scale_fill_discrete_qualitative()


# Plot frequencies of items in groups by depth

dfr <- data.frame(
  V1 = c(0.1, 0.2, 0.3, 0.01, 0.5),
  V2 = c(0.2, 0.3, 0.2, 0.01, 0.4),
  V3 = c(0.3, 0.6, 0.5, 0.01, 0.3),
  V4 = c(0.5, 0.1, 0.7, 0.01, 0.1),
  XX = 1:5,
  end_depth = c(1.5, 2.2, 3.9, 4.2, 5.4)
)

# mid points should be   1.5/2, (2.2-1.5)/2 + 1.5
# 0.75, 1.85, 3.05,
# widths should be  2.2 - 1.5
# 1.5, 0.7, 1.7



dfr %>%
  mutate(x_centre = c(end_depth[1]/2, zoo::rollmean(end_depth, 2)),
         x_width = c(end_depth[1], diff(end_depth))) %>%
  gather(variable,
         value,
         -XX,
         -x_centre,
         - x_width) %>%
  filter(variable != "end_depth") %>%
  ggplot(aes(x_centre,
             value,
             fill = variable)) +
  geom_bar(position = "fill",
           stat = "identity",
           aes(width = x_width))

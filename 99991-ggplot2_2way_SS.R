
## this script is meant to be sourced from other scripts ##

###########################################
####### Run code but do not modify ########
###########################################

# regression
step2 <- lm(y ~ z * x, data=dat)

# create figure
data_plot <- probe_interaction(step2, 
                               pred = z, 
                               modx = x, 
                               modx.values = "plus-minus")

# APA Publication Theme
apatheme <- theme_bw(base_family = "Times") +
                theme(
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  axis.line = element_line(),
                  legend.position = legend_loc,
                  legend.background = element_rect(fill = "transparent", color = NA))

# Generate APA 2-Way Interaction Figure
figure_1 <- data_plot$interactplot$data %>%
                ggplot(aes(x = z, y = y, group = modx_group)) +
                geom_line(
                  aes(color = modx_group, linetype = modx_group),
                  linewidth = 1.25) +
                scale_y_continuous(
                  limits = c(y_axis_low, y_axis_high),
                  breaks = seq(y_axis_low, y_axis_high, by = y_increment)) +
                # Restricts the x-axis limits and tick marks strictly to -1SD and +1SD
                scale_x_continuous(
                  limits = z_range,
                  breaks = z_range,
                  labels = z_values) +
                scale_linetype_manual(
                  name = modx_label,
                  labels = modx_values,
                  values = line_types,
                  guide = guide_legend(reverse = TRUE)) +
                scale_colour_manual(
                  name = modx_label,
                  labels = modx_values,
                  values = line_colors,
                  guide = guide_legend(reverse = TRUE)) +
                labs(x = z_label, y = y_label) +
                apatheme


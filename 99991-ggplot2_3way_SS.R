
## this script is meant to be sourced from other scripts ##

###########################################
####### Run code but do not modify ########
###########################################

# regression
step3 <- lm(y ~ z * x * w, data=dat)

# create figure
data_plot <- probe_interaction(step3, 
                               pred = z, 
                               modx = x, 
                               mod2 = w,
                               modx.values = "plus-minus",
                               mod2.values = "plus-minus")

### tells R to provide a plot in APA style, figure is MS ready
apatheme = theme_bw() +
           theme(panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 panel.border=element_blank(),
                 axis.line=element_line(),
                 text=element_text(family='Times'))

# create Panel A
panel_a <- data_plot$interactplot$data %>%
                filter(mod2_group == "Mean of w - 1 SD") %>%
                ggplot(aes(x = z, y = y, group = modx_group)) +
                geom_line(aes(color = modx_group,
                              linetype = modx_group),
                          linewidth = 1.25) +
                scale_y_continuous(limits = c(y_axis_low, y_axis_high),
                                   breaks = seq(y_axis_low, y_axis_high, y_increment)) + 
                labs(subtitle = panel_a_label) +
                ylab(y_label) +
                xlab(z_label) +
                scale_x_discrete(limits = z_range,
                                 labels = z_values) +  
                scale_linetype_manual(name = modx_label,
                                      labels = modx_values,
                                      values = line_types,
                                      guide = guide_legend(reverse = TRUE)) +
                scale_colour_manual(name = modx_label,
                                    labels = modx_values,
                                    values = line_colors,
                                    guide = guide_legend(reverse = TRUE)) +
                apatheme +
                theme(legend.position = legend_loc) +
                scale_fill_grey()

# ggsave('./figures/figure_1_panel_a.png', width=8, height=6, unit='in', dpi=300)

# create Panel B
panel_b <- data_plot$interactplot$data %>%
                filter(mod2_group == "Mean of w + 1 SD") %>%
                ggplot(aes(x = z, y = y, group = modx_group)) +
                geom_line(aes(color = modx_group,
                              linetype = modx_group),
                          linewidth = 1.25) +
                scale_y_continuous(limits = c(y_axis_low, y_axis_high),
                                   breaks = seq(y_axis_low, y_axis_high, y_increment)) + 
                labs(subtitle = panel_b_label) +
                ylab(y_label) +
                xlab(z_label) +
                scale_x_discrete(limits = z_range,
                                 labels = z_values) +  
                scale_linetype_manual(name = modx_label,
                                      labels = modx_values,
                                      values = line_types,
                                      guide = guide_legend(reverse = TRUE)) +
                scale_colour_manual(name = modx_label,
                                    labels = modx_values,
                                    values = line_colors,
                                    guide = guide_legend(reverse = TRUE)) +
                apatheme +
                theme(legend.position = legend_loc) +
                scale_fill_grey()

# combine plots into a single image
panel_a_b <- panel_a / panel_b


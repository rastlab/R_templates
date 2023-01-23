
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

### tells R to provide a plot in APA style, figure is MS ready
apatheme = theme_bw() +
           theme(panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 panel.border=element_blank(),
                 axis.line=element_line(),
                 text=element_text(family='Times'))

# create Panel A
figure_1 <- data_plot$interactplot$data %>%
                ggplot(aes(x = z, y = y, group = modx_group)) +
                geom_line(aes(color = modx_group,
                              linetype = modx_group),
                          linewidth = 1.25) +
                scale_y_continuous(limits = c(y_axis_low, y_axis_high),
                                   breaks = seq(y_axis_low, y_axis_high, y_increment)) + 
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


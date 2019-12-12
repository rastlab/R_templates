
## this script is meant to be sourced from other scripts ##

###########################################
####### Run code but do not modify ########
###########################################

(dat1 = describeBy(dat$dv, list(dat$iv1, dat$iv2), mat = TRUE, digits = 2))
names(dat1)[names(dat1) == 'group1'] = 'iv1'
names(dat1)[names(dat1) == 'group2'] = 'iv2'
dat1$se = dat1$sd/sqrt(dat1$n) # calculates SE for error bars if desired


### tells R to provide a plot in APA style, figure is MS ready
limits = aes(ymax = mean + (1.96*se), ymin=mean - (1.96*se))
dodge = position_dodge(width=0.9)
apatheme=theme_bw() +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'))

### now the actual figure
figure_1 <- ggplot(dat1, aes(x = iv1, y = mean, group = iv2)) +
              geom_bar(stat = "identity", position = "dodge", aes(fill = iv2)) +
              geom_errorbar(limits, position=dodge, width=0.25) +
              coord_cartesian(ylim=c(y_axis_low, y_axis_high)) +  # this is the range of the y-axis
              scale_y_continuous(breaks=seq(y_axis_low, y_axis_high, y_increment)) + # this is the increment ticks on the y-axis
              ylab(y_label) +
              scale_x_discrete(mod_label, labels = mod_values) + 
              guides(fill=guide_legend(title = x_label)) +
              apatheme +
              theme(legend.position = legend_loc) +
              scale_fill_grey()

### now the actual figure
figure_2 <- ggplot(dat1, aes(x = iv2, y = mean, group = iv1)) +
              geom_bar(stat = "identity", position = "dodge", aes(fill = iv1)) +
              geom_errorbar(limits, position=dodge, width=0.25) +
              coord_cartesian(ylim=c(y_axis_low, y_axis_high)) +  # this is the range of the y-axis
              scale_y_continuous(breaks=seq(y_axis_low, y_axis_high, y_increment)) + # this is the increment ticks on the y-axis
              ylab(y_label) +
              scale_x_discrete(mod_label, labels = mod_values) + 
              guides(fill=guide_legend(title = x_label)) +
              apatheme +
              theme(legend.position = legend_loc) +
              scale_fill_grey()


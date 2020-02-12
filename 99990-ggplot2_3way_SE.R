
## this script is meant to be sourced from other scripts ##

###########################################
####### Run code but do not modify ########
###########################################

(dat1 = describeBy(dat$dv, list(dat$iv1, dat$iv2, dat$iv3), mat = TRUE, digits = 2))
names(dat1)[names(dat1) == 'group1'] = 'iv1'
names(dat1)[names(dat1) == 'group2'] = 'iv2'
names(dat1)[names(dat1) == 'group3'] = 'iv3'
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
panel_a <- dat1 %>%
              filter(as.numeric(iv3) == 1) %>%
              ggplot(aes(x = iv1, y = mean, group = iv2)) +
                  geom_bar(stat = "identity", position = "dodge", aes(fill = iv2)) +
                  geom_errorbar(limits, position=dodge, width=0.25) +
                  coord_cartesian(ylim = c(y_axis_low, y_axis_high)) +  # this is the range of the y-axis
                  scale_y_continuous(breaks = seq(y_axis_low, y_axis_high, y_increment)) + # this is the increment ticks on the y-axis
                  labs(subtitle = panel_a_label) +
                  ylab(y_label) +
                  scale_x_discrete(x_label, labels = x_values) + 
                  guides(fill=guide_legend(title = mod_label)) +
                  apatheme +
                  theme(legend.position = legend_loc) +
                  scale_fill_grey()

panel_b <- dat1 %>%
              filter(as.numeric(iv3) == 2) %>%
              ggplot(aes(x = iv1, y = mean, group = iv2)) +
                  geom_bar(stat = "identity", position = "dodge", aes(fill = iv2)) +
                  geom_errorbar(limits, position=dodge, width=0.25) +
                  coord_cartesian(ylim = c(y_axis_low, y_axis_high)) +  # this is the range of the y-axis
                  scale_y_continuous(breaks = seq(y_axis_low, y_axis_high, y_increment)) + # this is the increment ticks on the y-axis
                  labs(subtitle = panel_b_label) +
                  ylab(y_label) +
                  scale_x_discrete(x_label, labels = x_values) + 
                  guides(fill=guide_legend(title = mod_label)) +
                  apatheme +
                  theme(legend.position = legend_loc) +
                  scale_fill_grey()
  
# combine plots into a single image
panel_a_b <- panel_a / panel_b




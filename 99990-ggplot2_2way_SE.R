###############################################
######### Plotting 2-way interaction ##########
###############################################

# the follow commands will create APA-style, MS ready figures

### create x, z, and y columns by renaming IVs
dat$iv1 <- dat$YOUR_IV1_NAME_HERE       # x-axis variable here
dat$iv2 <- dat$YOUR_IV2_NAME_HERE       # moderator_1 variable here
dat$dv  <- dat$YOUR_DV_NAME_HERE        # outcome variable here


## create labels for figure
# must use quotes for labels
# change labels in quotes to be what you want them to be

y_label        <- "dv_name"
y_axis_high    <- 9.0                    # high descrete numeric value displayed on y-axis
y_axis_low     <- 1.0                    # low descrete numeric value displayed on y-axis
y_increment    <- 1.0                    # increments for y-axis
x_label        <- "x-axis_label"         # x-axis variable (non-moderator)
x_values       <- c("low", "high")       # x-axis values
mod_label     <- "x_moderator_name"     # figure legend (moderator)
mod_values    <- c("low", "high")       # moderator values


# change legend location if needed
# in the format of (x,y), can be any number between 0 and 1
# (0.8, 0.8) is upper-right corner, (0.2, 0.8) is the top left of the figure
legend_loc    <- c(0.8, 0.8)

# run next line and figure will be automatically created
# source("https://raw.githubusercontent.com/rastlab/R_templates/master/99991-ggplot2_2way_SS.R")


#####################################
####### Here are the figures ########
#####################################

# verify figures look similar to the probe_interaction() commands at line 146 or 151
figure_1

# if you are happy with figures, save them
# can change dimensions, file type, and dpi as per journal requirement specifications
ggsave('./figures/figure_1.png', panel_a_b, width = 8, height = 8, unit = 'in', dpi = 320)



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
              scale_x_discrete(x_label, labels = x_values) + 
              guides(fill=guide_legend(title = mod_label)) +
              apatheme +
              theme(legend.position = legend_loc) +
              scale_fill_grey()


######################################
####### Here are your figures ########
######################################

# figure_1
# ggsave('./figures/figure_1.png', width=8, height=8, unit='in', dpi=300)





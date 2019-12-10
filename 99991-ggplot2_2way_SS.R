#####################################
### Import data & load libraries ####
#####################################

# load packages
# pacman::p_load(rio, pequod, tidyverse, janitor, jtools, interactions, ggstance, patchwork)

# import data
# dat <- import(file.choose())

# #####################################################################################
# ######################## Simple Slope Testing Automatically #########################
# #####################################################################################
# 
# ### create x, z, and y columns by renaming IVs
# dat$z <- NON-MODERATOR_HERE # x-axis variable here
# dat$x <- MODERATOR_HERE     # moderator_1 variable here
# dat$y <- DV_HERE            # outcome variable here
# 
# 
# ## create labels for figure
# # must use quotes for labels
# # change labels in quotes to be what you want them to be
# 
# y_label        <- "dv_name"
# y_range        <- c(-1.0, 1.0)           # desired numeric range of y-axis
# y_axis_high    <- 1.0                    # high descrete numeric value displayed on y-axis
# y_axis_low     <- -1.0                    # low descrete numeric value displayed on y-axis
# z_label        <- "z_non-moderator_name" # X-axis variable (non-moderator)
# z_values       <- c("low", "high")       # non-moderator values
# z_range        <- c(-1.0, 1.0)           # non-moderator numerical values
# modx_label     <- "x_moderator_name"     # figure legend (moderator)
# modx_values    <- c("low", "high")       # moderator values
# 
# # run but only modify these if needed
# # line type and color are set for APA 7ed
# line_types    <- c("longdash", "solid")   # can be “solid”, “dashed”, “dotted”, “dotdash”, “longdash”, “twodash”
# line_colors   <- c("#9E9E9E", "#000000")  # these are colorblind friendly color schemes, don't change unless needed
# 
# # change legend location if needed
# # in the format of (x,y), can be any number between 0 and 1
# # (0.8, 0.8) is upper-right corner, (0.2, 0.8) is the top left of the figure
# legend_loc    <- c(0.2, 0.8)

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
                          size = 1.25) +
                scale_y_continuous(limits = y_range,
                                   breaks = seq(y_axis_low, y_axis_high)) + 
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

# ggsave('./figures/figure_1_panel_b.png', width=8, height=6, unit='in', dpi=300)

######################################
####### Here are your figures ########
######################################

# figure_1
# ggsave('./figures/figure_1.png', width=8, height=8, unit='in', dpi=300)





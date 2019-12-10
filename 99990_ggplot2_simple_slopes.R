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
# ### create x, z, w, and y columns by renaming IVs
# dat$z <- NON-MODERATOR_HERE # x-axis variable here
# dat$x <- MODERATOR_HERE     # moderator_1 variable here
# dat$w <- PANEL_HERE         # moderator_2 variable here
# dat$y <- DV_HERE            # outcome variable here
#
# ## create labels for figure
# # must use quotes for labels
# # change labels to be what you want them to be
# 
# panel_a_label  <- "Panel A: YYY"         # panel A = low moderator
# panel_b_label  <- "Panel B: ZZZ"         # panel B = high moderator
# y_label        <- "dv_name"
# y_range        <- c(-1.0, 1.0)           # desired numeric range of y-axis
# y_axis_high    <- 1.0                    # high descrete numeric value displayed on y-axis
# y_axis_low     <- -1.0 
# z_label        <- "z_non-moderator_name" # X-axis variable (non-moderator)
# z_values       <- c("low", "high")       # non-moderator values
# modx_label     <- "x_moderator_name"     # figure legend (moderator)
# modx_values    <- c("high", "low")       # moderator values
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
# 
### create simple slopes using 'pequod'
model1 <- na.omit(lmres(y ~ z * x * w, data=dat))

# iv1 as non-moderator, iv2 and iv3 as moderators
s_slopes1 <- na.omit(simpleSlope(model1, pred = "z", mod1 = "x", mod2 = "w"))

# create dataframe for plotting
data_plot <- as_tibble(s_slopes1$Points) %>% 
             janitor::clean_names() %>% 
             rename(low  = starts_with("low"),
                   high = starts_with("high")) %>% 
             add_column(x = c(1, 1, 2, 2), 
                       w = c(1, 2, 1, 2)) %>%
             tidyr::pivot_longer(
              cols = low:high,
              names_to = "z",
              values_to = "y") %>%
              mutate(z = as.numeric(recode(z,"low" = "1", "high" = "2")),
                   x = factor(x, levels=c(1, 2), labels=c("low", "high")),
                   w = factor(w, levels=c(1, 2), labels=c("low", "high")),
                   z = factor(z, levels=c(1, 2), labels=c("low", "high")))

### tells R to provide a plot in APA style, figure is MS ready
apatheme = theme_bw() +
           theme(panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),
                 panel.border=element_blank(),
                 axis.line=element_line(),
                 text=element_text(family='Times'))

# create Panel A
panel_a <- data_plot %>%
                filter(str_detect(w, "low")) %>%
                ggplot(aes(x = z, y = y, group = x)) +
                geom_line(aes(color = x,
                              linetype = x),
                          size = 1.25) +
                coord_cartesian(ylim = y_range, xlim = c(1.3, 1.7)) +  
                scale_y_continuous(breaks = seq(y_axis_low, y_axis_high)) + 
                labs(subtitle = panel_a_label) +
                ylab(y_label) +
                xlab(z_label) +
                scale_x_discrete(labels = z_values) +  
                scale_linetype_manual(name = modx_label,
                                      breaks = modx_values,
                                      labels = modx_values,
                                      values = line_types) +
                scale_colour_manual(name = modx_label,
                                    breaks = modx_values,
                                    labels = modx_values,
                                    values = line_colors) +
                apatheme +
                theme(legend.position = legend_loc) +
                scale_fill_grey()

ggsave('./figures/figure_1_panel_a_pequod.png', width=8, height=6, unit='in', dpi=300)

# create Panel B
panel_b <- data_plot %>%
                filter(str_detect(w, "high")) %>%
                ggplot(aes(x = z, y = y, group = x)) +
                geom_line(aes(color = x,
                              linetype = x),
                          size = 1.25) +
                coord_cartesian(ylim = y_range, xlim = c(1.3, 1.7)) +  
                scale_y_continuous(breaks = seq(y_axis_low, y_axis_high)) + 
                labs(subtitle = panel_b_label) +
                ylab(y_label) +
                xlab(z_label) +
                scale_x_discrete(labels = z_values) +  
                scale_linetype_manual(name = modx_label,
                                      breaks= modx_values,
                                      labels = modx_values,
                                      values = line_types) +
                scale_colour_manual(name = modx_label,
                                    breaks = modx_values,
                                    labels = modx_values,
                                    values = line_colors) +
                apatheme +
                theme(legend.position = legend_loc) +
                scale_fill_grey()

ggsave('./figures/figure_1_panel_b_pequod.png', width=8, height=6, unit='in', dpi=300)

######################################
####### Here are your figures ########
######################################

# panel_a
# panel_b 

figure_1 <- panel_a / panel_b
ggsave('figure_1_pequod.png', width=8, height=8, unit='in', dpi=300)


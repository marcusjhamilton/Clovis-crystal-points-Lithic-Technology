#install the required libraries
library(readxl);library(dplyr);library(ggplot2);library(ggforce);
library(latticeExtra);library(egg);library(spaMM);library(ggrepel);library(rmarkdown);library(rcompanion);
library(BSDA);library(MASS);library(car);library(tidyverse);library(ggpubr);library(rstatix);
library(ggspatial);library(maps);library(ggforce);library(rgeos);library(sf);library(rnaturalearth);
library(rnaturalearthdata);library(scales);library(emmeans);library(raster); library(stargazer);
library(spaMM);library(ggpubr); library(xtable);library(stargazer); library(emmeans); library(xtable);
library(ggsignif); library(ggpubr); library(emmeans);library(ggplot2)

setwd("C:/Users/...") #laptop
crystal_data <- read_excel("C:/Users/.../crystal_data.xlsx", guess_max = 10000)
warp_data <- read_excel("C:/Users/.../warps.xlsx", guess_max = 10000)

#For xps13
#setwd("C:/Users/marcu/OneDrive/R_scripts/Crystal_ppts") #laptop
#crystal_data <- read_excel("C:/Users/marcu/OneDrive/R_scripts/Crystal_ppts/crystal_data.xlsx", guess_max = 10000)

#RM for fine-grained, RM2 for coarse-grained
l_plot<-ggplot(data = crystal_data, mapping = aes(x = logV, y = logL, color=RM3)) + 
  geom_point(size = 2, alpha = 1) + #, color = 'blue'
  xlab("log(Volume)") +
  ylab("log(Length)") +
  #xlim(0,15) +
  #ylim(0,5) +
  geom_smooth(method = lm, fill = "light grey", alpha = 0.4) + #, color = 'red'
  stat_regline_equation(label.y.npc = .99, label.x.npc = .01, aes(label =  paste(after_stat(eq.label), ..rr.label.., 
                                                                                sep = "~~")), size = 3, show.legend = FALSE,) +
  theme_bw() +
  theme(aspect.ratio = 2/3) +
  theme(axis.title = element_text())

w_plot<-ggplot(data = crystal_data, mapping = aes(x = logV, y = logW, color=RM3)) + #Tsig/100
  geom_point(size = 2, alpha = 1) + #, color = 'blue'
  xlab("log(Volume)") +
  ylab("log(Width)") +
  #xlim(0,15) +
  #ylim(0,5) +
  geom_smooth(method = lm, fill = "light grey", alpha = 0.4) + #, color = 'red'
  stat_regline_equation(label.y.npc = .99, label.x.npc = .01, aes(label =  paste(after_stat(eq.label), ..rr.label.., 
                                                                               sep = "~~")), size = 3, show.legend = FALSE,) +
  theme_bw() +
  theme(aspect.ratio = 2/3) +
  theme(axis.title = element_text())

t_plot<-ggplot(data = crystal_data, mapping = aes(x = logV, y = logT, color=RM3)) + #Tsig/100
  geom_point(size = 2, alpha = 1) + #, color = 'blue'
  xlab("log(Volume)") +
  ylab("log(Thickness)") +
  #xlim(0,15) +
  #ylim(0,5) +
  geom_smooth(method = lm, fill = "light grey", alpha = 0.4) + #, color = 'red'
  stat_regline_equation(label.y.npc = .99, label.x.npc = .01, aes(label =  paste(after_stat(eq.label), ..rr.label.., 
                                                                               sep = "~~")), size = 3, show.legend = FALSE,) +
  theme_bw() +
  theme(aspect.ratio = 2/3) +
  theme(axis.title = element_text())


scaling_plots <- egg::ggarrange(l_plot, w_plot, t_plot, 
                             labels = c("a", "b", "c"),
                             label.args = list(gp = grid::gpar(font = 2, cex = 1.5)),
                             ncol = 3, nrow = 1)
scaling_plots

pdf("scaling_plots.pdf")
print(scaling_plots)
dev.off()


#violin plots
comparisons_RM3 <- list( c("Other", "Qtz. crystal"))
l_RM3<-ggplot(crystal_data, aes(RM3, logL, fill=RM3)) + 
  geom_jitter(position=position_jitter(0.1), alpha = 0.8, aes(color = RM3)) +
  geom_violin(alpha = 0.3) +
  geom_boxplot(color = "white", width = 0.1, alpha = 0.3) +
  #geom_signif(comparisons = list(c("versicolor", "virginica")), map_signif_level = TRUE) +
  guides(color = "none") +
  guides(fill = "none") +
  labs(
    y = "log(Length, mm)") +
  ylim(NA, 2.7) +
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #labels = trans_foRM3at("log10", math_foRM3at(10^.x))) +
  #scale_y_continuous(trans = log10_trans(), labels = number) +
  #scale_x_continuous(trans = log10_trans()) +
  #ylim(1, 850) +
  theme_bw() +
  stat_compare_means(method="t.test", label.y = 2.5)+ # Add pairwise comparisons p-value, , label.y = c(7, 8, 9)
  theme(axis.title.x = element_blank(),
        text = element_text(size = 12)) +
  theme(aspect.ratio = 2/3)

w_RM3<-ggplot(crystal_data, aes(RM3, logW, fill=RM3)) + 
  geom_jitter(position=position_jitter(0.1), alpha = 0.8, aes(color = RM3)) +
  geom_violin(alpha = 0.3) +
  geom_boxplot(color = "white", width = 0.1, alpha = 0.3) +
  guides(color = "none") +
  guides(fill = "none") +
  labs(
    y = "log(Width, mm)") +
  ylim(NA, 2.0) +
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #labels = trans_foRM3at("log10", math_foRM3at(10^.x))) +
  #scale_y_continuous(trans = log10_trans(), labels = number) +
  #scale_x_continuous(trans = log10_trans()) +
  #ylim(1, 850) +
  theme_bw() +
  stat_compare_means(method="t.test", label.y = 1.9)+ # Add pairwise comparisons p-value, , label.y = c(7, 8, 9)
  theme(axis.title.x = element_blank(),
        text = element_text(size = 12)) +
  theme(aspect.ratio = 2/3)

t_RM3<-ggplot(crystal_data, aes(RM3, logT, fill=RM3)) + 
  geom_jitter(position=position_jitter(0.1), alpha = 0.8, aes(color = RM3)) +
  geom_violin(alpha = 0.3) +
  geom_boxplot(color = "white", width = 0.1, alpha = 0.3) +
  guides(color = "none") +
  guides(fill = "none") +
  labs(
    y = "log(Thickness, mm)") +
  ylim(NA, 1.3) +
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #labels = trans_foRM3at("log10", math_foRM3at(10^.x))) +
  #scale_y_continuous(trans = log10_trans(), labels = number) +
  #scale_x_continuous(trans = log10_trans()) +
  #ylim(1, 850) +
  theme_bw() +
  stat_compare_means(method="t.test", label.y = 1.2)+ # Add pairwise comparisons p-value, , label.y = c(7, 8, 9)
  theme(axis.title.x = element_blank(),
        text = element_text(size = 12)) +
  theme(aspect.ratio = 2/3)

v_RM3<-ggplot(crystal_data, aes(RM3, logV, fill=RM3)) + 
  geom_jitter(position=position_jitter(0.1), alpha = 0.8, aes(color = RM3)) +
  geom_violin(alpha = 0.3) +
  geom_boxplot(color = "white", width = 0.1, alpha = 0.3) +
  guides(color = "none") +
  guides(fill = "none") +
  labs(
    y = "log(Volume, mm^3)") +
  ylim(NA, 6) +
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #labels = trans_foRM3at("log10", math_foRM3at(10^.x))) +
  #scale_y_continuous(trans = log10_trans(), labels = number) +
  #scale_x_continuous(trans = log10_trans()) +
  #ylim(1, 850) +
  theme_bw() +
  stat_compare_means(method="t.test", label.y = 5.5)+ # Add pairwise comparisons p-value, , label.y = c(7, 8, 9)
  theme(axis.title.x = element_blank(),
        text = element_text(size = 12)) +
  theme(aspect.ratio = 2/3)

freqs <- egg::ggarrange(l_RM3, w_RM3, t_RM3, v_RM3,
                             labels = c("a", "b", "c", "d"),
                             label.args = list(gp = grid::gpar(font = 2, cex = 1.5)),
                             ncol = 2, nrow = 2)
freqs

pdf("freqs.pdf")
print(freqs)
dev.off()

#ANOVAs and post-hoc
cd_anova <- lm(data=crystal_data, logL ~ RM)
summary(cd_anova)

#check the distribution of residuals
hist(residuals(cd_anova))

#Shapiro-wilks normality test of residuals
shapiro.test(residuals(cd_anova))

#plot homoscedasticity of residuals
plot(fitted(cd_anova),
     residuals(cd_anova))

#run four plots of residuals, fitted values, and leverage
par(mfrow = c(2, 2))
plot(cd_anova)

#run pairwise comparisons between raw materials
posthoc <- emmeans(cd_anova,
                   pairwise ~ RM,
                   p.adjust.method = "tukey")
posthoc

#general linear model fits
length_fit <- lm(data=crystal_data, logL ~ logV*RM3)
summary(length_fit)
coef(length_fit)
confint(length_fit)
marg_length <- emtrends(length_fit, var="logV")
summary(marg_length)
pairs(marg_length)
margtable_length <- xtable(marg_length)
print(marg_length, type = "html")

#Width
width_fit <- lm(data=crystal_data, logW ~ logV*RM3)
summary(width_fit)
coef(width_fit)
confint(width_fit)
marg_width <- emtrends(width_fit, var="logV")
summary(marg_width)
pairs(marg_width)
margtable_width<- xtable(marg_width)
print(marg_width, type = "html")

#Thickness
thick_fit <- lm(data=crystal_data, logT ~ logV*RM3)
summary(thick_fit)
coef(thick_fit)
confint(thick_fit)
marg_thick <- emtrends(thick_fit, pairwise ~ RM3, var='logV')
summary(marg_thick)
pairs(marg_thick)
margtable_thick <- xtable(marg_thick)
print(marg_thick, type = "html")


models <- list(length_fit, width_fit, thick_fit)
stargazer(
  models,
  type = "text",
  keep.stat = c("n", "rsq", "adj.rsq"),
  title = "Volume scaling models",
  out = "volume2.html"
)

######################################################################################

#warp plot
warp1_plot<-ggplot(data = warp_data, mapping = aes(x = Length, y = RW1, color=RM)) + 
  geom_point(size = 3, alpha = 1) + #, color = 'blue'
  xlab("log(length)") +
  ylab("Warp 1") +
  #xlim(0,15) +
  #ylim(0,5) +
  #geom_smooth(method = lm, fill = "light grey", alpha = 0.4) + #, color = 'red'
  #stat_regline_equation(label.y.npc = .99, label.x.npc = .01, aes(label =  paste(after_stat(eq.label), ..rr.label.., 
   #                                                                              sep = "~~")), size = 3, show.legend = FALSE,) +
  theme_bw() +
  theme(aspect.ratio = 2/3) +
  theme(axis.title = element_text())
warp1_plot

pdf("warp1_plot.pdf")
print(warp1_plot)
dev.off()

warp2_plot<-ggplot(data = warp_data, mapping = aes(x = Length, y = RW2, color=RM)) + 
  geom_point(size = 3, alpha = 1) + #, color = 'blue'
  xlab("log(length)") +
  ylab("log(Warp 2)") +
  #xlim(0,15) +
  #ylim(0,5) +
  #geom_smooth(method = lm, fill = "light grey", alpha = 0.4) + #, color = 'red'
  #stat_regline_equation(label.y.npc = .99, label.x.npc = .01, aes(label =  paste(after_stat(eq.label), ..rr.label.., 
  #                                                                              sep = "~~")), size = 3, show.legend = FALSE,) +
  theme_bw() +
  theme(aspect.ratio = 2/3) +
  theme(axis.title = element_text())

warp3_plot<-ggplot(data = warp_data, mapping = aes(x = Length, y = RW3, color=RM)) + 
  geom_point(size = 3, alpha = 1) + #, color = 'blue'
  xlab("log(length)") +
  ylab("log(Warp 3)") +
  #xlim(0,15) +
  #ylim(0,5) +
  #geom_smooth(method = lm, fill = "light grey", alpha = 0.4) + #, color = 'red'
  #stat_regline_equation(label.y.npc = .99, label.x.npc = .01, aes(label =  paste(after_stat(eq.label), ..rr.label.., 
  #                                                                              sep = "~~")), size = 3, show.legend = FALSE,) +
  theme_bw() +
  theme(aspect.ratio = 2/3) +
  theme(axis.title = element_text())

warp4_plot<-ggplot(data = warp_data, mapping = aes(x = Length, y = RW4, color=RM)) + 
  geom_point(size = 3, alpha = 1) + #, color = 'blue'
  xlab("log(length)") +
  ylab("log(Warp 4)") +
  #xlim(0,15) +
  #ylim(0,5) +
  #geom_smooth(method = lm, fill = "light grey", alpha = 0.4) + #, color = 'red'
  #stat_regline_equation(label.y.npc = .99, label.x.npc = .01, aes(label =  paste(after_stat(eq.label), ..rr.label.., 
  #                                                                              sep = "~~")), size = 3, show.legend = FALSE,) +
  theme_bw() +
  theme(aspect.ratio = 2/3) +
  theme(axis.title = element_text())

warp_plots <- egg::ggarrange(warp1_plot, warp2_plot, warp3_plot, warp4_plot,
                                labels = c("a", "b", "c", "d"),
                                label.args = list(gp = grid::gpar(font = 2, cex = 1.5)),
                                ncol = 2, nrow = 2)
warp_plots

pdf("warp_plots.pdf")
print(warp_plots)
dev.off()

varplot1 <- ggplot(data = varmeans1gen, aes(y = variance, x = rate, color = pressure)) + 
  stat_smooth(size = 0.5, span = 0.2) +
  theme_linedraw() +
  #geom_point(shape = varmeans1genloglimit$pressure, size = 2.5, alpha = 0.7) +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1))
varplot1

varplot2 <- ggplot(data = varmeans2gen, aes(y = variance, x = rate, color = pressure)) + 
  stat_smooth(size = 0.5, span = 0.2) +
  theme_linedraw() +
  #geom_point(shape = varmeans2genloglimit$pressure, size = 2.5, alpha = 0.7) +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1))
varplot2

varplot5 <- ggplot(data = varmeans5gen, aes(y = variance, x = rate, color = pressure)) + 
  stat_smooth(size = 0.5, span = 0.2) +
  theme_linedraw() +
  #geom_point(shape = varmeans5genloglimit$pressure, size = 2.5, alpha = 0.7) +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1))
varplot5

varplot10 <- ggplot(data = varmeans10gen, aes(y = variance, x = rate, color = pressure)) + 
  stat_smooth(size = 0.5, span = 0.2) +
  theme_linedraw() +
  #geom_point(shape = varmeans10genloglimit$pressure, size = 2.5, alpha = 0.7) +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1))
varplot10

varplot100 <- ggplot(data = varmeans100gen, aes(y = variance, x = rate, color = pressure)) + 
  stat_smooth(size = 0.5, span = 0.2) +
  theme_linedraw() +
  #geom_point(shape = varmeans100genloglimit$pressure, size = 2.5, alpha = 0.7) +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1))
varplot100

varplot500 <- ggplot(data = varmeans500gen, aes(y = variance, x = rate, color = pressure)) + 
  stat_smooth(size = 0.5, span = 0.25) +
  theme_linedraw() +
  #geom_point(shape = varmeans500genloglimit$pressure, size = 2.5, alpha = 0.7) +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1))
varplot500


library(ggpubr)
all <- ggarrange(varplot1 + ylim(0, 8) + theme(legend.position="none", axis.title.y=element_blank()), varplot2 + ylim(0, 8) + theme(legend.position="none", axis.title.y=element_blank()), varplot5 + ylim(0, 8) + theme(legend.position="none", axis.title.y=element_blank()), varplot10 + ylim(0, 8) + theme(legend.position="none", axis.title.y=element_blank()), varplot100 + ylim(0, 8) + theme(legend.position="none", axis.title.y=element_blank()), varplot500 + ylim(0, 8) + theme(legend.position="none", axis.title.y=element_blank()), ncol = 6, nrow = 1, widths = c(3, 3, 3, 3))

all



##########Plots with SE BARS
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                        conf.interval=.95, .drop=TRUE) {
    library(plyr)
    
    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
      if (na.rm) sum(!is.na(x))
      else       length(x)
    }
    
    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun = function(xx, col) {
                     c(N    = length2(xx[[col]], na.rm=na.rm),
                       mean = mean   (xx[[col]], na.rm=na.rm),
                       sd   = sd     (xx[[col]], na.rm=na.rm)
                     )
                   },
                   measurevar
    )
    
    # Rename the "mean" column    
    datac <- rename(datac, c("mean" = measurevar))
    
    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
    
    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult
    
    return(datac)
  }
  

varplot1se <- ggplot(varsum1, aes(x=log(rate), y=variance, colour=pressure, group=pressure)) + 
  geom_errorbar(aes(ymin=variance-ci, ymax=variance+ci), colour="black", width=.1) +
  geom_line(size = 0.8) +
  #geom_point(shape = varsum1$pressure) + 
  theme_pubr() + theme(axis.title.x = element_blank()) + ggtitle("1 Generation") + scale_fill_discrete(name = "Pressure") + scale_x_continuous(limits = c(-6,0), breaks = c(-6, -5, -4, -3, -2, -1, 0))
varplot2se <- ggplot(varsum2, aes(x=log(rate), y=variance, colour=pressure, group=pressure)) + 
  geom_errorbar(aes(ymin=variance-ci, ymax=variance+ci), colour="black", width=.1) +
  geom_line(size = 0.8) +
 #geom_point(shape = varsum5$pressure) + 
  theme_pubr() + theme(axis.title.x = element_blank()) + ggtitle("2 Generations") + scale_x_continuous(limits = c(-6,0), breaks = c(-6, -5, -4, -3, -2, -1, 0))
varplot5se <- ggplot(varsum5, aes(x=log(rate), y=variance, colour=pressure, group=pressure)) + 
  geom_errorbar(aes(ymin=variance-ci, ymax=variance+ci), colour="black", width=.1) +
  geom_line(size = 0.8) +
  #geom_point(shape = varsum5$pressure) + 
  theme_pubr() + theme(axis.title.x = element_blank()) + ggtitle("5 Generations") + scale_x_continuous(limits = c(-6,0), breaks = c(-6, -5, -4, -3, -2, -1, 0))
varplot10se <- ggplot(varsum10, aes(x=log(rate), y=variance, colour=pressure, group=pressure)) + 
  geom_errorbar(aes(ymin=variance-ci, ymax=variance+ci), colour="black", width=.1) +
  geom_line(size = 0.8) +
  #geom_point(shape = varsum10$pressure) + 
  theme_pubr() + theme(axis.title.x = element_blank()) + ggtitle("10 Generations") + scale_x_continuous(limits = c(-6,0), breaks = c(-6, -5, -4, -3, -2, -1, 0))
varplot100se <- ggplot(varsum100, aes(x=log(rate), y=variance, colour=pressure, group=pressure)) + 
  geom_errorbar(aes(ymin=variance-ci, ymax=variance+ci), colour="black", width=.1) +
  geom_line(size = 0.8) +
  #geom_point(shape = varsum100$pressure) + 
  theme_pubr() + theme(axis.title.x = element_blank()) + ggtitle("100 Generations") + scale_x_continuous(limits = c(-6,0), breaks = c(-6, -5, -4, -3, -2, -1, 0))
varplot500se <- ggplot(varsum500, aes(x=log(rate), y=variance, colour=pressure, group=pressure)) + 
  geom_errorbar(aes(ymin=variance-ci, ymax=variance+ci), colour="black", width=.1) +
  geom_line(size = 0.8) +
  #geom_point(shape = varsum500$pressure) + 
  theme_pubr() + theme(axis.title.x = element_blank()) + ggtitle("500 Generations") + scale_x_continuous(limits = c(-6,0), breaks = c(-6, -5, -4, -3, -2, -1, 0))



allse <- ggarrange(varplot1se + ylim(0, 8.5) + theme(legend.position=c(0.2,0.55), axis.title.y=element_blank()), varplot2se + ylim(0, 8.5) + theme(legend.position="none", axis.title.y=element_blank()), varplot5se + ylim(0, 8.5) + theme(legend.position="none", axis.title.y=element_blank()), varplot10se + ylim(0, 8.5) + theme(legend.position="none", axis.title.y=element_blank()), varplot100se + ylim(0, 8.5) + theme(legend.position="none", axis.title.y=element_blank()), varplot500se + ylim(0, 8.5) + theme(legend.position="none", axis.title.y=element_blank()), ncol = 3, nrow = 2, widths = c(3, 3, 3, 3))

allse




#####################
##### gganimate #####
#####################
library(gganimate)
varplotall <- ggplot(data = varmeansall, aes(y = variance, x = cin, color = pressure)) + 
  stat_smooth(size = 0.5, span = 0.5) +
  theme_linedraw() +
  geom_point(shape = varmeansall$pressure, size = 2.5, alpha = 0.7) +
  theme(axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  scale_x_continuous(breaks = seq(0, 10, by = 1)) + 
  transition_states(gens, transition_length = 2, state_length= 1)
varplotall




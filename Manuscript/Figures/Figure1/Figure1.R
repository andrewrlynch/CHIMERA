# Figure 1
# Chromosome scores

Chromosomes <- seq(c(1:22,"X"))
Density <- c(0.026093553,
             0.066078754,
             0.082204684,
             0.047048825,
             0.043400188,
             0.027703262,
             0.146772851,
             0.087237865,
             0.063016249,
             0.001590754,
             0.052299334,
             0.112820145,
             -0.01910046,
             0.047536965,
             0.038189516,
             0.036985608,
             0.052848554,
             -0.034375379,
             0.049005781,
             0.088830153,
             -0.006184584,
             -0.010002619,
             0)

Abundance <- c(0.095,
               0.072,
               0.056,
               0.046,
               0.048,
               0.056,
               0.052,
               0.04,
               0.042,
               0.041,
               0.055,
               0.047,
               0.026,
               0.039,
               0.034,
               0.036,
               0.046,
               0.018,
               0.047,
               0.025,
               0.015,
               0.022,
               0.041)

Hybrid <- (Density + Abundance) / 2

ChromosomeScores <- data.frame(Chromosomes,Abundance,Density, Hybrid)
ChromosomeScores.Melt <- melt(ChromosomeScores,id.vars = "Chromosomes")

ggplot(ChromosomeScores.Melt, aes(x=as.factor(Chromosomes), y=value,label=value)) +
  geom_hline(yintercept=0,lty=2) +
  geom_point(aes(fill=variable),size=3,shape=21, col = "white") +
  ylim(-0.05, 0.15) +
  theme_pubr() +
  geom_vline(xintercept = 0,lwd=2) +
  labs(y="Relative Fitness Contribution",
       x="Chromosome",
       fill="") +
  scale_x_discrete(labels=c(seq(1:22),"X")) +
  theme(axis.text = element_text(size=7),
        axis.title = element_text(size=8),
        legend.text = element_text(size=8)) + 
  scale_fill_brewer(palette = "Dark2")

#ggsave(filename = "Figures/Figure1/ChromosomeScores.pdf", width=3,height=2,units = "in")

# Author: Daniel Villela
# Bayesian models for mosquito capture recapture experiments
# Description:  Plotting code

# gera plots

library("ggplot2")

filename <- "./results20151209a.RData"
load(filename)
totresults <- length(results)
w = results[[1]]
write.csv(w$wbout$BUGSoutput$summary, file="table.compare.csv")
df <- data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                 EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                 EstSurvivalM= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                 EstSurvivalU= w$wbout$BUGSoutput$sims.matrix[,"phiu"],
                 Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                 SurvivalU=w$phius, SurvivalM = w$phis)
for (i in 2:totresults) {
  w = results[[i]]
  if (w$M==2000) {
#  df <- rbind(df, data.frame(Abundance=w$wbout$BUGSoutput$sims.matrix["U"], Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
#                   SurvivalU=w$phius, SurvivalM = w$phi))
  df <- rbind(df, data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                   EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                   EstSurvivalM= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                   EstSurvivalU= w$wbout$BUGSoutput$sims.matrix[,"phiu"],
                   Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                   SurvivalU=w$phius, SurvivalM = w$phis)
  )
  }
}

gabun <- ggplot(aes(x=Unmarked, y=EstAbundance, 
                    group=interaction(as.factor(Marked),
                                      as.factor(Unmarked))), 
                data=df)
gabun1 <- gabun+ geom_boxplot(aes(color=as.factor(Marked)),
                             outlier.shape=NA)
gabun1 <- gabun1 + geom_abline(intercept=0, slope=1)
#gabun <- gabun+ geom_boxplot()
gabun1

# U=4000

w = results[[1]]
df <- data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                 EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                 EstSurvivalM= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                 EstSurvivalU= w$wbout$BUGSoutput$sims.matrix[,"phiu"],
                 Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                 SurvivalU=w$phius, SurvivalM = w$phis)
# number of released less than 300 did not have good convergence
for (i in 2:totresults) {
  w = results[[i]]
  if ((w$U==4000) && (w$p==0.05) && ((w$phis==.78) && (w$phius==.85))) {
    #  df <- rbind(df, data.frame(Abundance=w$wbout$BUGSoutput$sims.matrix["U"], Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
    #                   SurvivalU=w$phius, SurvivalM = w$phi))
    df <- rbind(df, data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                               EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                               EstSurvivalM= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                               EstSurvivalU= w$wbout$BUGSoutput$sims.matrix[,"phiu"],
                               Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                               SurvivalU=w$phius, SurvivalM = w$phis)
    )
  }
}

gabun <- ggplot(aes(x=as.factor(Marked), y=EstAbundance, 
                    group=interaction(as.factor(Marked),
                                      as.factor(Unmarked))), 
                data=df)
gabun2 <- gabun + geom_abline(slope=0, intercept=4000, color="red") +
#  geom_boxplot(aes(color=as.factor(Marked)),
  geom_boxplot(outlier.shape=NA, color="blue")+ 
  ggtitle("A.")+
  labs(x="Number of marked individuals", y="Abundance estimate") +
  theme(plot.title = element_text(hjust = 0, size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12), legend.position="none") 
  
#gabun <- gabun+ geom_boxplot()
gabun2


gp <- ggplot(aes(x=as.factor(Marked), y=EstCapture, group=Marked), data=df)
gp <- gp + 
  geom_abline(slope=0, intercept=0.05, color="red")+
#  geom_boxplot(aes(color=as.factor(Marked))) +
  geom_boxplot(color="blue") +
  # geom_abline(slope=0, intercept=0.5)+
  ggtitle("B.") +
  labs(x="Number of marked Individuals", y="Capture efficiency estimate") + 
  scale_color_discrete(name="Number of \n marked\n individuals") +
  theme(plot.title = element_text(hjust = 0, size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12), legend.position="none") 

gp

#require(grid)
require(gridExtra)
tiff(paste(filename, "Fig1.tif", sep="-"), compression="lzw", width=16, 
     height=16, units="cm", res=300)
grid.arrange(gabun2, gp, ncol=1)
dev.off()
#arrangeGrob(gabun2, gp, ncol=1)

w = results[[1]]
df <- data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                 EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                 EstSurvivalM= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                 EstSurvivalU= w$wbout$BUGSoutput$sims.matrix[,"phiu"],
                 EstRecruit = w$wbout$BUGSoutput$sims.matrix[,"l"],
                 Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                 recruit = w$b,
                 SurvivalU=w$phius, SurvivalM = w$phis, frecruit=1)

df <- rbind(df, data.frame(EstAbundance=w$wbout34$BUGSoutput$sims.matrix[,"U"], 
                 EstCapture= w$wbout34$BUGSoutput$sims.matrix[,"p"], 
                 EstSurvivalM= w$wbout34$BUGSoutput$sims.matrix[,"phi"], 
                 EstSurvivalU= w$wbout34$BUGSoutput$sims.matrix[,"phiu"],
                 EstRecruit = w$wbout34$BUGSoutput$sims.matrix[,"l"],
                 Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                 recruit = w$b,
                 SurvivalU=w$phius, SurvivalM = w$phis, frecruit=.75)
            )
df <- rbind(df, data.frame(EstAbundance=w$wbout24$BUGSoutput$sims.matrix[,"U"], 
                           EstCapture= w$wbout24$BUGSoutput$sims.matrix[,"p"], 
                           EstSurvivalM= w$wbout24$BUGSoutput$sims.matrix[,"phi"], 
                           EstSurvivalU= w$wbout24$BUGSoutput$sims.matrix[,"phiu"],
                           EstRecruit = w$wbout24$BUGSoutput$sims.matrix[,"l"],
                           Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                           recruit = w$b,
                           SurvivalU=w$phius, SurvivalM = w$phis, frecruit=.5)
)
df <- rbind(df, data.frame(EstAbundance=w$wbout14$BUGSoutput$sims.matrix[,"U"], 
                           EstCapture= w$wbout14$BUGSoutput$sims.matrix[,"p"], 
                           EstSurvivalM= w$wbout14$BUGSoutput$sims.matrix[,"phi"], 
                           EstSurvivalU= w$wbout14$BUGSoutput$sims.matrix[,"phiu"],
                           EstRecruit = w$wbout14$BUGSoutput$sims.matrix[,"l"],
                           Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                           recruit = w$b,
                           SurvivalU=w$phius, SurvivalM = w$phis, frecruit=.25)
)


for (i in 2:totresults) {
  w = results[[i]]
  if ((w$U==4000) && (w$p==0.05) && ((w$phis==.78) && (w$phius==.85))) {
#  if (w$U==4000) {
    #  df <- rbind(df, data.frame(Abundance=w$wbout$BUGSoutput$sims.matrix["U"], Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
    #                   SurvivalU=w$phius, SurvivalM = w$phi))
    df <- rbind(df, data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                               EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                               EstSurvivalM= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                               EstSurvivalU= w$wbout$BUGSoutput$sims.matrix[,"phiu"],
                               EstRecruit = w$wbout$BUGSoutput$sims.matrix[,"l"],
                               Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                               recruit = w$b,
                               SurvivalU=w$phius, SurvivalM = w$phis, frecruit=4/4)
    )
    df <- rbind(df, data.frame(EstAbundance=w$wbout34$BUGSoutput$sims.matrix[,"U"], 
                               EstCapture= w$wbout34$BUGSoutput$sims.matrix[,"p"], 
                               EstSurvivalM= w$wbout34$BUGSoutput$sims.matrix[,"phi"], 
                               EstSurvivalU= w$wbout34$BUGSoutput$sims.matrix[,"phiu"],
                               EstRecruit = w$wbout34$BUGSoutput$sims.matrix[,"l"],
                               Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                               recruit = w$b,
                               SurvivalU=w$phius, SurvivalM = w$phis, frecruit=.75)
    )
    df <- rbind(df, data.frame(EstAbundance=w$wbout24$BUGSoutput$sims.matrix[,"U"], 
                               EstCapture= w$wbout24$BUGSoutput$sims.matrix[,"p"], 
                               EstSurvivalM= w$wbout24$BUGSoutput$sims.matrix[,"phi"], 
                               EstSurvivalU= w$wbout24$BUGSoutput$sims.matrix[,"phiu"],
                               EstRecruit = w$wbout24$BUGSoutput$sims.matrix[,"l"],
                               Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                               recruit = w$b,
                               SurvivalU=w$phius, SurvivalM = w$phis, frecruit=.5)
    )
    df <- rbind(df, data.frame(EstAbundance=w$wbout14$BUGSoutput$sims.matrix[,"U"], 
                               EstCapture= w$wbout14$BUGSoutput$sims.matrix[,"p"], 
                               EstSurvivalM= w$wbout14$BUGSoutput$sims.matrix[,"phi"], 
                               EstSurvivalU= w$wbout14$BUGSoutput$sims.matrix[,"phiu"],
                               EstRecruit = w$wbout14$BUGSoutput$sims.matrix[,"l"],                               
                               Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                               recruit = w$b,
                               SurvivalU=w$phius, SurvivalM = w$phis, frecruit=.25)
    )
    
  }
}

df$frecruit <- as.factor(df$frecruit)
#names <- colnames(df)
colnames(df)[which(names(df)=="frecruit")] = "Efficiency"

gabun <- ggplot(aes(x=Efficiency, y=EstAbundance, 
                    group=interaction(as.factor(Marked), 
                                      as.factor(Efficiency))), data=df)
gabun2 <- gabun+ geom_boxplot(aes(color=as.factor(Marked)),
                              outlier.shape=NA) 
#gabun <- gabun+ geom_boxplot()
gabun2


gabun <- ggplot(aes(x=Unmarked, y=EstAbundance/1000, 
                    group=interaction(as.factor(Marked), 
                                      as.factor(Efficiency))), data=df)
gabun3 <- gabun+ geom_boxplot(aes(color=as.factor(Marked)),
                              outlier.shape=NA) + geom_abline(intercept=4, slope=0) +
  scale_color_discrete(name="Number of \n marked\n individuals") +
  ggtitle("A.") +
  labs(x="", y="Abundance estimate (x 1000)")+
  scale_x_continuous(breaks=NULL, labels=c())+
  theme(plot.title = element_text(hjust = 0, size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12), legend.position="none") +
  coord_flip()+
  facet_grid( ~ Efficiency, labeller=label_both)
#gabun <- gabun+ geom_boxplot()
gabun3

gsurvR <- ggplot(aes(x=recruit, y=EstRecruit, 
                    group= interaction(as.factor(Marked), 
                                       as.factor(Efficiency))), data=df)
gsurvR3 <- gsurvR + geom_boxplot(aes(color=as.factor(Marked)),
                              outlier.shape=NA) + geom_abline(intercept=600, slope=0) +
  scale_color_discrete(name="Number of \n marked\n individuals") +
  ggtitle("B.") +
  labs(x="", y="Recruitment estimate")+
  scale_x_continuous(breaks=NULL, labels=c())+
  theme(plot.title = element_text(hjust = 0, size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12), legend.position="none") +
  coord_flip()+
  facet_grid( ~ Efficiency, labeller=label_both)
gsurvR3

gsurvU <- ggplot(aes(x=SurvivalU, y=EstSurvivalU, 
                     group= interaction(as.factor(Marked), 
                                        as.factor(Efficiency))), data=df)
gsurvU3 <- gsurvU + geom_boxplot(aes(color=as.factor(Marked)),
                                 outlier.shape=NA) + geom_abline(intercept=.85, slope=0) +
  scale_color_discrete(name=" Number of \n marked individuals") +
  ggtitle("C.") +
  labs(x="", y="Survivorship (unmarked)")+
  scale_x_continuous(breaks=NULL, labels=c())+
  theme(plot.title = element_text(hjust = 0, size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12), 
        legend.text = element_text(size=10), 
        legend.title = element_text(size=8), legend.position="bottom") +
  coord_flip()+
  facet_grid( ~ Efficiency, labeller=label_both)
gsurvU3

gsurvU <- ggplot(aes(x=SurvivalU, y=EstSurvivalU, 
                     group= interaction(as.factor(Marked), 
                                        as.factor(Efficiency))), data=df)
gsurvU3N <- gsurvU + geom_boxplot(aes(color=as.factor(Marked)),
                                 outlier.shape=NA) + geom_abline(intercept=.85, slope=0) +
  scale_color_discrete(name=" Number of \n marked individuals") +
  ggtitle("C.") +
  labs(x="", y="Survivorship (unmarked)")+
  scale_x_continuous(breaks=NULL, labels=c())+
  theme(plot.title = element_text(hjust = 0, size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12), 
        legend.text = element_text(size=10), 
        legend.title = element_text(size=8), legend.position="none") +
  coord_flip()+
  facet_grid( ~ Efficiency, labeller=label_both)
gsurvU3N

#source("./shared_legend.R")
grob.x <- ggplotGrob(gsurvU3)
grob.s <- grob.x$grobs
legend <- grob.s[[which(sapply(grob.s, function(x) x$name) == "guide-box")]]
#max: 19 cm x 22 cm
require(gridExtra)
tiff(paste(filename, "Fig3.tif", sep="-"), compression="lzw", width=16, 
     height=16, units="cm", res=300)
#grid_arrange_shared_legend(gabun3,gsurvR3, gsurvU3)
#grid.arrange(gabun3, gsurvR3, gsurvU3N, legend, ncol=1)
require(grid)
grid.draw(rbind(ggplotGrob(gabun3), ggplotGrob(gsurvR3), 
          ggplotGrob(gsurvU3), size="last"))
dev.off()

gsurvM <- ggplot(aes(x=SurvivalM, y=EstSurvivalM, 
                     group= as.factor(Efficiency)), data=df)
gsurvM3 <- gsurvM + geom_boxplot(aes(color=as.factor(Efficiency)),
                                 outlier.shape=NA) + geom_abline(intercept=.78, slope=0) +
  facet_grid( ~ Efficiency)
gsurvM3

# varying Prob capt
# U=4000

w = results[[1]]
df <- data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                 EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                 EstSurvivalM= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                 EstSurvivalU= w$wbout$BUGSoutput$sims.matrix[,"phiu"],
                 Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                 SurvivalU=w$phius, SurvivalM = w$phis)
for (i in 2:totresults) {
  w = results[[i]]
  if ((w$U==4000) && (w$M == 2000) && ((w$phis==.78) && (w$phius==.85))) {
    #  df <- rbind(df, data.frame(Abundance=w$wbout$BUGSoutput$sims.matrix["U"], Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
    #                   SurvivalU=w$phius, SurvivalM = w$phi))
    df <- rbind(df, data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                               EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                               EstSurvivalM= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                               EstSurvivalU= w$wbout$BUGSoutput$sims.matrix[,"phiu"],
                               Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                               SurvivalU=w$phius, SurvivalM = w$phis)
    )
  }
}

colnames(df)[which(names(df)=="ProbCapt")] = "Capt.Efficiency"

gabun <- ggplot(aes(x=as.factor(Capt.Efficiency), y=EstAbundance, 
                    group=as.factor(Capt.Efficiency)), 
                data=df)
gabun4 <- gabun+ 
  geom_abline(intercept = 4000, slope=0, color="red")+
  #geom_boxplot(aes(color=as.factor(Capt.Efficiency)), outlier.shape=NA) + 
  geom_boxplot(color="blue", outlier.shape=NA) +  
  scale_color_discrete(name="Capture efficiency") +
  ggtitle("") +
  labs(x="Capture efficiency", y="Abundance estimate")+
  #scale_x_continuous(labels=c())+
  theme(plot.title = element_text(hjust = 0, size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12)) 
  # coord_flip()+
  #facet_grid( ~ Capt.Efficiency, labeller=label_both)
#gabun <- gabun+ geom_boxplot()
gabun4

tiff(paste(filename, "Fig5.tif", sep="-"), compression="lzw", width=16, 
     height=16, units="cm", res=300)
gabun4
dev.off()

gsurvM <- ggplot(aes(x=SurvivalM, y=EstSurvivalM, 
                     group= as.factor(Capt.Efficiency)), data=df)
gsurvM4 <- gsurvM + geom_violin(aes(color=as.factor(Capt.Efficiency)),
                                 outlier.shape=NA) + geom_abline(intercept=.78, slope=0) +
  facet_grid( ~ Capt.Efficiency)
gsurvM4

w = results[[1]]
df <- data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                 EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                 EstSurvival= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                 Cohort = "marked",
                 Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                 SurvivalU=w$phius, SurvivalM = w$phis)
df <- rbind(df, data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                 EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                 EstSurvival= w$wbout$BUGSoutput$sims.matrix[,"phiu"], 
                 Cohort = "unmarked",
                 Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                 SurvivalU=w$phius, SurvivalM = w$phis)
)

for (i in 2:totresults) {
  w = results[[i]]  
  if ((w$U==4000) && (w$M == 2000) && (w$p==0.05))  {
    cat("i" , i, " phi ", w$phis, "phiu ", w$phiu, "\n")
    #  df <- rbind(df, data.frame(Abundance=w$wbout$BUGSoutput$sims.matrix["U"], Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
    #                   SurvivalU=w$phius, SurvivalM = w$phi))
    df <- rbind(df, 
                data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                     EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                     EstSurvival= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                     Cohort = "marked",
                     Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                     SurvivalU=w$phius, SurvivalM = w$phis)
    )
    df <- rbind(df, data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                               EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                               EstSurvival= w$wbout$BUGSoutput$sims.matrix[,"phiu"], 
                               Cohort = "unmarked",
                               Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                               SurvivalU=w$phius, SurvivalM = w$phis)
    )
    
  }
}

gabun <- ggplot(aes(x=Unmarked, y=EstSurvival, 
                    group=as.factor(Cohort)), 
                data=df)
gabun5 <- gabun+ geom_boxplot(aes(color=as.factor(Cohort)),
                              outlier.shape=NA) + 
  ggtitle("") +
  labs(x="", y="Survivorship")+
  scale_color_discrete(name="Cohort") +
  scale_x_continuous(breaks=NULL, labels=c())+
  theme(plot.title = element_text(hjust = 0, size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12)) +
 #coord_flip()+
  facet_grid(. ~ SurvivalU + SurvivalM , labeller=label_both)
#gabun <- gabun+ geom_boxplot()
gabun5

tiff(paste(filename, "Fig2.tif", sep="-"), compression="lzw", width=16, 
     height=16, units="cm", res=300)
gabun5
dev.off()

w = results[[1]]
df <- data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                 EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                 EstSurvivalM= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                 EstSurvivalU= w$wbout$BUGSoutput$sims.matrix[,"phiu"],
                 EstRecruit= w$wbout$BUGSoutput$sims.matrix[,"l"],
                 Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                 SurvivalU=w$phius, SurvivalM = w$phis,
                 model="recruit")
df <- rbind(df, data.frame(
  EstAbundance=w$wbout_samesurv$BUGSoutput$sims.matrix[,"U"], 
  EstCapture= w$wbout_samesurv$BUGSoutput$sims.matrix[,"p"], 
  EstSurvivalM= w$wbout_samesurv$BUGSoutput$sims.matrix[,"phi"], 
  EstSurvivalU= w$wbout_samesurv$BUGSoutput$sims.matrix[,"phi"],
  EstRecruit= w$wbout_samesurv$BUGSoutput$sims.matrix[,"l"],
  Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
  SurvivalU=w$phius, SurvivalM = w$phis,
  model="samesurv"
  ))

for (i in 2:totresults) {
  w = results[[i]]
  if ((w$U==4000) && (w$p==.05) && (w$M > 1000) &&
       ((abs(w$phius - w$phis)>0.05) || 
          (abs(w$phius - w$phis)<0.01) )){
    #  df <- rbind(df, data.frame(Abundance=w$wbout$BUGSoutput$sims.matrix["U"], Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
    #                   SurvivalU=w$phius, SurvivalM = w$phi))
    df <- rbind(df, data.frame(EstAbundance=w$wbout$BUGSoutput$sims.matrix[,"U"], 
                               EstCapture= w$wbout$BUGSoutput$sims.matrix[,"p"], 
                               EstSurvivalM= w$wbout$BUGSoutput$sims.matrix[,"phi"], 
                               EstSurvivalU= w$wbout$BUGSoutput$sims.matrix[,"phiu"],
                               EstRecruit= w$wbout$BUGSoutput$sims.matrix[,"l"],
                               Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
                               SurvivalU=w$phius, SurvivalM = w$phis,
                               model="recruit")
    )
    df <- rbind(df, data.frame(
      EstAbundance=w$wbout_samesurv$BUGSoutput$sims.matrix[,"U"], 
      EstCapture= w$wbout_samesurv$BUGSoutput$sims.matrix[,"p"], 
      EstSurvivalM= w$wbout_samesurv$BUGSoutput$sims.matrix[,"phi"], 
      # has to be the same
      EstSurvivalU= w$wbout_samesurv$BUGSoutput$sims.matrix[,"phi"],
      EstRecruit= w$wbout_samesurv$BUGSoutput$sims.matrix[,"l"],
      Unmarked = w$U, Marked= w$M, ProbCapt=w$p,
      SurvivalU=w$phius, SurvivalM = w$phis,
      model="samesurv"
    ))
    
  }
}

df$model <- as.factor(df$model)

levels(df$model) <- c("M[RP]","M[RSU]") 

label_level <- function(variable, value) {
  #x <- value
  #str <- sprintf("Comp: %d", as.integer(x))
if (value=="recruit") {
  paste("model", parse(text="M[RP]"), sep=": ")
}
else {
  paste("model", "MRSU", sep=": ")
}
  #paste("Comp", as.integer(value), sep=": ")
}


grecr6 <- ggplot(aes(x=Unmarked, y=EstRecruit, 
                    group=as.factor(Marked)), 
                data=df)
grecr6 <- grecr6 + geom_boxplot(aes(color=as.factor(Marked)),
                              outlier.shape=NA) + 
  geom_abline(intercept=600, slope=0) +   
  ggtitle("B.") +
  labs(x="", y="Recruitment estimate")+
  scale_color_discrete(name=" Number of \n marked\n individuals") +
  scale_x_continuous(breaks=NULL, labels=c())+
  theme(plot.title = element_text(hjust = 0, size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12),
        legend.position="bottom") +
  facet_grid( model ~ SurvivalU + SurvivalM, labeller=labeller(model = label_parsed,SurvivalU=label_both,
                                                               SurvivalM=label_both))
#gabun <- gabun+ geom_boxplot()
grecr6


gabun6 <- ggplot(aes(x=Unmarked, y=EstAbundance, 
                     group=as.factor(Marked)), 
                 data=df)
gabun6 <- gabun6+ geom_boxplot(aes(color=as.factor(Marked)),
                              outlier.shape=NA) + 
  geom_abline(intercept=4000, slope=0) + 
  ggtitle("A.") +
  labs(x="", y="Abundance estimate")+
  scale_color_discrete(name= " Number of \n marked\n individuals") +
  scale_x_continuous(breaks=NULL, labels=c())+
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0, size=12),
        axis.title.y = element_text(size=12),
        axis.title.x = element_text(size=12)) +
  #coord_flip()+  
  facet_grid( model ~ SurvivalU + SurvivalM, labeller=labeller(model = label_parsed,SurvivalU=label_both,
                                                               SurvivalM=label_both))
#gabun <- gabun+ geom_boxplot()
gabun6

tiff(paste(filename, "Fig4.tif", sep="-"), compression="lzw", width=16, 
     height=16, units="cm", res=300)
grid.draw(rbind(ggplotGrob(gabun6), ggplotGrob(grecr6), 
                size="last"))
#grid.arrange(gabun6, grecr6, ncol=1)
dev.off()

#Table
w <- results[[1]]
#assuming same p=0.5 

estimate.string <- function(x, par="U") {
  m <- round(x$BUGSoutput$summary[par, "mean"])
  s <- sprintf("%d %s", m, ci.string(x,par))
  s
}

estimate.string1000 <- function(x, par="U", expected) {
  m <- round(x$BUGSoutput$summary[par, "mean"])
  s <- sprintf("%.1f %s", m/1000, ci.string1000(x,par, expected))
  s
}
ci.string <- function(x, par="U") {
  s = ""
  l = round(x$BUGSoutput$summary[par, "2.5%"])
  u = round(x$BUGSoutput$summary[par, "97.5%"])
#  sprintf(s, "(%f.0 -- %f.0)", l, u)
  s <- sprintf("(%d -- %d)", l, u)
  s
}
ci.string1000 <- function(x, par="U", expected=4000) {
  s = ""
  l = round(x$BUGSoutput$summary[par, "2.5%"])
  u = round(x$BUGSoutput$summary[par, "97.5%"])
  #  sprintf(s, "(%f.0 -- %f.0)", l, u)
  s <- sprintf("(%.1f - %.1f)%s", l/1000, u/1000, 
               ifelse((expected>=l) && (expected<=u), "*", ""))
  s
}

total_pupas <- w$total_pupas
fa <-  w$J*100/(w$area*w$iarea)
abundf <- data.frame(
  AbunSimul = w$U,
  Releases = w$M,
  PCapture = w$p,
  SurvM = w$phis,
  SurvU = w$phius,
  RecMed = estimate.string(w$wbout),
  SameSurv = estimate.string(w$wbout_samesurv),
  Buona = estimate.string(w$wbuona), 
  FisherFord = estimate.string(w$fisher),
#Pupae.median = round(median(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )))),
#Pupae.mean = round(mean(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )))),
#Pupae.CI = 
#  sprintf("(%d -- %d)", 
#          round(quantile(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )), probs=.025)),
#          round(quantile(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )), probs=.975))),
Lincoln = estimate.string(w$lincoln)
#Pupae=""
)

abundf1000 <- data.frame(
  AbunSimul = w$U,
  Releases = w$M,
  PCapture = w$p,
  SurvM = w$phis,
  SurvU = w$phius,
  RecMed = estimate.string1000(w$wbout, "U", w$U),
  SameSurv = estimate.string1000(w$wbout_samesurv, "U", w$U),
  Buona = estimate.string1000(w$wbuona, "U", w$U), 
  FisherFord = estimate.string1000(w$fisher, "U", w$U),
  #Pupae.median = round(median(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )))),
  #Pupae.mean = round(mean(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )))),
  #Pupae.CI = 
  #  sprintf("(%d -- %d)", 
  #          round(quantile(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )), probs=.025)),
  #          round(quantile(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )), probs=.975))),
  Lincoln = estimate.string1000(w$lincoln, "U", w$U)
  #Pupae=""
)


dicdf <- data.frame(
  AbunSimul = w$U,
  Releases = w$M,
  PCapture = w$p,
  SurvM = w$phis,
  SurvU = w$phius,
  RecMed.dic = w$wbout$BUGSoutput$DIC,
  SameSurv.dic = w$wbout_samesurv$BUGSoutput$DIC,                 
  Buona.dic = w$wbuona$BUGSoutput$DIC,
  FF.dic = w$fisher$BUGSoutput$DIC,
  L.dic = w$lincoln$BUGSoutput$DIC  
  )

for (i in 2:totresults) {
  w <- results[[i]]
  abundf <- rbind(abundf,
                  data.frame(
                    AbunSimul = w$U,
                    Releases = w$M,
                    PCapture = w$p,
                    SurvM = w$phis,
                    SurvU = w$phius,
                    RecMed = estimate.string(w$wbout),
                    SameSurv = estimate.string(w$wbout_samesurv),
                    Buona = estimate.string(w$wbuona), 
                    FisherFord = estimate.string(w$fisher),
                    #Pupae.median = round(median(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )))),
                    #Pupae.mean = round(mean(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )))),
                    #Pupae.CI = 
                    #  sprintf("(%d -- %d)", 
                    #          round(quantile(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )), probs=.025)),
                    #          round(quantile(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )), probs=.975))),
                    Lincoln = estimate.string(w$lincoln)
                    #Pupae=""
                  )
  )
                  
      abundf1000 <- rbind(abundf1000,
                                  data.frame(
                                    AbunSimul = w$U,
                                    Releases = w$M,
                                    PCapture = w$p,
                                    SurvM = w$phis,
                                    SurvU = w$phius,
                                    RecMed = estimate.string1000(w$wbout, "U", w$U),
                                    SameSurv = estimate.string1000(w$wbout_samesurv, "U", w$U),
                                    Buona = estimate.string1000(w$wbuona, "U", w$U), 
                                    FisherFord = estimate.string1000(w$fisher, "U", w$U),
                                    #Pupae.median = round(median(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )))),
                                    #Pupae.mean = round(mean(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )))),
                                    #Pupae.CI = 
                                    #  sprintf("(%d -- %d)", 
                                    #          round(quantile(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )), probs=.025)),
                                    #          round(quantile(total_pupas/(4*fa*(1-w$wbuona$BUGSoutput$sims.matrix[,"phi"] )), probs=.975))),
                                    Lincoln = estimate.string1000(w$lincoln, "U", w$U)
                                    #Pupae=""
                                  )                  
                                  

      )

dicdf <- rbind(dicdf,
               data.frame(
                 AbunSimul = w$U,
                 Releases = w$M,
                 PCapture = w$p,
                 SurvM = w$phis,
                 SurvU = w$phius,
                 RecMed.dic = w$wbout$BUGSoutput$DIC,
                 SameSurv.dic = w$wbout_samesurv$BUGSoutput$DIC,                 
                 Buona.dic = w$wbuona$BUGSoutput$DIC,
                 FF.dic = w$fisher$BUGSoutput$DIC,
                 L.dic = w$lincoln$BUGSoutput$DIC  
               )
               
)

}

#include wbout14
#wbout24
#wbout34?
write.csv(abundf, file="abundf_1116.csv")
write.csv(dicdf, file="dicdf_1116.csv")
write.csv(abundf1000, file="abundf1000_1116.csv" )
# construct table of DIC

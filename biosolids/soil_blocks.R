soil = read.csv("soil_blocks.csv")
head(soil)
soil_control = subset(soil, Treatment=="Control")
?aov
mod = aov(y.avg ~ Species, data = soil_control)

summary(mod)

pairwise.t.test(soil_control$y.avg, soil_control$Species, p.adjust.method = "bonferroni")

TukeyHSD(mod)

mod2 = lm(soil_control$y.avg ~ soil_control$Species)
summary(mod2)

#d)

litt = subset(soil_control, Species == "LITT")
heco = subset(soil_control, Species == "HECO")

t = t.test(litt$y.avg, heco$y.avg)




## activity 4
soil_biosolids = subset(soil, Treatment=="Biosolids")
soil_diff = soil_biosolids[,1:2]
soil_diff$avg.diff = soil_biosolids[,4] - soil_control[,4]
mod2 = aov(avg.diff ~ Species, data = soil_diff)
summary(mod2)

mod3 = lm(avg.diff ~ Species, data=soil_diff)

mod4 = aov(y.avg ~ Species + Treatment +  Species:Treatment, data = soil)
summary(mod4)

mod7 = aov(y.avg ~ Species + Treatment, data = soil)
summary(mod7)

mod8 = aov(y.avg ~ Species * Treatment, data = soil)
summary(mod8)


mod5 = lm(y.avg ~ Species + Treatment + Species:Treatment, data = soil)
summary(mod5)
mod6 = lm(y.avg ~ Species + Treatment, data = soil)
summary(mod6)


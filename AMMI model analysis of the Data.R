setwd("E:/MNCH 74-75W Report to press basnet")
BIKU<-read.csv("E:/MNCH 74-75W Report to press basnet/Sam For analysis.csv",header = TRUE)
print(BIKU)
attach(BIKU)
library(ggplot2)
library(metan)
options(max.print = 500)
getwd()
require(readxl)
BIKU$ENV<-factor(BIKU$ENV, levels=unique(BIKU$ENV))
BIKU$GEN <-factor(BIKU$GEN, levels=unique(BIKU$GEN))
BIKU$YEAR<- factor(BIKU$YEAR, levels=unique(BIKU$YEAR))
str(BIKU)
inspect(BIKU, plot=TRUE)
find_outliers(BIKU, var=DTA, plots=TRUE)
find_outliers(BIKU, var=DTS, plots=TRUE)
remove_rows_na(BIKU)
replace_zero(BIKU)
find_text_in_num(BIKU$DTA)
find_text_in_num(BIKU$DTS)
desc_stat(BIKU)
desc_stat(BIKU, stats="all")
ds<-desc_stat(BIKU, stats="all") 
ds
View(ds)
class(ds)
library(writexl)
write_xlsx(ds, "ds.xlsx")
mg<-means_by(BIKU, GEN)
mg
View(mg)
me<-means_by(BIKU, ENV)
me
View(me)
mge<-BIKU %>% 
  group_by(ENV, GEN) %>%
  desc_stat(DTA, DTS, stats="mean")
mge
View(mge)
pDTA<-ge_plot(BIKU, ENV, GEN, DTA)
pDTA
pDTA2 <- ge_plot(BIKU, ENV, GEN, DTA, type=2)
pDTA2
##DTS
pDTS<-ge_plot(BIKU, ENV, GEN, DTS)
pDTS
pDTS<-ge_plot(BIKU, ENV, GEN, DTS, type=2)
pDTS
win<-ge_winners(BIKU, ENV, GEN, resp = everything())
View(win)
ranks <-ge_winners(BIKU, ENV, GEN, resp = everything(), type = "ranks")
View(ranks)
ge_details(BIKU, ENV, GEN, resp = everything())
########################## fixed effect models #############################
########################### ind anova ####################################
indav<- anova_ind(BIKU, ENV, GEN, YEAR, resp = c(DTA, DTS))
# anova for height
indav$DTA$individual
iaht<-indav$DTA$individual
View(iaht)
## Bartlett test 
bartlett.test(BIKU$DTA~BIKU$ENV, data = BIKU)
#anova for yield
indav$DTA$individual
iayd<-indav$DTS$individual
View(iayd)
write_xlsx(iayd, "DTSldanv.xlsx")
# pooled anova for DTA
panv1<-anova_joint(BIKU, ENV, GEN, YEAR, DTA)
pavt1<-panv1$DTA$anova
View(pavt1)
# pooled anova for DTS
panv2<-anova_joint(BIKU, ENV, GEN, YEAR, DTS)
pavt2<-panv2$DTS$anova
View(pavt2)
######################## stability analysis ###########################
######################## anova based stability #######################
# Annichiarico env index
ann1<-Annicchiarico(BIKU, ENV, GEN, YEAR, DTA)
print(ann1)
View(ann1$DTA$environments)
ann2<-Annicchiarico(BIKU, ENV, GEN, YEAR, DTS)
print(ann2)
View(ann2$DTS$environments)
# ecovalence
eco1<-ecovalence(BIKU, ENV, GEN, YEAR, DTA)
eco1
View(eco1$DTA)
eco2<-ecovalence(BIKU, ENV, GEN, YEAR, DTS)
eco2
print(eco2)
############################ factor based #################################
fact1<- ge_factanal(BIKU, ENV, GEN, YEAR, DTA)
print(fact1)

fact2<- ge_factanal(BIKU, ENV, GEN, YEAR, DTS)
print(fact2)
plot(fact2)
View(fact2$DTS$PCA)
###################### ammi models #####################################
## HT
amod1 <-performs_ammi(BIKU, ENV, GEN, YEAR, DTA)
print(amod1)
plot(amod1)
View(amod1$DTA$ANOVA)
write_xlsx(amod1$DTA$ANOVA, "ammianova1.xlsx")

## YLD
amod2<-performs_ammi(BIKU, ENV, GEN, YEAR, DTS)
print(amod2)
plot(amod2)
View(amod2$DTS$ANOVA)

#### Significance of ipca
get_model_data(amod1, "ipca_pval")
get_model_data(amod2, "ipca_pval")

########################## ammi biplots ###################################
### DTA
a1<-plot_scores(amod1)
a1
a1<-plot_scores(amod1, x.lab = "DTA")
a1

b1<-plot_scores(amod1, type = 2)
b1
b1<-plot_scores(amod1, type = 2, polygon = TRUE)
b1
b1<-plot_scores(amod1, 
                  type = 2,
                  col.env = "blue",
                  col.gen = transparent_color(),
                  col.segm.env = "orange",
                  highlight = c("G1", "G2"),
                  col.highlight = "darkcyan",
                  axis.expand = 1.5)
b1
# export 
c1<-plot_scores(amod1, type = 4)
c1
#?plot_scores
c1<-plot_scores(amod1, type=4, repulsion = 2)
c1
c1 <- plot_scores(amod1, type = 4, 
                  size.tex.gen = 2,
                  x.lab = "PC1 of E",
                  y.lab = "Days to anthesis",
                  title=FALSE,
                  col.alpha.gen = 0) 
c1
arrange_ggplot(a1, b1, c1, tag_levels = "a", nrow = 1)

## days to silking

a2<-plot_scores(amod2)
a2

a2 <-plot_scores(amod2, x.lab = "days to silking")
a2

b2 <- plot_scores(amod2, type = 2, polygon = TRUE)
b2
c2 <- plot_scores(amod2, type = 4, size.tex.gen = 2,
                  x.lab = "PC1 of E",
                  y.lab = "Nominal for the days to silking")
c2

######################### ammi based stability statistics ##################
abs1 <-AMMI_indexes(amod1)
print(abs1)
View(abs1$DTA)

abs2<-AMMI_indexes(amod2)
print(abs2)
View(abs2$DTS)

################ ammi based on waas #################################
## DTA
waas1 <- waas(BIKU, ENV, GEN, YEAR, DTA)
View(waas1$DTA$anova)
print(waas1)

wp1_3<- plot_scores(waas1, type = 3)
wp1_3

wp1_2 <- plot_scores(waas1, type = 2, polygon = TRUE)
wp1_2

## DTs
waas2 <- waas(MMET, ENV, GEN, YEAR, DTS)
View(waas2$DTS$anova)

wp2_3 <- plot_scores(waas2, type = 3)
wp2_3
###################  not working waas based stats ################################
wabs1<-ammi_indexes(waas1)
View(wabs1$DTS)
print(wabs1)

wabs2<-ammi_indexes(waas2)
View(wabs2$DTS)
#
# 1 Basic biplot
# DTa
bbp1<- plot(gge_model1)
bbp1

bbp1<- plot(gge_model1, col.gen = "red")
bbp1

# dts
bbp2 <- plot(gge_model2)
bbp2

# 2 Discriminativeness vs representativeness
# HT
dvr1 <- plot(gge_model1, type = 4)
dvr1

# YLD
dvr2 <- plot(gge_model1, type = 4)
dvr2

dvr2 <- plot(gge_model1, type = 4, plot_theme = theme_gray())
dvr2

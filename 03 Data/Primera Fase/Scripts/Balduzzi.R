###
library(here)

setwd(here("Investigación", "Revisión sistemática", "06 Datos", "Analisis", "Scripts"))



# Data base

(joy <- tribble(
  ~author, ~year,~resp.h, ~fail.h, ~drop.h, ~resp.p, ~fail.p, ~drop.p,
  "Arvanitis"     ,1997,25,25,2,18,33,0, 
  "Beasley"       ,1996,29,18,22,20,14,34, 
  "Bechelli"      ,1983,12,17,1,2,28,1, 
  "Borison"       ,1992,3,9,0,0,12,0, 
  "Chouinard"     ,1993,10,11,0,3,19,0, 
  "Durost"        ,1964,11,8,0,1,14,0, 
  "Garry"         ,1962,7,18,1,4,21,1, 
  "Howard"        ,1974,8,9,0,3,10,0, 
  "Marder"        ,1994,19,45,2,14,50,2, 
  "Nishikawa"     ,1982,1,9,0,0,10,0, 
  "Nishikawa"     ,1984,11,23,3,0,13,0, 
  "Reschke"       ,1974,20,9,0,2,9,0, 
  "Selman"        ,1976,17,1,11,7,4,18, 
  "Serafetinides" ,1972,4,10,0,0,13,1, 
  "Simpson"       ,1967,2,14,0,0,7,1, 
  "Spencer"       ,1992,11,1,0,1,11,0, 
  "Vichaiya"      ,1971,9,20,1,0,29,1))


## Install R packages
##
install.packages(c("meta", "metasens"))

## Make R packages available
##
library(meta)
library(metasens)

## Default settings for R session:
## print results with two significant digits
##
settings.meta(digits = 2)


######## -
## Add new variable: miss
######## -
joy$miss = ifelse((joy$drop.h + joy$drop.p) == 0, 
                  c("Without missing data"), c("With missing data"))

joy

View(joy)
str(joy)


######## -
## Section 'Fixed effect and random effects meta-analysis'
######## -

m.publ <- metabin(resp.h, resp.h + fail.h, resp.p, resp.p + fail.p,
                 data = joy, studlab = paste0(author, " (", year, ")"),
                 method.tau = "PM")

## Print results of meta-analysis (Figure 1)
##
m.publ

# Figure 2
##
pdf("figure2.pdf", width = 10, height = 6)
forest(m.publ, sortvar = year, prediction = TRUE,
       label.left = "Favours placebo", label.right = "Favours haloperidol")
#dev.off()

##
##
## Section 'Assessing the impact of missing outcome data'
##
##

## Subgroup analysis of studies with and without missing data
##
m.publ.sub <- update(m.publ, byvar = miss, print.byvar = FALSE)
m.publ.sub


## Figure 3
##
pdf("figure3.pdf", width = 10, height = 7.05)
forest(m.publ.sub, sortvar = year,
       xlim = c(0.1, 100), at = c(0.1, 0.3, 1, 3, 10, 30, 100),
       test.subgroup.random = TRUE)
#dev.off()

## Impute as no events (ICA-0) - default
##
mmiss.0 <- metamiss(m.publ, drop.h, drop.p)
##
## Impute as events (ICA-1)
##
mmiss.1 <- metamiss(m.publ, drop.h, drop.p, method = "1")
##
## Observed risk in control group (ICA-pc)
##
mmiss.pc <- metamiss(m.publ, drop.h, drop.p, method = "pc")
##
## Observed risk in experimental group (ICA-pe)
##
mmiss.pe <- metamiss(m.publ, drop.h, drop.p, method = "pe")
##
## Observed group-specific risks (ICA-p)
##
mmiss.p <- metamiss(m.publ, drop.h, drop.p, method = "p")
##
## Best-case scenario (ICA-b)
##
mmiss.b <- metamiss(m.publ, drop.h, drop.p, method = "b", small.values = "bad")
##
## Worst-case scenario (ICA-w)
##
mmiss.w <- metamiss(m.publ, drop.h, drop.p, method = "w", small.values = "bad")
##
## Gamble-Hollis method
##
mmiss.gh <- metamiss(m.publ, drop.h, drop.p, method = "GH")
##
## IMOR.e = 2 and IMOR.c = 2
## (same as available case analysis)
##
mmiss.imor2 <- metamiss(m.publ, drop.h, drop.p, method = "IMOR", IMOR.e = 2)
##
## IMOR.e = 0.5 and IMOR.c = 0.5
##
mmiss.imor0.5 <- metamiss(m.publ, drop.h, drop.p, method = "IMOR", IMOR.e = 0.5)


## Figure 4
##
meths <- c("Available case analysis (ACA)",
          "Impute no events (ICA-0)", "Impute events (ICA-1)",
          "Observed risk in control group (ICA-pc)",
          "Observed risk in experimental group (ICA-pe)",
          "Observed group-specific risks (ICA-p)",
          "Best-case scenario (ICA-b)", "Worst-case scenario (ICA-w)",
          "Gamble-Hollis analysis",
          "IMOR.e = 2, IMOR.c = 2", "IMOR.e = 0.5, IMOR.c = 0.5")
##
## Use inverse-variance method for pooling (which is used for
## imputation methods)
##
m.publ.iv <- update(m.publ, method = "Inverse")
##
## Combine results (random effects)
##
mbr <- metabind(m.publ.iv,
               mmiss.0, mmiss.1,
               mmiss.pc, mmiss.pe, mmiss.p,
               mmiss.b, mmiss.w, mmiss.gh,
               mmiss.imor2, mmiss.imor0.5,
               name = meths, pooled = "random")
##
pdf("figure4.pdf", width = 7, height = 8)
##
forest(mbr, xlim = c(0.25, 4),
       label.left = "Favours placebo", label.right = "Favours haloperidol",
       leftcols = "studlab", leftlab = "Meta-Analysis Method",
       type.study = "diamond",
       hetlab = "", print.Q = TRUE, fs.study = 10)
##
#dev.off()


## Section 'Assessing and accounting for small-study effects'
##
##

## Funnel plot
##
funnel(m.publ)


## Harbord's score test for funnel plot asymmetry
##
metabias(m.publ, method.bias = "score")


## Trim-and-fill method
##
tf.publ = trimfill(m.publ)
tf.publ
summary(tf.publ)

funnel(tf.publ)


## Limit meta-analysis
##
l1.publ = limitmeta(m.publ)
l1.publ





##
##
## Figure 5
##
##

pdf("figure5.pdf", width = 10, height = 10)
##
par(mfrow = c(2, 2), pty = "s",
    oma = c(0, 0, 0, 0), mar = c(4.1, 3.1, 2.1, 1.1))
##
funnel(m.publ, xlim = c(0.05, 50), axes = FALSE)
axis(1, at = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 50))
axis(2, at = c(0, 0.5, 1, 1.5))
box()
title(main = "Panel A: Funnel plot", adj = 0)
##
funnel(m.publ, xlim = c(0.05, 50), axes = FALSE,
       contour.levels = c(0.9, 0.95, 0.99),
       col.contour = c("darkgray", "gray", "lightgray"))
legend("topright",
       c("p < 1%", "1% < p < 5%", "5% < p < 10%", "p > 10%"),
       fill = c("lightgray", "gray", "darkgray", "white"),
       border = "white", bg = "white")
axis(1, at = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 50))
axis(2, at = c(0, 0.5, 1, 1.5))
box()
title(main = "Panel B: Contour-enhanced funnel plot", adj = 0)
##
funnel(tf.publ, xlim = c(0.05, 50), axes = FALSE)
axis(1, at = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 50))
axis(2, at = c(0, 0.5, 1, 1.5))
box()
title(main = "Panel C: Trim-and-fill method", adj = 0)
##
funnel(l1.publ, xlim = c(0.05, 50), axes = FALSE,
       col.line = 8, lwd.line = 3)
axis(1, at = c(0.1, 0.2, 0.5, 1, 2, 5, 10, 50))
axis(2, at = c(0, 0.5, 1, 1.5))
box()
title(main = "Panel D: Limit meta-analysis", adj = 0)
##
# dev.off()

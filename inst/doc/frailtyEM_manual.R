### R code from vignette source 'frailtyEM_manual.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: roptions
###################################################
options(prompt="R> ")
options(width=60)


###################################################
### code chunk number 2: emfrail0
###################################################
library("frailtyEM")


###################################################
### code chunk number 3: emfrail1 (eval = FALSE)
###################################################
## emfrail(formula, data, distribution, control, ...)


###################################################
### code chunk number 4: emfrail2
###################################################
str(emfrail_dist())


###################################################
### code chunk number 5: cgd1
###################################################
data("cgd")


###################################################
### code chunk number 6: cgd2
###################################################
gam <- emfrail(Surv(tstart, tstop, status) ~ sex + treat + cluster(id),
  data = cgd)
summary(gam, lik_ci = TRUE)


###################################################
### code chunk number 7: bladder2_cumhaz
###################################################
library("ggplot2")
p1 <- autoplot(gam, type = "pred",
  newdata = data.frame(sex = "male", treat = "rIFN-g")) +
  ggtitle("rIFN-g") + ylim(c(0, 2)) + theme_minimal()

p2 <- autoplot(gam, type = "pred",
  newdata = data.frame(sex = "male", treat = "placebo")) +
  ggtitle("placebo") + ylim(c(0, 2)) + theme_minimal()


###################################################
### code chunk number 8: cgd_pred_plots
###################################################
pdf("cgd_pred.pdf", width = 9, height = 3)
gridExtra::grid.arrange(p1, p2, nrow = 1)
dev.off()


###################################################
### code chunk number 9: pred1 (eval = FALSE)
###################################################
## dat_pred <- data.frame(sex = c("male", "male"),
##   treat = c("rIFN-g", "placebo"))
## predict(gam, dat_pred)


###################################################
### code chunk number 10: pred_timechange
###################################################
dat_pred_b <- data.frame(sex = c("male", "male"),
  treat = c("placebo", "rIFN-g"),
  tstart = c(0, 200), tstop = c(200, Inf))

p <- autoplot(gam, type = "pred", newdata = dat_pred_b, individual = TRUE) +
  ggtitle("change placebo to rIFN-g at time 200") + theme_minimal()


###################################################
### code chunk number 11: cgd_pred_change
###################################################
pdf("cgd_treatdif.pdf", width = 5, height = 4)
p
dev.off()


###################################################
### code chunk number 12: bladder2_stable
###################################################
stab <- emfrail(Surv(tstart, tstop, status) ~ sex + treat + cluster(id),
  data = cgd,
  distribution = emfrail_dist(dist = "stable"))
summary(stab)


###################################################
### code chunk number 13: bladder2_hazardratios
###################################################
ig <- emfrail(Surv(tstart, tstop, status) ~ sex + treat + cluster(id),
  data = cgd,
  distribution = emfrail_dist(dist = "pvf"))

newdata <- data.frame(treat = c("placebo", "rIFN-g"),
  sex = c("male", "male"))

pl1 <- autoplot(gam, type = "hr", newdata = newdata) +
  ggtitle("gamma") + theme_minimal()

pl2 <- autoplot(stab, type = "hr", newdata = newdata) +
  ggtitle("PS") + theme_minimal()

pl3 <- autoplot(ig, type = "hr", newdata = newdata) +
  ggtitle("IG") + theme_minimal()


###################################################
### code chunk number 14: cgd_hr_plots
###################################################
pdf("cgd_hr.pdf", width = 9, height = 3)
gridExtra::grid.arrange(pl1, pl2, pl3, nrow = 1)
dev.off()


###################################################
### code chunk number 15: kidney1
###################################################
data("kidney")
kidney$sex <- ifelse(kidney$sex == 1, "male", "female")

m_gam <- emfrail(Surv(time, status) ~ age + sex + cluster(id),
  data = kidney)
s_gam <- summary(m_gam)
s_gam


###################################################
### code chunk number 16: kidney1
###################################################
m_stab <- emfrail(Surv(time, status) ~ age + sex + cluster(id),
  data = kidney,
  distribution = emfrail_dist(dist = "stable"))
s_stab <- summary(m_stab)
s_stab


###################################################
### code chunk number 17: kidney3
###################################################
zph1 <- cox.zph(coxph(Surv(time, status) ~ age + sex + cluster(id),
  data = kidney))
zph1


###################################################
### code chunk number 18: kidney4
###################################################
off_z_gam <- log(s_gam$frail$z)[match(kidney$id, s_gam$frail$id)]
off_z_stab <- log(s_stab$frail$z)[match(kidney$id, s_stab$frail$id)]

zph_gam <- cox.zph(coxph(Surv(time, status) ~
  age + sex + offset(off_z_gam) + cluster(id),
  data = kidney))
zph_stab <- cox.zph(coxph(Surv(time, status) ~
  age + sex + offset(off_z_stab) + cluster(id),
  data = kidney))
zph_gam
zph_stab



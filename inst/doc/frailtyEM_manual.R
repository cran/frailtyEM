### R code from vignette source 'frailtyEM_manual.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: test1
###################################################
library("frailtyEM")


###################################################
### code chunk number 2: bladder1
###################################################
data("cgd")


###################################################
### code chunk number 3: bladder2
###################################################
m1 <- emfrail(.data = cgd,
              .formula = Surv(tstart, tstop, status) ~ sex + treat + cluster(id))


###################################################
### code chunk number 4: distribution
###################################################
str(emfrail_distribution())


###################################################
### code chunk number 5: summary
###################################################
sm1 <- summary(m1, lik_ci = TRUE)
sm1


###################################################
### code chunk number 6: bladder2_asd
###################################################
m_cph <- coxph(Surv(tstart, tstop, status) ~ sex + treat + frailty(id),
            data = cgd,
            ties = "breslow")
m_cph


###################################################
### code chunk number 7: bladder2_frailtyestimates
###################################################
plot(exp(m_cph$frail),
     sm1$frail$z,
     xlab = "frailty estimates (coxph)",
     ylab = "frailty estimates (emfrail)")
abline(0,1)


###################################################
### code chunk number 8: bladder2_cumhaz
###################################################
library("ggplot2")
p1 <- ggplot_pred(m1,
          newdata = data.frame(sex = "male", treat = "rIFN-g")) +
  ggtitle("rIFN-g") + ylim(c(0, 2)) +
  theme_minimal()
p2 <- ggplot_pred(m1,
          newdata = data.frame(sex = "male", treat = "placebo")) +
    ggtitle("placebo") + ylim(c(0, 2)) +
    theme_minimal()

gridExtra::grid.arrange(p1, p2, nrow = 1)


###################################################
### code chunk number 9: bladder2_stable
###################################################
m2 <- emfrail(.data = cgd,
              .formula = Surv(tstart, tstop, status) ~ treat + sex + cluster(id),
              .distribution = emfrail_distribution(dist = "stable"))
summary(m2)


###################################################
### code chunk number 10: bladder2_hazardratios
###################################################
pl1 <- ggplot_hr(m1,
        newdata = data.frame(treat = c("placebo", "rIFN-g"),
                             sex = c("male", "male"))) +
  ggtitle("gamma") +
  theme_minimal()

pl2 <- ggplot_hr(m2,
        newdata = data.frame(treat = c("placebo", "rIFN-g"),
                             sex = c("male", "male"))) +
  ggtitle("stable") +
  theme_minimal()

gridExtra::grid.arrange(pl1, pl2, nrow = 1)


###################################################
### code chunk number 11: kidney1
###################################################
data(kidney)
kidney$sex <- ifelse(kidney$sex == 1, "male", "female")

m_gam <- emfrail(.data = kidney,
                 .formula = Surv(time, status) ~ age + sex + cluster(id))
summary(m_gam)


###################################################
### code chunk number 12: kidney1
###################################################
m_stab <- emfrail(.data = kidney,
                 .formula = Surv(time, status) ~ age + sex + cluster(id),
                 .distribution = emfrail_distribution(dist = "stable"))
summary(m_stab)


###################################################
### code chunk number 13: kidney3
###################################################
zph1 <- cox.zph(coxph(Surv(time, status) ~ age + sex + cluster(id), data = kidney))
zph1


###################################################
### code chunk number 14: kidney4
###################################################
s_gam <- summary(m_gam)
off_z <- log(s_gam$frail$z)[match(kidney$id, s_gam$frail$id)]
zph2 <- cox.zph(coxph(Surv(time, status) ~
                        age + sex + offset(off_z) + cluster(id),
                      data = kidney))
zph2



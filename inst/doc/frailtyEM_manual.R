### R code from vignette source 'frailtyEM_manual.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: test1
###################################################
library(frailtyEM)


###################################################
### code chunk number 2: bladder1
###################################################
bladder2$rx <- as.factor(bladder2$rx)


###################################################
### code chunk number 3: bladder2
###################################################
m1 <- emfrail(.data = bladder2,
              .formula = Surv(start, stop, event) ~ rx + number + size + cluster(id))
m1


###################################################
### code chunk number 4: distribution
###################################################
str(emfrail_distribution())


###################################################
### code chunk number 5: distribution
###################################################
m1


###################################################
### code chunk number 6: summary
###################################################
sm1 <- summary(m1)
sm1


###################################################
### code chunk number 7: bladder2_asd
###################################################
m_cph <- coxph(Surv(start, stop, event) ~ rx + number + size + frailty(id),
            data = bladder2,
            ties = "breslow")
m_cph


###################################################
### code chunk number 8: bladder2_frailtyestimates
###################################################
plot(exp(m_cph$frail),
     sm1$frail$z,
     xlab = "frailty estimates (coxph)",
     ylab = "frailty estimates (emfrail)")
abline(0,1)


###################################################
### code chunk number 9: bladder2_cumhaz
###################################################
par(mfrow=c(1,2))
plot_pred(m1,
          newdata = data.frame(rx = "2", number = 3, size = 3),
          ylim = c(0,4),
          main = "treatment")
plot_pred(m1,
          newdata = data.frame(rx = "1", number = 3, size = 3),
          ylim = c(0,4),
          main = "placebo")


###################################################
### code chunk number 10: bladder2_stable
###################################################

m2 <- emfrail(.data = bladder2,
              .formula = Surv(start, stop, event) ~ rx + number + size + cluster(id),
              .distribution = emfrail_distribution(dist = "stable"))
summary(m2)


###################################################
### code chunk number 11: bladder2_hazardratios
###################################################
par(mfrow=c(1,2))
plot_hr(m1,
        newdata = data.frame(rx = c("1", "2"), number = 3, size = 3),
        main = "gamma")

plot_hr(m2,
        newdata = data.frame(rx = c("1", "2"), number = 3, size = 3),
        main = "stable")



###################################################
### code chunk number 12: kidney1
###################################################
data(kidney)
kidney$sex <- ifelse(kidney$sex == 1, "male", "female")

m_gam_d <- emfrail(.data = kidney,
                 .formula = Surv(time, status) ~ age + sex + disease + cluster(id))
summary(m_gam_d)

m_gam <- emfrail(.data = kidney,
                 .formula = Surv(time, status) ~ age + sex + cluster(id))
summary(m_gam)


###################################################
### code chunk number 13: kidney1
###################################################
m_stab <- emfrail(.data = kidney,
                 .formula = Surv(time, status) ~ age + sex + cluster(id),
                 .distribution = emfrail_distribution(dist = "stable"))
summary(m_stab)


###################################################
### code chunk number 14: kidney3
###################################################
zph1 <- cox.zph(coxph(Surv(time, status) ~ age + sex + cluster(id), data = kidney))
zph1


###################################################
### code chunk number 15: kidney4
###################################################
s_gam <- summary(m_gam)
off_z <- log(s_gam$frail$z)[match(kidney$id, s_gam$frail$id)]
zph2 <- cox.zph(coxph(Surv(time, status) ~
                        age + sex + offset(off_z) + cluster(id),
                      data = kidney))
zph2



####### R Code Companion to "Modern Statistical Methods for HCI"
####### Chapter 7: "Nonparametric Statistics in Human-Computer Interaction"
####### By Jacob O. Wobbrock and Matthew Kay, University of Washington

### Note: Where library(name) is used, you may first need to do install.packages("name")
### to download and install the R package that enables access to that library.



####### one-sample tests, 2 response levels

prefs1AB = read.csv("chapter7/prefs1AB.csv")

### one-sample t-test
t.test(prefs1AB$email_preference == "A-mail", mu=0.5)

### chi-squared test
email_preferences = xtabs( ~ email_preference, data=prefs1AB)
chisq.test(email_preferences)

### binomial test
binom.test(email_preferences)



####### one-sample tests, 3+ response levels

### multinomial test
prefs1ABC = read.csv("chapter7/prefs1ABC.csv")
library(XNomial)
email_preferences = xtabs( ~ email_preference, data=prefs1ABC)
xmulti(email_preferences, c(1/3, 1/3, 1/3), stat="Prob")

# post hoc binomial tests
a_test = binom.test(sum(prefs1ABC$email_preference == "A-mail"), nrow(prefs1ABC), p=1/3)
b_test = binom.test(sum(prefs1ABC$email_preference == "B-mail"), nrow(prefs1ABC), p=1/3)
c_test = binom.test(sum(prefs1ABC$email_preference == "C-mail"), nrow(prefs1ABC), p=1/3)
p.adjust(c(a_test$p.value, b_test$p.value, c_test$p.value))

### chi-square test 
email_preferences = xtabs( ~ email_preference, data=prefs1ABC)
chisq.test(email_preferences)



####### N-sample tests

### chi-square test
prefs2ABC = read.csv("chapter7/prefs2ABC.csv")
# we specify multiple factors in the xtabs formula to get 
# crosstabs of higher dimensions
email_preferences = xtabs( ~ email_preference + team, data=prefs2ABC)
chisq.test(email_preferences)

### G-test
library(RVAideMemoire)
G.test(email_preferences)

### Fisher's Exact Test
fisher.test(email_preferences)



####### single-factor between-subjects tests, 2 levels

### testing anova assumptions

# histograms
salesXY = read.csv("chapter7/salesXY.csv")
hist(salesXY[salesXY$team == "X",]$sales)
hist(salesXY[salesXY$team == "Y",]$sales)

# Shapiro-Wilk normality test
shapiro.test(salesXY[salesXY$team == "X",]$sales)
shapiro.test(salesXY[salesXY$team == "Y",]$sales)

# Kolmogorov-Smirnov normality test
library(nortest)
lillie.test(salesXY[salesXY$team == "X",]$sales)
lillie.test(salesXY[salesXY$team == "Y",]$sales)

# Anderson-Darling normality test
library(nortest)
ad.test(salesXY[salesXY$team == "X",]$sales)
ad.test(salesXY[salesXY$team == "Y",]$sales)

# Levene's test for homogeneity of variance, aka homoscedasticity
library(car)
leveneTest(sales ~ team, data=salesXY)


### independent-samples t-test
t.test(sales ~ team, data=salesXY)

### median test
library(coin)
# the distribution="exact" parameter specifies the exact version of this
# test, and can be dropped if an asymptotic test is needed (e.g.,
# if this code takes too long to execute).
median_test(sales ~ team, data=salesXY, distribution="exact")
# incidently, this seems to give the same results as:
fisher.test(xtabs(cbind(sales > median(sales), sales <= median(sales)) ~ team, data=salesXY))

### Mann-Whitney U test
library(coin)
wilcox_test(sales ~ team, data=salesXY, distribution="exact")
# similar results are given by: 
wilcox.test(sales ~ team, data=salesXY)


####### single-factor between-subjects tests, 3+ levels

### parametric one-way anova
salesXYZ = read.csv("chapter7/salesXYZ.csv")
hist(salesXYZ[salesXYZ$team == "Z",]$sales)
summary(aov(sales ~ team, data=salesXYZ))

### Kruskal-Wallis test
library(coin)
kruskal_test(sales ~ team, data=salesXYZ, distribution="asymptotic")
# the same results are given by: 
kruskal.test(sales ~ team, data=salesXYZ)



####### single-factor within-subjects tests, 2 levels

salesYY = read.csv("chapter7/salesYY.csv")

### paired-samples t-test
library(reshape2)	# for dcast
# for a paired t-test we must use a wide-format table; most functions
# in R do not require a wide-format table, but the dcast function
# offers a quick way to translate long-format into wide-format when
# we do need it.
salesYY_wide = dcast(salesYY, subject ~ watch, value.var="sales")
t.test(salesYY_wide$pre, salesYY_wide$post, paired=TRUE)

# generate summary statistics for the sales, split
# into groups according to the levels of watch
library(plyr)
ddply(salesYY, ~ watch, function(data) summary(data$sales))


### sign test
# assuming salesYY_wide was constructed as above by dcast,
# we can conduct a sign test simply by crosstabulating the 
# number of times post-watch sales are greater than pre-watch
# sales.
post_sales_greater = xtabs( ~ post > pre, data=salesYY_wide)
binom.test(post_sales_greater)


### wilcoxon signed-rank test
library(coin)
wilcoxsign_test(sales ~ watch | subject, data=salesYY, distribution="exact")



####### single-factor within-subjects tests, 3+ levels

### one-way repeated measures ANOVA
salesYY2 = read.csv("chapter7/salesYY2.csv")
hist(salesYY2[salesYY2$watch == "two",]$sales)

# Mauchly's test of sphericity
library(ez)
# here we specify the dependent variable (sales), within-subjects
# variables (watch), and the variable that identifies subjects
# (subject).
m = ezANOVA(dv=sales, within=watch, wid=subject, data=salesYY2)
# we then check the model for violations of sphericity
m$Mauchly
# and given no violations, examine the uncorrected ANOVA. If
# violations were found, we would instead look at m$Sphericity.
m$ANOVA


### Friedman test
library(coin)
friedman_test(sales ~ watch | subject, data=salesYY2, distribution="asymptotic")
# conduct all pairwise comparisons among the three levels of watch (none, one, two)
library(plyr)
# make all pairwise combinations of levels of the watch factor, equivalent to:
# combn(levels(salesYY2$watch), 2, simplify=FALSE)
comparisons = list(c("none", "one"), c("none", "two"), c("one", "two"))
# run wilcoxon signed-rank tests on each pair of levels, collecting 
# the test statistic and the p-value into a single table
post_hoc_tests = ldply(comparisons, function(watch_levels) {
    wt = wilcoxsign_test(sales ~ factor(watch) | subject, 
        data=salesYY2[salesYY2$watch %in% watch_levels,],
        distribution="exact")
    data.frame(
        comparison = paste(watch_levels, collapse=" - "),
        z = statistic(wt),
        pvalue = pvalue(wt) 
    )
})
# derive adjusted p values using Holm's sequential Bonferroni procedure
post_hoc_tests$adjusted_pvalue = p.adjust(post_hoc_tests$pvalue, method="holm") 
post_hoc_tests
# finally examine the median sales by level of watch (none, one, two)
ddply(salesYY2, ~ watch, function(data) summary(data$sales))



####### Multifactor tests

####### factorial anova
salesYY2city = read.csv("chapter7/salesYY2city.csv")

### mixed anova (parametric)
library(ez) 
m = ezANOVA(dv=sales, between=city, within=watch, wid=subject, data=salesYY2city)
m$Mauchly
m$ANOVA
# the results are the same as for this method:
m = aov(sales ~ watch * city + Error(subject/watch), data=salesYY2city)
summary(m)
# interaction plot
with(salesYY2city, interaction.plot(watch, city, sales))


### aligned rank transform (nonparametric)
library(ARTool)
m = art(sales ~ watch * city + (1|subject), data=salesYY2city)
anova(m)
# pairwise post hoc tests among levels of watch factor
library(lsmeans)
lsmeans(artlm(m, "watch"), pairwise ~ watch)



####### Generalized Linear Models (GLM, aka GZLM)

### multinomial logistic regression (aka nominal logistic regression)

# one factor, team
prefs2ABC = read.csv("chapter7/prefs2ABC.csv")
library(nnet) # for multinom
library(car) # for Anova
m = multinom(email_preference ~ team, data=prefs2ABC)
Anova(m)

# two factor, team * sex
prefs2ABCsex = read.csv("chapter7/prefs2ABCsex.csv")
library(nnet) # for multinom
library(car) # for Anova
contrasts(prefs2ABCsex$team) <- "contr.sum"
contrasts(prefs2ABCsex$sex) <- "contr.sum"
m = multinom(email_preference ~ team * sex, data=prefs2ABCsex)
Anova(m, type=3)


### ordinal logistic regression
prefs2ABClove = read.csv("chapter7/prefs2ABClove.csv")
library(MASS) # for polr
library(car) # for Anova
contrasts(prefs2ABClove$team) <- "contr.sum"
contrasts(prefs2ABClove$sex) <- "contr.sum"
# transform the numeric variable love into an ordinal variable
prefs2ABClove$love = ordered(prefs2ABClove$love)
# run ordinal logistic regression
m = polr(love ~ team * sex, data=prefs2ABClove)
Anova(m, type=3)


### poisson regression
prefs2ABClate = read.csv("chapter7/prefs2ABClate.csv")
contrasts(prefs2ABClate$team) <- "contr.sum"
contrasts(prefs2ABClate$sex) <- "contr.sum"
contrasts(prefs2ABClate$email_preference) <- "contr.sum"
m = glm(late_responses ~ team * sex * email_preference, data=prefs2ABClate, family=quasipoisson)
Anova(m, type=3)
# summary stats
library(plyr)
ddply(prefs2ABClate, ~ team, function(data) summary(data$late_responses))

# extract the estimated ratio of rates of late responses between the two sales teams
library(multcomp) # for glht
library(lsmeans) # for lsm
team_effect = confint(glht(m, lsm(pairwise ~ team)))
team_effect
# effects of a Poisson model are on a log scale because
# of the log link function, so exponentiate the effects
# to interpret them.
exp(team_effect$confint)

# another way to estimate differences between teams
library(lsmeans)
lsmeans(m, pairwise ~ team, type="response")


### gamma / exponential regression
# A Gamma distribution applies to skewed, continuous data with a theoretical minimum, 
# often zero. It is defined by two parameters, "shape" and "scale." The inverse of 
# the scale parameter is called the "rate." The Exponential distribution is a special 
# case of the Gamma distribution where the shape parameter equals one.
salesXYsex = read.csv("chapter7/salesXYsex.csv")
# visualize the distributions by sex
hist(salesXYsex[salesXYsex$sex == "M",]$sales)
hist(salesXYsex[salesXYsex$sex == "F",]$sales)
# conduct the gamma regression
contrasts(salesXYsex$team) <- "contr.sum"
contrasts(salesXYsex$sex) <- "contr.sum"
m = glm(sales ~ team * sex, data=salesXYsex, family=Gamma(link="log"))
Anova(m, type=3)



####### Generalized Linear Mixed Models (GLMM)

salesYY2city = read.csv("chapter7/salesYY2city.csv")
library(lme4) # for glmer
library(lmerTest)
library(car) # for Anova
contrasts(salesYY2city$city) <- "contr.sum"
contrasts(salesYY2city$watch) <- "contr.sum"
# here (1|subject) indicates a random intercept
# dependent on subject
m = glmer(sales ~ city * watch + (1|subject), data=salesYY2city, family=Gamma(link="log"))
Anova(m, type=3)



####### Generalized Estimating Equaations (GEE)

salesYY2city = read.csv("chapter7/salesYY2city.csv")
library(geepack) # for geeglm
# geeglm requires data sorted by grouping variable, so we sort
# by subject (so that all rows for a given subject are
# contiguous)
salesYY2city = salesYY2city[order(salesYY2city$subject),] 
m = geeglm(sales ~ city * watch, id=subject, data=salesYY2city, family=Gamma(link="log"))
anova(m)

# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

hello <- function() {
  print("Hello, world!")
}


LinearRegression <- function() {
  paste("######  import cars data set  ######")
  data(cars)
  names(cars)

  paste("###### Visualize the relationship between the two variables and find the correlation coefficient ######")
  plot(cars$speed, cars$dist, xlab = "Speed", ylab = "Distance")
  cor(cars$speed, cars$dist)

  paste("#####  Fit a meaningful linear regression model and assess the adequacy of the fitted model.  #####")
  model <- lm(dist ~ speed, data = cars)
  summary(model)

  paste("#####  Residual analysis  ######")
  SR <- rstandard(model)
  paste("#####  Standardized residuals vs fitted values  #######")
  plot(model$fitted.values, SR, xlab = "Fitted values", ylab = "Standardized residuals")
  abline(h = 0, col = "red")

  paste("##### Check normality of residuals")
  qqnorm(SR, ylab = "Standardized Residuals of Model")
  qqline(SR, col = "red")
  shapiro.test(SR)
  hist(SR, col = 2, probability = TRUE)
  paste("###### Check outliers based on standardized residuals")
  which(SR > 3 | SR < (-3)) # no outlier
  paste("###### Goodness of fit")
  R2 <- summary(model)$r.squared
}

ContigencyTables_01 <- function() {
  library(epitools)
  data(wcgs)
  attach(wcgs)

  paste("## Construct a contingency table for behavior pattern type and CHD occurrence.")
  tab = table(dibpat0, chd69)

  paste("## Estimate the ‘risk ratio’ and the ‘odds ratio’ considering behavior pattern type B as the reference level.")
  riskratio(tab)
  riskratio(tab)$measure #2.2191
  oddsratio(tab)
  oddsratio(tab)$measure #2.3697

  paste("## Conduct a suitable statistical test to check the independence of the two variables")
  expected(tab)
  chisq.test(tab)
  #p-value =4.069e-10<0.05
  #reject H0
  #There is a relationship b/w 2 variables
}

ContigencyTables_02 <- function(file_path) {
  paste("# age group - Age of the person. Levels: under 50, 50 +.")
  paste("# vaccine status - Vaccination status of the person. Levels: vaccinated, unvaccinated")
  paste("# outcome - Did the person die from the Delta variant? Levels: death and survived.")

  paste("# Load the dataset into R.")
  paste("# Construct a 2 × 2 table to summarize the variables vaccine status and outcome.")
  covid <- read.csv(file_path)
  covid$outcome <- as.factor(covid$outcome)
  covid$outcome <- relevel(covid$outcome, ref = "survived")
  tab2 <- table(covid$vaccine_status, covid$outcome)

  paste("# Calculate the ‘risk ratio’ to quantify the association between vaccine status and outcome.")
  riskratio(tab2)$measure

  paste("# Re-calculate the risk ratios in subgroups defined by the age group and comment on your findings.")
  u50 <- covid[covid$age_group == "under 50", ]
  o50 <- covid[covid$age_group == "50 +", ]
  paste("#Risk ratio for under 50 group")
  tab_u50 <- table(u50$vaccine_status, u50$outcome)
  tab_u50
  riskratio(tab_u50)$measure #0.7191
  paste("#Risk ratio for 50+ group")
  tab_o50 <- table(o50$vaccine_status, o50$outcome)
  tab_o50
  riskratio(tab_o50)$measure #0.2827
  paste("#Although the overall risk ratio, which is larger than 1, indicates that vaccination increases")
  paste("#the risk of death, once we consider the two subgroups separately it shows that vaccination")
  paste("#reduces the risk of death. This phenomenon is called the \"Simpson's Paradox\" or \"Yule–Simpson effect\".")
}

CRE_01 <- function() {
  paste("## Consider a completely randomized experiment (CRE) with a binary treatment A where n = 5 and n1 = 3.")
  paste("## How many different treatment assignment mechanisms are possible?")
  choose(5, 3)

  paste("## Write an R function to generate all possible treatment assignment mechanisms for any given n and n1.")
  paste("#  (Hint: Use the functions choose and combn)")
  trtassignment <- function(n, n1) {
    M <- choose(n, n1)
    treat.index <- combn(n, n1)
    A <- matrix(0, n, M)
    for (i in 1:M) {
      treat <- treat.index[, i]
      A[treat, i] <- 1
    }
    A
  }

  paste("## Find all the possible treatment assignment mechanisms for a CRE with n = 5 and n1 = 3.")
  trtassignment(5, 3)
}

CRE_02 <- function(file_path) {
  paste("## Angrist et al. (2009) conducted an experiment to evaluate different strategies to improve academic")
  paste("## performance among college freshmen in a Canadian university. The data is available in the file")
  paste("## “star.dta”. The outcome is the GPA at the end of the first year. We will focus on two covariates:")
  paste("## gender (encoded by female) and baseline GPA(encoded by gpa0).")

  paste("## Extract a subset of the dataset which only contains records for the control group (encoded")
  paste("## by the variable control) and the treatment group which was offered academic support services")
  paste("## and financial incentives for good grades (encoded by the variable sfsp).")
  library("foreign")
  angrist = read.dta(file_path)
  table(angrist$control, angrist$sfsp)
  data <- subset(angrist, control == 1 | sfsp == 1)
  str(data)

  paste("## Impute the missing outcomes with the observed average.")
  y <- data$GPA_year1
  meany <- mean(y, na.rm = TRUE)
  y <- ifelse(is.na(y), meany, y)
  mean(y)

  paste("## Check the balance of the two covariates. (Hint: For each covariate, conduct a suitable two-sample test to compare the covariate means or proportions")
  paste("## in the control and treatment groups)")
  a <- data$sfsp
  addmargins(table(data$female, a), c(1, 2))
  prop.test(x = c(574, 82), n = c(1006, 150), correct = FALSE)

  gpa0a0 <- data$gpa0[a == 0]
  gpa0a1 <- data$gpa0[a == 1]
  var.test(gpa0a1, gpa0a0)
  t.test(gpa0a1, gpa0a0, var.equal = TRUE)

  paste("## Estimate the ATE without adjusting for covariates. (Hint: The unadjusted estimator is")
  paste("## numerically identical to the difference-in-means of the outcome)")
  mean(y[a == 1]) - mean(y[a == 0])
  fit <- lm(y ~ a)
  summary(fit)
  ate_unadj <- coef(fit)[2] #0.0518

  paste("## Estimate the ATE adjusted for the covariates.")
  x <- data[, c("female", "gpa0")]
  x <- scale(x)
  fitadj <- lm(y ~ a * x)
  summary(fitadj)
  ate_adj <- coef(fitadj)[2] #0.0682
}









FRD_01 <- function() {

  paste("#### Table 1 presents a subsample of observations from a completely randomized experiment to evaluate")
  paste("#### the effect of an educational television program on reading skills. The unit is a class of students.")
  paste("#### The outcome of interest is the average reading score in the class. Half of the classes were shown")
  paste("#### the program of interest, and the other half did not have access to the program")

  paste("#     Unit  Treatment     Observed Outcome(Y_i)      Potential Outcomes (Y_i(0)   Y_i(1) )")
  paste("#     1       0              55.0                             _             __")

  paste("### Complete the two columns of potential outcomes.")
  data <- data.frame(
    Unit = 1:6,
    Treatment = c(0, 1, 0, 1, 0, 1),
    ObservedOutcome = c(55.0, 70.0, 72.0, 66.0, 72.7, 78.9)
  )

  paste("# Assuming potential outcomes are observable outcomes under the given treatment")
  data$Yi0 <- ifelse(data$Treatment == 0, data$ObservedOutcome, NA)
  data$Yi1 <- ifelse(data$Treatment == 1, data$ObservedOutcome, NA)

  paste("# Printing the updated data frame")
  print(data)

  paste("##  State the null hypothesis to conduct the Fisher’s randomization test")
  paste("#H0:Yi(1)=Yi(0) for alli")

  paste("##  Fill in the missing values for potential outcomes under the null hypothesis.")
  paste("# Assuming data from part (a)")
  data$Yi0[is.na(data$Yi0)] <- data$ObservedOutcome[is.na(data$Yi0)]
  data$Yi1[is.na(data$Yi1)] <- data$ObservedOutcome[is.na(data$Yi1)]
  print(data)

  paste("###  Consider the difference-in-means as the test statistic and compute it for the observed data.")
  diffmean <- (sum(T * Y) - sum((1 - T) * Y)) / 3 # 5.067

  paste("#### How many different treatment assignments are possible?")
  M <- choose(6, 3) # 20

  paste("#### Derive the randomization distribution of the test statistic, compute the exact p-value and")
  paste("####  state your conclusions.")
  trtassignment <- function(n, n1) {
    M <- choose(n, n1)
    treat.index <- combn(n, n1)
    A <- matrix(0, n, M)
    for (i in 1:M) {
      treat <- treat.index[, i]
      A[treat, i] <- 1
    }
    A
  }

  A <- data.frame(t(trtassignment(6, 3)))
  for (i in 1:M) {
    t <- A[i, 1:6]
    A[i, "est"] <- (sum(t * Y) - sum((1 - t) * Y)) / 3
  }
  p_FRT <- sum(A$est >= diffmean) / 20 # 0.3
  paste("#Since 0.3>0.05 we do not reject H0 and conclude that")
  paste("#there is no evidence to claim that the television program improves reading skills.")

  paste("#Your conclusion should change depending on whether you choose a two-sided p-value or not")
  p_FRT_2 <- sum(abs(A$est) >= diffmean) / 20
  paste("#Since 0.6>0.05 we do not reject H0 and conclude that")
  paste("#there is no evidence to claim that the television program has any effect on reading skills.")
}

FRD_02 <- function() {
  library(Matching)
  data(lalonde)
  a <- lalonde$treat
  y <- lalonde$re78
  n1 <- sum(a == 1)
  n0 <- sum(a == 0)

  paste("#### Estimate the average treatment effect assuming a completely randomized design.")
  paste("#Difference in means estimator")
  est.diff <- mean(y[a == 1]) - mean(y[a == 0]) # 1794.343

  paste("##### Consider the estimator you selected in part (a) as the test statistic. By randomly permuting")
  paste("### the treatment vector, obtain the Monte Carlo approximation of the randomization distribution")
  paste("### of this test statistic. (Consider 104 permutations)")
  set.seed(1234)
  MC <- 10^4
  Diffhat <- rep(0, MC)
  for (mc in 1:MC) {
    aperm <- sample(a)
    Diffhat[mc] <- mean(y[aperm == 1]) - mean(y[aperm == 0])
  }

  paste("### Compute the exact p-value using Monte Carlo approximation")
  exact.pv.diff <- mean(Diffhat >= est.diff) # 0.0022

  paste("####  Repeat parts (b) and (c) using t-statistic with equal variance assumption as the test statistic")
  t.stat <- t.test(y[a == 1], y[a == 0], var.equal = TRUE)$statistic
  TStathat <- rep(0, MC)
  for (mc in 1:MC) {
    aperm <- sample(a)
    TStathat[mc] <- t.test(y[aperm == 1], y[aperm == 0], var.equal = TRUE)$statistic
  }
  exact.pv.tstat <- mean(TStathat >= t.stat) # 0.0021

  paste("#### Without using Monte Carlo method, compute the asymptotic p-value for part (d), assuming")
  paste("## normal distributions for the two potential outcome distributions and equal variances")
  asymp.pv <- t.test(y[a == 1], y[a == 0], var.equal = TRUE)$p.value # 0.0048

  paste("#What are the possible reasons for any differences that you have observed between the p-values")
  paste("## in parts (c), (d) and (e)?")
  paste("#due to asymptotic approximations.")
  paste("#the default choice for t.test is a two-sided test.")
  paste("#for a fair comparison we should multiply p-values in (c) and (d) by 2.")

  paste("# Assess the covariate balance with respect to all pre-treatment variables.")
  attach(lalonde)
  paste("#age")
  var.test(age[a == 1], age[a == 0])
  t.test(age[a == 1], age[a == 0], var.equal = TRUE)
  paste("#educ")
  var.test(educ[a == 1], educ[a == 0])
  t.test(educ[a == 1], educ[a == 0], var.equal = TRUE)
  paste("#black")
  prop.test(table(a, black))
  paste("#hisp")
  prop.test(table(a, hisp))
  paste("#married")
  prop.test(table(a, married))
  paste("#nodegr")
  prop.test(table(a, nodegr))
  paste("#re74")
  var.test(re74[a == 1], re74[a == 0])
  t.test(re74[a == 1], re74[a == 0], var.equal = FALSE)
  paste("#re75")
  var.test(re75[a == 1], re75[a == 0])
  t.test(re75[a == 1], re75[a == 0], var.equal = TRUE)
  paste("#u74")
  prop.test(table(a, u74))
  paste("#u75")
  prop.test(table(a, u75))

  paste("#### Estimate a linear regression model with all pre—treatment variables as controls")
  paste("###(no interactions), and report the estimate of the average treatment effect and its standard error.")
  fit <- lm(re78 ~ ., data = lalonde)
  summary(fit)
  fit$coefficients["treat"]
}

Observatioanl_Studies <- function() {

  paste("# Use the dataset homocyst in the package senstrat. The outcome is homocysteine, the homocysteine level,")
  paste("## and the treatment is z, where z = 1 for a daily smoker and z = 0 for a never smoker.")
  paste("## Covariates are female, age3, ed3, bmi3, pov2 with detailed explanations in the package, and")
  paste("## st is a stratum indicator, defined by all the combinations of the discrete covariates.")

  paste("## How many strata have only treated or control units?")
  library(dplyr)
  no_tc <- data %>% group_by(stf) %>% summarise(no_t = sum(z == 1), no_c = sum(z == 0)) %>%
    filter(no_t == 0 | no_c == 0)
  strata <- c(no_tc$stf)

  paste("## What is the proportion of the units in these strata?")
  sum(no_tc$no_t + no_tc$no_c) / nrow(data)

  paste("## Drop these strata and perform a stratified analysis of the observational study. Report the")
  paste("## point estimator, variance estimator, and 95% confidence interval for the average causal effect.")
  df <- data %>% filter(!(stf %in% strata))
  stf.levels <- levels(droplevels(df$stf))
  tau <- 0
  N <- nrow(df)
  for (i in stf.levels) {
    y <- df[df$stf == i, "homocysteine"]
    a <- df[df$stf == i, "z"]
    pi <- length(y) / N
    n1 <- sum(a == 1)
    n0 <- sum(a == 0)
    tau <- tau + pi * (mean(y[a == 1]) - mean(y[a == 0]))
  }
  tau

  paste("#Cannot compute variance estimate V_hat because there are strata of size 1")
  paste("#One option is to compute bootstrap standard errors which we will discuss later")

  paste("#There are 19 strata with only one unit in treatment group")
  paste("#Let's remove these as well and compute V_hat")
  one_tc <- df %>% group_by(stf) %>% summarise(no_t = sum(z == 1), no_c = sum(z == 0)) %>%
    filter(no_t == 1 | no_c == 1)
  strata2 <- c(one_tc$stf)
  df2 <- df %>% filter(!(stf %in% strata2))
  stf.levels2 <- levels(droplevels(df2$stf))
  tau2 <- 0
  V <- 0
  N <- nrow(df2)
  for (i in stf.levels2) {
    y <- df2[df2$stf == i, "homocysteine"]
    a <- df2[df2$stf == i, "z"]
    pi <- length(y) / N
    n1 <- sum(a == 1)
    n0 <- sum(a == 0)
    tau2 <- tau2 + pi * (mean(y[a == 1]) - mean(y[a == 0]))
    V <- V + pi^2 * (var(y[a == 1]) / n1 + var(y[a == 0]) / n0)
  }
  tau2; V
  paste("#95% confidence interval")
  c(tau2 - 1.96 * sqrt(V), tau2 + 1.96 * sqrt(V))

  paste("########  Run the OLS of the outcome on the treatment indicator and covariates without interactions.")
  paste("#OLS with all strata")
  fit1 <- lm(homocysteine ~ 1 + z + female + age3 + ed3 + bmi3 + pov2, data = data)
  summary(fit1)
  fit1$coefficients["z"] # 1.353452
  library(car)
  sqrt(hccm(fit1, type = "hc2")[2, 2]) # 0.370545

  paste("#OLS with no treatment or control groups removed")
  fit2 <- lm(homocysteine ~ 1 + z + female + age3 + ed3 + bmi3 + pov2, data = df)
  summary(fit2)
  fit2$coefficients["z"] # 1.377624
  sqrt(hccm(fit2, type = "hc2")[2, 2]) # 0.3750971

  paste("## Report the coefficient of the treatment and the standard error.")
  paste("#Suppose we didn't remove strata with only treated or control units")
  stf.levels3 <- levels(data$stf)
  tau3 <- 0
  N3 <- nrow(data)
  for (i in stf.levels3) {
    y <- data[data$stf == i, "homocysteine"]
    a <- data[data$stf == i, "z"]
    pi <- length(y) / N3
    n1 <- sum(a == 1)
    n0 <- sum(a == 0)
    tau3 <- tau3 + pi * (mean(y[a == 1]) - mean(y[a == 0]))
  }
  tau3

  paste("## If you do not drop the strata with only treated or control units, what will happen?")
}

four_method <- function() {

  paste("## Card (1993) used the National Longitudinal Survey of Young Men to estimate the causal effect of")
  paste("# education on earnings. The dataset ‘card1995.csv’ contains 3010 men with ages between 14 and")
  paste("# 24 in the year 1966, and Card (1993) leveraged the geographic variation in college proximity as an")
  paste("# IV for education. Here, the IV ‘nearc4’ is the indicator of growing up near a four-year college,")
  paste("# the treatment ‘educ’ measures the years of education, and the outcome ‘lwage’ is the log wage")
  paste("# in the year 1976, ranging from 4.6 to 7.8.")
  paste("# Among the available additional covariates consider only the covariates exper, expersq, black,")
  paste("# south, smsa, reg661, reg662, reg663, reg664, reg665, reg666, reg667, reg668 and smsa66 for")
  paste("# the analysis.")

  library("car")
  paste("## Card Data")
  card.data <- read.csv("card1995.csv")
  Y <- card.data[, "lwage"]
  A <- card.data[, "educ"]
  Z <- card.data[, "nearc4"]
  X <- card.data[, c("exper", "expersq", "black", "south",
                     "smsa", "reg661", "reg662", "reg663",
                     "reg664", "reg665", "reg666",
                     "reg667", "reg668", "smsa66")]
  X <- as.matrix(X)
  paste("##### Fit a 2-stage least square (2SLS) regression and estimate the causal effect of ‘educ’ on ‘lwage’.")
  A_hat <- lm(A ~ Z + X)$fitted.values
  Y_hat <- lm(Y ~ A_hat + X)

  paste("#2SLS estimate of causal effect")
  twoSLS_est <- coef(Y_hat)[2]

  paste("#Obtain the corrected residuals")
  res.correct <- Y - cbind(1, A, X) %*% coef(Y_hat)
  Y_hat$residuals <- as.vector(res.correct)

  paste("#Compute the standard error")
  SE <- sqrt(hccm(Y_hat, type = "hc0")[2, 2])

  paste("#Estimate and 95% confidence interval")
  results <- c(twoSLS_est, twoSLS_est - 1.96 * SE, twoSLS_est + 1.96 * SE)
  names(results) <- c("2SLS_est", "95% CI Lower", "95% CI Upper")
  round(results, 4)
}

Propensity_Score <- function(file_path) {

  paste("# Load the dataset")
  nhanes <- read.csv(file_path)[, -1]

  paste("# Identify outcome and treatment variables")
  a <- nhanes$School_meal
  y <- nhanes$BMI
  x <- as.matrix(nhanes[, -c(1, 2)])  # Make sure to exclude the proper columns
  x <- scale(x)

  paste("# Estimate propensity scores using all covariates")
  pscore <- glm(a ~ x, family = binomial)$fitted.values

  paste("# Discretize propensity score with K = 5")
  K <- 5
  q <- quantile(pscore, (1:(K - 1)) / K)
  psq <- cut(pscore, breaks = c(0, q, 1), labels = 1:K)

  paste("# Stratification function to estimate tau and standard error")
  strat.est <- function(a, y, ps, K) {
    q <- quantile(ps, (1:(K - 1)) / K)
    psq <- cut(ps, breaks = c(0, q, 1), labels = 1:K)

    piK <- rep(0, K)
    tauK <- rep(0, K)
    vK <- rep(0, K)

    for (k in 1:K) {
      ak <- a[psq == k]
      yk <- y[psq == k]
      piK[k] <- length(ak) / length(a)
      tauK[k] <- mean(yk[ak == 1]) - mean(yk[ak == 0])
      vK[k] <- var(yk[ak == 1]) / sum(ak) + var(yk[ak == 0]) / sum(1 - ak)
    }

    return(c(sum(piK * tauK), sqrt(sum(piK^2 * vK))))
  }

  paste("# Apply for K = 5")
  strat.est(a, y, pscore, 5)

  paste("# Apply for K = 10, 20, 50, 80")
  ns <- c(5, 10, 20, 50, 80)
  results <- data.frame(K = numeric(0), est = numeric(0), se = numeric(0))
  for (i in ns) {
    est <- strat.est(a, y, pscore, i)[1]
    se <- strat.est(a, y, pscore, i)[2]
    results <- rbind(results, c(i, est, se))
  }
  print(results)

  paste("# Difference in means estimator")
  difmean <- mean(y[a == 1]) - mean(y[a == 0])
  print(c(difmean, sqrt(var(y[a == 1]) / sum(a == 1) + var(y[a == 0]) / sum(a == 0))))

  paste("# Regression model without covariates")
  fit1 <- lm(y ~ a)
  print(c(fit1$coefficients["a"], sqrt(hccm(fit1, type = "hc2")[2, 2])))

  paste("# Outcome regression without interactions")
  fit2 <- lm(y ~ a + x)
  print(c(fit2$coefficients["a"], sqrt(hccm(fit2, type = "hc2")[2, 2])))

  paste("# Outcome regression with interactions")
  fit3 <- lm(y ~ a + x + a:x)
  print(c(fit3$coefficients["a"], sqrt(hccm(fit3, type = "hc2")[2, 2])))

  paste("# Horvitz–Thompson and H´ajek estimators with truncations")
  ipw <- function(a, y, x, trnc) {
    pscore <- glm(a ~ x, family = binomial)$fitted.values
    pscore_trnc <- pmax(trnc[1], pmin(trnc[2], pscore))
    tau_ht <- mean(a * y / pscore_trnc - (1 - a) * y / (1 - pscore_trnc))
    tau_hk <- sum(a * y / pscore_trnc) / sum(a / pscore_trnc) -
      sum((1 - a) * y / (1 - pscore_trnc)) / sum((1 - a) / (1 - pscore_trnc))
    return(c(tau_ht = tau_ht, tau_hk = tau_hk))
  }

  paste("# Bootstrap standard errors")
  n.boot <- 500
  sd.boot <- function(a, y, x, trnc) {
    boot.est <- data.frame()
    n <- length(a)

    for (i in 1:n.boot) {
      id <- sample(1:n, n, replace = TRUE)
      boot.est <- rbind(boot.est, ipw(a[id], y[id], x[id,], trnc))
    }

    colnames(boot.est) <- c("tau_ht", "tau_hk")
    return(c("sd.tau_ht" = sd(boot.est$tau_ht), "sd.tau_hk" = sd(boot.est$tau_hk)))
  }

  paste("# Compute estimators for various truncations")
  trunc.list <- list(trunc0 = c(0, 1), trunc.05 = c(0.05, 0.95), trunc.1 = c(0.1, 0.9))
  results <- lapply(trunc.list, function(t) {
    est <- ipw(a, y, x, t)
    sd <- sd.boot(a, y, x, t)
    return(cbind(est, sd))
  })

  return(results)
}


# INTRODUCTION tab ----

# Run t-test:
ttest.res <- reactive({
  t.test.summ(rv$mean1, rv$mean2, rv$sd1, rv$sd2, rv$n1, rv$n2, input$H1hyp.cls)
})

# Left panel, H0 depending on the chosen H1, classic:
output$H1hyp.cls <- renderUI({ switch(input$H1hyp.cls, 
                                      "H1.diff0"    = "$\\mathcal{H}_0:\\delta=0$", 
                                      "H1.larger0"  = "$\\mathcal{H}_0:\\delta\\leq 0$", 
                                      "H1.smaller0" = "$\\mathcal{H}_0:\\delta\\geq 0$") })

# Left panel, H0 depending on the chosen H1, Bayes:
H1hyp.bys.reactive <- renderText({ switch(input$H1hypbys, 
                                          "H1.diff0"    = "$\\mathcal{H}_0:\\delta=0$", 
                                          "H1.larger0"  = "$\\mathcal{H}_0:\\delta\\leq 0$", 
                                          "H1.smaller0" = "$\\mathcal{H}_0:\\delta\\geq 0$", 
                                          "H1.point"    = "$\\mathcal{H}_0:\\delta=0$") })
output$H1hyp.bys <- renderUI({
  tagList(
    #withMathJax(),
    HTML(H1hyp.bys.reactive()),
    tags$script(HTML(js)),
    tags$script(
      async="",
      src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    )
  )
})

# General prior parameters for the introduction tab only:
location.bys   <- reactive({ switch(input$priorbys, 
                                    'cauchy'    = req(input$location.c.bys), 
                                    'normal'    = req(input$location.n.bys), 
                                    't.student' = req(input$location.t.bys)) })
scale.bys      <- reactive({ switch(input$priorbys, 
                                    'cauchy'    = req(input$scale.c.bys), 
                                    'normal'    = req(input$scale.n.bys), 
                                    't.student' = req(input$scale.t.bys)) })
df.bys         <- reactive({ switch(input$priorbys, 
                                    'cauchy'    = NULL, 
                                    'normal'    = NULL, 
                                    't.student' = req(input$df.t.bys)) })

# Prior for the introduction tab only:
prior.bys <- function(delta)
{
  switch(input$priorbys, 
         "cauchy"    = cauchy.prior(delta, location.bys(), scale.bys()), 
         "normal"    = normal.prior(delta, location.bys(), scale.bys()), 
         "t.student" = tstude.prior(delta, location.bys(), scale.bys(), df.bys()))
}

output$ttestB <- function() {
  tab <- data.frame(
    c(
      "$\\textbf{Hypotheses}$",
      "$\\textbf{Sampling distribution}\\hspace{5mm}$",
      "$\\textbf{Observed effect size}$", 
      "$\\textbf{Test statistic}$", 
      "$p\\textbf{-value}$"
    ), 
    c(
      paste0(switch(input$H1hyp.cls,
                    "H1.diff0"    = "$\\mathcal{H}_0: \\delta=0\\quad\\text{vs}\\quad",
                    "H1.larger0"  = "$\\mathcal{H}_0: \\delta\\leq 0\\quad\\text{vs}\\quad",
                    "H1.smaller0" = "$\\mathcal{H}_0: \\delta\\geq 0\\quad\\text{vs}\\quad"), 
             switch(input$H1hyp.cls,
                    "H1.diff0"    = "\\mathcal{H}_1: \\delta\\not=0$",
                    "H1.larger0"  = "\\mathcal{H}_1: \\delta>0$",
                    "H1.smaller0" = paste0("\\mathcal{H}_1: \\delta", HTML("&#60;"), "0$"))
      ), 
      paste0("$t\\text{-Student (df = }", ttest.res()["df"], "\\text{)}$"), 
      paste0("$d=", round(cohen.d(), 3), "$"), 
      paste0("$t=", round(ttest.res()["t"], 3), "$"), 
      if (round(ttest.res()["p"], 3) >= .001) paste0("$p=", round(ttest.res()["p"], 3), "$") else "$p<.001$"
    ), 
    stringsAsFactors = FALSE, 
    check.names = FALSE)
  colnames(tab) <- NULL
  
  tab %>%
    knitr::kable("html", escape = FALSE, align = 'l') %>%
    row_spec(1, extra_css = "border-top: 2px solid; padding: 3px;") %>%
    row_spec(2, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
    row_spec(3, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
    row_spec(4, extra_css = "border-top: 1px solid white; padding: 3px;") %>%
    row_spec(5, extra_css = "border-bottom: 2px solid; border-top: 1px solid white; padding: 3px;", background = "#005E3C1A") %>%
    kable_styling(full_width = FALSE)
}

output$ttest.crit <- renderPlot({
  xlimupp <- max(5, ceiling(abs(ttest.res()["t"]))+2)
  par(mar = c(3, 1, 5, 1), xpd = TRUE)
  curve(dt(x, ttest.res()["df"]), from = -xlimupp, to = xlimupp, n = 1024, 
        ylim = c(0, 1.1 * dt(0, ttest.res()["df"])), 
        bty = "n", yaxt = "n", ylab = "", lwd = 2, xaxt = "n", xlab = "", yaxs = "i", 
        main = paste0("t-Student (df = ", round(ttest.res()["df"], 3), ")"), 
        col = "#005E3C")
  axis(1, seq(-xlimupp, xlimupp, 1))
  mtext("Test statistic", 1, 2)
  switch(input$H1hyp.cls, 
         "H1.diff0" = 
           {
             x.supp.crit <- seq(qt(1-input$alpha/2, ttest.res()["df"]), xlimupp, length.out = 101)
             # polygon(x = c(x.supp.crit, rev(x.supp.crit)),
             #         y = c(rep(1.*dt(0, ttest.res()["df"]), 101), rep(0, 101)),
             #         col = "#FF000005", border = NA)
             # polygon(x = c(-x.supp.crit, rev(-x.supp.crit)),
             #         y = c(rep(1.*dt(0, ttest.res()["df"]), 101), rep(0, 101)),
             #         col = "#FF000005", border = NA)
             polygon(x = c(x.supp.crit, rev(x.supp.crit)), 
                     y = c(dt(x.supp.crit, ttest.res()["df"]), rep(0, 101)), 
                     col = "#FF00000D", border = NA)
             polygon(x = c(-x.supp.crit, rev(-x.supp.crit)),
                     y = c(dt(-x.supp.crit, ttest.res()["df"]), rep(0, 101)),
                     col = "#FF00000D", border = NA)
             segments(-xlimupp, 0, qt(input$alpha/2, ttest.res()["df"]), 0, lwd = 4, col = "#FF0000")
             segments(qt(1-input$alpha/2, ttest.res()["df"]), 0, xlimupp, 0, lwd = 4, col = "#FF0000")
             x.supp <- seq(abs(ttest.res()["t"]), xlimupp, length.out = 101)
             polygon(x = c(x.supp, rev(x.supp)), 
                     y = c(dt(x.supp, ttest.res()["df"]), rep(0, 101)), 
                     col = "#DCA55966", border = NA, density = 10, lwd = 4)
             polygon(x = c(-x.supp, rev(-x.supp)), 
                     y = c(dt(-x.supp, ttest.res()["df"]), rep(0, 101)), 
                     col = "#DCA55966", border = NA, density = 10, lwd = 4)
           }, 
         "H1.smaller0" = 
           {
             x.supp.crit <- seq(-xlimupp, qt(input$alpha, ttest.res()["df"]), length.out = 101)
             # polygon(x = c(x.supp.crit, rev(x.supp.crit)),
             #        y = c(rep(1.*dt(0, ttest.res()["df"]), 101), rep(0, 101)),
             #        col = "#FF000005", border = NA)
             polygon(x = c(x.supp.crit, rev(x.supp.crit)), 
                     y = c(dt(x.supp.crit, ttest.res()["df"]), rep(0, 101)), 
                     col = "#FF00000D", border = NA)
             segments(-xlimupp, 0, qt(input$alpha, ttest.res()["df"]), 0, lwd = 4, col = "#FF0000")
             x.supp <- seq(-xlimupp, ttest.res()["t"], length.out = 101)
             polygon(x = c(x.supp, rev(x.supp)), 
                     y = c(dt(x.supp, ttest.res()["df"]), rep(0, 101)), 
                     col = "#DCA55966", border = NA, density = 10, lwd = 4)
           }, 
         "H1.larger0" = 
           {
             x.supp.crit <- seq(qt(1-input$alpha, ttest.res()["df"]), xlimupp, length.out = 101)
             # polygon(x = c(x.supp.crit, rev(x.supp.crit)),
             #         y = c(rep(1.*dt(0, ttest.res()["df"]), 101), rep(0, 101)),
             #         col = "#FF000005", border = NA)
             polygon(x = c(x.supp.crit, rev(x.supp.crit)), 
                     y = c(dt(x.supp.crit, ttest.res()["df"]), rep(0, 101)), 
                     col = "#FF00000D", border = NA)
             segments(qt(1-input$alpha, ttest.res()["df"]), 0, xlimupp, 0, lwd = 4, col = "#FF0000")
             x.supp <- seq(ttest.res()["t"], xlimupp, length.out = 101)
             polygon(x = c(x.supp, rev(x.supp)), 
                     y = c(dt(x.supp, ttest.res()["df"]), rep(0, 101)), 
                     col = "#DCA55966", border = NA, density = 10, lwd = 4)
           }, 
  )
  segments(ttest.res()["t"], 0, ttest.res()["t"], 1.1 * dt(0, ttest.res()["df"]), lwd = 2, col = "#DCA559")
  text(ttest.res()["t"], 1.05*dt(0, ttest.res()["df"]), paste0("t = ", round(ttest.res()["t"], 3)), cex = 1.4, pos = if (ttest.res()["t"] < 0) 2 else 4)
  # text(if (ttest.res()["t"] < 0) xlimupp else -xlimupp, 1.05*dt(0, ttest.res()["df"]), paste0("Critical region (probability = ", input$alpha, ")"), pos = if (ttest.res()["t"] < 0) 2 else 4, cex = 1.4, col = "#F00000")
  # legend:
  legend("bottomleft", paste0("Critical region (probability = ", input$alpha, ")"), pch = 15, col = "#FF00000D", 
         inset=c(0,1), xpd = TRUE, horiz = TRUE, bty = "n", seg.len = 4, pt.cex = 3)
})

output$introduction1a <- renderUI({
  outtext <- paste0(
    "Null hypothesis significance testing (NHST in short) is one of the most popular tools currently in use in science for performing statistical inference.", 
    br(), 
    "Concepts such as the null hypothesis ($\\mathcal{H}_0$) and the alternative hypothesis ($\\mathcal{H}_1$), type I and II error rates, significance level, the $p$-value, 'reject' or 'fail to reject' $\\mathcal{H}_0$, all became common buzz words in science.", 
    br(), 
    "We use them all the time in our research and we pretty much feel obliged to include them in our scientific reports.", 
    br(), br(), 
    "Being this as it may be, it has been widely established that NHST and the $p$-value are poorly understood by practitioners", 
    br(), 
    "(e.g., Belia et al., 2005; Falk and Greenbaum, 1995; Goodman, 2008; Greenland et al., 2016; Haller and Kraus, 2002; Hoekstra et al., 2014; Oakes, 1986).", 
    br(), br(), 
    "This has led to various developments to try to mitigate the problem.", 
    br(), 
    "The ", em("Bayes factor"), " can be considered one such development (even though its origins go back about 100 years; Etz & Wagenmakers, 2017).", 
    br(), br(), 
    h4("Null hypothesis tests used"), 
    "We focus on one particular hypothesis testing scenario: The ", 
    em("two-sided independent samples $t$-test with equal variances assumed"), 
    ".", 
    br(), 
    "The idea is to use a very simple and familiar testing setting, so that we can allocate most of our attention on the Bayes factor itself.", 
    br(), 
    "All general guidelines discussed for this test extend to about all other Bayes factors currently available."
  )
  
  tagList(h3(strong("Background")), 
          br(), 
          HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)), 
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction1b <- renderUI({
  outtext <- paste0(
    "Thus, suppose we have two groups, say, A and B.", 
    br(), br(), 
    "It is assumed that the population associated to each group is normally distributed, with mean $\\mu_A$ (group A) and $\\mu_B$ (group B), and that the population standard deviation is the same for both groups (say, $\\sigma$).", 
    br(), br(), 
    "The hypotheses of the ", em("two-tailed"), " test are defined by $$\\mathcal{H}_0: \\mu_A = \\mu_B\\quad\\text{ versus }\\quad \\mathcal{H}_1: \\mu_A \\not= \\mu_B.$$ The null hypothesis in this test is a point value (of 0 in this case).", 
    br(), 
    "For this reason we often refer to such a test as a ", em("null hypothesis test."), 
    br(), br(), 
    em("One-tailed"), " tests are defined as follows: $$\\begin{eqnarray*}\\mathcal{H}_0: \\mu_A \\leq \\mu_B &\\quad\\text{ versus }\\quad \\mathcal{H}_1: \\mu_A > \\mu_B\\\\ \\hphantom{\\mathcal{H}_0: \\mu_A \\leq \\mu_B} &\\quad\\text{ and }\\quad \\hphantom{\\mathcal{H}_1: \\mu_A > \\mu_B}\\\\ \\mathcal{H}_0: \\mu_A \\geq \\mu_B &\\quad\\text{ versus }\\quad \\mathcal{H}_1: \\mu_A < \\mu_B.\\end{eqnarray*}$$", 
    br(), 
    "Equivalently, defining $\\mu_D=(\\mu_A-\\mu_B)$, the same tests can be reexpressed in terms of parameter $\\mu_D$", 
    br(), 
    "(for example, the two-tailed test becomes $\\mathcal{H}_0: \\mu_D = 0 \\text{ versus } \\mathcal{H}_1: \\mu_D \\not= 0.$)", 
    br(), br(), 
    h4("Hypotheses in Bayesian testing"), 
    "For the Bayesian independent samples $t$-test used in this app, the parameter being tested is actually a ", em("standardized effect size"), " defined as $\\delta=\\frac{\\mu_D}{\\sigma}$ (Rouder et al., 2009).", 
    br(), 
    "The hypotheses being tested are: $$\\begin{eqnarray*}\\mathcal{H}_0: \\delta=0 &\\quad\\text{ versus }\\quad \\mathcal{H}_1: \\delta\\not=0,\\\\ \\mathcal{H}_0: \\delta\\leq 0 &\\quad\\text{ versus }\\quad \\mathcal{H}_1: \\delta>0,\\\\ \\mathcal{H}_0: \\delta\\geq 0 &\\quad\\text{ versus }\\quad \\mathcal{H}_1: \\delta<0.\\end{eqnarray*}$$ We will also be able to test a ", em("point alternative hypothesis"), ", so the following test is also available in the Bayesian module: $$\\mathcal{H}_0: \\delta=0\\quad\\text{ versus }\\quad \\mathcal{H}_1: \\delta=\\delta_1,$$ where $\\delta_1$ is a real number.", 
    br(), br(), 
    h4("References"), 
    div(style = "color: gray;", 
        icon("file-lines"), " Belia, S., Fidler, F., Williams, J., & Cumming, G. (2005). Researchers Misunderstand Confidence Intervals and Standard Error Bars. ", em("Psychological Methods"), ", ", em("10"), "(4), 389–396. ", a("https://doi.org/10.1037/1082-989X.10.4.389", href="https://doi.org/10.1037/1082-989X.10.4.389", target="_blank"),
        br(), 
        icon("file-lines"), " Etz, A. & Wagenmakers, E.-J. (2017). J. B. S. Haldane's contribution to the Bayes factor hypothesis test. ", em("Statistical Science"), ", ", em("32"), "(2), 313–329. ", a("http://doi.org/10.1214/16-STS599", href=" http://dx.doi.org/10.1214/16-STS599", target="_blank"), 
        br(), 
        icon("file-lines"), " Falk, R., & Greenbaum, C. W. (1995). Significance Tests Die Hard: The Amazing Persistence of a Probabilistic Misconception. ", em("Theory & Psychology"), ", ", em("5"), "(1), 75–98. ", a("https://doi.org/10.1177/0959354395051004", href=" https://doi.org/10.1177/0959354395051004", target="_blank"), 
        br(), 
        icon("file-lines"), " Goodman, S. (2008). A Dirty Dozen: Twelve P-Value Misconceptions. ",  em("Seminars in Hematology"), ", ", em("45"), "(3), 135–140. ",  a("https://doi.org/10.1053/j.seminhematol.2008.04.003", href="https://doi.org/10.1053/j.seminhematol.2008.04.003", target="_blank"), 
        br(), 
        icon("file-lines"), " Greenland, S., Senn, S. J., Rothman, K. J., Carlin, J. B., Poole, C., Goodman, S. N., & Altman, D. G. (2016). Statistical tests, P values, confidence intervals, and power: A guide to misinterpretations. ", em("European Journal of Epidemiology"), ", ", em("31"), "(4), 337–350. ", a("https://doi.org/10.1007/s10654-016-0149-3", href="https://doi.org/10.1007/s10654-016-0149-3", target="_blank"), 
        br(), 
        icon("file-lines"), " Haller, H., & Kraus, S. (2002). Misinterpretations of significance: A problem students share with their teachers? ",  em("Methods of Psychological Research"), ", ", em("7"), "(1), 1–20. ", a("https://psycnet.apa.org/record/2002-14044-001", href="https://psycnet.apa.org/record/2002-14044-001", target="_blank"), 
        br(), 
        icon("file-lines"), " Hoekstra, R., Morey, R. D., Rouder, J. N., & Wagenmakers, E.-J. (2014). Robust misinterpretation of confidence intervals. ", em("Psychonomic Bulletin & Review"), ", ", em("21"), "(5), 1157–1164. ", a("https://doi.org/10.3758/s13423-013-0572-3", href="https://doi.org/10.3758/s13423-013-0572-3", target="_blank"), 
        br(), 
        icon("file-lines"), " Oakes, M. W. (1986). ", em("Statistical inference: A commentary for the social and behavioural sciences."), "John Wiley & Sons.", 
        br(), 
        icon("file-lines"), " Rouder, J. N., Speckman, P. L., Sun, D., & Morey, R. D. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. ", em("Psychonomic Bulletin & Review"), ", ", em("16"), "(2), 225–237. ", a("https://doi.org/10.3758/PBR.16.2.225", href="https://doi.org/10.3758/PBR.16.2.225", target="_blank")
    )
  )
  
  tagList(br(), 
          HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)), 
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$intro.topic1.plot1 <- renderPlot({
  x <- seq(-5, 5, by = .01)
  par(mar = c(4, .5, .5, .5), xpd = TRUE)
  plot(NULL, xlim = c(-5, 5), ylim = c(0, 1.10*dnorm(0)), ylab="", xlab="", main = "", bty = "n", xaxt = "n", yaxt = "n", yaxs = "i")
  polygon(c(x, rev(x)), c(dnorm(x, -1, 1), rep(0, length(x))), col = "#005E3C1A", border = NA)
  polygon(c(x, rev(x)), c(dnorm(x,  1, 1), rep(0, length(x))), col = "#DCA5591A", border = NA)
  points(x, dnorm(x, -1, 1), type = "l", col = "#005E3C", lwd = 1)
  points(x, dnorm(x,  1, 1), type = "l", col = "#DCA559", lwd = 1)
  segments(-1, 0, -1, 1.05*dnorm(0), col = "#005E3C", lwd = 2, lty = 2)
  segments( 1, 0,  1, 1.05*dnorm(0), col = "#DCA559", lwd = 2, lty = 2)
  arrows(-1, 1.05*dnorm(0), 1, 1.05*dnorm(0), length = .2, lwd = 2, code = 3)
  axis(1, at = c(-5, -1, 1, 5), labels = c("", expression(mu["A"]), expression(mu["B"]), ""), las = 1, cex.axis = 1.5)
  text(0, 1.05*dnorm(0), "??", pos = 3, cex = 1.5)
  text(-2, .6*dnorm(0), "Group A", pos = 2, cex = 1.5)
  text( 2, .6*dnorm(0), "Group B", pos = 4, cex = 1.5)
  
})

output$introduction2a <- renderUI({
  outtext <- paste0(
    h4("An example"), 
    "Let's start by using what we know, which is the classical independent samples $t$-test.", 
    br(), 
    "But we'll need some data!", 
    br(), br(), 
    "Go to the left-side menu, ", strong("Descriptives"), ".", 
    br(), 
    "Choose the mean, standard deviation, and sample size for each group.", 
    br(), br(), 
    "Below is the outcome from running a frequentist independent samples $t$-test.", 
    br(), 
    "You can also manipulate the test's significance level and see how that affects the test's decision."
  )
  
  tagList(HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)), 
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction2b <- renderUI({
  outtext <- paste0(
    h4("Interpretation"),
    "The green curve in the plot above shows the ", em("sampling distribution"), " of the test statistic $t$.", 
    br(), 
    "This is the distribution of all possible values of $t$ under repeated sampling, assuming that $\\mathcal{H}_0$ were indeed true.",
    br(), br(), 
    "The ", em("critical region"), " (red color) consists of all the values of $t$ that would lead to rejecting $\\mathcal{H}_0$ at $\\alpha=", round(100*(input$alpha), 3), "\\%$ significance level.", 
    br(), 
    "The probability of $t$ belonging to the critical region, under repeated sampling under $\\mathcal{H}_0$, is precisely equal to $\\alpha$.",
    br(), br(), 
    "Finally, the $p$-", em("value"), " (brown color) is the probability of observing a value of $t$ at least as extreme as the one we did observe, assuming $\\mathcal{H}_0$ is true.",
    br(), br(),
    "In significance testing, we decide to reject $\\mathcal{H}_0$ when the $p$-value is smaller than $\\alpha$ (the result is 'statistically significant') and we fail to reject $\\mathcal{H}_0$ otherwise (the result is not statistically significant).",
    br(),
    "For the data above, the test result is ",
    if (ttest.res()["p"] > input$alpha) "not ",
    "statistically significant at $", round(100*(input$alpha), 3), "\\%$ significance level ($t = ",
    round(ttest.res()["t"], 3),
    "$, df $ = ",
    if (ttest.res()["df"] - round(ttest.res()["df"]) < 1e-12) round(ttest.res()["df"], 0) else round(ttest.res()["df"], 3), "$, $p ",
    if (round(ttest.res()["p"], 3) <= .001) "< .001" else paste0("=", round(ttest.res()["p"], 3)),
    "$).",
    br(), br(),
    em("Conclusion: "),
    br(),
    "We ",
    if (round(ttest.res()["p"], 3) > input$alpha) "fail to ",
    "reject the null hypothesis which states that the population mean of group A is ", 
    switch(input$H1hyp.cls,
           "H1.diff0"    = "equal to",
           "H1.larger0"  = "at most as large as",
           "H1.smaller0" = "at least as large as"), 
    " the population mean of group B.",
    br(), br(),
    h4("Misconceptions"),
    "There are a lot of misconceptions related to NHST in general and to the $p$-value in particular.",
    br(), br(),
    "Just to mention a few examples (for an extended list see Goodman, 2008; Greenland et al., 2016), do observe that the following interpretations of the $p$-value are ", em("all incorrect:"),
    br(), br(),
    HTML(renderMarkdown(text = "1. The probability of \\$\\mathcal{H}_0\\$ being true is equal to \\$p\\$.\n 1. The probability of \\$\\mathcal{H}_1\\$ being true is equal to \\$(1-p)\\$.\n 1. A non-significant test result implies that \\$\\mathcal{H}_0\\$ is true.\n 1. A significant test result implies that \\$\\mathcal{H}_1\\$ is true.\n 1. A non-significant test result implies that the effect size is small.\n 1. A significant test result implies that the effect size is large.\n 1. The probability that a significant test result is a false positive is equal to \\$\\alpha\\$.")),
    "It is a good exercise to consider why each statement above is actually incorrect! Can you explain?", 
    br(), 
    em("(Note:"), " Goodman, 2008 and Greenland et al., 2016 discuss these topics at length.)", 
    br(), br(), 
    "The main conclusion here is that NHST and its $p$-value are rather elusive and hard to understand concepts.",
    br(),
    "This is really problematic since most science relies on hypothesis testing.",
    br(),
    "And this has motivated statisticians to search for patches and valid alternatives to the $p$-value.",
    br(), br(),
    "One such alternative is, precisely, the ", em("Bayes factor"), ".", br(), br(),
    h4("References"),
    div(style = "color: gray;", 
        icon("file-lines"), " Goodman, S. (2008). A Dirty Dozen: Twelve P-Value Misconceptions. ", em("Seminars in Hematology"), ", ", em("45"), " (3), 135–140. ", a("https://doi.org/10.1053/j.seminhematol.2008.04.003", href="https://doi.org/10.1053/j.seminhematol.2008.04.003", target="_blank"),
        br(),
        icon("file-lines"), " Greenland, S., Senn, S. J., Rothman, K. J., Carlin, J. B., Poole, C., Goodman, S. N., & Altman, D. G. (2016). Statistical tests, P values, confidence intervals, and power: A guide to misinterpretations. ", em("European Journal of Epidemiology"), ", ", em("31"), " (4), 337–350. ", a("https://doi.org/10.1007/s10654-016-0149-3", href="https://doi.org/10.1007/s10654-016-0149-3", target="_blank")
    )
  )
  
  tagList(br(),
          HTML(outtext),
          br(), br(),
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction3 <- renderUI({
  outtext <- paste0(
    "Let's denote the observed data by $D$.", 
    br(), br(), 
    "While the $p$-value is defined as 
      $$p\\text{-value} = p(D\\text{ or more extreme data}|\\mathcal{H}_0),$$ 
      the Bayes factor is defined as follows: 
      $$BF_{01} = \\frac{p(D|\\mathcal{H}_0)}{p(D|\\mathcal{H}_1)}.$$ 
      Three immediately apparent differences between the $p$-value and the Bayes factor are clear:", 
    br(), br(), 
    HTML(renderMarkdown(text = "- While the \\$p\\$-value only entertains what happens in case \\$\\mathcal{H}_0\\$ were true, the Bayes factor entertains both scenarios of either \\$\\mathcal{H}_0\\$ or \\$\\mathcal{H}_1\\$ being true.\n - While the \\$p\\$-value involves 'imaginary data' (data _more extreme than the observed data_), the Bayes factor only relies on the observed data.\n - While the \\$p\\$-value is a real number between 0 and 1, the Bayes factor can be any non-negative real number.")), 
    "The Bayes factor offers a means of comparing the ", em("predictive ability"), " of both hypotheses (better: of both statistical models represented by the hypotheses).", 
    br(), 
    "$BF_{01}$ indicates how many times are the observed data more likely under $\\mathcal{H}_0$ in comparison to $\\mathcal{H}_1$.", 
    br(), 
    "$BF_{10} = \\frac{p(D|\\mathcal{H}_1)}{p(D|\\mathcal{H}_0)}$, on the other hand, is equal to $\\frac{1}{BF_{01}}$ and it indicates how many times are the observed data more likely under $\\mathcal{H}_1$ in comparison to $\\mathcal{H}_0$.", 
    br(), br(), 
    "It is important to note that, even though the Bayes factor may indicate that the data are more likely to occur under one particular hypothesis relative to another, one should not infer that such a hypothesis is more likely. We will further stress this idea in ", actionLink("intro.tab4d", "Keep in mind", style = "font-weight: bold;"), ", section 1.", 
    br(), br(), 
    h4("Example"), 
    "Suppose we had found that $BF_{10} = 5.2$. Then we may say this:", 
    br(), 
    HTML("&nbsp;&nbsp;&nbsp;"), em("\"The observed data are 5.2 times more likely in case $\\mathcal{H}_1$ were true than if $\\mathcal{H}_0$ were true.\""), 
    br(), br(), 
    "Had we found that $BF_{10} = 0.08$ we could then conclude the following:", 
    br(), 
    HTML("&nbsp;&nbsp;&nbsp;"), em("\"The observed data are $\\frac{1}{0.08}=12.5$ times more likely in case $\\mathcal{H}_0$ were true than if $\\mathcal{H}_1$ were true.\""), 
    br(), br(), 
    "To be thorough, we must supply more information than what we show here.", 
    br(), 
    "This is related to what is actually required to compute a Bayes factor.", 
    br(), 
    "We further explain this when we discuss ", em("prior distributions"), " (", HTML("<font color=\"#DCA559\"><b>Introduction</b></font>"), ", section 5)."
  )
  
  tagList(h3(strong("The Bayes factor")), 
          br(), 
          h4("Definition 1: As a ratio of marginal likelihoods for comparing the predictive ability of two hypotheses"), 
          br(), 
          HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction4a <- renderUI({
  outtext <- paste0(
    "It can be shown (see the Box below for details) that the following formula holds:", 
    br(), 
    "$$\\underset{\\text{prior odds}}{\\underbrace{\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)}}} \\times \\textcolor{#DCA559}{\\underset{BF_{01}}{\\underbrace{\\frac{p(D|\\mathcal{H}_0)}{p(D|\\mathcal{H}_1)}}}} = \\underset{\\text{posterior odds}}{\\underbrace{\\frac{p(\\mathcal{H}_0|D)}{p(\\mathcal{H}_1|D)}}}.$$
      This formula recovers the Bayes factor formula from Definition 1.", 
    br(), 
    "But now the Bayes factor can also be interpreted as ", em("the factor updating the prior odds into the posterior odds."), 
    br(), br(), 
    h4("Prior odds"), 
    "The prior odds reflect the relative initial belief on either hypothesis.", 
    br(), 
    "For example, equal initial belief implies that the prior odds equal $\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)} = \\frac{.5}{.5} = 1$.", 
    br(), br(), 
    "Or, strong initial belief in $\\mathcal{H}_0$, say 80% for $\\mathcal{H}_0$ and 20% for $\\mathcal{H}_1$, implies that the prior odds equal $\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)} = \\frac{.8}{.2} = 4$.", 
    br(), 
    "In this case, the odds are 4-to-1 in favor of $\\mathcal{H}_0$, before considering the data.", 
    br(), br(), 
    h4("Posterior odds"), 
    "Likewise, the posterior odds reflect the relative belief on either hypothesis, but now ", em("after"), " looking at the data.", 
    br(), br(), 
    "So, if the results indicate that the posterior probability of $\\mathcal{H}_0$ is equal to 40% (and therefore the posterior probability of $\\mathcal{H}_1$ is equal to 60%), then the posterior odds equal $\\frac{p(\\mathcal{H}_0|D)}{p(\\mathcal{H}_1|D)} = \\frac{.4}{.6} = \\frac{2}{3}$.", 
    br(), 
    "In this case, the odds are 2-to-3 in favor of $\\mathcal{H}_1$, after considering the data.", 
    br(), br(), 
    h4("Example"), 
    "The Bayes factor indicates how a rational agent (i.e., one who adheres to basic axioms of probability) updates their relative belief on each hypothesis in light of the observed data.", 
    br(), br(), 
    "Suppose we had found that $BF_{01} = 4$. Then we may say this:", 
    br(), 
    HTML("&nbsp;&nbsp;&nbsp;"), em("\"In light of the observed data, one must revise his or her initial relative belief by a factor of 4-to-1 in favor of $\\mathcal{H}_0$.\""), 
    br(), br(), 
    "Observe that this interpretation holds ", em("irrespective of the prior odds"), ":", 
    br(), br(), 
    HTML(renderMarkdown(text = paste0("- Given equal prior odds (i.e., \\$\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)} = 1\\$), then the posterior odds are equal to \\$1\\times \\textcolor{#DCA559}{4}=4\\$<br> (so, \\$p(\\mathcal{H}_0|D)=.8\\$ and \\$p(\\mathcal{H_1}|D)=.2\\$).\n - If the prior odds are equal to \\$\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)} = \\frac{3}{2}\\$, then the posterior odds are equal to \\$\\frac{3}{2}\\times \\textcolor{#DCA559}{4}=6\\$<br> (so, \\$p(\\mathcal{H}_0|D)=\\frac{6}{6+1}=.86\\$ and \\$p(\\mathcal{H_1}|D)=\\frac{1}{6+1}=.14\\$).\n - ...")))
  )
  
  tagList(h3(strong("The Bayes factor")), 
          br(), 
          h4("Definition 2: As an updating factor from prior odds to posterior odds"), 
          br(), 
          HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction4b <- renderUI({
  outtext <- paste0(
    "Let us, for the time being only, focus on two specific hypotheses $\\mathcal{H}_0$ and $\\mathcal{H}_1$. This is not to say that we expect that one of them is ", em("true"), " (and worse, that we intend to use a hypotheses test to sort this out!). In fact, about ", em("always"), " we expect that both hypotheses will be false (recall George Box's adage stating that all model are wrong...). Now this may sound confusing: If the hypotheses are ", em("complementary"), " (e.g., $\\mathcal{H}_0:\\delta=0$ versus $\\mathcal{H}_1:\\delta\\not=0$), how can ", em("both"), " be wrong? The problem is that a hypothesis is typically not completely defined by simply specifying the parameter values being tested; one must further specify ", em("all"), " conditions of the test. For example, the independent samples $t$-test assumes that both populations are normally distributed. Now, how can we ever ", em("establish"), " that? The answer is: We cannot, ever. Furthermore, in Bayesian statistics we must also specify prior distributions for all parameters. In case you ask yourself 'Which prior distribution is true in the population?', then you are bound to be disappointed: There is no such thing. There are no ", em("true"), " prior distributions. Prior distributions are a means for us to express how uncertain we are about the value of a parameter. Were we omnipotent we would have no such doubts. In other words: Prior distributions, as much as assumptions like normality and equal group variances, are abstractions imposed by the analyst (= you) that really reflect how much we do not know. Such assumptions cannot really be ", em("established"), " or ", em("proven"), " beyond a shadow of a doubt. We can ", em("test"), " for such things, but we must be modest about the inferences we can draw from such tests.", 
    br(), br(), 
    "And that brings us back to $\\mathcal{H}_0$ and $\\mathcal{H}_1$. Again, we don't really think that either is true. However, we may expect one of the two to outperform the other, in a ", em("predictive"), " sense: The observed data may be more likely under one hypothesis in comparison to the other. In this sense, there is value in hypotheses testing. But never forget: This approach says nothing about the ", em("truthfulness"), " of any hypothesis. We must accept that, when performing a test, we are entertaining only two of potentially an infinity of possible hypotheses. Our inferences should be conditioned on the very two specific hypotheses we considered. We believe this is often disregarded in practice, leading researchers to mistakenly make statements which go much beyond what the given hypotheses and testing procedure allow.", 
    br(), br(), 
    "Thus, and for the time being only, let us entertain $\\mathcal{H}_0$ and $\\mathcal{H}_1$ as two very interesting hypotheses that could have generated the observed data. The term 'hypothesis' is probably better replaced by the term 'model'. What we are testing against each other is two models. Each model specifies the possible parameter values being considered, the distribution of the data conditional on those parameters (i.e., the likelihood function, like the normality in the case of the $t$-test), and all prior distributions (one per parameter).", 
    br(), br(), 
    "The Bayes factor can be derived from the ", tagList("", a("Bayes theorem", href="https://en.wikipedia.org/wiki/Bayes%27_theorem", target="_blank")), ", $$\\overset{\\text{posterior}}{\\overbrace{p(\\mathcal{H}_i|D)}} = \\frac{\\overset{\\text{prior}}{\\overbrace{p(\\mathcal{H}_i)}}\\ \\overset{\\text{marginal likelihood}}{\\overbrace{p(D|\\mathcal{H}_i)}}}{\\underset{\\text{evidence}}{\\underbrace{p(D)}}}, \\text{ for } i=0, 1.$$", 
    br(), 
    "Let's decode what we can see in Bayes theorem:", 
    br(), br(), 
    HTML(renderMarkdown(text = paste0("- \\$p(\\mathcal{H}_0)\\$ and \\$p(\\mathcal{H}_1)\\$ are the so-called _prior probabilities_ of \\$\\mathcal{H}_0\\$ and \\$\\mathcal{H}_1\\$, respectively.<br> These probabilities reflect our initial belief on either hypothesis, before we consider the observed data.<br>Because \\$\\mathcal{H}_0\\$ and \\$\\mathcal{H}_1\\$ are the only hypotheses of interest, these prior probabilities are complementary, that is, they sum to 1: \\$p(\\mathcal{H}_0)+p(\\mathcal{H}_1)=1\\$.<br><br>\n - \\$p(D|\\mathcal{H}_0)\\$ and \\$p(D|\\mathcal{H}_1)\\$ are the _marginal likelihoods_ of the data under either hypothesis.<br> These values reflect the probability of the observed data under either hypothesis.<br><br>\n - \\$p(D)\\$ is known as the _evidence_. It represents the probability of the observed data across both hypotheses.<br><br>\n - Finally, \\$p(\\mathcal{H}_0|D)\\$ and \\$p(\\mathcal{H}_1|D)\\$ are the so-called _posterior probabilities_ of \\$\\mathcal{H}_0\\$ and \\$\\mathcal{H}_1\\$, respectively.<br> These probabilities reflect our belief on either hypothesis, _after_ we consider the observed data.<br> Also these probabilities sum to 1: \\$p(\\mathcal{H}_0|D)+p(\\mathcal{H}_1|D)=1\\$."))), 
    br(), 
    "Now we derive the Bayes factor from the Bayes theorem.", 
    br(), 
    "Consider the two equations from the formula above, one when $i=0$ and other when $i=1$.", 
    br(), 
    "Dividing the equations member by member we have that 
      $$\\frac{p(\\mathcal{H}_0|D)}{p(\\mathcal{H}_1|D)} = \\frac{p(\\mathcal{H}_0)p(D|\\mathcal{H}_0) \\bigg/ p(D)}{p(\\mathcal{H}_1)p(D|\\mathcal{H}_1) \\bigg/ p(D)}.$$ Do note that $p(D)$ is the same for both hypotheses (namely, $p(D)=p(\\mathcal{H}_0)p(D|\\mathcal{H}_0) + p(\\mathcal{H}_1)p(D|\\mathcal{H}_1)$; this is due to the ", tagList("", a("law of total probability", href="https://en.wikipedia.org/wiki/Law_of_total_probability", target="_blank")), ").", 
    br(), 
    "We can therefore drop the two $p(D)$ terms in the equation above. Simplifying and rearranging the remaining terms finally leads to 
      $$\\underset{\\text{prior odds}}{\\underbrace{\\frac{p(\\mathcal{H}_0)}{p(\\mathcal{H}_1)}}} \\times \\textcolor{#DCA559}{\\underset{BF_{01}}{\\underbrace{\\frac{p(D|\\mathcal{H}_0)}{p(D|\\mathcal{H}_1)}}}} = \\underset{\\text{posterior odds}}{\\underbrace{\\frac{p(\\mathcal{H}_0|D)}{p(\\mathcal{H}_1|D)}}}.$$"
  )
  
  tagList(HTML(outtext), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction5a <- renderUI({
  outtext <- paste0(
    "The formula to compute either probability of the Bayes factor, $p(D|\\mathcal{H}_i)$ for $i=0, 1$, is not simple (see the Box below for the technical details).", 
    br(), 
    "It involves two steps:", 
    br(), br(), 
    HTML(renderMarkdown(text = "1. We need to choose a <font color=\"#DCA559\"><b>prior distribution</b></font> for each parameter (here we have two parameters: The standardized effect size \\$\\delta\\$ and the common groups standard deviation \\$\\sigma\\$).<br> A prior distribution allocates probability to each possible parameter value, irrespective of what the observed data are.<br> This may be done taking various things into account, for example: Current knowledge, differing scientific perspectives (e.g., skeptical or liberal), or known parameter constraints (e.g., avoid negative variance values \\$0\\$).<br><br>\n 2. For each hypothesis, we need to compute a weighted sum of the probability of the observed data at each combination of parameter values.<br> The weights are determined by the prior distributions.")), 
    "The quantity $p(D|\\mathcal{H}_i)$ is known as the ", em("marginal likelihood"), " of the observed data under $\\mathcal{H}_i$.", 
    br(), 
    "'Marginal' means 'across all possible parameter values', with weights given by the prior distributions.", 
    br(), br(), 
    "At this point what is important to notice is that choosing prior distributions is not only important; it is crucial.", 
    br(), 
    "Prior distributions are used directly in the computation of the Bayes factor.", 
    br(), 
    "Different prior distributions ultimately instantiate different hypotheses (even if the set of parameter values being tested remains the same!), and this will lead to different Bayes factor values.", 
    br(), br(), 
    "Furthermore, users must avoid choosing very ", em("wide"), " (or ", em("non-informative"), ", ", em("improper"), ", ", em("diffuse"), ") priors for the parameter(s) being tested.", 
    br(), 
    "The reason is that, in such cases, the Bayes factor will invariably penalize the model at hand and provide unreasonably large support for $\\mathcal{H}_0$, ", em("regardless of the observed data"), ". ", 
    br(), br(), 
    "For these reasons, it does not suffice to simply say something like \"$\\mathcal{H}_1$ is the hypothesis that $\\delta\\not=0$\".", 
    br(), 
    "We must further specify, and report, all prior distributions that we used, and priors need to be carefully chosen.",
    br(), br(), 
    h4("Default priors"), 
    "Choosing a prior distribution for each parameter is not trivial. As explained above, we may take different things into account when choosing a prior.",
    br(), br(), 
    "To make things easier, most software packages suggest seemingly sensible <font color=\"#DCA559\"><b>default priors</b></font>.", 
    br(), 
    "Such priors are chosen by taking some idealized properties into consideration (often mathematical reasons).", 
    br(), 
    "Whether such priors match the requirements of the researcher for each performed test is of course unknown. Only the researcher may be able to answer such a question.", 
    br(), br(), 
    "We strongly advice that researchers consider the priors they are using. At a bare minimum, vizualizing the priors can be extremely insightful. We will do so below.",
    br(), 
    "Let's discuss the options available for both parameters (standardized effect size $\\delta$ and the standard deviation $\\sigma$).", 
    br(), br(),
    h4(em("Prior for $\\delta$")), 
    "For a point hypothesis such as $\\mathcal{H}_0: \\delta=0$, there is really only one possible prior for $\\delta$ &#8212; the distribution assigning all probability to the point ($0$ in this case).", 
    br(), br(), 
    "Choosing a prior for $\\delta$ under one-tailed or two-tailed hypotheses is a notoriously more difficult task.", 
    br(), 
    "There is an infinity of possible distributions that we could choose from.", 
    br(), br(), 
    "This app offers three possible priors for $\\delta$, based on the Cauchy, normal, and $t$-Student distribution families.", 
    br(), 
    "Try changing the distribution and their parameters and see how that affects the probability of each value of $\\delta$.", 
    br(), 
    "When looking at the plots, consider whether the allocation of probability across the various possible values of $\\delta$ closely matches what you know about the standardized effect size that you are studying.", 
    br()
  )
  
  tagList(h3(strong("Priors")), 
          br(), 
          h4("Marginal likelihoods"), 
          br(), 
          HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction5b <- renderUI({
  outtext <- paste0(
    h4(em("Prior for $\\sigma$")), 
    "Parameter $\\sigma$ is common to both hypotheses $\\mathcal{H}_0$ and $\\mathcal{H}_1$. In such situations, it is usual to choose a common $\\sigma$ prior under both hypotheses.", 
    br(), 
    "The idea is that such priors purportedly have a very little influence on the Bayes factor (see, e.g., Rouder et al., 2009).", 
    br(), br(), 
    "In this app we use a common default prior for $\\sigma$ (for $\\sigma^2$, in fact), also used in software such as ", a("JASP", href="https://jasp-stats.org/", target="_blank") , " and R's ", a("BayesFactor", href="https://cran.r-project.org/web/packages/BayesFactor/vignettes/manual.html", target="_blank"), " package.", 
    br(), 
    "It is known as ", a("Jeffreys' prior", href="https://en.wikipedia.org/wiki/Jeffreys_prior", target="_blank"), " (Jeffreys, 1961).", 
    br(), br(),  
    "For the case at hand, the prior is given by $p(\\sigma^2)\\propto\\frac{1}{\\sigma^2}$ ($\\propto$ means ", em("is proportional to"), ").", 
    br(), 
    "Interestingly, this distribution is ", em("improper"), " (i.e., it does not integrate to 1), although that does not prevent the Bayes factor from being computed."
  )
  
  tagList(HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction5c <- renderUI({
  outtext <- paste0(
    h4("References"), 
    div(style = "color: gray;", 
        icon("file-lines"), " Jeffreys, H. (1961). ", em("Theory of probability (3rd ed.)."), "Oxford England: Oxford University Press.", 
        br(), 
        icon("file-lines"), " Rouder, J. N., Speckman, P. L., Sun, D., & Morey, R. D. (2009). Bayesian t tests for accepting and rejecting the null hypothesis. ", em("Psychonomic Bulletin & Review"), ", ", em("16"), "(2), 225–237. ", a("https://doi.org/10.3758/PBR.16.2.225", href="https://doi.org/10.3758/PBR.16.2.225", target="_blank")
    )
  )
  
  tagList(HTML(outtext), 
          br(), br(), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction5d <- renderUI({
  outtext <- paste0(
    "The <font color=\"#DCA559\"><b>marginal likelihood</b></font> for $\\mathcal{H}_i$ is given by $$p(D|\\mathcal{H}_i) = \\int_{\\Theta_i}\\underbrace{p(D|\\theta_i,\\mathcal{H}_i)}_{\\text{likelihood}}\\,\\underbrace{p(\\theta_i|\\mathcal{H}_i)}_{\\text{prior}}d\\theta_i.$$ The first term under the integral is the ", em("likelihood function"), " (for the $t$-test, the normal distribution), evaluated for the observed data $D$.", 
    br(), 
    "The second term under the integral is the joint ", em("prior distribution"), " for all parameters from the likelihood function (for the Bayesian $t$-test, $\\delta$ and $\\sigma$). ", 
    br(), 
    "The parameters of the likelihood function are jointly denoted by vector $\\theta_i$. Thus, for this $t$-test in particular, $\\theta_i=(\\delta, \\sigma)$.", 
    br(), 
    "And finally, all possible values for the parameters (here, all possible parameter combinations $(\\delta, \\sigma)$) are denoted by $\\Theta_i$.",
    br(), br(), 
    "In simple terms, $p(D|\\mathcal{H}_i)$ is related to the probability of the observed data, taking into account all the uncertainty in the parameter values as dictated by the chosen prior distributions.", 
    br(), br(), 
    "$p(D|\\mathcal{H}_i)$ is actually a <font color=\"#DCA559\"><b>weighted average</b></font> of $p(D|\\theta_i,\\mathcal{H}_i)$, which is related to the probability of the observed data at each combination of parameter values, $\\theta_i$.", 
    br(), 
    "The weights are determined by $p(\\theta_i|\\mathcal{H}_i)$, the prior distribution for the parameters.", 
    br(), br(), 
    "Incidentally, the equation above helps understanding why we should not use improper or even extremely wide priors for the parameter(s) being tested, when computing marginal likelihoods.", 
    br(), 
    "Too wide priors will lower the weights that the prior provides for reasonable values of the parameter, while increasing the weights for unreasonable (very small or very large) parameter values.", 
    br(), 
    "The observed data will not accord with such a prior allocation of relative credibility of the parameter values, and this will lead towards artificially lowering the marginal likelihood.", 
    br(), 
    "It is therefore very important to use priors that allocate most of its credibility to reasonable parameter values."
  )
  
  tagList(HTML(outtext), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

output$introduction6 <- renderUI({
  outtext <- paste0(
    "NHBT and its Bayes factor is not just a simple replacement to NHST and its $p$-value. The Bayes factor and the $p$-value are different statistical quantities, with different properties and allowing for different types of conclusions.", 
    br(), 
    "For users of the $p$-value who are now trying to transition towards the Bayes factor, this cannot be stressed enough: ", em("The Bayes factor must be learned and appreciated by its own merits and pitfalls."), 
    br(), br(), 
    "Here we highlight one clear difference between the $p$-value and the Bayes factor:", 
    br(), 
    HTML("&nbsp;&nbsp;&nbsp;"), em("Bayes factors allow providing relative support in favor of $\\mathcal{H}_0$, whereas $p$-values do not."), 
    br(), br(), 
    "Let's assume that $\\mathcal{H}_0$ is ", em("true"), ".", 
    br(), br(), 
    "It is ", tagList("", a("well known", href="https://statproofbook.github.io/P/pval-h0.html", target="_blank")), " that, in this case, the $p$-value is uniformly distributed in the $(0,1)$ interval, across repeated sampling.", 
    br(), 
    "In other words, the $p$-value is equally likely to assume any value between 0 and 1. At significance level $\\alpha$, it is therefore expected that the test will turn out non-significant (i.e., $p>\\alpha$) with probability $(1-\\alpha)$, irrespective of the sample size.", 
    br(), 
    "And in case the observed means are ", em("exactly"), " equal to each other, then the $p$-value will be exactly equal to 1, thus the test outcome will always be non-significant.", 
    br(), 
    "Unfortunately, we can not derive any conclusion from a non-significant test result. Non-significant results may happen when either $\\mathcal{H}_0$ or $\\mathcal{H}_1$ is true, and there is no way to disentangle between the two possibilities.",
    br(), br(), 
    "The Bayes factor is different from the $p$-value in this regard. ", em("For a given alternative hypothesis $\\mathcal{H}_1$"), ", large values of $BF_{01}$ do provide relative evidence favoring $\\mathcal{H}_0$ over ", em("the particular alternative hypothesis $\\mathcal{H}_1$"), " that was used in the testing procedure.", 
    br(), br(), 
    "The app below gives an idea about how the Bayes factor varies, in an idealized setting.", 
    br(), 
    "The two group means are fixed and ", em("exactly"), " equal to each other at each sample size value (rather arbitrarily we used a common mean of 0 for both groups). Both group standard deviations are equal to 1, and we use a standard normal prior distribution of the effect size $\\delta$.", 
    br(), 
    "You can see how the $p$-value and the Bayes factor behave as the sample size increases."
  )
  
  tagList(h3(strong("Supporting $\\mathcal{H}_0$")), 
          br(), 
          HTML(outtext), 
          br(), 
          tags$script(HTML(js)),
          tags$script(
            async="",
            src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
          )
  )
})

# Compute things to render the plot in intro.topic 6:
{# Common sample size:
  N.supp.it6  <- seq(50, 5000, by = 50)
  t.vals.it6  <- sapply(N.supp.it6, function(n) t.test.summ(0, 0, 1, 1, n, n)["t"])
  p.vals.it6  <- sapply(N.supp.it6, function(n) t.test.summ(0, 0, 1, 1, n, n)["p"])
  BF.vals.it6 <- sapply(1:length(N.supp.it6), function(i) B01(t.vals.it6[i], N.supp.it6[i], N.supp.it6[i], normal.prior, "H1.diff0", 0, scale = 1))
}

output$intro.topic6.plot1 <- renderPlot({
  par(mar = c(4, 5.5, .5, 1))
  plot(N.supp[1:(input$Ncommon.BF.p/50)], BF.vals.it6[1:(input$Ncommon.BF.p/50)], type = "l", las = 1, bty = "n", yaxs = "i", xaxs = "i",
       xlim = c(50, 5000), ylim = c(0, 50),
       col = "#005E3C", lwd = 2,
       xlab = "", ylab = "", yaxt = "n", xaxt = "n")
  axis(1, at = seq(0, 5000, by = 1000), las = 1)
  axis(2, at = seq(0, 50, 10), las = 1)
  mtext("Sample size per group", 1, 2.5)
  mtext(expression("BF"["01"]), 2, 3)
})

output$intro.topic6.plot2 <- renderPlot({
  par(mar = c(4, 5.5, .5, 1))
  plot(N.supp[1:(input$Ncommon.BF.p/50)], p.vals.it6[1:(input$Ncommon.BF.p/50)], type = "l", las = 1, bty = "n", yaxs = "i", xaxs = "i",
       xlim = c(50, 5000), ylim = c(0, 1.01), col = "#005E3C", lwd = 2,
       xlab = "", ylab = "", yaxt = "n", xaxt = "n")
  axis(1, at = seq(0, 5000, by = 1000), las = 1)
  axis(2, at = seq(0, 1, by = .2), las = 1)
  mtext("Sample size per group", 1, 2.5)
  mtext(expression(italic("p") * "-value"), 2, 3)
})

output$intro.topic5.plot1 <- renderPlot({
  layout(matrix(c(1, 2), 1, 2, byrow = TRUE))
  
  # Left plot:
  if (input$H1hypbys %in% c("H1.diff0", "H1.point"))
  {
    par(mar = c(4, 4, 1.5, .5))
    plot(NULL, xlim = c(-2, 2), ylim = c(0, 1.05), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n", 
         cex.axis = 1.5, main = expression("Prior for " * delta * "  under " * H[0]), cex.main = 1.2, font.main = 1)
    axis(1, c(-2, 0, 2), c("", "0", ""))
    axis(2, c(0, 1), las = 1)
    segments(0, 0, 0, 1, lty = 2, col = "gray")
    points(0, 1, pch = 16, cex = 2, col = "#005E3C")
    segments(-2, 0, 2, 0, lty = 1, col = "#005E3C", lwd = 2)
    points(0, 0, pch = 21, cex = 2, bg = "white", col = "#005E3C")
    mtext(expression(delta), 1, 2.5, cex = 1.2)
    mtext("Probability", 2, 2, cex = 1.2)
  } else if (input$H1hypbys == "H1.larger0")
  {
    x.abs  <- max(abs(c(floor(location.bys() - 3.5*scale.bys()), ceiling(location.bys() + 3.5*scale.bys()))))
    x.supp <- seq(-x.abs, 0, length.out = 1024)
    y.area <- integrate(function(delta) prior.bys(delta), lower = -Inf, upper = 0)[[1]]
    y      <- prior.bys(x.supp) / y.area
    y.max  <- max(prior.bys(c(-x.supp, x.supp)) / y.area )
    
    par(mar = c(4, 4.5, 1.5, .5))
    plot(x.supp, y, xlim = c(-x.abs, 0), ylim = c(0, 1.2*y.max), ylab = "", xlab = "", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", 
         cex.main = 1.2, font.main = 1, 
         main = switch(input$priorbys,
                       "cauchy"    = expression("Prior for " * delta * "  under " * H[0] * " (Cauchy-)"),
                       "normal"    = expression("Prior for " * delta * "  under " * H[0] * " (Normal-)"),
                       "t.student" = expression("Prior for " * delta * "  under " * H[0] * " (" * italic(t) * "-Student-)")))
    axis(1, at = min(x.supp):0)
    mtext(expression(delta), 1, 2.5, cex = 1.2)
    mtext("Density", 2, 3, cex = 1.2)
    polygon(c(x.supp, rev(x.supp)), c(y, rep(0, 1024)), col = "#DCA55966", border = NA)
  } else 
  {
    x.abs  <- max(abs(c(floor(location.bys() - 3.5*scale.bys()), ceiling(location.bys() + 3.5*scale.bys()))))
    x.supp <- seq(0, x.abs, length.out = 1024)
    y.area <- integrate(function(delta) prior.bys(delta), lower = 0, upper = Inf)[[1]]
    y      <- prior.bys(x.supp) / y.area
    y.max  <- max(prior.bys(c(-x.supp, x.supp)) / y.area )
    
    par(mar = c(4, 4.5, 1.5, .5))
    plot(x.supp, y, xlim = c(0, x.abs), ylim = c(0, 1.2*y.max), ylab = "", xlab = "", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", 
         cex.main = 1.2, font.main = 1, 
         main = switch(input$priorbys,
                       "cauchy"    = expression("Prior for " * delta * "  under " * H[0] * " (Cauchy+)"),
                       "normal"    = expression("Prior for " * delta * "  under " * H[0] * " (Normal+)"),
                       "t.student" = expression("Prior for " * delta * "  under " * H[0] * " (" * italic(t) * "-Student+)")))
    axis(1, at = 0:max(x.supp))
    mtext(expression(delta), 1, 2.5, cex = 1.2)
    mtext("Density", 2, 3, cex = 1.2)
    polygon(c(x.supp, rev(x.supp)), c(y, rep(0, 1024)), col = "#DCA55966", border = NA)
  }
  
  # Right plot:
  if (input$H1hypbys == "H1.point")
  {
    par(mar = c(4, .5, 1.5, 4.5))
    plot(NULL, xlim = c(-2, 2), ylim = c(0, 1.05), ylab = "", xlab = "", xaxt = "n", yaxt = "n", bty = "n", 
         cex.axis = 2, main = expression("Prior for " * delta * "  under " * H[1]), cex.main = 1.2, font.main = 1)
    axis(1, c(-2, input$H1pointslide.bys, 2), c("", input$H1pointslide.bys, ""))
    axis(4, c(0, 1), las = 1)
    segments(input$H1pointslide.bys, 0, input$H1pointslide.bys, 1, lty = 2, col = "gray")
    points(input$H1pointslide.bys, 1, pch = 16, cex = 2, col = "#005E3C")
    segments(-2, 0, 2, 0, lty = 1, col = "#005E3C", lwd = 2)
    points(input$H1pointslide.bys, 0, pch = 21, cex = 2, bg = "white", col = "#005E3C")
    mtext(expression(delta), 1, 2.5, cex = 1.2)
    mtext("Probability", 4, 2, cex = 1.2)
  } else if (input$H1hypbys == "H1.diff0")
  {
    
    x.supp <- switch(input$priorbys,
                     "cauchy"    = seq(floor(location.bys() - 3.5*scale.bys()), ceiling(location.bys() + 3.5*scale.bys()), length.out = 1024),
                     "normal"    = seq(floor(location.bys() - 3.5*scale.bys()), ceiling(location.bys() + 3.5*scale.bys()), length.out = 1024),
                     "t.student" = seq(floor(location.bys() - 3.5*scale.bys()), ceiling(location.bys() + 3.5*scale.bys()), length.out = 1024))
    y      <- switch(input$priorbys,
                     "cauchy"    = cauchy.prior(x.supp, location.bys(), scale.bys()),
                     "normal"    = normal.prior(x.supp, location.bys(), scale.bys()),
                     "t.student" = tstude.prior(x.supp, location.bys(), scale.bys(), df.bys()))
    
    par(mar = c(4, .5, 1.5, 4.5))
    plot(x.supp, y, xlim = c(min(x.supp), max(x.supp)), ylim = c(0, 1.2*max(y)), ylab = "", xlab = "", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", yaxt = "n", 
         cex.main = 1.2, font.main = 1, 
         main = switch(input$priorbys,
                       "cauchy"    = expression("Prior for " * delta * "  under " * H[1] * " (Cauchy)"),
                       "normal"    = expression("Prior for " * delta * "  under " * H[1] * " (Normal)"),
                       "t.student" = expression("Prior for " * delta * "  under " * H[1] * " (" * italic(t) * "-Student)")))
    axis(1, at = min(x.supp):max(x.supp))
    axis(4, las = 1)
    mtext(expression(delta), 1, 2.5, cex = 1.2)
    mtext("Density", 4, 3, cex = 1.2)
    polygon(c(x.supp, rev(x.supp)), c(y, rep(0, 1024)), col = "#DCA55966", border = NA)
  } else if (input$H1hypbys == "H1.smaller0")
  {
    x.abs  <- max(abs(c(floor(location.bys() - 3.5*scale.bys()), ceiling(location.bys() + 3.5*scale.bys()))))
    x.supp <- seq(-x.abs, 0, length.out = 1024)
    y.area <- integrate(function(delta) prior.bys(delta), lower = -Inf, upper = 0)[[1]]
    y      <- prior.bys(x.supp) / y.area
    y.max  <- max(prior.bys(c(-x.supp, x.supp)) / y.area )
    
    par(mar = c(4, .5, 1.5, 4.5))
    plot(x.supp, y, xlim = c(-x.abs, 0), ylim = c(0, 1.2*y.max), ylab = "", xlab = "", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", yaxt = "n", 
         cex.main = 1.2, font.main = 1, 
         main = switch(input$priorbys,
                       "cauchy"    = expression("Prior for " * delta * "  under " * H[1] * " (Cauchy-)"),
                       "normal"    = expression("Prior for " * delta * "  under " * H[1] * " (Normal-)"),
                       "t.student" = expression("Prior for " * delta * "  under " * H[1] * " (" * italic(t) * "-Student-)")))
    axis(1, at = min(x.supp):0)
    axis(4, las = 1)
    mtext(expression(delta), 1, 2.5, cex = 1.2)
    mtext("Density", 4, 3, cex = 1.2)
    polygon(c(x.supp, rev(x.supp)), c(y, rep(0, 1024)), col = "#DCA55966", border = NA)
  } else 
  {
    x.abs  <- max(abs(c(floor(location.bys() - 3.5*scale.bys()), ceiling(location.bys() + 3.5*scale.bys()))))
    x.supp <- seq(0, x.abs, length.out = 1024)
    y.area <- integrate(function(delta) prior.bys(delta), lower = 0, upper = Inf)[[1]]
    y      <- prior.bys(x.supp) / y.area
    y.max  <- max(prior.bys(c(-x.supp, x.supp)) / y.area )
    
    par(mar = c(4, .5, 1.5, 4.5))
    plot(x.supp, y, xlim = c(0, x.abs), ylim = c(0, 1.2*y.max), ylab = "", xlab = "", bty = "n",
         las = 1, type = "l", col = "#005E3C", lwd = 2, xaxt = "n", yaxt = "n", 
         cex.main = 1.2, font.main = 1, 
         main = switch(input$priorbys,
                       "cauchy"    = expression("Prior for " * delta * "  under " * H[1] * " (Cauchy+)"),
                       "normal"    = expression("Prior for " * delta * "  under " * H[1] * " (Normal+)"),
                       "t.student" = expression("Prior for " * delta * "  under " * H[1] * " (" * italic(t) * "-Student+)")))
    axis(1, at = 0:max(x.supp))
    axis(4, las = 1)
    mtext(expression(delta), 1, 2.5, cex = 1.2)
    mtext("Density", 4, 3, cex = 1.2)
    polygon(c(x.supp, rev(x.supp)), c(y, rep(0, 1024)), col = "#DCA55966", border = NA)
  }
  
})

output$intro.topic5.plot2 <- renderPlot({
  x.supp <- seq(.05, 1.2, by = .001)
  y      <- 1 / (x.supp^2)
  
  par(mar = c(4, 4.5, 1.5, .5))
  plot(x.supp, y, xlim = c(0, 1.2), ylab = "", xlab = "", bty = "n",
       las = 1, type = "l", col = "#005E3C", lwd = 2, yaxt = "n", 
       cex.main = 1.5, font.main = 1, 
       main = expression("Prior for " * sigma^"2"))
  mtext(expression(sigma^"2"), 1, 3, cex = 1.5)
  polygon(c(x.supp, rev(x.supp)), c(y, rep(0, length(x.supp))), col = "#DCA55966", border = NA)
})





























# INSTRUCTIONS tab ----

output$instructions <- renderUI({
  outtext <- paste0(
    "Welcome to <font color=\"#DCA559\"><b>The Bayes factor</b></font> app!", 
    br(), br(),
    h4(tags$i(class = "fa-solid fa-clipboard-question"), "What", em("is"), "the Bayes factor?"), 
    "Think of the Bayes factor as the Bayesian counterpart to the $p$-value.", 
    br(), 
    "The $p$-value and the Bayes factor are not equivalent concepts, however, so it is important to carefully distinguish between them.", 
    br(), br(),
    h4(tags$i(class = "fa-solid fa-bullseye"), "Goal"), 
    "This app offers a practical tutorial of the Bayes factor in the context of ", em("null hypothesis Bayesian testing"), " (NHBT in short).", 
    br(), 
    "We will use the ", em("independent samples $t$-test"), " comparing the means of two groups (say, A and B) as the exemplary testing setting.",
    br(), br(), 
    "Our main focus is on ", em("understanding"), " and ", em("correctly"), " interpreting the Bayes factor.", 
    br(), 
    "We do provide some formulas in the ", actionLink("intro.tab2a", "Introduction", style = "font-weight: bold;"), ", but we keep things as conceptual as possible.",
    br(), br(),
    h4(tags$i(class = "fa-solid fa-arrows-down-to-people"), "Intended audience"), 
    "The app is particularly aimed at those who are new to Bayesian hypothesis testing and to the Bayes factor and need to ", em("apply"), " it in their research.", 
    br(), 
    "This may include academics at any stage of their careers.", 
    br(), 
    "Also journal editors and reviewers may benefit from the insights that we offer, in order to improve the quality of the published literature with respect to Bayesian hypotheses testing.", 
    br(), br(), 
    "For those also interested in the more technical details, supplementary materials are also provided (separately, in boxes).", 
    br(),
    "This material can be entirely skipped by those only interested in the application part of the Bayes factor.", 
    br(), br(), 
    h4(tags$i(class = "fa-solid fa-circle-info"), "How to use this app"), 
    "This app may be used for self-learning, or as a support medium in a workshop.", 
    br(), br(), 
    "There are a lot of important concepts and nuances to learn and discuss.", 
    br(), 
    "We therefore suggest that it may be best to allocate a minimum of 2 hours to work through all the material in detail.", 
    br(), br(), 
    "We suggest the following approach:", 
    br(), br(), 
    HTML(renderMarkdown(text = paste0("- Proceed to the ", actionLink("intro.tab2b", "Introduction", style = "font-weight: bold;"), " tab.<br> We start by running the classic independent samples \\$t\\$-test, which is \\$p\\$-value based.<br> We do so in order to start from familiar ground, and also to start highlighting the similarities and differences between NHST and the \\$p\\$-value and their Bayesian counterparts (NHBT and the Bayes factor).<br><br> We then introduce the Bayes factor. The Bayes factor may be thought of in two complementary ways. We discuss each in turn.<br> We end the introduction by underlining some clear differences between the Bayes factor and the \\$p\\$-value.<br> This is important, because the Bayes factor is not simply the 'Bayesian \\$p\\$-value'.<br><br>\n- Proceed to the ", actionLink("intro.tab3a", "Bayesian \\$t\\$-test", style = "font-weight: bold;"), " tab.<br> Feel free to manipulate the data (left-hand side menu, **Descriptives**) and the various test options (left-hand side menu, **Bayesian test**), and see how that affects the various results displayed.<br> We also show how the Bayes factor can be interpreted.<br><br>\n- Proceed to the ", actionLink("intro.tab4a", "Keep in mind", style = "font-weight: bold;"), " tab.<br> Here we highlight important ideas, especially pitfalls that one should avoid (Tendeiro et al., 2019, 2023; Wong et al., 2022).<br> We also provide some good examples that may be used as a template.<br><br>\n- Proceed to the ", actionLink("intro.tab5a", "Let's practice!", style = "font-weight: bold;"), " tab.<br> Test your knowledge by answering to a few questions.<br> Feedback is provided to each question, including links to the app's sections where the point at hand was discussed, or extra suggested literature.") )), 
    br(), 
    h4(tags$i(class = "fa-solid fa-people-group"), "About us"), 
    "This app was developed and is being maintained by:", 
    br(), br(), 
    HTML(renderMarkdown(text = paste0(tagList("", a("Jorge N. Tendeiro", href="https://www.jorgetendeiro.com/", target="_blank")), "<br>Hiroshima University, Professor<br>Author, developer, maintainer.") )), 
    HTML(renderMarkdown(text = paste0(tagList("", a("Henk A. L. Kiers", href="https://www.rug.nl/staff/h.a.l.kiers/?lang=en", target="_blank")), "<br>University of Groningen, Professor<br>Collaborator, developer.") )), 
    HTML(renderMarkdown(text = paste0(tagList("", a("Rink Hoekstra", href="https://www.rug.nl/staff/r.hoekstra/?lang=nl", target="_blank")), "<br>University of Groningen, Assistant Professor<br>Collaborator, developer.") )), 
    HTML(renderMarkdown(text = paste0(tagList("", a("Tsz Keung Wong", href="https://www.linkedin.com/in/tsz-keung-wong-a93738161/", target="_blank")), "<br>Tilburg University, Ph.D candidate<br>Collaborator, developer.") )), 
    br(), 
    h4(tags$i(class = "fa-solid fa-code"), "App"), 
    "Version: 0.2.0.<br>License: GPL-3.<br>Source code available at ", a("GitHub", href="https://github.com/jorgetendeiro/StatsEdgeShinyApps", target="_blank"), ".", 
    br(), br(), 
    h4("References"), 
    div(style = "color: gray;", 
        icon("file-lines"), " Tendeiro, J. N., & Kiers, H. A. L. (2019).  A review of issues about null hypothesis Bayesian testing. ", em("Psychological Methods"), ", ", em("24"), "(6), 774–795. ", a("http://dx.doi.org/10.1037/met0000221", href="http://dx.doi.org/10.1037/met0000221", target="_blank"), 
        br(), 
        icon("file-lines"), " Tendeiro, J. N., Kiers, H. A. L., Hoekstra, R., Wong, T. K., & Morey, R. D. (2023). Diagnosing the use of the Bayes factor in applied research. ", em("In press"), a("https://psyarxiv.com/du3fc/", href="https://psyarxiv.com/du3fc/", target="_blank"), 
        br(), 
        icon("file-lines"), " Wong, T. K., Kiers, H. A. L., & Tendeiro, J. N. (2022). On the potential mismatch between the function of the Bayes factor and researchers' expectations. ", em("Collabra: Psychology"), ", ", em("8"), " (1). ", a("https://doi.org/10.1525/collabra.36357", href="https://doi.org/10.1525/collabra.36357", target="_blank")
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

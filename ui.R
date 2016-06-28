library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Power Analysis with Random Targets and Participants"),
  p("Judd, C. M., Westfall, J., & Kenny, D. A. (2016). Experiments with more than one random factor: Designs, analytic models, and statistical power.",em("Annual Review of Psychology.")),
  fluidRow(
    column(1, div(a("Article", href="http://jakewestfall.org/publications/JWK_AnnRev.pdf"))),
    column(3, div(a("Supplemental Appendix: Additional topics", href="http://jakewestfall.org/publications/JWK_AnnRev_Appendix.pdf"))),
    column(3, div(a("Code for this app (using package 'shiny' in R)", href="https://github.com/jake-westfall/two_factor_power"))),
    column(2, div(a("Back to JakeWestfall.org", href="http://jakewestfall.org")))
  ),
  helpText("Note: when sharing the link to this app, please use the stable redirecting page at",
           a("jakewestfall.org/two_factor_power/,", href="http://jakewestfall.org/two_factor_power/"),
           "as the app's current URL is possibly subject to change."),

  sidebarPanel(
    selectInput("design", "Choose a design:", 
                choices = c("CCC", "CNC", "NCC", "NNC",
                            "CCNp", "CNNp", "NNNp",
                            "CCNt", "NCNt", "NNNt",
                            "R(CCC)", "R(CNC)", "R(NCC)", "R(NNC)",
                            "Counterbalanced")),
    selectInput("type", "Standardized or Unstandardized input:", 
                choices = c("Standardized", "Unstandardized")),
    conditionalPanel(condition = "input.type == 'Standardized'",
                     helpText("Note: with Standardized input, all of the Variance Partitioning Coefficients (VPCs) must sum to 1.")),
    
    p("-------------------------------------------"),
    
    p("Enter the design parameters below."),
    p("To compute power estimates, enter an X for the variable you wish to solve for, then click the 'Solve for X' button."),
    actionButton("solve", "Solve for X"),
    p(),
    
    htmlOutput("get_d"),
    conditionalPanel(condition = "input.type == 'Unstandardized'",
                     textInput("code", "Contrast codes: (plus or minus)", 1)),
    htmlOutput("get_E"),
    htmlOutput("get_P"),
    htmlOutput("get_S"),
    htmlOutput("get_PS"),
    htmlOutput("get_PC"),
    htmlOutput("get_SC"),
    conditionalPanel(condition = "input.design == 'CCNt' | input.design == 'NCNt' | input.design == 'NNNt'",
                     helpText("Note: for this design, the number of Participants should be a multiple of the number of Targets.")),
    textInput("p", value=20, label="Total number of Participants:"),
    conditionalPanel(condition = "input.design == 'CCNp' | input.design == 'CNNp' | input.design == 'NNNp'",
                     helpText("Note: for this design, the number of Targets should be a multiple of the number of Participants.")),
    textInput("q", value=20, label="Total number of Targets:"),
    conditionalPanel(condition = "input.design == 'R(CCC)' | input.design == 'R(CNC)' | input.design == 'R(NCC)' | input.design == 'R(NNC)'",
      textInput("b", value=5, label="Total number of Replications:")),
    textInput("power", "Power:", "X")
  ), # end sideBarPanel 
  
  mainPanel(
    h4("Design schematic"),
    helpText("(The interpretation of this design schematic is explained in the accompanying paper.)"),
    tableOutput("designTab"),
    h4("Solution from power analysis"),
    verbatimTextOutput("ans"),
    h4("Additional power analysis information"),
    verbatimTextOutput("info"),
    h4("Technical output (for troubleshooting)"),
    verbatimTextOutput("debug"),
    h4("R/SAS/SPSS code for estimating the mixed model"),
    helpText("(Note: 'condition' is assumed to be entered as a numeric variable, manually contrast coded, and NOT as a factor (in R), class (in SAS), or string (in SPSS) variable. See paper for details.)"),
    p("R code:"),
    helpText("(Note: the 'lme4' and 'lmerTest' packages must be installed and loaded.)"),
    verbatimTextOutput("R_code"),
    p("SAS code:"),
    verbatimTextOutput("SAS_code"),
    p("SPSS code:"),
    verbatimTextOutput("SPSS_code")
  ) # end main Panel
  
) # end fluidePage
) # end shinyUI
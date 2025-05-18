# TOP 2025 Transparency Score Calculator
# This Shiny app evaluates research transparency based on TOP 2025 guidelines
# Developed by Ahmad Sofi-Mahmudi (a.sofimahmudi@gmail.com)

# Load required libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "TOP 2025 Transparency Evaluator"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Study Metadata", tabName = "metadata", icon = icon("file-alt")),
      menuItem("About", tabName = "about", icon = icon("info-circle")),
      menuItem("Research Practices", tabName = "research", icon = icon("flask")),
      menuItem("Verification Practices", tabName = "verification", icon = icon("check-circle")),
      menuItem("Verification Studies", tabName = "studies", icon = icon("search")),
      menuItem("Results", tabName = "results", icon = icon("chart-bar"))
    )
  ),

  dashboardBody(
    tabItems(
      # Study Metadata tab
      tabItem(tabName = "metadata",
              fluidRow(
                box(
                  width = 12,
                  title = "Study Information",
                  status = "primary",
                  solidHeader = TRUE,
                  p("Enter basic information about your study or journal:")
                )
              ),
              fluidRow(
                column(width = 6,
                       box(width = NULL,
                           title = "Basic Information",
                           status = "info",
                           solidHeader = TRUE,
                           textInput("study_title", "Study Title:", placeholder = "Enter the full title of your study"),
                           textAreaInput("study_authors", "Authors:", placeholder = "Enter author names (separated by commas)"),
                           selectInput("discipline", "Research Discipline:",
                                       choices = list("Select discipline" = "",
                                                      "Biomedical" = "biomedical",
                                                      "Psychology" = "psychology",
                                                      "Social Sciences" = "social_sciences",
                                                      "Economics" = "economics",
                                                      "Computer Science" = "computer_science",
                                                      "Environmental Sciences" = "environmental",
                                                      "Engineering" = "engineering",
                                                      "Other" = "other")),
                           conditionalPanel(
                             condition = "input.discipline == 'other'",
                             textInput("other_discipline", "Please specify:", placeholder = "Enter your discipline")
                           ),
                           dateInput("study_date", "Study Completion Date:", value = Sys.Date())
                       )
                ),
                column(width = 6,
                       box(width = NULL,
                           title = "Publication Information",
                           status = "info",
                           solidHeader = TRUE,
                           textInput("journal_name", "Journal/Publication Venue:", placeholder = "Enter journal or publication venue"),
                           textInput("publication_doi", "Publication DOI:", placeholder = "e.g., 10.1000/xyz123"),
                           selectInput("publication_status", "Publication Status:",
                                       choices = list("Select status" = "",
                                                      "Published" = "published",
                                                      "In Press" = "in_press",
                                                      "Under Review" = "under_review",
                                                      "In Preparation" = "in_preparation",
                                                      "Preprint" = "preprint")),
                           conditionalPanel(
                             condition = "input.publication_status == 'published' || input.publication_status == 'in_press' || input.publication_status == 'preprint'",
                             dateInput("publication_date", "Publication Date:", value = Sys.Date())
                           ),
                           actionButton("save_metadata", "Save Study Information", icon = icon("save"),
                                        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                       )
                )
              )
      ),

      # About tab
      tabItem(tabName = "about",
              fluidRow(
                box(
                  width = 12,
                  title = "TOP 2025 Guidelines",
                  status = "primary",
                  solidHeader = TRUE,
                  p("This app helps researchers and journals evaluate transparency based on the updated Transparency and Openness Promotion (TOP) Guidelines."),
                  p("The TOP Guidelines provide a framework for journals to design and implement publication standards aligned with transparency and openness values.
                    The 2025 update reorganizes standards into three categories with varying levels of implementation."),
                  p("Use this tool to assess your research, review journal policies, or plan improvements in transparency practices.")
                )
              ),
              fluidRow(
                box(
                  width = 8,
                  title = "Conceptual Framework",
                  status = "info",
                  solidHeader = TRUE,
                  p("The primary objective of the TOP Guidelines is to promote the verifiability of empirical research claims through:"),
                  tags$ul(
                    tags$li(strong("Research Practices:"), "Discrete practices that make empirical research more transparent and open"),
                    tags$li(strong("Verification Practices:"), "Ways to verify that reported results are accurate descriptions of what studies found"),
                    tags$li(strong("Verification Studies:"), "Specific empirical research designs and publication formats that focus on verification of results")
                  ),
                  plotOutput("conceptualFramework")
                ),
                box(
                  width = 4,
                  title = "App Developer",
                  status = "success",
                  solidHeader = TRUE,
                  div(
                    style = "text-align: center; padding: 10px;",
                    h4("Ahmad Sofi-Mahmudi"),
                    p(icon("envelope"), " ", a("a.sofimahmudi@gmail.com", href = "mailto:a.sofimahmudi@gmail.com")),
                    p(icon("twitter"), " ", a("@ASofiMahmudi", href = "https://twitter.com/ASofiMahmudi", target = "_blank")),
                    p(icon("linkedin"), " ", a("@asofimahmudi", href = "https://linkedin.com/in/asofimahmudi", target = "_blank")),
                    hr(),
                    p("This app was created to help researchers evaluate and improve their compliance with the TOP 2025 guidelines."),
                    p("If you have suggestions for improving this tool, please contact me using the information above.")
                  )
                )
              )
      ),

      # Research Practices tab
      tabItem(tabName = "research",
              fluidRow(
                box(
                  width = 12,
                  title = "Research Practices",
                  status = "primary",
                  solidHeader = TRUE,
                  p("Select the level of implementation for each research practice standard:")
                )
              ),
              fluidRow(
                column(width = 6,
                       box(width = NULL,
                           title = "Study Registration",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("study_reg", "Implementation Level:",
                                        choices = list("Not Implemented" = 0,
                                                       "Level 1: Disclosed" = 1,
                                                       "Level 2: Shared and Cited" = 2,
                                                       "Level 3: Certified" = 3),
                                        selected = 0),
                           conditionalPanel(
                             condition = "input.study_reg > 0",
                             selectInput("registry_type", "Registration Platform:",
                                         choices = list("Select platform" = "",
                                                        "OSF Registries" = "osf",
                                                        "ClinicalTrials.gov" = "clinicaltrials",
                                                        "PROSPERO" = "prospero",
                                                        "AEA Registry" = "aea",
                                                        "ISRCTN" = "isrctn",
                                                        "ResearchRegistry" = "researchregistry",
                                                        "Other" = "other")),
                             conditionalPanel(
                               condition = "input.registry_type == 'other'",
                               textInput("other_registry", "Specify Other Registry:", placeholder = "Enter registry name")
                             ),
                             textInput("registration_url", "Registration URL:", placeholder = "Enter full URL of registration"),
                             textInput("registration_id", "Registration ID/DOI:", placeholder = "Enter registration ID or DOI if available"),
                             dateInput("registration_date", "Registration Date:", value = NULL),
                             checkboxInput("registration_prior", "Registration was completed prior to data collection/access", value = FALSE)
                           ),
                           p("Study registration involves entering a time-stamped, publicly-available record about a study in a register that assigns a unique and permanent study identifier.")
                       ),
                       box(width = NULL,
                           title = "Study Protocol",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("protocol", "Implementation Level:",
                                        choices = list("Not Implemented" = 0,
                                                       "Level 1: Disclosed" = 1,
                                                       "Level 2: Shared and Cited" = 2,
                                                       "Level 3: Certified" = 3),
                                        selected = 0),
                           conditionalPanel(
                             condition = "input.protocol > 0",
                             selectInput("protocol_location", "Protocol Location:",
                                         choices = list("Select location" = "",
                                                        "OSF" = "osf",
                                                        "GitHub" = "github",
                                                        "Zenodo" = "zenodo",
                                                        "Journal Supplementary Materials" = "journal",
                                                        "Institutional Repository" = "institutional",
                                                        "Published Protocol Paper" = "protocol_paper",
                                                        "Other" = "other")),
                             conditionalPanel(
                               condition = "input.protocol_location == 'other'",
                               textInput("other_protocol_location", "Specify Other Location:", placeholder = "Enter location name")
                             ),
                             textInput("protocol_url", "Protocol URL:", placeholder = "Enter full URL of protocol"),
                             textInput("protocol_doi", "Protocol DOI/ID:", placeholder = "Enter DOI or persistent identifier if available"),
                             dateInput("protocol_date", "Protocol Date:", value = NULL),
                             checkboxInput("protocol_prior", "Protocol was shared prior to data collection/access", value = FALSE)
                           ),
                           p("Study protocols provide comprehensive information about planned methods. They may be shared as preprints or journal articles, included with study registrations, and/or made publicly available.")
                       ),
                       box(width = NULL,
                           title = "Analysis Plan",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("analysis_plan", "Implementation Level:",
                                        choices = list("Not Implemented" = 0,
                                                       "Level 1: Disclosed" = 1,
                                                       "Level 2: Shared and Cited" = 2,
                                                       "Level 3: Certified" = 3),
                                        selected = 0),
                           conditionalPanel(
                             condition = "input.analysis_plan > 0",
                             checkboxInput("same_as_protocol", "Analysis plan is included in the study protocol", value = FALSE),
                             conditionalPanel(
                               condition = "!input.same_as_protocol",
                               selectInput("analysis_plan_location", "Analysis Plan Location:",
                                           choices = list("Select location" = "",
                                                          "OSF" = "osf",
                                                          "GitHub" = "github",
                                                          "Zenodo" = "zenodo",
                                                          "Journal Supplementary Materials" = "journal",
                                                          "Institutional Repository" = "institutional",
                                                          "Other" = "other")),
                               conditionalPanel(
                                 condition = "input.analysis_plan_location == 'other'",
                                 textInput("other_analysis_location", "Specify Other Location:", placeholder = "Enter location name")
                               ),
                               textInput("analysis_plan_url", "Analysis Plan URL:", placeholder = "Enter full URL of analysis plan"),
                               textInput("analysis_plan_doi", "Analysis Plan DOI/ID:", placeholder = "Enter DOI or persistent identifier if available")
                             ),
                             dateInput("analysis_plan_date", "Analysis Plan Date:", value = NULL),
                             checkboxInput("analysis_plan_prior", "Analysis plan was shared prior to data analysis", value = FALSE),
                             checkboxInput("analysis_plan_code", "Analysis plan includes executable code", value = FALSE)
                           ),
                           p("Analysis plans provide comprehensive information about planned analyses. They may be in the same document as the protocol or different documents.")
                       ),
                       box(width = NULL,
                           title = "Reporting Transparency",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("reporting", "Implementation Level:",
                                        choices = list("Not Implemented" = 0,
                                                       "Level 1: Disclosed" = 1,
                                                       "Level 2: Shared and Cited" = 2,
                                                       "Level 3: Certified" = 3),
                                        selected = 0),
                           p("Reporting transparency is implemented through the use of reporting guidelines to ensure comprehensive reporting of study details.")
                       )
                ),
                column(width = 6,
                       box(width = NULL,
                           title = "Materials Transparency",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("materials", "Implementation Level:",
                                        choices = list("Not Implemented" = 0,
                                                       "Level 1: Disclosed" = 1,
                                                       "Level 2: Shared and Cited" = 2,
                                                       "Level 3: Certified" = 3),
                                        selected = 0),
                           p("Materials transparency involves sharing research materials such as stimuli, survey instruments, and intervention materials.")
                       ),
                       box(width = NULL,
                           title = "Data Transparency",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("data", "Implementation Level:",
                                        choices = list("Not Implemented" = 0,
                                                       "Level 1: Disclosed" = 1,
                                                       "Level 2: Shared and Cited" = 2,
                                                       "Level 3: Certified" = 3),
                                        selected = 0),
                           conditionalPanel(
                             condition = "input.data > 0",
                             selectInput("data_availability", "Data Availability:",
                                         choices = list("Select availability" = "",
                                                        "Fully available" = "full",
                                                        "Partially available" = "partial",
                                                        "Available upon request" = "request",
                                                        "Not available due to restrictions" = "restricted")),
                             conditionalPanel(
                               condition = "input.data_availability == 'full' || input.data_availability == 'partial'",
                               selectInput("data_repository", "Data Repository:",
                                           choices = list("Select repository" = "",
                                                          "OSF" = "osf",
                                                          "Dataverse" = "dataverse",
                                                          "Dryad" = "dryad",
                                                          "figshare" = "figshare",
                                                          "Zenodo" = "zenodo",
                                                          "GitHub" = "github",
                                                          "Journal Supplementary Materials" = "journal",
                                                          "Institutional Repository" = "institutional",
                                                          "Field-specific Repository" = "field_specific",
                                                          "Other" = "other")),
                               conditionalPanel(
                                 condition = "input.data_repository == 'field_specific' || input.data_repository == 'other'",
                                 textInput("other_data_repository", "Specify Repository:", placeholder = "Enter repository name")
                               ),
                               textInput("data_url", "Data URL:", placeholder = "Enter full URL to data"),
                               textInput("data_doi", "Data DOI:", placeholder = "Enter DOI if available"),
                               checkboxInput("data_documented", "Data includes documentation/codebook", value = FALSE),
                               checkboxInput("data_machine_readable", "Data is in machine-readable format", value = FALSE)
                             ),
                             conditionalPanel(
                               condition = "input.data_availability == 'restricted'",
                               checkboxGroupInput("data_restrictions", "Reason for Restrictions:",
                                                  choices = list("Ethics constraints" = "ethics",
                                                                 "Privacy/Confidentiality" = "privacy",
                                                                 "Proprietary data" = "proprietary",
                                                                 "Data sovereignty" = "sovereignty",
                                                                 "Legal restrictions" = "legal",
                                                                 "Other" = "other")),
                               conditionalPanel(
                                 condition = "input.data_restrictions.includes('other')",
                                 textInput("other_restriction", "Specify Other Restriction:", placeholder = "Enter restriction details")
                               )
                             )
                           ),
                           p("Data transparency involves sharing the data underlying reported results, with appropriate metadata and documentation.")
                       ),
                       box(width = NULL,
                           title = "Analytic Code Transparency",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("code", "Implementation Level:",
                                        choices = list("Not Implemented" = 0,
                                                       "Level 1: Disclosed" = 1,
                                                       "Level 2: Shared and Cited" = 2,
                                                       "Level 3: Certified" = 3),
                                        selected = 0),
                           conditionalPanel(
                             condition = "input.code > 0",
                             selectInput("code_location", "Code Repository:",
                                         choices = list("Select repository" = "",
                                                        "GitHub" = "github",
                                                        "OSF" = "osf",
                                                        "Zenodo" = "zenodo",
                                                        "BitBucket" = "bitbucket",
                                                        "GitLab" = "gitlab",
                                                        "Journal Supplementary Materials" = "journal",
                                                        "Institutional Repository" = "institutional",
                                                        "Personal/Lab Website" = "website",
                                                        "Other" = "other")),
                             conditionalPanel(
                               condition = "input.code_location == 'other'",
                               textInput("other_code_location", "Specify Repository:", placeholder = "Enter repository name")
                             ),
                             textInput("code_url", "Code URL:", placeholder = "Enter full URL to code"),
                             textInput("code_doi", "Code DOI:", placeholder = "Enter DOI if available"),
                             selectInput("coding_language", "Primary Programming Language:",
                                         choices = list("Select language" = "",
                                                        "R" = "r",
                                                        "Python" = "python",
                                                        "SPSS" = "spss",
                                                        "Stata" = "stata",
                                                        "SAS" = "sas",
                                                        "MATLAB" = "matlab",
                                                        "Julia" = "julia",
                                                        "JavaScript" = "javascript",
                                                        "Other" = "other")),
                             conditionalPanel(
                               condition = "input.coding_language == 'other'",
                               textInput("other_language", "Specify Language:", placeholder = "Enter programming language")
                             ),
                             checkboxInput("code_documented", "Code includes documentation/comments", value = FALSE),
                             checkboxInput("code_version_control", "Code is under version control", value = FALSE),
                             checkboxInput("code_environment", "Computational environment/dependencies are documented", value = FALSE)
                           ),
                           p("Analytic code transparency involves sharing the code used to process data and generate reported results.")
                       )
                )
              )
      ),

      # Verification Practices tab
      tabItem(tabName = "verification",
              fluidRow(
                box(
                  width = 12,
                  title = "Verification Practices",
                  status = "primary",
                  solidHeader = TRUE,
                  p("Indicate whether the following verification practices were implemented:")
                )
              ),
              fluidRow(
                column(width = 6,
                       box(width = NULL,
                           title = "Results Transparency",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("results_trans", "Implementation:",
                                        choices = list("Not Implemented" = 0,
                                                       "Implemented" = 1),
                                        selected = 0),
                           p("A party independent from the researchers verified that results have not been reported selectively based on the nature of the findings. To verify, the independent party can check that the study registration, protocol, and analysis plan match the final report–and the final report acknowledges any deviations.")
                       )
                ),
                column(width = 6,
                       box(width = NULL,
                           title = "Computational Reproducibility",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("comp_repro", "Implementation:",
                                        choices = list("Not Implemented" = 0,
                                                       "Implemented" = 1),
                                        selected = 0),
                           p("A party independent from the researchers verified that reported results reproduce using the same data and following the same computational procedures. To verify, the independent party can check that they obtain the same results using data and code deposited in a trusted repository.")
                       )
                )
              )
      ),

      # Verification Studies tab
      tabItem(tabName = "studies",
              fluidRow(
                box(
                  width = 12,
                  title = "Verification Studies",
                  status = "primary",
                  solidHeader = TRUE,
                  p("Indicate which verification study types were implemented:")
                )
              ),
              fluidRow(
                column(width = 6,
                       box(width = NULL,
                           title = "Replication",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("replication", "Implementation:",
                                        choices = list("Not Implemented" = 0,
                                                       "Implemented" = 1),
                                        selected = 0),
                           p("A study that aims to provide diagnostic evidence about claims from a prior study by repeating the original study procedures in a new sample.")
                       ),
                       box(width = NULL,
                           title = "Registered Report",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("reg_report", "Implementation:",
                                        choices = list("Not Implemented" = 0,
                                                       "Implemented" = 1),
                                        selected = 0),
                           p("A registered study in which a study protocol and analysis plan are peer reviewed, and the study is accepted in-principle by a publication outlet, before the research is undertaken.")
                       )
                ),
                column(width = 6,
                       box(width = NULL,
                           title = "Multiverse",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("multiverse", "Implementation:",
                                        choices = list("Not Implemented" = 0,
                                                       "Implemented" = 1),
                                        selected = 0),
                           p("A study in which a single research team examines the research question of interest across different, reasonable choices for processing and analyzing the same data.")
                       ),
                       box(width = NULL,
                           title = "Many Analysts",
                           status = "info",
                           solidHeader = TRUE,
                           radioButtons("many_analysts", "Implementation:",
                                        choices = list("Not Implemented" = 0,
                                                       "Implemented" = 1),
                                        selected = 0),
                           p("A study in which independent analysis teams conduct plausible alternative analyses of a research question on the same dataset.")
                       )
                )
              )
      ),

      # Results tab
      tabItem(tabName = "results",
              fluidRow(
                valueBoxOutput("researchScore", width = 4),
                valueBoxOutput("verificationScore", width = 4),
                valueBoxOutput("studiesScore", width = 4)
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "TOP 2025 Summary Report",
                  status = "primary",
                  solidHeader = TRUE,
                  column(width = 8,
                         h4(textOutput("studyTitleOutput")),
                         htmlOutput("studyMetaOutput"),
                         htmlOutput("qualityBadges"),
                         hr(),
                         h4("Transparency Scores"),
                         plotOutput("overallScore", height = "250px")
                  ),
                  column(width = 4,
                         downloadButton("downloadReport", "Download Report",
                                        style = "margin-bottom: 15px; color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                         downloadButton("downloadBadges", "Download Badges",
                                        style = "margin-bottom: 15px; color: #fff; background-color: #5cb85c; border-color: #4cae4c"),
                         tags$div(style = "margin-top: 20px;",
                                  actionButton("createCitation", "Generate Citation",
                                               icon = icon("quote-right"),
                                               style = "width: 100%; margin-bottom: 10px; color: #fff; background-color: #5bc0de; border-color: #46b8da")
                         ),
                         conditionalPanel(
                           condition = "input.createCitation > 0",
                           wellPanel(
                             style = "margin-top: 10px; background-color: #f5f5f5; border-color: #ddd;",
                             h4("Citation"),
                             verbatimTextOutput("citationOutput"),
                             actionButton("copyCitation", "Copy to Clipboard",
                                          icon = icon("copy"),
                                          style = "width: 100%; margin-top: 10px;")
                           )
                         )
                  )
                )
              ),
              fluidRow(
                box(
                  width = 6,
                  title = "Research Practices Implementation",
                  status = "info",
                  solidHeader = TRUE,
                  plotOutput("researchPracticesPlot")
                ),
                box(
                  width = 6,
                  title = "Detailed Results",
                  status = "info",
                  solidHeader = TRUE,
                  tableOutput("detailedResults")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  title = "Recommendations for Improvement",
                  status = "warning",
                  solidHeader = TRUE,
                  htmlOutput("recommendations")
                )
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {

  # Save study metadata
  observeEvent(input$save_metadata, {
    showModal(modalDialog(
      title = "Metadata Saved",
      "Your study information has been saved successfully.",
      easyClose = TRUE,
      footer = NULL
    ))
  })

  # Detect registry type from URL
  observe({
    if (!is.null(input$registration_url) && input$registration_url != "") {
      url <- tolower(input$registration_url)

      # Auto-detect registry type based on URL
      if (grepl("osf.io", url)) {
        updateSelectInput(session, "registry_type", selected = "osf")
      } else if (grepl("clinicaltrials.gov", url)) {
        updateSelectInput(session, "registry_type", selected = "clinicaltrials")
      } else if (grepl("crd.york.ac.uk/prospero", url)) {
        updateSelectInput(session, "registry_type", selected = "prospero")
      } else if (grepl("socialscienceregistry.org", url)) {
        updateSelectInput(session, "registry_type", selected = "aea")
      } else if (grepl("isrctn.com", url)) {
        updateSelectInput(session, "registry_type", selected = "isrctn")
      } else if (grepl("researchregistry.com", url)) {
        updateSelectInput(session, "registry_type", selected = "researchregistry")
      }

      # Extract ID from URL for common registries
      if (grepl("osf.io", url)) {
        id <- sub(".*osf.io/([^/]+).*", "\\1", url)
        updateTextInput(session, "registration_id", value = id)
      } else if (grepl("clinicaltrials.gov/ct2/show/", url)) {
        id <- sub(".*show/([^/\\?]+).*", "\\1", url)
        updateTextInput(session, "registration_id", value = id)
      } else if (grepl("doi.org", url)) {
        doi <- sub(".*doi.org/([^\\s]+).*", "\\1", url)
        updateTextInput(session, "registration_id", value = doi)
      }
    }
  })

  # Extract DOI from URL in data section
  observe({
    if (!is.null(input$data_url) && input$data_url != "") {
      url <- tolower(input$data_url)

      if (grepl("doi.org", url)) {
        doi <- sub(".*doi.org/([^\\s]+).*", "\\1", url)
        updateTextInput(session, "data_doi", value = doi)
      }
    }
  })

  # Extract DOI from URL in code section
  observe({
    if (!is.null(input$code_url) && input$code_url != "") {
      url <- tolower(input$code_url)

      if (grepl("doi.org", url)) {
        doi <- sub(".*doi.org/([^\\s]+).*", "\\1", url)
        updateTextInput(session, "code_doi", value = doi)
      } else if (grepl("zenodo.org", url)) {
        updateSelectInput(session, "code_location", selected = "zenodo")
      } else if (grepl("github.com", url)) {
        updateSelectInput(session, "code_location", selected = "github")
      } else if (grepl("osf.io", url)) {
        updateSelectInput(session, "code_location", selected = "osf")
      } else if (grepl("gitlab.com", url)) {
        updateSelectInput(session, "code_location", selected = "gitlab")
      } else if (grepl("bitbucket.org", url)) {
        updateSelectInput(session, "code_location", selected = "bitbucket")
      }
    }
  })

  # Sample conceptual framework diagram (simplified version)
  output$conceptualFramework <- renderPlot({
    par(mar = c(1, 1, 1, 1))
    plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), axes = FALSE, xlab = "", ylab = "")

    # Draw boxes
    rect(1, 6, 4, 9, col = "lightgreen", border = "darkgreen")
    rect(6, 6, 9, 9, col = "lightcoral", border = "darkred")
    rect(6, 1, 9, 4, col = "lightblue", border = "darkblue")
    rect(3.5, 3.5, 6.5, 6.5, col = "lightgrey", border = "black")

    # Add text
    text(2.5, 8.5, "Research\nPractices", font = 2)
    text(7.5, 8.5, "Verification\nPractices", font = 2)
    text(7.5, 3.5, "Verification\nStudies", font = 2)
    text(5, 5, "Increased\nVerifiability of\nEmpirical Research\nClaims", font = 2)

    # Add arrows
    arrows(4, 7.5, 6, 7.5, length = 0.1, lwd = 2)
    arrows(7.5, 6, 7.5, 4, length = 0.1, lwd = 2)
    arrows(6, 3.5, 5.5, 4.5, length = 0.1, lwd = 2)
    arrows(2.5, 6, 2.5, 5, length = 0.1, lwd = 2)
    arrows(2.5, 5, 3.5, 5, length = 0.1, lwd = 2)
    arrows(4, 5, 2.5, 2.5, length = 0.1, lwd = 2)
    arrows(2.5, 2.5, 6, 2.5, length = 0.1, lwd = 2)
  })

  # Calculate Research Practices score
  researchPracticesScore <- reactive({
    practices <- c(
      as.numeric(input$study_reg),
      as.numeric(input$protocol),
      as.numeric(input$analysis_plan),
      as.numeric(input$reporting),
      as.numeric(input$materials),
      as.numeric(input$data),
      as.numeric(input$code)
    )

    # Calculate percentage of maximum possible score
    sum(practices) / (7 * 3) * 100
  })

  # Calculate Verification Practices score
  verificationPracticesScore <- reactive({
    practices <- c(
      as.numeric(input$results_trans),
      as.numeric(input$comp_repro)
    )

    # Calculate percentage of maximum possible score
    sum(practices) / 2 * 100
  })

  # Calculate Verification Studies score
  verificationStudiesScore <- reactive({
    studies <- c(
      as.numeric(input$replication),
      as.numeric(input$reg_report),
      as.numeric(input$multiverse),
      as.numeric(input$many_analysts)
    )

    # Calculate percentage of maximum possible score
    sum(studies) / 4 * 100
  })

  # Calculate overall score
  overallScore <- reactive({
    # Weighted average (research practices have higher weight)
    (researchPracticesScore() * 0.6) +
      (verificationPracticesScore() * 0.2) +
      (verificationStudiesScore() * 0.2)
  })

  # Display Research Practices score
  output$researchScore <- renderValueBox({
    score <- researchPracticesScore()
    color <- ifelse(score >= 80, "green",
                    ifelse(score >= 50, "yellow", "red"))

    valueBox(
      paste0(round(score, 1), "%"),
      "Research Practices Score",
      icon = icon("flask"),
      color = color
    )
  })

  # Display Verification Practices score
  output$verificationScore <- renderValueBox({
    score <- verificationPracticesScore()
    color <- ifelse(score >= 80, "green",
                    ifelse(score >= 50, "yellow", "red"))

    valueBox(
      paste0(round(score, 1), "%"),
      "Verification Practices Score",
      icon = icon("check-circle"),
      color = color
    )
  })

  # Display Verification Studies score
  output$studiesScore <- renderValueBox({
    score <- verificationStudiesScore()
    color <- ifelse(score >= 80, "green",
                    ifelse(score >= 50, "yellow", "red"))

    valueBox(
      paste0(round(score, 1), "%"),
      "Verification Studies Score",
      icon = icon("search"),
      color = color
    )
  })

  # Generate quality badges
  output$qualityBadges <- renderUI({
    badges <- list()

    # Registration badge
    if (as.numeric(input$study_reg) >= 2 && !is.null(input$registration_url) && input$registration_url != "") {
      badges <- c(badges, tags$span(class = "badge bg-success", style = "margin-right: 5px; padding: 5px 10px; font-size: 14px;",
                                    tags$i(class = "fa fa-check-circle", style = "margin-right: 5px;"), "Registered"))
    }

    # Protocol badge
    if (as.numeric(input$protocol) >= 2 && ((input$same_as_protocol && !is.null(input$registration_url) && input$registration_url != "") ||
                                            (!input$same_as_protocol && !is.null(input$protocol_url) && input$protocol_url != ""))) {
      badges <- c(badges, tags$span(class = "badge bg-success", style = "margin-right: 5px; padding: 5px 10px; font-size: 14px;",
                                    tags$i(class = "fa fa-file-text", style = "margin-right: 5px;"), "Protocol Shared"))
    }

    # Open Data badge
    if (as.numeric(input$data) >= 2 && input$data_availability %in% c("full", "partial") &&
        !is.null(input$data_url) && input$data_url != "") {
      badges <- c(badges, tags$span(class = "badge bg-success", style = "margin-right: 5px; padding: 5px 10px; font-size: 14px;",
                                    tags$i(class = "fa fa-database", style = "margin-right: 5px;"), "Open Data"))
    }

    # Open Code badge
    if (as.numeric(input$code) >= 2 && !is.null(input$code_url) && input$code_url != "") {
      badges <- c(badges, tags$span(class = "badge bg-success", style = "margin-right: 5px; padding: 5px 10px; font-size: 14px;",
                                    tags$i(class = "fa fa-code", style = "margin-right: 5px;"), "Open Code"))
    }

    # Computational Reproducibility badge
    if (as.numeric(input$comp_repro) == 1) {
      badges <- c(badges, tags$span(class = "badge bg-success", style = "margin-right: 5px; padding: 5px 10px; font-size: 14px;",
                                    tags$i(class = "fa fa-repeat", style = "margin-right: 5px;"), "Computationally Reproducible"))
    }

    # Registered Report badge
    if (as.numeric(input$reg_report) == 1) {
      badges <- c(badges, tags$span(class = "badge bg-primary", style = "margin-right: 5px; padding: 5px 10px; font-size: 14px;",
                                    tags$i(class = "fa fa-clipboard-check", style = "margin-right: 5px;"), "Registered Report"))
    }

    # Return badges div
    if (length(badges) > 0) {
      tags$div(
        tags$h4("Quality Badges:"),
        tags$div(style = "margin-top: 10px;", badges)
      )
    } else {
      NULL
    }
  })

  # Display study title in results
  output$studyTitleOutput <- renderText({
    if (!is.null(input$study_title) && input$study_title != "") {
      paste(input$study_title)
    } else {
      "Transparency Evaluation Report"
    }
  })

  # Display study metadata in results
  output$studyMetaOutput <- renderUI({
    meta_items <- list()

    if (!is.null(input$study_authors) && input$study_authors != "") {
      meta_items <- c(meta_items, paste("<strong>Authors:</strong>", input$study_authors))
    }

    if (!is.null(input$discipline) && input$discipline != "") {
      discipline_text <- input$discipline
      if (discipline_text == "other" && !is.null(input$other_discipline) && input$other_discipline != "") {
        discipline_text <- input$other_discipline
      } else if (discipline_text == "biomedical") {
        discipline_text <- "Biomedical Sciences"
      } else if (discipline_text == "psychology") {
        discipline_text <- "Psychology"
      } else if (discipline_text == "social_sciences") {
        discipline_text <- "Social Sciences"
      } else if (discipline_text == "economics") {
        discipline_text <- "Economics"
      } else if (discipline_text == "computer_science") {
        discipline_text <- "Computer Science"
      } else if (discipline_text == "environmental") {
        discipline_text <- "Environmental Sciences"
      } else if (discipline_text == "engineering") {
        discipline_text <- "Engineering"
      }
      meta_items <- c(meta_items, paste("<strong>Discipline:</strong>", discipline_text))
    }

    if (!is.null(input$journal_name) && input$journal_name != "") {
      meta_items <- c(meta_items, paste("<strong>Journal/Publication:</strong>", input$journal_name))
    }

    if (!is.null(input$publication_status) && input$publication_status != "") {
      status_text <- input$publication_status
      if (status_text == "published") {
        status_text <- "Published"
      } else if (status_text == "in_press") {
        status_text <- "In Press"
      } else if (status_text == "under_review") {
        status_text <- "Under Review"
      } else if (status_text == "in_preparation") {
        status_text <- "In Preparation"
      } else if (status_text == "preprint") {
        status_text <- "Preprint"
      }
      meta_items <- c(meta_items, paste("<strong>Status:</strong>", status_text))
    }

    if (!is.null(input$publication_doi) && input$publication_doi != "") {
      meta_items <- c(meta_items, paste("<strong>DOI:</strong>", input$publication_doi))
    }

    if (length(meta_items) > 0) {
      HTML(paste(meta_items, collapse = " | "))
    } else {
      NULL
    }
  })

  # Generate citation text
  citation_text <- reactive({
    # Default if no metadata
    default_citation <- "TOP Guidelines Advisory Board. (2025). TOP 2025: An Update to the Transparency and Openness Promotion Guidelines."

    if (is.null(input$study_title) || input$study_title == "") {
      return(default_citation)
    }

    authors <- if (!is.null(input$study_authors) && input$study_authors != "") {
      input$study_authors
    } else {
      "Author, A."
    }

    year <- if (!is.null(input$publication_date)) {
      format(input$publication_date, "%Y")
    } else {
      format(Sys.Date(), "%Y")
    }

    journal <- if (!is.null(input$journal_name) && input$journal_name != "") {
      paste0(input$journal_name, ".")
    } else {
      ""
    }

    doi <- if (!is.null(input$publication_doi) && input$publication_doi != "") {
      paste0(" https://doi.org/", input$publication_doi)
    } else {
      ""
    }

    # APA style citation
    paste0(authors, " (", year, "). ", input$study_title, ". ", journal, doi)
  })

  # Display citation when button clicked
  output$citationOutput <- renderText({
    citation_text()
  })

  # Download report
  output$downloadReport <- downloadHandler(
    filename = function() {
      study_name <- if (!is.null(input$study_title) && input$study_title != "") {
        gsub("[^a-zA-Z0-9]", "_", input$study_title)
      } else {
        "TOP_2025_Report"
      }
      paste0(study_name, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Generate report content here
      # In a real app this would generate a PDF report
      saveRDS(list(
        study_title = input$study_title,
        authors = input$study_authors,
        discipline = input$discipline,
        journal = input$journal_name,
        doi = input$publication_doi,
        research_score = researchPracticesScore(),
        verification_score = verificationPracticesScore(),
        studies_score = verificationStudiesScore(),
        overall_score = overallScore()
      ), file)
    }
  )

  # Display Research Practices implementation levels
  output$researchPracticesPlot <- renderPlot({
    practices <- data.frame(
      Practice = c("Study Registration", "Study Protocol", "Analysis Plan",
                   "Reporting", "Materials", "Data", "Code"),
      Level = c(
        as.numeric(input$study_reg),
        as.numeric(input$protocol),
        as.numeric(input$analysis_plan),
        as.numeric(input$reporting),
        as.numeric(input$materials),
        as.numeric(input$data),
        as.numeric(input$code)
      )
    )

    # Reorder factors for better visualization
    practices$Practice <- factor(practices$Practice, levels = rev(practices$Practice))

    # Create the plot
    ggplot(practices, aes(x = Practice, y = Level)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      geom_text(aes(label = Level), hjust = -0.2) +
      scale_y_continuous(limits = c(0, 3.5), breaks = 0:3,
                         labels = c("None", "Level 1", "Level 2", "Level 3")) +
      coord_flip() +
      labs(y = "Implementation Level", x = "") +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)
      )
  })

  # Display detailed results table
  output$detailedResults <- renderTable({
    data.frame(
      Category = c(
        "Study Registration", "Study Protocol", "Analysis Plan",
        "Reporting Transparency", "Materials Transparency",
        "Data Transparency", "Code Transparency",
        "Results Transparency", "Computational Reproducibility",
        "Replication", "Registered Report", "Multiverse", "Many Analysts"
      ),
      Level = c(
        ifelse(as.numeric(input$study_reg) == 0, "Not Implemented",
               ifelse(as.numeric(input$study_reg) == 1, "Disclosed",
                      ifelse(as.numeric(input$study_reg) == 2, "Shared & Cited", "Certified"))),
        ifelse(as.numeric(input$protocol) == 0, "Not Implemented",
               ifelse(as.numeric(input$protocol) == 1, "Disclosed",
                      ifelse(as.numeric(input$protocol) == 2, "Shared & Cited", "Certified"))),
        ifelse(as.numeric(input$analysis_plan) == 0, "Not Implemented",
               ifelse(as.numeric(input$analysis_plan) == 1, "Disclosed",
                      ifelse(as.numeric(input$analysis_plan) == 2, "Shared & Cited", "Certified"))),
        ifelse(as.numeric(input$reporting) == 0, "Not Implemented",
               ifelse(as.numeric(input$reporting) == 1, "Disclosed",
                      ifelse(as.numeric(input$reporting) == 2, "Shared & Cited", "Certified"))),
        ifelse(as.numeric(input$materials) == 0, "Not Implemented",
               ifelse(as.numeric(input$materials) == 1, "Disclosed",
                      ifelse(as.numeric(input$materials) == 2, "Shared & Cited", "Certified"))),
        ifelse(as.numeric(input$data) == 0, "Not Implemented",
               ifelse(as.numeric(input$data) == 1, "Disclosed",
                      ifelse(as.numeric(input$data) == 2, "Shared & Cited", "Certified"))),
        ifelse(as.numeric(input$code) == 0, "Not Implemented",
               ifelse(as.numeric(input$code) == 1, "Disclosed",
                      ifelse(as.numeric(input$code) == 2, "Shared & Cited", "Certified"))),
        ifelse(as.numeric(input$results_trans) == 0, "Not Implemented", "Implemented"),
        ifelse(as.numeric(input$comp_repro) == 0, "Not Implemented", "Implemented"),
        ifelse(as.numeric(input$replication) == 0, "Not Implemented", "Implemented"),
        ifelse(as.numeric(input$reg_report) == 0, "Not Implemented", "Implemented"),
        ifelse(as.numeric(input$multiverse) == 0, "Not Implemented", "Implemented"),
        ifelse(as.numeric(input$many_analysts) == 0, "Not Implemented", "Implemented")
      )
    )
  })

  # Generate recommendations
  output$recommendations <- renderUI({
    recommendations <- list()

    # Study metadata recommendations
    if (is.null(input$study_title) || input$study_title == "") {
      recommendations <- c(recommendations, "Add your study title to better track your transparency evaluation.")
    }

    # Research Practices recommendations
    if (as.numeric(input$study_reg) < 2) {
      recommendations <- c(recommendations, "Consider registering your study in a public registry and citing this registration in your publication.")
    } else if (as.numeric(input$study_reg) == 2 && (is.null(input$registration_url) || input$registration_url == "")) {
      recommendations <- c(recommendations, "You've indicated Level 2 for study registration, but no URL was provided. Please add the registration URL.")
    }

    if (as.numeric(input$protocol) < 2) {
      recommendations <- c(recommendations, "Share your study protocol publicly and cite it in your publication.")
    } else if (as.numeric(input$protocol) == 2 && !input$same_as_protocol && (is.null(input$protocol_url) || input$protocol_url == "")) {
      recommendations <- c(recommendations, "You've indicated Level 2 for study protocol, but no URL was provided. Please add the protocol URL.")
    }

    if (as.numeric(input$analysis_plan) < 2) {
      recommendations <- c(recommendations, "Share your analysis plan publicly and cite it in your publication.")
    } else if (as.numeric(input$analysis_plan) == 2 && !input$same_as_protocol && (is.null(input$analysis_plan_url) || input$analysis_plan_url == "")) {
      recommendations <- c(recommendations, "You've indicated Level 2 for analysis plan, but no URL was provided. Please add the analysis plan URL.")
    }

    if (as.numeric(input$reporting) < 2) {
      recommendations <- c(recommendations, "Use and share a completed reporting guideline checklist appropriate for your study design.")
    }

    if (as.numeric(input$materials) < 2) {
      recommendations <- c(recommendations, "Share your study materials in a trusted repository and cite them in your publication.")
    }

    if (as.numeric(input$data) < 2) {
      recommendations <- c(recommendations, "Share your data in a trusted repository with appropriate metadata and cite it in your publication.")
    } else if (as.numeric(input$data) >= 1 && input$data_availability == "restricted") {
      recommendations <- c(recommendations, "You've indicated data restrictions. Consider sharing a synthetic dataset or de-identified subset if possible.")
    } else if (as.numeric(input$data) == 2 && input$data_availability %in% c("full", "partial") && !input$data_documented) {
      recommendations <- c(recommendations, "Enhance your shared data by adding proper documentation or a codebook.")
    }

    if (as.numeric(input$code) < 2) {
      recommendations <- c(recommendations, "Share your analytic code in a trusted repository and cite it in your publication.")
    } else if (as.numeric(input$code) == 2 && !input$code_documented) {
      recommendations <- c(recommendations, "Add documentation or comments to your shared code to improve usability.")
    } else if (as.numeric(input$code) == 2 && !input$code_environment) {
      recommendations <- c(recommendations, "Document your computational environment or dependencies to improve reproducibility.")
    }

    # Verification Practices recommendations
    if (as.numeric(input$results_trans) == 0) {
      recommendations <- c(recommendations, "Consider having an independent party verify that your results have not been selectively reported.")
    }

    if (as.numeric(input$comp_repro) == 0) {
      recommendations <- c(recommendations, "Consider having an independent party verify that your results can be computationally reproduced.")
    }

    # Verification Studies recommendations
    verification_studies_implemented <- sum(
      as.numeric(input$replication),
      as.numeric(input$reg_report),
      as.numeric(input$multiverse),
      as.numeric(input$many_analysts)
    )

    if (verification_studies_implemented == 0) {
      recommendations <- c(recommendations, "Consider implementing at least one verification study approach (Replication, Registered Report, Multiverse Analysis, or Many Analysts approach).")
    }

    # Format recommendations
    if (length(recommendations) > 0) {
      HTML(paste0("<p><strong>To improve your transparency score, consider the following recommendations:</strong></p>",
                  "<ul>",
                  paste0("<li>", recommendations, "</li>", collapse = ""),
                  "</ul>"))
    } else {
      HTML("<p><strong>Congratulations!</strong> Your research meets high standards for transparency and openness according to the TOP 2025 Guidelines. Continue maintaining these excellent practices.</p>")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

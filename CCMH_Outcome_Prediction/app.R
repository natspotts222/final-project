#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# CCMH Outcome Prediction Shiny App
# PSY 525 Final Project
# Natalie Pottschmidt
#

library(shiny)
library(tidyverse)
library(shinythemes)
load("prediction_app_data.rda")

# For readability below, define variables holding lists of strings for selection choices
predictor_variables <- c("--Select--" = "--Select--",
                         "Gender" = "Gender",
                         "Age Group" = "ClientAgeGroups",
                         "Sexual identity" = "Sexuality",
                         "Race/ethnicity"= "Race_Ethnicity",
                         "Racial minority status" = "RacialMinority",
                         "Prior counseling" = "PriorCounseling_yn",
                         "Prior hospitalization" = "PriorHospital_yn",
                         "Prior self-injury" = "PriorNSSI_yn",
                         "Prior suicidal ideation" = "PriorSI_yn",
                         "Prior suicide attempt" = "PriorAttempt_yn",
                         "Prior medication use" = "PriorMedication_yn",
                         "CCAPS subscale severity" = "Severity_first",
                         "Suicidal thoughts" = "SI_level",
                         "Violent thoughts" = "HI_level",
                         "Top presenting concern" = "TopConcern",
                         "Any presenting concern" = "Concern",
                         "Number of presenting concerns" = "NumConcernsGroups")
### (Right now, I'm limiting predictor variables to categorical options)
race_ethnicity <- c("African American/Black",
                    "American Indian or Alaskan Native",
                    "Asian American/Asian",
                    "Hispanic/Latinx",
                    "Native Hawaiian or Pacific Islander",
                    "Multi-Racial",
                    "White",
                    "Other")
concern_list <- c("Anxiety", "O/C", "Perfectionism",
                  "Stress", "Depression",
                  "Mood_instability",
                  "Anger_management", "Relationship",
                  "Interpersonal_fx", "Social_isolation",
                  "Family", "Grief", "Medical",
                  "Eating", "Sleep", "Sexual", "Pregnancy",
                  "Identity", "Self_esteem", "Adjustment",
                  "Cultural", "Sexual_orientation",
                  "Gender_identity", "Religion",
                  "Discrimination", "Academic",
                  "Career", "Attention", "Alcohol", 
                  "Drugs", "Addiction", "Self_injury",
                  "Suicidality", "Violence", "Psychoticism",
                  "Trauma", "Physical_abuse", "Sexual_abuse",
                  "Harassment", "Stalking", "Financial",
                  "Legal", "None", "Other",
                  "Emotion_dysregulation", "Autism",
                  "Learning_disability", "Dissociation",
                  "Mood_instability", "Attention",
                  "Generalized_anx", "Social_anx",
                  "Panic", "Test_anx", "Phobia", "Other_anx")

#### Define UI for application ####
ui <- fluidPage(theme = shinytheme("superhero"),

# Site tabs
    navbarPage(title = "Precision Outcome Prediction",
    
    # User instructions for using the different pieces of the prediction tool           
    tabPanel("Using this Tool",
             headerPanel(
                 "Welcome to the CCMH Precision Outcome Prediction (POP) tool!"
                 ),
             wellPanel("POP is designed to provide estimates of clinically significant outcomes that may be expected for various",
             "client populations. The estimates come from real client data collected in university counseling centers (UCCs)",
             "across the United States who are part of the Centers for Collegiate Mental Health (CCMH).",
             "Although we hope that the estimates provided here will help clinicians adjust their expectations when taking",
             "on a new client, please keep in mind that these are still averages and should be interpreted cautiously."),
             
             headerPanel(
                 "Population Builder"
             ),
             
             wellPanel("If you're interested in a specific demographic population, this tab allows you to specify such criteria",
             "as race, gender, age, and sexual orientation to limit all outcome predictions to this specific population.",
             "This can be helpful if you want to predict outcomes with more precision for a population of interest;",
             "however, if you'd like to compare outcomes across demographic groups, you can skip this tab."),
             
             headerPanel(
                 "Outcome Visualizations"
             ),

             wellPanel("If you specify demographic criteria in the Population Builder tab, the next tab (Outcomes Within the",
             "Selected Population) will display the clinical outcomes for that group. In the final tab, you can specify",
             "additional criteria to further define your population into subgroups. Alternatively, you can use this last",
             "tab alone to show outcomes for subgroups defined by 1 or 2 specified prediction characteristics.")),
    
    # Allows the user to select specific demographic groups to build a population subset
    tabPanel("Population Builder",
             titlePanel("UNDER CONSTRUCTION")),
    
    # Displays outcome plots averaging across the demographic subset created in previous tab
    tabPanel("Outcomes Within the Selected Population",
             titlePanel("UNDER CONSTRUCTION")),
    
    # Allows the user to select 1-2 baseline characteristics to present outcome plots ####
    # broken down by the subgroups (either with full dataset or the PopulationBuilder data subset)
    tabPanel("Prediction by Selected Characteristics",
        # Sidebar with predictor variable selectors 
        sidebarLayout(
            sidebarPanel(
                wellPanel(
                    selectInput(inputId = "predictor1",
                                label = "Please select a client characteristic to predict outcomes:",
                                choices = predictor_variables,
                                selected = predictor_variables[2]),
                    conditionalPanel(condition = "input.predictor1 == 'Race_Ethnicity'",
                                     selectInput(inputId = "racePredictor",
                                                 label = "Select 2-4 racial/ethnic groups to compare:",
                                                 choices = race_ethnicity,
                                                 multiple = T,
                                                 selected = race_ethnicity[c(1,3,4,7)])),
                    conditionalPanel(condition = "input.predictor1 == 'Concern'",
                                     selectInput(inputId = "concPredictor",
                                                 label = "Select up to 5 presenting concerns to compare:",
                                                 choices = concern_list,
                                                 multiple = T,
                                                 selected = concern_list[c(1,4,5,26)])),
                    conditionalPanel(condition = "input.predictor1 == 'TopConcern'",
                                     selectInput(inputId = "topConcPredictor",
                                                 label = "Select up to 5 top concerns to compare:",
                                                 choices = concern_list,
                                                 multiple = T,
                                                 selected = concern_list[c(1,4,5,26)]))
                ),
                wellPanel(
                    selectInput(inputId = "predictor2",
                                label = "You may select another characteristic, but it's not required.",
                                choices = predictor_variables),
                    conditionalPanel(condition = "input.predictor2 == 'Race_Ethnicity'",
                                     selectInput(inputId = "racePredictor2",
                                                 label = "Select 2-4 racial/ethnic groups to compare:",
                                                 choices = race_ethnicity,
                                                 multiple = T,
                                                 selected = race_ethnicity[c(1,3,4,7)])),
                    conditionalPanel(condition = "input.predictor2 == 'Concern'",
                                     selectInput(inputId = "concPredictor2",
                                                 label = "Select up to 5 presenting concerns to compare:",
                                                 choices = concern_list,
                                                 multiple = T,
                                                 selected = concern_list[c(1,4,5,26)])),
                    conditionalPanel(condition = "input.predictor2 == 'TopConcern'",
                                     selectInput(inputId = "topConcPredictor2",
                                                 label = "Select up to 5 top concerns to compare:",
                                                 choices = concern_list,
                                                 multiple = T,
                                                 selected = concern_list[c(1,4,5,26)]))
                ),
                wellPanel(
                    actionButton(inputId = "updatePlots", label = "Update Plots")
                    )
            ),

         # Create tabs with general overview + outcome visualizations
            mainPanel(
                tabsetPanel(
                    tabPanel("Overview",
                            "The plot below displays the number of clients per subgroup of the characteristic(s)",
                            "that you're interested in.",
                            "Keep these n's in mind when viewing the outcomes on subsequent tabs.",
                             plotOutput(outputId = "overviewPlot")),
                    tabPanel("Post-treatment CCAPS Subscales",
                             "The plot below displays average scores on the post-treatment CCAPS subscales.",
                             "Keep in mind that these scores are affected strongly by initial CCAPS subscale severity,",
                             "so for the most valid representation you may consider including severity as one of your",
                             "predictor variables of interest. Higher scores indicate higher symptom severity. The red",
                             "line indicates the cutoff for high clinical severity in each subscale, while the green line indicates the low cut.",
                             plotOutput(outputId = "postTxCCAPS")),
                    tabPanel("CCAPS Symptom Change",
                             "The plot below displays average change of CCAPS subscale scores over the treatment course.",
                             "Keep in mind that these scores are affected strongly by initial CCAPS subscale severity,",
                             "so for the most valid representation you may consider including severity as one of your",
                             "predictor variables of interest. More negative change scores indicate greater improvement.",
                             "The green line indicates the reliable change index (RCI) for each subscale.",
                             plotOutput(outputId = "changeCCAPS")),
                    tabPanel("Sessions Used",
                             "The plot below displays average number of individual counseling sessions used by clients",
                             "in each subgroup of your selected characteristic(s).",
                            plotOutput(outputId = "sessionsUsed")),
                    tabPanel("Attendance Rate",
                             "The plot below displays average rate of appointment attendance for clients",
                             "in each subgroup of your selected characteristic(s) - aka, how often they attended a scheduled",
                             "session instead of cancelling the appointment.",
                             plotOutput(outputId = "sessionRate"))
                )
            )
        )
        )
    )
)


## Defines the server ####

server <- function(input, output) {

# Define theme adjustments for plots below
    white_line <- element_line(color = "white")
    bold_white_text <- element_text(face = "bold", color = "white")
    white_text <- element_text(color = "white")
    my_theme <- function() {
        theme(plot.background = element_blank(),
              panel.background = element_blank(),
              panel.grid.major = white_line,
              panel.grid.minor = element_blank(),
              axis.title = bold_white_text,
              axis.text = white_text,
              axis.text.x = element_text(angle = 90))}
    
# Create an auxiliary dataframe to hold values for CCAPS subscale high/low cuts and reliable change indices
    hline_dat <- data.frame(Subscale = c("Depression34", "Anxiety34", "SocialAnxiety34", "Academics34",
                                         "Eating34", "Hostility34", "Alcohol34", "DI"),
                            lowCut = c(1, 1.33, 1.4, 1.25, 0.96, 0.84, 0.6, 1.3),
                            highCut = c(1.83, 2.1, 2.5, 2.5, 1.5, 1.17, 1.1, 2.25),
                            RCI = c(-1.05, -1.07, -1.09, -1.38, -1.34, -0.98, -1.12, -0.79))

# Further define predictor dataset  ####
    defPredData <- eventReactive(input$updatePlots, {
        if(req(input$predictor1) == "Race_Ethnicity") {
            if(req(input$predictor2) == "Concern") {
                OutPredApp %>% filter(Race_Ethnicity %in% input$racePredictor &
                                                Concern %in% input$concPredictor2)
            } else if(req(input$predictor2) == "TopConcern") {
                OutPredApp %>% filter(Race_Ethnicity %in% input$racePredictor &
                                                TopConcern %in% input$topConcPredictor2)
            } else {
                OutPredApp %>% filter(Race_Ethnicity %in% input$racePredictor)
            }
        } else if(req(input$predictor1) == "Concern") {
            if(req(input$predictor2) == "Race_Ethnicity") {
                OutPredApp %>% filter(Concern %in% input$concPredictor &
                                                Race_Ethnicity %in% input$racePredictor2)
            } else if(req(input$predictor2) == "TopConcern") {
                OutPredApp %>% filter(Concern %in% input$concPredictor &
                                                TopConcern %in% input$topConcPredictor2)
            } else {
                OutPredApp %>% filter(Concern %in% input$concPredictor)
            }
        } else if(req(input$predictor1) == "TopConcern") {
            if(req(input$predictor2) == "Race_Ethnicity") {
                OutPredApp %>% filter(TopConcern %in% input$topConcPredictor &
                                                Race_Ethnicity %in% input$racePredictor2)
            } else if(req(input$predictor2) == "Concern") {
                OutPredApp %>% filter(TopConcern %in% input$topConcPredictor &
                                                Concern %in% input$concPredictor2)
            } else {
                OutPredApp %>% filter(TopConcern %in% input$topConcPredictor)
            }
        } else if(req(input$predictor2) == "Race_Ethnicity") {
            OutPredApp %>% filter(Race_Ethnicity %in% input$racePredictor2)
        } else if(req(input$predictor2) == "Concern") {
            OutPredApp %>% filter(Concern %in% input$concPredictor2)
        } else if(req(input$predictor2) == "TopConcern") {
            OutPredApp %>% filter(TopConcern %in% input$topConcPredictor2)
        } else {OutPredApp}
    })    

    

# Create reactive expressions for evaluating chosen predictors as variable names ####
    NamePred1 <- eventReactive(input$updatePlots, {
        as.name(input$predictor1)
    })
    
    NamePred2 <- eventReactive(input$updatePlots, {
        as.name(input$predictor2)
    })
    
# Bar graph for number of clients in each subgroup of the chosen predictor variable(s) ####
reOverviewPlot <- eventReactive(input$updatePlots, {
    if(req(input$predictor2) == "--Select--" ) {
        # Define base ggplot object for 1 predictor
        overview1pred <- ggplot(data = subset(defPredData(), 
                                              !is.na(eval(NamePred1())))) +
            geom_bar(aes(x = eval(NamePred1()), 
                         y = UniqueClientID),
                     stat = "summary", fun.y = "n_distinct", fill = "darkorange2") +
            labs(x = input$predictor1,
                 y = "Number of Clients") + 
            my_theme()
        
        # This part of the code generates the appropriate plot based on conditional selection
        if(req(input$predictor1) == "Severity_first") {
            overview1pred +
                facet_wrap(~Subscale)
        } else {
            overview1pred
        }
    } else {
        # 2 predictors
        overview2pred <- ggplot(data = subset(defPredData(), 
                                              (!is.na(eval(NamePred1())) & 
                                                   !is.na(eval(NamePred2()))))) +
            geom_bar(aes(x = eval(NamePred1()), 
                         y = UniqueClientID, 
                         fill = eval(NamePred2())),
                     stat = "summary", fun.y = "n_distinct", position = "dodge") +
            labs(x = input$predictor1,
                 y = "Number of Clients by Subgroups",
                 fill = input$predictor2) + 
            scale_fill_brewer(palette = "PuOr") +
            my_theme()
        
        # This part of the code generates the appropriate plot based on conditional selection
        if(req(input$predictor1) == "Severity_first" | req(input$predictor2) == "Severity_first") {
            overview2pred +
                facet_wrap(~Subscale)
        } else {
            overview2pred
        }
    }
})    
    
    output$overviewPlot <- renderPlot({
        reOverviewPlot()
    }, bg = "transparent")
    
# Bar graph for mean treatment outcome CCAPS scores based on chosen predictors ####
rePostPlot <- eventReactive(input$updatePlots, {
    if(req(input$predictor2) == "--Select--" ) {
        # Define base ggplot object for 1 predictor
        ggplot(data = subset(defPredData(),
                             (!is.na(Score_last) &
                                  !is.na(eval(NamePred1()))))) +
            geom_bar(aes(x = eval(NamePred1()), 
                         y = Score_last),
                     stat = "summary", fun.y = "mean", fill = "darkorange2") +
            facet_wrap(~Subscale) +
            geom_hline(data = hline_dat, aes(yintercept = lowCut), size = .75, color = "green3") +
            geom_hline(data = hline_dat, aes(yintercept = highCut), size = .75, color = "red3") +
            labs(x = input$predictor1,
                 y = "Mean Post-Treatment CCAPS Score") + 
            my_theme()
        
    } else {
        # 2 predictors
        ggplot(data = subset(defPredData(),
                             (!is.na(Score_last) &
                                  !is.na(eval(NamePred1())) &
                                  !is.na(eval(NamePred2()))))) +
            geom_bar(aes(x = eval(NamePred1()), 
                         y = Score_last, 
                         fill = eval(NamePred2())),
                     stat = "summary", fun.y = "mean", position = "dodge") +
            facet_wrap(~Subscale) +
            geom_hline(data = hline_dat, aes(yintercept = lowCut), size = .75, color = "green3") +
            geom_hline(data = hline_dat, aes(yintercept = highCut), size = .75, color = "red3") +
            labs(x = input$predictor1,
                 y = "Mean Post-Treatment CCAPS Score",
                 fill = input$predictor2) + 
            scale_fill_brewer(palette = "PuOr") +
            my_theme()
    }
})
    
    output$postTxCCAPS <- renderPlot({
        rePostPlot()
    }, bg = "transparent")

# Bar graph for change in CCAPS scores based on chosen predictors ####
    reChangePlot <- eventReactive(input$updatePlots, {
        if(req(input$predictor2) == "--Select--" ) {
            # Define base ggplot object for 1 predictor
            ggplot(data = subset(defPredData(),
                                 (!is.na(change_score) &
                                      !is.na(eval(NamePred1()))))) +
                geom_bar(aes(x = eval(NamePred1()), 
                             y = change_score),
                         stat = "summary", fun.y = "mean", fill = "darkorange2") +
                facet_wrap(~Subscale) +
                geom_hline(data = hline_dat, aes(yintercept = RCI), size = .75, color = "green3") +
                labs(x = input$predictor1,
                     y = "Mean Change in CCAPS Score") + 
                my_theme()
        } else {
            # 2 predictors
            ggplot(data = subset(defPredData(),
                                 (!is.na(change_score) &
                                      !is.na(eval(NamePred1())) &
                                      !is.na(eval(NamePred2()))))) +
                geom_bar(aes(x = eval(NamePred1()), 
                             y = change_score, 
                             fill = eval(NamePred2())),
                         stat = "summary", fun.y = "mean", position = "dodge") +
                facet_wrap(~Subscale) +
                geom_hline(data = hline_dat, aes(yintercept = RCI), size = .75, color = "green3") +
                labs(x = input$predictor1,
                     y = "Mean Change in CCAPS Score",
                     fill = input$predictor2) + 
                scale_fill_brewer(palette = "PuOr") +
                my_theme()
        }
    })
    
    output$changeCCAPS <- renderPlot({
        reChangePlot()
    }, bg = "transparent")
     
# Bar graph for number of sessions used based on chosen predictors ####
reSessionPlot <- eventReactive(input$updatePlots, {
    if(req(input$predictor2) == "--Select--" ) {
        # Define base ggplot object for 1 predictor
        sessions1pred <- ggplot(data = subset(defPredData(), 
                                              !is.na(num_appt_attended) &
                                                  !is.na(eval(NamePred1())))) +
            geom_bar(aes(x = eval(NamePred1()), 
                         y = num_appt_attended),
                     stat = "summary", fun.y = "mean", fill = "darkorange2") +
            labs(x = input$predictor1,
                 y = "Average Number of Appointments Attended") + 
            my_theme()
        
        # This part of the code generates the appropriate plot based on conditional selection
        if(req(input$predictor1) == "Severity_first") {
            sessions1pred +
                facet_wrap(~Subscale)
        } else {
            sessions1pred
        }
    } else {
        # 2 predictors
        sessions2pred <- ggplot(data = subset(defPredData(), 
                                              (!is.na(num_appt_attended) &
                                                   !is.na(eval(NamePred1())) & 
                                                   !is.na(eval(NamePred2()))))) +
            geom_bar(aes(x = eval(NamePred1()), 
                         y = num_appt_attended, 
                         fill = eval(NamePred2())),
                     stat = "summary", fun.y = "mean", position = "dodge") +
            labs(x = input$predictor1,
                 y = "Average Number of Appointments Attended",
                 fill = input$predictor2) + 
            scale_fill_brewer(palette = "PuOr") +
            my_theme()
        
        # This part of the code generates the appropriate plot based on conditional selection
        if(req(input$predictor1) == "Severity_first" | req(input$predictor2) == "Severity_first") {
            sessions2pred +
                facet_wrap(~Subscale)
        } else {
            sessions2pred
        }
    }
})
    output$sessionsUsed <- renderPlot({
        reSessionPlot()
    }, bg = "transparent")
    
# Bar graph for rate of appointment attendance based on chosen predictors ####
reRatePlot <- eventReactive(input$updatePlots, {
    if(req(input$predictor2) == "--Select--" ) {
        # Define base ggplot object for 1 predictor
        rate1pred <- ggplot(data = subset(defPredData(), 
                                          !is.na(attend_rate) &
                                              !is.na(eval(NamePred1())))) +
            geom_bar(aes(x = eval(NamePred1()), 
                         y = attend_rate),
                     stat = "summary", fun.y = "mean", fill = "darkorange2") +
            labs(x = input$predictor1,
                 y = "Average Appointment Attendance Rate") + 
            my_theme()
        
        # This part of the code generates the appropriate plot based on conditional selection
        if(req(input$predictor1) == "Severity_first") {
            rate1pred +
                facet_wrap(~Subscale)
        } else {
            rate1pred
        }
    } else {
        # 2 predictors
        rate2pred <- ggplot(data = subset(defPredData(), 
                                          (!is.na(attend_rate) &
                                               !is.na(eval(NamePred1())) & 
                                               !is.na(eval(NamePred2()))))) +
            geom_bar(aes(x = eval(NamePred1()), 
                         y = attend_rate, 
                         fill = eval(NamePred2())),
                     stat = "summary", fun.y = "mean", position = "dodge") +
            labs(x = input$predictor1,
                 y = "Average Appointment Attendance Rate",
                 fill = input$predictor2) + 
            scale_fill_brewer(palette = "PuOr") +
            my_theme()
        
        # This part of the code generates the appropriate plot based on conditional selection
        if(req(input$predictor1) == "Severity_first" | req(input$predictor2) == "Severity_first") {
            rate2pred +
                facet_wrap(~Subscale)
        } else {
            rate2pred
        }
    }
})
    output$sessionRate <- renderPlot({
        reRatePlot()
    }, bg = "transparent")
}


# Run the application 
shinyApp(ui = ui, server = server)

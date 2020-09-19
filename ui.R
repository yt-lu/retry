#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(DT)
library(shiny)
library(shinyalert)
library(shinydashboard)

calendar_ui <- read.csv('calendar.csv', strip.white = TRUE)

dashboardPage(
    dashboardHeader(title = "Retry Appointments"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Appointments", tabName = "students", icon = icon("th")),
            menuItem("Admin", tabName = "admin", icon = icon("th"))
        )
    ),#end of dashboardSiderbar
    dashboardBody(
        tabItems(
            tabItem(tabName = "students", 
                    box(
                        title = "Make an Appointment",
                        numericInput('idnew', 'MU ID', NULL),
                        selectInput('weeknew', 'Week',
                                    calendar_ui$Week), 
                        selectInput('daynew', "Day", 
                                    c("", "Monday", "Tuesday", "Wednesday", 
                                      "Thursday", "Friday")),
                        selectInput('standardnew', 'Learning Standard',
                                    c("", "L1", "L2", "L3", "LC",
                                      "D1", "D2", "D3", "D4", "D5",
                                      "A1", "A2", "A3", "AC",
                                      "I1", "I2", "I3", "I4", "IC")
                        ),
                        selectInput('formatnew', 'Retry format',
                                    c("Written", "Oral")),
                        conditionalPanel(
                            condition = "input.formatnew == 'Written'",
                            selectInput('timew', 'Time', 
                                        c('', format(seq(strptime("8:00 am", "%I:%M %p"), strptime("8:00 pm", "%I:%M %p"), by = 1800), "%I:%M %p")))
                        ),
                        conditionalPanel(
                            condition = "input.formatnew == 'Oral'",
                            textInput('timeo', 'Please enter a few time slots for your instructor to choose from. Preferred during the office hours.', value = "")
                        ),
                        useShinyalert(),
                        actionButton("new", "Submit")
                    ), #end of box 1
                    
                    box(
                        title = "My Upcoming Appointments",
                        numericInput('idview', 'MU ID', NULL), 
                        useShinyalert(),
                        actionButton("view", "View"),
                        actionButton("download", "Download"),
                        DT::dataTableOutput("view")
                        
                    ), # end of box 2
                    
                    box(
                        title = "Cancel an Appointment",
                        numericInput('idcancel', 'MU ID', NULL), 
                        selectInput('weekcancel', 'Week',
                                    calendar_ui$Week), 
                        selectInput('daycancel', "Day", 
                                    c("", "Monday", "Tuesday", "Wednesday", 
                                      "Thursday", "Friday")),
                        selectInput('standardcancel', "Standard", 
                                    c("", "L1", "L2", "L3", "LC",
                                      "D1", "D2", "D3", "D4", "D5",
                                      "A1", "A2", "A3", "AC",
                                      "I1", "I2", "I3", "I4", "IC")),
                        useShinyalert(),
                        actionButton("cancel", "Remove")
                    ) # end of box 3
                    ), #end of tabItem student
            
            
            
            tabItem(tabName = "admin", h2("Admin Page"),
                    passwordInput('pwadmin', "Password"),
                    
                
                    
                    box(
                        conditionalPanel(condition = "input.pwadmin == '1team'", 
                                         selectInput('instructor', 'Instructor',
                                                     c("", "MVP", "Symington", "Troupe"))
                        ),
                        
                        selectInput('weekadmin', 'Week',
                                    calendar_ui$Week), 
                        selectInput('dayadmin', 'Day',
                                    c("", "Monday", "Tuesday", "Wednesday", "Thursday","Friday")), 
                        selectInput('standardadmin', 'Standard',
                                    c("", "L1", "L2", "L3", "LC",
                                      "D1", "D2", "D3", "D4", "D5",
                                      "A1", "A2", "A3", "AC",
                                      "I1", "I2", "I3", "I4", "IC")
                                    ),
                        conditionalPanel(condition = "input.pwadmin == '1team'",
                                         useShinyalert(),
                                         actionButton("summary", "View Appointments")
                        ),
                    ),
                    
                    
                    box(
                        title = "Appointment Management",
                        numericInput('idadmin', 'MU ID', NULL),
                        radioButtons("actionadmin", "",
                                     c("Update Result" = "result",
                                       "Update time" = "time")
                                     ),
                        conditionalPanel(
                            condition = "input.actionadmin == 'time'",
                            selectInput('timeadmin', 'Time', 
                                        c('', format(seq(strptime("8:00 am", "%I:%M %p"), strptime("9:00 pm", "%I:%M %p"), by = 900), "%I:%M %p")))
                        ),
                        
                        conditionalPanel(
                            condition = "input.actionadmin == 'result'",
                            selectInput('resultadmin', 'Result',
                                    c("Fail", "Pass"))
                        ),
                        
                        conditionalPanel(condition = "input.pwadmin == '1team'",
                                         useShinyalert(),
                                         actionButton("update", "Update")
                        )
                    ),
                    
                    DT::dataTableOutput("apts")
                    ) # end of tabItem admin
        ), # end of tabItems 
        tags$head(tags$style(HTML('.skin-blue .main-header .logo {font-weight: 600;}
                                  ')))
    )#end of dashboardBody
)

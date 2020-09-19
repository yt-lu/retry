library(DT)
library(shiny)
library(shinyalert)
library(shinydashboard)

Sys.setenv(TZ = 'US/Eastern')

shinyServer(function(input, output, session) {
  ################################################
  ## New Appointment Panel
  ################################################
  observeEvent(input$new, {
    # Get roster
    roster <- read.csv('roster.csv' ,strip.white = TRUE)
    app <- read.csv('app.csv', strip.white = TRUE)
    
    if (input$idnew %in% roster$ID) {
      
      # Retrieve the row number of the student/ID
      j <- which(roster$ID == input$idnew)
      
      error_create <- HTML("<em>Sorry! Your passcode did not match the first one. Reservation failed.</em>")
      error_incomplete <- HTML("<em>Please fill all fields.</em>")
      error_late <- HTML("<em>Sorry! Appointment needs to be done by 5:00 pm the day before the desired appointment day.</em>")
      error_max_retry <- HTML("<em>Sorry! You have reached the max number of learning standards you can retry for the chosen week.</em>")
      error_no_match <- HTML(paste0("<em>Reservation Failed.<br>Please check your passcode.<br>", 
                                   "If you forgot it, please contact your instructor.</em>"))
      error_no_token <- HTML("<em>Sorry! You have scheduled one standard already in the chosen week. You don't have a token for scheduling another standard.</em>")
      error_numalpha_only <- HTML("<em>Sorry! Please use letters and digits only. Reservation failed.</em>")
      error_pass <- HTML(paste0("<em>Your have shown mastery of ", input$standardnew," this week. You need to demonstrate the second mastery in another week.</em>"))
      error_pending <- HTML(paste0("<em>Your last retry of ", input$standardnew," in this week is pending grading. You cannot make a new appointment before your instructor removes the lock.</em>"))
      error_time_conflict <- HTML("<em>Sorry! You already have an appointment at the time you selected.</em>")
      msg_create <- HTML("<em>Please create a passcode with only letters and numbers. 
                                No restrictions on length or cases.</em>")
      msg_create2 <- HTML("<em>Please confirm your passcode.</em>")
      msg_input <- HTML(paste0("<em>Please input your passcode.<br>", 
                               "If you forgot it, please contact<br>",
                               "LU_Y@MERCER.EDU</em>"))
      msg_token <- HTML("<em>You have scheduled one standard already in the chosen week. Continue the current appointment will cost you a token. To quit the current reservation, skip the passcode window in the next screen.</em>")
      msg_yeah <- HTML(paste0("<em>Hello, ", 
                              sub('(.*)\\,\\s+(.*)','\\2 \\1',roster$Student[j]), 
                              ", your appoinment has been confirmed.<br>",
                              "Please <span style='color:red'>save your passcode</span> for future appointments.",
                              "</em>"))
      
      combo1 <- input$formatnew == "Written" & input$timew == ""
      combo2 <- input$formatnew == "Oral" & input$timeo == ""
      if (input$weeknew == "" | input$daynew == "" | input$standardnew == "" | input$formatnew == "" | combo1 | combo2 ) {
        shinyalert(error_incomplete, type = "error", html = TRUE)   
      }
      else{
        calendar <- read.csv('calendar.csv', strip.white = TRUE)
        appdate <- calendar[which(calendar$Week == input$weeknew & calendar$Day == input$daynew), which(names(calendar) == 'Date')]
        aheadoftime <- (as.Date(appdate) - Sys.Date()) > 1 | ((as.Date(appdate) - Sys.Date()) == 1 & (strptime("5:00 pm", "%I:%M %p") - Sys.time()) > 0 ) 
        
        if (!aheadoftime) {
          shinyalert(error_late, type = "error", html = TRUE)
        }
        else {
          
          already <- app[which(app$ID == input$idnew & app$Week == input$weeknew),]
          if (input$formatnew == 'Written' & input$timew %in% unique(already[which(already$Day == input$daynew), which(names(already) == 'Time')])) {
            shinyalert(error_time_conflict, type = "error", html = TRUE)
          }
          else{
            
            if (length(unique(already$Standards)) == 2 & !(input$standardnew %in% unique(already$Standards))){
              shinyalert(error_max_retry, type = "error", html = TRUE)
            }
            else if (length(unique(already$Standards)) == 1 & !(input$standardnew %in% unique(already$Standards)) & roster[j, which(names(roster) == "Tokens")] == 0) {
              shinyalert(error_no_token, type = "error", html = TRUE)
            }
            else {
              
              lasttry <- tail(already[which(already$Standards == input$standardnew), ],1)
              
              lock <- ifelse(nrow(lasttry) == 0, NA, lasttry$Result)
              lock <- ifelse(lock == '', NA, lock)
              
              if (nrow(lasttry) > 0 & is.na(lock)) {
                shinyalert(error_pending, type = "error", html = TRUE)
              }
              else if (nrow(lasttry) > 0 & lock == 'Pass') {
                shinyalert(error_pass, type = "error", html = TRUE)
              }
              else {
                if (length(already$Standards) == 1 & !(input$standardnew %in% unique(already$Standards))) {shinyalert(msg_token, type = "info", html = TRUE)}
                
                if (input$formatnew == 'Written') {t <- input$timew}
                else {t <- input$timeo}
                
                # If PIN does not exist ... 
                # Pop out a window for password and repeat password
                # Check if the input has special characters or space
                # Ask for confirming the password again
                # Print a success message
                if (is.na(roster$PIN[j])) {
                  
                  shinyalert(
                    msg_create, type = "input", html = TRUE,
                    callbackR = function(x) { if(x != '' & !grepl('[[:punct:]]|[[:space:]]', x)) {
                      shinyalert(msg_create2, type = "input", html = TRUE,
                                 callbackR = function(x2) { if(x2 == x) {
                                   apt <- data.frame(roster$Student[j], roster$ID[j], roster$Instructor[j], input$standardnew, 
                                                     input$weeknew, input$daynew, input$formatnew, t, '')
                                   write.table(apt, 'app.csv', append = TRUE, 
                                               col.names = FALSE, row.names = FALSE, sep = ",")
                                   roster$PIN[j] <- x
                                   write.table(roster, "roster.csv", sep = ',', row.names = FALSE)
                                   shinyalert(msg_yeah, "Good Luck!", type = "success", html = TRUE)
                                 }else{
                                   shinyalert(error_create, type = "error", html = TRUE)
                                 } 
                                 })# end of second callbackR and shinyalert
                    }else {
                      shinyalert(error_numalpha_only, type = "error", html = TRUE)
                    } # end of first callbackR ifelse
                    }) # end of first callbackR and shinyalert
                }else{
                  shinyalert(msg_input, type = "input", html = TRUE,
                             callbackR = function(x) {if (x == roster$PIN[j]) {
                               apt <- data.frame(roster$Student[j], roster$ID[j], roster$Instructor[j], input$standardnew, 
                                                 input$weeknew, input$daynew, input$formatnew, t, '')
                               write.table(apt, 'app.csv', append = TRUE, 
                                           col.names = FALSE, row.names = FALSE, sep = ",")
                               if (length(unique(already$Standards)) == 1 & !(input$standardnew %in% unique(already$Standards))) {
                                 roster[j, which(names(roster) == "Tokens")] <- roster[j, which(names(roster) == "Tokens")] - 1
                                 write.table(roster, 'roster.csv',
                                             col.names = TRUE, row.names = FALSE, sep = ",")
                               }
                               shinyalert(msg_yeah, "Good Luck!", type = "success", html = TRUE)
                             }else {
                               shinyalert(error_no_match, type = "error", html = TRUE)
                             }
                             })
                }
              }
            }
          }
        }
      }
    }# end if id exists
    else{
      shinyalert("Sorry!", "Your MU ID is not found.", type = "error")
    }
  })
  
  ################################################
  ## My Current Appointment Panel
  ################################################
  userdata <- eventReactive(input$view, {
    roster <- read.csv('roster.csv' ,strip.white = TRUE)
    app <- read.csv('app.csv', strip.white = TRUE)
    if (input$idview %in% app$ID) {
      x <- app[which(app$ID == input$idview), ]
      return(x)
    }
  })
  
  output$view <- DT::renderDT({
    x <- userdata()
    
    datatable(x[, 4:8], rownames = FALSE, options = list(
      pageLength = 100,
      dom = "t",
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:1))))
  })
  
  
  # observeEvent(input$OutputFile, {
  #   msg_download <- HTML('You are about to download your retry question.')
  #   roster <- read.csv('roster.csv' ,strip.white = TRUE)
  #   if (input$idview %in% roster$ID) {
  #     shinyalert(msg_download, type = "input", html = TRUE,
  #                callbackR = function(pw) {
  #                  if (pw == roster[which(roster$ID == input$idview), which(names(roster) == "PIN")]) {
  #                    downloadHandler(
  #                      filename = "your-pdf-name.pdf",
  #                      content = function(file) {
  #                        file.copy("Presentation", file)
  #                      }
  #                    )
  #                  }else {}
  #                })
  #   }
  # })
  
  
  ################################################
  ## Cancel Appointment Panel
  ################################################
  observeEvent(input$cancel, {
    
    roster <- read.csv('roster.csv', strip.white = TRUE)
    app <- read.csv('app.csv', strip.white = TRUE)
    x <- app[which(app$ID == input$idcancel & app$Week == input$weekcancel & app$Day == input$daycancel & app$Standards == input$standardcancel), ]
    
    error_late_cancel <- HTML("<em>Sorry! Cancellation is only allowed before the appointment.</em>")
    error_pwnomatchcancel <-  HTML(paste0("<em>Cancellation Failed.<br>Please check your passcode.<br>", 
                                        "If you forgot it, please contact your instructor.</em>"))
    msg_cancel <- HTML(paste0("<em>You are about to cancel your appointment on week ",
                              x$Week, " ", x$Day, " ", x$Time, " ", x$Standards, ".<br>", 
                              "Please type in your passcode to proceed.</em>"))
    msg_confirmcancel <- HTML(paste0("<em>Your appointment on week ",
                                     x$Week, " ", x$Day, " ", x$Time, " ",x$Standards, " has now been canceled.</em>"))
    msg_nullcancel <- HTML("<em>There is nothing to cancel. Please double-check the information you filled in.</em>")
    
    
    
    if (nrow(x) == 1) {
      
      calendar <- read.csv('calendar.csv', strip.white = TRUE)
      canceldate <- calendar[which(calendar$Week == input$weekcancel & calendar$Day == input$daycancel), which(names(calendar) == 'Date')]
      
      if (tail(x, 1)$Time %in% format(seq(strptime("8:00 am", "%I:%M %p"), strptime("8:00 pm", "%I:%M %p"), by = 60))) {
        aheadoftime <- ((as.Date(canceldate) - Sys.Date()) >= 1 | (strptime(x$Time, "%I:%M %p") - Sys.time()) > 0)
      }else {
        aheadoftime <- (as.Date(canceldate) - Sys.Date()) >= 1
      }
      
      if (!aheadoftime) {
        shinyalert(error_late_cancel, type = "error", html = TRUE)
      }else{
        
        shinyalert(msg_cancel, type = "input", html = TRUE,
                   callbackR = function(pw) {
                     if (pw == roster[which(roster$ID == input$idcancel), which(names(roster) == "PIN")]) {
                       app <- read.csv('app.csv', strip.white = TRUE)
                       
                       j <- which(app$ID == input$idcancel & app$Week == input$weekcancel & app$Day == input$daycancel & app$Standards == input$standardcancel)
                       app <- app[-j, ]
                       write.table(app, 'app.csv', 
                                   col.names = TRUE, row.names = FALSE, sep = ",")
                       
                       
                       # Return token if eligible
                       s <- app[which(app$ID == input$idcancel & app$Week == input$weekcancel), which(names(app) == 'Standards')]
                       if (length(unique(s)) == 1) {
                         roster[which(roster$ID == input$idcancel), which(names(roster) == "Tokens")] <- roster[which(roster$ID == input$idcancel), which(names(roster) == "Tokens")] + 1
                         write.table(roster, 'roster.csv',
                                     col.names = TRUE, row.names = FALSE, sep = ",") 
                       }
                       shinyalert(msg_confirmcancel, type = "success", html = TRUE)
                     }else { shinyalert(error_pwnomatchcancel, type = "error", html = TRUE)}
                   })
        }
    }else {
      shinyalert(msg_nullcancel, type = "info", html = TRUE)
      }
  })
  
  ################################################
  ## Admin Panel
  ################################################
  admindata <- eventReactive(input$summary, {
    if (input$pwadmin == "1team")  {       
      app <- read.csv('app.csv', strip.white = TRUE)
      return(app)
    }else return(NULL)
  })
  
  
  output$apts <- DT::renderDT({
    x <- admindata()
    if (input$weekadmin != "") {w <- input$weekadmin} else {w <- 5:15}
    if (input$instructor != "") {i <- input$instructor} else {i <- c("MVP", "Symington", "Troupe")}
    if (input$dayadmin != "") {d <- input$dayadmin} else {d <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")}
    if (input$standardadmin != "") {s <- input$standardadmin} else {
      s <- c("L1", "L2", "L3", "LC",
             "D1", "D2", "D3", "D4", "D5",
             "A1", "A2", "A3", "AC",
             "I1", "I2", "I3", "I4", "IC")
    }
    show <- x[which(x$Week %in% w & x$Day %in% d & x$Instructor %in% i & x$Standards %in% s),]
    datatable(show, rownames = FALSE, options = list(
      pageLength = 100,
      dom = "t",
      autoWidth = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = 0:1))))
  })
  
  observeEvent(input$update, {
    roster <- read.csv('roster.csv', strip.white = TRUE)
    if (input$pwadmin == "1team" & input$idadmin %in% roster$ID)  {       
      app <- read.csv('app.csv', strip.white = TRUE)
      k <- which(app$ID == input$idadmin & app$Week == input$weekadmin & app$Day == input$dayadmin & app$Standards == input$standardadmin)
      
      if (identical(k, integer(0))) {
        shinyalert("No record found", type = "info")
      }else {
        if (input$actionadmin == 'result') {
          app[k, which(names(app) == "Result")] <- input$resultadmin
        }else {
          app[k, which(names(app) == "Time")] <- input$timeadmin
        }
        write.table(app, 'app.csv', 
                    col.names = TRUE, row.names = FALSE, sep = ",")
      }
    }else {
      shinyalert("No record found", type = "info")
    }
  })
})
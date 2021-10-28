SayHi <- function(name) {
  sprintf("Hi, %s", name);
}

SayHi("Dave")
choose.files(default = "", caption = "Select input data (.xlsx)", multi = FALSE)
require(tcltk)
msgBox <- tkmessageBox(title = "Title of message box",
                       message = "Hello, world!", icon = "info", type = "ok")

library(progress)
pb2 <- progress_bar$new(total = 10)
for (i in 1:10) {
  pb2$tick() #progress bar
  Sys.sleep(1 / 10)
  
}
print("Exit")


#subject <- "My message"
#message <- paste('CMD /C "ECHO',subject,'&& PAUSE"' )
#system(message, invisible=FALSE, wait=FALSE)
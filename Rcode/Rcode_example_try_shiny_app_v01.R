# This is a simple example of a Shiny app in R.

# https://shiny.posit.co/r/getstarted/shiny-basics/lesson1/
# Du kan også kigge efter inspiration her:
# https://shiny.posit.co/r/gallery/
  
#install.packages("shiny")
library(shiny)

# runExample("01_hello")      # a histogram
# runExample("02_text")       # tables and data frames
# runExample("03_reactivity") # a reactive expression
# runExample("04_mpg")        # global variables
# runExample("05_sliders")    # slider bars
# runExample("06_tabsets")    # tabbed panels
# runExample("07_widgets")    # help text and submit buttons
# runExample("08_html")       # Shiny app built from HTML
# runExample("09_upload")     # file upload wizard
# runExample("10_download")   # file download wizard
# runExample("11_timer")      # an automated timer

#
# Installing Necessary Packages
# install.packages("shiny")
# install.packages("shinythemes")
# install.packages("ggplot2")

# Loading the libraries
library(shiny)
library(shinythemes)
library(ggplot2)
# https://www.geeksforgeeks.org/create-an-interactive-web-app-using-shiny-package-in-r/

# AND https://mastering-shiny.org/basic-ui.html
ui <- fluidPage(
  textInput("name", "What's your name?"),
  passwordInput("password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3)
)
ui <- fluidPage(
  dateInput("dob", "When were you born?"),
  dateRangeInput("holiday", "When do you want to go on vacation next?")
)



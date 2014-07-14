library(shiny)

# Define UI for application that draws netflow
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Anomaly Detection"),
  
  # Sidebar with a slider input for the number of deviations
  sidebarLayout(position = "right", fluid = TRUE,
		#define the panel content, starting with standard method
		sidebarPanel(h2("Standard method"), 
		 	#define input slider
			sliderInput("devs","Number of standard deviations:",
				min = 1,
				max = 15,
				value = 4),
			br(),
			#print out ports, ips involved in any anomalies found
			"Unique ports ",
   		textOutput("plist"),
			"Unique IPs",
			textOutput("iplist"),
			br(),
		#define panel content relating to linear method
		h2("Linear method"),
			#define slider input, value = defaults
			sliderInput("dev2","Number of standard deviations:",
				min = 1,
				max = 15,
				value = 4),
			br(),
			#print out ports, ips involved in any anomalies found
			"Unique ports",
			textOutput("lplist"),
			"Unique IPs",
			textOutput("liplist")
		),
    
    # Show the plots, linear, standard methods
    mainPanel(
		h5("The graphs below show the current network detection levels. The top is Standard Deviation based, where the bottom is Linear Regression. "),	
      plotOutput("distPlot"),
      plotOutput("linearPlot")
    )
  )
))

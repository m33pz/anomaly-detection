#<anomaly detection system by m33p>
#Copyright (C) <2014> <m33p>
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.


library(shiny)

# Define server logic required to run standard method, linear method
shinyServer(function(input, output) {

  #find length of file
  n <- length(readLines('ipoutput.csv'))  
  #read in file minus 2 so as to avoid cut off lines
  DF <- read.table("ipoutput.csv",header=FALSE,nrows=n-2,sep=",")
  #file is read into columns, 1 is date, 8 is packet density, 7 packet size, and 6 is protocol, for this script to work you will want to change the columns to match your data. 5 is destination port, 4 is destination ip

  #import date into posix format
  dates <- strptime(as.character(DF$V1), "%b %d %Y %H:%M:%S")
  DF=data.frame(date=dates,DF)

  #create the model for linear method
  g <- lm(DF$V8 ~ DF$V7+DF$V6)
  
  #create the stdev plot
  output$distPlot <- renderPlot({

	#find the upper and lower bounds based on standard deviation for standard method
	upperbound <- mean(DF$V8,na.rm=TRUE)+input$devs*sd(DF$V8,na.rm=TRUE)
	lowerbound <- mean(DF$V8,na.rm=TRUE)-input$devs*sd(DF$V8,na.rm=TRUE)

  	#if the lower bound goes below 0, set it to 0
  	if (lowerbound < 0) lowerbound = 0

	#retrieve the ports and ips involved in the standard method
	unp <- unique(DF$V5[which(DF$V8 > upperbound | DF$V8 < lowerbound)])
	unip <- unique(DF$V4[which(DF$V8 > upperbound | DF$V8 < lowerbound)])

  	#format the output for printing
	output$plist <- renderPrint({cat(unp,sep="\n")}) 
	output$iplist <- renderPrint({cat(as.character(unip))})
    
	#create the plot
	plot(DF$date,DF$V8,type="l",xlab="date",ylab="packet density")
	#add in lines
    abline(h=mean(DF$V8,na.rm=TRUE),col="red")
    abline(h=lowerbound,col="blue")
    abline(h=upperbound,col="blue")
	#set the legend
    legend('topleft',c("Mean","deviations"),lty=c(1,1),col=c("red","blue"))
  })
  
  #create the linear plot
  output$linearPlot <- renderPlot({

		#find the upper & lower bounds of linear method
		lupper <- mean(resid(g))+input$dev2*sd(resid(g))
		llower <- mean(resid(g))-input$dev2*sd(resid(g))

		#retrieve the unique ips/ports for linear
		lunp <- unique(DF$V5[which(resid(g) > lupper | resid(g) < llower)])
		lunip <- unique(DF$V4[which(resid(g) > lupper | resid(g) < llower)])

 		#format the output for printing
 		output$lplist <- renderPrint({cat(lunp,sep="\n")})
		output$liplist <- renderPrint({cat(as.character(lunip),sep="\n")})
        
		#plot the linear data
		plot(DF$date,resid(g),xlab="date",ylab="residuals",type="l")
		abline(h=mean(resid(g)),col="red")
		abline(h=llower,col="blue")
		abline(h=lupper,col="blue")
		legend('topleft',c("Mean","deviations"),lty=c(1,1),col=c("red","blue"))
  })
})

library(plotly)
library(zoo)

ui <- fluidPage(

    titlePanel("[INSERT TITLE]"),

    inputPanel(
        sliderInput("rate", "Annual Interest Rate (%)", min = 1, max = 30,
                    value = 5, step = .5, animate = TRUE),
        sliderInput("years", "Mortgage Term (Years)", min = 10, max = 50,
                    value = 25, step = 1, animate = TRUE),
        sliderInput("loan", "Loan Amount", min = 0, max = 500000,
                    value = 200000, step = 20000, animate = TRUE)
    ),

    mainPanel(
        tabsetPanel(type = "pills",
            tabPanel(title = "Cumulative Principal", plotlyOutput("cumPrinOut")),
            tabPanel(title = "Percent of Home Owned", plotlyOutput("perOwnedOut")),
            tabPanel(title = "Monthly Interest Paid", plotlyOutput("intOut")),
            tabPanel(title = "Cumulative Interest", plotlyOutput("cumIntOut"))
        ),
        img(src="mi.png")#, height = 400, width = 400)
    )
)

server <- function(input, output) {

    dataInput <- reactive({

        ## use Girke's mortgage amortization from here
        ## source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R")
        ## Author: Thomas Girke
                                        #{{{
    ## Function definition from site:
### begin Girke Code -------
    mortgage <- function(P=500000, I=6, L=30, amort=T, plotData=T) { 
	J <- I/(12 * 100)
	N <- 12 * L
	M <- P*J/(1-(1+J)^(-N))
	monthPay <<- M
	cat("\nThe payments for this loan are:\n 
			Monthly payment: $", M, " (stored in monthPay)\n
			Total cost: $", M*N, "\n\n", sep="")
	# Calculate Amortization for each Month
	if(amort==T) {
		Pt <- P # current principal or amount of the loan
		currP <- NULL
		while(Pt>=0) {
			H <- Pt * J # this is the current monthly interest
			C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
			Q <- Pt - C # this is the new balance of your principal of your loan
			Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
			currP <- c(currP, Pt)
		}
		monthP <- c(P, currP[1:(length(currP)-1)])-currP
		aDFmonth <<- data.frame(
					      Amortization=c(P, currP[1:(length(currP)-1)]), 
					      Monthly_Payment=monthP+c((monthPay-monthP)[1:(length(monthP)-1)],0),
					      Monthly_Principal=monthP, 
					      Monthly_Interest=c((monthPay-monthP)[1:(length(monthP)-1)],0), 
					      Year=sort(rep(1:ceiling(N/12), 12))[1:length(monthP)]
				)
		aDFyear <- data.frame(
					     Amortization=tapply(aDFmonth$Amortization, aDFmonth$Year, max), 
					     Annual_Payment=tapply(aDFmonth$Monthly_Payment, aDFmonth$Year, sum), 
					     Annual_Principal=tapply(aDFmonth$Monthly_Principal, aDFmonth$Year, sum), 
					     Annual_Interest=tapply(aDFmonth$Monthly_Interest, aDFmonth$Year, sum), 
					     Year=as.vector(na.omit(unique(aDFmonth$Year)))
					     )
		aDFyear <<- aDFyear
		cat("The amortization data for each of the", N, "months are stored in \"aDFmonth\".\n\n")
		cat("The amortization data for each of the", L, "years are stored in \"aDFyear\".\n\n")
	}
	if(plotData==T) {
	barplot(t(aDFyear[,c(3,4)]), 
		col=c("blue", "red"), 
		main="Annual Interest and Principal Payments", 
		sub="The data for this plot is stored in aDFyear.",
		xlab="Years", ylab="$ Amount", 
		legend.text=c("Principal", "Interest"), 
		ylim=c(0, max(aDFyear$Annual_Payment)*1.3))
	}
}
# Definitions: 
#	  P = principal, the initial amount of the loan
#	  I = annual interest rate
#	  L = length of the loan in years, or at least the length over which the loan is amortized.
#	  J = monthly interest in decimal form = I / (12 x 100)
#	  M = monthly payment; formula: M = P * ( J / (1 - (1 + J) ^ -N))
#	  N = number of months over which loan is amortized = L x 12
    ## see also: http://www.jeacle.ie/mortgage/instructions.html
    #}}}

### end Girke Code -------
        
        mortgage(P = input$loan, I = input$rate, L = input$years, amort=T, plotData=F)

        months <- 1:(12 * input$years)
        cumPrin <- cumsum(aDFmonth$Monthly_Principal)
        cumPrin.zoo <- zoo(cumPrin, months)

        percentHomeOwned <- cumPrin / input$loan
        percentHomeOwned.zoo <- zoo(percentHomeOwned, months)

        monthlyInt <- aDFmonth$Monthly_Interest
        monthlyInt.zoo <- zoo(monthlyInt, months)
        
        cumInt <- cumsum(aDFmonth$Monthly_Interest)
        cumInt.zoo <- zoo(cumInt, months)

        ## do i use this anywhere anymore -- if not :delete:
        data2plot <- data.frame(cbind(months, cumPrin, percentHomeOwned, monthlyInt, cumInt))
        names(data2plot) <- c("Months", "Cumulative_Principal", "Percent_of_Home_Owned", "Monthly_Interest",  "Cumulative_Interest")
        
        ## Also calculate each for 15 and 30 year mortgages and add them to plot ----

        ## 15 year
        
        mortgage(P = input$loan, I = input$rate, L = 15, amort=T, plotData=F)

        months.15 <- 1:(12 * 15)
        cumPrin.15 <- cumsum(aDFmonth$Monthly_Principal)
        cumPrin.15.zoo <- zoo(cumPrin.15, months.15)

        percentHomeOwned.15 <- cumPrin.15 / input$loan
        percentHomeOwned.15.zoo <- zoo(percentHomeOwned.15, months.15)

        monthlyInt.15 <- aDFmonth$Monthly_Interest
        monthlyInt.15.zoo <- zoo(monthlyInt.15, months.15)
        
        cumInt.15 <- cumsum(aDFmonth$Monthly_Interest)
        cumInt.15.zoo <- zoo(cumInt.15, months.15)

        data2plot.15 <- data.frame(cbind(months.15, cumPrin.15, percentHomeOwned.15, monthlyInt.15, cumInt.15))
        names(data2plot.15) <- c("Months_15_Yr", "Cumulative_Principal_15_Yr", "Percent_of_Home_Owned_15_Yr", "Monthly_Interest_15_Yr",  "Cumulative_Interest_15_Yr")


        ## 30 year
        
        mortgage(P = input$loan, I = input$rate, L = 30, amort=T, plotData=F)

        months.30 <- 1:(12 * 30)
        cumPrin.30 <- cumsum(aDFmonth$Monthly_Principal)
        cumPrin.30.zoo <- zoo(cumPrin.30, months.30)

        percentHomeOwned.30 <- cumPrin.30 / input$loan
        percentHomeOwned.30.zoo <- zoo(percentHomeOwned.30, months.30)

        monthlyInt.30 <- aDFmonth$Monthly_Interest
        monthlyInt.30.zoo <- zoo(monthlyInt.30, months.30)
        
        cumInt.30 <- cumsum(aDFmonth$Monthly_Interest)
        cumInt.30.zoo <- zoo(cumInt.30, months.30)
        
        
        data2plot.30 <- data.frame(cbind(months.30, cumPrin.30, percentHomeOwned.30, monthlyInt.30, cumInt.30))
        names(data2plot.30) <- c("Months_30_Yr", "Cumulative_Principal_30_Yr", "Percent_of_Home_Owned_30_Yr", "Monthly_Interest_30_Yr",  "Cumulative_Interest_30_Yr")

        ## plot each metric with 15 and 30 year
        months <- zoo(1:(12 * max(30, input$years)), 1:(12 * max(30, input$years)))

### data to plot for cumulative principal
        data.cumPrin <- merge.zoo(cbind(months, cumPrin.zoo, cumPrin.15.zoo, cumPrin.30.zoo), all = TRUE, fill = NA)
        data.cumPrin.df <- data.frame(data.cumPrin)
        names(data.cumPrin.df) <- c("Months", "Cumulative_Principal", "Cumulative_Principal_15_Yr", "Cumulative_Principal_30_Yr")

        data.percentHomeOwned <- merge.zoo(cbind(months, percentHomeOwned.zoo, percentHomeOwned.15.zoo, percentHomeOwned.30.zoo), all = TRUE, fill = NA)
        data.percentHomeOwned.df <- data.frame(data.percentHomeOwned)
        names(data.percentHomeOwned.df) <- c("Months", "Percent_Home_Owned", "Percent_Home_Owned_15_Yr", "Percent_Home_Owned_30_Yr")


        data.monthyInt <- merge.zoo(cbind(months, monthlyInt.zoo, monthlyInt.15.zoo, monthlyInt.30.zoo), all = TRUE, fill = NA)
        data.monthlyInt.df <- data.frame(data.monthyInt)
        names(data.monthlyInt.df) <- c("Months", "Monthly_Interest", "Monthly_Interest_15_Yr", "Monthly_Interest_30_Yr")


        data.cumInt <- merge.zoo(cbind(months, cumInt.zoo, cumInt.15.zoo, cumInt.30.zoo), all = TRUE, fill = NA)
        data.cumInt.df <- data.frame(data.cumInt)
        names(data.cumInt.df) <- c("Months", "Cumulative_Interest", "Cumulative_Interest_15_Yr", "Cumulative_Interest_30_Yr")



### List of data to be used in plots below -----
        theData <- list(forCumPrin = data.cumPrin.df, years = input$years, months = months, forPercentHomeOwned = data.percentHomeOwned.df, forMonthlyInt = data.monthlyInt.df, forCumInt = data.cumInt.df)
        
    })

    output$cumPrinOut <- renderPlotly({
        theData <- dataInput()
        p <- plot_ly(
            ## x = monthRange,
            ## y = ratioPVCFLoan,
            data = theData$forCumPrin,
            x = ~Months,
            y = ~Cumulative_Principal,
            xaxis = "Month",
            type = "scatter",
            mode = "lines",
            name = paste0("Cumulative Principal, ", theData$years, " Yr"), 
            text = paste0("Cumulative Principal: Month ", theData$months)
        ) %>%
            add_trace(y = ~Cumulative_Principal_30_Yr, name = "Cumulative Principal, 30 Yr") %>%
            add_trace(y = ~Cumulative_Principal_15_Yr, name = "Cumulative Principal, 15 Yr")
        p
    })

        output$perOwnedOut <- renderPlotly({
        theData <- dataInput()
        p <- plot_ly(
            ## x = monthRange,
            ## y = ratioPVCFLoan,
            data = theData$forPercentHomeOwned,
            x = ~Months,
            y = ~Percent_Home_Owned,
            xaxis = "Month",
            type = "scatter",
            mode = "lines",
            name = paste0("Percent of Home Owned, ", theData$years, " Yr"), 
            text = paste0("Percent of Home Owned: Month ", theData$months)
        ) %>%
            add_trace(y = ~Percent_Home_Owned_30_Yr, name = "Percent of Home Owned, 30 Yr") %>%
            add_trace(y = ~Percent_Home_Owned_15_Yr, name = "Percent of Home Owned, 15 Yr")
        p
    })


    output$intOut <- renderPlotly({
        theData <- dataInput()
        p <- plot_ly(
            ## x = monthRange,
            ## y = ratioPVCFLoan,
            data = theData$forMonthlyInt,
            x = ~Months,
            y = ~Monthly_Interest,
            xaxis = "Month",
            type = "scatter",
            mode = "lines",
            name = paste0("Monthly Interest, ", theData$years, " Yr"), 
            text = paste0("Monthly Interest: Month ", theData$months)
        ) %>%
            add_trace(y = ~Monthly_Interest_30_Yr, name = "Monthly Interest, 30 Yr") %>%
            add_trace(y = ~Monthly_Interest_15_Yr, name = "Monthly Interest, 15 Yr")
        p
    })

    output$cumIntOut <- renderPlotly({
        theData <- dataInput()
        p <- plot_ly(
            ## x = monthRange,
            ## y = ratioPVCFLoan,
            data = theData$forCumInt,
            x = ~Months,
            y = ~Cumulative_Interest,
            xaxis = "Month",
            type = "scatter",
            mode = "lines",
            name = paste0("Cumulative Interest, ", theData$years, " Yr"), 
            text = paste0("Cumulative Interest: Month ", theData$months)
        ) %>%
            add_trace(y = ~Cumulative_Interest_30_Yr, name = "Cumulative Interest, 30 Yr") %>%
            add_trace(y = ~Cumulative_Interest_15_Yr, name = "Cumulative Interest, 15 Yr")
        p
    })

    
}


shinyApp(ui = ui, server = server)

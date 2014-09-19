shinyUI(
	fluidPage(
		title = "EuroMillions Statistics",

		fluidRow(
			column(3, h4("Euromillions Statistics"),
				sliderInput("from.year", "From Year:",
							min=2004, max=2014, value=2004),
				sliderInput("to.year", "To Year:",
							min=2004, max=2014, value=2014),
				br()
			),
			column(3, br(), br(),
				   selectInput("type", "Show:", choices = c('Main Numbers'="numbers", 'Lucky Stars'="stars")),
				   selectInput("yVar", "", choices = c('Frequency' = "freq", 'Relative Frequency' = "pr")),
				   br()
			),
			column(3, br(), br(),
				   selectInput("orderBy", "Order By:", choices = c('Number' = "number", 'Frequency' = "freq")),
				   selectInput("orderDecr", "", choices = c('Increasing' = "false", 'Decreasing' = "true")),
				   br()
			)

		),
		div(
		mainPanel(
			div(
				tabsetPanel(
					tabPanel("Plot", 	htmlOutput("barPlot")),
					tabPanel("Table", 	htmlOutput("table")),
					tabPanel("Help", 	htmlOutput("help"))
				), class = "span13")
		), class = "span14"),
		br(), br(), br(),
		fluidRow(column(2, h4(""), br(), br()))
	)
)
shinyServer(
	function(input, output) {
		yearFrom <- reactive({ as.character(input$from.year) })

		yearTo <- reactive({ as.character(input$to.year) })

		orderBy <- reactive({ input$orderBy })

		orderDecr <- reactive({ as.logical(input$orderDecr) })

		yVar <- reactive({ input$yVar })

		type <- reactive({ input$type })

		data <- reactive({
			if(type() == "stars") {
				data <- stars.year
			} else {
				data <- numbers.year
			}
			aggregateData(data, yearFrom(), yearTo(), orderBy(), orderDecr())
		})

		output$barPlot <- renderGvis(
			columnChart(data(), yVar(), type(), stacked = TRUE)
		)

		output$table <- renderGvis(
			tableChart(data(), orderBy(), orderDecr())
		)

		output$help <- renderUI(helpPage())

		#aggregate number occurrences in the interval given by [yearFrom, yearTo]
		#additionally, creates 'pr' column with the relative frequency of every number,
		#calculated as: frequency / number of draws
		aggregateData <- function(df, yearFrom, yearTo, orderBy="number", decr = F) {
			tmp.df <- df[df$year >= yearFrom & df$year <= yearTo,]
			counts <- cbind(tmp.df, freq = rep(1, nrow(tmp.df)))
			counts <- aggregate(freq ~ number, data = counts, FUN = sum)
			counts$pr <- rep(0, nrow(counts))
			totalDraws <- 0
			for(y in yearFrom : yearTo) {
				counts$tmp = df$freq[df$year == y]
				nm <- names(counts)
				nmLength <- length(nm)
				nm[nmLength] <- as.character(y)
				names(counts) <- nm
				totalDraws <- totalDraws + draws.year[[as.character(y)]]
			}

			counts$pr <- round(counts$freq / totalDraws, 5)
			if(orderBy == "freq") {
				rank <- order(counts$freq, counts$number, decreasing = decr)
				counts <- counts[rank,]
			} else if (decr){
				rank <- order(counts$number, decreasing = decr)
				counts <- counts[rank,]
			}
			#counts$mean <- rep(sum(counts$freq) / 50, nrow(counts))
			#counts$meanFreq <- rep(sum(counts$freq) / 50 / totalDraws, nrow(counts))
			counts
		}

		#build gVisColumnChart
		columnChart <- function(data, yvar = "freq", type = "numbers", stacked = FALSE) {
			if(stacked) {
				stacked = "true"
			} else {
				stacked = "false"
			}
			#configure options
			if (type == "stars") {
				xtitle <- "Stars"
				ytitle <- 'frequency'
				ymax <- 175
				ygrid <- 8
				charWidth <- 800
			} else {
				xtitle <- "Numbers"
				ytitle <- 'frequency'
				ymax <- 100
				ygrid <- 6
				charWidth <- 1300
			}

			if (yvar == "pr"){
				ytitle <- 'relative frequency'
				yvars <- c("pr")
				stacked = "false"
				if (type == "stars") {
					ymax <- 0.4
					ygrid <- 6
				} else {
					ymax <- 0.25
					ygrid <- 6
				}
			} else if (stacked) {
				yvars <- names(data[-c(1,2,3)])
			} else {
				yvars <- c("freq")
			}
			#call gvis api and create the chart
			gvisColumnChart(data, xvar = "number", yvar = yvars,
						options = list(
							height = 460,
							width = charWidth,
							chartArea = "{height: 400}",
							bar = "{groupWidth:'95%'}",
							fontSize = 12,
							legend = "{position: 'none'}",
							isStacked = stacked,
							vAxis = sprintf("{title: '%s',
											maxValue: %s,
											minValue: 0,
											viewWindow: { max: %s},
											gridlines: {count: %i}
									}", ytitle, ymax, ymax, ygrid),
							hAxis = sprintf("{ title: '%s',
										textStyle: {fontSize: 10},
										minTextSpacing: 5,
										maxAlternation: 1
									}", xtitle)
						))
		}

		tableChart <- function(data, orderBy = "number", decr = FALSE) {
			if (orderBy == "freq") {
				column <- 1
			} else {
				column <- 0
			}
			gvisTable(data(), options = list(
				pageSize = 50,
				sort = "enable",
				sortAscending = !decr,
				sortColumn = column))
		}

		helpPage <- function() {
			tags$html(
				tags$body(
					h3("About EuroMillions"),
					div(
					p("EuroMillions is a European lottery that takes place on Tuesday and Friday evenings. Whereas national lotteries are generally limited to the residents of one particular country, the EuroMillions lottery pools the stakes to create huge jackpots and prizes. "),
					p("The first EuroMillions lottery draw took place on Friday 13th February 2004. It was originally presented by three major lottery organisers: Camelot in the United Kingdom who operate the UK National Lottery, Francaise des Jeux in France and the Loterias y Apuestas des Estad in Spain. The first jackpot was worth €15 million. "),
					p("The game rules are: "),
					tags$ul(
						tags$li("Players must select five main numbers between 1 and 50."),
						tags$li(" Players must select two additional Lucky Star numbers between 1 and 11.")
					),
					p("On 10th May 2011, a Tuesday EuroMillions draw was launched (before there was only a Friday draw) and the numbers 10 and 11 were added to the Lucky Star ball pool. EuroMillions statistics displayed by this application might seem somehow strange or not coherent if these facts are not considered."),
					h3("About this app"),
					p("This application allows to visualize how often have been picked the numbers and also the Lucky Stars. In the tab panel, the user can choose between two different visualizations:  "),
					tags$ul(
						tags$li("Bar plot. When absolute frequencies are displayed, partial counts per year are also shown, stacked in the same bar."),
						tags$li("Data Table. The table allows to sort the data by clicking in the columns' headers.")
					),
					p("The dataset used by the application was built by myself scrapping a website where the winning numbers were availabe in HTML format. The data contains all draws up to and including 12th September 2014."),
					p("There are several controls in the top panel, which allow to filter sort and control the data displayed. These controls are described below:"),
					tags$ul(
						tags$li("Using the sliders 'From Year' and 'To Year' the dataset can be filtered to select only a certain interval."),
						tags$li("The select input below the label 'Show', is used to choose which statistics to show, whether main numbers (*numbers*) or Lucky Stars (*stars*)"),
						tags$li("The select input below the previous one, is used to choose whether to show absolute frequencies of the numbers in the selected period, or the realtive frequencies, i.e. frequencies divided by number of draws in the selected period."),
						tags$li("Last, the 'Order By' input allows to choose which column use to order the data: whether number or the frequency of the numbers.")
					)
					, class = "span11")
				)
			)
		}
	}
)
# ------------------------------------------------------------------
# AmeriFlux Sandboxing 3,000 site years
# An app to interact with AmeriFlux data from all sites without downloading

# August 22, 2022
# Sophie Ruehr
# sophie_ruehr@berkeley.edu
# ------------------------------------------------------------------


# 1. SET UP  -----
# Load libraries
library(dplyr)
library(ggplot2)
library(shiny)
library(patchwork)
library(plotly)
library(gapminder)
library(shinycssloaders)
library(readxl)
library(stringi)
library(shinythemes)
library(cowplot)
library(imager)
library(naniar)
library(GGally)
library(shinydashboard)
library(tidyr)


# Uncomment when running locally:
# setwd('/Users/sophieruehr/Documents/Academic/Berkeley/Projects/sandbox/August_attempt_6')

# 2. FUNCTIONS -----
get_data <- function(site1) { # Prep individual site data
  file <- paste0(site1, '.csv')
  output <- read.csv(file)
  output <- output %>% subset(select = -X) 
  output$date <- as.Date(output$date)
  return(output)
}

get_units <- function(dat) { # Get units for individual site data
  colnamesdat <- colnames(dat)
  shortnames <- sapply(strsplit(colnamesdat, split = "(_[0-9])"),'[',1)
  shortnames <- sapply(strsplit(shortnames, split = "_PI"),'[',1)
  unitsout <- data.frame(name = colnamesdat,
                         variable = shortnames)
  unitsout <- merge(units, unitsout, by = 'variable', all = T) %>% filter(!is.na(name))
  unitsout$unit[unitsout$variable=='FETCH'] <- 'm'
  return(unitsout)
}

# 3. LOAD DATA ----- 
# DOI information
dois <- read.csv('dois.csv')

# Site information
sites <- unique(dois$site)

# Summary statistics
summary_stats <- readRDS('summary_stats.RDS')

# Data from all sites 
all_sites <- read.csv('all_sites.csv')
all_sites <- all_sites %>% subset(select = -X)
all_sites$Date <- as.Date(all_sites$Date)

# Missing data from all sites
missing_data <- readRDS('missing_data_by_year.RDS')

# Variable units
units <- read.csv('units.csv')
nondim <- units$variable[which(units$unit == 'nondimensional')]
units_dim <- units %>% filter(unit != 'nondimensional')

# X variable names (all sites)
xvars <- c('Date', 'GPP', 'NEE', 'RECO',
                     'SWC', 'VPD', 'RH', 'USTAR', 'P','TA', 
                     'LE','H', 'NETRAD', 'G')
# Y variable names (all sites)
yvars <- c('GPP', 'NEE', 'RECO',
           'SWC', 'VPD', 'RH', 'USTAR', 'P','TA', 
           'LE','H', 'NETRAD', 'G')
# Grouping variables (all sites)
all_groups <- c('IGBP', 'Latitude', 'Longitude', 'Elevation', 'MAT', 'MAP')
all_groups_units <- c('', '(˚)', '(˚)', '(m)', '(˚C)', '(mm)')
names(all_groups_units) <- all_groups

# Load site data from first site when app initalizies 
site <- sites[1]
data <- get_data(site)

# 4. USER INTERFACE ----
ui <- dashboardPage(skin = 'black', # Begin UI 
                
                                   
        dashboardHeader(title = "AmeriFlux Visualization"),
                 
        dashboardSidebar(sidebarMenu(
             menuItem("Individual sites", tabName = "indiv"),
             menuItem("All sites", tabName = "all"),
             menuItem("About", tabName = "about")
         )), # End dashboard sidebar
        
         
   dashboardBody( # Begin bashboard body
     
     # Suppress warnings
     tags$style(type="text/css",
                ".shiny-output-error { visibility: hidden; }",
                ".shiny-output-error:before { visibility: hidden; }"),
   
     tabItems(
       tabItem( # Begin 'Individual Sites' page
         h1('Individual sites'),
         
         tabName = "indiv",
              # Select site from dropdown list
               selectInput('site', 'Select site', sites,
                           multiple = F),
         
               br(), br(), # Spacers
               
               # 1. Site information (main panel)
               
               # Site ID and name
               uiOutput("site_id",
                        style = "font-size:20px"),
               
               # Site DOI
               uiOutput("doisite",
                        style = "font-size:15px"),
               
               # Link to site page
               uiOutput("url",
                        style = "font-size:15px"),
         
               br(), br(),
               
               tabsetPanel( # Begin tab panels section
                 
                 tabPanel( # Begin time series tab
                   "Time series",
         
                   br(),
                   
                          
                          # Select main variable
                          fluidRow( column(2, selectInput('xcol', 'X-axis variable', names(data[-c(1)]))),
                                    
                          # Select date range for time series
                          column(10,  sliderInput(inputId = "range", width = '80%',
                                                  label = "Date range",
                                                  min = min(data$date, na.rm = T),
                                                  max = max(data$date, na.rm = T),
                                                  value = c(min(data$date, na.rm = T),
                                                            max(data$date, na.rm = T))))),
                          br(),
                          
                          # Ouput time series plot with a spinner
                                 shinycssloaders::withSpinner(plotlyOutput('timeseries_plots'),
                                                              type = getOption("spinner.type", default = 5),
                                                              color = getOption("spinner.color", default = "#4D90FE")),
                   h5(em('Plot shows daily average of half-hourly data.'), align = 'center')
                  
                        ), # End time series tab 
                 
                 
                 tabPanel( # Begin density and scatter plots tab 
                   "Density & scatter plots",
                          
                
                   br(),
                          
                          # Input first (X) variable for scatter plot
                          fluidRow(column(4, # Make both inputs on same row
                                          # Input first (Z) variable for scatter plot                               
                                          selectInput('zcol', 'First variable', names(data[-c(1)]))),
                                   column(4, 
                                          # Input second (Y) variable for scatter plot 
                                          selectInput('ycol', 'Second variable', names(data[-c(1:2)])))),
                          br(),
                          
                          # Output scatter and density plots
                          shinycssloaders::withSpinner(plotOutput('scatter_plots',
                                                                  width = '100%', height = '100%'),
                                                       type = getOption("spinner.type", default = 5)),
                   
                          inline = TRUE,
                   h5(em('Plots show daily averages of half-hourly data.'))
                   
                   ), # End density and scatter plots tab
                 
                 tabPanel( # Begin missing data tab
                   "Missing data",
                   
                   br(),

                          # Output missing data plot
                          shinycssloaders::withSpinner(plotlyOutput('missing_plots',
                                                                  width = '100%', height="100%"),
                                                       type = getOption("spinner.type", default = 5)),
                   h5(em('Colors represent missing data percentage for each year and each variable. 100% means all data are marked as NA.'), align = 'center')
                   
                   ), # End missing data tab
                 

                 tabPanel(# Begin summary statistics tab
                   "Summary statistics",
                   
              
                   
                   br(),
                          
                          # Output summary statistics for all variables at site
                          shinycssloaders::withSpinner(tableOutput("Summary_stats"),
                                                       type = getOption("spinner.type", default = 5),
                                                       color = getOption("spinner.color", default = "#4D90FE")),
                           tags$head(tags$style("#dummy table {background-color: red; }", media="screen", type="text/css")),
                   h5(em('Minima, maxima, IQRs, means, medians and missing data for all variables across all years.'))
                   
                   
                   )# End summary statistics tab
                 ) # End tab panels section
         ), # End 'Individual Sites' page
              
       tabItem( # Begin 'All Sites' page
         tabName = "all",
         h1('All sites'),
         
          # Select inputs for plot
          fluidRow( column(4, selectInput('xcol_all', 'X-axis variable', xvars)), 
                    column(4, selectInput('ycol_all', 'Y-axis variable', yvars)),
                    column(4, selectInput('groups_all', 'Group by', all_groups))),

          br(),
         
         # Output plot
         shinycssloaders::withSpinner(plotlyOutput('all_plots', 
                                                                               width = '100%',  height = '100%'),
                                                                  type = getOption("spinner.type", default = 5)),

          
          inline = TRUE,
         h5(em('Data are a random 2% sample from each site to reduce plotting times.'), align = 'center'),
        
         br()
       ), # End 'All Sites' page
       
       tabItem( # Begin 'About' page
         tabName = 'about',
         
          # Informational text 
          h4('About the data'),
          h5(style="text-align: justify;",
             'Data displayed with this tool were downloaded from the AmeriFlux data portal on August 26, 2022. Variables in "All Sites" plots were selected based on general interest from the community and represent the first sensor and first position of their kind appearing within the dataset. For example, if two relative humidity (RH) measurements were recorded at a flux tower, RH_1_1_1 (the first position and first sensor) will be chosen for the plots (instead of RH_1_2_1). All data were averaged to daily scales.'),
          h5(style="text-align: justify;",
             'Find variable information (including naming conventions and units) on the ', tags$a(href="https://ameriflux.lbl.gov/data/aboutdata/data-variables/", "AmeriFlux data variables page.", target="_blank")),
          
          br(),
          h4('About this visualization tool'),
          h5(style="text-align: justify;", 
             'This app was developed to celebrate 3,000 site years of AmeriFlux data. It is intended to be used only for initial data visualization and exploration to give users a better understanding of data availability before downloading. It is not intended for detailed analysis or network synthesis.'),
          h5(style="text-align: justify;",
             'If you are interested in learning more, improving this app, or building one yourself, check out the ', tags$a(href = 'https://shiny.rstudio.com/', 'RShiny package in R', target = 'blank'), ' or contact ', tags$a(href="https://sruehr.github.io/", "Sophie Ruehr.", target = 'blank'), 
             'Code for this app is available on ', tags$a(href = 'https://github.com/sruehr?tab=repositories', 'GitHub.', target = 'blank')), 
         br(),
         h4('Citation'),
         h5('If using any of these tools in publication or presentations, please acknowledge as "AmeriFlux Data Visualization Tool, Sophie Ruehr (2022), 10.5281/zenodo.7023749."'),
         
        
         br(),
          h4('Acknowledgements'),
          h5(style="text-align: justify;",
             'This application was developed by Sophie Ruehr with support from members of the AmeriFlux community and management team: Rachel Hollowgrass, Karla Leibowitz, Christin Buechner, Housen Chu, Trevor Keenan and Margaret Torn.')
         
         ) # End 'About' page
      ), # End all pages         
     hr(),
     h5('App designed and developed by Sophie Ruehr, 2022.')
  ) # End dashobard body
) # End UI


# 5. SERVER ----- 
server <- function(input, output, session) { # Begin server
  
  observeEvent(input$site, { # Change output based on site selection
    
    # Update site
    sitei <- which(sites == input$site) 
    # Update data 
    data <- get_data(input$site)
    # Update DOI
    doi <- dois$DOI[dois$site == input$site] 
    # Update site URL 
    url <- tags$a(href = dois$url[dois$site == input$site], 'Site link', target="_blank")
    # Update site name 
    sitename <- dois$sitename[dois$site == input$site]

    # Units
    dat_units <- get_units(data)
    
    # Summary stats
    summary <- summary_stats[[which(names(summary_stats)==input$site)]]
    summary <- as.data.frame(summary)
    dat_units_summary <- dat_units %>% subset(select = -variable)
    colnames(dat_units_summary) <- c('Units', 'Variable')
    summary <- merge(summary, dat_units_summary, by = 'Variable', all = T)
    summary <- summary[,c(1,10, 2,3,4,5,6,7,8,9)]
    summary <- summary[-which(summary$Variable == 'date'), ]

    # Missing data
    missingsm <- missing_data[[which(names(missing_data)==input$site)]]
    
    # Update X variable (time series)
    updateSelectInput(session, 'xcol', choices = names(data[-c(1)]))
    
    # Update Y variable (scatter plot Y variable)
    updateSelectInput(session, 'ycol', choices = names(data[,-c(1,2)]))
    
    # Update Z variable  (scatter plot X variable)
    updateSelectInput(session, 'zcol', choices = names(data[,-c(1)]))
    
    # Update time series limits
    updateSliderInput(inputId = "range",
                      min = min(data$date, na.rm = T),
                      max = max(data$date, na.rm = T),
                      value = c(min(data$date, na.rm = T),
                                max(data$date, na.rm = T)))
    
    # Select data for plots based on user inputs
    selectedData <- reactive({ # Time series plots
      data[, c(input$xcol, input$ycol)] 
    })
    
    selectedDatascatter<- reactive({ # Density and scatter plots
      data[, c(input$zcol, input$ycol)] 
    })
    
    # OUTPUTS:  ----- 
  
    # Site outputs
    output$doisite <-  renderText({paste('DOI:', doi)})
    output$site_id<- renderText({paste0(input$site, ': ', sitename)})
    output$url<- renderText({paste0(url)})

    # a) Time series plots
      output$timeseries_plots <- renderPlotly({ 
        dat_names <- names(selectedData())[1] # Get name of variable selected
        ylabel <- paste0(dat_names[1], ' (', dat_units$unit[which(dat_units$name == dat_names[1])], ')')
       
        Value <- selectedData()[,1] # Data from variable
        Date <- data$date # Date data
        p1 <- ggplot() +
          geom_line(aes(x = Date,
                        y = Value),
                    na.rm = T, col = '#B21B00') + # color of line
          theme_bw() + # plot theme
          theme(text=element_text(size=20), #change font size of all text
                axis.text=element_text(size=15), #change font size of axis text
                axis.title=element_text(size=12), #change font size of axis titles
                plot.title=element_text(size=20), #change font size of plot title
                legend.text=element_text(size=15), #change font size of legend text
                legend.title=element_text(size=15),
                plot.margin = margin(t = 20,  # Top margin
                                     r = 30,  # Right margin
                                     b = 30,  # Bottom margin
                                     l = 30)) + # Left margin +
          ylab(ylabel) +
          xlab('Date')  + # relabl X axis
         xlim(input$range[1], input$range[2]) # change date limits to user input 
        p1 <- ggplotly(p1) # create plotly 
      }) # End time series plots
      
      # b) Scatter and density plots
      output$scatter_plots <- renderPlot({
        varnames1 <- names(selectedDatascatter()[1])
        varnames2 <- names(selectedDatascatter()[2])
        
        label1 <- paste0(varnames1[1], ' (', dat_units$unit[which(dat_units$name == varnames1[1])], ')')
        label2 <- paste0(varnames2[1], ' (', dat_units$unit[which(dat_units$name == varnames2[1])], ')')
          
        title <- paste(label1, 'vs.', label2)
        
        corr_plots <- selectedDatascatter() %>% na.omit() %>% 
          ggpairs(# create scatter and density plots
                              lower = list(continuous = wrap("points", alpha = 0.2,    size=0.3, col = '#B21B00'), 
                                           combo = wrap("dot", alpha = 0.4,            size=0.3)),
                              upper =list(continuous = wrap("points", alpha = 0.2,    size=0.3, col = '#B21B00'), 
                                          combo = wrap("dot", alpha = 0.4,            size=0.3))) +
          theme_bw() + # change theme
          theme(axis.text.x = element_text(color = "black", size = 12), # update axis font 
                axis.text.y = element_text(color = "black", size = 12),
                strip.text = element_text(size=14),
                plot.title = element_text(size = 13, face = "bold")) +
          ggtitle(title)
        
        corr_plots # return plot
      }, height = 500, width = 600) # End scatter and density plots
      
      # c) Summary statistics table
      colnames(summary) <- c('Variable', 'Unit',	'Min.',	'1st.Qu',	'Median',	'Mean',	'3rd.Qu',	'Max', 'NAs', '%NA')
      output$Summary_stats <- renderTable({
        summary
      }) # End summary stats table
      
      # d) Missing data plots
      colnames(missingsm) <- c('Year', 'Variable', 'NAs')
      missingsm$Year <- as.factor(missingsm$Year)
      heightplot <- 20 * length(unique(missingsm$Variable))
      output$missing_plots <- renderPlotly({
        p3 <- ggplot(missingsm, aes(x = Year, y = Variable, fill = NAs)) +
          geom_raster() +
          xlab('') + ylab('') + labs(fill= '%NA') +
          scale_fill_gradient(high = 'red', low = 'blue',
                              limits=c(0, 100)) +
          theme_classic() +
          theme(axis.text.x = element_text(angle = 40))
        p3 <- ggplotly(p3, height = heightplot) 
      }) # End missing data plot
  }) # End reactive to site input
  
     # e) All sites plot
      output$all_plots <- renderPlotly({ 
        xlab <- paste(input$xcol_all)
        ylab <- paste(input$ycol_all)
        legend1 <- paste(input$groups_all)
        
        if(xlab != 'Date') {
          units1 <- units_dim$unit[which(sapply(strsplit(units_dim$variable,"_"),'[',1) == xlab)[1]] # Get units for variable selected
          xlab <- paste0(xlab, ' (', units1, ')') # Create Y-axis label with units
        }
        
        units2 <- units_dim$unit[which(sapply(strsplit(units_dim$variable,"_"),'[',1) == ylab)[1]] # Get units for variable selected
        ylab <- paste0(ylab, ' (', units2, ')') # Create Y-axis label with units
        
        p2 <- ggplot(all_sites) +
          geom_point(aes(x = .data[[input$xcol_all]],
                         y = .data[[input$ycol_all]],
                         col = .data[[input$groups_all]],
                         group = .data[['site']]),
                     na.rm = T, alpha = 0.3, size = 0.5) + # color of line
          theme_bw() + # plot theme
          theme(text=element_text(size=20), #change font size of all text
                axis.text=element_text(size=15), #change font size of axis text
                axis.title=element_text(size=15), #change font size of axis titles
                plot.title=element_text(size=20), #change font size of plot title
                legend.text=element_text(size=8), #change font size of legend text
                legend.title=element_text(size=10)) +
          xlab(xlab) +
          ylab(ylab)
        
        if (input$groups_all != 'IGBP') { # Change colorbar if grouping variable is numerical (e.g., MAP, longitude)
          label1 <- all_groups_units[which(legend1 == names(all_groups_units))]
          label1 <- paste(legend1, label1)
           p2 <- p2 + 
            scale_colour_gradient(high ='red', low = 'blue') +
            labs(col = label1) 
          }
       
        p2 <- ggplotly(p2) # create plotly 
    }) # End all sites plot
} # End server

# 6. RUN APP -----
shinyApp(ui = ui, server = server)




#######global.R########

## Libraries----

library(leaflet)
library(RPostgres)
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(plyr)
library(tidyverse)
library(stringi)
library(DBI)
library(ggthemes)
library(ggplot2)
library(scales)
library(htmltools)
library(DT)


#_create a remote connection to BMW database----


con2 = dbConnect(
  drv = dbDriver('Postgres'),
  dbname = 'bmw',
  host = 'xxxx',
  port = xxx,
  user = 'xxx',
  password = 'xxxxxx'
)

#######server.R########


server <- function(input, output, session) {
    
    # tab2: BMW sales per region----
    output$chrt1 <- renderPlot(
        {
            sql <- paste0(
                'SELECT sales_dt, COUNT(car_id)::numeric as total',
                ' FROM sales',
                ' JOIN dealers USING (dealer_id)',
                ' JOIN city USING (city_id)',
                ' JOIN subregions USING (subregion_id)',
                ' JOIN regions USING (region_id)',
                ' WHERE region LIKE \'', input$reg, '%\'',
                ' GROUP BY sales_dt',
                ' ORDER BY sales_dt ASC;'
            )
            z <- dbGetQuery(con2, sql)
            
     
            ggplot(z, aes(sales_dt, total)) +
              geom_line(na.rm=TRUE) +  
              ggtitle("Units per day") +
              xlab("Date") + ylab("Sales") +
              scale_x_date(labels=date_format("%b %y")) +
              theme(plot.title = element_text(lineheight=.8, face="bold", 
                                              size = 20)) +
              theme(text = element_text(size=18)) +
              geom_smooth() + theme_economist()
            
            
            
               }
    )
    
    # tab 3: Dealership Map----
    
    output$bmwMap <- renderLeaflet(
        {
            sql <- 'SELECT dealer, lat, lng
                    FROM dealers;'
            z <- dbGetQuery(con2, sql)
            z %>% 
                leaflet() %>% 
                addProviderTiles(
                    providers$Esri.WorldImagery
                ) %>%
                addMarkers(
                    label = z$dealer
                ) %>%
                setView(
                    lat = 37,
                    lng = -97,
                    zoom = 3
                )
        }
    )
    
    # tab4: Best Dealerships per region ----
    
    output$chrt4 <- renderLeaflet(
      {
        sql4 <- paste0(
          'SELECT COUNT(car_id) as total, dealer, lat, lng',
          ' FROM sales',
          ' JOIN dealers USING (dealer_id)',
          ' JOIN city USING (city_id)',
          ' JOIN subregions USING (subregion_id)',
          ' JOIN regions USING (region_id)',
          ' WHERE region LIKE \'', input$reg2, '%\'',
          ' GROUP BY dealer, lat, lng',
          ' ORDER BY total DESC',
          ' LIMIT 1;'
        )
        z4 <- dbGetQuery(con2, sql4)
        z4 %>% 
          leaflet() %>% 
          addProviderTiles(
            providers$Esri.WorldStreetMap
          ) %>%
          addMarkers(
            label = z4$dealer,
            labelOptions = labelOptions(textsize = "17px"),
            popup = ~htmlEscape(z4$total)
          ) %>%
          
          setView(
            lat = 37,
            lng = -97,
            zoom = 4
          )
        
      }
    )
    
    
    
    # tab 5: SQL Sandbox----
    
    # reactive variable to hold result of query run
    sq <- reactiveValues(out = NULL)
    
    # run query from SQL textbox when RUN button clicked
    observeEvent(
        eventExpr = input$run,
        {
            if (input$db == 1) {
                sq$out <- dbGetQuery(con2, input$sql)
            } else {
                sq$out <- dbGetQuery(con2, input$sql)
                }
        }
    )
    
    # render table from SQL run
    output$tbl <- renderDataTable(
        datatable(
            data = sq$out,
            options = list(scrollX = TRUE)
        )    
    )
    


    #tab 6:Sales per model----
    
    output$chrt3 <- renderPlot(
        {
            sql3 <- paste0(
                'with t as (
      select count(*) as count_models, car_id
      from sales
      group by car_id)
      select c.car_id, c.model, count_models from car_models c
      JOIN t on t.car_id = c.car_id 
      order by count_models desc
      limit 10;'
            )
            
            
      z3 <- dbGetQuery(con2, sql3)
      ggplot(z3, aes(x = reorder(model, -as.integer(count_models)), y=as.integer(count_models))) +
          geom_bar(stat = 'identity', width = 0.5, fill="steelblue") +
          scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
          labs(x = 'Car Models', y = 'Qty Sold') +
          theme_minimal(base_size = 16)
      
           
                
        }
    )
    

    
    ###tab 7:Top 10 region (Q2: Identifying georaphic regions)----
    
    output$chrt6 <- renderPlot(
      {
        sql5 <- paste0(
          "
    select a. model as car_model_name,region, count(sales_id) as sales_number
	  from (select car_id, model from car_models) as a  
	  join (select sales_id, dealer_id, sales_price, car_id,sales_dt
			from sales) as b on a.car_id = b.car_id
	  join dealers d on d.dealer_id=b.dealer_id 
	  join city i on i.city_id=d.city_id
	  join subregions s on s.subregion_id=i.subregion_id
	  join regions r on s.region_id=r.region_id	  
	  where a.model in (select model
			from(select c.car_id, c.model, count_models 
			from car_models as c
			 join t on t.car_id = c.car_id 
			 order by count_models desc
			 limit 10) as ttt) 
	  group by model, region
	  order by sales_number desc
	  limit 40

")
        
        z5 <- dbGetQuery(con2, sql5)
        df_cumsum <- ddply(z5, "car_model_name", transform,
                           label_ypos=cumsum((sales_number)) - 0.5*(sales_number))
        ggplot(data=df_cumsum, aes(x = reorder(car_model_name, -(sales_number)), y=as.integer(sales_number), fill=region)) +
          geom_bar(stat="identity",width = 0.5)+
          labs(x = 'Car Models', y = 'Qty Sold')+ scale_fill_brewer(palette="Paired")+ theme_minimal(base_size = 15)
        
      }
    )
    

    ###tab 8: Q5:dealer----
    
    output$chrt7 <- renderPlot(
      {
        sql6 <- paste0(
          "
    select a. model as car_model_name,market, count(sales_id) as sales_number
	  from (select car_id, model from car_models) as a  
	  join (select sales_id, dealer_id, sales_price, car_id,sales_dt
			from sales) as b on a.car_id = b.car_id
	  join dealers d on d.dealer_id=b.dealer_id 
	  join city i on i.city_id=d.city_id
	  join market m on m.market_id = i. market_id 
	  where a.model in (select model
			from(select c.car_id, c.model, count_models 
			from car_models as c
			 join t on t.car_id = c.car_id 
			 order by count_models desc
			 limit 10) as ttt) 
	  group by model, market
	  order by sales_number desc
	  limit 40
")
        
        z6 <- dbGetQuery(con2, sql6)
        df_cumsum <- ddply(z6, "car_model_name", transform,
                           label_ypos=cumsum((sales_number)) - 0.5*(sales_number))
        ggplot(data=df_cumsum, aes(x = reorder(car_model_name, -(sales_number)), y=as.integer(sales_number), fill=market)) +
          geom_bar(stat="identity",width = 0.5)+
          labs(x = 'Car Models', y = 'Qty Sold')+ scale_fill_brewer(palette="Paired")+ theme_minimal(base_size = 15)
        
      }
    )
    
    
    ##tab 9:Top 10 months (Q4 )----
    
    output$chrt8 <- renderPlot(
      
      {sql7 <- paste0(
        "select a.model as car_model_name, b.month, count(sales_id) as sales_number
	  from (select car_id, model from car_models) as a 
	  join (select sales_id, sales_price, car_id,sales_dt,
case    when sales_dt between '2019-01-01' and '2019-01-31' then 'January'
            when sales_dt between '2019-02-01' and '2019-02-28' then 'February'
            when sales_dt between '2019-03-01' and '2019-03-31' then 'March'
            when sales_dt between '2019-04-01' and '2019-04-30' then 'April'
            when sales_dt between '2019-05-01' and '2019-05-31' then 'May'
            when sales_dt between '2019-06-01' and '2019-06-30' then 'June'
            when sales_dt between '2019-07-01' and '2019-07-31' then 'July'
            when sales_dt between '2019-08-01' and '2019-08-31' then 'August'
            when sales_dt between '2019-09-01' and '2019-09-30' then 'September'
            when sales_dt between '2019-10-01' and '2019-10-31' then 'October'
            when sales_dt between '2019-11-01' and '2019-11-30' then 'November'
            when sales_dt between '2019-12-01' and '2019-12-31' then 'December'
			end as month
			from sales) as b on a.car_id = b.car_id
	  where a.model in (select model
			from(select c.car_id, c.model, count_models 
			from car_models as c
			 join t on t.car_id = c.car_id 
			 order by count_models desc
			 limit 10) as ttt) 
	  group by model, month
	  order by sales_number desc")
      
      z7 <- dbGetQuery(con2, sql7)
      
      df_cumsum <- ddply(z7, "car_model_name",
                         transform, 
                         label_ypos=cumsum(as.integer(sales_number)) - 0.5*as.integer(sales_number))
      
      gg = ggplot(data=df_cumsum, aes(x = reorder(car_model_name, -as.integer(sales_number)),      
                                      y=as.integer(sales_number), 
                                      fill=reorder(month, as.integer(sales_number)))) +
        geom_bar(stat="identity",width = 0.5)+
        
        
        labs(x = 'Car Models', y = 'Qty Sold')+
        scale_fill_brewer(palette="Paired")+
        theme_minimal(base_size = 15)
      gg + guides(fill=guide_legend(title="Months"))}
    )
    
    
    ## tab 10: top 10 income levels (Q2 income level)----
    
    output$chrt9 <- renderPlot(
      
      { sql8 <- paste0(
        "select count(a.cust_id) as population,c.model as model, 
  case   when income ilike ' 0-49999'  then '0-49999'
         when income ilike ' 50000-99999'  then '50000-99999'
         when income ilike ' 100000-149999'  then '100000-149999'
         when income ilike  ' 150000-199999' then '150000-199999'
         when income ilike ' 200000-249999'  then '200000-249999'
         when income ilike ' 250000-499999'  then '250000-499999'
         when income ilike ' 500000-999999'  then '500000-999999'
         when income ilike ' 1000000+'  then '1000000+'
     end as incomess
     from cust a 
     join sales b on a.cust_id=b.cust_id
     join car_models c on b.car_id=c.car_id
     where c.model in (select model
          from f)
     group by 2,3
     order by population desc"
      )
      
      z8 <- dbGetQuery(con2, sql8)
      
      
      ggplot(data=z8, aes(x = reorder(model, -as.integer(population)), y=as.integer(population), fill=incomess)) +
        geom_bar(stat="identity",width = 0.5)+
        labs(x = 'Car Models', y = 'Qty Sold')+ scale_fill_brewer(palette="Paired")+ theme_minimal(base_size = 15)
      
      }
      )
    
    
    
    # Final     
}


#######ui.R########

#Navigation window----

ui <- tagList(
    
    useShinyjs(),
    
    setBackgroundImage(src = 'M4_Gallery.jpg'),
    
    navbarPage(
        
        id = 'navBar',
        title = 'BMW App',
        windowTitle = 'BMW app',
        position = 'fixed-top',
        collapsible = TRUE,
        inverse = TRUE,
        theme = shinytheme('lumen'),
        
# Home ----
        
        tabPanel(
            title = div(
                img(src = 'BMW_logo_2.png', height = 25)
            ),
            tags$head(
                tags$style(
                    type = 'text/css', 
                    'body {padding-top: 70px;}' 
                )
            )
        ),
        
# tab1: Team----
        tabPanel(
            
            title = 'Our Team',
            
            div(
                class = 'outer',
                tags$head(includeCSS('www/styles.CSS')),
                h1('Board of Directors'),
                  fluidRow(
                  align = 'center',      
                  column(
                      width = 3,
                      wellPanel(
                          img(
                              src = '.jpeg',
                              height = '150px'
                          ),
                          h2(''),
                          h4('Business Analyst')
                      )
                  ),  
                  column(
                      width = 3,
                      wellPanel(
                      img(
                          src = '.jpg',
                          height = '150px'
                      ),
                          h2(''),
                          h4('Business Analyst')
                      )
                  ),
                  column(
                      width = 3,
                      wellPanel(
                      img(
                          src = '.jpg',
                          height = '150px'
                      ),
                          h2(''),
                          h4('Business Analyst')
                      )
                  ),
                  column(
                      width = 3,
                      wellPanel(
                      img(
                          src = '.jpg',
                          height = '150px'
                      ),
                          h2(''),
                          h4('Business Analyst')
                      )
                  ),
                  column(
                      width = 4,
                      wellPanel(
                      img(
                          src = '.jpeg',
                          height = '150px'
                      ),
                          h2(''),
                          h4('Project Manager')
                      )
                  ),
                  column(
                      width = 4,
                      wellPanel(
                      img(
                          src = '.jpeg',
                          height = '150px'
                      ),
                          h2(''),
                          h4('Business Analyst')
                      )
                  ),
                  column(
                      width = 4,
                      wellPanel(
                      img(
                          src = '.jpg',
                          height = '150px'
                      ),
                          h2(''),
                          h4('Business Analyst')
                      )
                  )
                  )
            )
            
        ),
        
# tab2: BMW sales per region---- 
        
        tabPanel(
            
            title = 'Sales per region',
            
            div(
                class = 'outer',
                tags$head(includeCSS('www/styles.CSS')),
                h1('BMW Sales per region'),
                fluidRow(
                    column(
                        width = 4,
                        align = 'center',
                        img(
                            src = 'BMW_logo_2.png',
                            height = '150px'
                        ),
                        pickerInput(
                            inputId = 'reg',
                            label = 'Sales per Region',
                            choices = c(
                                'ALL' = '',
                                'CENTRAL' = 'C',
                                'EASTERN' = 'E',
                                'SOUTHERN' = 'S',
                                'WESTERN' = 'W'
                            )
                        )
                    ),
                    column(
                        width = 8,
                        plotOutput(
                            outputId = 'chrt1',
                            height = '500px'
                        )
                )
            )
        )
    ),
        
    
# tab 3: Dealership Map----
        
        tabPanel(
            
            title = 'Dealerships Map',
            div(
                class = 'outer',
                tags$head(includeCSS('www/styles.CSS')),
                leafletOutput(
                    outputId = 'bmwMap',
                    width = '100%',
                    height = '100%'
                )
            )
        ),
    
# tab4: Best Dealerships per region ----
    
    tabPanel(
        
        title = 'Best dealership per region',
        
        div(
            class = 'outer',
            tags$head(includeCSS('www/styles.CSS')),
            h1('Dealer of the Year'),
            fluidRow(
                column(
                    width = 4,
                    align = 'center',
                    img(
                        src = 'dship_award.png',
                        height = '150px'
                    ),
                    pickerInput(
                        inputId = 'reg2',
                        label = 'Region',
                        choices = c(
                            'ALL' = '',
                            'CENTRAL' = 'C',
                            'EASTERN' = 'E',
                            'SOUTHERN' = 'S',
                            'WESTERN' = 'W'
                        )
                    )
                ),
                column(
                    width = 8,
                    leafletOutput(
                        outputId = 'chrt4',
                        height = '500px'
                    )
                )
            )
        )
    ), # end of the Panel
    
        
#tab 5: SQL Sandbox----
        
        tabPanel(
            
            title = 'SQL Sandbox',
            div(
                class = 'outer',
                tags$head(includeCSS('www/styles.CSS')),
                dropdownButton(
                    h3('Login'),
                    passwordInput(
                        inputId = 'pwd', 
                        label = 'Admin Password:'
                    ),
                    label = 'LOGIN',
                    circle = FALSE, 
                    status = 'danger',
                    icon = icon('gear'), 
                    width = '300px',
                    tooltip = tooltipOptions(title = 'Click to Login')
                ),
                
                tags$head(
                    tags$style(
                        HTML('textArea {font-family: Courier;
                 font-size: 36px;
                 font-weight: bold;}'
                        )
                    )
                ),
                conditionalPanel(
                    'input.pwd == 123', 
                    wellPanel(
                        fluidRow(
                            column(
                                width = 5,
                                fluidRow(
                                    column(
                                        width = 6,
                                        h5('SQL')
                                    ),
                                    column(
                                        width = 6,
                                        align = 'right',
                                        actionBttn(
                                            inputId = 'run',
                                            label = 'RUN', 
                                            style = 'jelly',
                                            color = 'success',
                                            size = 'sm'
                                        )
                                    )
                                ),
                                textAreaInput(
                                    inputId = 'sql', 
                                    label = NULL, 
                                    value = '',
                                    width = '100%', 
                                    height = '300px',
                                    resize = 'vertical'
                                ),
                                pickerInput(
                                    inputId = 'db',
                                    label = 'Database',
                                    choices = c('team6_bmw' = 1)
                                )
                            ),
                            column(
                                width = 7,
                                dataTableOutput('tbl')
                            )
                        )
                    )
                )
            )
        ),
    
    
#tab 6:Sales per model----
    
    tabPanel(
        
        title = 'Sales per model',
        plotOutput(
            outputId = 'chrt3',
            width = '100%',
            height = '580px'
        )
    ), # end of the Panel
    
###tab 7:Top 10 region (Q2: Identifying georaphic regions)----
    
    tabPanel(
        
        title = 'Top10 Region',
        plotOutput(
            outputId = 'chrt6',
            width = '100%',
            height = '580px'
        )
    ),  # end of the Panel
    
###tab 8: Q5:dealer----
    
    tabPanel(
        
        title = 'Top10 Market',
        plotOutput(
            outputId = 'chrt7',
            width = '100%',
            height = '580px'
        )
    ),  # end of the Panel
    
##tab 9:Top 10 months (Q4 )----
    
    tabPanel(
        
        title = 'Top10 Month',
        plotOutput(
            outputId = 'chrt8',
            width = '100%',
            height = '580px'
        )
    ),  # end of the Panel
    
## tab 10: top 10 income levels (Q2 income level)----
    
    tabPanel(
        
        title = 'Top10 Income',
        plotOutput(
            outputId = 'chrt9',
            width = '100%',
            height = '580px'
        )
    )  # end of the Panel
    
    
    
    )  
)
 









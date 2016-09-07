library(shiny)
library(RODBC)
library(scales)
library(plyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(shinydashboard)
library(DT)

# Store names as they appear in Sage 
storeNames <- c('Careline Medical Supplies', 'Health Products for You','Zulily Inc', 
                'AMAZON.COM','CVS.com/TPF','Drugstore.com','Medbarn.com','Shoebuy.com, Inc.',
                'The Betty Mills Company','UnbeatableSale Inc','Walgreens Co.',
                'Walmart','eBay','Groupon Goods', 'AMAZON FBA-MEDBARN', 'Amazon.com', 'Target.com',
                'AMAZON DROPSHIP') 
# Store labels for plotting and selection convenience
storeLabels <- c('Careline', 'HPY','Zul', 
                 'AMZ-Med','CVS','Drugst','Medbarn','Shoebuy',
                 'BettyMills','Unbeat','Walgr',
                 'Walmrt','eBay','Grpon', 'AMZ-FBA', 'AMZ-WS', 
                 'Target', 'AMZ-DS')

# Read table that helps to translate between Sage ItemCodes and Main Build product names
productInfo <- read.csv('~/RCode/sales_dashboard/product_info.csv')


# Definition of web user interface
ui <- dashboardPage(title="Salesa",
  dashboardHeader(title = "Salesa"),
  dashboardSidebar(
   
    sidebarMenu(
      menuItem("Today's Sales", tabName = "allOfToday", icon = icon("dashboard"),
               collapsable = menuSubItem('Now', tabName = 'today', selected=TRUE, icon=icon("clock-o")),
               collapsable = menuSubItem('Details', tabName = 'details', icon = icon("bars"))),
      #menuItem("In Details", tabName = "details", icon = icon("bars")),
      menuItem("Store Performance", tabName = "analysis", icon = icon("line-chart")),
      menuItem("Product Popularity", tabName='bestsellers', icon=icon("signal")),
      menuItem("Inventory", tabName='inventory', icon=icon("database"),
               collapsable = menuSubItem('Inventory Count', tabName = 'lowInventory', icon=icon('bolt')),
               collapsable = menuSubItem('Inventory Planning', tabName = 'inventoryPlanning', icon=icon('battery-full')))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # Sales as they happen right now
      tabItem(tabName = "today",
              fluidRow(valueBoxOutput("totalOrdersValueBox"),
                       valueBoxOutput("totalItemsValueBox"),
                       valueBoxOutput("totalSalesAmountValueBox")
              ),
              
              fluidRow(
                box(plotOutput("salesByItemsPlot"),  solidHeader = TRUE, title="Units sold", status="warning"),
                
                box(
                  title = "Summary",
                  solidHeader = TRUE, 
                  status = 'warning',
                  tableOutput("todaysSales"),
                  hr(),
                  textOutput("currentTime")
                )
              )
              
      ),
      
      # Sold items (detailed) tab content
      tabItem(tabName = "details",
              h3("Sold Items"),
              #box(tableOutput("todaysSalesDetails")),
              DT::dataTableOutput("todaysSalesDetailsDT", width = "900px"),
              
              h3("Returns"), 
              DT::dataTableOutput("todaysReturnsDetailsDT", width = "900px")
      ), 
      
      # Performance analysis tab content
      tabItem(tabName = "analysis",
              fluidRow(
                column(9,
                       box(width=NULL, status='warning',
                           plotOutput("salesHistory", height="650px")
                       )),  
                column(3,
                       #br(),
                       box(width=NULL, status='warning',
                           selectizeInput("storeNamesSelect", "Select stores (default is today's stores)", 
                                          choices=storeNames, 
                                          multiple=TRUE),
                           dateRangeInput('dateRange',
                                          label = 'Date range: yyyy-mm-dd',
                                          start = Sys.Date()-14, end =NULL, width = "80%"),
                           fluidRow(
                             
                             column(5,
                                    radioButtons("radioPerfDisplay", "Count:",
                                                 c("Pieces" = "perfShowPieces",
                                                   "$$$" = "perfShowRevenue"),
                                                 selected = "perfShowPieces",
                                                 
                                    )
                             ),
                             
                             column(7,
                                    radioButtons("inRadio", "Sum by:",
                                                 c("Same Day of Wk" = "sameDayOfWeek",
                                                   "Daily" = "dailyOption",
                                                   "Weekly" = "weeklyOption",
                                                   "Monthy" = "monthlyOption",
                                                   "Yearly" = "yearlyOption"),
                                                 selected = "dailyOption"))
                             
                           ),
                           checkboxInput('combineShops', 'Combine stores', value = TRUE),
                           checkboxInput('independentScales', 'Unique y-axis', value=TRUE), 
                           #checkboxInput('smoother', 'Show trends'),
                           submitButton("  Go  ")
                           
                       )
                )
              )
      ),
      # Analysis of bestseller products
      tabItem(tabName='bestsellers',
              fluidRow(
                column(9,
                       box(width=NULL, status='warning', title = "Units sold", solidHeader = TRUE,
                           plotOutput("bestsellersPlot", height="650px")
                       )
                ),
                column(3,
                       box(width=NULL, status='warning', title = "Settings", solidHeader = TRUE,
                           selectizeInput("storeNamesSelectBestSell", "Select stores (defaults to all stores)", 
                                          selected="All Stores (online+offline)",
                                          choices=c("All Stores (online+offline)", storeNames), 
                                          multiple=TRUE),
                           p(
                             class = "text-muted",
                             paste("Note: 'All Stores' inlcudes both online and offline accounts.",
                                   "Unselect it if you want to analyse separate stores."
                             )
                           ),
                           dateRangeInput('dateRangeBestSell',
                                          label = 'Date range: yyyy-mm-dd',
                                          start = Sys.Date()-30, end = NULL, width = "77%"),
                           numericInput("nBestsellers", "N top sellers", 10,
                                        min = 1, max = 50, width="38%"),
                           radioButtons("radioBestSellers", "Show:",
                                        c("By Product (all variations)"="byStyleOption",
                                          "By Variation"="byItemOption"
                                        ),
                                        selected = "byItemOption"
                           ),
                           checkboxInput('splitByBrands', 'Split into brands', value = FALSE),
                           #checkboxInput('downloadData', 'Download data', value = FALSE),
                           submitButton(" Go "),
                           br(),
                           hr(),
                           p(class='text-muted',
                             'Before downloading, press \'Go\' first or else wrong results will be downloaded'),
                           downloadButton('downloadData', 'Download')
                       )
                )
              )
              #              fluidRow(
              #                column(3,
              #                        box(width=NULL, status='warning'
              #                            #uiOutput('topSeller1')
              #                      )
              #                ),
              #                column(3,
              #
              #                  box(width=NULL, status="warning", title = "Top seller 1", solidHeader = TRUE,
              #                    imageOutput("topSeller2", height='150px')
              #                    )
              #                   #imageOutput("topSeller3", height="10%", width='10%')
              #                  )
              #            )
      ),
      tabItem(tabName='lowInventory', 
              fluidRow(
                column(9,
                       box(width=NULL, status='warning', title = "Inventory Count", solidHeader = TRUE,
                           p(class='text-muted', "By default, sorted by the number of Available items. Click on a column header, to sort by that column."),
                           DT::dataTableOutput('lowInventoryDataTable')
                       )
                ),
                column(3,
                       box(width=NULL, status='warning', title = "Options", solidHeader = TRUE,
                           #checkboxInput('excludeInHouse', 'Exclude In-House Products', value = FALSE),
                           radioButtons("inventoryFilterRadio", "Filter Data", 
                                        c("Show all"="all", "In-House (Lena)"="inhouse", "Other vendors"="outsourced")),
                           hr(),
                           checkboxInput("invSalesVelCheckbox", 'Show sales velocity', value = FALSE),
                           selectInput("invTimePeriodSelect", "Choose past period for velocity", choices = c("30 days", "60 days", "90 days", "120 days", "180 days", "360 days")),
                           submitButton(" Go ")
                           # h3('Inventory Count'),
                           # p(class='text-muted',
                           # 'By default, sorted by the number of Available items. Click on a column header, to sort by that column.'),
                           # DT::dataTableOutput('lowInventoryDataTable', width="900px")
                       )
                )
              )  
      )
    )
  )
)

#)

server <- function(input, output, session) {
  
  # Transfer this function into sage100 library
  # and rename it into GetSoldItems
  ReadHistoricalData <- function(backInTime, selectedStores) {
    
    #browser()
    # If no store is selected, all stores will be included, both e-commerce and brick and mortar
    if (("All Stores (online+offline)" %in% selectedStores) | (is.null(selectedStores))) salesHistQuery <- paste0("SELECT t1.BillToName, t1.InvoiceNo, t1.InvoiceDate, t2.ItemCode, t2.ProductLine, t2.ItemCodeDesc, t2.QuantityOrdered, t2.UnitPrice, t2.ExtensionAmt
                                                                                                                  FROM AR_InvoiceHistoryHeader t1, AR_InvoiceHistoryDetail t2
                                                                                                                  WHERE t1.InvoiceNo = t2.InvoiceNo
                                                                                                                  AND t1.HeaderSeqNo = t2.HeaderSeqNo")
    else salesHistQuery <- paste0("SELECT t1.BillToName, t1.InvoiceNo, t1.InvoiceDate, t2.ItemCode, t2.ProductLine, t2.ItemCodeDesc, t2.QuantityOrdered, t2.UnitPrice, t2.ExtensionAmt
                                  FROM AR_InvoiceHistoryHeader t1, AR_InvoiceHistoryDetail t2
                                  WHERE t1.InvoiceNo = t2.InvoiceNo
                                  AND t1.HeaderSeqNo = t2.HeaderSeqNo
                                  AND (t1.BillToName In ('", paste0(selectedStores, collapse="','"), 
                                  "'))")
    
    #print(selectedStores)
    #if (is.null(selectedStores)) browser()
    #backInTime <- ymd(Sys.Date()) - days(360+4*30) # To get data one week before
    salesHistQueryWithDate <- paste0(salesHistQuery, " AND t1.InvoiceDate>={d'", backInTime,"'}")
    #print(salesHistQueryWithDate)
    salesHistory <-  sqlQuery(conn,
                              salesHistQueryWithDate,
                              believeNRows = FALSE,
                              rows_at_time = 1)
    #print(salesHistQueryWithDate)
    print(paste0("# of rows in salesHistory: ", nrow(salesHistory)))
    #browser()
    return(salesHistory)
    #odbcClose(conn)
  }
  
  ReadHistoricalDataReactive <- reactive({
    if (is.null(input$storeNamesSelect)) whichStoresToAnalyse <- byStore$BillToName
    else whichStoresToAnalyse <- input$storeNamesSelect
    print("*** In ReadHistoricalDataReactive ***")
    print(input$dateRange[1])
    print(input$storeNamesSelect)
    #print(output$storeNamesSelect)
    ReadHistoricalData(input$dateRange[1], whichStoresToAnalyse)
  })
  
  ### WORKING HERE
  ReadInventoryCountReactive <- reactive({
    if (!exists('inventoryCount')) {
      inventoryCount <- sqlQuery(conn, "SELECT 
                                 t1.ItemCode, 
                                 t1.RoutingNo, 
                                 t2.WarehouseCode, 
                                 t2.QuantityOnHand, 
                                 t2.QuantityOnSalesOrder, 
                                 t2.QuantityOnBackOrder, 
                                 t2.QuantityOnPurchaseOrder  
                                 FROM 
                                 CI_Item t1, 
                                 IM_ItemWarehouse t2
                                 WHERE t1.ItemCode = t2.ItemCode
                                 AND t2.WarehouseCode = '000'  
                                 ORDER BY t2.WarehouseCode")
      # Compute remaining inventory 
      inventoryCount <- mutate(inventoryCount, RemainingQuantity=QuantityOnHand - QuantityOnSalesOrder - QuantityOnBackOrder)
      # Merge the ItemCode with ProductInfo. This will eliminate Items which are old
      # and are 0 just because they are no longer produced. 
      activeInventory <- merge(inventoryCount, productInfo, by = 'ItemCode')
      # Select only relevant columns
      activeInventory <- activeInventory[, c('ItemCode', 'Style','Vendor', 'Simplified_Brand', 'RoutingNo', 'QuantityOnHand', 
                                             'QuantityOnSalesOrder','QuantityOnBackOrder',
                                             'QuantityOnPurchaseOrder','RemainingQuantity')]
      # Tidier column names
      colnames(activeInventory) <- c('SageSKU', 'Style', 'Vendor', 'Brand', 'RoutingNo',
                                     'OnHand', 'OnSalesOrder', 'OnBackOrder',
                                     'OnPurchaseOrder', 'Remaining')
      # Sort so that items with lowest remaining stock are shown first
      activeInventory <- arrange(activeInventory, RoutingNo)
      #browser()
      return(activeInventory)
    }
  })
  
  
  
  
  connToSage <- function() { odbcConnect("SOTAMAS90", uid = 'arman|ITA', pwd = 'polarena') }
  #if (exists("conn")) print(conn)
  if (!exists("conn"))
    conn <- connToSage() else 
      if (!RODBC:::odbcValidChannel(conn))
        conn <-  connToSage()
  
  todaySalesData <- sqlQuery(conn, paste0("SELECT SO_InvoiceHeader.BillToName,
                                          SO_InvoiceHeader.InvoiceNo,
                                          SO_InvoiceHeader.InvoiceDate,
                                          SO_InvoiceDetail.ItemCode,
                                          SO_InvoiceDetail.ProductLine,
                                          SO_InvoiceDetail.ItemCodeDesc,
                                          SO_InvoiceDetail.QuantityOrdered,
                                          SO_InvoiceDetail.UnitPrice,
                                          SO_InvoiceDetail.ExtensionAmt
                                          FROM SO_InvoiceHeader SO_InvoiceHeader, SO_InvoiceDetail SO_InvoiceDetail
                                          WHERE SO_InvoiceHeader.InvoiceNo = SO_InvoiceDetail.InvoiceNo
                                          AND (SO_InvoiceHeader.BillToName In ('", paste0(storeNames, collapse="','"), 
                                          "')) ORDER BY SO_InvoiceHeader.BillToName, SO_InvoiceHeader.InvoiceNo, SO_InvoiceDetail.ItemCode"),
                             believeNRows=FALSE, rows_at_time=1)
  # Remove items from other days (this can happen when an old batch wasn't closed)
  todaySalesData <- subset(todaySalesData, InvoiceDate == Sys.Date())
  # If the day's batch was close, read from history
  print(todaySalesData)
  if (!nrow(todaySalesData)) {
    todaySalesData <- ReadHistoricalData(Sys.Date(), storeNames)
    todaySalesData <- subset(todaySalesData, InvoiceDate == Sys.Date())
    print("******* Reading historical data for today *********")  
  }
  
  # Tidy up data types
  todaySalesData$InvoiceDate <- format(todaySalesData$InvoiceDate,'%Y-%m-%d')
  todaySalesData$QuantityOrdered  <- as.integer(todaySalesData$QuantityOrdered)
  #salesHistory$InvoiceDate <- format(salesHistory$InvoiceDate,'%Y-%m-%d')
  
  #browser()
  sales <- droplevels(subset(todaySalesData, QuantityOrdered >= 0)) # Positive numbers indicate sold items
  returns <- droplevels(subset(todaySalesData, QuantityOrdered < 0)) # Negative numbers indicate returns
  
  byStore <- ddply(sales, .(BillToName), summarise, TotalOrders=length(unique(InvoiceNo)), TotalItems=sum(QuantityOrdered), SalesAmount=sum(ExtensionAmt))
  
  # Compute total accross columns
  tots <- with(byStore, data.frame(BillToName = "<strong> Total </strong>", 
                                   TotalOrders = sum(TotalOrders),
                                   TotalItems = sum(TotalItems),
                                   SalesAmount = sum(SalesAmount)))
  
  output$todaysSales <- renderTable({
    sortedDat <- byStore[order(-byStore$TotalOrders),]
    #sortedDat <- rbind(sortedDat, tots)
    
  }, sanitize.text.function=function(x){x})
  
  output$todaysReturnsDetails <- renderTable({
    returns[,c("BillToName", "InvoiceDate", "InvoiceNo", "ItemCode", "ItemCodeDesc", "QuantityOrdered", "UnitPrice")]
  })
  
  # Returns in DataTable format
  output$todaysReturnsDetailsDT <- renderDataTable({
    datatable(returns[,c("BillToName", "InvoiceDate", "InvoiceNo", "ItemCode", "ItemCodeDesc", "QuantityOrdered", "UnitPrice")],
              rownames=FALSE, colnames=c("Store","Date","Invoice #", "Code", "Description", "N Piece", "Price"), 
              options = list(pageLength = 150), 
              class = 'table-condensed')
  })
  
  #  output$todaysSalesDetails <- renderTable({
  #    sales[,c("BillToName", "InvoiceDate", "InvoiceNo", "ItemCode", "ItemCodeDesc", "QuantityOrdered", "UnitPrice")]
  #  })
  
  # Sales by item in DataTable format
  output$todaysSalesDetailsDT <- renderDataTable({
    datatable(sales[,c("BillToName", "InvoiceDate", "InvoiceNo", "ItemCode", "ItemCodeDesc", "QuantityOrdered", "UnitPrice")],
              rownames=FALSE, colnames=c("Store","Date","Invoice #", "Code", "Description", "N Piece", "Price"), 
              options = list(pageLength = 150, info=TRUE), 
              class = 'table-condensed')
    #datatable(sales[,c("BillToName", "InvoiceDate", "InvoiceNo", "ItemCode", "ItemCodeDesc", "QuantityOrdered", "UnitPrice")])
    #print(sales[,c("BillToName", "InvoiceDate", "InvoiceNo", "ItemCode", "ItemCodeDesc", "QuantityOrdered", "UnitPrice")])
  })
  
  #})
  
  output$salesByItemsPlot <- renderPlot({
    byStore$BillToName <- factor(byStore$BillToName, levels = byStore$BillToName[order(-byStore$TotalItems)])
    #print(paste0("Current Day: ", Sys.Date(), " ***"))
    #print(paste0("Current time: ", Sys.time(), " ***"))
    print(paste0("*** IP: ", session$clientData$url_hostname, " ***"))
    
    ggplot(byStore, aes(x=BillToName, y = TotalItems, fill=BillToName)) + 
      theme_wsj() + 
      #ggtitle("") + 
      xlab("") + 
      ylab("Number of items") + 
      expand_limits(y=0) + 
      scale_x_discrete(breaks=storeNames, labels=storeLabels) + 
      scale_y_continuous(breaks=pretty_breaks(7)) +
      #geom_line(aes(group=1), color='grey50') + 
      theme(axis.text.x = element_text(size=11), legend.position="none") + 
      #coord_flip() + 
      geom_bar(stat='sum', width=.8) 
    
  })
  # Sort by amount sold 
  output$salesByAmountPlot <- renderPlot({
    byStore$BillToName <- factor(byStore$BillToName, levels = byStore$BillToName[order(-byStore$SalesAmount)])
    
    ggplot(byStore, aes(x=BillToName, y = SalesAmount)) + 
      theme_wsj() + 
      ggtitle("Sales Amount ($)") + 
      xlab("Store") + ylab("Number of orders") + 
      scale_y_continuous(breaks=pretty_breaks(6)) +
      geom_line(aes(group=1), color='grey50') + 
      coord_flip() + 
      geom_point(size=3) 
    
  })
  
  output$salesHistory <- renderPlot({
    #print(exists('salesHistoryDat'))
    progress <- Progress$new(session, min=1, max=5)
    on.exit(progress$close())
    progress$set(message="Analysis in progress", detail = "Getting Sage data")
    progress$set(value=2)
    
    if (is.null(input$storeNamesSelect)) {
      whichStoresToAnalyse <- byStore$BillToName        
      updateSelectizeInput(session, 'storeNamesSelect', choices = storeNames, selected = byStore$BillToName, server = TRUE)
    } else whichStoresToAnalyse <- input$storeNamesSelect
    
    # if (!exists('salesHistoryDat')) salesHistoryDat <- ReadHistoricalData(input$dateRange[1], whichStoresToAnalyse)
    # if (!exists('salesHistoryDat')) salesHistoryDat <- ReadHistoricalData(input$dateRange[1], whichStoresToAnalyse) 
    # else { browser() 
    #   salesHistoryDat <- ReadHistoricalDataReactive()
    # }
    #debug(ReadHistoricalDataReactive)
    salesHistoryDat <- ReadHistoricalDataReactive()
    
    
    #browser()
    # Add today's data if requested 
    if (input$dateRange[2] >= Sys.Date()) {
      # Load today's day only if today's data isn't in the history table yet
      # When the day is over, today's data goes into history table and 
      # ReadHistoricalData will have that data already, so no need to join the same 
      # data twice
      if (!(Sys.Date() %in% salesHistoryDat$InvoiceDate )) {
        salesHistoryDat <- rbind(salesHistoryDat, subset(sales, BillToName %in% whichStoresToAnalyse))
        print("TODAY IS INCLUDED")
      }
    }
    progress$set(value=4)
    
    #browser()
    salesHistoryDat <- subset(salesHistoryDat, InvoiceDate <=input$dateRange[2] & QuantityOrdered > 0) # Upper limit of date and select only non-return items
    
    #browser()
    salesHistoryDat$Day <- as.Date(cut(salesHistoryDat$InvoiceDate, breaks = "1 day"))
    salesHistoryDat$Month <- as.Date(cut(salesHistoryDat$InvoiceDate, breaks = "1 month"))
    salesHistoryDat$Week <- as.Date(cut(salesHistoryDat$InvoiceDate, breaks = "1 week"))
    salesHistoryDat$Year <- as.Date(cut(salesHistoryDat$InvoiceDate, breaks = "1 year"))
    salesHistoryDat$Weekday <- wday(salesHistoryDat$InvoiceDate, label = TRUE)
    dayOfWeek <- wday(today(), label = TRUE)
    
    if (input$inRadio=='sameDayOfWeek') gg <- ggplot(subset(salesHistoryDat, Weekday==dayOfWeek)) + ggtitle(paste("Same day in the past:", dayOfWeek))
    else  gg <- ggplot(salesHistoryDat) 
    
    if (input$radioPerfDisplay == 'perfShowPieces') {
      yAxisName = 'QuantityOrdered'
      gg <- gg + ggtitle("Sold Pieces")
    }
    if (input$radioPerfDisplay == 'perfShowRevenue') {
      yAxisName = 'ExtensionAmt'
      gg <- gg + ggtitle("Revenue($$)") + scale_y_continuous(labels = scales::dollar)
    }
    
    if (input$inRadio=='sameDayOfWeek') gg <- gg + aes_string(x='Day', y=yAxisName) 
    if (input$inRadio=='dailyOption') gg <- gg + aes_string(x='InvoiceDate', y=yAxisName)
    if (input$inRadio=='monthlyOption') gg <- gg + aes_string(x='Month', y=yAxisName) #+ geom_text(aes_string(label = yAxisName, x = 'Month'), size = 3)
    if (input$inRadio=='weeklyOption') gg <- gg + aes_string(x='Week', y=yAxisName)
    if (input$inRadio=='yearlyOption') gg <- gg + aes_string(x='Year', y=yAxisName) 
    
    gg <- gg + stat_summary(fun.y=sum, geom='point', size=3) + 
      theme_wsj() +
      stat_summary(fun.y=sum, geom='line', aes(group=1))
    # if (input$smoother) {
    #   gg <- gg + stat_smooth(se=FALSE)
    #   print("smoothing")
    # }
    if (!input$combineShops) {
      if (input$independentScales) gg <- gg + facet_wrap(~BillToName, scales = "free_y") + aes(color=BillToName) + theme(legend.position='none')
      else gg <- gg + facet_wrap(~BillToName) + aes(color=BillToName) + theme(legend.position='none')
    }
    gg <- gg + expand_limits(y=0)
    progress$set(value=5)
    print(gg)
    #browser()
    
  })
  
  output$currentTime <- renderText({
    #invalidateLater(1000, session)
    paste("Generated on", Sys.time())
  })  
  
  output$totalOrdersValueBox <- renderValueBox({
    valueBox(
      tots$TotalOrders, "New Orders", icon = icon("th-list"), color = "purple"
    ) 
  })
  
  output$totalItemsValueBox <- renderValueBox({
    valueBox(
      tots$TotalItems, "Sold Pieces", icon = icon("list"),
      color = "teal"
    )
  })
  
  output$totalSalesAmountValueBox <- renderValueBox({
    valueBox(
      paste0(tots$SalesAmount), "Sales Amount", icon = icon("dollar"),
      color = "olive"
    )
  })
  
  ## Compute and plot bestselling products
  output$bestsellersPlot <- renderPlot({
    storesForBestSell <- input$storeNamesSelectBestSell
    if (!exists('salesHistForBestSell')) salesHistForBestSell <- ReadHistoricalData(input$dateRangeBestSell[1], storesForBestSell)
    # Positive numbers are sales, negative numbers are returns. We keep only sales. 
    salesHistForBestSell <- subset(salesHistForBestSell, QuantityOrdered > 0)
    
    # Merge with table that helps to translate Sage ItemCodes into Main Build product names
    salesHistForBestSell <- merge(salesHistForBestSell, productInfo, by="ItemCode")
    
    # Compute bestsellers by Product (combine all variations of size and color)
    if (input$radioBestSellers=='byStyleOption') {
      #browser()
      salesHistSum <- ddply(salesHistForBestSell, .(Simplified_Brand, Style), summarise, UnitsSold = sum(QuantityOrdered))
      salesHistSum$Style <- factor(salesHistSum$Style, levels = salesHistSum$Style[order(-salesHistSum$UnitsSold)])
      salesHistSum <- salesHistSum[order(-salesHistSum$UnitsSold),]
      salesHistSumToPlot <- salesHistSum[1:input$nBestsellers,]
      #print(head(salesHistSum))
      thePlot <- ggplot(salesHistSumToPlot, aes(x=Style, y=UnitsSold, fill=Style)) + 
        theme_wsj() + 
        #ggtitle("Pieces Sold") + 
        #coord_flip() + 
        #facet_wrap(~Simplified_Brand, scales='free_x') + 
        theme(axis.text.x = element_text(angle=90, size=12), legend.position='none') + 
        geom_bar(stat='identity') 
      if (input$splitByBrands) thePlot <- thePlot + facet_wrap(~Simplified_Brand, scales='free_x')
    }
    
    # Compute bestseller by SKU (broken down into size, color, etc)
    if (input$radioBestSellers=='byItemOption') {
      salesHistSum <- ddply(salesHistForBestSell, .(Simplified_Brand, ItemCode), summarise, UnitsSold = sum(QuantityOrdered))
      salesHistSum$ItemCode <- factor(salesHistSum$ItemCode, levels = salesHistSum$ItemCode[order(-salesHistSum$UnitsSold)])
      salesHistSum <- salesHistSum[order(-salesHistSum$UnitsSold),]
      salesHistSumToPlot <- salesHistSum[1:input$nBestsellers,]
      #print(head(salesHistSum))
      thePlot <- ggplot(salesHistSumToPlot, aes(x=ItemCode, y=UnitsSold, fill=ItemCode)) + 
        theme_wsj() + 
        #ggtitle("Pieces Sold") + 
        #coord_flip() + 
        #facet_wrap(~Simplified_Brand, scales='free_x') + 
        theme(axis.text.x = element_text(angle=90, size=12), legend.position='none') + 
        geom_bar(stat='identity') 
      if (input$splitByBrands) thePlot <- thePlot + facet_wrap(~Simplified_Brand, scales='free_x')
      
      
    }
    
    # Compute speed of sales of the product
    # First, get number of days included in the period
    salesHistSum$PeriodInDays <- as.numeric(input$dateRangeBestSell[2] - input$dateRangeBestSell[1])
    # Calculate speed of product sales, which is the total number of sold units divided by the period
    # which is done for each item
    #browser()
    # Index of column that shows either ItemCode or Style (depending which type of output is requested)
    colIx <- which(colnames(salesHistSum) %in% c("Style", "ItemCode"))
    salesHistSum <- ddply(salesHistSum, colIx, transform, UnitsSoldPerDay=round(UnitsSold/PeriodInDays,4))
    
    # Number of days for forecasting
    forecastDays <- 90
    # Compute expected sales for number of days asked in forecastDays
    salesHistSum$ExpectedSales <- ceiling(salesHistSum$UnitsSoldPerDay * forecastDays)
    
    if (input$radioBestSellers=='byItemOption') {
      # Get inventory count to see how many items remain 
      invCount <- ReadInventoryCountReactive()
      # Have only ItemCodes and number of remaining items
      #browser()
      invCount <- invCount[,c( "RoutingNo", "SageSKU", "Vendor","Remaining", "OnPurchaseOrder")]
      colnames(invCount) <- c("RoutingNo", "ItemCode",  "Vendor", "Total", "PurchaseOrder")
      # Merge inventory count with salesHistSum
      #browser()
      salesHistSum <- merge(salesHistSum, invCount)
      salesHistSum <- ddply(salesHistSum, .(ItemCode), transform, StockAfterXDays=Total + PurchaseOrder - ExpectedSales)
      # Move Vendor column to the front
      ixCols <- which(colnames(salesHistSum) %in% c("RoutingNo", "Vendor"))
      salesHistSum <- salesHistSum[, c("RoutingNo", "Vendor", colnames(salesHistSum)[-ixCols])]
      ## CONTINUE FROM HERE
      # CHANGE NAME OF STOCKAFTERXDAYS into STOCKAFTER'forecastDays'DAYS
      stockAfterColName <- paste0("StockAfter", forecastDays, "Days")
      #print(stockAfterColName)
      colnames(salesHistSum)[colnames(salesHistSum)=="StockAfterXDays"] <- stockAfterColName
      salesHistSum <- arrange(salesHistSum, RoutingNo)
      
      # plyr::rename(salesHistSum, c("StockAfterXDays"=stockAfterColName))
    }
    
    #filename <- 'bestsellers.csv'
    #write.csv(salesHistSum, filename)
    output$downloadData <- downloadHandler(
      filename = function() {paste0('ProductSales-', Sys.Date(), '.csv')},
      content = function(file) {
        datToWrite <- salesHistSum[,-which(colnames(salesHistSum)=="ExpectedSales")]
        write.csv(datToWrite, file, row.names=FALSE)
      }
    )
    return(thePlot)
    
  })

  # Data table of items 
  output$lowInventoryDataTable <- renderDataTable({
    inventory <- ReadInventoryCountReactive()
    if (input$inventoryFilterRadio=="inhouse") inventory <- droplevels(subset(inventory, Vendor=="ITA, Lena"))
    if (input$inventoryFilterRadio=="outsourced") inventory <- droplevels(subset(inventory, Vendor!="ITA, Lena"))
    
    # Names of columns for display in the datatable
    columnNames <- c('Routing #', 'Item Code', 'Style', 'Brand', 'On Hands',
                     'Sales Order', 'Back Order', 'Production Order', 
                     'Available')
    
    # Compute sales velocity for the past 180 days
    if (input$invSalesVelCheckbox) {
      daysBack <- switch(input$invTimePeriodSelect,
                         "30 days" = 30,
                         "60 days" = 60,
                         "90 days" = 90,
                         "120 days" = 120,
                         "180 days" = 180,
                         "360 days" = 360)
      backInTime <- Sys.Date() - daysBack
      salesHist <- ReadHistoricalData(backInTime, selectedStore=NULL)
      # Exclude returns
      salesHist <- subset(salesHist, QuantityOrdered > 0)
      # Units sold during the period
      salesHistSum <- ddply(salesHist, .(ItemCode), summarise, UnitsSold = sum(QuantityOrdered))
      # Sales speed - units sold each day
      salesHistSum$UnitsSoldPerDay <- round(salesHistSum$UnitsSold / daysBack, 4)
      # Compute expected sales for number of days asked in forecastDays
      forecastDays <- daysBack
      salesHistSum$ExpectedSales <- ceiling(salesHistSum$UnitsSoldPerDay * forecastDays)
      colnames(salesHistSum)[names(salesHistSum)=="ItemCode"] <- c("SageSKU")
      # Merge inventory count and sales speed
      inventory <- merge(inventory, salesHistSum, all.x=TRUE) # left join includes items that were not sold during the period
      # Set NA values to 0, which means items were not sold during that period
      inventory$UnitsSold[is.na(inventory$UnitsSold)] <- 0
      inventory$UnitsSoldPerDay[is.na(inventory$UnitsSoldPerDay)] <- 0
      inventory$ExpectedSales[is.na(inventory$ExpectedSales)] <- 0
      #browser()
      # Expected number of items remaining in stock after X days, where X equals to the period in past for which
      # sales velocity was computed. This is convenient because helps to see predictions for different time periods 
      # and for different vendors
      inventory <- ddply(inventory, .(SageSKU), transform, StockAfterXDays=Remaining + OnPurchaseOrder - ExpectedSales)
      #browser()
      
      # Add name of sales velocity column to the list of datatable column titles
      # and predicted stock column name
      remainingStockColName <- paste0("Stock after ", daysBack, " days")
      columnNames <- c(columnNames, "Units Sold", remainingStockColName)
      #browser()
    } 
    
    
    toShow <- inventory[, -which(names(inventory) %in% c("Vendor", "UnitsSoldPerDay", "ExpectedSales"))]
    # Move RoutingNo column to the front
    ixCols <- which(colnames(toShow) %in% c("RoutingNo"))
    toShow <- toShow[, c("RoutingNo", colnames(toShow)[-ixCols])]
    # Find items that will be out of stock after X number of days
    # those rows will be highlighted in the datable
    if (input$invSalesVelCheckbox) {
      lowStockItems <- which((toShow$StockAfterXDays < 4) & (toShow$UnitsSold >0))
    } else {
      lowStockItems <- NULL
    }
    #browser()
    datatable(toShow,
              options = list(pageLength = 300, info=TRUE, search=list(regex=TRUE)),
              selection = list(mode="multiple", selected=lowStockItems, target="row"),
              rownames = FALSE, 
              colnames=columnNames)
    
  })
  
  
  output$topSeller1 <- renderUI({
    #images <- c("http://www.i2symbol.com/images/abc-123/o/white_smiling_face_u263A_icon_256x256.png"
    #      , "http://www.clipartbest.com/cliparts/yco/GGE/ycoGGEacE.png")
    image <- "http://www.itamed.com/Images/Itamed/CC-240AO/CC-240AO_SizeChart.jpg"
    tags$img(src= image)
    
  })
  
  output$topSeller2 <- renderImage({
    library(jpeg)
    myurl <- "http://cdn1.bigcommerce.com/server1200/a5834/images/stencil/1280x1280/products/232/1915/412_White_M_NEW__42659.1470353666.jpg"
    z <- tempfile()
    download.file(myurl,z,mode="wb")
    pic <- readJPEG(z)
    file.remove(z)
    
    
    width  <- session$clientData$output_topSeller2_width
    height <- session$clientData$output_topSeller2_height
    pixelratio <- session$clientData$pixelratio
    print(width)
    print(height)
    outfile <- tempfile()
    jpeg(outfile, width=width*pixelratio, height=height*pixelratio,
         res=72*pixelratio)
    par(mar = c(0,0,0,0))
    plot(as.raster(pic))
    dev.off()
    #CONTINUE FROM HERE
    #pic <- readJPEG(z)
    #file.remove(z) # cleanup
    
    #image <- "http://www.itamed.com/Images/Itamed/CC-240AO/CC-240AO_SizeChart.jpg"
    list(src = outfile,
         contentType = 'image/jpeg',
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
  output$topSeller3 <- renderImage({
    library(jpeg)
    myurl <- "http://cdn1.bigcommerce.com/server1200/a5834/images/stencil/1280x1280/products/313/1440/H-160_Black_NEW__45040.1470086257.jpg"
    z <- tempfile()
    download.file(myurl,z,mode="wb")
    #pic <- readJPEG(z)
    #file.remove(z) # cleanup
    
    #image <- "http://www.itamed.com/Images/Itamed/CC-240AO/CC-240AO_SizeChart.jpg"
    list(src = z,
         contentType = 'image/jpeg',
         alt = "This is alternate text")
  }, deleteFile = TRUE)
  
  
}

shinyApp(ui, server)


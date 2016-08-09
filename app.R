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
                'Walmart','eBay','Groupon Goods', 'AMAZON FBA-MEDBARN', 'Amazon.com') 
# Store labels for plotting and selection convenience
storeLabels <- c('Careline', 'HPY','Zul', 
                 'AMZ-DS','CVS','Drugst','Medbarn','Shoebuy',
                 'BettyMills','Unbeat','Walgr',
                 'Walmrt','eBay','Groupon', 'AMZ-FBA', 'AMZ-WS')

# Read table that helps to translate between Sage ItemCodes and Main Build product names
productInfo <- read.csv('~/RCode/sales_dashboard/product_info.csv')


# Definition of web user interface
ui <- dashboardPage(
  dashboardHeader(title = "ITA-MED Co."),
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
                box(plotOutput("salesByItemsPlot")),
                
                box(
                  title = "Summary",
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
                           submitButton("History")
                           
                       )
                )
              )
      ),
      # Analysis of bestseller products
      tabItem(tabName='bestsellers',
              fluidRow(
                column(9,
                       box(width=NULL, status='warning',
                           plotOutput("bestsellersPlot", height="650px")
                       )
                ),
                column(3,
                       box(width=NULL, status='warning',
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
                                        c("By Product"="byStyleOption",
                                          "By SKU"="byItemOption"
                                        ),
                                        selected = "byStyleOption"
                           ),
                           checkboxInput('splitByBrands', 'Split into brands', value = FALSE),
                           #checkboxInput('downloadData', 'Download data', value = FALSE),
                           submitButton("Pieces Sold"),
                           br(),
                           hr(),
                           p(class='text-muted',
                             'To First press \'Sold Pieces\' then \'Download\' or else wrong results will be downloaded'),
                           downloadButton('downloadData', 'Download')
                           
                           
                       )
                )
                
              )
              
      ),
      tabItem(tabName='lowInventory', 
              h3('Inventory Count'),
              p(class='text-muted',
                'By default, sorted by the number of Available items. Click on a column header, to sort by that column.'),
              DT::dataTableOutput('lowInventoryDataTable', width="900px")
      )
      
    )
  )  
)
#)

server <- function(input, output, session) {
  
  
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
    
    print(selectedStores)
    #if (is.null(selectedStores)) browser()
    #backInTime <- ymd(Sys.Date()) - days(360+4*30) # To get data one week before
    salesHistQueryWithDate <- paste0(salesHistQuery, " AND t1.InvoiceDate>={d'", backInTime,"'}")
    #print(salesHistQueryWithDate)
    salesHistory <-  sqlQuery(conn,
                              salesHistQueryWithDate,
                              believeNRows = FALSE,
                              rows_at_time = 1)
    print(salesHistQueryWithDate)
    print(paste0("# of rows in salesHistory: ", nrow(salesHistory)))
    return(salesHistory)
    #odbcClose(conn)
  }
  
  ReadHistoricalDataReactive <- reactive({
    #browser()
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
      activeInventory <- activeInventory[, c('ItemCode', 'Style','Vendor', 'Simplified_Brand', 'QuantityOnHand', 
                                             'QuantityOnSalesOrder','QuantityOnBackOrder',
                                             'QuantityOnPurchaseOrder','RemainingQuantity')]
      # Tidier column names
      colnames(activeInventory) <- c('SageSKU', 'Style', 'Vendor', 'Brand', 
                                     'OnHand', 'OnSalesOrder', 'OnBackOrder',
                                     'OnPurchaseOrder', 'Remaining')
      # Sort so that items which are 0 are shown first
      activeInventory <- arrange(activeInventory, Remaining, Brand)
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
  # If the day's batch was close, read from history
  if (nrow(todaySalesData)==0) todaySalesData <- ReadHistoricalData(Sys.Date(), storeNames)
  # Remove items from the other days (this can happen when the batch wasn't closed)
  todaySalesData <- subset(todaySalesData, InvoiceDate == Sys.Date())
  
  
  #print(head(salesHistory,5))
  #print(tail(salesHistory,5))
  
  # Tidy up data types
  todaySalesData$InvoiceDate <- format(todaySalesData$InvoiceDate,'%Y-%m-%d')
  todaySalesData$QuantityOrdered	<- as.integer(todaySalesData$QuantityOrdered)
  #salesHistory$InvoiceDate <- format(salesHistory$InvoiceDate,'%Y-%m-%d')
  
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
    print(paste0("Current Day: ", Sys.Date(), " ***"))
    print(paste0("Current time: ", Sys.time(), " ***"))
    print(paste0("*** IP: ", session$clientData$url_hostname, " ***"))
    
    ggplot(byStore, aes(x=BillToName, y = TotalItems, fill=BillToName)) + 
      theme_wsj() + 
      ggtitle("Sold Pieces") + 
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
    
    # Compute bestsellers by Product (combine SKUs of size and color)
    if (input$radioBestSellers=='byStyleOption') {
      #browser()
      salesHistSum <- ddply(salesHistForBestSell, .(Simplified_Brand, Style), summarise, TotalOrders = sum(QuantityOrdered))
      salesHistSum$Style <- factor(salesHistSum$Style, levels = salesHistSum$Style[order(-salesHistSum$TotalOrders)])
      salesHistSum <- salesHistSum[order(-salesHistSum$TotalOrders),]
      salesHistSumToPlot <- salesHistSum[1:input$nBestsellers,]
      #print(head(salesHistSum))
      thePlot <- ggplot(salesHistSumToPlot, aes(x=Style, y=TotalOrders, fill=Style)) + 
        theme_wsj() + 
        ggtitle("Pieces Sold") + 
        #coord_flip() + 
        #facet_wrap(~Simplified_Brand, scales='free_x') + 
        theme(axis.text.x = element_text(angle=90, size=12), legend.position='none') + 
        geom_bar(stat='identity') 
      if (input$splitByBrands) thePlot <- thePlot + facet_wrap(~Simplified_Brand, scales='free_x')
    }
    
    # Compute bestseller by SKU (broken down into size, color, etc)
    if (input$radioBestSellers=='byItemOption') {
      salesHistSum <- ddply(salesHistForBestSell, .(Simplified_Brand, ItemCode), summarise, TotalOrders = sum(QuantityOrdered))
      salesHistSum$ItemCode <- factor(salesHistSum$ItemCode, levels = salesHistSum$ItemCode[order(-salesHistSum$TotalOrders)])
      salesHistSum <- salesHistSum[order(-salesHistSum$TotalOrders),]
      salesHistSumToPlot <- salesHistSum[1:input$nBestsellers,]
      #print(head(salesHistSum))
      thePlot <- ggplot(salesHistSumToPlot, aes(x=ItemCode, y=TotalOrders, fill=ItemCode)) + 
        theme_wsj() + 
        ggtitle("Pieces Sold") + 
        #coord_flip() + 
        #facet_wrap(~Simplified_Brand, scales='free_x') + 
        theme(axis.text.x = element_text(angle=90, size=12), legend.position='none') + 
        geom_bar(stat='identity') 
      if (input$splitByBrands) thePlot <- thePlot + facet_wrap(~Simplified_Brand, scales='free_x')
    }
    
    #filename <- 'bestsellers.csv'
    #write.csv(salesHistSum, filename)
    output$downloadData <- downloadHandler(
      filename = function() {paste0('ProductSales-', Sys.Date(), '.csv')},
      content = function(file) {
        write.csv(salesHistSum, file)
      }
    )
    return(thePlot)
    
  })
  
  
  # Data table of items 
  output$lowInventoryDataTable <- renderDataTable({
    inventory <- ReadInventoryCountReactive()
    
    datatable(inventory,
              options = list(pageLength = 300, info=TRUE),
              rownames = FALSE, 
              colnames=c('Sage Code', 'Build Style', 'In-House', 'Brand', 'On Hands',
                         'On Sales Order', 'On Back Order', 'On Purchase Order', 
                         'Available'))
    
    # datatable(returns[,c("BillToName", "InvoiceDate", "InvoiceNo", "ItemCode", "ItemCodeDesc", "QuantityOrdered", "UnitPrice")],
    #              rownames=FALSE, colnames=c("Store","Date","Invoice #", "Code", "Description", "N Piece", "Price"), 
    #              options = list(pageLength = 150), 
    #              class = 'table-condensed')
    
    #datatable(sales[,c("BillToName", "InvoiceDate", "InvoiceNo", "ItemCode", "ItemCodeDesc", "QuantityOrdered", "UnitPrice")],
    #          rownames=FALSE, colnames=c("Store","Date","Invoice #", "Code", "Description", "N Piece", "Price"), 
    #          options = list(pageLength = 150, info=TRUE), 
    #          class = 'table-condensed')
    #datatable(sales[,c("BillToName", "InvoiceDate", "InvoiceNo", "ItemCode", "ItemCodeDesc", "QuantityOrdered", "UnitPrice")])
    #print(sales[,c("BillToName", "InvoiceDate", "InvoiceNo", "ItemCode", "ItemCodeDesc", "QuantityOrdered", "UnitPrice")])
  })
  
  
  #})
}

shinyApp(ui, server)

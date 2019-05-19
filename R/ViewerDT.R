library(shiny)
library(miniUI)
library(DT)
library(data.table)

ViewDT <- function(data) {
  
  ui <- miniPage(
    tags$style(HTML(
      'table.dataTable tr.selected {outline: 1px solid rgba(0,0,0,0.5) !important;}
      ')),
    DT::dataTableOutput("mytable1", height = "100%")
    )
  
  server <- function(input, output, session) {
    
    df = as.data.table(data)
    
    num_cols = names(df)[df[,sapply(df,is.numeric)]]
    fac_cols = names(df)[df[,sapply(df,is.factor)]]
    brks <- df[,lapply(.SD, quantile, probs = seq(.05, .95, .05), na.rm = TRUE), .SDcols=num_cols]
    brks_fac <- df[,lapply(.SD, as.numeric), .SDcols=fac_cols]
    brks_fac <- brks_fac[,lapply(.SD, quantile, probs = seq(.05, .95, .05), na.rm = TRUE)]
    for (col in names(brks_fac))
      brks_fac[, eval(col):=lapply(.SD, function(x) levels(df[,get(col)])[x]), .SDcols=col]
    clrs <- round(seq(255, 40, length.out = 20), 0) %>%
    {paste0("rgb(", ., ",", ., ",255)")}
    clrs_fac <- round(seq(255, 40, length.out = 19), 0) %>%
    {paste0("rgb(", ., ",255,", ., ")")}
    
    output$mytable1 <- DT::renderDataTable({
      table <- DT::datatable(df, 
                    extensions = c('Buttons','FixedColumns','Scroller'), 
                    filter = list(position = 'top'),
                    class = 'compact table-hover',
                    fillContainer = TRUE,
                    options = list(dom = 'BfrtS', 
                                   buttons = I('colvis'),
                                   scrollX = TRUE,
                                   scrollY = 200,
                                   deferRender = TRUE,
                                   fixedColumns = TRUE,
                                   paging = T
                                   )
                    )
      for (col in num_cols)
        table <- formatStyle(table, col, backgroundColor = styleInterval(brks[,get(col)], clrs))
      for (col in fac_cols)
        table <- formatStyle(table, col, backgroundColor = styleEqual(brks_fac[,get(col)], clrs_fac))
      table
    })
  }
  
  runGadget(ui, server, viewer = paneViewer())
}

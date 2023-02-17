library(shiny)
library(dplyr)
library(nnet)
library(plm)
library(openxlsx)
library(readxl)
library(stringr)
library(fastDummies)

# Load data
fileBaseGW <- "M:\\%5C\\temp\\fpiazza\\GW\\Input\\"
fileBaseCompanyData <- "M:\\%5C\\Risk Analysis\\Workstreams\\31_Sustainable finance\\Greenwashing\\RepRisk\\Input\\"
df <- read.xlsx(paste0(fileBaseGW, "CARs202021Beta_all.xlsx"))
STOXX600_Control_variables <- read_excel(paste0(fileBaseCompanyData, "STOXX600_Control variables.xlsx"),
                                         sheet = "Sheet2")
STOXX600_Control_variables$price_to_book <- as.numeric(STOXX600_Control_variables$price_to_book)

df = merge(x=df, y = STOXX600_Control_variables, by.x = "ISIN", by.y = "isin")

df <- df %>% 
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30"),
         year = format(Date, "%Y"),
         ym = format(Date, "%m%Y"),
         NEW_REACH = if_else(SOURCE.REACH == "high-reach" | SOURCE.REACH == "medium-reach", "medium+high", "low"),
         NEW_SEVERITY = if_else(SEVERITY.OF.INCIDENT == "Very severe" | SEVERITY.OF.INCIDENT == "Severe", "severe", "less severe"),
         SECTOR.x = if_else(SECTOR.x == 'Oil And Gas' | SECTOR.x == 'Oil and Gas', 'Oil and Gas', SECTOR.x),
         SECTOR.x = str_to_title(SECTOR.x),
         SECTOR.x = if_else(str_detect(SECTOR.x, "Banks"), "Banks Financial Services", SECTOR.x)) %>% 
  mutate(legal = if_else(str_detect(DESCRIPTION, paste(c("litigation" , "lawsuit", "fine ", "fined", "sue ", "sued", "judge", "lawyer", "trial", "court", "legal", "prosecut"), collapse = "|")), "legal", "nonlegal"),
         ESG.PILLAR = case_when(
           ESG.PILLAR == "Environmental, Social, Governance" ~ "Environmental, Social",
           ESG.PILLAR == "Environmental, Governance" ~ "Environmental",
           ESG.PILLAR == "Social, Governance" ~ "Social",
           ESG.PILLAR == "" ~ "Governance",
           TRUE ~ ESG.PILLAR
         ),
         COMPANY = factor(COMPANY)
  )

df_dummy <- df %>%
  select(-c(SOURCE.REACH, SEVERITY.OF.INCIDENT, SECTOR.x, DESCRIPTION)) %>%
  dummy_cols(select_columns = c("NEW_REACH", "NEW_SEVERITY", "legal", "ESG.PILLAR"), remove_first_dummy = TRUE,) %>%
  select(-c(NEW_REACH, NEW_SEVERITY, legal, ESG.PILLAR))

df_dummy <- df_dummy %>%
  rename_all(~ gsub("-", "minus", .)) %>%
  rename_all(~ gsub(",", "_", .)) %>%
  rename_all(~ gsub("\\+", "plus", .)) %>%
  rename_all(~ gsub(" ", "", .))



# Define the UI
ui <- fluidPage(
  titlePanel("Linear Regression Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "dependent_variable",
        "Select Dependent Variable:",
        choices = colnames(df_dummy[ ,grep("CAR", colnames(df_dummy))]),
        selected = "y"
      ),
      uiOutput("independent_variables"),
      actionButton("update", "Update Regression"),
    ),
    mainPanel(
      verbatimTextOutput("regression_summary")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Create a reactive expression to subset the data based on user input
  data_subset <- reactive({
    df_dummy %>%
      select(input$dependent_variable, input$independent_variables) %>%
      na.omit()
  })
  
  # Create a reactive expression for the regression model
  regression_model <- reactive({
    lm(
      formula = formula(paste(input$dependent_variable, "~", paste(input$independent_variables, collapse = "+"))),
      data = data_subset(),
      na.action = na.omit
    )
  })
  
  # Print the summary of the regression model
  output$regression_summary <- renderPrint({
    summary(regression_model())
  })
  
  # Create checkbox group for independent variables
  output$independent_variables <- renderUI({
    checkboxGroupInput(
      inputId = "independent_variables",
      label = "Independent Variables",
      choices = colnames(df_dummy[ ,-grep("CAR", colnames(df_dummy))]),
      selected = colnames(df_dummy[ ,-grep("CAR", colnames(df_dummy))])[1]
    )
  })
  
}

# Run the Shiny app
shinyApp(ui, server)
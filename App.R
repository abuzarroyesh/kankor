
##Libraries
library(tidyverse)
library(sf)
library(shiny)
library(ggiraph)


##Parameters for mapping
districts <- read_sf("Afghanistan_Districts_2/afg_admbnda_adm2_agcho_20180522.shp")
provinces <- read_sf("Afghanistan_provinces/afg_admbnda_adm1_agcho_20180522.shp")
  
custom_NA_color <- "#f2f2f2" #missing values

custom_colors <- 
  c(
    "#740023",
    "#740023",
    "#b63132",
    "#d27952",
    "#e9b777",
    "#fdf6a3",
    "#a2c0a1",
    "#55889e",
    "#195473",
    "#004364", 
    "#004364"
  )

mean_values <- #for average scores
  c(
    78, 
    124.8536, 
    124.8536 + (173.871 - 124.8536) * 1/4,
    124.8536 + (173.871 - 124.8536) * 2/4,
    124.8536 + (173.871 - 124.8536) * 3/4,
    173.871,
    173.871 + (299.7186 - 173.871) * 1/4,
    173.871 + (299.7186 - 173.871) * 2/4,
    173.871 + (299.7186 - 173.871) * 3/4,
    299.7186,
    311
  ) %>% 
  scales::rescale()

count_values <- #For number of students
  c(#1, 44221
    1, 
    1, 
    1 + (83 - 1) * 1/4,
    1 + (83 - 1) * 2/4,
    1 + (83 - 1) * 3/4,
    83,
    83 + (600 - 83) * 1/4,
    83 + (600 - 83) * 2/4,
    83 + (600 - 83) * 3/4,
    600,
    600
  ) %>% 
  scales::rescale()

pct_values <- #for percent admitted students
  c(
    0, 
    0, 
    0 + (0.4098361 - 0) * 1/4,
    0 + (0.4098361 - 0) * 2/4,
    0 + (0.4098361 - 0) * 3/4,
    0.4098361,
    0.4098361 + (1 - 0.4098361) * 1/4,
    0.4098361 + (1 - 0.4098361) * 2/4,
    0.4098361 + (1 - 0.4098361) * 3/4,
    1,
    1
  ) %>%
  scales::rescale()

mean_scores <- read_csv("mean_scores.csv")


ui <- navbarPage(
  title = "Afghanistan University Entrance Test (Kankor) Data", 
    tabPanel(
      "Data", 
      sidebarPanel(
        selectInput('variable', 'Variable', choices = c("Average Score", "Number of Students", "Percent of Admitted Students")),
        sliderInput('year', 'Year', min = 1393, max = 1398, value = 1398, sep = ""),
        selectInput('gender', 'Gender', choices = c("All", "Male", "Female")), 
        width = 3
      ),
      mainPanel(
        girafeOutput('kankor_map', width = "100%", height = "300px"), 
        width = 9
      ),
      print("© Abuzar Royesh, 2020")
    ), 
    tabPanel(
      "Data Sources and Notes", 
      h4("Overview: "),
      p("This interactive dashboard shows the number of students who have taken Afghanistan’s national university entrance exam (Kankor), the average score, and the percent of students who have been admitted to public 4- or 2-year higher education institutions. All the data is disaggregated at the district level. The maps currently cover the period between 1393 (2014) and 1398 (2019) and will be updated annually as new data becomes available. "), 
      p("Given the absence of standardized testing at school level, this data provides the first systematic assessment of educational outcomes at the district level in Afghanistan. The dashboard has been designed for public use. When using the content from this website, include the following citation:"), 
      p("Royesh, Abuzar. Afghanistan University Entrance Test (Kankor) Data (database). Available at https://afg-kankor.shinyapps.io/kankor_shiny/."), 
      br(),
      h4("Data Sources:"), 
      p("The data in these maps come from three sources: "), 
      div(HTML(
      "<ul> 
               <li> University entrance test scores are released annually by the Afghan Ministry of Higher Education and archived in various websites including <i>anis.af</i>, <i>sayahonline.com</i>, etc. </li> 
               <li> Dataset of schools and geographical locations was requested from the Afghan Ministry of Education. </li> 
               <li> Dataset-	District and province boundaries come from Afghanistan Geodesy and Cartography Head Office (AGCHO), published online by OCHA Afghanistan. The dataset was last updated on May 29, 2018. </li>
      </ul>")),
      br(),
      h4("Data Sampling: "),
      p("These maps present the data on 916,296 out of 1,131,473 students (81 percent) who took the Kankor test between 1393 (2014) and 1398 (2019). These were the students whose school data matched a unique school record in the data from the Ministry of Education. The school records for the remaining 19 percent of students were not matched for one of these three reasons: 1) the school field was missing from the Kankor dataset, 2) no school with that name existed in that province, or 3) the school name matched more than one unique school record in that province."), 
      br(),
      h4("Limitations:"), 
      p("In the process of data cleaning and wrangling, this study faced a number of challenges, which are listed below."), 
      div(HTML(
        "<ul> 
               <li> The Kankor data, as compiled from the various sources, did not come in a standard format. The records for all years are manually typed and include countless typos; </li> 
               <li> The Kankor data did not include unique school ids, which could have been used to match the data with the school records from the Ministry of Education. All observations, including school names, are manually typed, which made matching records a cumbersome task; </li> 
               <li> As explained in sampling, the records for a number of students was not matched to any unique school records in the Ministry of Education data; and  </li>
               <li>	Some district names in the dataset from the Ministry of Education did not match district names in the maps released by OCHA Afghanistan. To fix these discrepancies, I researched the origins for the districts that were not merged. In the cases where the districts were created by splitting another district, I recoded them as the original district. The following are some examples of district names that were not matched. </li>
               <ul> 
               <li> The MoE dataset classifies <i>Ghormach</i> as a district in Faryab province, while the OCHA Afghanistan shapefile classifies it as a district in Badghis province. </li>
               <li> The MoE dataset lists <i>Pato</i> as a district in Daikundi Province. While the OCHA Afghanistan cartographic data doesn’t include a district with that name. </li>
               <li> The MoE dataset includes <i>Yakawlang</i> and <i>Yakawlang Number 2</i> as two different districts in Bamiyan province, while no such distinction is made in the OCHA Afghanistan data. </li>
               <li> The MoE dataset lists <i>Marja</i> and <i>Nad Ali</i> two different districts in Helmand province, while the OCHA Afghanistan data only include <i>Nad Ali</i>. </li>
               <li> The MoE data included three more districts In Paktiya than the OCHA Afghanistan data. The three districts are <i>Gerda Serai</i>, <i>Laja wa Mangal</i>, and <i>Mirzaka</i>. </li>
               </ul>
      </ul>")),
      hr(),
      p("Abuzar Royesh is a Knight-Hennessy Scholar and a dual Master’s student in International Policy and Management Sciences and Engineering (computational social science track) at Stanford University. For any inquiries about this data, you can reach him at <abuzarroyesh [at] gmail [dot] com>.")
    )
)

server <- function(input, output) {
  
  gg <-  reactive({
    mean_scores %>% 
      filter(
        variable == input$variable,
        gender == input$gender, 
        year == input$year
      ) %>% 
      right_join(districts, by = "ADM2_PCODE") %>% 
      mutate(
        desc = 
          if_else(
            is.na(n), 
            str_glue(
              "{ADM2_EN}, {ADM1_EN}
        Number of Students: NA 
        Average Score: NA 
        Percent Admitted: NA"
            ), 
            desc)
      ) %>% 
      ggplot() + 
      geom_sf_interactive(aes(fill = value, tooltip = desc, geometry = geometry), size = 0.01) + 
      geom_sf(data = provinces, color = "white", fill = NA, size = 0.4) + 
      theme_void() +
      coord_sf(datum = NA, ylim = c(29, 39), xlim = c(61, 74.5)) +
      scale_fill_gradientn(
        colors = custom_colors, 
        na.value = custom_NA_color, 
        values = 
          if(input$variable == "Average Score") {
            mean_values
          } else if(input$variable == "Number of Students") {
            count_values
          } else {
            pct_values
          }, 
        breaks = 
          if(input$variable == "Average Score") {
            seq(100, 300, 50)
          } else if(input$variable == "Number of Students") {
            seq(0, 600, 100)
          } else {
            seq(0, 1, 0.2)
          }, 
        limits = 
          if(input$variable == "Average Score") {
            c(78, 311)
          } else if(input$variable == "Number of Students") {
            c(0, 600)
          } else {
            c(0, 1)
          }, 
        labels = 
          if(input$variable == "Average Score") {
            waiver()
          } else if(input$variable == "Number of Students") {
            c(seq(0, 500, 100), "600+")
          } else {
            scales::percent_format(1)
          }
      ) + 
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7)
      ) +
      guides(
        fill = guide_colorbar(
          title = NULL,
          nbin = 6,
          ticks = FALSE,
          barwidth = 18,
          barheight = 0.6,
          raster = TRUE
        )
      )
  })
  
  
  output$kankor_map <- renderGirafe({ 
    
    girafe(ggobj = gg(), width_svg = 10, height_svg = 9)
    
  })
}


shinyApp(ui = ui, server = server)








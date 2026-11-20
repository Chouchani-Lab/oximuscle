library(shiny)
library(DT)
library(shinyjs)
library(glue)
library(shinydashboard)
library(png)
library(readr)
library(plotly)
library(tidyr)
library(ggplot2)
library(dplyr)

#setwd("~/Library/CloudStorage/Dropbox-PartnersHealthCare/ECLab/Chouchani Lab/Data/Jon/Human exercise redox rerun/For website/From Shelley")
#setwd("/Users/shelleywei/Downloads/EC lab/yu website")

end_oxi <- read_csv("Endurance_oxidation.csv")
sprint_oxi <- read_csv("Sprint_oxidation.csv")
res_oxi <- read_csv("Resistance_oxidation.csv")

end_delta <- read_csv("Endurance_delta.csv")
sprint_delta <- read_csv("Sprint_delta.csv")
res_delta <- read_csv("Resistance_delta.csv")

Table_S1_endurance <- read_csv("endurance_all_sites.csv")
Table_S1_sprint <- read_csv("sprint_all_sites.csv")
Table_S1_resistance <- read_csv("resistance_all_sites.csv")
Table_S2 <- read_csv("all_sites_ranked.csv")


human_seq <- read_csv('human_seq.csv')

#uniprot column = protein name

addResourcePath("www", file.path(getwd(), "www"))
ui <- dashboardPage(
  dashboardHeader(title = "A quantitative redox proteome of the human muscle response to exercise", titleWidth = 500),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Help", tabName = "help"),
      menuItem("Search", tabName = "search"),
      menuItem("Download", tabName = "download"),
      menuItem("Acknowledgements", tabName = "acknowledgements"),
      menuItem("Copyright and License Information", tabName = "copyright"),
      menuItem(" ")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML('.sequence-card {
  background: #ffffff;
  color: #222222;
  border: 1px solid #d0d7de;
  border-radius: 6px;
  padding: 18px 22px;
  font-family: Menlo, Monaco, Consolas, "Courier New", monospace;
  font-size: 14px;
  line-height: 1.8;
  overflow-x: auto;
  box-shadow: 0 1px 3px rgba(0,0,0,0.08);
}

.seq-line {
  white-space: nowrap;
  border-bottom: 1px solid #eeeeee;
}

.seq-line:last-child {
  border-bottom: none;
}

.seq-pos {
  display: inline-block;
  width: 55px;
  color: #777777;
  text-align: right;
  margin-right: 25px;
  user-select: none;
}

.seq-text {
  letter-spacing: 2px;
}
        body, .skin-blue .content-wrapper, .right-side {
          background-color: #f9fafc !important;
        }
        .sidebar-menu, .main-sidebar {
          background-color: #222d32 !important;
        }
        .content-wrapper, .main-sidebar {
          overflow-y: scroll !important;
          max-height: calc(1vh - 80px) !important; /* Adjust the max-height value as needed */
        }
        .sidebar {
          overflow-y: auto !important;
        }
      '))
    ),
    tabItems(
      tabItem(
        tabName = "home",
        tags$div(
          style = "text-align: center; margin-bottom: 20px;",
          tags$img(
            src = "www/Oximuscle_image.png",
            style = "max-width: 900px; width: 100%; height: auto;"
          )
        ),
        h2(HTML("Welcome to Oximuscle!")),
        p("ABSTRACT: Reactive oxygen species (ROS) regulate protein function through reversible cysteine oxidation. In human skeletal muscle, exercise-induced ROS initiates adaptations such as mitochondrial biogenesis, increased insulin sensitivity, and hypertrophy. However, specific protein targets of ROS regulation during exercise remain unclear owing to longstanding challenges in analyzing redox proteomes in vivo. We applied cysteine derivatization and multiplexed proteomics to map muscle protein cysteine oxidation in humans during exercise. The OxiMuscle dataset quantifies reversible modifications across 17,492 cysteine sites in young men undergoing three types of exercise, providing the first comprehensive analysis of the exercise-regulated redox cysteine proteome in humans. We systematically define cysteine oxidation targets regulated by at least one form of exercise, many of which reside in proteins with established roles in muscle physiology. Among these sites is a redox-regulated cysteine on the autophagy receptor protein p62. We demonstrate that reversible oxidation of this cysteine regulates p62-mediated autophagy upon myotube contraction and mouse muscle adaptation to exercise in vivo. Together, these results unveil a redox-driven mechanism involved in exercise-induced autophagy and muscle adaptation."),
        p(HTML(""))
      ),
      tabItem(
        tabName = "help",
        h2("Guide to exploring Oximuscle"),
        p("-Select the exercise type you would like to explore."),
        p("-Add the UniProt name for your favorite protein to see all cysteine sites mapped for that protein."),
        p("-If your protein is not found, likely it was not mapped in the selected dataset. Please ensure you have entered the correct UniProt name and exercise type."),
        p("-Cysteine data for individual proteins can be downloaded from the search window."),
        p("-The entire dataset can be downloaded from the Download tab."),
        p("Note the cysteine site numbers indicate the cysteine(s) that were labeled for a given peptide."),
        p("The web application was developed by Shelley Wei, Yu Lei, and Jonathan Petrocelli (Chouchani Lab, DFCI/HMS)."),
        p(HTML(""))
      ),
      tabItem(
        tabName = "search",
        h2("Enter your search query:"),
        p("Search for a cysteine site"),
        mainPanel(
          selectizeInput("search_query", "Enter your search query:", choices = NULL, multiple = FALSE),
          selectInput("search_type", "Search for:", c("Endurance", "Sprint", "Resistance"),
                      selected = "Endurance"),  # Set the default selection
          actionButton("search_button", "Search"),
          p(" "),
          conditionalPanel(
            condition = "input.search_button > 0",
            style = "width: 90%;",
            downloadButton("download_search_results", "Download Search Results (csv)"),
            tags$hr(),
            
            h2("Summary Table"),
            DTOutput("table_all"),
            
            tags$hr(),
            
            h2("Median % Oxidation"),
            plotlyOutput("bar_graph_oxi"),
            
            tags$hr(),
            
            h2("Delta % Oxidation from Pre"),
            plotlyOutput("bar_graph_delta"),
            
            tags$hr(),
            
            h2("Mapped Cysteine Position"),
            h3("Human"),
            plotlyOutput("sequence_plot_human"),
            
            tags$hr(),
            
            h2("Protein Sequence"),
            h3("Human"),
            box(
              width = 12,
              solidHeader = TRUE,
              background = "light-blue",
              uiOutput("seq_human")
            ),
            
            tags$hr(),
            
            h2("External Resources"),
            uiOutput("alphafold_link")
          )
        )
      ),
      tabItem(
        tabName = "download",
        h2("Bulk Download Data Files"),
        p("Download flat files (comma-separated) containing all data displayed in the Oximuscle website."),
        p(" "),
        downloadButton("download_btn_1", "Download All Cysteine Data"),
        br(), br(),
        
        downloadButton("download_table_s1_endurance", "Download Endurance All Sites"),
        br(), br(),
        
        downloadButton("download_table_s1_sprint", "Download Sprint All Sites"),
        br(), br(),
        
        downloadButton("download_table_s1_resistance", "Download Resistance All Sites"),
        br(), br(),
        
        downloadButton("download_table_s2", "Download All Sites Ranked")
      ),
      
      tabItem(
        tabName = "acknowledgements",
        h2("Acknowledgements"),
        h3("The Oximuscle website uses the following libraries and tools:"),
        p("• InnateDB: systems biology of the innate immune response database ", a("https://www.innatedb.com/", href = "https://www.innatedb.com/")),
        p("• Uniprot: ", a("https://www.uniprot.org/", href = "https://www.uniprot.org/")),
        p("• Oximouse dataset: ", a("https://oximouse.hms.harvard.edu/", href = "https://oximouse.hms.harvard.edu/")),
        #p("• InterPro Pfam domains classification of protein families database: ", a("https://www.ebi.ac.uk/interpro/", href = "https://www.ebi.ac.uk/interpro/")),
        p("• PDB protein data bank: ", a("https://www.rcsb.org/", href = "https://www.rcsb.org/")),
        p("• AlphaFold protein structure database: ", a("https://alphafold.ebi.ac.uk/", href = "https://alphafold.ebi.ac.uk/")),
        p("• DEPOD human DEPhOsphorylation database: ", a("https://depod.bioss.uni-freiburg.de", href = "https://depod.bioss.uni-freiburg.de")),
        p("• KinaseMD: Kinase mutations and drug response database: ", a("https://bioinfo.uth.edu/kmd/index.html", href = "https://bioinfo.uth.edu/kmd/index.html")),
        p("• Human Protein Atlas: ", a("https://www.proteinatlas.org/", href = "https://www.proteinatlas.org/")),
        p("• TFLink database: ", a("https://tflink.net", href = "https://tflink.net")),
        p("• Mammalian metabolic enzyme database: ", a("https://esbl.nhlbi.nih.gov/Databases/KSBP2/Targets/Lists/MetabolicEnzymes/MetabolicEnzymeDatabase.html", href = "https://esbl.nhlbi.nih.gov/Databases/KSBP2/Targets/Lists/MetabolicEnzymes/MetabolicEnzymeDatabase.html")),
        p("• Shiny: ", a("https://shiny.posit.co", href = "https://shiny.posit.co"))
      ),
      tabItem(
        tabName = "copyright",
        h2("Copyright and License Information"),
        p("The Oximuscle website was developed and is licensed under the Creative Commons Attribution-ShareAlike 4.0 International License."),
        p("Images presented and generated on the Oximuscle website are licensed under the Creative Commons Attribution-ShareAlike 4.0 International License.")
      )
    )
  ),
  skin = "blue"
)

server <- function(input, output, session) {
  # Create a reactive value to store the selected row
  selectedRow <- reactiveVal(NULL)
  filtered_data_delta <- reactiveVal(NULL)
  filtered_data_oxi <- reactiveVal(NULL)
  
  df_oxi <- reactiveVal()
  df_delta <- reactiveVal()
  oxi_delta <- reactiveVal()
  
  # Create a list of unique values for autocomplete suggestions
  autocomplete_choices <- reactive({
    df_to_search <- switch(input$search_type,
                           "Endurance" = end_delta,
                           "Sprint" = sprint_delta,
                           "Resistance" = res_delta
    )
    c("-", unique(sapply(strsplit(df_to_search[[1]], "\\|"), `[`, 3)))
  })
  
  observe({
    updateSelectizeInput(session, "search_query", choices = autocomplete_choices())
  })
  
  observeEvent(input$search_button, {
    search_query <- tolower(input$search_query)
    search_type <- input$search_type
    
    if (search_type == "Endurance") {
      df_to_search <- end_delta
      df_to_search2 <- end_oxi
    } else if (search_type == "Sprint") {
      df_to_search <- sprint_delta
      df_to_search2 <- sprint_oxi
    } else if (search_type == "Resistance") {
      df_to_search <- res_delta
      df_to_search2 <- res_oxi
    } else {
      df_to_search <- NULL
      df_to_search2 <- NULL
    }
    
    if (!is.null(df_to_search$cys_site)) {
      
      df_to_search <- df_to_search %>%
        mutate(across(everything(), as.character)) %>%
        mutate(across(everything(), ~ ifelse(is.na(.), "NA", .)))
      df_to_search2 <- df_to_search2 %>%
        mutate(across(everything(), as.character)) %>%
        mutate(across(everything(), ~ ifelse(is.na(.), "NA", .)))
      
      filtered_data_delta(df_to_search[grepl(search_query, df_to_search$cys_site, ignore.case = TRUE), ])
      filtered_data_oxi(df_to_search2[grepl(search_query, df_to_search2$cys_site, ignore.case = TRUE), ])
      
    }
  })
  
  
  output$table_all <- renderDT({
    filtered_oxi <- filtered_data_oxi()
    filtered_delta <- filtered_data_delta()
    
    if (is.null(filtered_oxi) || is.null(filtered_delta)) {
      return(datatable(data.frame(Message = "Search for a site first"), rownames = FALSE))
    }
    
    if (nrow(filtered_oxi) == 0 && nrow(filtered_delta) == 0) {
      return(datatable(data.frame(Message = "No results found"), rownames = FALSE))
    }
    
    make_numeric <- function(x) {
      as.numeric(unlist(x))
    }
    
    sem <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) <= 1) return(NA)
      sd(x) / sqrt(length(x))
    }
    
    mean_sem <- function(x) {
      x <- x[!is.na(x)]
      if (length(x) == 0) return("NA")
      paste0(sprintf("%.1f", mean(x)), " ± ", sprintf("%.1f", sem(x)))
    }
    
    if (nrow(filtered_oxi) > 0) {
      pre_oxi <- make_numeric(filtered_oxi[, 2:9])
      post_oxi <- make_numeric(filtered_oxi[, 10:17])
      rec_oxi <- make_numeric(filtered_oxi[, 18:25])
    } else {
      pre_oxi <- post_oxi <- rec_oxi <- NA
    }
    
    if (nrow(filtered_delta) > 0) {
      pre_delta <- make_numeric(filtered_delta[, 2:9])
      post_delta <- make_numeric(filtered_delta[, 10:17])
      rec_delta <- make_numeric(filtered_delta[, 18:25])
    } else {
      pre_delta <- post_delta <- rec_delta <- NA
    }
    
    result <- data.frame(
      Measurement = c("% Oxidation", "% Oxidation shift from Pre"),
      Pre = c(mean_sem(pre_oxi), "--" ),
      Post = c(mean_sem(post_oxi), mean_sem(post_delta)),
      Recovery = c(mean_sem(rec_oxi), mean_sem(rec_delta))
    )
    
    datatable(
      result,
      options = list(
        dom = 't',
        autoWidth = TRUE
      ),
      rownames = FALSE
    )
  })
  
  observeEvent(input$search_button, {
    gene <- toupper(paste(strsplit(input$search_query, "_")[[1]][1:2], collapse = "_"))
    sequence_match_human <- human_seq[grepl(gene, human_seq$'Entry Name', ignore.case = TRUE), ]
    output$alphafold_link <- renderUI({
      if (nrow(sequence_match_human) == 0) {
        return(NULL)
      }
      
      accession <- sequence_match_human$Entry[1]
      
      tagList(
        tags$a(
          href = paste0("https://alphafold.ebi.ac.uk/entry/", accession),
          target = "_blank",
          class = "btn btn-primary",
          "Open AlphaFold Structure"
        ),
        tags$span(" "),
        tags$a(
          href = paste0("https://www.uniprot.org/uniprotkb/", accession),
          target = "_blank",
          class = "btn btn-default",
          "Open UniProt"
        )
      )
    })
    
    if (nrow(sequence_match_human) > 0) {
      
      sequence_match_human <- sequence_match_human[1, ]
      
      protein_sequence_human <- as.character(sequence_match_human$Sequence[1])
      highlighted_sequence_human <- gsub(
        "C",
        '<span style="color: red;">C</span>',
        protein_sequence_human,
        fixed = TRUE
      )
      
      positions <- unlist(gregexpr("C", protein_sequence_human, fixed = TRUE))
      positions <- positions[positions > 0]
      sequence_length_human <- nchar(protein_sequence_human)
      site_source <- filtered_data_delta()
      
      if (is.null(site_source) || nrow(site_source) == 0) {
        site_source <- filtered_data_oxi()
      }
      
      third_split_values_human <- sapply(
        strsplit(as.character(site_source[, 1]), "_"),
        function(x) {
          if (length(x) >= 3) x[3] else NA
        }
      )
      
      secondary_split_list_human <- unlist(strsplit(third_split_values_human, ";"))
      
      unique_values_human <- unique(
        suppressWarnings(as.numeric(secondary_split_list_human))
      )
      
      unique_values_human <- unique_values_human[!is.na(unique_values_human)]
      
      # Highlight "c"s in the sequence
      protein_sequence_human <- isolate(protein_sequence_human)
      
      format_sequence <- function(seq, line_width = 60) {
        starts <- seq(1, nchar(seq), by = line_width)
        
        lines <- sapply(starts, function(start) {
          end <- min(start + line_width - 1, nchar(seq))
          line_seq <- substr(seq, start, end)
          
          line_seq <- gsub(
            "C",
            '<span style="color:#d62728;font-weight:700;">C</span>',
            line_seq,
            fixed = TRUE
          )
          
          paste0(
            '<div class="seq-line">',
            '<span class="seq-pos">', start, '</span>',
            '<span class="seq-text">', line_seq, '</span>',
            '</div>'
          )
        })
        
        paste(lines, collapse = "")
      }
      
      seq_html_human <- HTML(paste0(
        '<div class="sequence-card">',
        format_sequence(protein_sequence_human, line_width = 60),
        '</div>'
      ))
      
      # Return the HTML
      output$seq_human <- renderUI({
        return(seq_html_human)
      })
      
      # Render the sequence plot
      output$sequence_plot_human <- renderPlotly({
        
        df <- data.frame(
          x = 1:sequence_length_human,
          y = rep(1, sequence_length_human)
        )
        
        cysteine_plot <- data.frame(
          Residue = unique_values_human,
          y = rep(1, length(unique_values_human))
        )
        
        all_cysteines_plot <- data.frame(
          Residue = positions,
          y = rep(1, length(positions))
        )
        
        other_cysteines_plot <- all_cysteines_plot[
          !all_cysteines_plot$Residue %in% cysteine_plot$Residue,
        ]
        
        p <- ggplot(df, aes(x = x, y = y)) +
          geom_line(color = "#222222", size = 3) +
          
          geom_point(
            data = other_cysteines_plot,
            aes(x = Residue, y = y),
            inherit.aes = FALSE,
            color = "grey65",
            shape = 16,
            size = 3.5,
            alpha = 0.6
          ) +
          
          geom_point(
            data = cysteine_plot,
            aes(x = Residue, y = y),
            inherit.aes = FALSE,
            color = "black",
            shape = 16,
            size = 8
          ) +
          
          geom_point(
            data = cysteine_plot,
            aes(x = Residue, y = y),
            inherit.aes = FALSE,
            color = "#008b8b",
            shape = 16,
            size = 7
          ) +
          
          theme_minimal() +
          labs(
            x = "Residue Position",
            y = NULL,
            title = "Mapped Cysteine Site"
          ) +
          theme(
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.title = element_text(hjust = 0.5)
          )
        
        ggplotly(p)
      })
      
      output$bar_graph_oxi <- renderPlotly({
        filtered_oxi <- filtered_data_oxi()
        
        if (is.null(filtered_oxi) || nrow(filtered_oxi) == 0) {
          return(NULL)
        }
        
        pre_oxi <- as.numeric(unlist(filtered_oxi[, 2:9]))
        post_oxi <- as.numeric(unlist(filtered_oxi[, 10:17]))
        rec_oxi <- as.numeric(unlist(filtered_oxi[, 18:25]))
        
        df <- data.frame(
          Condition = factor(
            c("Pre", "Post", "Recovery"),
            levels = c("Pre", "Post", "Recovery")
          ),
          Median = c(
            median(pre_oxi, na.rm = TRUE),
            median(post_oxi, na.rm = TRUE),
            median(rec_oxi, na.rm = TRUE)
          )
        )
        
        p <- ggplot(df, aes(
          x = Condition,
          y = Median,
          text = paste0("Median % Oxidation: ", round(Median, 2))
        )) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(y = "Median % Oxidation", x = NULL) +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
      })
      
      
      output$bar_graph_delta <- renderPlotly({
        filtered_delta <- filtered_data_delta()
        
        if (is.null(filtered_delta) || nrow(filtered_delta) == 0) {
          return(NULL)
        }
        
        post_delta <- as.numeric(unlist(filtered_delta[, 10:17]))
        rec_delta <- as.numeric(unlist(filtered_delta[, 18:25]))
        
        df <- data.frame(
          Condition = factor(
            c("Post", "Recovery"),
            levels = c("Post", "Recovery")
          ),
          Delta = c(
            median(post_delta, na.rm = TRUE),
            median(rec_delta, na.rm = TRUE)
          )
        )
        
        p <- ggplot(df, aes(
          x = Condition,
          y = Delta,
          text = paste0("Delta % Oxidation from Pre: ", round(Delta, 2))
        )) +
          geom_bar(stat = "identity", fill = "steelblue") +
          labs(y = "Delta % Oxidation from Pre", x = NULL) +
          theme_minimal()
        
        ggplotly(p, tooltip = "text")
      })
    } else {
      output$seq_human <- renderUI({
        return(NULL)
      })
    }
  })
  
  output$download_btn_1 <- downloadHandler(
    filename = function() {
      "cysteine_data_all.csv"
    },
    content = function(file) {
      all_data <- bind_rows(
        Endurance_Oxidation = end_oxi,
        Sprint_Oxidation = sprint_oxi,
        Resistance_Oxidation = res_oxi,
        Endurance_Delta = end_delta,
        Sprint_Delta = sprint_delta,
        Resistance_Delta = res_delta,
        .id = "Dataset"
      )
      
      write.csv(all_data, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_table_s1_endurance <- downloadHandler(
    filename = function() {
      "endurance_all_sites.csv"
    },
    content = function(file) {
      write.csv(Table_S1_endurance, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_table_s1_sprint <- downloadHandler(
    filename = function() {
      "sprint_all_sites.csv"
    },
    content = function(file) {
      write.csv(Table_S1_sprint, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_table_s1_resistance <- downloadHandler(
    filename = function() {
      "resistance_all_sites.csv"
    },
    content = function(file) {
      write.csv(Table_S1_resistance, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_table_s2 <- downloadHandler(
    filename = function() {
      "all_sites_ranked.csv"
    },
    content = function(file) {
      write.csv(Table_S2, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  output$download_search_results <- downloadHandler(
    filename = function() {
      paste0("search_results_", tolower(input$search_query), ".csv")
    },
    content = function(file) {
      oxi <- filtered_data_oxi()
      delta <- filtered_data_delta()
      
      if (!is.null(oxi) && nrow(oxi) > 0) {
        oxi$Data_Type <- "Oxidation"
      }
      
      if (!is.null(delta) && nrow(delta) > 0) {
        delta$Data_Type <- "Delta"
      }
      
      search_results <- bind_rows(oxi, delta)
      
      write.csv(search_results, file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
}
shinyApp(ui = ui, server = server)

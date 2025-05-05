#########################

install.load.package <- function(x) {
  if (!require(x, character.only = TRUE))
    install.packages(x, repos = 'http://cran.us.r-project.org')
  require(x, character.only = TRUE)
}
package_vec <- c("workflowr", "plotly", "dplyr", "readr", "RODBC", "lubridate", "htmlwidgets")
sapply(package_vec, install.load.package)

###################

# Load Access database from 2024
channel <- odbcConnectAccess2007("data/database/db_kig_nesting_waterfowl_2024_20250428.accdb")
deployment_info <- sqlFetch(channel, "tbl_iButton")
pred_hatch <- sqlFetch(channel, "qry_estHatch")
odbcClose(channel)

# Create identifier to match iButton file names
deployment_info <- deployment_info %>%
  mutate(file_id = paste0("24", id_nest, "_", id_iButton),
         dt_deployed = as.POSIXct(dt_deployed, tz = "UTC"))
# merge tables and tbl_iButton in order to match predicted hatch date
pred_hatch <- pred_hatch %>%
  left_join(deployment_info %>% select(id_nest, id_iButton), by = "id_nest") %>%
  mutate(file_id = paste0("24", id_nest, "_", id_iButton))


# List iButton csv files
iButton_csv_files <- list.files(path = "data", pattern = "\\.csv$", full.names = TRUE)
file_names <- list.files(path = "data", pattern = "\\.csv$")

# List to store plots
plots <- list()

# Loop through files and create plots
for (i in seq_along(iButton_csv_files)) {
  df <- read_csv(iButton_csv_files[i], skip = 14, show_col_types = FALSE)
  #Parse Date/Time
  df$`Date/Time` <- parse_date_time(df$`Date/Time`, orders = c("ymd HMS", "mdy HMS"), tz = "UTC")
  #Fix broken year
  year(df$`Date/Time`) <- 2024

  # Extract file_id (remove .csv extension)
  file_id <- tools::file_path_sans_ext(file_names[i])

  # Match deployment time using file_id
  deploy_time <- deployment_info %>%
    filter(file_id == !!file_id) %>%
    pull(dt_deployed)

  # Match predicted hatch date
  hatch_time <- pred_hatch %>%
    filter(file_id == !!file_id) %>%
    pull(estHatch)

  # Create plot
  p <- plot_ly(data = df, x = ~`Date/Time`, y = ~Value,
               type = 'scatter', mode = 'lines+markers' , marker = list(size = 4),
               name = file_id) %>%
    layout(
      title = list(text = file_id, x = 0.05),
      xaxis = list(title = "Date/Time"),
      yaxis = list(title = "Temperature (°C)")
    )

  # If deployment date exists, add a vertical line
  if (length(deploy_time) == 1 && !is.na(deploy_time)) {
    p <- p %>%
      add_lines(x = c(deploy_time, deploy_time),
                y = c(min(df$Value, na.rm = TRUE), max(df$Value, na.rm = TRUE)),
                line = list(color = 'red', dash = 'dot'),
                inherit = FALSE,
                showlegend = FALSE)

  }

  # If estimated hatch date exists, add a vertical line
  if (length(hatch_time) == 1 && !is.na(hatch_time)) {
    p <- p %>%
      add_lines(x = c(hatch_time, hatch_time),
                y = c(min(df$Value, na.rm = TRUE), max(df$Value, na.rm = TRUE)),
                line = list(color = 'green', dash = 'dot'),
                inherit = FALSE,
                showlegend = FALSE)

  }

  # Store plot
  plots[[file_id]] <- p
}

#export each plot to an html file
for (plot_name in names(plots)) {
  out_file <- file.path("output", paste0(plot_name, ".html"))
  saveWidget(plots[[plot_name]], out_file, selfcontained = TRUE)
}

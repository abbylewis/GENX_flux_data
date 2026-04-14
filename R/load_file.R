load_file <- function(path_display, output_dir) {
  url <- "https://content.dropboxapi.com/2/files/download"
  name <- sub("/GCREW_LOGGERNET_DATA/archive_data/", "",path_display)
  name <- sub("/SERC_Tower_MET/SERC_Tower_Rawdata_Loggernet/SERC_Tower_Rawdata_Archive/", "",name)
  name <- sub("Hiremutt Projects/GENX Heating/GENX_data/NormalizedData/genx_export/2025_combined", "",name)
  name <- sub("Hiremutt Projects/GENX Heating/Genx_data/NormalizedData/genx_ardlog/", "",name)
  name <- sub("Hiremutt Projects/GENX Heating/Genx_data/Rolling_MSD_Data/yearly_RMSD_genx_ardlog/", "",name)
  
  if (grepl("current", name)) name <- "current.dat"

  httr::POST(
    url = url,
    httr::config(token = get_dropbox_token()),
    httr::add_headers("Dropbox-API-Arg" = jsonlite::toJSON(
      list(
        path = path_display
      ),
      auto_unbox = TRUE
    )),
    httr::write_disk(here::here(output_dir, name), overwrite = T)
  )
}

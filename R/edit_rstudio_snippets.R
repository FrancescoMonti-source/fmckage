function (type = c("r", "markdown", "c_cpp",
                   "css", "html", "java", "javascript",
                   "python", "sql", "stan", "tex"))
{
    type <- tolower(type)
    type <- match.arg(type)
    file <- path_ext_set(type, "snippets")
    new_rstudio <- !rstudioapi::isAvailable() || rstudioapi::getVersion() >=
        "1.3.0"
    old_path <- path_home_r(".R", "snippets", file)
    new_path <- rstudio_config_path("snippets", file)
    if (new_rstudio && file_exists(old_path) && !file_exists(new_path)) {
        create_directory(path_dir(new_path))
        file_copy(old_path, new_path)
        ui_done("Copying snippets file to {ui_path(new_path)}")
    }
    path <- if (new_rstudio)
        new_path
    else old_path
    if (!file_exists(path)) {
        ui_done("New snippet file at {ui_path(path)}")
        ui_info(c("This masks the default snippets for {ui_field(type)}.",
                  "Delete this file and restart RStudio to restore the default snippets."))
    }
    edit_file(path)
}

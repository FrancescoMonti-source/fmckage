#' Save an Editable ggplot2 Plot as a PowerPoint Slide
#'
#' This function takes a ggplot2 plot and saves it as an editable graphic in a new PowerPoint file.
#' The function checks if the necessary packages ("ggplot2", "officer", "rvg") are installed,
#' and if not, prompts the user to install them.
#'
#' @param plot The ggplot2 plot to be saved.
#' @param filename A character string specifying the name of the PowerPoint file to be created (without the .pptx extension).
#' @param path A character string specifying the directory where the PowerPoint file should be saved.
#' Defaults is create a new directory called "graphs" under the current working directory which is in accordance with the ProjectTemplate folders arborescence.
#' @param extension Currently the only working extension is .pptx
#'
#' @return A PowerPoint file containing the ggplot2 plot as an editable graphic is saved to the specified directory.
#'
#' @examples
#' \dontrun{
#' p <- ggplot(mtcars, aes(mpg, hp)) +
#'   geom_point() +
#'   theme_minimal()
#' save_editable_plot(p, filename = "my_plot", path = "your_project_name/graphs")
#' }
#'
#' @export

save_editable_plot <- function(plot, filename, path = "../graphs/", extension = ".pptx") {

    # List of necessary packages
    necessary_packages <- c("ggplot2", "officer", "rvg")

    # Check if packages are installed
    for (pkg in necessary_packages) {
        if (!require(pkg, character.only = TRUE)) {
            print(paste(pkg, "not found. Do you want to install it? (y/n)"))

            # Ask for user input
            answer <- readline(prompt = "Enter y or n: ")

            # If user input is "y", install the package
            if (answer %in% c("y","Y")) {
                install.packages(pkg)
                library(pkg, character.only = TRUE)
            }
        }
    }

    # Create a new PowerPoint document using officer
    doc <- read_pptx()

    # Add a slide and then add the ggplot to the slide
    doc <- doc %>%
        add_slide(layout = "Title and Content", master = "Office Theme") %>%
        ph_with(value = dml(code = print(plot)), location = ph_location_type(type = "body"))

    # Save the PowerPoint document
    print(doc, target = paste0(path, "/", filename, extension))
}

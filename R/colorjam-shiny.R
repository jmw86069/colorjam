
#' Colorjam R-shiny App
#'
#' Colorjam R-shiny App for color selection and custom adjustments
#'
#' @returns An object that represents the R-shiny app. Printing the
#'    object or passing it to `shiny::runApp()` will run the app.
#'
#' @param ... additional arguments are passed to `shiny::shinyApp()`
#'    when the argument name is recognized by `shiny::shinyApp()`.
#' @param envir `environment`
#' @param options `list` passed to `shiny::shinyApp()`. The most common
#'    options are:
#'    * `host` with either a specific IP address which then requires
#'    all incoming web requests to match that IP address,
#'    or `host="0.0.0.0"` to accept web requests for any host.
#'    * `port` with specific `numeric` web port, or `port=NULL`
#'    to assign a random port number when the R-shiny app is started.
#'    * `width` with a user-configurable width for the R-shiny app,
#'    useful when graphical components should expect at least a minimum
#'    width.
#' @param verbose `logical` indicating whether to print verbose output.
#'
#' @family colorjam R-shiny functions
#'
#' @examples
#' if (interactive()) {
#'    launchColorjamShiny();
#' }
#' @export
launchColorjamShiny <- function
(...,
 envir=globalenv(),
 options=list(width=1200,
    host="0.0.0.0",
    port=8080),
 verbose=TRUE)
{
   # condition upon having the shiny package
   if (!suppressPackageStartupMessages(require(shiny))) {
      cli::cli_abort(c("x"=paste("{.pkg shiny} is required by",
         "{.fun colorjam::launchColorjamShiny}.")))
   }
   # grab UI and Server components
   ui <- colorjamShinyUI;
   server <- colorjamShinyServer;

   # start the shiny app
   jamba::call_fn_ellipsis(shiny::shinyApp,
      ui=colorjamShinyUI,
      server=colorjamShinyServer,
      options=options,
      ...)
   # enjoy
}


#' Colorjam R-shiny app UI
#'
#' Colorjam R-shiny app UI
#'
#' This function contains the UI for the Colorjam R-shiny app.
#'
#' The Colorjam R-shiny app is started by `launchColorjamShiny()`, which
#' calls `shiny::shinyApp()`, using arguments `server`, `ui`,
#' `onStart`, and `options`. This function fulfills the
#' argument `ui`.
#'
#' @returns A user interface definition suitable to pass
#' to `shiny::shinyUI()`.
#'
#' @family colorjam R-shiny functions
#'
#' #@import shiny
#' #@import shinydashboard
#' #@import shinydashboardPlus
#' #@import shinyWidgets
#'
#' @param ... additional arguments are ignored.
#'
#' @export
colorjamShinyUI <- function
(...)
{
   # header
   # header <- shinydashboard::dashboardHeader(
   #    title=htmltools::tagList("Colorjam R-shiny App",
   #       shiny::icon("map"))
   # );
   # title
   title_panel <- shiny::titlePanel(
      title="Colorjam R-shiny App",
      windowTitle="Colorjam")

   # simple sidebar
   sidebar_panel <- shiny::sidebarPanel(width=12,
      shiny::selectInput("colorjam_preset",
         "Colorjam preset:",
         choices=colorjam_presets(),
         selected="dichromat2"),
      shiny::selectInput("colorjam_step",
         "Colorjam step:",
         choices=colorjam_steps(),
         selected=tail(colorjam_steps(), 1)),
      shiny::sliderInput("colorjam_n",
         "Number of colors:",
         min=1,
         max=100,
         value=12),
      shiny::selectInput("colorjam_nameStyle",
         "Name style:",
         choices=eval(formals(rainbowJam)$nameStyle),
         selected="n"),
      shiny::textInput("colorjam_phase",
         "Phase (comma-delimited order of steps):",
         value=jamba::cPaste(seq_len(max(lengths(
            colorjam_steps(tail(colorjam_steps(), 1))))))),
      shiny::textInput("colorjam_subset",
         "Subset colors (comma-delimited index numbers):",
         value=""),
      shiny::textInput("colorjam_hue_pad_percent",
         "Hue pad percent (numeric value):",
         value="0"),
      shiny::checkboxGroupInput("colorjam_dichromat",
         label="Dichromat simulations?:",
         choices=c("none",
            "deutan",
            "protan",
            "tritan"),
         selected="none")
   )

   # assemble the page
   shiny_ui <- shiny::fluidPage(
      title_panel,
      shiny::fluidRow(
         shiny::column(width=3,
            sidebar_panel),
         shiny::column(width=9,
            shiny::fluidRow(
               shiny::column(width=8,
                  # style="background: #EEEEEE;",
                  shiny::plotOutput("colorjam_plot",
                     width=500,
                     height=450)),
               shiny::column(width=4,
                  shiny::htmlOutput("color_table"))
            ),
            shiny::fluidRow(
               shiny::column(width=8,
                  # style="background: #EEEEEE;",
                  plotly::plotlyOutput("preset_plotly",
                     height=500,
                     width=500)),
               shiny::column(width=4,
                  shiny::htmlOutput("preset_table"))
            )
         )
      )
   )
   return(shiny_ui);
}



#' Colorjam R-shiny app server
#'
#' Colorjam R-shiny app server
#'
#' This function contains the server logic for the Colorjam R-shiny app.
#'
#' The Colorjam R-shiny app is started by `launchColorjamShiny()`, which
#' calls `shiny::shinyApp()`, using arguments `server`, `ui`,
#' `onStart`, and `options`. This function fulfills the
#' argument `ui`.
#'
#' @family colorjam R-shiny functions
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session provided by shiny
#'
#' @export
colorjamShinyServer <- function
(input,
 output,
 session)
{
   #

   # dynamic color preset names

   # use debounce to slow down rendering a plot while changing parameters
   # debounce_ms <- 1000;

   # color data.frame view as kable
   color_kable <- shiny::reactiveVal(
      jamba::kable_coloring(
         data.frame(colors=c("red")),
         colorSub=jamba::nameVector("red"),
         caption="Colors shown.",
         row.names=FALSE)
   )
   output$color_table <- shiny:::renderText({
      color_kable()
   })

   # preset data.frame view as kable
   preset_kable <- shiny::reactiveVal(
      jamba::kable_coloring(
         data.frame(h1=1, h2=1)[1, , drop=FALSE],
         caption="Preset values for h1 and h2.",
         row.names=FALSE)
   )
   output$preset_table <- shiny:::renderText({
      preset_kable()
   })

   # when preset is changed, also change step to the default_step
   shiny::observe({
      new_preset <- input$colorjam_preset;
      h1h2 <- colorjam_presets(preset=new_preset)
      new_step <- head(h1h2$default_step, 1);
      if (length(new_step) > 0 && nchar(default_step) > 0) {
         shiny::updateSelectInput(session=session,
            inputId="colorjam_step",
            choices=colorjam_steps(),
            selected=new_step)
      }
   })

   # when step is changed, update default_step if the preset is custom
   shiny::observe({
      new_step <- input$colorjam_step;
      h1h2 <- colorjam_presets(preset=input$colorjam_preset)
      ## to custom preset
      if (grepl("custom", input$colorjam_preset)) {
         h1h2$default_step <- input$colorjam_step;
         # update custom preset
         add_colorjam_preset(preset=input$colorjam_preset,
            h1=h1h2$h1,
            h2=h1h2$h2,
            direction=head(h1h2$direction, 1),
            default_step=head(h1h2$default_step, 1))
      }
   })

   # render the main plot
   output$colorjam_plot <- shiny::renderPlot({
      # define color palette
      preset_df <- preset_kable();
      phase <- jamba::rmNA(
         as.numeric(strsplit(
            input$colorjam_phase, ",")[[1]]))
      if (length(phase) == 0) {
         phase <- c(1)
      }
      hue_pad_percent <- jamba::rmNA(
         as.numeric(input$colorjam_hue_pad_percent),
         naValue=0);
      use_colors <- rainbowJam(
         n=input$colorjam_n,
         preset=input$colorjam_preset,
         step=input$colorjam_step,
         direction=input$colorjam_direction,
         hue_pad_percent=hue_pad_percent,
         phase=phase,
         nameStyle=input$colorjam_nameStyle
      )

      # optional subset
      subset <- jamba::rmNA(
         as.numeric(strsplit(
            input$colorjam_subset, ",")[[1]]))
      if (length(subset) == 0) {
         subset <- seq_along(use_colors);
      } else {
         subset <- subset[subset %in% seq_along(use_colors)];
      }

      # update the color table
      max_color_rows <- 8;
      use_ncol <- ceiling(length(use_colors[subset]) / max_color_rows);
      use_nrow <- min(c(
         ceiling(length(use_colors[subset])/use_ncol),
         max_color_rows));
      color_vector <- rep("", use_ncol * use_nrow);
      color_vector[seq_along(subset)] <- use_colors[subset];
      color_df <- as.data.frame(matrix(byrow=TRUE,
         ncol=use_ncol,
         color_vector))
      color_kable(kable_coloring(
         color_df,
         caption="Colors shown.",
         colorSub=jamba::nameVector(use_colors),
         row.names=FALSE))

      colorjam_title <- paste0(
         input$colorjam_n, " color palette\n",
         "preset='", input$colorjam_preset, "'\n",
         "step='", input$colorjam_step);
      par("mar"=c(1, 1, 6, 1), "xpd"=NA);
      # optional dichromat adjustments
      ctypes <- c(input$colorjam_dichromat);
      if (length(ctypes) == 0) {
         ctypes <- c("none")
      }
      use_colors_list <- lapply(ctypes, function(itype){
         if (!"none" %in% itype) {
            jamba::nameVector(
               dichromat::dichromat(use_colors[subset],
                  type=itype),
               subset);
         } else {
            use_colors[subset]
         }
      })
      par("mar"=c(0.2, 0.2, 3.5, 0.2));
      color_pie(use_colors_list,
         radius=0.8,
         main=colorjam_title);
      par("mar"=c(5, 4, 4, 2) + 0.1);
   })

   # output plotly
   output$preset_plotly <- plotly::renderPlotly({
      preset_kbl <- preset_kable();
      h1h2 <- as.data.frame(jamba::rmNULL(
         validate_colorjam_preset(
            colorjam_presets(input$colorjam_preset))))
      h1h2 <- jamba::mixedSortDF(h1h2,
         byCols=c(2, direction));
      axis_at <- seq(from=-30, to=390, by=30);

      # update the preset_kable
      preset_kable(kable_coloring(
         row.names=FALSE,
         caption="Preset values for h1 and h2.",
         h1h2[, c("h2", "h1"), drop=FALSE]))

      # prepare editable plotly visualization
      presetly <- plot_colorjam_preset(
         h1=h1h2$h1,
         h2=h1h2$h2,
         direction=head(h1h2$direction, 1),
         style="plotly")

      presetly
   })


   # reactive variable to hold previous edit
   preset_event_data <- shiny::reactiveVal(
      c(0, 0))

   # observe edits in the preset points
   # update x/y reactive values in response to changes in shape anchors
   shiny::observe({
      h1h2 <- as.data.frame(colorjam_presets(input$colorjam_preset))
      # sort h1h2 consistent with plot_colorjam_preset()
      h1h2 <- jamba::mixedSortDF(h1h2,
         byCols=c(1, 2 * direction));
      ed <- plotly::event_data("plotly_relayout")
      # check whether data has changed
      if (identical(preset_event_data(), ed)) {
         # skip any downstream processing
      } else {
         # update stored data for comparison next time
         preset_event_data(ed)

         # downstream processing
         shape_anchors <- ed[grepl("^shapes.*anchor$", names(ed)), drop=FALSE]
         if (length(shape_anchors) == 0) {
            return()
         }
         row_index_values <- gsub("shapes[[]|][.].anchor", "",
            names(shape_anchors));
         # print("shape_anchors:");print(shape_anchors);# debug
         shape_anchors_list <- split(shape_anchors, row_index_values)
         for (i in seq_along(shape_anchors_list)) {
            inum <- as.numeric(names(shape_anchors_list)[i]) + 1;
            shape_anchors_i <- shape_anchors_list[[i]];
            is_x <- which(grepl("xanchor", names(shape_anchors_list[[i]])));
            is_y <- which(grepl("yanchor", names(shape_anchors_list[[i]])));
            new_x <- shape_anchors_list[[i]][[is_x]];
            new_y <- shape_anchors_list[[i]][[is_y]];
            jamba::printDebug("new_x:", new_x, ", new_y:", new_y);# debug
            jamba::printDebug("inum:", inum);# debug

            {
               jamba::printDebug("h1h2 (before):");
               print(jamba::mixedSortDF(byCols=c(2, direction), h1h2));# debug
            }
            h1h2[inum, c("h2", "h1")] <- c(new_x, new_y);
            # run validation
            {
               jamba::printDebug("h1h2 (edited):");
               print(jamba::mixedSortDF(byCols=c(2, direction), h1h2));# debug
            }
            h1h2 <- as.data.frame(jamba::rmNULL(
               validate_colorjam_preset(preset="custom",
                  h1=h1h2$h1,
                  h2=h1h2$h2,
                  direction=head(h1h2$direction, 1),
                  default_step=head(h1h2$default_step, 1))));
            # force values between 0 and 359
            # h1h2$h2 <- h1h2$h2 %% 360;
            {
               jamba::printDebug("h1h2 (validated):");
               print(h1h2);
               print(jamba::mixedSortDF(byCols=c(2, direction), h1h2));# debug
            }
            # update the preset_kable
            preset_kable(kable_coloring(
               row.names=FALSE,
               caption="Preset values for h1 and h2.",
               h1h2))

            # - validate other values in the series?

            # create new preset "custom dichromat2" unless "custom" already appears
            if (!grepl("custom", input$colorjam_preset)) {
               new_preset <- paste("custom", input$colorjam_preset)
            } else {
               new_preset <- input$colorjam_preset
            }
            ## consider adding current input$colorjam_step as default_step
            ## to custom preset
            if (grepl("custom", input$colorjam_preset)) {
               h1h2$default_step <- input$colorjam_step;
            }
            # save new preset
            add_colorjam_preset(preset=new_preset,
               h1=h1h2$h1,
               h2=h1h2$h2,
               direction=head(h1h2$direction, 1),
               default_step=head(h1h2$default_step, 1))

            # update the colorjam_preset UI element with to use this preset name
            shiny::updateSelectInput(session=session,
               inputId="colorjam_preset",
               choices=colorjam_presets(),
               selected=new_preset)
         }
      }
   })
}

# useful step alternatives for v24
# 1,2,6,5,4,3,2,5,6,1,4,3
#
# 1,2,15,5,16,6,7,14,3,8,11,3
#
# 2,1,3,5,6,1,3,4,2,1,5,6
#
# dark-to-light
# 1,4,6,3,5,2
#
# light-to-dark
# 2,5,3,6,4,1
#
# dark-light-dark cycle
# 1,4,6,3,5,2,5,3,6,4
#
# light-dark-light cycle
# 2,5,3,6,4,1,4,6,3,5
#
# alt light-dark
# 2,6,5,4,3,1
#
# alt dark-light
# 2,6,5,4,3,1

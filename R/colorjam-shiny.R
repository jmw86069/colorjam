
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
         value=jamba::cPaste(1:6)),
      shiny::textInput("colorjam_subset",
         "Subset colors (comma-delimited index numbers):",
         value=""),
      shiny::textInput("colorjam_hue_pad_percent",
         "Hue pad percent (numeric value):",
         value="0")
   )

   # main body
   # main_panel <- shiny::mainPanel(
   #    shiny::fluidRow(
   #       shiny::column(width=5,
   #          shiny::plotOutput("colorjam_plot")),
   #       shiny::column(width=1,
   #          shiny::htmlOutput("color_table"))
   #    ),
   #    shiny::fluidRow(
   #       shiny::column(width=5,
   #          plotly::plotlyOutput("preset_plotly")),
   #       shiny::column(width=1,
   #          shiny::htmlOutput("preset_table"))
   #    )
   # )

   # assemble the page
   shiny_ui <- shiny::fluidPage(
      title_panel,
      shiny::fluidRow(
         shiny::column(width=3,
            sidebar_panel),
         shiny::column(width=9,
            shiny::fluidRow(
               shiny::column(width=9,
                  shiny::plotOutput("colorjam_plot")),
               shiny::column(width=3,
                  shiny::htmlOutput("color_table"))
            ),
            shiny::fluidRow(
               shiny::column(width=9,
                  plotly::plotlyOutput("preset_plotly")),
               shiny::column(width=3,
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
#' @import jamba
#' @import dplyr
#' @import ggplot2
#' @import plotly
#' @importFrom plotly subplot
#' @import GenomicRanges
#' @importFrom magrittr %>%
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

      # update the color table
      use_ncol <- ceiling(length(use_colors) / 12);
      use_nrow <- min(c(ceiling(length(use_colors)/use_ncol), 12));
      color_vector <- rep("", use_ncol * use_nrow);
      color_vector[seq_along(use_colors)] <- use_colors;
      color_df <- as.data.frame(matrix(byrow=TRUE,
         ncol=use_ncol,
         color_vector))
      color_kable(kable_coloring(
         color_df,
         caption="Colors shown.",
         colorSub=jamba::nameVector(use_colors),
         row.names=FALSE))

      # optional subset
      subset <- jamba::rmNA(
         as.numeric(strsplit(
            input$colorjam_subset, ",")[[1]]))
      if (length(subset) == 0) {
         subset <- seq_along(use_colors);
      } else {
         subset <- subset[subset %in% seq_along(use_colors)];
      }
      colorjam_title <- paste0(
         input$colorjam_n, " color palette\n",
         "preset='", input$colorjam_preset, "'\n",
         "step='", input$colorjam_step);
      par("mar"=c(1, 1, 6, 1), "xpd"=NA);
      color_pie(use_colors[subset],
         radius=0.9,
         main=colorjam_title);
   })

   # render the preset plot
   output$preset_plot <- shiny::renderPlot({
      h1h2 <- as.data.frame(colorjam_presets(input$colorjam_preset))

      # h1h2 <- jamba::mixedSortDF(as.data.frame(h1h2), byCols=2)
      # simple plot showing the color warping
      plot(x=h1h2$h1 %% 360,
         y=h1h2$h2 %% 360,
         xlim=c(-10, 370),
         ylim=c(-10, 370),
         xaxt="n",
         yaxt="n",
         asp=1,
         pch=".",
         col="transparent",
         xlab="h1",
         ylab="h2",
         main=paste0("preset='", input$colorjam_preset, "'"))
      title(xlab="h1", ylab="h2")
      axis_at <- seq(from=-30, to=390, by=30);
      abline(h=axis_at, v=axis_at, col="grey", lty=2)
      abline(h=c(0, 360), v=c(0, 360), col="grey", lty=1);
      axis(1,
         at=axis_at,
         labels=axis_at)
      axis(2,
         las=2,
         at=axis_at,
         labels=axis_at)
      # approximate intermediate points
      hseq <- seq(from=0, to=359)
      h1new <- approx_degrees(h1=h1h2$h2,
         h2=h1h2$h1,
         h=hseq)
      # approximate full saturation color by converting HCL to HSL
      hclhue2hsl <- function(hseq){
         hsl_seq <- round(col2hsl(hcl2col(H=hseq, C=60, L=70)))["H",]
         hsl_vals <- hsl2col(H=hsl_seq, S=100, L=50)
      }
      # input colors
      hsl_vals <- hclhue2hsl(hseq);
      rect(
         xleft=hseq, xright=hseq+1,
         ybottom=-10, ytop=-2,
         col=hsl_vals, border=NA)
      rect(
         xleft=hseq, xright=hseq+1,
         ybottom=362, ytop=370,
         col=hsl_vals, border=NA)
      # output colors
      hsl_vals_new <- hclhue2hsl(h1new);
      rect(
         xleft=-10, xright=-2,
         ybottom=hseq, ytop=hseq+1,
         col=hsl_vals_new, border=NA)
      rect(
         xleft=362, xright=370,
         ybottom=hseq, ytop=hseq+1,
         col=hsl_vals_new, border=NA)

      # draw intermediate points
      points(x=h1new,
         y=hseq,
         pch=20,
         cex=1,
         col=hsl_vals_new)
      points(x=rev(h1new),
         y=rev(hseq),
         pch=20,
         cex=0.5,
         col=rev(hsl_vals_new))
      # re-draw labels
      points(x=h1h2$h1 %% 360,
         y=h1h2$h2 %% 360,
         pch=21,
         cex=2,
         bg="#FFFFFF66")
      text(x=h1h2$h1 %% 360,
         y=h1h2$h2 %% 360,
         labels=rownames(h1h2),
         cex=0.7)

   })

   # output plotly
   output$preset_plotly <- plotly::renderPlotly({
      preset_kbl <- preset_kable();
      h1h2 <- as.data.frame(colorjam_presets(input$colorjam_preset))
      axis_at <- seq(from=-30, to=390, by=30);

      # update the preset_kable
      preset_kable(kable_coloring(
         row.names=FALSE,
         caption="Preset values for h1 and h2.",
         h1h2))

      # prepare editable shapes
      presetly <- plotly::plot_ly(mode="markers", type="scatter") %>%
         plotly::layout(
            xaxis=list(range=range(axis_at)),
            yaxis=list(range=range(axis_at)),
            shapes=lapply(seq_len(nrow(h1h2)), function(i){
               list(
                  type="circle",
                  xanchor=(h1h2$h1[i] %% 360),
                  yanchor=(h1h2$h2[i] %% 360),
                  # grey points with darker outline
                  fillcolor="gray80",
                  line=list(color="gray50"),
                  # 4-pixel radius
                  x0=-8, x1=8,
                  y0=-8, y1=8,
                  xsizemode="pixel",
                  ysizemode="pixel")
            })
         ) %>%
         plotly::config(
            edits=list(
               shapePosition=TRUE))

      # approximate intermediate points
      hseq <- seq(from=0, to=359)
      h1new <- approx_degrees(h1=h1h2$h2,
         h2=h1h2$h1,
         h=hseq)
      # approximate full saturation color by converting HCL to HSL
      hclhue2hsl <- function(hseq){
         hsl_seq <- round(col2hsl(hcl2col(H=hseq, C=60, L=70)))["H",]
         hsl_vals <- hsl2col(H=hsl_seq, S=100, L=50)
      }
      hsl_vals_new <- hclhue2hsl(h1new);
      h1h2new <- data.frame(h1=h1new,
         h2=hseq,
         color=hsl_vals_new)

      k <- 1:10;
      h1h2new$color <- factor(h1h2new$color, levels=(unique(h1h2new$color)))
      head(h1h2new$color)
      # h1h2new$color <- as.character(h1h2new$color)
      presetly <- presetly %>%
         plotly::add_trace(data=h1h2new,
            x=~h1,
            y=~h2,
            # color=~color,
            marker=list(color=h1h2new$color),
            showlegend=FALSE,
            mode="markers")
      # add sidebar colors
      presetly <- presetly %>%
         plotly::add_trace(data=h1h2new,
            x=-5,
            y=~h2,
            # color=~color,
            marker=list(color=h1h2new$color),
            showlegend=FALSE,
            mode="markers")
      # add sidebar reference color spectrum
      hsl_vals <- hclhue2hsl(hseq);
      # h1h2ref <- data.frame(h1=hseq, h2=-5, color=hsl_vals)
      presetly <- presetly %>%
         plotly::add_trace(
            x=hseq,
            y=-5,
            # color=~color,
            marker=list(color=hsl_vals),
            showlegend=FALSE,
            mode="markers")

      presetly
   })


   # reactive variable to hold previous edit
   preset_event_data <- shiny::reactiveVal(
      c(0, 0))

   # observe changes in the preset points
   # update x/y reactive values in response to changes in shape anchors
   observe({
      h1h2 <- as.data.frame(colorjam_presets(input$colorjam_preset))
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

            # edit h1h2 at the proper row
            h1h2 <- as.data.frame(colorjam_presets(input$colorjam_preset))
            jamba::printDebug("input$colorjam_preset:", input$colorjam_preset);# debug
            # jamba::printDebug("h1h2 (before):");print(h1h2);# debug
            h1h2[inum, c("h1", "h2")] <- c(new_x, new_y);
            # force values between 0 and 359
            h1h2$h2 <- h1h2$h2 %% 360;
            # jamba::printDebug("h1h2 (after):");print(h1h2);# debug
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
            # save new preset
            add_colorjam_preset(preset=new_preset,
               h1=h1h2$h1,
               h2=h1h2$h2)

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

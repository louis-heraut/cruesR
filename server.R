# Copyright 2023 Louis Héraut (louis.heraut@inrae.fr)*1
#                     
# *1   INRAE, France
#
# This file is part of cruesR R package.
#
# cruesR R package is free software: you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# cruesR R package is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with cruesR R package.
# If not, see <https://www.gnu.org/licenses/>.


# Define server
#' @title server
#' @description ...
#' @param input ...
#' @param output ...
#' @param session ...
#' @return ...
#' @examples
#' ...
#' @export
server = function(input, output, session) {
    # session$onSessionEnded(stopApp)

    js <- "
    function(el, x, inputName){
      var id = el.getAttribute('id');
      var gd = document.getElementById(id);
      var d3 = Plotly.d3;
      Plotly.update(id).then(attach);
        function attach() {
          gd.addEventListener('click', function(evt) {
            var xaxis = gd._fullLayout.xaxis;
            var yaxis = gd._fullLayout.yaxis;
            var bb = evt.target.getBoundingClientRect();
            var x = xaxis.p2d(evt.clientX - bb.left);
            var y = yaxis.p2d(evt.clientY - bb.top);
            var coordinates = [x, y];
            Shiny.setInputValue(inputName, coordinates);
          });
        };
  }
  "
    
    rv = reactiveValues(filename=NULL,
                        mode="",
                        x1_tmp=NA,
                        x1=NA,
                        x2_tmp=NA,
                        x2=NA,
                        lim=NULL,
                        id_selection=0,
                        id_linearise=0,
                        data=NULL,
                        data_load=NULL,
                        idDate=NA,
                        idValue=NA,
                        zoom=NULL,
                        linearise=tibble(),
                        selection=tibble(),
                        selection_full=tibble())

    
    observeEvent(input$upload, {
        deselect_mode(session, rv)
        if (!is.null(input$upload)) {
            rv$filename = gsub("[.].*$", "",
                               basename(input$upload$name))
            r = try(read.table(file=input$upload$datapath,
                               header=TRUE,
                               sep=";"), silent=TRUE)

            if (!any("try-error" %in% class(r)) & !any(is.na(r))) {
                r = read.table(file=input$upload$datapath,
                               header=TRUE,
                               sep=";")

                if (ncol(r) > 1) {
                    rv$data = as_tibble(read.table(
                        file=input$upload$datapath,
                        header=TRUE,
                        sep=";"))
                    
                    rv$idDate = 1
                    rv$idValue = 2

                    rv$data = rv$data[, c(rv$idDate, rv$idValue)]
                    
                    rv$data[[rv$idDate]] =
                        as.POSIXct(as.character(rv$data[[rv$idDate]]),
                                   tryFormats=tryFormats,
                                   tz="UTC")
                    
                    rv$data = distinct(
                        rv$data,
                        !!names(rv$data)[rv$idDate]:=
                            get(names(rv$data)[rv$idDate]),
                        .keep_all=TRUE)

                    from = min(rv$data[[rv$idDate]], na.rm=TRUE)
                    to = max(rv$data[[rv$idDate]], na.rm=TRUE)
                    by = min(diff(rv$data[[rv$idDate]]), na.rm=TRUE)
                    
                    Date = seq.POSIXt(from, to, by)
                    data_no_miss =
                        tibble(!!names(rv$data)[rv$idDate]:=Date)
                    rv$data = left_join(data_no_miss, rv$data,
                                        by=names(rv$data)[rv$idDate])
                    
                    rv$data_load = rv$data

                    showElement(id='ana_panel')
                    showElement(id='info_panel')
                    showElement(id='plot_panel')
                    
                } else {
                    rv$data = NULL
                }
            } else {
                rv$data = NULL
            }
        } else {
            rv$data = NULL
        }
    })



    observeEvent(input$mode_choice, {
        deselect_mode(session, rv)
        if (input$mode_choice == "Linearise") {
            hide(id='selection_row')
            hide(id='download_row')
            showElement(id='linearise_row')
            
        } else if (input$mode_choice == "Selection") {
            hide(id='linearise_row')
            hide(id='download_row')
            showElement(id='selection_row')
            
        } else if (input$mode_choice == "Download") {
            hide(id='linearise_row')
            hide(id='selection_row')
            showElement(id='download_row')
        }
    })

    observe({
        if (!is.null(input$linearise_select) |
            !is.null(input$selection_select)) {
            if (rv$mode != "select") {
                rv$mode = "select"
            }
        } else {
            if (rv$mode == "select") {
                deselect_mode(session, rv)
            }
        }
    })

    observeEvent(input$clickposition, {
        if (rv$mode == "select") {
            if (is.na(rv$x1_tmp)) {
                rv$x1_tmp = as.POSIXct(input$clickposition[1],
                                       tz="UTC")
            } else {
                rv$x2_tmp = as.POSIXct(input$clickposition[1],
                                       tz="UTC")
            }
        }
    })

    observeEvent({
        rv$x1_tmp
        rv$x2_tmp
    }, {
        if (rv$mode == "select" &
            !is.na(rv$x1_tmp) & !is.na(rv$x2_tmp)) {
            rv$x1 = rv$x1_tmp
            rv$x2 = rv$x2_tmp
            rv$x1_tmp = NA
            rv$x2_tmp = NA
        }
    })

    
    observeEvent(input$linearise.linearise_button, {
        if (!is.na(rv$x1) & !is.na(rv$x2)) {
            deselect_mode(session, rv)
            
            x1 = as.POSIXct(rv$x1, tz="UTC")
            x2 = as.POSIXct(rv$x2, tz="UTC")
            
            id1 = which.min(abs(rv$data[[rv$idDate]]-x1))
            y1 = rv$data[[rv$idValue]][id1]
            id2 = which.min(abs(rv$data[[rv$idDate]]-x2))
            y2 = rv$data[[rv$idValue]][id2]
            
            a = (y2-y1) / as.numeric(difftime(x2, x1, units="secs"))
            b = y1 - a*as.numeric(x1)

            ok = x1 <= rv$data[[rv$idDate]] &
                rv$data[[rv$idDate]] <= x2
            
            rv$data[[rv$idValue]][ok] =
                a*as.numeric(rv$data[[rv$idDate]][ok]) + b
            
            rv$id_linearise = rv$id_linearise + 1
            rv$linearise = bind_rows(
                rv$linearise,
                tibble(start=x1,
                       end=x2))
            rv$x1 = NA
            rv$x2 = NA
        }
    })

    observeEvent(input$linearise.remove_button, {
        deselect_mode(session, rv)
        range = rv$linearise[nrow(rv$linearise),]

        ok = range$start <= rv$data[[rv$idDate]] &
            rv$data[[rv$idDate]] <= range$end
        rv$data[[rv$idValue]][ok] = rv$data_load[[rv$idValue]][ok]
        
        rv$linearise = rv$linearise[1:(nrow(rv$linearise)-1),]
        rv$id_linearise = rv$id_linearise - 1
    })

    observeEvent(input$linearise.reset_button, {
        deselect_mode(session, rv)
        rv$id_linearise = 0
        rv$linearise = tibble()
    })
    
    observeEvent(input$selection.store_button, {
        deselect_mode(session, rv)
        rv$id_selection = rv$id_selection + 1
        xmin = min(c(rv$x1, rv$x2), na.rm=TRUE)
        xmax = max(c(rv$x1, rv$x2), na.rm=TRUE)
        data = filter(rv$data,
                      xmin <= rv$data[[rv$idDate]] &
                      rv$data[[rv$idDate]] <= xmax)
        rv$selection_full = bind_rows(
            rv$selection_full,
            tibble(data,
                   id_selection=rv$id_selection))
        rv$selection = bind_rows(
            rv$selection,
            tibble(start=xmin,
                   end=xmax))
        rv$x1 = NA
        rv$x2 = NA
    })

    observeEvent(input$selection.remove_button, {
        deselect_mode(session, rv)
        rv$selection = rv$selection[1:(nrow(rv$selection)-1),]
        rv$selection_full = filter(rv$selection_full,
                                   id_selection != rv$id_selection)
        rv$id_selection = rv$id_selection - 1
    })

    observeEvent(input$selection.reset_button, {
        deselect_mode(session, rv)
        rv$id_selection = 0
        rv$selection = tibble()
        rv$selection_full = tibble()
    })



    info = reactive({
        if (nrow(rv$selection) == 0 & is.na(rv$x1) & is.na(rv$x2)) {
            "<font size='3' color='#00a3a6'>Select a period on the graph and click on the </font><font size='3' color='#66c1bf'><b>Store</b></font><font size='3' color='#00a3a6'> button</font>"
        } else {

            start = rev(rv$selection$start)
            end = rev(rv$selection$end)
            
            if (!is.na(rv$x1) & !is.na(rv$x2)) {
                xmin = min(c(rv$x1, rv$x2))
                xmax = max(c(rv$x1, rv$x2))
                start = c(xmin, start)
                end = c(xmax, end)
            }
            
            date_start = format(start, "%d %b %Y")
            time_start = format(start, "%H:%M:%S")
            date_end = format(end, "%d %b %Y")
            time_end = format(end, "%H:%M:%S")

            
            tmp = paste0(
                "<font color='", darkCyanCOL,"'> ",
                "<b><font size='3'> ", date_start, "</font></b>",
                "<font size='2'> ", time_start, "</font>",
                " / ",
                "<b><font size='3'> ", date_end, "</font></b>",
                "<font size='2'> ", time_end, "</font></font>")

            if (!is.na(rv$x1) & !is.na(rv$x2)) {
                tmp[1] = gsub("275662", "66c1bf", tmp[1])
            }
            
            tmp = head(tmp, n=4)
            
            paste0(tmp, collapse="&emsp;")
        }
    })

    output$info = renderUI({
        HTML(info())
    })

    observeEvent(input$selection.download_button, {
        output$downloadData = downloadHandler(
            filename = function () {
                paste0("period_selection_for_",
                       rv$filename,
                       ".txt")
            },
            content = function (file) {
                write.table(rv$selection_full,
                            file,
                            sep=";",
                            row.names=FALSE)
            }
        )
        jsinject = "setTimeout(function()
                        {window.open($('#downloadData')
                        .attr('href'))}, 100);"
        session$sendCustomMessage(type='jsCode',
                                  list(value=jsinject))
    })

    
    # React to changes in date range input
    observeEvent({
        rv$data
        rv$x1
        rv$x2
    }, {

        if (!is.null(rv$data)) {
            output$plot = renderPlotly({
                
                validate(need(!is.null(input$upload),
                              message=FALSE))
                
                p = plot_ly()

                x_load = rv$data_load[[rv$idDate]]
                y_load = rv$data_load[[rv$idValue]]
                
                x = rv$data[[rv$idDate]]
                y = rv$data[[rv$idValue]]
                maxValue_win = max(y, na.rm=TRUE)*1.05
                x_label = names(rv$data)[rv$idDate]
                y_label = names(rv$data)[rv$idValue]

                xmin = min(c(rv$x1, rv$x2), na.rm=TRUE)
                xmax = max(c(rv$x1, rv$x2), na.rm=TRUE)

                if (nrow(rv$selection) > 0) {
                    for (i in 1:nrow(rv$selection)) {
                        p = add_trace(
                            p,
                            type="scatter",
                            mode="lines",
                            x=c(rv$selection[[1]][i],
                                rv$selection[[1]][i],
                                rv$selection[[2]][i],
                                rv$selection[[2]][i],
                                rv$selection[[1]][i]),
                            y=c(0, maxValue_win,
                                maxValue_win, 0, 0),
                            fill="toself",
                            opacity=0.4,
                            fillcolor=darkCyanCOL,
                            line=list(width=0),
                            text=paste0(
                                "from <b>",
                                rv$selection[[1]][i], "</b>",
                                " to <b>", rv$selection[[2]][i],
                                "</b>"),
                            hoverinfo="text",
                            hoveron="fills",
                            hoverlabel=
                                list(bgcolor=darkCyanCOL,
                                     font=list(color="white",
                                               size=12),
                                     bordercolor="white"))
                    }
                }

                p = add_trace(
                    p,
                    type="scatter",
                    mode="lines",
                    x=x_load,
                    y=y_load,
                    line=list(color=INRAECyanCOL,
                              width=1.2),
                    xhoverformat="%d/%m/%Y",
                    hovertemplate = paste0(
                        x_label, " ", "%{x}<br>",
                        "<b>", y_label, "</b> load %{y}",
                        "<extra></extra>"),
                    hoverlabel=list(
                        bgcolor=INRAECyanCOL,
                        font=list(size=12,
                                  color="white"),
                        bordercolor="white"))
                
                p = add_trace(
                    p,
                    type="scatter",
                    mode="lines",
                    x=x,
                    y=y,
                    line=list(color=grey94COL, width=1.2),
                    xhoverformat="%d/%m/%Y",
                    hovertemplate = paste0(
                        x_label, " ", "%{x}<br>",
                        "<b>", y_label, "</b> %{y}",
                        "<extra></extra>"),
                    hoverlabel=list(
                        bgcolor="#161B22",
                        font=list(size=12,
                                  color="white"),
                        bordercolor="white"))

                if (!is.na(rv$x1) & !is.na(rv$x2)) {
                    p = add_trace(
                        p,
                        type="scatter",
                        mode="lines",
                        x=c(rv$x1, rv$x1,
                            rv$x2, rv$x2, rv$x1),
                        y=c(0, maxValue_win,
                            maxValue_win, 0, 0),
                        fill="toself",
                        opacity=0.4,
                        fillcolor=lightCyanCOL,
                        line=list(width=0),
                        text=paste0(
                            "from <b>",
                            xmin, "</b>",
                            " to <b>", xmax,
                            "</b>"),
                        hoverinfo="text",
                        hoveron="fills",
                        hoverlabel=
                            list(bgcolor=lightCyanCOL,
                                 font=list(color="white",
                                           size=12),
                                 bordercolor="white"))
                }
                
                p = layout(
                    p,
                    
                    autosize=FALSE,
                    separators='. ',
                    width=input$dimension[1]-45,
                    height=340,
                    margin=list(l=0,
                                r=0,
                                b=0,
                                t=0,
                                pad=0),
                    
                    xaxis=list(showgrid=FALSE,
                               range=rv$lim,
                               ticks="outside",
                               tickcolor="#516b90",
                               tickfont=
                                   list(color="#b3c0d4"),
                               showticklabels=TRUE),
                    
                    yaxis=list(
                        range=c(0, maxValue_win),
                        title=list(
                            text=paste0(
                                "<b>",
                                y_label,
                                "</b>"),
                            font=
                                list(color="#405472")),
                        gridcolor="#1e2735",
                        gridwidth=0.6,
                        ticks="outside",
                        tickcolor="#516b90",
                        tickfont=list(color="#b3c0d4"),
                        zerolinecolor="#1e2735",
                        zerolinewidth=1.4,
                        fixedrange=TRUE),

                    autosize=FALSE,
                    plot_bgcolor='transparent',
                    paper_bgcolor='transparent',
                    showlegend=FALSE)

                p = config(
                    p,
                    locale="fr",
                    displaylogo=FALSE,
                    toImageButtonOptions =
                        list(format="svg"),
                    modeBarButtonsToRemove =
                        list("lasso2d",
                             "select2d",
                             "drawline",
                             # "zoom2d",
                             "drawrect",
                             "autoScale2d",
                             "hoverCompareCartesian",
                             "hoverClosestCartesian")
                )
                p = event_register(p, 'plotly_relayout')
                onRender(p, js, data="clickposition")
            })
        } else {
            output$plot = renderPlotly({
                p = plot_ly()
            })
        }
    })

    observe({
        rv$zoom = event_data("plotly_relayout")
    })

    observeEvent({
        rv$zoom
        rv$data
    }, {        
        if(is.null(rv$zoom) ||
           names(rv$zoom[1]) %in% c("xaxis.autorange", "width")) {
            xmin = min(rv$data[[rv$idDate]], na.rm=TRUE)
            xmax = max(rv$data[[rv$idDate]], na.rm=TRUE)
        } else {
            xmin = gsub("[.][[:digit:]]+$", "", rv$zoom$`xaxis.range[0]`)
            xmax = gsub("[.][[:digit:]]+$", "", rv$zoom$`xaxis.range[1]`)
            xmin = as.POSIXct(xmin, tryFormats=tryFormats, tz="UTC")
            xmax = as.POSIXct(xmax, tryFormats=tryFormats, tz="UTC")

        }
        rv$lim = c(xmin, xmax)
    })



    observeEvent(input$arrowRight, { 
        if(!is.null(rv$zoom) &
           !(names(rv$zoom[1]) %in% c("xaxis.autorange",
                                      "width"))) {
            rv$lim = rv$lim + years(1)
        }
    })

    observeEvent(input$arrowLeft, { 
        if(!is.null(rv$zoom) &
           !(names(rv$zoom[1]) %in% c("xaxis.autorange",
                                      "width"))) {
            rv$lim = rv$lim - years(1)
        }
    })
    
    


}

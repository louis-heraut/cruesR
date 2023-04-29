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


ui = bootstrapPage(
    
    tags$head(tags$script('
	var dimension = [0, 0];
	$(document).on("shiny:connected", function(e) {
		dimension[0] = window.innerWidth;
		dimension[1] = window.innerHeight;
		Shiny.onInputChange("dimension", dimension);
	});
	$(window).resize(function(e) {
		dimension[0] = window.innerWidth;
		dimension[1] = window.innerHeight;
		Shiny.onInputChange("dimension", dimension);
	});
    ')),

    useShinyjs(),

    downloadLink("downloadData", label=""),
    tags$head(tags$script(HTML('Shiny.addCustomMessageHandler("jsCode",
                                function(message) {
                                eval(message.value);});'))),

    includeCSS("www/ui.css"),

    absolutePanel(
        id="bar_panel",
        class="Panel card-bar",
        fixed=TRUE,
        width="100%",
        height="100px",
        left=0, top=0,
        div(img(src="back.jpg", width="100%", height="100%",
                style='position: absolute;
                       object-fit: cover;'))),

    absolutePanel(
        id="INRAE_panel",
        class="Panel card-empty",
        fixed=TRUE,
        width="auto",
        height="50px",
        left=30, top=18,
        div(Button(class="Button-INRAE",
                   'inrae_button',
                   label=NULL,
                   icon_name=iconLib$INRAE,
                   tooltip="INRAE"))),

    absolutePanel(
        id="description_panel",
        class="Panel card-empty",
        fixed=TRUE,
        width="300px",
        height="50px",
        right=100, top=15,
        div(h4(HTML("<b>cruesR</b> is web application that make time series events selection simpler")))),

    absolutePanel(
        id="about_panel",
        class="Panel card-empty",
        fixed=TRUE,
        width="auto",
        height="50px",
        right=10, top=70,
        div(Button(class="Button-default",
                   'about_button',
                   label=NULL,
                   icon_name=iconLib$help_outline_white,
                   tooltip="About"))
    ),

    absolutePanel(
        id="upload_panel",
        class="Panel card-ana",
        fixed=TRUE,
        height="auto",
        left=10, top=110,

        div(class="Row",
            div(class="row-label",
                HTML(paste0("<span><b>Data</b></span>"))),
            div(class="sep"),
            div(class="bunch",
                fileInput(inputId="upload",
                          label="")))
    ),

    
    hidden(
        absolutePanel(
            id='ana_panel',
            class="Panel card-ana",
            fixed=TRUE,
            height="auto",
            left=10, top=170,

            div(class="Row",
                div(class="row-label",
                    HTML(paste0("<span><b>Mode</b></span>"))),
                div(class="sep"),
                div(class="bunch",
                    radioButton(
                        class="radioButton",
                        inputId="mode_choice",
                        choiceNames=c("Linearise",
                                      "Selection",
                                      "Download"),
                        selected="Linearise",
                        choiceTooltips=c(
                            "Linearise some parts of a chronic",
                            "Selection of high flow events",
                            "Download your work")))),

            hidden(
                div(class="Row", id="linearise_row",
                    div(class="row-label",
                        HTML(paste0("<span><b>Linearise</b></span>"))),
                    div(class="sep"),
                    div(class="bunch",

                        selectButton(class="selectButton",
                                     inputId="linearise_select",
                                     label="Select",
                                     icon_name=iconLib$data_array_white,
                                     selected=FALSE,
                                     tooltip=
                                         "Select a period between two clicks"),

                        Button(class="Button",
                               inputId="linearise.linearise_button",
                               label="Linearise",
                               icon_name=iconLib$done_white,
                               tooltip="Linearise the selected period"),
                        
                        Button(class="Button",
                               inputId="linearise.remove_button",
                               label="Remove",
                               icon_name=iconLib$remove_white,
                               tooltip="Remove last linearise period"),
                        
                        Button(class="Button",
                               inputId="linearise.reset_button",
                               label="Reset",
                               icon_name=iconLib$refresh_white,
                               tooltip="Reset all linearise period")))
            ),
            
            hidden(
                div(class="Row", id="selection_row",
                    div(class="row-label",
                        HTML(paste0("<span><b>Selection</b></span>"))),
                    div(class="sep"),
                    div(class="bunch",

                        selectButton(class="selectButton",
                                     inputId="selection_select",
                                     label="Select",
                                     icon_name=iconLib$data_array_white,
                                     selected=FALSE,
                                     tooltip=
                                         "Select a period between two clicks"),

                        Button(class="Button",
                               inputId="selection.store_button",
                               label="Store",
                               icon_name=iconLib$add_white,
                               tooltip="Store the selected period"),
                        
                        Button(class="Button",
                               inputId="selection.remove_button",
                               label="Remove",
                               icon_name=iconLib$remove_white,
                               tooltip="Remove last selected period"),
                        
                        Button(class="Button",
                               inputId="selection.reset_button",
                               label="Reset",
                               icon_name=iconLib$refresh_white,
                               tooltip="Reset all selected period")))
            ),

            hidden(
                div(class="Row", id="download_row",
                    div(class="row-label",
                        HTML(paste0("<span><b>Download</b></span>"))),
                    div(class="sep"),
                    div(class="bunch",
                        
                        Button(class="Button",
                               inputId="selection.download_button",
                               label="All",
                               icon_name=NULL,
                               tooltip=
                                   "Download all the selected period")))
            )
        )
    ),

    hidden(
        absolutePanel(
            id='info_panel',
            class="Panel card-info",
            fixed=TRUE,
            width="auto",
            left=10, top=250,

            div(class="Row",
                div(class="row-label",
                    HTML(paste0("<span><b>Storage</b></span>"))),
                div(class="sep"),
                div(htmlOutput(outputId="info")))
        )
    ),

    hidden(
        absolutePanel(
            id='plot_panel',
            class="Panel card-insert-c",
            fixed=TRUE,
            width="auto",
            top=310,
            
            div(style="margin-top: 10px;
                   margin-left: 10px;
                   margin-bottom: 10px;
                   margin-right: 20px;",
                plotly::plotlyOutput("plot",
                                     width="auto",
                                     height="auto"))
        )
    )




    
    
)

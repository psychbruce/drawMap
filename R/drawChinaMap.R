#### Standard China Map ####

## China Map Template
if(FALSE) {
  bou=rgdal::readOGR("data-raw/bou2_4p.shp")
  bou@data$id=rownames(bou@data)
  maptemp=dplyr::full_join(ggplot2::fortify(bou), bou@data, by="id")
  usethis::use_data(maptemp, overwrite=TRUE)

  provdata_temp=rio::import("data-raw/provdata_demo.xlsx", sheet="prov")
  usethis::use_data(provdata_temp, overwrite=TRUE)

  provdata_demo=rio::import("data-raw/provdata_demo.xlsx", sheet="demo")
  usethis::use_data(provdata_demo, overwrite=TRUE)
}

#' Draw standard China map
#' @import ggplot2
#' @importFrom cowplot ggdraw draw_plot save_plot
#' @importFrom glue glue_col
#' @importFrom crayon green blue
#' @param provdata Province-level data.
#' You can use \code{\link[dplyr]{left_join}} or \code{\link[dplyr]{right_join}}
#' to merge your province-level data with \code{provdata_temp} (see Examples).
#' @param citydata City-level data with two variables (must be "geoE" and "geoN") specifying the longitude and latitude of cities, respectively.
#' @param var.prov The variable of provinces, e.g., \code{"prov"}.
#' @param var The variable you want to map on the plot.
#' @param multiply A number useful when you want to expand the raw values by, e.g., 100 times.
#' @param log \code{TRUE} or \code{FALSE} (default). Whether to log-transform the raw values.
#' @param nsmall Number of decimal places of output. Default is 0.
#' @param colors Color palettes. The following palettes are available (see \code{\link[ggplot2]{scale_color_brewer}}):
#'
#' \strong{Sequential:}
#' \code{Blues, Greens, Greys, Oranges, Purples, Reds,
#' BuGn, BuPu, GnBu, OrRd, PuBu, PuRd, RdPu, YlGn,
#' PuBuGn, YlGnBu, YlOrBr, YlOrRd}
#'
#' \strong{Diverging:}
#' \code{BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral}
#'
#' \strong{Qualitative (not suggested):}
#' \code{Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3}
#' @param direc \code{1} (default) or \code{-1}, specifying the direction of color palette.
#' @param cityshape The shape of city dots. I recommend using 16 (round) or 18 (rhombus). Default is 18. For details, see \href{http://sape.inf.usi.ch/quick-reference/ggplot2/shape}{shape parameter}.
#' @param cityalpha The transparency of city dots. Default is 0.9.
#' @param addlabel \code{TRUE} (default) or \code{FALSE}. Whether to add value labels. For clarity, value labels are only added to provinces but not to cities.
#' @param labelprefix A character specifying a variable in your data for adding label prefix, usually \code{"prov"} if you want to add the names of provinces prior to values.
#' (Note: You can draw the label prefix only, by setting \code{addlable=FALSE} and \code{labelprefix="yourvariable"}.)
#' @param labelseg A character specifying the joint character between label prefix and values (e.g., setting to \code{": "} will make a label look like \code{"Beijing: 1.23"}).
#' @param tag Tag of the map (left-top corner). Default is \code{""}, which leaves the position occupied but without any text.
#' @param title Title of the map. Default is the variable name.
#' @param subtitle Subtitle of the map. Default is \code{NULL}, which does not occupy the position.
#' @param guidetitle Title of the colorbar guide.
#' @param addguidelabel \code{TRUE} (default) or \code{FALSE}. Whether to add values under the colorbar guide.
#' @param guidelimits [Optional] A number vector specifying the range of values to plot (relevant both to the main plot and to the colorbar guide). Default is the actual range of your variable.
#' @param guidebreaks [Optional] A number vector specifying the breaking points of colorbar, e.g., \code{seq(0, 100, 25)}.
#' @param guidelabels [Optional] A vector re-setting the labels of the colorbar guide.
#' @param bordersize Line size of map border. Default is \code{0.2}.
#' @param bordercolor Line color of map border. Default is \code{"grey70"}.
#' @param na.color A color for those provinces with missing values. Default is \code{"grey90"}.
#' @param font Text font.
#' @param filename File name to create on disk. The file type can be any of ".pdf", ".png", ".jpg", ".bmp", ".tiff", ".eps", ... (see \code{\link[ggplot2]{ggsave}}).
#' @param dpi Dots per inch (DPI). A higher DPI produces clearer and more detailed output. Academic papars usually require 300 dpi at least. Here I use 500 as a default value.
#' (Note: PDF documents are not influenced by DPI.)
#' @return Invisibly return a list of two maps (a main map and a sub-map for Nanhai islands).
#' @examples
#' ## Template
#' View(provdata_temp)  # a template province-level dataset
#' drawChinaMap()  # draw a template of China map (no variables)
#' drawChinaMap(provdata_temp, var="geoE", nsmall=1, filename="ChinaMap1.png")
#' drawChinaMap(provdata_temp, var="geoN", nsmall=1, colors="Reds", direc=-1, addlabel=FALSE, filename="ChinaMap2.png")
#'
#' ## How to use it with a real dataset?
#' View(provdata_demo)  # a demo dataset (per capita GDP for 31 mainland provinces)
#'
#' # Method 1: Use the 'var.prov' parameter
#' drawChinaMap(provdata_demo, var.prov="Province", var="GDPpc", nsmall=0, filename="ChinaMap_GDPpc.png")
#'
#' # Method 2: Use dplyr::left_join() or dplyr::right_join() to merge datasets
#' provdata=dplyr::right_join(provdata_temp, provdata_demo, by=c("prov"="Province"))
#' drawChinaMap(provdata, var="GDPpc", nsmall=0, title="GDP per capita", filename="ChinaMap_GDPpc.png")
#' @export
drawChinaMap=function(provdata=NULL, citydata=NULL,
                      var.prov="prov",
                      var=NA, multiply=1, log=FALSE, nsmall=0,
                      colors="Blues", direc=1,
                      cityshape=18, cityalpha=0.9,
                      addlabel=TRUE, labelprefix="", labelseg=": ",
                      tag="", title=var, subtitle="",
                      guidetitle="", addguidelabel=TRUE,
                      guidelimits=NULL, guidebreaks=NULL, guidelabels=NULL,
                      bordersize=0.2, bordercolor="grey70", na.color="grey90",
                      font="Arial",
                      filename="ChinaMap.png", dpi=500) {
  # Merge data
  if(is.null(citydata)) {
    level="prov"
    if(is.null(provdata)) {
      provdata=provdata_demo
      title="China Map (demo)"
      labelprefix="prov"
    }
    if(!"NAME" %in% names(provdata)) {
      provdata=dplyr::right_join(provdata_temp, provdata, by=c("prov"=var.prov))
    }
    data=provdata
  } else {
    level="city"
    data=citydata
  }
  suppressWarnings({
    mapdata=dplyr::full_join(maptemp, provdata, by="NAME")
  })

  # Basic settings
  map.long=c(73.4, 135.1)
  map.lat=c(17.4, 53.6)
  jdx.long=c(108.5, 121.5)
  jdx.lat=c(5.5, 25)
  jdx=data.frame(ID=rep(1:10, each=2),
                 long=c(109.10, 109.80,
                        110.20, 109.90,
                        108.56, 108.55,
                        112.35, 113.90,
                        116.85, 117.75,
                        119.50, 119.90,
                        119.80, 119.75,
                        119.60, 119.90,
                        120.80, 121.70,
                        122.60, 123.00),
                 lat=c(16.40, 15.20,
                       13.30, 12.00,
                        9.10,  7.70,
                        5.45,  5.80,
                        8.50,  9.50,
                       11.80, 13.00,
                       14.70, 15.90,
                       17.50, 18.70,
                       20.40, 21.30,
                       23.20, 24.40))
  maptheme=theme_void() +
    theme(legend.position=c(0.24, ifelse(guidetitle!="" & addguidelabel==FALSE, 0.05, 0.07)),
          legend.title=element_text(size=14, color="black"),
          plot.tag=element_text(size=16, color="black", face="bold",
                                margin=margin(-1, 0, ifelse(is.null(title), 0, 1), 0.5, "lines")),
          plot.title=element_text(size=16, color="black", face="bold", hjust=0.5,
                                  margin=margin(-0.5, 0, 0, 0, "lines")),
          plot.subtitle=element_text(size=14, color="black", face="bold", hjust=0.5,
                                     margin=margin(1.5, 0, -1.5, 0, "lines")))
  mapguide=guide_colorbar(title=guidetitle, title.position="top", title.hjust=0.5,
                          direction="horizontal", label=addguidelabel, ticks=FALSE,
                          barwidth=unit(5,"cm"), barheight=unit(5,"mm"))
  if(is.na(var)==FALSE) {
    if(log) {
      guide.range=log(range(data[[var]], na.rm=TRUE))
    } else {
      guide.range=range(data[[var]], na.rm=TRUE)
    }
    if(is.null(guidelimits)) guidelimits=guide.range
    if(is.null(guidebreaks)) guidebreaks=guide.range
    if(is.null(guidelabels)) guidelabels=sprintf(paste0("%.", nsmall, "f"), guide.range*multiply) # c(floor(guide.range[1]), ceiling(guide.range[2]))
  }
  windowsFonts(FONT=windowsFont(font))

  # Draw maps
  map=ggplot() + maptheme
  if(is.na(var) | level=="city") {
    bordercolor="grey30"
    map=map + geom_polygon(data=mapdata, aes(x=long, y=lat, group=group), fill="grey95", color=bordercolor, size=bordersize)
  } else {
    if(log) {
      map=map + geom_polygon(data=mapdata, aes(x=long, y=lat, group=group, fill=log(get(var))), color=bordercolor, size=bordersize)
    } else {
      map=map + geom_polygon(data=mapdata, aes(x=long, y=lat, group=group, fill=get(var)), color=bordercolor, size=bordersize)
    }
    map=map +
      scale_fill_distiller(palette=colors, direction=direc, na.value=na.color,
                           limits=guidelimits, breaks=guidebreaks, labels=guidelabels,
                           guide=mapguide)
  }
  if(level=="city") {
    if(log) {
      map=map + geom_point(data=citydata, aes(x=geoE, y=geoN, color=log(get(var))), shape=cityshape, size=3, alpha=cityalpha)
    } else {
      map=map + geom_point(data=citydata, aes(x=geoE, y=geoN, color=get(var)), shape=cityshape, size=3, alpha=cityalpha)
    }
    map=map +
      scale_color_distiller(palette=colors, direction=direc, na.value=na.color,
                            limits=guidelimits, breaks=guidebreaks, labels=guidelabels,
                            guide=mapguide)
  }
  map1=map +
    coord_map("lambert",  # or "albers"
              parameters=c(25, 47),
              xlim=map.long, ylim=map.lat)
  map2=ggplot() + theme_void() +
    geom_polygon(data=mapdata, aes(x=long, y=lat, group=group), fill="grey95", color="grey30", size=0.2) +
    geom_line(data=jdx, aes(x=long, y=lat, group=ID), color="black", size=0.5) +
    coord_map("lambert", parameters=c(25, 47), xlim=jdx.long, ylim=jdx.lat) +
    theme(legend.position="none",
          panel.border=element_rect(color="black", fill=NA, size=0.5))

  # Add labels
  if(level=="prov") {
    if((is.na(var)==TRUE | addlabel==FALSE) & labelprefix!="") {
      map1=map1 + geom_text(data=provdata, aes(x=geoE, y=geoN, label=get(labelprefix)), fontface="bold", size=3, family="FONT")
    }
    if(is.na(var)==FALSE & addlabel==TRUE & labelprefix=="") {
      map1=map1 + geom_text(data=provdata, aes(x=geoE, y=geoN, label=sprintf(paste0("%.", nsmall, "f"), get(var)*multiply)), size=3, family="FONT")
    }
    if(is.na(var)==FALSE & addlabel==TRUE & labelprefix!="") {
      map1=map1 + geom_text(data=provdata, aes(x=geoE, y=geoN, label=paste0(get(labelprefix), labelseg, sprintf(paste0("%.", nsmall, "f"), get(var)*multiply))), size=3, family="FONT")
    }
  }
  map1=map1 + labs(tag=tag, title=title, subtitle=subtitle) +
    theme(text=element_text(family="FONT", face="bold"))

  # Output (with 'cowplot' package)
  suppressWarnings({
    save_plot(filename, base_width=8, base_height=6, dpi=dpi,
              plot=ggdraw() +
                draw_plot(map1) +
                draw_plot(map2, x=0.76, y=0.06, width=0.2, height=0.2))
  })

  # Feedback
  path=ifelse(grepl(":", filename), filename, paste0(getwd(), '/', filename))
  print(glue_col("{green \u2714} Saved to {blue '{path}'}"))

  invisible(list(map.main=map1, map.jdx=map2))
}

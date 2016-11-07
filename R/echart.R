#' Create an ECharts widget
#'
#' Create an HTML widget for ECharts that can be rendered in the R console, R
#' Markdown documents, or Shiny apps. You can add more components to this widget
#' and customize options later using <setEchartWidgets> functions, <aesthetic>
#' functions, and others.. \code{eChart()} is an alias of \code{echart()}.
#'
#' @param data a data object (usually a data frame or a list)
#' @rdname eChart
#' @export
#' @examples library(recharts2)
#' echart(iris, Sepal.Length, Sepal.Width)
#' echart(iris, Sepal.Length, Sepal.Width, series = Species)
echart = function(data, ...) {
    UseMethod('echart')
}

#' Create an ECharts widget
#'
#' @param width width
#' @param height height
#' @export
#' @rdname eChart
echart.list = function(data, width = NULL, height = NULL,  ...) {
    htmlwidgets::createWidget(
        'echarts', x = data, width = width, height = height,
        package = 'recharts2'
    )
}

#' Create an ECharts widget
#'
#' @param x independent variable(s). Some charts calls for one, some calls for two.
#' @param y dependent variable(s). Most charts calls for one, but some calls for more.
#' @param series data series variables.
#' \describe{
#'   \item{single coordinate system}{In Cartesian Coordinate System (bar, scatter, k, ...) and
#'   some other charts (venn), series represents data series. Different data series
#'   are displayed in the same coordinate system}
#'   \item{multiple coordinate system}{In polar coordinate system (pie, radar, ...) and
#'   some other charts (map, tree, ...), series represents index of coordinate system.
#'   Different coordinate system indices are displayed in paralell coordinate systems.}
#' }
#' @param t timeline variable. When \code{t} is defined, recharts2 builds a timeline
#' widget to show the changes along with time.
#' @param weight In some charts (bar, bubble, line, ...), weight represents the size
#' of the graph elements.
#' @param facet The variable as factor mapping to facets.
#' @param lat latitude variable (-180 ~ 180) for map/heatmap
#' @param lng longitude variable (-90 ~ 90) for map/heatmap
#' @param type chart type. Now recharts2 supports major types of
#' \itemize{
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_01_Scatterplot.html}{scatter/point/bubble}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_02_Bar.html}{bar/column/histogram}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_03_Line.html}{line/area/curve/wave}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_04_K.html}{candlestick}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_05_eventRiver.html}{eventRiver}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_06_boxplot.html}{boxplot}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_21_Pie.html}{pie/ring/rose}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_22_Funnel.html}{funnel/pyramid}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_23_Radar.html}{radar/spider/star}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_11_Force.html}{force/force_line}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_12_Chord.html}{chord}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_24_Gauge.html}{gauge}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_13_WordCloud.html}{wordCloud}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_14_Venn.html}{venn}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_31_Map.html}{map_world/map_china/map_world_multi/map_china_multi}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_32_Tree.html}{tree/htree/tree_inv/htree_inv}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_33_Treemap.html}{treemap}
#'   \item \href{http://madlogos.github.io/recharts2/Basic_Plots_15_Heatmap.html}{heatmap}
#' }
#' @param subtype some chart types support subtypes.
#' \describe{
#'   \item{bar/column}{"stack"}
#'   \item{histogram}{c("count","freq","density")}
#'   \item{line/area}{c("stack","smooth","dotted","solid","dashed")}
#'   \item{curve/wave}{c("stack","solid")}
#'   \item{map_world/world_map/map_world_multi/world_map_multi}{c("sum","average",
#'   [Valid Country Names])}
#'   \item{map_china/china_map/map_china_multi/china_map_multi}{c("sum","average",
#'   [Valid China Province Names])}
#'   \item{pie}{c("multi","clock","clockwise")}
#'   \item{ring}{c("info","multi","clock","clockwise")}
#'   \item{rose}{c("area","radius","multi","clock","clockwise")}
#'   \item{chord}{c("ribbon","scale","scaletext","rotatelab","hidelab","clock",
#'   "clockwise","asc","desc","ascsub","descsub")}
#'   \item{force/force_line}{c("arrow","triangle")}
#'   \item{funnel/pyramid}{c("left","center","right")}
#'   \item{tree/vtree/htree/tree_inv/vtree_inv/htree_inv}{c("curve","broken",
#'   "dotted","solid","dashed")}
#'   \item{radar/spider/star}{"fill"}
#' }
#' @param ... Other params to pass to echarts object
#'
#' @import compiler
#' @export
#' @references
#' Online Manual: \url{http://madlogos.github.io/recharts2}
#' @export
#' @rdname eChart
echart.data.frame = function(
    data, x = NULL, y = NULL, series = NULL, t = NULL, weight = NULL,
    facet = NULL, lat = NULL, lng = NULL, type = 'auto', subtype = NULL,
...) {

    if (is.null(data)){
        data = data.frame(x='', stringsAsFactors=FALSE)
        dataVars = list('x')
        xvarRaw = 'x'
        hasT = FALSE
        hasF = FALSE
        xlab = ylab = ''
    }else{
        lapply(seq_along(names(data)), function(j){
            if (is.character(data[,j]))
                data[,j] = enc2native(data[,j])
        })
        if (inherits(data, 'data.frame')) data = as.data.frame(data)

        #-------------get all arguments as a list-----------------
        vArgs = as.list(match.call(expand.dots=TRUE))
        dataVars = intersect(names(vArgs), c(
            'x', 'y', 't', 'series', 'weight', 'facet', 'lat', 'lng'))
        vArgsRaw = vArgs[dataVars]  # original arg names
        vArgs = lapply(vArgsRaw, function(v) {
            symbols = all.names(v)
            if (any(symbols %in% names(data)))
                return(as.symbol(symbols[symbols %in% names(data)]))
            # should be sapply(symbols[symbols %in% names(data)], as.symbol)
            v
        })  # get arg names corresponding to data vars

        # ------------extract var names and values-----------------
        eval(parse(text=paste0(names(vArgs), "var = evalVarArg(",
                               sapply(vArgs, deparse), ", data, eval=FALSE)")))
        eval(parse(text=paste0(names(vArgs), "varRaw = evalVarArg(",
                               sapply(vArgsRaw, deparse), ", data, eval=FALSE)")))
        eval(parse(text=paste0(names(vArgsRaw), " = evalVarArg(",
                               sapply(vArgsRaw, deparse), ", data)")))
        hasT = ! is.null(t)
        hasF = ! is.null(facet)
        if (!is.null(facet))
            for (i in seq_along(facetvarRaw)){
                if (!is.factor(data[,facetvarRaw[i]]))
                    data[,facetvarRaw[i]] = as.factor(data[,facetvarRaw[i]])
            }
        if (!is.null(series))
            for (i in seq_along(seriesvar)){
                if (!is.factor(data[,seriesvar[i]]))
                    data[,seriesvar[i]] = as.factor(data[,seriesvar[i]])
            }
        if (!is.null(x))
            for (i in seq_along(xvarRaw)){
                if (is.factor(data[,xvarRaw[i]]))
                    data[,xvarRaw[i]] = as.character(data[,xvarRaw[i]])
            }
        if (!is.null(y))
            for (i in seq_along(yvarRaw)){
                if (is.factor(data[,yvarRaw[i]]))
                    data[,yvarRaw[i]] = as.character(data[,yvarRaw[i]])
            }
        # ------------------x, y lab(s)----------------------------
        #xlab = ylab = NULL
        if (!missing(x)) xlab = as.character(vArgsRaw$x) else xlab = "x"
        if (!missing(y)) ylab = as.character(vArgsRaw$y) else ylab = "Freq"
        xlab = sapply(xlab, autoArgLabel, auto=deparse(substitute(xvar)))
        ylab = sapply(ylab, autoArgLabel, auto=deparse(substitute(xvar)))
        xlab = gsub("^\"|\"$", "", xlab)
        ylab = gsub("^\"|\"$", "", ylab)
        if (length(ylab) == 0) ylab = "Freq"
    }

    # ----------------Layout, type, subtype---------------------
    layout = arrangeLayout(
        if ('series' %in% dataVars) data.frame(data[, seriesvar]) else NULL,
        if ('facet' %in% dataVars) data.frame(data[, facetvarRaw]) else NULL
    )
    layout$type = if (is.null(type)) NA else tolower(matchLayout(type, layout))
    layout$subtype = if (is.null(subtype)) NA else
        matchLayout(tolower(subtype), layout)
    auto.layout = autoMultiChartLayout(max(layout$ifacet))
    if ('facet' %in% dataVars) if (length(facetvarRaw)>1)
        auto.layout = autoMultiChartLayout(
            max(layout$ifacet), max(layout$irow), max(layout$icol))
    facets = data.frame(ifacet=unique(layout$ifacet))
    facets[,c('centerX', 'centerY')] = auto.layout$centers
    facets[,c('radius', 'width', 'height')] = list(
        auto.layout$radius,  auto.layout$width, auto.layout$height)
    facets[,c('left', 'top', 'right', 'bottom')] = auto.layout$grids
    layout = merge(layout, facets, by='ifacet', sort=FALSE)



    # -----------------split df to lists---------------------
    .makeMetaDataList = cmpfun(function(df) {
        # generate metaData, df wrapped in lists

        vars = sapply(dataVars, function(x) {
            eval(parse(text=paste0(x, 'varRaw')))}, simplify=TRUE)
        if (!is.null(dim(vars)))
            vars = paste0(
                'c(', apply(vars, 2, function(x) paste(x, collapse=',')), ')'
            )
        assignment = paste('rownames = data.frame(rownames = if (nrow(df) > 0)',
                           '1:nrow(df) else numeric(0))')
        assignment = c(assignment, paste0(dataVars, " = evalVarArg(", vars, ", ",
                             substitute(df, parent.frame()), ")"))
        #assignment = gsub("\\\"",  "", assignment)
        eval(parse(text=paste0("list(", paste(assignment, collapse=", "), ")")))
    })

    .splitFacetSeries = cmpfun(function(df){
        # inherit dataVars, facetvarRaw, seriesvar
        # note: the splitted list should be consistent with df layout

        facetvarRaw = get0("facetvarRaw")
        seriesvar = get0("seriesvar")
        if ('facet' %in% dataVars){
            if ('series' %in% dataVars){
                out = split(df, if (length(facetvarRaw) > 1) {
                    list(df[,seriesvar[1]], df[,facetvarRaw[2]],
                         df[,facetvarRaw[1]])
                }else{
                    list(df[,seriesvar[1]], df[,facetvarRaw[1]])
                })
            }else{
                out = split(df, if (length(facetvarRaw) > 1) {
                    list(df[,facetvarRaw[2]], df[,facetvarRaw[1]])
                }else{
                    list(df[,facetvarRaw[1]])
                })
            }
        }else{
            if ('series' %in% dataVars){
                out = split(df,  list(df[,seriesvar[1]]))
            }else{
                out = list(df)
            }
        }
        return(out)
    })

    fullMeta = .makeMetaDataList(data)
    if (hasT){
        if (is.numeric(t[,1]) || inherits(t[,1], c('Date', 'POSIXlt', 'POSIXct'))){
            data = data[order(data[,tvar[1]]),]
            uniT = sort(unique(t[,1]))
        }else{
            uniT = unique(t[,1])
        }
        if (is.factor(uniT)) uniT = as.character(uniT)
        tSize = unique(table(data[,tvar]))

        # timeslices not in equal size across t, suppl it
        if (length(tSize) > 1 && is.character(x[,1])) {
            expandData = data.frame(expand.grid(unique(x[,1]), unique(t[,1]),
                                                 stringsAsFactors=FALSE))
            names(expandData) = c(xvar[1], tvar[1])
            data = merge(expandData, data, all.x=TRUE, sort=FALSE)
            data = data[order(data[,tvar[1]]),]
        }

        dataByT = split(data, factor(data[,tvar[1]], levels=uniT))
        metaData = lapply(dataByT, .makeMetaDataList)
        names(metaData) = uniT

        seriesData = lapply(dataByT, .splitFacetSeries)
        seriesData = lapply(seriesData, function(lst){
            lapply(lst, .makeMetaDataList)
        })
        # if (! identical(unique(t[,1]), sort(unique(t[,1]))) &&
        #     ! identical(unique(t[,1]), sort(unique(t[,1]), TRUE)))
        #     warning("t is not in order, the chart may not show properly!")
    }else{
        metaData = fullMeta
        seriesData = .splitFacetSeries(data)
        seriesData = lapply(seriesData, .makeMetaDataList)
    }

    # -----------------check / determine chart types--------------------------
    ## ------------------Modify layout-------------------------
    check.types = unname(sapply(c('auto', validChartTypes$name),
                                 grepl, x=layout$type))
    if (is.null(dim(check.types)))
        check.types = matrix(check.types, nrow=1)
    if (!any(check.types))
        stop("Invalid chart type!\n", paste(type[which(rowSums(check.types)==0)], ', '),
             " not matching the valid chart type table.")
    if (!is.null(series)) {
        lvlSeries = levels(as.factor(series[,1]))
        nSeries = length(lvlSeries)
    }else{
        nSeries = 1
    }
    # determine chart types if 'auto'
    layout$type = sapply(1:nrow(layout), function(i){
        if (layout$type[[i]] == 'auto') {
            seriesSlice = if (hasT) seriesData[[1]] else seriesData
            if (nrow(seriesSlice[[i]]$x) == 0) 'scatter' else
            determineType(
                seriesSlice[[i]]$x[,1], seriesSlice[[i]]$y[,1])
        }else{
            layout$type[i]
        }
    })

    ## special: geoJSON map, -- not working
    # geoJSON = NULL
    # if (any(type == 'map'))
    #     if (all(! grep('.+[Jj][Ss][Oo][Nn]$', subtype))) {
    #         stop('When type is "map", geoJSON file must be provided in subtype')
    #     }else{
    #         if (grep('.+[Jj][Ss][Oo][Nn]$', subtype) > 1)
    #             warning('echart only accepts the first geoJSON file.')
    #         geoJSON = subtype[grep('.+[Jj][Ss][Oo][Nn]$', subtype)][1]
    #         con = system.file('htmlwidgets/lib/echarts/ext/loadGeoJSON.js',
    #                            package='recharts2')
    #         paramPath = system.file('htmlwidgets/lib/echarts/ext',
    #                                  package='recharts2')
    #         if (file.exists(con)){
    #             writeLines(paste0(
    #                 "require('", paramPath, "/param').params.newmap = { ",
    #                 "getGeoJson: function (callback) { ",
    #                 "$.getJSON('", geoJSON, "',callback);","}})"),
    #                 con)
    #         }
    #     }

    ## Convert chart type to a data.frame, modify df layout
    ## colnames: [id name type subtype misc coordSys]
    dfType = sapply(validChartTypes$name, function(x) grepl(x, layout$type))
    if (is.null(dim(dfType))){
        typeIdx = unname(which(dfType))
    }else{
        typeIdx = unname(unlist(sapply(seq_len(nrow(dfType)),
                                        function(i) which(dfType[i,]))))
    }
    dfType = validChartTypes[typeIdx,]
    # modify df layout
    layout$coordSys = dfType$coordSys
    layout = arrangeCoordIndex(layout)
    layout$xAxisIdx = layout$yAxisIdx = NA
    layout$xAxisIdx[layout$coordSys == 'cartesian2d'] =
        layout$yAxisIdx[layout$coordSys == 'cartesian2d'] =
        layout$coordIdx[layout$coordSys == 'cartesian2d']
    # subtype
    lstSubtype = rep('', nrow(layout))
    if (!missing(subtype)) if (length(subtype) > 0)
        lstSubtype = lapply(1:nrow(layout), function(i){
            str = layout[i, 'subtype']
            validSubtype = eval(parse(text=tolower(dfType[i, 'subtype'])))
            strSubtype = unlist(strsplit(str, '[_|\\+]'))
            strSubtype = gsub("^ +| +$", "", strSubtype)
            jsonFile = strSubtype[grepl("\\.[Jj][Ss][Oo][Nn]$", strSubtype)]
            o = intersect(validSubtype, strSubtype)
            if (length(jsonFile) > 0) o = c(o, jsonFile)
            if (length(o) == 0) o = ''
            return(o)
        })

    ## if pie/rose/ring, with series, shrink radius
    if (!all(is.na(layout$series))){
        which.pie = which(layout$type %in% c('pie', 'ring', 'rose'))
        if (length(which.pie) > 0){
            pie.layout = layout[which.pie,]
            if (convPct2Num(pie.layout$radius[1]) < 0.25 * max(pie.layout$iseries)){
                delta.radius = (convPct2Num(pie.layout$radius[1]) - 0.25)/
                    (max(pie.layout$iseries)-1)
            }else{
                delta.radius = 0.25
            }
            pie.layout$radius = convNum2Pct(
                convPct2Num(pie.layout$radius) -
                    (pie.layout$iseries-1) * delta.radius
            )
            layout[which.pie, 'radius'] = pie.layout$radius
        }
    }

    # ---------------------------params list----------------------
    .makeSeriesList = cmpfun(function(t){
        # each timeline create an options list
        # inherits seriesData, dfType, lstSubtype, layout

        if (is.null(t)){  # no timeline
            time_metaData = lapply(seriesData, function(lst){
                lapply(lst, function(df){
                    data.frame(lapply(df, function(col) {
                        if (inherits(col, c("Date", "POSIXlt", "POSIXct")))
                            col = convTimestamp(col)
                        return(col)
                    }))
                })
            })
            lstSeries = lapply(1:nrow(layout), function(i) {
                series_fun = getFromNamespace(paste0('series_', dfType$type[i]),
                                              'recharts2')
                series_fun(seriesData[[i]], type = dfType[i,],
                           subtype = lstSubtype[[i]], layout = layout[i,],
                           meta = metaData, fullMeta = fullMeta, layouts = layout)
            })
            out = structure(list(series = lstSeries), meta = seriesData)
            # out = structure(seriesData, layout=layout)
        }else{  # with timeline
            time_metaData = lapply(seriesData, function(lstT){
                lapply(lstT, function(lst) {
                    lapply(lst, function(df) {
                        data.frame(lapply(df, function(col){
                            if (inherits(col, c("Date", "POSIXlt", "POSIXct")))
                                col = convTimestamp(col)
                            return(col)
                        }))
                    })
                })
            })
            lstSeries = lapply(1:nrow(layout), function(i) {
                series_fun = getFromNamespace(paste0('series_', dfType$type[i]),
                                              'recharts2')
                series_fun(time_metaData[[t]][[i]], type = dfType[i,],
                           subtype = lstSubtype[[i]], layout = layout[i,],
                           meta = metaData, fullMeta = fullMeta, layouts = layout)
            })
            out = structure(list(series = lstSeries), meta = metaData[[t]])
        }

        return(out)
    })

    if (hasT){  ## has timeline
        params = list(
            baseOption=list(timeline=structure(list(), sliceby=tvar)),
            options=lapply(1:length(uniT), .makeSeriesList)
        )
        if (!is.null(series))
            params$baseOption$legend = list(
                data = as.list(levels(as.factor(series[,1])))
            )
    }else{
        params = .makeSeriesList(NULL)
        if (!is.null(series))
            params$legend = list(
                data = as.list(levels(as.factor(series[,1])))
            )
    }

    # ------------------dependency js------------------------
    depJS = list()
    if (any(grepl('map_world|world_map', dfType$name)))
        depJS = append(depJS, list(
            getDependency('world', 'htmlwidgets/lib/echarts/geo')))
    mapJSName = unique(c(mapShapeJS[,2][mapShapeJS[,1] %in% unlist(subtype)],
                   mapShapeJS[,2][mapShapeJS[,2] %in% unlist(subtype)]))
    if (length(mapJSName) > 0)
        depJS = append(depJS, lapply(mapJSName, getDependency,
                                      src='htmlwidgets/lib/echarts/geo'))

    # -------------------output-------------------------------
    chart = htmlwidgets::createWidget(
        'echarts', params, width = NULL, height = NULL, package = 'recharts2',
        dependencies = depJS, preRenderHook = function(instance) { instance }
    )
    attr(chart$x, 'layout') = layout
    attr(chart$x, 'fullMeta') = fullMeta
browser()

    if (hasT)
        chart = chart %>% setTimeline(show=TRUE, data=uniT)
    #if (!is.null(geoJSON)) chart$geoJSON = geoJSON
    chart = chart %>% autoFacetTitle()
    if (any(dfType$type %in% c('map'))){
        chart = chart %>% setTooltip() %>% setToolbox() %>% setLegend()
    }else if (any(dfType$type %in% c('heatmap'))){
        chart = chart %>% autoAxis(TRUE, FALSE) %>%
            setGrid(borderWidth=0)
    }else if (any(dfType$type %in% c(
        'line', 'bar', 'scatter', 'candlestick', 'effectScatter', 'lines', 'boxplot'))){
        chart = chart %>% autoAxis() %>%
            setTooltip() %>% setToolbox() %>% setLegend() %>%
            flipAxis(flip=which(grepl("flip", dfType$misc)))
    }else{
        chart = chart %>% autoAxis(showMainAxis=FALSE) %>%
            setGrid(borderWidth=0) %>%
            setTooltip() %>% setToolbox() %>% setLegend() %>%
            autoPolar(type=dfType)
    }
    chart
}

#' @export
#' @rdname eChart
echart.default = echart.data.frame

#' @export
#' @rdname eChart
eChart = echart


arrangeLayout = function(series=NULL, facet=NULL){
    # arrange the layout of the charts: facets, row & col
    # series, facet must be factors

    if (!is.null(facet)) {
        if (! is.factor(facet[,1])) facet[,1] = as.factor(facet[,1])
        if (ncol(facet) == 1) {
            facet = levels(facet[,1])
            layout = data.frame(irow=seq_along(facet), icol=1)
            layout$row = facet
            layout$col = NA
        }else if (ncol(facet) > 1) {
            if (! is.factor(facet[,2])) facet[,2] = as.factor(facet[,2])
            facet = expand.grid(
                levels(facet[,2]), levels(facet[,1]))[,2:1]
            layout = data.frame(
                irow = as.numeric(factor(facet[,1])),
                icol = as.numeric(factor(facet[,2])))
            layout[,c('row', 'col')] = facet[,1:2]
        }
    }else{
        layout = data.frame(irow=1, icol=1, row=NA, col=NA)
    }
    layout$ifacet = seq_len(nrow(layout))

    if (!is.null(series)) {
        if (! is.factor(series[,1])) series[,1] = as.factor(series[,1])
        series = levels(series[,1])
    }else{
        series = NA
    }
    layout = do.call('rbind', lapply(series, function(ser){
        layout$series = ser
        return(layout)
    }))
    layout$series = factor(layout$series, levels=series)
    layout$iseries = sapply(layout$series, function(s){
        which(unique(layout$series) == s)
    })
    layout = layout[order(layout$irow, layout$icol, layout$ser),]
    rownames(layout) = layout$i = seq_len(nrow(layout))
    return(layout[,c('i', 'ifacet', 'irow', 'icol', 'row', 'col', 'iseries', 'series')])
}

matchLayout = function(param, layout){
    # layout must be the output of arrangeLayout()
    # param must be a vector or list, could be type/subtype/coordSys
    stopifnot(is.data.frame(layout))
    stopifnot(is.vector(param) || is.list(param))
    if (max(layout$irow) == 1 && max(layout$icol) == 1){  # no facets
        if (is.list(param)) param = unlist(param)
        if (length(param) >= nrow(layout)){
            param = param[1:nrow(layout)]
        }else{
            param = c(param, rep(param[length(param)],
                                 nrow(layout) - length(param)))
        }
    }else if (max(layout$irow) > 1 || max(layout$icol) > 1){  # with facets
        if (!is.list(param)) param = list(param)
        if (length(param) >= length(unique(layout$ifacet))){
            param = param[1:length(unique(layout$ifacet))]
        }else{
            param = append(param, rep(
                param[length(param)], length(unique(layout$ifacet)) - length(param)))
        }
        lvl.series = levels(layout$series)
        for (i in 1:length(param)){  # suppl params
            if (length(param[[i]]) >= length(lvl.series)){
                param[[i]] = param[[i]][1:length(lvl.series)]
            }else{
                param[[i]] = c(param[[i]], rep(
                    param[[i]][length(param[[i]])], length(lvl.series) - length(param[[i]])))
            }
        }
    }else{
        stop('the layout (', max(layout$irow), ' x ', max(layout$icol),
             ') is not correctly defined!')
    }
    return(unlist(param))
}

arrangeCoordIndex = function(layout){
    # layout must be outcome df of arrangeLayout()
    # cols: [i, ifacet, irow, icol, row, col, series, type, subtype, coordSys]
    layout$coordSys = as.factor(layout$coordSys)
    lstLayout = split(layout, layout$coordSys)
    .arrangeIndex = function(df){
        if (nrow(df)>0){
            uniIdx = unique(df$ifacet)
            o = data.frame(i = df$i, coordIdx = sapply(df$ifacet, function(x) {
                which(uniIdx==x)-1})
            )
            return(o)
        }
    }
    lstIdx = lapply(lstLayout, .arrangeIndex)
    bindIdx = do.call('rbind', lstIdx)
    return(merge(layout, bindIdx, all.x=TRUE, sort=FALSE))
}


determineType = function(x, y) {
    if (is.numeric(x) && is.numeric(y)) return('scatter')
    # when y is numeric, plot y against x; when y is NULL, treat x as a
    # categorical variable, and plot its frequencies
    if ((is.factor(x) || is.character(x)) && (is.numeric(y) || is.null(y)))
        return('vbar')
    if (is.null(x) && is.ts(y)) return('line')
    # FIXME: 'histogram' is not a standard plot type of ECharts
    # http://echarts.baidu.com/doc/doc.html
    if ((is.numeric(x) && is.null(y)) || (is.numeric(y) && is.null(x)))
        return('histogram')
    if ((inherits(x, c("Date", "POSIXlt", "POSIXct")) && is.numeric(y)))
        return('curve')
    if ((is.character(x) || is.factor(x)) && (is.character(y) || is.factor(y)))
        return('bubble')
    message('The structure of x:')
    str(x)
    message('The structure of y:')
    str(y)
    stop('Unable to determine the chart type from x and y automatically')
}


getDependency = function(type, src='htmlwidgets/lib/echarts') {
    if (is.null(type)) return()
    htmltools::htmlDependency(
        'echarts-module', EChartsVersion,
        src = system.file(src, package = 'recharts2'),
        script = sprintf('%s.js', type)
    )
}

getAttr = function(obj, attr){
    if (inherits(obj, "echarts"))
        attr(obj$x, attr, exact = TRUE)
    else
        attr(obj, attr, exact = TRUE)
}

getMeta = function(obj) getAttr(obj, 'meta')
getFullMeta = function(obj) getAttr(obj, 'fullMeta')
getLayout = function(obj) getAttr(obj, 'layout')

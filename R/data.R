#' @importFrom data.table melt
series_scatter <- function(lst, type, subtype, return=NULL, ...){
    # g = echartr(mtcars, wt, mpg, am)
    if (is.null(lst$x) || is.null(lst$y))
        stop('scatter charts need x and y!')
    lst <- mergeList(list(weight=NULL, series=NULL), lst)
    if (!is.numeric(lst$x[,1])) stop('x and y must be numeric')
    data <- cbind(lst$y[,1], lst$x[,1])

    if (!is.null(lst$weight)){  # weight as symbolSize
        data <- cbind(data, lst$weight[,1])
        minWeight <- min(abs(lst$weight[,1]), na.rm=TRUE)
        maxWeight <- max(abs(lst$weight[,1]), na.rm=TRUE)
        range <- maxWeight - minWeight
        folds <- maxWeight / minWeight
        if (abs(folds) < 50){  # max/min < 50, linear
            jsSymbolSize <- JS(paste0('function (value){
                return ', switch(ceiling(abs(folds)/10), 4,3.5,3,2.5,2),
                '*Math.round(Math.abs(value[2]/', minWeight,'));
                }'))
        }else{  # max/min >= 50, normalize
            jsSymbolSize <- JS(paste0('function (value){
                return Math.round(1+29*(Math.abs(value[2])-', minWeight,')/', range, ');
            }'))
        }

        if(is.numeric(lst$weight[,1])){
            dfWgt <- data.frame(s=if (is.null(lst$series)) '' else lst$series[,1],
                                w=lst$weight[,1], stringsAsFactors = FALSE)
            lvlWgt <- data.table::dcast(dfWgt, s~., mean, value.var='w')
            lvlWgt[,2][is.na(lvlWgt[,2])] <- 0
            pctWgt <- lvlWgt[,2]/sum(lvlWgt[,2])
            lineWidths <- 8*(pctWgt-min(pctWgt))/(max(pctWgt)-min(pctWgt)) +1
            lineWidths[is.na(lineWidths)] <- 1
        }
    }
    obj <- list()
    if (is.null(lst$series)) {  # no series
        if (is.null(lst$weight)){
            obj <- list(list(type=type$type[1], data=asEchartData(data[,2:1])))
        }else{
            obj <- list(list(type=type$type[1], data=asEchartData(data[,c(2:1,3)])))
            if (grepl('bubble', type$misc[1])) obj[[1]]$symbolSize <- jsSymbolSize
        }
    }else{  # series-specific
        data <- cbind(data, lst$series[,1])
        data <- split(as.data.frame(data), lst$series[,1])
        if (is.null(lst$weight)){
            obj <- lapply(seq_along(data), function(i){
                list(name = names(data)[i], type = type$type[i],
                     data = asEchartData(data[[i]][,2:1]))
            })  ## only fetch col 1-2 of data, col 3 is series
        }else{
            obj <- lapply(seq_along(data), function(i){
                out <- list(name = names(data)[i], type = type$type[i],
                            data = asEchartData(data[[i]][,c(2:1, 3)]))
                if (grepl('bubble', type$misc[i])) out$symbolSize <- jsSymbolSize
                # line, weight links to line width
                if (type$type[i] == 'line' && !is.null(lineWidths)){
                    if (is.null(out$itemStyle)) out$itemStyle <- list()
                    if (is.null(out$itemStyle$normal))
                        out$itemStyle$normal <- list()
                    if (is.null(out$itemStyle$normal$lineStyle))
                        out$itemStyle$normal$lineStyle <- list()
                    out$itemStyle$normal$lineStyle <- mergeList(
                        out$itemStyle$normal$lineStyle, list(width=lineWidths[i])
                    )
                }
                return(out)
            })  ## fetch col 1-2 and 3 (x, y, weight)
        }
    }

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}

series_bar <- function(lst, type, subtype, return=NULL, ...){
    # example:
    # echartr(mtcars, row.names(mtcars), mpg,
    #     series=factor(am,labels=c('Manual','Automatic')),
    #     type=c('hbar','scatter'))
    lst <- mergeList(list(series=NULL), lst)
    data <- cbind(lst$y[,1], lst$x[,1])

    if (!'y' %in% names(lst)) {  # y is null, then...
        if (any(grepl('hist', type$misc))){  # histogram
            hist <- hist(data[,1], plot=FALSE)
            if ('density' %in% subtype[[1]]){
                data <- as.matrix(cbind(hist$density, hist$mids))  # y, x
            }else{
                data <- as.matrix(cbind(hist$counts, hist$mids))  # y, x
            }
        }else{ # simply run freq of x
            if (is.numeric(data[,1])){
                data <- as.matrix(as.data.frame(table(data[,1])))
            }else{
                data <- as.matrix(table(data[,1]))
            }
        }
    }

    obj <- list()
    if (is.null(lst$series)) {  # no series
        if (is.numeric(lst$x[,1])){
            obj <- list(list(type=type$type[1], data=asEchartData(data[,2:1])))
            if (any(grepl("flip", type$misc[[1]]))) obj[[1]]$barHeight=10
            if (grepl('hist',type$misc[[1]])) {
                obj[[1]]$barGap = '1%'
                obj[[1]]$barWidth = JS(paste0(
                    "(document.getElementById('temp').offsetWidth-200)/",
                    length(hist$breaks)))
                obj[[1]]$barMaxWidth = floor(820 / length(hist$breaks))
            }
        }else{
            obj <- list(list(type=type$type[1], data=asEchartData(data[,1])))
        }
    }else{  # series-specific
        dataCross <- tapply(data[,1], list(data[,2], lst$series[,1]), function(x) {
            if (length(x) == 1) return(x)
            stop('y must only have one value corresponding to each combination of x and series')
        })
        idx <- match(unique(data[,2]),rownames(dataCross))
        dataCross <- dataCross[idx,]
        #rownames(dataCross) <- data[,2]
        data <- dataCross

        # weight link to barWidth/lineWidth
        barWidths <- NULL
        lineWidths <- NULL
        if ('weight' %in% names(lst)) if(is.numeric(lst$weight[,1])){
            dfWgt <- data.frame(s=if (is.null(lst$series)) '' else lst$series[,1],
                                w=lst$weight[,1], stringsAsFactors = FALSE)
            lvlWgt <- data.table::dcast(dfWgt, s~., mean, value.var='w')
            lvlWgt[,2][is.na(lvlWgt[,2])] <- 0
            pctWgt <- lvlWgt[,2]/sum(lvlWgt[,2])
            barWidths <- paste0(
                "(document.getElementById('temp').offsetWidth-200)/",
                nrow(data), "*0.8*", pctWgt)
            lineWidths <- 8*(pctWgt-min(pctWgt))/(max(pctWgt)-min(pctWgt)) +1
            lineWidths[is.na(lineWidths)] <- 1
        }

        obj <- lapply(seq_len(ncol(data)), function(i){
            if (is.numeric(lst$x[,1])){
                o = list(name = colnames(data)[i], type = type$type[i],
                         data = asEchartData(cbind(as.numeric(rownames(data)),
                                                        data[,i])))
                if (any(grepl("flip", type$misc)))
                    o <- mergeList(o, list(barHeight=10))
            }else{
                o = list(name = colnames(data)[i], type = type$type[i],
                         data = asEchartData(data[,i]))
            }
            if (!is.null(barWidths)){
                if (any(grepl("flip", type$misc[[i]])))
                    o$barWidth <- JS(gsub("offsetWidth", "offsetHeight", barWidths[i]))
                else
                    o$barWidth <- JS(barWidths[i])
            }

            if (type$type[i] == 'line' && !is.null(lineWidths)){
                if (is.null(o$itemStyle)) o$itemStyle <- list()
                if (is.null(o$itemStyle$normal)) o$itemStyle$normal <- list()
                if (is.null(o$itemStyle$normal$lineStyle))
                    o$itemStyle$normal$lineStyle <- list()
                o$itemStyle$normal$lineStyle <- mergeList(
                    o$itemStyle$normal$lineStyle, list(width=lineWidths[i])
                )
            }
            if ('stack' %in% ifnull(subtype[i], '')[[1]]) o[['stack']] = 'Group'
            return(o)
        })
    }

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}

series_line = function(lst, type, subtype, return=NULL, ...) {
    # Example:
    # g=echartr(airquality, as.character(Day), Temp,z=Month, type='curve')
    # g=echartr(airquality, as.character(Day), Temp,z=Month, type='area_smooth')
    lst <- mergeList(list(series=NULL), lst)
    data <- cbind(lst$y[,1], lst$x[,1])

    if (is.null(lst$x[,1]) && is.ts(lst$y[,1])) {
        lst$x[,1] = as.numeric(time(lst$y[,1]))
        lst$y[,1] = as.numeric(lst$y[,1])
    }
    obj <- list()

    if (is.numeric(lst$x[,1])) {
        obj = series_scatter(lst, type = type, subtype = subtype)
    }else{
        if (is.null(lst$series)) {
            obj = list(list(type = 'line', data = asEchartData(lst$y[,1])))
        }
    }
    if (length(obj) == 0) obj = series_bar(lst, type = type, subtype = subtype)

    # area / stack / smooth
    areaIdx <- which(grepl("fill", type$misc))
    stackIdx <- which(sapply(subtype, function(x) 'stack' %in% x))
    solidIdx <- which(sapply(subtype, function(x) 'solid' %in% x))
    dashedIdx <- which(sapply(subtype, function(x) 'dashed' %in% x))
    dottedIdx <- which(sapply(subtype, function(x) 'dotted' %in% x))
    smoothIdx <- which(sapply(subtype, function(x) 'smooth' %in% x) ||
                           grepl('smooth', type$misc))

    if (length(areaIdx) > 0){
        for (i in areaIdx)  obj[[i]][['itemStyle']] <-
                list(normal=list(areaStyle=list(
                    type='default')))
    }
    if (length(solidIdx) > 0)
        for (i in solidIdx) {
            if (is.null(obj[[i]]$itemStyle)) obj[[i]]$itemStyle <- list()
            if (is.null(obj[[i]]$itemStyle$normal))
                obj[[i]]$itemStyle$normal <- list()
            if (is.null(obj[[i]]$itemStyle$normal$lineStyle))
                obj[[i]]$itemStyle$normal$lineStyle <- list()
            obj[[i]]$itemStyle$normal$lineStyle <- list(type='solid')
        }
    if (length(dottedIdx) > 0)
        for (i in dottedIdx) {
            if (is.null(obj[[i]]$itemStyle)) obj[[i]]$itemStyle <- list()
            if (is.null(obj[[i]]$itemStyle$normal))
                obj[[i]]$itemStyle$normal <- list()
            if (is.null(obj[[i]]$itemStyle$normal$lineStyle))
                obj[[i]]$itemStyle$normal$lineStyle <- list()
            obj[[i]]$itemStyle$normal$lineStyle <- list(type='dotted')
        }
    if (length(dashedIdx) > 0)
        for (i in dashedIdx) {
            if (is.null(obj[[i]]$itemStyle)) obj[[i]]$itemStyle <- list()
            if (is.null(obj[[i]]$itemStyle$normal))
                obj[[i]]$itemStyle$normal <- list()
            if (is.null(obj[[i]]$itemStyle$normal$lineStyle))
                obj[[i]]$itemStyle$normal$lineStyle <- list()
            obj[[i]]$itemStyle$normal$lineStyle <- list(type='dashed')
        }
    if (length(stackIdx) > 0)
        for (i in stackIdx) obj[[i]][['stack']] <- 'Group'
    if (length(smoothIdx) > 0)
        for (i in smoothIdx) obj[[i]][['smooth']] <- TRUE
    for (i in seq_along(obj)){
        obj[[i]]$showAllSymbol <- TRUE
        #obj[[i]]$symbolSize <- 0
    }

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }

}

series_k <- function(lst, type, subtype, return=NULL, ...){
    # Example:
    # g=echartr(stock, date, c(open, close, low, high), type='k')

    data <- cbind(lst$y[,1], lst$x[,1])
    obj <- list(list(name='Stock', type=type$type[1], data=asEchartData(lst$y[,1:4])))
    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}

series_pie <- function(lst, type, subtype, return=NULL, ...){
    # Example:
    # g=echartr(iris, Species, Sepal.Width, type='pie')
    # g=echartr(mtcars, am, mpg, gear, type='pie')
    # g=echartr(mtcars, y=mpg, series=gear,type='ring')
    ## ring_info
    # ds=data.frame(q=c('68% feel good', '29% feel bad', '3% have no feelings'),
    #               a=c(68, 29, 3))
    # g=echartr(ds, q, a, type='ring_info')
    # dev.width=paste0("document.getElementById('", g$elementId,"').offsetWidth")
    # dev.height=paste0("document.getElementById('", g$elementId,"').offsetHeight")
    # g %>% setLegend(pos=c('center','top','vertical'),
    #                 itemGap=JS(paste0(dev.height,"*0.4/3"))) %>%
    #       relocLegend(x=JS(paste0(dev.width,"/2")), y=JS(paste0(dev.height,"/10")))

    if (is.null(lst$y)) stop('pie/funnel charts need y!')
    if (is.null(lst$x) && is.null(lst$series)) stop('pie/funnel charts need either x or series!')
    data <- data.frame(lst$y[,1])
    if (!is.null(lst$x)){
        data[,2] <- if (matchSubtype('info', subtype))
            'TRUE' else lst$x[,1]
        series <- if (matchSubtype('info', subtype))
            c('TRUE', 'FALSE') else as.character(unique(lst$x[,1]))
    }else{
        data[,2] <- if (matchSubtype('info', subtype))
            lst$series[,1] else 'TRUE'
        series <- if (matchSubtype('info', subtype))
            as.character(unique(lst$series[,1])) else c('TRUE','FALSE')
    }
    if (!is.null(lst$series)){
        data[,3] <- lst$series[,1]
        pies <- as.character(unique(lst$series[,1]))
    }else{
        data[,3] <- if (matchSubtype('info', subtype))
            lst$x[,1] else 'Proportion'
        pies <- if (any(sapply(subtype, function(x) 'info' %in% x)))
            as.character(unique(lst$x[,1])) else 'Proportion'
        if (any(sapply(subtype, function(x) 'info' %in% x))){
            type[2:length(pies),] <- type[1,]
            if (length(subtype) < length(type))
                subtype <- rep(subtype[length(subtype)],
                               length(type)-length(subtype))
        }

    }
    names(data) <- c('y', 'x', 'series')
    data <- data.table::dcast(data, x~series, sum, value.var='y')

    if (all(data$x == 'TRUE')) {
        sum.prop <- sum(data[data$x == 'TRUE', 2:ncol(data)], na.rm=TRUE)
        data[nrow(data)+1, ] <- c('FALSE', sum.prop - data[data$x == 'TRUE',
                                                           2:ncol(data)])
    }
    if (is.null(lst$z)){
        layouts <- autoMultiPolarChartLayout(length(pies))
    }else{
        layouts <- autoMultiPolarChartLayout(length(pies), bottom=15)
    }

    rows <- layouts$rows
    cols <- layouts$cols
    centers <- layouts$centers
    rownames(centers) <- pies
    radius <- layouts$radius

    ## place holder styles
    placeHolderStyle = list(normal = list(
            color = 'rgba(0,0,0,0)', label = list(show=FALSE),
            labelLine = list(show=FALSE)
        ),
        emphasis = list(color = 'rgba(0,0,0,0)')
    )
    grayStyle = list(normal = list(
        color='#ccc', label=list(show=FALSE, position='center'),
        labelLine=list(show=FALSE)
        ),
        emphasis=list(color='rgba(0,0,0,0)')
    )
    normalStyle = list(normal=list(label=list(show=FALSE),
                                  labelLine=list(show=FALSE)))

    obj <- list()
    for (pie in pies){
        iType <- type[which(pies == pie),]
        iSubtype <- subtype[[which(pies == pie)]]
        o <- list(
            name=pie, type=iType$type,
            data=unname(apply(data[,c('x', pie)], 1, function(row) {
                if (row[1] == 'FALSE')
                    return(list(name='', value= ifna(as.numeric(row[2]), '-'),
                         itemStyle=grayStyle))
                else
                    return(list(name=ifelse(as.character(unname(row[1]))=='TRUE',
                                            pie, as.character(unname(row[1]))),
                                value=ifna(as.numeric(unname(row[2])), '-'),
                                itemStyle=normalStyle))
                })),
            center=paste0(unname(centers[pie,]), '%'), width=paste0(radius, '%'),
            x=paste0(centers[pie, 1]-radius/2, '%'), radius=paste0(radius, '%'),
            max=ifelse(all(is.na(data[,pie])), 0,
                       max(unname(data[,pie]), na.rm=TRUE)),
            height=ifelse(rows==1, '70%', paste0(radius, '%')),
            y=ifelse(rows==1, rep('15%', length(pies)), paste0(centers[pie, 2]-radius/2, '%')),
            selectedMode=if ('multi' %in% iSubtype) 'multiple' else NULL
        )
        if (grepl('ring', iType$misc)){
            o[['radius']] <- paste0(c(radius * 2/3, radius), '%')
            o[['itemStyle']] <- list(
                normal=list(label=list(show=TRUE)),
                emphasis=list(label=list(show=TRUE, position='center', textStyle=list(
                    fontSize='30',fontWeight='bold'
                )))
            )
            o[['clockWise']] <- any(c('clock', 'clockwise') %in% iSubtype)
        }
        if ('radius' %in% iSubtype){
            o[['roseType']] <- 'radius'
            o[['radius']] <- paste0(c(radius/5, radius), '%')
        }else if ('area' %in% iSubtype){
            o[['roseType']] <- 'area'
            o[['radius']] <- paste0(c(radius/5, radius), '%')
        }else if ('info' %in% iSubtype){
            o[['data']][[2]][['itemStyle']] <- placeHolderStyle
            ringWidth <- 40 / length(pies)
            o[['radius']] <- paste0(c(80 - ringWidth*(which(pies == pie)-1),
                                      80 - ringWidth*which(pies == pie)), '%')
            o[['center']] <- c('50%', '50%')
            o[['clockWise']] <- any(c('clock', 'clockwise') %in% iSubtype)
        }else{
            if (is.null(o)) o[['radius']] <- paste0(radius, '%')
        }
        ## additional for funnel charts
        if (iType$type == 'funnel'){
            if (grepl('ascending', iType$misc)) o[['sort']] <- 'ascending'
            o[['itemStyle']] <- mergeList(o[['itemStyle']], list(normal=list(
                labelLine=list(show=TRUE)))
            )
            if ('left' %in% iSubtype){
                o[['funnelAlign']] <- 'left'
            }else if ('right' %in% iSubtype){
                o[['funnelAlign']] <- 'right'
            }
        }

        obj[[pie]] <- o
    }
    obj <- unname(obj)

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}

series_funnel <- series_pie

series_radar <- function(lst, type, subtype, return=NULL, ...){
    # Example:
    # cars = mtcars[c('Merc 450SE','Merc 450SL','Merc 450SLC'),
    #               c('mpg','disp','hp','qsec','wt','drat')]
    # cars$model <- rownames(cars)
    # cars <- data.table::melt(cars, id.vars='model')
    # names(cars) <- c('model', 'indicator', 'Parameter')
    # echartr(cars, indicator, Parameter, model, type='radar') %>%
    #        setTitle('Merc 450SE  vs  450SL  vs  450SLC')
    # echartr(cars, c(indicator, model), Parameter, type='radar', sub='fill')
    # echartr(cars, c(indicator, model), Parameter, type='target') %>%
    #         setSymbols('none')
    #
    # echartr(cars, indicator, Parameter, z=model, type='radar')
    # ----------------
    #
    # carstat = data.table::dcast(data.table::data.table(mtcars),
    #               am + carb + gear ~., mean,
    #               value.var=c('mpg','disp','hp','qsec','wt','drat'))
    # carstat = data.table::melt(carstat, id=c('am', 'carb', 'gear'))
    # names(carstat) <- c('am', 'carb', 'gear', 'indicator', 'Parameter')
    # levels(carstat$indicator) <- gsub("_mean_\\.", "",
    #                                   levels(carstat$indicator))
    # carstat$am <- factor(carstat$am, labels=c('A', 'M'))
    # fullData <- data.frame(expand.grid(levels(carstat$indicator),
    #             levels(carstat$am), unique(carstat$carb)))
    # carstat <- merge(fullData, carstat, all.x=TRUE)
    # echartr(carstat, c(indicator, am),
    #         Parameter, carb, z=gear, type='radar')

    # x[,1] is x, x[,2] is series; y[,1] is y; series[,1] is polorIndex
    if (is.null(lst$y) || is.null(lst$x)) stop('radar charts need x and y!')
    ds <- data.frame(lst$y[,1], lst$x[,1:(ifelse(ncol(lst$x) > 1, 2, 1))])
    if (ncol(lst$x) == 1) ds[,ncol(ds)+1] <- names(lst$y)[1]
    if (is.null(lst$series)) ds[,ncol(ds)+1] <- 0
    else ds[,ncol(ds)+1] <- lst$series[,1]
    names(ds) <- c('y', 'x', 'series', 'index')
    ds$x <- as.factor(ds$x)
    ds$series <- as.factor(ds$series)
    ds$index <- as.factor(ds$index)

    data <- data.table::dcast(ds, index+x+series~., sum, value.var='y')
    names(data) <- c('index', 'x', 'series', 'y')
    fullData <- data.frame(expand.grid(
        if (is.null(lst$series)) levels(ds$index) else
            if (is.factor(lst$series[,1])) levels(lst$series[,1]) else
                unique(lst$series[,1]),
            levels(ds$x), levels(ds$series)))
    if (ncol(lst$x) > 1) if (is.factor(lst$x[,2])){
        fullData <- data.frame(expand.grid(
            if (is.null(lst$series)) levels(ds$index) else
                if (is.factor(lst$series[,1])) levels(lst$series[,1]) else
                    unique(lst$series[,1]),
            levels(ds$x), levels(lst$x[,2])))
    }

    names(fullData) <- c('index', 'x', 'series')
    data <- merge(fullData, data, all.x=TRUE, sort=FALSE)
    data$x <- as.character(data$x)
    index <- if (length(unique(fullData$index)) == 1) 0 else
        (1:nlevels(fullData$index))-1
    obj <- lapply(index, function(i){
        dt <- if (length(index) == 1) data else
            data[data$index==levels(fullData$index)[i+1],]
        out <- list(type=type[i+1, 'type'], symbol='none',
                    name=if (length(index) == 1) 0 else
                        levels(fullData$index)[i+1],
                    data=lapply(unique(dt$series), function(s){
                        list(name=as.character(s),
                             value=lapply(dt[dt$series==s, 'y'], function(x){
                                 ifna(x, '-')}))
                    }))
        if (i>0) out[['polarIndex']] <- i
        if ('fill' %in% subtype[[i+1]])
            out[['itemStyle']] <- list(normal=list(areaStyle=list(type='default')))
        return(out)
    })

    if (is.null(return)){
        return(obj)
    }else{
        return(obj[intersect(names(obj), return)])
    }
}

series_force <- function(lst, type, subtype, return=NULL, ...){
    # x: node/link, x2: link, series: series/relation, y: weight/value
    # df with x2 NA is nodes, !NA is links. If all !NA, no categories are linked
    # or x: name column, y: matrix
    # Example
    # echartr(yu, c(source, target), value, relation, type='force')
    # echartr(deutsch, c(club, player), weight, role, type='chord')
    if (is.null(lst$y) || is.null(lst$x))
        stop('radar charts need x and y!')
    if (is.null(lst$series)){
        if (ncol(lst$y) != nrow(lst$y)) stop('When there is no series, y must be a matrix!')
    }else{
        if (ncol(lst$x) < 2) stop('x must have at least 2 columns: 1st as source, 2nd as target!')
    }

    if (is.null(lst$series)){  #matrix mode
        data <- data.frame(lst$y, lst$x[,1])
        matrix <- unname(as.matrix(lst$y))
        categories <- as.character(unique(lst$x[,1]))
    }else{  # nodes and links mode
        data <- data.frame(lst$y[,1], lst$x[,1:2], lst$series[,1])
        if (!any(is.na(data[,3]))){
            nodes <- unique(c(as.character(data[,2]), as.character(data[,3])))
            categories <- as.character(data[,4])
        }else{
            nodes <- data[is.na(data[,3]), c(1,2,4)]
            names(nodes) <- c("value", "name", "series")
            categories <- as.character(unique(nodes$series))
        }
        links <- data[!is.na(data[,3]),]
        names(links) <- c("value", "source", "target", "name")
    }

    if (any(type$type %in% c('force', 'chord'))){
        types <- type$type
        o <- list(list(
            type=types[1], name='Connection', roam='move',
            itemStyle=list(normal=list(
                label=list(show=TRUE, textStyle=list(color='#333')),
                nodeStyle=list(brushType='both', strokeColor='rgba(255,215,0,0.4)'),
                linkStyle=list(type=ifelse(grepl('line', type$misc[1]), 'line',
                                           ifelse(is.null(lst$series), 'line', 'curve')))
            ), emphasis=list(
                label=list(show=FALSE), nodeStyle=list(), lineStyle=list()
            )),
            minRadius=8, maxRadius=20
        ))
        # nodes/links or matrix
        if (is.null(lst$series)){  # data/matrix
            o[[1]]$matrix <- asEchartData(matrix)
            o[[1]]$data <- lapply(categories, function(catg){
                list(name=unname(catg))})
        }else{  # categories, nodes/links
            if (is.null(dim(nodes))){
                o[[1]]$nodes <- lapply(nodes, function(vec){
                    list(name=vec)
                })
            }else{
                o[[1]]$nodes <- unname(apply(nodes, 1, function(row){
                    list(category=which(categories==row[['series']])-1,
                         name=row[['name']], value=as.numeric(row[['value']]))
                }))
                o[[1]]$categories <- lapply(categories, function(catg){
                    list(name=unname(catg))})
            }

            o[[1]]$links <- unname(apply(links, 1, function(row){
                list(source=row[['source']], target=row[['target']],
                     name=row[['name']], weight=as.numeric(row[['value']]))
            }))
        }

        #other params
        ## linkSymbol
        if ('arrow' %in% subtype[[1]]) o[[1]]$linkSymbol <- 'arrow'
        if ('triangle' %in% subtype[[1]]) o[[1]]$linkSymbol <- 'triangle'

        ## auto ribbon
        if (types[1] == 'force'){
            if (is.null(lst$series)){
                 o[[1]]$ribbonType <- TRUE
            }else{
                if (sum(paste(links$source, links$target) ==
                        paste(links$target, links$source), na.rm=TRUE) / nrow(links) > 0.5)
                    o[[1]]$ribbonType <- TRUE
                else o[[1]]$ribbonType <- FALSE
            }
        }else{
            o[[1]]$ribbonType <- 'ribbon' %in% subtype[[1]]
        }

        ## sort, sortSub, rotateLabel, scale
        if ('asc' %in% subtype[[1]]) o[[1]]$sort <- 'ascending'
        if ('ascsub' %in% subtype[[1]]) o[[1]]$sortSub <- 'ascending'
        if ('desc' %in% subtype[[1]]) o[[1]]$sort <- 'descending'
        if ('descsub' %in% subtype[[1]]) o[[1]]$sortSub <- 'descending'
        if ('rotatelab' %in% subtype[[1]]) {
            if (is.null(o[[1]]$itemStyle)) o[[1]]$itemStyle <- list()
            if (is.null(o[[1]]$itemStyle$normal)) o[[1]]$itemStyle$normal <- list()
            if (is.null(o[[1]]$itemStyle$normal$label))
                o[[1]]$itemStyle$normal$label <- list()
            o[[1]]$itemStyle$normal$label$rotate <- TRUE
        }
        if ('hidelab' %in% subtype[[1]]) {
            if (is.null(o[[1]]$itemStyle)) o[[1]]$itemStyle <- list()
            if (is.null(o[[1]]$itemStyle$normal)) o[[1]]$itemStyle$normal <- list()
            if (is.null(o[[1]]$itemStyle$normal$label))
                o[[1]]$itemStyle$normal$label <- list()
            o[[1]]$itemStyle$normal$label$show <- FALSE
        }
        o[[1]]$showScale <- any(c('scale', 'scaletext') %in% subtype[[1]])
        o[[1]]$showScaleText <- 'scaletext' %in% subtype[[1]]

        ## clockWise
        if ('clock' %in% subtype[[1]] || 'clockwise' %in% subtype[[1]])
            o[[1]]$closeWise <- TRUE

        if (is.null(return)){
            return(o)
        }else{
            return(o[intersect(names(o), return)])
        }
    }
}

series_chord <- series_force

series_gauge <- function(lst, type, subtype, return=NULL, ...){
    if (is.null(lst$x) || is.null(lst$y))
        stop('gauge charts need x and y!')
    data <- data.frame(y=lst$y[,1], x=lst$x[,1])
    data$series <- if (is.null(lst$series)) '' else lst$series[,1]
    nSeries <- length(unique(data$series))
    layouts <- autoMultiPolarChartLayout(nSeries)
    rows <- layouts$rows
    cols <- layouts$cols
    cols <- layouts$cols
    centers <- layouts$centers
    radius <- layouts$radius

    if ('fullMeta' %in% names(list(...))){
        fullMeta <- list(...)$fullMeta
        meta <- cbind(
            as.character(unlist(lapply(fullMeta, function(l) unlist(l$x)))),
            unlist(lapply(fullMeta, function(l) unlist(l$y))))
    }

    out <- lapply(unique(data$series), function(series){
        dt <- data[data$series==series,]
        idx <- which(unique(data$series)==series)
        iType <- type[idx,]
        o <- list(type=iType$type, center=paste0(centers[idx,], '%'),
                  radius=paste0(radius, '%'),
                  data=unname(apply(dt, 1, function(row){
                      list(name=unname(as.character(row['x'])),
                           value=unname(as.numeric(row['y'])))
                  })))
        if (series != '') o[['name']] <- unname(series)
        if ('fullMeta' %in% names(list(...))){
            o[['max']] <- max(as.numeric(meta[meta[,1]==series, 2]), na.rm=TRUE)
        }else{
            o[['max']] <- max(dt$y[dt$x==dt$x[1]], na.rm=TRUE)
        }
        return(o)
    })

    if (is.null(return)){
        return(out)
    }else{
        return(out[intersect(names(out), return)])
    }
}

series_map <- function(lst, type, subtype, return=NULL, ...){
    # x[,1] x; x[,2] series; y[,1] value; y[,2] selected; series[,1] multi-maps

    # Example:
    # echartr(NULL, type="map_china")
    x <- if (is.null(lst$x)) NA else lst$x[,1]
    series <- if (is.null(lst$x)) '' else if (ncol(lst$x) > 1) lst$x[,2] else ''
    y <- if (is.null(lst$y)) NA else lst$y[,1]
    sel <- if (is.null(lst$y)) FALSE else
        if (ncol(lst$y) > 1) as.logical(lst$y[,2]) else FALSE
    idx <- if (is.null(lst$series)) '' else lst$series[,1]
    data <- data.frame(y, x, series, sel, idx, stringsAsFactors=FALSE)

    # special case: geoJSON
    if (any(type$misc == 'geojson')){
        iGeoJSON <- which(type$misc == 'geojson')
        subtype[iGeoJSON] <- 'newmap'
    }

    # two modes: series - mono map mulit series; split - multi map mono series
    mode <- if (is.null(lst$series)) 'series' else 'split'
    lvlSeries <- if (mode=='series') as.character(unique(data$series)) else
        as.character(unique(data$idx))
    nSeries <- length(lvlSeries)

    if (nrow(type) < nSeries)
        type[nrow(type)+1:nSeries,] <- type[nrow(type),]
    if (length(subtype) < nSeries)
        subtype[length(subtype)+1:nSeries] <- subtype[length(subtype)]

    #layouts
    if (mode=='split'){
        if (is.null(lst$z)){
            layouts <- autoMultiPolarChartLayout(nSeries, col.max=4)
        }else{
            layouts <- autoMultiPolarChartLayout(nSeries, bottom=15, col.max=4)
        }
        rows <- layouts$rows
        cols <- layouts$cols
        ul.corners <- layouts$centers - layouts$radius/2
    }

    out <- lapply(lvlSeries, function(series){
        if (mode=='series'){
            dt <- data[!is.na(data$x) & !is.na(data$y) & data$series==series,]
            idx <- which(unique(data$series)==series)
        }else{
            dt <- data[!is.na(data$x) & !is.na(data$y) & data$idx==series,]
            idx <- which(unique(data$idx)==series)
        }
        iType <- type[idx,]
        validSubtypes <- eval(parse(text=iType$subtype))
        iSubtype <- subtype[[idx]]
        if (!identical(iSubtype, ''))
            iSubtype <- validSubtypes[which(tolower(validSubtypes) %in% iSubtype)]
        if (length(iSubtype[!iSubtype %in% c(
            "", "sum", "average", "scale", "move")]) == 0){
            mapType <- gsub("^.*(china|world|newmap).*$", "\\1", iType$misc)
        }else{
            if (mode=='series'){
                if (grepl('world', iType$misc))
                    mapType <-paste('world',
                          iSubtype[! iSubtype %in% c(
                              'sum', 'average', 'scale', 'move')][1],
                          sep='|')
                else
                    mapType <- iSubtype[! iSubtype %in% c(
                        'sum', 'average', 'scale', 'move')][1]
            }else{
                mapType <- paste(gsub("^.*(china|world).*$", "\\1", iType$misc),
                      iSubtype[! iSubtype %in% c(
                          'sum', 'average', 'scale', 'move')][1], sep="|")
            }
        }
        o <- list(
            type=iType$type, mapType=mapType,
            itemStyle=list(normal=list(label=list(show=FALSE)),
                           emphasis=list(label=list(show=TRUE))),
            data=list()
        )
        if ('move' %in% iSubtype){
            o$roam <- 'move'
            if ('scale' %in% iSubtype) o$roam <- TRUE
        }else if ('scale' %in% iSubtype){
            o$roam <- 'scale'
        }
        o$name <- if (!is.null(lst$y)) names(lst$y)[1]
        if (grepl('multi', iType$misc)) o$selectedMode <- 'multiple'
        if (mode=='split')
            o$mapLocation <- list(
                x=paste0(ul.corners[idx,1], '%'),
                y=paste0(ul.corners[idx,2], '%'),
                width=paste0((90-4*cols)/cols, '%'),
                height=paste0((90-4*rows)/rows, '%'))
        if (nrow(dt) > 0)
            o$data <- unname(apply(dt, 1, function(row){
                list(name=as.character(unname(row['x'])),
                     value=ifna(as.numeric(unname(row['y'])), '-'),
                     selected=ifna(as.logical(unname(row['sel'])), FALSE))
            }))
        if (mode=='series') if (series != "") o$name <- series
        if (mode=='split') if (! (is.na(dt$series) || all(dt$series=='')))
            o$name <- dt$series[1]
        if ('average' %in% iSubtype) o$mapValueCalculation <- 'average'

        return(o)
    })

    if (is.null(return)){
        return(out)
    }else{
        return(out[intersect(names(out), return)])
    }
}


series_wordCloud <- function(lst, type, subtype, return=NULL, ...){
    # does not accept series
    if (is.null(lst$y) || is.null(lst$x))
        stop('wordCloud charts need x and y!')
    data <- data.frame(lst$y[,1], lst$x[,1])
    names(data)[1:2] <- c('y', 'x')
    data <- data.table::dcast(data, x~., sum, value.var='y')
    names(data)[2] <- 'y'

    colors <- getColFromPal()

    if (is.null(lst$series)){
        o <- list(list(data=unname(apply(data, 1, function(row){
            list(name=unname(row['x']), value=unname(ifna(as.numeric(row['y']), '-')),
                 itemStyle=list(normal=list(color=sample(colors,1))))
            })), textRotation=c(0,-45,-90,45,90), type=type$type[1],
            size=list('80%', '80%')))

    }else{
        data$series <- as.factor(lst$series[,1])
        if (length(colors) < length(nlevels(data$series)))
            colors <- rep(colors, ceiling(nlevels(data$series)/length(colors)))
        data$color <- colors[as.numeric(data$series)]

        o <- list(list(data=unname(apply(data, 1, function(row){
            list(name=unname(row['x']), value=unname(ifna(as.numeric(row['y']), '-')),
                 itemStyle=list(normal=list(color=unname(row['color']))))
        })), textRotation=c(0,-45,45,90), type=type$type[1],
        size=list('80%', '80%')))
        attr(o[[1]]$data, 'meta') <- data$series
    }

    if (is.null(return)){
        return(o)
    }else{
        return(o[intersect(names(o), return)])
    }
}

series_eventRiver <- function(lst, type, subtype, return=NULL, ...){
    # x: slice time, event name, slice title, slice url, slice img;
    # y: slice value, event weight;  series: series, series weight
    if (is.null(lst$x) || is.null(lst$y)) stop('eventRiver chart needs x and y!')
    if (ncol(lst$x) < 2)
        stop(paste('x should be comprised of 2 compulsory columns:',
                   'event slice time, events name and 3 optional columns:',
                   'event slice title, event slice links, event slice images.',
                   '(the exact order)'))
    if (!is.numeric(lst$x[,1]))
        stop('x[,1] should be transformed to time first.')
    if (any(duplicated(paste(lst$x[,1], lst$x[,2]))))
        stop(paste('No duplicated combination of x[,1] and x[,2] is allowed!',
                   'Please check row', which(duplicated(paste(lst$x[,1], lst$x[,2])))), '.')
    if (ncol(lst$y) < 2) lst$y[,2] <- 1

    data <- cbind(lst$y[,1:2], lst$x[,1:2])
    names(data) <- c('value', 'weight', 'time', 'event')
    data$slice <- if (ncol(lst$x) >= 3) lst$x[,3] else NA
    data$link <- if (ncol(lst$x) >= 4) lst$x[,4] else NA
    data$image <- if (ncol(lst$x) >= 5) lst$x[,5] else NA
    if (is.null(lst$series)) {
        data$series <- ''
        data$seriesWgt <- 1
    }else{
        data$series <- lst$series[,1]
        data$seriesWgt <- if (ncol(lst$series) > 1) lst$series[,2] else 1
    }

    series <- unique(as.character(data$series))
    data$time <- format(convTimestamp(data$time, 'JS', 'R'), "%Y-%m-%d %T")

    out <- lapply(series, function(s){
        type <- type[which(series == s),]
        dt <- data[data$series==s,]
        o <- list(type=type$type, data=lapply(as.character(unique(dt$event)),
                                              function(event){
            ds <- dt[dt$event==event,]
            list(name=event, weight=ifna(as.numeric(ds$weight[1]), '-'),
                 evolution=unname(apply(ds, 1, function(row){
                     evo <- list(
                         time=unname(row['time']),
                         value=ifna(as.numeric(unname(row['value'])), '-'),
                         detail=list())
                     if (!is.na(row['link']))
                         evo$detail[['link']] = unname(row['link'])
                     if (!is.na(row['slice']))
                         evo$detail[['text']] = unname(row['slice'])
                     if (!is.na(row['image']))
                         evo$detail[['img']] = unname(row['image'])
                     return(evo)
                 })))
            }), weight=as.numeric(dt$seriesWgt[1])
            )
        if (s != '') o$name <- unname(s)
        return(o)
    })

    if (is.null(return)){
        return(out)
    }else{
        return(out[intersect(names(out), return)])
    }
}

series_venn <- function(lst, type, return=NULL, ...){
    if (is.null(lst$x) || is.null(lst$y)) stop('venn charts need x and y!')
    if (nrow(lst$y) < 3) stop('y has to have 3 rows with the last row be intersection.')
    data <- data.frame(y=lst$y[,1], x=lst$x[,1])[1:3,]
    o <- list(list(type='venn', itemStyle=list(
        normal=list(label=list(show=TRUE)),
        emphasis=list(borderWidth=3, borderColor='yellow')
        ),
        data=unname(apply(data, 1, function(row){
            list(value=unname(ifna(as.numeric(row['y']), '-')),
                 name=unname(row['x']))
        }))
    ))

    if (is.null(return)){
        return(o)
    }else{
        return(o[intersect(names(o), return)])
    }
}

series_tree <- function(lst, type, subtype, return=NULL, ...){
    if (is.null(lst$x) || is.null(lst$y))
        stop("tree/treemap charts need x and y!")
    if (ncol(lst$x) < 2 && any(type$type == 'tree'))
        stop(paste('for tree charts, x must contain 2 columns. x[,1] is node name,',
                   'x[,2] is parent node name.'))
    data <- data.frame(value=lst$y[,1], name=lst$x[,1],
                       parent=if (ncol(lst$x)<2) NA else lst$x[,2])

    data$series <- if (is.null(lst$series)) '' else lst$series[,1]
    nSeries <- length(unique(data$series))
    if (nSeries > 4) warning('Too many series! The layout will be messy!')

    out <- lapply(unique(data$series), function(series){
        idx <- which(unique(data$series) == series)
        dt <- data[data$series == series, c('name', 'value', 'parent')]
        iType <- type[idx,]
        iSubtype <- subtype[[idx]]
        orient <- ifelse(grepl('horizontal', iType$misc), 'horizontal', 'vertical')
        inv <- grepl('inv', iType$misc)
        center <- list(paste0(5+90/nSeries*(idx-0.5), '%'), '50%')
        size <- list(paste0((90-nSeries*5)/nSeries, '%'), '80%')
        lineType <- ifelse('broken' %in% iSubtype, 'broken',
                           ifelse('dotted' %in% iSubtype, 'dotted',
                                  ifelse('solid' %in% iSubtype, 'solid',
                                         ifelse('dashed' %in% iSubtype,
                                                      'dashed', 'curve'))))

        o <- list(type=iType$type, orient=orient, roam=TRUE,
                  direction=ifelse(inv, 'inverse', ''),
                  data=parseTreeNodes(dt)
        )
        if (series != '') o$name <- as.character(series) else
            if (iType$type=='treemap') o$name <- as.character(names(lst$x))[1]
        if (iType$type == 'tree'){
            o$nodePadding <- 1
            o$rootLocation <- list(
                x=ifelse(orient=='vertical',
                         paste0(10+80/nSeries*(idx-0.5), '%'),
                         ifelse(inv, paste0(10+80/nSeries*(idx), '%'),
                                paste0(10+80/nSeries*(idx-1), '%'))),
                y=ifelse(orient=='vertical', ifelse(inv, '90%', '10%'),
                         '50%'))
            o$itemStyle=list(normal=list(
                label=list(show=FALSE, formatter="{b}"),
                lineStyle=list(
                    color='#48b', shadowColor='#000', shadowBlur=3,
                    shadowOffsetX=2, shadowOffsetY=3, type=lineType)),
                emphasis=list(label=list(show=TRUE))
            )
        }else if (iType$type == 'treemap'){
            o$center <- center
            o$size <- size
            o$itemStyle=list(normal=list(
                label=list(show=TRUE, formatter="{b}")),
                emphasis=list(label=list(show=TRUE))
            )
        }
        return(o)
    })

    if (is.null(return)){
        return(out)
    }else{
        return(out[intersect(names(out), return)])
    }
}

series_treemap <- series_tree

#' @importFrom data.table between
series_heatmap <- function(lst, type, subtype, return=NULL, ...){
    # data = rbind(data.frame(lng=100+rnorm(100,0, 1)*600,
    #        lat=150+rnorm(100,0, 1)*50, y=abs(rnorm(100,0,1))),
    # data.frame(lng=rnorm(200,0, 1)*1000,
    #        lat=rnorm(200,0, 1)*800, y=abs(rnorm(200,0,1))),
    # data.frame(lng=400+rnorm(20,0, 1)*300,
    #        lat=5+rnorm(20,0, 1)*10, y=abs(rnorm(100,0,1))))
    # echartr(data,lng=lng,lat=lat,y=y,type='heatmap')
    if (is.null(lst$lng) || is.null(lst$lat) || is.null(lst$y))
        stop("heatmap needs lng, lat and y!")
    if (!all(data.table::between(lst$y[,1], 0, 1)))
        lst$y[,1] = (max(lst$y[,1], na.rm=TRUE)-lst$y[,1]) /
            (max(lst$y[,1], na.rm=TRUE)-min(lst$y[,1], na.rm=TRUE))

    data <- data.frame(y=lst$y[,1], lng=lst$lng[,1], lat=lst$lat[,1])
    o <- list(list(type=type$type[1], minAlpha=0.2, opacity=0.6,
              gradientColors=c('blue', 'cyan', 'limegreen', 'yellow', 'red'),
              data=asEchartData(unname(data[,c('lng', 'lat', 'y')]))
    ))
    if (is.null(return)){
        return(o)
    }else{
        return(o[intersect(names(o), return)])
    }
}

#---------------------------legacy functions-----------------------------------
# split the data matrix for a scatterplot by series
data_scatter = function(x, y, series = NULL, type = 'scatter') {
  xy = unname(cbind(x, y))
  if (is.null(series)) return(list(list(type = type, data = xy)))
  xy = split(as.data.frame(xy), series)
  nms = names(xy)
  obj = list()
  for (i in seq_along(xy)) {
    obj[[i]] = list(name = nms[i], type = type, data = unname(as.matrix(xy[[i]])))
  }
  obj
}

data_bar = function(x, y, series = NULL, type = 'bar') {

  # plot the frequencies of x when y is not provided
  if (is.null(y)) {

    if (is.null(series)) {
      y = table(x)
      return(list(list(type = type, data = unname(c(y)))))
    }

    y = table(x, series)
    nms = colnames(y)
    obj = list()
    for (i in seq_len(ncol(y))) {
      obj[[i]] = list(name = nms[i], type = type, data = unname(y[, i]))
    }
    return(obj)

  }

  # when y is provided, use y as the height of bars
  if (is.null(series)) {
    return(list(list(type = type, data = y)))
  }

  xy = tapply(y, list(x, series), function(z) {
    if (length(z) == 1) return(z)
    stop('y must only have one value corresponding to each combination of x and series')
  })
  xy[is.na(xy)] = 0
  nms = colnames(xy)
  obj = list()
  for (i in seq_len(ncol(xy))) {
    obj[[i]] = list(name = nms[i], type = type, data = unname(xy[, i]))
  }
  obj

}

data_line = function(x, y, series = NULL) {
  if (is.null(x) && is.ts(y)) {
    x = as.numeric(time(y))
    y = as.numeric(y)
  }
  if (is.numeric(x)) {
    return(data_scatter(x, y, series, type = 'line'))
  }
  if (is.null(series)) {
    return(list(list(type = 'line', data = y)))
  }
  data_bar(x, y, series, type = 'line')
}

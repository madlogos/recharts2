## ------------------------------------------------------------------------
library(recharts)
echartr(iris, ~Sepal.Length, ~Sepal.Width, series = ~Species)

## ------------------------------------------------------------------------
head(mtcars)

## ------------------------------------------------------------------------
echartr(mtcars, wt, mpg)

## ---- echo=FALSE---------------------------------------------------------
str(args(echartr))

## ------------------------------------------------------------------------
knitr::kable(recharts:::validChartTypes[,c(1:3,5)])

## ------------------------------------------------------------------------
echartr(mtcars, wt, mpg, factor(am, labels=c('Automatic', 'Manual')))

## ------------------------------------------------------------------------
echartr(mtcars, wt, mpg, am, weight=gear, type='bubble')

## ------------------------------------------------------------------------
d <- data.table::dcast(mtcars, carb+gear~., mean, value.var='mpg')
names(d)[3] <- 'mean.mpg'
d$carb <- as.character(d$carb)
echartr(d, carb, "mean.mpg", gear, type=c('vbar', 'vbar', 'line')) %>% 
    setSymbols('emptycircle')

## ------------------------------------------------------------------------
echartr(d, carb, mean.mpg, gear, type='line', 
        subtype=c('stack + smooth', 'stack + dotted', 'smooth + dashed')) %>%
    setSymbols('emptycircle')

## ------------------------------------------------------------------------
g = echartr(mtcars, wt, mpg, factor(am, labels=c('Automatic', 'Manual')))

## ------------------------------------------------------------------------
g %>% setSeries(series=2, symbolSize=8, symbolRotate=30)

## ------------------------------------------------------------------------
g %>% addMarkLine(data=data.frame(type='average', name1='Avg'))

## ------------------------------------------------------------------------
g %>% addMarkPoint(series=1, data=data.frame(type='max', name='Max'))

## ------------------------------------------------------------------------
link <- 'https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html'
g %>% setTitle('wt vs mpg', paste0('[Motor Trend](', link, ')'), 
               textStyle=list(color='red'))

## ------------------------------------------------------------------------
g %>% setLegend(selected='Automatic', textStyle=list(color='lime'))

## ------------------------------------------------------------------------
g %>% setToolbox(lang='en', pos=2)

## ------------------------------------------------------------------------
g %>% setDataZoom()

## ------------------------------------------------------------------------
g %>% setXAxis(min=0) %>% setYAxis(min=0)

## ------------------------------------------------------------------------
g %>% setTheme('dark', calculable=TRUE)

## ------------------------------------------------------------------------
g %>% setSymbols(c('heart', 'star6'))

## ------------------------------------------------------------------------
g %>% setSeries(series=2, symbolSize=8, symbolRotate=30) %>% 
    addMarkLine(data=data.frame(type='average', name1='Avg')) %>%
    addMarkPoint(series=1, data=data.frame(type='max', name='Max')) %>%
    setTitle('wt vs mpg', paste0('[Motor Trend](', link, ')'), 
               textStyle=list(color='red')) %>%
    setLegend(selected='Automatic', textStyle=list(color='lime')) %>%
    setToolbox(lang='en', pos=2) %>% setDataZoom() %>% 
    setTheme('dark', calculable=TRUE) %>% setSymbols(c('heart', 'star6'))

## ------------------------------------------------------------------------
chordEx1 = list(
  title = list(
    text = '测试数据',
    subtext = 'From d3.js',
    x = 'right',
    y = 'bottom'
  ),
  tooltip = list(
    trigger = 'item',
    formatter = JS('function(params) {
      if (params.indicator2) { // is edge
        return params.value.weight;
      } else {// is node
        return params.name
      }
    }')
  ),
  toolbox = list(
    show = TRUE,
    feature = list(
      restore = list(show = TRUE),
      magicType = list(show = TRUE, type = c('force', 'chord')),
      saveAsImage = list(show = TRUE)
    )
  ),
  legend = list(
    x = 'left',
    data = c('group1', 'group2', 'group3', 'group4')
  ),
  series = list(
    list(
      type = 'chord',
      sort = 'ascending',
      sortSub = 'descending',
      showScale = TRUE,
      showScaleText = TRUE,
      data = list(
        list(name = 'group1'),
        list(name = 'group2'),
        list(name = 'group3'),
        list(name = 'group4')
      ),
      itemStyle = list(
        normal = list(
          label = list(show = FALSE)
        )
      ),
      matrix = rbind(
        c(11975,  5871, 8916, 2868),
        c( 1951, 10048, 2060, 6171),
        c( 8010, 16145, 8090, 8045),
        c( 1013,   990,  940, 6907)
      )
    )
  )
)

echart(chordEx1)


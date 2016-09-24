
tooltipJS <- function(type) {
    # tooltipJS templates for echarts
    js <- list()

    js[['scatter']] <- JS('function (params) {
    var i;
    var text = params.value[0];
    if (params.seriesName === null || params.seriesName === ""){
        if (params.value.length > 1) {
            for (i = 1; i < params.value.length; i++){
                text += " ,    " + params.value[i];
            }
            return text;
        } else {
            return params.name + " : " + text;
        }
    } else {
        if (params.value.length > 1) {
            text = params.seriesName + " :<br/>" + text;
            for (i = 1; i < params.value.length; i++) {
                text += " ,    " + params.value[i];
            }
            return text;
        } else {
            return params.seriesName + " :<br/>"
            + params.name + " : " + text;
        }
    }
    }')

	js[['scatter_time']] <- JS('function (params) {
    var i;
    var text;
    var sDate = new Date(params.value[0]);
    if (sDate.getMonth && !isNaN(sDate.getMonth())) {
        if (sDate.getMinutes() < 10) {
            min = "0" + sDate.getMinutes();
        } else {
            min = sDate.getMinutes();
        }
        if (sDate.getSeconds() < 10) {
            sec = "0" + sDate.getSeconds();
        } else {
            sec = sDate.getSeconds();
        }
        text = sDate.getFullYear() + "-"
            + (sDate.getMonth() + 1) + "-"
            + sDate.getDate() + " "
            + sDate.getHours() + ":"
            + min + ":" + sec;
    } else {
        text = params.value[0];
    }
    if (params.seriesName === null || params.seriesName === ""){
        if (params.value.length > 1) {
            for (i = 1; i < params.value.length; i++){
                text += " ,    " + params.value[i];
            }
            return text;
        } else {
            return params.name + " : " + text;
        }
    } else {
        if (params.value.length > 1) {
            text = params.seriesName + " :<br/>" + text;
            for (i = 1; i < params.value.length; i++) {
                text += " ,    " + params.value[i];
            }
            return text;
        } else {
            return params.seriesName + " :<br/>"
            + params.name + " : " + text;
        }
    }
    }')

    js[['chord_mono']] <- JS('function (params) {
    if (params.name && params.name.indexOf("-") != -1) {
        return params.name.replace("-", " " + params.seriesName + " ");
    }
    else {
        return params.name ? params.name : params.data.id;
    }
    }')

    js[['chord_multi']] <- JS('function (params) {
    if (params.indicator2) {    // is edge
        if (isNaN(params.value.weight)){
            return params.indicator + " -> "
            + params.indicator2 + " (" + params.name + ")";
        } else {
            return params.indicator + " -> "
            + params.indicator2 + " (" + params.name + ") : "
            + params.value.weight;
        }

    } else {    // is node
        return params.name;
    }
    }')

    js[['pie']] <- '{a} <br/>{b} : {c} ({d}%)'

    js[['line']] <- JS('function (params) {
       var i;
       var j;
       var text;
       var text1;
       var sName = "";
       if (params.length > 1){
            if (params[0].value.length >1) {
                text1 = params[0].value[0];
            } else {
                text1 = params[0].value;
            }
            text = params[0].name;
            if (params[0].seriesName) {
                sName = params[0].seriesName;
            }
       } else {
            if (params[0].value.length > 1) {
                text1 = params[0].value[0];
            } else {
                text1 = params[0].value;
            }
            text = params[0].name;
            if (params[0].seriesName) {
                sName = params[0].seriesName;
            }
       }
       if (params.length > 1){
           text += "<br/>" + params[0].seriesName + " :  " + text1;
           if (params[0].value.length > 1){
                for (j = 1; j < params[0].value.length; j++){
                    text += ", " + params[0].value[j];
                }
           }
           for (i = 1; i < params.length; i++) {
                if (params[i].value.length > 1){
                   text += "<br/>" + params[i].seriesName + " :  "
                        + params[i].value[0];
                   for (j = 1; j < params[i].value.length; j++){
                        text += ", " + params[i].value[j];
                   }
                } else {
                   text += "<br/>" + params[i].seriesName + " :  "
                        + params[i].value;
                }
           }
       } else {
           if (params[0].seriesName){
               text += "<br/>" + params[0].seriesName + " :  " + text1;
           } else {
               text += " :  " + text1;
           }
           if (params[0].value.length > 1){
                for (j = 1; j < params[0].value.length; j++){
                    text += ", " + params[0].value[j];
                }
           }
       }
       return text;
    }')

	js[['line_time']] <- JS('function (params) {
       var i;
       var j;
       var sDate;
       var text;
       var text1;
       var sName = "";
       if (params.length > 1){
            if (params[0].value.length >1) {
                text1 = params[0].value[0];
            } else {
                text1 = params[0].value;
            }
            sDate = new Date(text1);
            text = params[0].name;
            if (params[0].seriesName) {
                sName = params[0].seriesName;
            }
       } else {
            if (params.value.length > 1) {
                text1 = params.value[0];
            } else {
                text1 = params.value;
            }
            sDate = new Date(text1);
            text = params.name;
            if (params.seriesName) {
                sName = params.seriesName;
            }
       }
       if (sDate.getMonth && !isNaN(sDate.getMonth())) {
            if (sDate.getMinutes() < 10) {
                min = "0" + sDate.getMinutes();
            } else {
                min = sDate.getMinutes();
            }
            if (sDate.getSeconds() < 10) {
                sec = "0" + sDate.getSeconds();
            } else {
                sec = sDate.getSeconds();
            }
            text1 = sName + sDate.getFullYear() + "-"
                + (sDate.getMonth() + 1) + "-"
                + sDate.getDate() + " "
                + sDate.getHours() + ":" + min + ":" + sec;
       }
       if (params.length > 1) {
           text += "<br/>" + params[0].seriesName + " :  " + text1;
           if (params[0].value.length > 1){
                for (j = 1; j < params[0].value.length; j++) {
                    text += ", " + params[0].value[j];
                }
           }
           for (i = 1; i < params.length; i++) {
                if (params[i].value.length > 1) {
                   text += "<br/>" + params[i].seriesName + " :  "
                        + params[i].value[0];
                   for (j = 1; j < params[i].value.length; j++) {
                        text += ", " + params[i].value[j];
                   }
                } else {
                   text += "<br/>" + params[i].seriesName + " :  "
                        + params[i].value;
                }
           }
       } else {
           if (params.seriesName) {
               text += "<br/>" + params.seriesName + " :  " + text1;
           } else {
               text = text1;
           }
           if (params.value.length > 1){
                for (j = 1; j < params.value.length; j++) {
                    text += ", " + params.value[j];
                }
           }
       }
       return text;
    }')

    js[['k']] <- JS('function (params) {
        var res = params[0].name;
        res += "<br/>Open\u5F00\u76D8 : " + params[0].value[0] +
        "<br/>High\u6700\u9AD8 : " + params[0].value[3];
        res += "<br/>Close\u6536\u76D8 : " + params[0].value[1] +
        "<br/>Low\u6700\u4F4E : " + params[0].value[2];
        return res;
    }')

    js[['hist']] <- JS('function (params){
        return "~ " + params.value[2] + " ~<br/>:" + params.value[1];
    }')

    switch(type,
           scatter = js$scatter, scatter_time = js$scatter_time,
           chord_mono = js$chord_mono,
           chord_multi = js$chord_multi, pie = js$pie, k = js$k, line = js$line,
           line_time = js$line_time, bar = js$line, bar_time = js$line_time,
           hist = js$hist)
    }

convFormat2JS <- function(fmt, type=c("value", "category", "time")){
    # fmt is fmt char in sprintf()
    type <- match.arg(type)
    js <- NULL
    if (type %in% c('value', 'log')){
        if (grepl("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[dfGg](.*)$", fmt)){
            pre = gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[dfGg](.*)$","\\1", fmt)
            n = as.numeric(gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[dfGg](.*)$",
                                "\\3", fmt))
            unit = gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[dfGg](.*)$","\\4", fmt)
            n = ifna(n, 0)
            js = JS(paste0("function(x){return '", pre, "' + x.toFixed(", n,
                           ") + '", unit, "';}"))
        }else if (grepl("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[Ee](.*)$", fmt)){
            pre = gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[Ee](.*)$","\\1", fmt)
            n = as.numeric(gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[Ee](.*)$",
                                "\\3", fmt))
            unit = gsub("^%([+-]*| *)(\\d*)\\.{0,1}(\\d*)[Ee](.*)$","\\4", fmt)
            n = ifna(n, 0)
            js = JS(paste0("function(x){return '", pre, "' + x.toExponential(",
                           n, ") + '", unit, "';}"))
        }
    }else if (type == 'category'){
        if (grepl("^(.*)%s(.*)$", fmt)){
            pre = gsub("^(.*)%s(.*)$","\\1", fmt)
            unit = gsub("^(.*)%s(.*)$","\\2", fmt)
            js = JS(paste0("function(x){return '", pre, "' + x + '", unit, "';}"))
        }
    ## for time, fmt is a strftime format
    }else if (type == 'time'){
        if (fmt == '%F')  # %Y-%m-%d
            js = JS(paste0("function(x) {return x.getFullYear() + '-' + ",
                           "x.getMonth() + 1 + '-' + x.getDate();}"))
        if (fmt == '%D')  # %m/%d/%y
            js = JS(paste0("function(x) {return x.getMonth() + 1 + '/' + ",
                           "x.getDate() + '/' + x.getYear();}"))
        if (fmt == '%c')  # %a %b %e %H:%M:%S %Y
            js = JS(paste0("function(x) {
                           var monthNames = ['January', 'February', 'March',
                           'April', 'May', 'June', 'July', 'August', 'September',
                           'October', 'November', 'December'];",
                           "var weekNames = ['Monday', 'Tuesday', 'Wednesday',
                           'Thursday', 'Friday', 'Saturday', 'Sunday'];",
                           "return weekNames[x.getDay()] + ' ' + ",
                           "monthNames[x.getMonth()] + ' ' + x.getDate() + ' ' + ",
                           "x.getHours() + ':' + x.getMinutes() + ':' + ",
                           "x.getSeconds() + ' ' + x.getFullYear();}"))
        if (fmt %in% c('%T','%X'))  # %H:%M:%S
            js = JS(paste0("function(x) {return x.getHours() + ':' + ",
                           "x.getMinutes() + ':' + x.getSeconds();}"))
        if (fmt %in% c('%R'))  # %H:%M
            js = JS(paste0("function(x) {return x.getHours() + ':' + ",
                           "x.getMinutes();}"))
        if (fmt %in% c('%x'))  # %y/%m/%d
            js = JS(paste0("function(x) {return x.getYear() + '/' + ",
                           "x.getMonth() + 1 + '/' + x.getDate();}"))
        if (is.null(js)){
            js = fmt
            js = gsub("%[Aa]", "' + weekNames[x.getDay()] + '", js)
            js = gsub("%[Bbh]", "' + monthNames[x.getMonth()] + '", js)
            js = gsub("%C", "' + floor(x.getFullYear() / 100) + '", js)
            js = gsub("%[de]", "' + x.getDate() + '", js)
            js = gsub("%[gy]", "' + x.getYear() + '", js)
            js = gsub("%[GY]", "' + x.getFullYear() + '", js)
            js = gsub("%H", "' + x.getHours() + '", js)
            js = gsub("%I", paste0("' + x.getHours()>12 ? x.getHours()-12 : '",
                                   "x.getHours() + "), js)
            js = gsub("%M", "' + x.getMinutes() + '", js)
            js = gsub("%m", "' + x.getMonth() + 1 + '", js)
            js = gsub("%p", "' + x.getHours()>12 ? 'PM' : 'AM' + '", js)
            js = gsub("%S", "' + x.getSeconds() + '", js)
            js = gsub("%u", "' + weekNames[x.getDay()] + '", js)
            js = gsub("%w", "' + x.getDay() + '", js)
            js = paste0("function(x) {",
                        "var monthNames = ['January', 'February', 'March',
                        'April', 'May', 'June', 'July', 'August', 'September',
                        'October', 'November', 'December'];",
                        "var weekNames = ['Monday', 'Tuesday', 'Wednesday',
                        'Thursday', 'Friday', 'Saturday', 'Sunday'];",
                        "return ", substr(js, 4, nchar(js)))
            js = gsub("^(.+) \\+ ' *$", "\\1;\\}", js)
            js = JS(gsub("\\+ '' \\+", "+", js))
        }
    }
    if (is.null(js)) {
        if (grepl("^\\{.+\\}.*$", fmt)) js <- fmt
        else js <- JS("function (x) {return x;}")
    }
    return(js)
}


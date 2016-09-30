(function (root, factory) {
    if (typeof define === 'function' && define.amd) {
        // AMD. Register as an anonymous module.
        define(['exports', 'echarts'], factory);
    } else if (typeof exports === 'object' && typeof exports.nodeName !== 'string') {
        // CommonJS
        factory(exports, require('echarts'));
    } else {
        // Browser globals
        factory({}, root.echarts);
    }
}(this, function (exports, echarts) {
    var log = function (msg) {
        if (typeof console !== 'undefined') {
            console && console.error && console.error(msg);
        }
    };
    if (!echarts) {
        log('ECharts is not Loaded');
        return;
    }

    var colorPalette = [
		'#8aedd5','#93bc9e','#cef1db','#7fe579','#a6d7c2',
        '#bef0bb','#99e2vb','#94f8a8','#7de5b8','#4dfb70'
	];


    var theme = {
        color: colorPalette,

        visualMap: {
            itemWidth: 15,
            color: ['#93bc92','#bef0bb']
        },

		// K线图默认参数
		candlestick: {
			// barWidth : null          // 默认自适应
			// barMaxWidth : null       // 默认自适应 
			itemStyle: {
				normal: {
					color: '#8aedd5',          // 阳线填充颜色
					color0: '#7fe579',      // 阴线填充颜色
					lineStyle: {
						width: 1,
						color: '#8aedd5',   // 阳线边框颜色
						color0: '#7fe579'   // 阴线边框颜色
					}
				},
				emphasis: {
					// color: 各异,
					// color0: 各异
				}
			}
		},
		
		// 饼图默认参数
		pie: {
			label: {
				normal: {
					show: true,
					position: 'outer',
					textStyle: {color: '#1b1b1b'},
					lineStyle: {color: '#1b1b1b'}
						// textStyle: null      // 默认使用全局文本样式，详见TEXTSTYLE
				}
			},
			labelLine: {
				normal: {
					show: true,
					length: 20,
					lineStyle: {
						width: 1,
						type: 'solid'
					}
				}
			},
			itemStyle: {
				normal: {
					// color: 各异,
					borderColor: '#fff',
					borderWidth: 1
				}
			}
		},				
		
		map: {			
			showLegendSymbol : true,       // 显示图例颜色标识（系列标识的小圆点），存在legend时生效
			label: {
				normal: {
					show: false,
					textStyle: {
						color: 'rgba(139,69,19,1)'
					}
				},
				emphasis: {
					show: false,
					textStyle: {
						color: 'rgba(139,69,19,1)'
					}
				}
			},
			itemStyle: {
				normal: {
					// color: 各异,
					borderColor: '#fff',
					borderWidth: 1,
					areaColor: '#ccc'//rgba(135,206,250,0.8)					
				},
				emphasis: {                 // 也是选中样式
					// color: 各异,
					borderColor: 'rgba(0,0,0,0)',
					borderWidth: 1,
					areaColor: '#f3f39d'
				}
			}
		},
		
        graph: {
            color: colorPalette,
			label: {
				normal: {
					show: false
				},
				emphasis: {
					show: false
				}
			},
			links: {
				lineStyle: {
					normal: {
						strokeColor : '#49b485'
					}
				}
			},
			lineStyle: {
				normal: {
					color : 'rgba(128, 128, 128, 0.5)'
				},
				emphasis: {
					color : 'rgba(128, 128, 128, 0.5)'
				}
			},
			itemStyle : {
				normal : {
					brushType : 'both',
					strokeColor : '#49b485'
				},
				emphasis : {}
			}
        },		
		
		gauge : {
			axisLine: {            // 坐标轴线
				show: true,        // 默认显示，属性show控制显示与否
				lineStyle: {       // 属性lineStyle控制线条样式
					color: [[0.2, '#93bc9e'],[0.8, '#8aedd5'],[1, '#a6d7c2']], 
					width: 8
				}
			},
			axisTick: {            // 坐标轴小标记
				splitNumber: 10,   // 每份split细分多少段
				length :12,        // 属性length控制线长
				lineStyle: {       // 属性lineStyle控制线条样式
					color: 'auto'
				}
			},
			axisLabel: {           // 坐标轴文本标签，详见axis.axisLabel
				textStyle: {       // 其余属性默认使用全局文本样式，详见TEXTSTYLE
					color: 'auto'
				}
			},
			splitLine: {           // 分隔线
				length : 18,         // 属性length控制线长
				lineStyle: {       // 属性lineStyle（详见lineStyle）控制线条样式
					color: 'auto'
				}
			},
			pointer : {
				length : '90%',
				color : 'auto'
			},
			title : {
				textStyle: {       // 其余属性默认使用全局文本样式，详见TEXTSTYLE
					color: '#333'
				}
			},
			detail : {
				textStyle: {       // 其余属性默认使用全局文本样式，详见TEXTSTYLE
					color: 'auto'
				}
			}
		}
    };

    echarts.registerTheme('mint', theme);
}));

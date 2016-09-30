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
        '#d8361b','#f16b4c','#f7b4a9','#d26666',
        '#99311c','#c42703','#d07e75'
    ];


    var theme = {
        color: colorPalette,

		// 图表标题
		title: {
			textStyle: {
				fontWeight: 'normal',
				color: '#d8361b'
			}
		},
		
		// 值域
        visualMap: {
            itemWidth: 15,
            color: ['#bd0707','#ffd2d2']
        },
		
		// 工具箱
		toolbox: {
            color : ['#d8361b','#d8361b','#d8361b','#d8361b'],
			iconStyle: {
                normal: {
                    borderColor: colorPalette[0]
                }
            }
        },
		
		// 提示框		
		tooltip: {
			backgroundColor: 'rgba(0,0,0,0.5)',
			axisPointer : {            // 坐标轴指示器，坐标轴触发有效
				type : 'line',         // 默认为直线，可选为：'line' | 'shadow'
				lineStyle : {          // 直线指示器样式设置
					color: '#d8361b',
					type: 'dashed'
				},
				crossStyle: {
					color: '#d8361b'
				},
				shadowStyle : {                     // 阴影指示器样式设置
					color: 'rgba(200,200,200,0.3)'
				}
			}
		},

		// 区域缩放控制器
		dataZoom: {
			dataBackgroundColor: '#eee',            // 数据背景颜色
			fillerColor: 'rgba(216,54,27,0.2)',   // 填充颜色
			handleColor: '#d8361b'     // 手柄颜色
		},
		
		// 网格
		grid: {
			borderWidth: 0
		},


		// 类目轴
		categoryAxis: {
			axisLine: {            // 坐标轴线
				lineStyle: {       // 属性lineStyle控制线条样式
					color: '#d8361b'
				}
			},
			splitLine: {           // 分隔线
				lineStyle: {       // 属性lineStyle（详见lineStyle）控制线条样式
					color: ['#eee']
				}
			}
		},

		// 数值型坐标轴默认参数
		valueAxis: {
			axisLine: {            // 坐标轴线
				lineStyle: {       // 属性lineStyle控制线条样式
					color: '#d8361b'
				}
			},
			splitArea : {
				show : true,
				areaStyle : {
					color: ['rgba(250,250,250,0.1)','rgba(200,200,200,0.1)']
				}
			},
			splitLine: {           // 分隔线
				lineStyle: {       // 属性lineStyle（详见lineStyle）控制线条样式
					color: ['#eee']
				}
			}
		},

		timeline : {
			lineStyle : {
				color : '#d8361b'
			},
			controlStyle : {
				normal : { color : '#d8361b'},
				emphasis : { color : '#d8361b'}
			}
		},

		textStyle: {
			fontFamily: '微软雅黑, Arial, Verdana, sans-serif'
		},
		
		// K线图默认参数
		candlestick: {
			itemStyle: {
				normal: {
					color: '#f16b4c',          // 阳线填充颜色
					color0: '#f7b4a9',      // 阴线填充颜色
					lineStyle: {
						width: 1,
						color: '#d8361b',   // 阳线边框颜色
						color0: '#d26666'   // 阴线边框颜色
					}
				}
			}
		},

        map: {
            label: {
                normal: {
                    textStyle: {
						color: '#ddd'
					}
                },
				emphasis: {
					textStyle: {
						color: '#c12e34'
					}
				}
            },
            itemStyle: {
                normal: {
                    borderColor: '#eee',
                    areaColor: '#ddd'
                },
                emphasis: {
                    areaColor: '#fe994e'
                }
            }
        },		
		
        graph: {
            color: colorPalette,
			links: {
				lineStyle: {
					normal: {
						color : '#d8361b'
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
					borderWidth: 1,
					borderColor: 'rgba(128, 128, 128, 0.5)'
				},
				emphasis : {
					borderWidth: 1,
					borderColor: 'rgba(128, 128, 128, 0.5)'
				}
			}
        },

        gauge : {
			axisLine: {            // 坐标轴线
				show: true,        // 默认显示，属性show控制显示与否
				lineStyle: {       // 属性lineStyle控制线条样式
					color: [[0.2, '#f16b4c'],[0.8, '#d8361b'],[1, '#99311c']], 
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
				color : 'auto',
				width: 5
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

    echarts.registerTheme('red', theme);
}));
